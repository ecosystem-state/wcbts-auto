library(dplyr)
library(sf)
library(sdmTMB)

run_num <- 1

haul <- readRDS("data/wcbts_haul.rds")
catch <- readRDS("data/wcbts_catch.rds")

haul$year <- as.numeric(substr(haul$date_yyyymmdd, 1, 4))
haul$month <- as.numeric(substr(haul$date_yyyymmdd, 5, 6))
haul$day <- as.numeric(substr(haul$date_yyyymmdd, 7, 8))

haul <- dplyr::rename(haul,
  o2 = o2_at_gear_ml_per_l_der,
  degc = temperature_at_gear_c_der,
  depthm = depth_hi_prec_m
) %>%
  dplyr::select(
    o2, degc, depthm, latitude_dd, longitude_dd,
    performance, trawl_id
  )

# convert decimal deg -> UTM
haul$latitude <- as.numeric(haul$latitude_dd)
haul$longitude <- as.numeric(haul$longitude_dd)

dat_coords <-
  st_as_sf(haul[, c("longitude", "latitude")],
    coords = c("longitude", "latitude"),
    crs = 4326
  )
dat_coords <- st_transform(x = dat_coords, crs = 32610)
haul$longitude <- as.numeric(st_coordinates(dat_coords)[, 1])
haul$latitude <- as.numeric(st_coordinates(dat_coords)[, 2])
haul <- as.data.frame(haul)
haul$longitude <- haul$longitude / 1000 # to kms
haul$latitude <- haul$latitude / 1000 # to kms

# Join data
dat <- dplyr::left_join(catch, haul)

# filter out problematic spp
dat <- dplyr::filter(dat, common_name %in% c("rock sole unident.", "sand sole", "pacific flatnose",
                                             "starry rockfish", "speckled rockfish", "olive rockfish",
                                             "kelp greenling", "honeycomb rockfish", "cabezon",
                                             "california scorpionfish") == FALSE)

# Get prediction grid
grid <- readRDS("data/wc_grid.rds")
grid <- dplyr::rename(grid, longitude = X, latitude = Y) %>%
  dplyr::mutate(
    longitude = longitude * 10,
    latitude = latitude * 10
  ) %>%
  dplyr::select(
    -log_depth_scaled,
    -log_depth_scaled2
  )
grid$log_depth <- log(-grid$depth)
grid$cell <- seq(1, nrow(grid))
pred_grid <- expand.grid(cell = grid$cell, year = unique(dat$year))
pred_grid <- dplyr::left_join(pred_grid, grid)
pred_grid$fyear <- as.factor(pred_grid$year)

spp <- unique(dat$common_name)

# divide spp into thirds for GH actions
sp <- rep(1:3, length(spp))[1:length(spp)]
indx <- which(sp == run_num)

for (i in 1:length(indx)) {

  sub <- dplyr::filter(dat, common_name == spp[indx[i]])
  if (i == 1) {
    # this is equivalent to about 375 knots
    mesh <- sdmTMB::make_mesh(sub,
      xy_cols = c("longitude", "latitude"),
      cutoff = 20
    )
  }

  sub$fyear <- as.factor(sub$year) # year as factor
  sub$present <- ifelse(sub$cpue_kg_km2 > 0, 1, 0)
  sub$log_depth <- log(sub$depth_m)

  formula = cpue_kg_km2 ~ -1 + fyear + s(log_depth)
  if(spp[indx[i]] == "lingcod") formula = cpue_kg_km2 ~ -1 + fyear + log_depth + I(log_depth^2)
  fit <- try(sdmTMB(formula = formula,
                    spatiotemporal = "off",
                    time = "year",
                    family = tweedie(),
                    mesh = mesh,
                    data = sub
  ), silent = TRUE)

  if (class(fit) == "sdmTMB") {
    # do predictions for coastwide-COG
    predictions <- predict(fit, newdata = pred_grid, return_tmb_object = TRUE)
    coastwide_index <- get_index(predictions, bias_correct = TRUE)
    coastwide_index$region <- "Coastwide"
    coastwide_cog <- get_cog(predictions, bias_correct = FALSE)
    coastwide_cog$region <- "Coastwide"

    # generate quantiles on the distribution from predictions
    df <- predictions$data[, c("est", "latitude", "year")]
    quantile_df <- dplyr::group_by(df, year) %>%
      do(sample_n(., size = 10000, replace = T, weight = exp(est))) %>%
      dplyr::group_by(year) %>%
      dplyr::summarize(
        q_01 = quantile(latitude, 0.01),
        q_05 = quantile(latitude, 0.05),
        q_10 = quantile(latitude, 0.1),
        q_25 = quantile(latitude, 0.25),
        q_75 = quantile(latitude, 0.75),
        q_90 = quantile(latitude, 0.90),
        q_95 = quantile(latitude, 0.95),
        q_99 = quantile(latitude, 0.99)
      )
    quantile_df$common_names <- spp[indx[i]]

    # do predictions for north-COG north of Cape Mendocino
    # Cape Mendocino @ 40.440100, -124.409500
    predictions <- predict(fit, newdata = dplyr::filter(pred_grid, latitude > 4477559.74 / 1000), return_tmb_object = TRUE)
    north_index <- get_index(predictions, bias_correct = TRUE)
    north_index$region <- "North"
    north_cog <- get_cog(predictions, bias_correct = FALSE)
    north_cog$region <- "North"

    # do predictions for central-COG south of Cape Mendocino and north of Pt Conception
    # 34.4486, -120.4716
    predictions <- predict(fit, newdata = dplyr::filter(pred_grid, latitude > 3814797.98 / 1000, latitude < 4477559.74 / 1000), return_tmb_object = TRUE)
    central_index <- get_index(predictions, bias_correct = TRUE)
    central_index$region <- "Central"
    central_cog <- get_cog(predictions, bias_correct = FALSE)
    central_cog$region <- "Central"

    # do predictions for central-COG south of Cape Mendocino and north of Pt Conception
    # 34.4486, -120.4716
    predictions <- predict(fit, newdata = dplyr::filter(pred_grid, latitude < 3814797.98 / 1000), return_tmb_object = TRUE)
    south_index <- get_index(predictions, bias_correct = TRUE)
    south_index$region <- "South"
    south_cog <- get_cog(predictions, bias_correct = FALSE)
    south_cog$region <- "South"

    spp_cog <- rbind(coastwide_cog, north_cog, central_cog, south_cog)
    spp_index <- rbind(coastwide_index, north_index, central_index, south_index)
    spp_cog$species <- spp[indx[i]]
    spp_index$species <- spp[indx[i]]

    if (!exists("all_cog")) {
      all_cog <- spp_cog
      all_index <- spp_index
      all_quantile <- quantile_df
    } else {
      all_cog <- rbind(all_cog, spp_cog)
      all_index <- rbind(all_index, spp_index)
      all_quantile <- rbind(all_quantile, quantile_df)
    }
  }

}

saveRDS(all_cog, paste0("output/all_cog_",run_num,".rds"))
saveRDS(all_index, paste0("output/all_index_",run_num,".rds"))
saveRDS(all_quantile, paste0("output/all_quantile_",run_num,".rds"))

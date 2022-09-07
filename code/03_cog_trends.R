library(broom)
library(dplyr)
library(tidyr)
library(purrr)

d_1 <- readRDS("output/all_cog_1.rds")
d_2 <- readRDS("output/all_cog_2.rds")
d_3 <- readRDS("output/all_cog_3.rds")
d <- rbind(d_1, d_2, d_3)

d <- d %>%
  dplyr::filter(coord == "Y") %>% # only include latitude
  dplyr::filter(!is.na(se)) %>% # model didn't converge
  dplyr::filter(species %in% c("starry rockfish", "speckled rockfish", "olive rockfish", "kelp greenling", "honeycomb rockfish", "cabezon", "california scorpionfish") == FALSE)

d$region <- factor(d$region, levels = c("Coastwide", "North", "Central", "South"))

# workflow = nest / map / unnest
nested <- d %>%
  group_by(species, region) %>%
  nest()

fit <- nested %>%
  mutate(
    fit = map(data, ~ lm(est ~ year, data = ., weights = 1 / (se^2))),
    tidied = map(fit, tidy)
  ) %>%
  unnest(tidied) %>%
  #dplyr::filter(term == "year") %>%
  dplyr::select(-"data", -"fit") %>%
  as.data.frame()

saveRDS(fit, "output/regression_cog.rds")


# # # 18 have no trend in COG
# no_trend <- dplyr::group_by(fit, species) %>%
#   dplyr::summarize(n_sig = length(which(p.value < 0.05))) %>%
#   dplyr::filter(n_sig == 0)
# # 9 have significant trends in all 3 regions
# three_trend <- dplyr::group_by(fit, species) %>%
#   dplyr::summarize(n_sig = length(which(p.value < 0.05))) %>%
#   dplyr::filter(n_sig == 3)

# # to show why this is important
# dbrk <- dplyr::filter(d, species == "darkblotched rockfish") %>%
#   dplyr::group_by(region) %>%
#   dplyr::mutate(mu = mean(est), std = est - mu) %>%
#   ggplot(aes(year, std)) +
#   geom_abline(aes(intercept = 0, slope = 0), col = "red", alpha = 0.3, size = 1.5) +
#   geom_line(col = viridis(3)[2], size = 2) +
#   xlab("Year") +
#   ylab("Northings (de-meaned)") +
#   theme_bw() +
#   # scale_color_viridis_d(end=0.8) +
#   facet_wrap(~region, ncol = 1) +
#   theme(
#     strip.background = element_rect(fill = "white"),
#     text = element_text(size = 14)
#   )
# ggsave(dbrk, file = "dbrk_cog.png", width = 5, height = 4)
#
# brrk <- dplyr::filter(d, species == "brown rockfish") %>%
#   dplyr::group_by(region) %>%
#   dplyr::mutate(mu = mean(est), std = est - mu) %>%
#   ggplot(aes(year, std)) +
#   geom_abline(aes(intercept = 0, slope = 0), col = "red", alpha = 0.3, size = 1.5) +
#   geom_line(col = viridis(3)[2], size = 2) +
#   xlab("Year") +
#   ylab("Northings (de-meaned)") +
#   theme_bw() +
#   # scale_color_viridis_d(end=0.8) +
#   facet_wrap(~region, ncol = 1) +
#   theme(
#     strip.background = element_rect(fill = "white"),
#     text = element_text(size = 14)
#   )
# ggsave(brrk, file = "brrk_cog.png", width = 5, height = 4)

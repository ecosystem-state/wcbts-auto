library(broom)
library(dplyr)
library(tidyr)
library(purrr)

d_1 <- readRDS("output/all_quantile_1.rds")
d_2 <- readRDS("output/all_quantile_2.rds")
d_3 <- readRDS("output/all_quantile_3.rds")
d <- rbind(d_1, d_2, d_3)

d <- tidyr::pivot_longer(d, cols = 2:9)
d <- dplyr::rename(d, metric = name, species = common_names) %>%
  dplyr::filter(species %in% c("rock sole unident.", "sand sole", "pacific flatnose", "starry rockfish", "speckled rockfish", "olive rockfish", "kelp greenling", "honeycomb rockfish", "cabezon", "california scorpionfish") == FALSE)

# workflow = nest / map / unnest
nested <- d %>%
  group_by(species, metric) %>%
  nest()

fit <- nested %>%
  mutate(
    fit = map(data, ~ lm(value ~ year, data = .)),
    tidied = map(fit, tidy)
  ) %>%
  unnest(tidied) %>%
  #dplyr::filter(term == "year") %>%
  dplyr::select(-"data", -"fit")

saveRDS(fit, "output/regression_quantile.rds")

# # to show why this is important
# strp <- dplyr::filter(d, species %in% c("stripetail rockfish", "pacific spiny dogfish")) %>%
#   ggplot(aes(year, value, col = metric)) +
#   geom_abline(aes(intercept = 0, slope = 0), col = "red", alpha = 0.3, size = 2) +
#   geom_line(size = 2) +
#   xlab("Year") +
#   ylab("Northings") +
#   theme_bw() +
#   facet_wrap(~species, scale = "free_y") +
#   theme(
#     strip.background = element_rect(fill = "white"),
#     text = element_text(size = 20)
#   ) +
#   scale_color_viridis_d(end = 0.8)
# ggsave(strp, file = "strp.png", width = 11, height = 4.5)
#
# brrk <- dplyr::filter(d, species == "brown rockfish") %>%
#   dplyr::group_by(region) %>%
#   dplyr::mutate(mu = mean(est), std = est - mu) %>%
#   ggplot(aes(year, std)) +
#   geom_abline(aes(intercept = 0, slope = 0), col = "red", alpha = 0.3, size = 2) +
#   geom_line(col = viridis(3)[2], size = 2) +
#   xlab("Year") +
#   ylab("Northings (de-meaned)") +
#   theme_bw() +
#   # scale_color_viridis_d(end=0.8) +
#   facet_wrap(~region, ncol = 1) +
#   theme(strip.background = element_rect(fill = "white"))
# ggsave(brrk, file = "brrk_cog.png")

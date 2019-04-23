# https://dasl.datadescription.com/datafile/scottish-hill-races/
library(tidyverse)
races <- readr::read_tsv("data/scottish-hill-races.txt") %>%
  janitor::clean_names() %>%
  tidyr::gather(outcome, time_min, mens_time_min, womens_time_min) %>%
  filter(!is.na(time_min)) %>%
  mutate(
    climb_km = climb_m / 1000
  )
d

5,
20
(d$distance_km / d$mens_time_min) * 60
library(rstanarm)
m <- stan_glm(
  mens_time_min  ~ distance_km ,
  data = races,
  # prior = normal(12, 6, autoscale = FALSE),
  family = gaussian
)


# m <- stan_glm(
#   mens_time_min ~ climb_km + distance_km,
#   data = d,
#   # prior = normal(12, 6, autoscale = FALSE),
#   family = gaussian
# )
#
# summary(m)

m
pp_check(m, nreps = 100)
# tjmisc::ggpreview()
pp <- posterior_predict(m, draws = 100)

bayesplot::ppc_intervals(d$time_min , pp, d$distance_km)
bayesplot::ppc_error_scatter_avg_vs_x(d$time_min, pp, x = d$climb_km) + stat_smooth()


m2 <- stan_glm(
  mens_time_min ~ climb_km * distance_km,
  data = d,
  # prior = normal(12, 6, autoscale = FALSE),
  family = gaussian
)

summary(m2)

m2
pp_check(m2, nreps = 100)
# tjmisc::ggpreview()
pp <- posterior_predict(m2, draws = 100)

bayesplot::ppc_intervals(d$mens_time_min, pp, d$distance_km)

bayesplot::ppc_error_scatter_avg_vs_x(
  y = d$mens_time_min,
  yrep = pp,
  x = d$climb_km) +
  stat_smooth()


m2 <- stan_gamm4(
  mens_time_min ~ s(distance_km, climb_m),
  data = d,
  # prior = normal(12, 6, autoscale = FALSE),
  family = gaussian
)
summary(m2)
# plot_nonlinear(m2)

pp_check(m2, nreps = 100)
# tjmisc::ggpreview()
pp <- posterior_predict(m2, draws = 100)
bayesplot::ppc_intervals(d$mens_time_min, pp, x = d$climb_m)
bayesplot::ppc_error_scatter_avg_vs_x(d$mens_time_min, pp, x = d$climb_m) + stat_smooth()


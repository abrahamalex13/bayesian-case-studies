rm(list=ls())
library(tidyverse)
library(EnvStats)

# NOTE: Murphy parameterizes Pareto(X | k=shape, m=location)
generate_xy_dpareto <- function(par_pareto, x_dpareto, name_series) {
  
  data.frame(
    "x" = x_dpareto
    , "y" = dpareto(x_dpareto, 
                    location = par_pareto[['location']],
                    shape = par_pareto[['shape']])
    , "series" = name_series
    )
  
}



prior_uninform <- generate_xy_dpareto(
  list("location" = 0.01, "shape" = .01),
  seq(0, 50, by = .1),
  name_series = "Scenario 0: Weak Prior Expectation"
)

prior_lmle1 <- generate_xy_dpareto(
  list("location" = 200, shape = .01),
  seq(100, 1000, by = 1),
  name_series = "Scenario 1: Pointed Prior Expectation"
)

prior_lmle2 <- generate_xy_dpareto(
  list("location" = 500, shape = .01),
  seq(400, 1000, by = 1),
  name_series = "Scenario 2: Pointed Prior Expectation"
)

xy_dpareto <- bind_rows(
  prior_uninform, prior_lmle1, prior_lmle2
  )

p <- ggplot(xy_dpareto) + 
  geom_point(aes(x, y), size = .5) + 
  facet_wrap(~series, ncol = 1, scales = 'free_y') + 
  labs(x = "Total Taxi Count", y = "Weight", 
       title = paste0("Prior Knowledge about Total Taxi Count: \n",
                      "'Strength of Belief' in Possible Values")
  )

saveRDS(p, "viz_priors.rds")
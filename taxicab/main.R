rm(list=ls())
library(tidyverse)
library(EnvStats)


# NOTE: Murphy parameterizes Pareto(X | k=shape, m=location)

x_step <- 1

# analyze prior for Unif(0, \theta) ---------------------------------

x <- seq(0, 100e3, by=x_step)
prior_uninform <- data.frame(
  "x" = x, "y" = dpareto(x, location=1e-4, shape=1e-4), 
  "series" = "1: (Mathematically) Uninformative Prior Beliefs"
)

x <- seq(100, 100e3, by = x_step)
prior_ebayes <- data.frame(
  "x" = x, "y" = dpareto(x, location=100, shape=1e-2), 
  "series" = "2: Prior Beliefs Anchor Toward 100 Taxis"
)

x <- seq(1000, 100e3, by = x_step)
prior_outside <- data.frame(
  "x" = x, "y" = dpareto(x, location=1000, shape=1e-1), 
  "series" = "3: Prior Beliefs Anchor Toward 1,000 Taxis"
)

priors <- bind_rows(prior_uninform, prior_ebayes, prior_outside)

p <- ggplot(priors %>% dplyr::filter(x <= 10e3)) + 
  theme_minimal() + 
  theme(axis.text.y=element_blank(), axis.ticks.y=element_blank(),
        legend.position='top') + 
  guides(linewidth = FALSE, color = guide_legend(nrow=3)) + 
  geom_point(aes(x, y, group=series, color=series), size = .5) + 
  labs(x = "Total Taxi Count", y = "Weight (Strength of Belief)", color=NULL,
       title = "Prior Beliefs Over Hypotheses for Total Taxi Count"
  )

saveRDS(p, "viz_priors.rds")


# analyze posteriors for Unif(0, \theta) ----------------------------
# following single data observation 

x <- seq(100, 100e3, by=x_step)
posterior_uninform <- data.frame(
  "x" = x, "y" = dpareto(x, location=100, shape=1e-4 + 1), 
  "series" = "1: (Mathematically) Uninformative Prior Beliefs"
)

x <- seq(100, 100e3, by = x_step)
posterior_ebayes <- data.frame(
  "x" = x, "y" = dpareto(x, location=100, shape=1e-2 + 1), 
  "series" = "2: Prior Beliefs Anchor Toward 100 Taxis"
)

x <- seq(1000, 100e3, by = x_step)
posterior_outside <- data.frame(
  "x" = x, "y" = dpareto(x, location=1000, shape=1e-1 + 1), 
  "series" = "3: Prior Beliefs Anchor Toward 1,000 Taxis"
)

posteriors <- bind_rows(
  posterior_uninform, posterior_ebayes, posterior_outside
)

# analyze posterior relative to prior

priors[['stage']] <- "Prior Beliefs"
posteriors[['stage']] <- "Updated (Posterior) Beliefs"

priors_posteriors <- bind_rows(priors, posteriors)

priors_posteriors_t <- priors_posteriors %>% 
  spread(key=stage, value=y)

# how does probability mass concentrate across x?
priors_posteriors_t %>% 
  mutate(prior_lt_posterior = `Prior Beliefs` < `Updated (Posterior) Beliefs`,
         x_lt_10e3 = x < 10e3) %>% 
  group_by(series, x_lt_10e3) %>% 
  summarize(mean(prior_lt_posterior, na.rm=TRUE))

p <- ggplot(priors_posteriors %>% dplyr::filter(x <= 10e3)) + 
  theme_minimal() +
  theme(
    axis.text.y=element_blank(), 
    axis.ticks.y=element_blank(),
    legend.position='top'
  ) + 
  geom_point(aes(x, y, color=stage), size = .5) + 
  facet_wrap(~series, nrow = 1, labeller = label_wrap_gen()) + 
  labs(x = "Total Taxi Count", 
       y = "Weight (Strength of Belief)", 
       color=NULL,
       title = paste0("Updated vs Prior Beliefs ",
                      "Over Hypotheses for Total Taxi Count"),
       subtitle="After Seeing Taxi #100"
  )

saveRDS(p, "viz_priors_posteriors.rds")


# analyze posterior predictive ----------------

# following Murphy's Problem 3.9 notation
posterior_predictive <- function(x, k, b) {
  
  max_D <- max(x)
  N <- length(x)
  
  if (max_D <= b) {
    k / ( (N + k)*(b)^N )
  } else if (max_D > b) {
    k * (b^k) / ( (N + k)*max_D^(N + k) )
  }
  
}

# large upper bound, and small step size, help check that SUM(distr) = 1
# discrete approximation will inevitably err
x <- seq(0.01, 1e6, by=.1)
posterior_pred_uninform <- data.frame(
  "x" = x,
  "y_prior" = unlist(lapply(
    x, FUN=posterior_predictive, k=1e-4, b=1e-4
  )),
  "y_posterior" = unlist(lapply(
    x, FUN=posterior_predictive, k=(1+1e-4), b=100
  )),
  "series" = "1: (Mathematically) Uninformative Prior Beliefs"
)
# does probability mass sum to 1 over support?
apply(posterior_pred_uninform[, c("y_prior", "y_posterior")], 
      MARGIN=2, function(x) sum(x))

# ggplot(posterior_pred_uninform %>% 
#          dplyr::filter(x <= 1000)) + 
#   geom_point(aes(x, y1), color='red') +
#   geom_point(aes(x, y2), color='blue')

posterior_pred_ebayes <- data.frame(
  "x" = x,
  "y_prior" = unlist(lapply(
    x, FUN=posterior_predictive, k=1e-2, b=100
  )),
  "y_posterior" = unlist(lapply(
    x, FUN=posterior_predictive, k=(1+1e-2), b=100
  )),
  "series" = "2: Prior Beliefs Anchor Toward 100 Taxis"
)
apply(posterior_pred_ebayes %>% dplyr::select(y_prior, y_posterior), 
      MARGIN=2, function(x) sum(x))

ggplot(posterior_pred_ebayes %>%
         dplyr::filter(x <= 1000)) +
  geom_point(aes(x, y_prior), color='red') +
  geom_point(aes(x, y_posterior), color='blue')

posterior_pred_outside <- data.frame(
  "x" = x,
  "y_prior" = unlist(lapply(
    x, FUN=posterior_predictive, k=1e-1, b=1000
  )),
  "y_posterior" = unlist(lapply(
    x, FUN=posterior_predictive, k=1+1e-1, b=1000
  )),
  "series" = "3: Prior Beliefs Anchor Toward 1,000 Taxis"
)

posterior_pred <- bind_rows(
  posterior_pred_uninform, posterior_pred_ebayes, posterior_pred_outside
  )

ggplot(posterior_pred %>% dplyr::filter(x <= 10e3)) + 
  geom_point(aes(x, y_prior), color="Before Data") + 
  geom_point(aes(x, y_posterior), color = "After Data")
  
rm(list=ls())
library(tidyverse)
library(EnvStats)
set.seed(7)

X_STEP <- 1

approx_discrete_pdf_integral <- function(a, b, dens, n_sample=100e3) {
  # via Monte Carlo integration
  abs(b-a)*mean(sample(dens, size=n_sample, replace=TRUE))
}

# analyze prior for Unif(0, \theta) ---------------------------------
# NOTE: Murphy parameterizes Pareto(X | k=shape, m=location)

x_max <- 5e3

# before declaring support X, ascertain Pareto spec's range
qpareto(0.9, 1e-4, 1e-4)
x <- seq(0, x_max, by=X_STEP)
prior_uninform <- data.frame(
  "x" = x, "y" = dpareto(x, location=1e-4, shape=1e-4), 
  "series" = "1: (Mathematically) Uninformative Prior Beliefs"
)
approx_discrete_pdf_integral(0, x_max, prior_uninform[['y']])

qpareto(0.9, location=100, shape=1e-2)
qpareto(0.9, location=100, shape=1)
x <- seq(100, x_max, by = X_STEP)
prior_ebayes <- data.frame(
  "x" = x, "y" = dpareto(x, location=100, shape=1), 
  "series" = "2: Prior Beliefs Anchor Toward 100 Taxis"
)
approx_discrete_pdf_integral(100, x_max, prior_ebayes[['y']])

qpareto(0.9, location=1000, shape=1.5)
x <- seq(1000, x_max, by = X_STEP)
prior_outside <- data.frame(
  "x" = x, "y" = dpareto(x, location=1000, shape=1.5), 
  "series" = "3: Prior Beliefs Anchor Toward 1,000 Taxis"
)
approx_discrete_pdf_integral(1000, x_max, prior_outside[['y']])

priors <- bind_rows(prior_uninform, prior_ebayes, prior_outside)

p <- ggplot(priors) + 
  theme_minimal(base_size=12) + 
  theme(axis.text.y=element_blank(), axis.ticks.y=element_blank(),
        legend.position='top') + 
  guides(linewidth = FALSE, color = guide_legend(nrow=3)) + 
  geom_point(aes(x, y, group=series, color=series), size = .5) + 
  geom_hline(yintercept = 0, linetype='dashed') + 
  labs(x = "Total Taxi Count", y = "Weight (Strength of Belief)", color=NULL,
       title = "Prior Beliefs Over Hypotheses for Total Taxi Count",
       caption = "Dashed line indicates zero weight."
  )

saveRDS(p, "taxicab/viz_priors.rds")


# analyze posteriors for Unif(0, \theta) ----------------------------
# following single data observation 

x_max <- 5e3

x <- seq(0, x_max, by=X_STEP)
posterior_uninform <- data.frame(
  "x" = x, "y" = dpareto(x, location=100, shape=1e-4 + 1), 
  "series" = "1: (Mathematically) Uninformative Prior Beliefs"
)
approx_discrete_pdf_integral(0, x_max, posterior_uninform[["y"]])

x <- seq(100, x_max, by = X_STEP)
posterior_ebayes <- data.frame(
  "x" = x, "y" = dpareto(x, location=100, shape=1 + 1), 
  "series" = "2: Prior Beliefs Anchor Toward 100 Taxis"
)
approx_discrete_pdf_integral(100, x_max, posterior_ebayes[["y"]])

x <- seq(1000, x_max, by = X_STEP)
posterior_outside <- data.frame(
  "x" = x, "y" = dpareto(x, location=1000, shape=1.5 + 1), 
  "series" = "3: Prior Beliefs Anchor Toward 1,000 Taxis"
)
approx_discrete_pdf_integral(1000, x_max, posterior_outside[["y"]])

posteriors <- bind_rows(
  posterior_uninform, posterior_ebayes, posterior_outside
)

# analyze posterior relative to prior

priors[['stage']] <- "Prior Beliefs"
posteriors[['stage']] <- "Updated Beliefs"

priors_posteriors <- bind_rows(priors, posteriors)

priors_posteriors_t <- priors_posteriors %>% 
  spread(key=stage, value=y)

p <- ggplot(priors_posteriors) + 
  theme_minimal(base_size=12) +
  theme(
    axis.text.y=element_blank(), 
    axis.ticks.y=element_blank(),
    legend.position='top'
  ) + 
  geom_point(aes(x, y, color=stage), size = .5) + 
  geom_hline(yintercept=0, linetype='dashed') + 
  facet_wrap(~series, nrow = 1, labeller = label_wrap_gen()) + 
  labs(x = "Total Taxi Count", 
       y = "Weight (Strength of Belief)", 
       color=NULL,
       title = paste0("Updated vs Prior Beliefs ",
                      "Over Hypotheses for Total Taxi Count"),
       subtitle="After Seeing Taxi #100",
       caption = "Dashed line indicates zero weight."
  )

saveRDS(p, "taxicab/viz_priors_posteriors.rds")


# analyze posterior predictive ----------------

x <- seq(1, 5e3, by=X_STEP)

# following Murphy's Problem 3.9 notation
posterior_predictive <- function(x, k, b) {
  
  max_D <- max(x)
  N <- length(x)
  
  if (max_D <= b) {
    p <- k / ( (N + k)*(b)^N )
  } else if (max_D > b) {
    p <- k * (b^k) / ( (N + k)*max_D^(N + k) )
  }
  
  p
  
}

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
approx_discrete_pdf_integral(
  0, x_max, posterior_pred_uninform[["y_posterior"]]
  )

posterior_pred_ebayes <- data.frame(
  "x" = x,
  "y_prior" = unlist(lapply(
    x, FUN=posterior_predictive, k=1, b=100
  )),
  "y_posterior" = unlist(lapply(
    x, FUN=posterior_predictive, k=(1+1), b=100
  )),
  "series" = "2: Prior Beliefs Anchor Toward 100 Taxis"
)
approx_discrete_pdf_integral(
  0, x_max, posterior_pred_ebayes[["y_posterior"]]
)

posterior_pred_outside <- data.frame(
  "x" = x,
  "y_prior" = unlist(lapply(
    x, FUN=posterior_predictive, k=1.5, b=1000
  )),
  "y_posterior" = unlist(lapply(
    x, FUN=posterior_predictive, k=(1.5+1), b=1000
  )),
  "series" = "3: Prior Beliefs Anchor Toward 1,000 Taxis"
)
approx_discrete_pdf_integral(
  0, x_max, posterior_pred_outside[["y_posterior"]]
)

posterior_pred <- bind_rows(
  posterior_pred_uninform, posterior_pred_ebayes, posterior_pred_outside
  )

p <- ggplot(posterior_pred) + 
  theme_minimal(base_size=12) +
  geom_point(aes(x, y_prior, color="Forecast From Prior Beliefs")) + 
  geom_point(aes(x, y_posterior, color = "Forecast From Updated Beliefs")) + 
  geom_hline(yintercept=0, linetype='dashed') + 
  facet_wrap(~series, nrow=1, labeller = label_wrap_gen()) + 
  theme(legend.position='top') + 
  guides(linewidth = FALSE, color = guide_legend(nrow=3)) + 
  labs(x = "Next Taxi's Number", 
       y = "Probability Forecast", 
       color=NULL,
       title = "Probability Forecasts for Next Taxi's Number",
       subtitle="Based on Taxi #100 sighting and prior beliefs",
       caption = "Dashed line indicates zero weight."
  )

saveRDS(p, "taxicab/viz_posterior_pred.rds")
  
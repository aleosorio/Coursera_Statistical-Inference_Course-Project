# PRIMARY PARAMETERS
library(tidyverse)

set.seed(1)
nosim <- 1000
n <- 40
lambda <- 0.2
theor_values <- 1/lambda

# MEANS OF 40 EXPONENTIALS
## Sample
sample <- matrix(rexp(nosim * n, rate = lambda), nosim) %>%
        apply(1, mean) %>%
        as_data_frame()
names(sample) <- c("mean")

## Final Parameters
sample_mean <- mean(sample$mean)
sample_var_R <- var(sample$mean)
sample_sd_R <- sqrt(sample_var_R)
sample_var_theor <- theor_values^2/n
sample_sd_theor <- sqrt(sample_var_theor)

## PLAIN EXPONENTIALS
sample2 <- c(rexp(nosim, rate = lambda)) %>%
        as_data_frame()
names(sample2) <- c("mean")

# PLOTTING
## Histogram of means of 40 exponentials
ggplot(sample, aes(x = mean)) +
        geom_histogram(bins = 30)

## Histogram of exponentials
ggplot(sample2, aes(x = mean)) +
        geom_histogram(bins = 30)

## Density Functions of Means of 40 exponentials
ggplot(sample, aes(x = mean)) +
        geom_density(kernel = "gaussian", color = "red") +
        stat_function(fun = dnorm, args = list(mean = theor_values, sd = sample_sd_theor)) +
        geom_vline(xintercept = theor_values) +
        stat_function(fun = dnorm, args = list(mean = sample_mean, sd = sample_sd_R), color = "green") +
        geom_vline(xintercept = sample_mean, color = "green")
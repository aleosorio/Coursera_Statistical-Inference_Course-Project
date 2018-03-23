library(tidyverse)
set.seed(1)
nosim <- 1000
n <- 40
lambda <- 0.2
theor_values <- 1/lambda
sample <- matrix(rexp(nosim * n, rate = lambda), nosim) %>%
        apply(1, mean) %>%
        as_data_frame()
names(sample) <- c("mean")
sample <- mutate(sample, binwidth = (max(mean)-min(mean))/30)
sample_mean <- mean(sample$mean)
sample_var_R <- var(sample$mean)
sample_var_theor <- theor_values^2/n
sample_sd_theor <- sqrt(sample_var_theor)

## Plotting Histogram
ggplot(sample, aes(x = mean)) +
        geom_histogram(bins = 30)

## Plotting Density Functions
ggplot(sample, aes(x = mean, y = density)) +
        geom_smooth()
        stat_function(fun = dnorm, args = list(mean = theor_values, sd = sample_var_theor)) +
        scale_y_continuous(breaks = NULL)


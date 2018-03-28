# LOADING SETUP
library(tidyverse)
data("ToothGrowth")

#PRELIMINARY DATA ANALYSIS
head(ToothGrowth)
dim(ToothGrowth)
table(is.na(ToothGrowth))
str(ToothGrowth)
summary(ToothGrowth)

ggplot(ToothGrowth, aes(x = supp, y = len)) +
        geom_boxplot(aes(fill = supp), alpha = 0.5) +
        facet_wrap(~ dose, nrow = 1)

#HYPOTHESIS TESTING
## Dosage Hypothesis (interval 0.5 - 1; Ho: No growth; Ha: Significant growth due to dosage)
dose05 <- filter(ToothGrowth, ToothGrowth$dose == 0.5) %>%
        .$len
dose1 <- filter(ToothGrowth, ToothGrowth$dose == 1) %>%
        .$len

t.test(dose1, dose05, paired = TRUE, conf.level = 0.95)

## Dosage Hypothesis (interval 1 - 2; Ho: No growth; Ha: Significant growth due to dosage)
dose2 <- filter(ToothGrowth, ToothGrowth$dose == 2) %>%
        .$len

t.test(dose2, dose1, paired = TRUE, conf.level = 0.95)

## Supplement Hypothesis (Ho: No difference in growth among supplements; Ha: Significant growth between supplements)
suppoj <- filter(ToothGrowth, ToothGrowth$supp == "OJ") %>%
        .$len
suppvc <- filter(ToothGrowth, ToothGrowth$supp == "VC") %>%
        .$len

t.test(suppoj, suppvc, paired = TRUE, conf.level = 0.95)

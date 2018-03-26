data("ToothGrowth")

ggplot(ToothGrowth, aes(x = supp, y = len)) +
        geom_boxplot(aes(fill = supp), alpha = 0.5) +
        facet_wrap(~ dose, nrow = 1)
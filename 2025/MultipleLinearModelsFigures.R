pak::pkg_install(c("rstanarm", "bayesplot", "ggplot2", "cowplot", "viridis", "patchwork", "latex2exp", "extrafont", "tidyverse", "stats4"))
pak::pkg_install(c("colorspace", "viridisLite", "RColorBrewer", "munsell", "labeling", "isoband", "gtable", "farver", "scales", "numDeriv", "generics", "backports", "mvtnorm", "coda"))
remotes::install_github("rmcelreath/rethinking", force = TRUE)
library(rethinking)
library(rstanarm)
library(bayesplot)
library(ggplot2)
library(cowplot)
library(viridis)
library(patchwork)
library(latex2exp)
library(extrafont)
library(tidyverse)
library(stats4)
library(rio)

set.seed(1)

df = import("2025/ipomopsis.txt")
p = ggplot(df, aes(Root, Fruit, color = Grazing)) + 
    geom_point() + 
    labs(x = "Basal diameter (mm)", y = "Fruit Dry Mass (mg)") +
    scale_color_manual(values = c("black", 2)) +
    theme_cowplot(10) + 
    theme(legend.position = "bottom") 
ggsave("figures/ipomopsis-plot.png", plot = p, 
       width = 900, height = 750, units = "px")

# model with just treatment

fit = stan_glm(Fruit ~ Grazing, data = df, cores = 4)
summary(fit)[1:3, 1:5]

summary(fit, probs = c(0.025, 0.975))
df$Grazing0 = ifelse(df$Grazing == "Ungrazed", 1, 0)
df$Fruit = scale(df$Fruit, center = TRUE, scale = FALSE)
m1 = ulam(alist(Fruit ~ normal(mu, sigma),
           mu <- a + b*Grazing0,
           a ~ normal(0, 1),
           b ~ normal(0, 2),
           sigma ~ exponential(1)), 
     data = df, chains = 4, cores = 4)
precis(m1, prob = 0.95)

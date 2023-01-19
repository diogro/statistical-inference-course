pak::pkg_install(c("rmcelreath/rethinking", "rstanarm", "bayesplot", "ggplot2", "cowplot", "viridis", "patchwork", "latex2exp", "extrafont", "tidyverse", "stats4"))
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

#font_import()
loadfonts(device = "all", quiet = TRUE) 

ggplot(data.frame(x = rnorm(1000)), aes(x)) +
  geom_histogram(aes(y = ..density..), bins=2^6, colour="white", fill="DarkGray") +
  geom_function(fun = dnorm, colour = 2, linewidth  = 2) + theme_cowplot() +
  labs(x = expression(theta))

fig.height = 500
fig.width = fig.height * 1.2
axis.font.size = 30
figure.font = "Calibri"

# Data
library(rethinking)
d = Howell1
d2 <- Howell1[ d$age >= 18 , ] 

# Model
fit = ulam(alist(
  y ~ normal(mu, sigma),
  mu <- a + b*x,
  a ~ normal(0, 20),
  b ~ lognormal(0, 1),
  sigma ~ exponential(1)), 
  data = list(y = d2$height, 
              x = d2$weight),
  iter = 2000, chains = 4, cores = 4)
precis(fit)

png(here::here("figures", "trace_plot.png"), 
    height = 400, 
    width = 800, bg = "transparent")
traceplot_ulam(fit) 
dev.off()

samples = as_tibble(extract.samples(fit))
samples

png(here::here("figures", "height_weight_fit.png"), height = fig.height, width = fig.width, bg = "transparent")
ggplot(d2, aes(weight, height)) + 
  xlim(x_range[1], x_range[2]) + 
  geom_point(alpha = 0.5) +
  geom_abline(data = as.data.frame(samples)[1:30,], aes(intercept = a, slope = b), color= "gray", alpha = 0.7) +
  geom_abline(intercept = mean(samples$a), slope = mean(samples$b), color = 2, linewidth = 1) +
  theme_minimal() + labs(x = "weight (kg)", y = "height (cm)") + 
  geom_vline(xintercept = 0, linewidth = 1) + 
  theme(title = element_text(size = axis.font.size, color = "#586E75"),
        axis.title = element_text(size = axis.font.size, family = figure.font, color = "#586E75"), 
        axis.text = element_text(size = axis.font.size*0.8, family = figure.font, color = "#586E75"),
        legend.position = "none")
dev.off()

png(here::here("figures/height_weight_posteriors.png"), 
    height = 500, width = 1000, bg = "transparent")
par(mfrow = c(1, 2))
dens(samples$a, xlim = c(105, 125), lwd = 5, col = 2, main = expression(alpha))
dens(samples$b, xlim = c(0.7, 1.1), lwd = 5, col = 2, main = expression(beta))
dev.off()
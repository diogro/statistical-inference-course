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

set.seed(1)

df <- data.frame(y = rnorm(100, 3, 1))
p = ggplot(df, aes(x = y)) + 
    geom_histogram(aes(y = after_stat(density)),
                   breaks = seq(-1, 8, by = 0.5), 
                   colour = "black", 
                   fill = "lightgray") +
                   theme_cowplot(10) + 
    stat_function(fun = dnorm, args = list(mean = mean(df$y), sd = sd(df$y)),
                  col = 2, linewidth = 1) + 
    scale_y_continuous(limits = c(0,0.5), expand = c(0, 0)) +
    labs(x = "y", y = "Density") 
save_plot("figures/NormalDist.png", p, 
            base_height = 5, base_width = 5*1.2)
ggsave("figures/plotname.png", plot = p, 
       width = 900, height = 750, units = "px")

fit <- stan_glm(y ~ 1, data = df, cores = 4)
summary(fit) 
df <- data.frame(y = rnorm(100, 3, 1))
ols_fit = lm(y ~ 1, data = df)
precis(ols_fit, prob = 0.95)
posterior <- as.matrix(fit)

df <- data.frame(y = rnorm(100, 3, 1))
fit = ulam(alist(y ~ normal(mu, sigma),
                 mu ~ normal(0, 1),
                 sigma ~ exponential(1)), 
           data = df, chains = 4, cores = 4)
precis(fit, prob = 0.95)
png("figures/normal-precis.png", width = 900, height = 750, units = "px")
plot(precis(fit, prob = 0.95))
dev.off()

# bayesplot confidence intervals plot
samples = as.data.frame(rethinking::extract.samples(fit))
p = mcmc_intervals(samples, pars = c("mu", "sigma"), 
                   point_size = 1, inner_size = 0.5, outer_size = 0.25)
ggsave("figures/normal-intervals.png", plot = p, 
       width = 680, height = 567, units = "px")

p = ggplot(df, aes(x = y)) + 
    stat_function(fun = dnorm, 
                  args = list(mean = 0, sd = 1),
                  col = 2, linewidth = 1) + 
    scale_y_continuous(limits = c(0,0.5), expand = c(0, 0)) +
    scale_x_continuous(limits = c(-4,4), expand = c(0, 0)) +
    labs(x = TeX('$\\mu$'), y = TeX('$P(\\mu)$')) +
    theme_cowplot(10) 
ggsave("figures/standard-normal.png", plot = p, 
       width = 900, height = 750, units = "px")

p = ggplot(df, aes(x = y)) + 
    stat_function(fun = dexp, 
                  args = list(rate = 1),
                  col = 2, linewidth = 1) + 
    scale_y_continuous(limits = c(0,1), expand = c(0, 0)) +
    scale_x_continuous(limits = c(0,5), expand = c(0, 0)) +
    labs(x = TeX('$\\sigma$'), y = TeX('$P(\\sigma)$')) +
    theme_cowplot(10) 
ggsave("figures/standard-exp.png", plot = p, 
       width = 900, height = 750, units = "px")


df <- data.frame(growth = c(12, 10, 8, 11, 6, 7, 2, 3, 3),
                 tannin = c(0, 1, 2, 3, 4, 5, 6, 7, 8))
df$tannin = scale(df$tannin, scale = FALSE)
df$growth = scale(df$growth, scale = FALSE)
ols_fit = lm(growth ~ tannin, data = df)
precis(ols_fit)
fit  = ulam(alist(growth ~ normal(mu, sigma),
                  mu <- a + b*tannin,
                  a ~ normal(0, 1),
                  b ~ normal(0, 1),
                  sigma ~ exponential(1)), 
            data = df, chains = 4, cores = 4)   
precis(fit, prob = 0.95)

png("figures/linear-precis.png")
plot(precis(fit, prob = 0.95))
dev.off()

p = ggplot(df, aes(x = tannin, y = growth)) + 
    geom_point() + 
    geom_abline(intercept = coef(fit)["a"], slope = coef(fit)["b"], col = 2) +
    labs(x = "Tannin", y = "Growth") +
    theme_cowplot(10)
ggsave("figures/linear-regression-plot.png", plot = p, 
       width = 900, height = 750, units = "px")

samples = as.data.frame(rethinking::extract.samples(fit))
p = mcmc_intervals(samples, pars = c("a", "b", "sigma"), 
                   point_size = 1, inner_size = 0.5, outer_size = 0.25)

## Sample data
set.seed(0)
dat <- data.frame(x=(x=runif(100, 0, 50)),
                  y=rnorm(100, 10*x, 100))

## breaks: where you want to compute densities
breaks <- seq(0, max(df$x), len=1)
dat$section <- cut(df$x, breaks)

## Get the residuals
df$res <- residuals(lm(growth ~ tannin, data=df))

## Compute densities for each section, and flip the axes, and add means of sections
## Note: the densities need to be scaled in relation to the section size (2000 here)
d <- density(df$res, n = 9)
res <- data.frame(x=max(df$tannin) - df$growth*9, y=d$x+mean(df$growth))
res <- res[order(res$y), ]
## Get some data for normal lines as well
xs <- seq(min(df$res), max(df$res), len=50)
res <- data.frame(y=xs,
                  x=2 - 9*dnorm(xs, 0, coef(fit)["sigma"]))
res

## Plot both empirical and theoretical
## Just normal
p = ggplot(df, aes(x = tannin, y = growth)) + 
    geom_point() + 
    geom_abline(intercept = coef(fit)["a"], slope = coef(fit)["b"], col = 2) +
    labs(x = "Tannin", y = "Growth") + 
  geom_path(data=res, aes(x, y), color="salmon", lwd=1.1) +
  theme_cowplot(10) 
ggsave("figures/linear-regression-plot-sigma.png", plot = p, 

# Binary predictor example

x = rbinom(100, 1, 0.5)
y = rnorm(100, 0.5 + 1.5*x, 1)
df = data.frame(x = x, y = y)

fit = ulam(alist(y ~ normal(mu, sigma),
                 mu <- a + b*x,
                 a ~ normal(0, 1),
                 b ~ normal(0, 1),
                 sigma ~ exponential(1)), 
           data = df, chains = 4, cores = 4)

precis(fit, prob = 0.95)

p = ggplot(df, aes(x = x, y = y)) + 
    geom_jitter(width = 0.05) + 
    geom_abline(intercept = coef(fit)["a"], slope = coef(fit)["b"], col = 2) +
    labs(x = "", y = "y") +
    scale_x_continuous(breaks = c(0, 1), labels = c("Control", "Treatment")) +
    theme_cowplot(10)
p
ggsave("figures/binary-predictor-plot-model.png", plot = p, 
       width = 900, height = 750, units = "px")

# Example with 5 categories, maybe diets
set.seed(123)
diets <- c("omnivore", "carnivore", "herbivore", "frugivore", "insectivore")
x <- factor(sample(diets, 100, replace = TRUE), levels = diets)
y <- rnorm(100, mean = as.numeric(x), sd = 1)
df <- data.frame(diet = x, growth = y)
df$diet0 = as.numeric(df$diet, levels = diets)
fit <- ulam(alist(
  growth ~ normal(mu, sigma),
  mu <- a[diet0],
  a[diet0] ~ normal(0, 1),
  sigma ~ exponential(1)
), data = df, chains = 4, cores = 4)

precis(fit, depth = 2, prob = 0.95)

p <- ggplot(df, aes(x = diet, y = growth)) + 
  geom_jitter(width = 0.1, height = 0) + 
  labs(x = "Diet", y = "Growth") +
  theme_cowplot(10)
p
ggsave("figures/categorical-predictor-plot.png", plot = p, 
       width = 900, height = 750, units = "px")

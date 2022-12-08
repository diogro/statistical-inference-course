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

# font_import()
loadfonts(device = "all", quiet = TRUE) 

ggplot(data.frame(x = rnorm(1000)), aes(x)) +
  geom_histogram(aes(y = ..density..), bins=2^6, colour="white", fill="DarkGray") +
  geom_function(fun = dnorm, colour = 2, linewidth  = 2) + theme_cowplot() +
  labs(x = expression(theta))

fig.height = 800
fig.width = fig.height * 1.2
axis.font.size = 20
figure.font = "Calibri"

### Posterior samples

png(here::here("figures", "posterior_samples.png"), height = fig.height, width = fig.width, bg = "transparent")
ggplot(data.frame(x = rbeta(2000, 6, 2)), aes(x)) +
  geom_histogram(aes(y = ..density..), bins=2^6, fill= "#586E75", color = "#FDF6E3") +
  geom_function(fun = dbeta, args = list(shape1 = 6, shape2 = 2), colour = 2, linewidth  = 2) + theme_minimal() +
  scale_x_continuous(limits = c(0, 1)) + 
  labs(x = TeX('$\\theta$'), y = expression(paste("P(", theta, "|y)"))) +
  theme(axis.title = element_text(size = axis.font.size, family = figure.font, color = "#586E75"), 
        axis.text = element_text(size = axis.font.size*0.8, family = figure.font, color = "#586E75"))
dev.off()


### Maximum Likelihood surface

N <- 100
x <- rnorm(N, mean = 0, sd = 1)
y = rnorm(N, 1 + 2*x)

LL <- function(mu, beta) {
     R = dnorm(y, mu + beta*x, log = T)
     -sum(R)
  }
estimate = mle(LL, start = list(mu = 1, beta = 2))
opt_value_rev = data.frame(x = 1, y = 2)
d = expand_grid(x = seq(0, 2, 0.05), y = seq(1, 4, 0.05))
d$LL = 0
for(i in 1:nrow(d)) d$LL[i] = LL(d$x[i], d$y[i])
png(here::here("figures", "logliksurface.png"), height = fig.height, width = fig.width, bg = "transparent")
ggplot(data = d) +
  geom_tile(aes(x = x, y = y, fill = LL)) +
  stat_contour(aes(x = x, y = y, z = LL), color = 2) +
  geom_point(data = opt_value_rev, aes(x = x, y = y), color = "orange", size = 4, shape = 4, stroke = 2) +
  scale_fill_viridis_c(option = "A") + 
  labs(x = TeX('$\\mu$'), y =  TeX('$\\beta$')) +
  theme_minimal() +
  theme(axis.title = element_text(size = axis.font.size, family = figure.font, color = "#586E75"), 
        axis.text = element_text(size = axis.font.size*0.8, family = figure.font, color = "#586E75"),
        legend.position = "none")
dev.off()

### Confidence intervals

fit <- stan_glm(mpg ~ ., data = mtcars)
posterior <- as.matrix(fit)

plot_title <- ggtitle("Posterior distributions",
                      "with medians and 80% intervals")
png(here::here("figures", "confidence_intervals.png"), height = fig.height, width = fig.width, bg = "transparent")
mcmc_areas(posterior,
           pars = c("cyl", "drat", "am", "wt"),
           prob = 0.8) + theme_minimal() +
        geom_vline(xintercept = 0, linewidth = 1) + 
        theme(axis.title = element_text(size = axis.font.size, family = figure.font, color = "#586E75"), 
        axis.text = element_text(size = axis.font.size*0.8, family = figure.font, color = "#586E75"),
        legend.position = "none")
dev.off()

### Regression of weight on height for prior illustration

data(Howell1)
d <- Howell1
d2 <- d[ d$age >= 18 , ] 
xbar <- mean(d2$weight) 

png(here::here("figures", "height_weight_regression.png"), height = fig.height, width = fig.width, bg = "transparent")
ggplot(d2, aes(weight, height)) + 
  xlim(x_range[1], x_range[2]) + 
  geom_point() +
  theme_minimal() + labs(x = "weight (kg)", y = "height (cm)") + 
        geom_vline(xintercept = 0, linewidth = 1) + 
        theme(title = element_text(size = axis.font.size, color = "#586E75"),
          axis.title = element_text(size = axis.font.size, family = figure.font, color = "#586E75"), 
        axis.text = element_text(size = axis.font.size*0.8, family = figure.font, color = "#586E75"),
        legend.position = "none")
dev.off()


set.seed(2971)
N <- 100                   # 100 lines
a <- rnorm( N , 178 , 20 )
b <- rnorm( N , 0 , 10 )
data = data.frame(a, b)
x_range = range(d2$weight)
png(here::here("figures", "wide_prior.png"), height = fig.height, width = fig.width, bg = "transparent")
ggplot() + 
  xlim(x_range[1], x_range[2]) + 
  ylim(-100,400) +
  geom_abline(data = data, aes(slope = b, intercept = a - b*mean(d2$weight)), alpha = 0.2) + 
  geom_hline(yintercept = 0) +
  geom_hline(yintercept = 272) +
  theme_minimal() + labs(x = "weight (kg)", y = "height (cm)") + 
        geom_vline(xintercept = 0, linewidth = 1) + 
        theme(title = element_text(size = axis.font.size, color = "#586E75"),
          axis.title = element_text(size = axis.font.size, family = figure.font, color = "#586E75"), 
        axis.text = element_text(size = axis.font.size*0.8, family = figure.font, color = "#586E75"),
        legend.position = "none") + ggtitle(TeX('$\\beta \\sim Normal(0, 10)$')) 
dev.off()


set.seed(2971)
N <- 100                   # 100 lines
a <- rnorm( N , 178 , 20 )
b <- rlnorm( N , 0 , 1 )
data = data.frame(a, b)
x_range = range(d2$weight)
png(here::here("figures", "sensible_prior.png"), height = fig.height, width = fig.width, bg = "transparent")
ggplot() + 
  xlim(x_range[1], x_range[2]) + 
  ylim(-100,400) +
  geom_abline(data = data, aes(slope = b, intercept = a - b*mean(d2$weight)), alpha = 0.2) + 
  geom_hline(yintercept = 0) +
  geom_hline(yintercept = 272) +
  theme_minimal() + labs(x = "weight (kg)", y = "height (cm)") + 
        geom_vline(xintercept = 0, linewidth = 1) + 
        theme(title = element_text(size = axis.font.size, color = "#586E75"),
          axis.title = element_text(size = axis.font.size, family = figure.font, color = "#586E75"), 
        axis.text = element_text(size = axis.font.size*0.8, family = figure.font, color = "#586E75"),
        legend.position = "none") + ggtitle(TeX('$log(\\beta) \\sim Normal(0, 1)$')) + 
        annotate("text", x = 32, y = 273, label = "Tallest person ever (272cm)", size = axis.font.size*0.4, vjust=0, hjust = 0, family = figure.font)
dev.off()

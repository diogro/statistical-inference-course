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
opt_value_rev = data.frame(x = estimate@coef[1], y = estimate@coef[2])
d = expand_grid(x = seq(0, 2, 0.05), y = seq(1, 4, 0.05))

d$LL = 0
for(i in 1:nrow(d)) d$LL[i] = LL(d$x[i], d$y[i])
png(here::here("figures", "logliksurface.png"), height = fig.height, width = fig.width, bg = "transparent")
ggplot(data = d) +
  geom_tile(aes(x = x, y = y, fill = LL)) +
  stat_contour(aes(x = x, y = y, z = LL), color = 2) +
  geom_point(data = opt_value_rev, aes(x = x, y = y), 
  color = "orange", size = 4, shape = 4, stroke = 2) +
  scale_fill_viridis_c(option = "A") + 
  labs(x = TeX('$\\alpha$'), y =  TeX('$\\beta$')) +
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
x_range = range(d2$weight)

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
        theme(
          title = element_text(size = axis.font.size, color = "#586E75"),
          axis.title = element_text(size = axis.font.size*0.8, 
          family = figure.font, color = "#586E75"), 
        axis.text = element_text(size = axis.font.size*0.8, family = figure.font, color = "#586E75"),
        legend.position = "none") + 
        ggtitle(TeX('$log(\\beta) \\sim Normal(0, 1)$')) + 
        annotate("text", x = 32, y = 274, label = "Tallest person ever (272cm)", 
        size = axis.font.size*0.2, vjust=0, hjust = 0, family = figure.font)
dev.off()


 # Data
library(rethinking)
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
  iter = 1000, chains = 4, cores = 4)
precis(fit)
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

## Constrasts

data(milk)
d <- milk
levels(d$clade)
d$clade_id <- as.integer( d$clade )
d$K <- standardize( d$kcal.per.g )
m5.9 <- ulam(
  alist(
    K ~ normal( mu , sigma ),
    mu <- a[clade_id],
    a[clade_id] ~ normal( 0 , 0.5 ),
    sigma ~ exponential( 1 )
  ) , data=d[,c("K", "clade_id")] )
labels <- paste("a[" , 1:4 , "]:" , levels(d$clade) , sep="" )

sample = as.data.frame(extract.samples(m5.9))
png("figures/milk_model_coef.png", height = 500, width = 1000, bg = "transparent")
mcmc_intervals(sample,
           pars = c("a.1", "a.2", "a.3", "a.4"),
           prob = 0.5, prob_outer = .9) + theme_minimal() +
  geom_vline(xintercept = 0, linewidth = 1) + 
  scale_y_discrete(labels = levels(d$clade)) +
  labs(x = "expected kcal (std)") +
  theme(axis.title = element_text(size = axis.font.size, family = figure.font, color = "#586E75"), 
        axis.text = element_text(size = axis.font.size*0.8, family = figure.font, color = "#586E75"),
        legend.position = "none")
dev.off()

sample$diff_23 = sample$a.2 - sample$a.3
png("figures/milk_model_constrast.png", height = 500, width = 1000, bg = "transparent")
mcmc_dens(sample,
               pars = c("diff_23"),
               prob = 0.5, prob_outer = .9) + theme_minimal() +
  geom_vline(xintercept = 0, linewidth = 1) + 
  labs(x = "Difference between New and Old Monkeys") +
  theme(title = element_text(size = axis.font.size, 
                                  family = figure.font, color = "#586E75"),
    axis.title = element_text(size = axis.font.size, 
                                  family = figure.font, color = "#586E75"), 
        axis.text = element_text(size = axis.font.size*0.8,
                                 family = figure.font, color = "#586E75"),
        legend.position = "none")
dev.off()

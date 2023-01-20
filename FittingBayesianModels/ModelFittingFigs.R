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
data("Howell1")
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

png(here::here("figures/height_weight_pairs.png"), 
    height = 500, width = 700, bg = "transparent")
pairs(fit)
dev.off()


m2 = ulam(alist(
  y ~ normal(mu, sigma),
  mu <- a + b * x,
  a ~ normal(0, 1),
  b ~ lognormal(0, 1),
  sigma ~ exponential(1)), 
  data = list(y = d2$height - mean(d2$height), 
              x = (d2$weight - mean(d2$weight))/sd(d2$weight)),
  iter = 1000, chains = 4, cores = 4)

png(here::here("figures/height_weight_pairs_centered.png"), 
    height = 500, width = 700, bg = "transparent")
pairs(m2)
dev.off()

# Simulated data

N = nrow(d2)
n_samples = 30
x = d2$weight
simulated_data = array(NA, dim = c(N, n_samples))
samples = data.frame(samples)
for(k in 1:n_samples)
  simulated_data[,k] = rnorm(N, 
                             mean = samples[k, "a"] + samples[k, "b"] * x, 
                             sd = samples[k, "sigma"])

png(here::here("figures/height_weight_posteriors_simulation.png"), 
    height = 500, width = 700, bg = "transparent")
plot(d2$height~x, pch = 19, ylab="Height", xlab = "Weight")
for(k in 1:n_samples)
  points(simulated_data[,k]~x, col = adjustcolor(2, alpha = 0.2))
points(d2$height~x, pch = 19)
dev.off()

### Typical set

library(evolqg)
N = 10000
x = array(NA, dim = c(N, 30))
for(d in 1:30){
  samples = matrix(rnorm(d*N), ncol = d)
  x[,d] = apply(samples, 1, Norm)
}

library(ggridges)
library(gganimate)
melt_x = reshape2::melt(x)
anim = ggplot(melt_x, aes(value, group = Var2, fill = as.factor(Var2))) + 
  geom_histogram(alpha = 0.8, bins = 100)  + scale_fill_viridis_d() + 
  theme_cowplot() +
  labs(x = "Distance from distribution mean") + 
  theme(legend.position = "none") + 
  transition_states(Var2,
                    transition_length = 2,
                    state_length = 1) +  
  theme(title = element_text(size = axis.font.size, color = "#586E75"),
        axis.title = element_text(size = axis.font.size, 
                                  family = figure.font, color = "#586E75"),
        axis.text = element_text(size = axis.font.size*0.8, 
                                 family = figure.font, color = "#586E75"),
        legend.position = "none")
anim_save(filename = "figures/typical_set.gif", animation = anim ) 

png(here::here("figures", "2d_gauss.png"), height = fig.height, width = fig.width, bg = "transparent")
ggplot(data.frame(x = x[,2]), aes(x)) + geom_histogram(bins = 100) +
  theme_minimal() + labs(x = "Distance from mean (2d)", y = "Count") + 
  geom_vline(xintercept = 0, linewidth = 1) + 
  theme(title = element_text(size = axis.font.size, color = "#586E75"),
        axis.title = element_text(size = axis.font.size, family = figure.font, color = "#586E75"), 
        axis.text = element_text(size = axis.font.size*0.8, family = figure.font, color = "#586E75"),
        legend.position = "none")
dev.off()

## Milk model

data(milk)
m = milk[,c("kcal.per.g", "perc.fat", "perc.lactose")]
m$K <- standardize( m$kcal.per.g )
m$F <- standardize( m$perc.fat )
m$L <- standardize( m$perc.lactose )

m1 = ulam(alist(
  K ~ normal(mu, sigma),
  mu <- a + bx*x,
  a ~ normal(0, 0.05),
  bx ~ normal(0, 1),
  sigma ~ normal(0, 0.5)
), data = list(K = m$K, 
               x = m$F),
iter = 2000, chains = 4, cores = 4)

m2 = ulam(alist(
  K ~ normal(mu, sigma),
  mu <- a + bz*z,
  a ~ normal(0, 0.05),
  bz ~ normal(0, 1),
  sigma ~ normal(0, 0.5)
), data = list(K = m$K, 
               z = m$L),
iter = 2000, chains = 4, cores = 4)

m3 = ulam(alist(
  K ~ normal(mu, sigma),
  mu <- a + bx*x + bz*z,
  a ~ normal(0, 0.05),
  bx ~ normal(0, 1),
  bz ~ normal(0, 1),
  sigma ~ normal(0, 0.5)
), data = list(K = m$K, 
               x = m$F, 
               z = m$L),
iter = 2000, chains = 4, cores = 4)
ms = list("m1:Fat" = extract.samples(m1)[[2]], 
          "m2:Sugar" = extract.samples(m2)[[2]], 
          "m3:Fat" = extract.samples(m3)[[2]], 
          "m3:Sugar" = extract.samples(m3)[[3]])

png(here::here("figures", "milk_coefs.png"), 
    height = fig.height, width = fig.width, bg = "transparent")
par(mar = c(3, 1, 1, 1))
plot(precis(ms), xlab = "Effect on Caloric content", 
     cex = 1.5, main = "Effect on Caloric content")
dev.off()
png(here::here("figures", "milk_coef_pairs.png"), 
    height = fig.height, width = fig.width, bg = "transparent")
pairs(m3)
dev.off()

png(here::here("figures", "milk_predictor_pairs.png"), 
    height = fig.height, width = fig.width, bg = "transparent")
pairs( ~ kcal.per.g + perc.fat + perc.lactose , data=m , col=rangi2, pch = 19 )
dev.off()

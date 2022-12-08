library(rethining)
library(rstanarm)
library(bayesplot)
library(ggplot2)
library(cowplot)
library(patchwork)
library(latex2exp)
library(extrafont)
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
  geom_histogram(aes(y = ..density..), bins=2^6, colour="white", fill= "#586E75", color = "#FDF6E3") +
  geom_function(fun = dbeta, args = list(shape1 = 6, shape2 = 2), colour = 2, linewidth  = 2) + theme_minimal() +
  scale_x_continuous(limits = c(0, 1)) + 
  labs(x = TeX('$\\theta$'), y = expression(paste("P(", theta, "|y)"))) +
  theme(axis.title = element_text(size = axis.font.size, family = figure.font, color = "#586E75"), 
        axis.text = element_text(size = axis.font.size*0.8, family = figure.font, color = "#586E75"))
dev.off()


### Maximum Likelihood surface

library(ggplot2)
library(viridis)

N <- 100
x <- rnorm(N, mean = 0, sd = 1)
y = rnorm(N, 1 + 2*x)

LL <- function(mu, beta) {
     R = dnorm(y, mu + beta*x, log = T)
     -sum(R)
  }
library(stats4)
estimate = mle(LL, start = list(mu = 1, beta = 2, sigma=1))
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

### Regression of weight on height for prior illustration


fit <- stan_glm(mpg ~ ., data = mtcars)
posterior <- as.matrix(fit)

plot_title <- ggtitle("Posterior distributions",
                      "with medians and 80% intervals")
mcmc_areas(posterior,
           pars = c("cyl", "drat", "am", "wt"),
           prob = 0.8) + plot_title



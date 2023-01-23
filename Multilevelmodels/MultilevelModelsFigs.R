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

### Baseball example

# set.seed(4)
set.seed(5)
N_players = 25

true_avg = rbeta(N_players, 8, 20)
hist(true_avg)

d = matrix(NA, ncol = 2, nrow = N_players)
for(i in 1:N_players){
  d[i, 1] = at_bats = sample(5:30, 1)
  d[i, 2] = rbinom(1, at_bats, true_avg[i])
}
MLE = d[,2]/d[,1]
m1 = ulam(alist(
  hits ~ binomial(at_bats, theta),
  theta <- p[player],
  p[player] ~ beta(nu0, nu1),
  nu0 ~ lognormal(0, 1),
  nu1 ~ lognormal(0, 1)
), data = list(hits = d[,2], 
               at_bats = d[,1], 
               player = 1:N_players), 
chains = 4, cores = 4, iter = 2000)
ulam = precis(m1, depth = 2)$mean[1:N_players]
xl = range(c(ulam, MLE, true_b)) + c(-0.01, 0.01)
{
png("figures/baseball_estimates.png", height = 500, width = 700, bg = "transparent")
par(mar = c(5, 6, 2,2))
  plot(MLE, rep(3, N_players), 
     ylim = c(0.5, 3.5), pch = 19, 
     col = "blue", xlim = xl, xlab = "Batting Average (AVG)",
     yaxt="n", ylab="" , cex.axis=2, cex.lab = 2)
points(ulam, rep(1, N_players), pch = 19, col = 2)
points(true_avg, rep(2, N_players), pch = 19, col = 1)
segments(MLE, rep(3, N_players), true_avg, rep(2, N_players), lwd = 0.7, lty = 2)
segments(ulam, rep(1, N_players), true_avg, rep(2, N_players), lwd = 0.7, lty = 2)
axis(2, at = c(1, 2, 3), labels = c("Bayes", "True", "MLE"), cex.axis=2, adj = 1,     las = 2,)
dev.off()
}

library(rethinking)

## Fork

set.seed(2)
N = 100
z = rnorm(N)            # z ~ normal(0, 1)
x = rnorm(N, 1 + z)     # x ~ normal(1 + z, 1)
y = rnorm(N, 1 + x + z) # y ~ normal(1 + x + z, 1)

m1 = ulam(alist(
    y ~ normal(a + b*x, sigma),
    a ~ normal(0, 0.3),
    b ~ normal(0, 0.3),
    sigma ~ exponential(1)
), data = list(y = y, x = x), iter = 1000, chains = 4, cores = 4)
precis(m1)
s1 = extract.samples(m1)

png(here::here("figures/x-y_fork_non_corrected.png"), 
    height = 500, width = 500, bg = "transparent")
dens(s1$b, xlim = c(0.8, 1.9), lwd = 5, col = 2)
abline(v = 1, lwd = 5)
dev.off()

m2 = ulam(alist(
    y ~ normal(a + b*x + c*z, sigma),
    a ~ normal(0, 0.3),
    b ~ normal(0, 0.3),
    c ~ normal(0, 0.3),
    sigma ~ exponential(1)
), data = list(y = y, x = x, z = z), iter = 1000, chains = 4, cores = 4)
precis(m2)
s2 = extract.samples(m2)

png(here::here("figures/x-y_fork_corrected.png"), 
    height = 500, width = 500, bg = "transparent")
dens(s1$b, xlim = c(0.7, 1.9), lwd = 3, col = alpha(rgb(0,0,0), 0.3))
dens(s2$b, xlim = c(0.7, 1.9), lwd = 5, col = 2, add = T)
abline(v = 1, lwd = 5)
dev.off()


## Pipe

set.seed(1)
N = 100
x = rnorm(N)        # x ~ normal(0, 1)
z = rnorm(N, 1 + x) # z ~ normal(1 + x, 1)
y = rnorm(N, 1 + z) # y ~ normal(1 + x + z, 1)

m1 = ulam(alist(
    y ~ normal(a + b*x, sigma),
    a ~ normal(0, 0.3),
    b ~ normal(0, 0.3),
    sigma ~ exponential(1)
), data = list(y = y, x = x), iter = 1000, chains = 4, cores = 4)
precis(m1)
s1 = extract.samples(m1)

png(here::here("figures/x-y_pipe_non_corrected.png"), 
    height = 500, width = 500, bg = "transparent")
dens(s1$b, xlim = c(0.2, 1.5), lwd = 5, col = 2)
abline(v = 1, lwd = 5)
dev.off()

m2 = ulam(alist(
    y ~ normal(a + b*x + c*z, sigma),
    a ~ normal(0, 0.3),
    b ~ normal(0, 0.3),
    c ~ normal(0, 0.3),
    sigma ~ exponential(1)
), data = list(y = y, x = x, z = z), iter = 1000, chains = 4, cores = 4)
precis(m2)
s2 = extract.samples(m2)

png(here::here("figures/x-y_pipe_corrected.png"), 
    height = 500, width = 500, bg = "transparent")
dens(s1$b, xlim = c(-0.6, 1.4), lwd = 3, col = alpha(rgb(0,0,0), 0.3))
dens(s2$b, xlim = c(-0.6, 1.4), lwd = 5, col = 2, add = T)
abline(v = 1, lwd = 5)
dev.off()

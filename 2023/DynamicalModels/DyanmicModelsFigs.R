
pak::pkg_install(c("rmcelreath/rethinking", "rstanarm", "bayesplot", "ggplot2", "cowplot", "viridis", "patchwork", "latex2exp", "extrafont", "tidyverse", "stats4", "ggrepel"))
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
library(ggrepel)

# font_import()
loadfonts(device = "all", quiet = TRUE) 

ggplot(data.frame(x = rnorm(1000)), aes(x)) +
  geom_histogram(aes(y = ..density..), bins=2^6, colour="white", fill="DarkGray") +
  geom_function(fun = dnorm, colour = 2, linewidth  = 2) + theme_cowplot() +
  labs(x = expression(theta))

fig.height = 500
fig.width = fig.height * 1.2
axis.font.size = 30
figure.font = "Calibri"

data(Kline2) # load the ordinary data, now with coordinates
d <- Kline2
d$society <- 1:10 # index observations

data(islandsDistMatrix)
# display (measured in thousands of km)
Dmat <- islandsDistMatrix
colnames(Dmat) <- c("Ml","Ti","SC","Ya","Fi","Tr","Ch","Mn","To","Ha")
round(Dmat,1)

dat_list <- list(
  T = d$total_tools,
  P = d$population,
  society = d$society,
  Dmat=islandsDistMatrix )

m14.8 <- ulam(
  alist(
    T ~ dpois(lambda),
    lambda <- (a*P^b/g)*exp(k[society]),
    # non-centered Gaussian Process prior
    transpars> vector[10]: k <<- L_SIGMA * z,
    vector[10]: z ~ normal( 0 , 1 ),
    transpars> matrix[10,10]: L_SIGMA <<- cholesky_decompose( SIGMA ),
    transpars> matrix[10,10]: SIGMA <- cov_GPL2( Dmat , etasq , rhosq , 0.01 ),
    c(a,b,g) ~ dexp( 1 ),
    etasq ~ dexp( 2 ),
    rhosq ~ dexp( 0.5 )), 
  data=dat_list , chains=4 , cores=4 , iter=2000 )


# plot the prior covariance function
set.seed(7) # 5 7
x_seq <- seq( from=0 , to=10 , length.out=100 )
prior = data.frame(x = x_seq, etasq = rexp(50, 1), rhosq = rexp(50, 1/2))
K_ij = function(x, etasq, rhosq) etasq*exp(-rhosq*x^2)
p = ggplot(prior, aes(x)) + scale_y_continuous(limits = c(0, 3.5))
for ( i in 1:50 )
  p = p + geom_function(fun = K_ij, args = list(etasq = prior$etasq[i], 
                                                rhosq = prior$rhosq[i]),
                        alpha = 0.3)
png("figures/prior_covariances.png", height = 500, width = 700, 
    bg = "transparent")
p +
  theme_minimal() +
  labs(y = "covariance", x = "distance (thousand km)") +  
  theme(legend.position = "none", 
        plot.title = element_text(size = axis.font.size, 
                                  family = figure.font, 
                                  color = "#586E75")
        , axis.title = element_text(size = axis.font.size, 
                                    family = figure.font, 
                                    color = "#586E75"), 
        axis.text = element_text(size = axis.font.size*0.8, 
                                 family = figure.font, 
                                 color = "#586E75"))
dev.off()

post <- extract.samples(m14.8)

# compute posterior mean covariance
x_seq <- seq( from=0 , to=10 , length.out=100 )
pmcov <- sapply( x_seq , function(x) post$etasq*exp(-post$rhosq*x^2) )
pmcov_mu <- apply( pmcov , 2 , mean )

# plot the prior covariance function
x_seq <- seq( from=0 , to=10 , length.out=100 )
posterior = data.frame(x = x_seq, etasq = post$etasq, rhosq = post$rhosq)
K_ij = function(x, etasq, rhosq) etasq*exp(-rhosq*x^2)
p = ggplot(posterior, aes(x)) + scale_y_continuous(limits = c(0, 2))
for ( i in 1:50 )
  p = p + geom_function(fun = K_ij, args = list(etasq = posterior$etasq[i], 
                                                rhosq = posterior$rhosq[i]),
                        alpha = 0.3)
p = p + geom_function(fun = K_ij, args = list(etasq = median(posterior$etasq), 
                                              rhosq = median(posterior$rhosq)),
                      col = 2, linewidth = 1)
png("figures/posterior_covariances.png", height = 500, width = 700, 
    bg = "transparent")
p +
  theme_minimal() +
  labs(y = "covariance", x = "distance (thousand km)") +  
  theme(legend.position = "none", 
        plot.title = element_text(size = axis.font.size, 
                                  family = figure.font, 
                                  color = "#586E75")
        , axis.title = element_text(size = axis.font.size, 
                                    family = figure.font, 
                                    color = "#586E75"), 
        axis.text = element_text(size = axis.font.size*0.8, 
                                 family = figure.font, 
                                 color = "#586E75"))
dev.off()


# compute posterior median covariance among societies
K <- matrix(0,nrow=10,ncol=10)
for ( i in 1:10 )
  for ( j in 1:10 )
    K[i,j] <- median(post$etasq) *
  exp( -median(post$rhosq) * islandsDistMatrix[i,j]^2 )
diag(K) <- median(post$etasq) + 0.01
Rho <- round( cov2cor(K) , 2 )
colnames(Rho) <- c("Ml","Ti","SC","Ya","Fi","Tr","Ch","Mn","To","Ha")
rownames(Rho) <- colnames(Rho)
Rho

# scale point size to logpop
psize <- d$logpop / max(d$logpop)
psize <- exp(psize*1.5)-2
d$psize = psize

p = ggplot(d, aes(lon2, lat, size = psize)) 
for( i in 1:10 )
  for ( j in 1:10 )
    if ( i < j )
      p = p + geom_segment(x = d$lon2[i], xend = d$lon2[j],
                           y = d$lat[i],  yend = d$lat[j], 
                           alpha = Rho[i,j]^2, linewidth = 2)

pdf("figures/island_correlatons.pdf", height = 5, width = 7, 
    bg = "transparent")
p + geom_point(pch = 19, col = 2) + 
    geom_text_repel(aes(label = culture), 
                    size = axis.font.size*0.2) +
  theme_minimal() +
  labs(y = "longitude", x = "latitude") +  
  theme(legend.position = "none", 
        plot.title = element_text(size = axis.font.size, 
                                  family = figure.font, 
                                  color = "#586E75")
        , axis.title = element_text(size = axis.font.size, 
                                    family = figure.font, 
                                    color = "#586E75"), 
        axis.text = element_text(size = axis.font.size*0.8, 
                                 family = figure.font, 
                                 color = "#586E75"))
dev.off()

# compute posterior median relationship, ignoring distance
logpop.seq <- seq( from=6 , to=14 , length.out=30 )
lambda <- sapply( logpop.seq , function(lp) exp( log(post$a) + post$b*lp - log(post$g)) )
lambda.median <- apply( lambda , 2 , median )
lambda.PI80 <- apply( lambda , 2 , PI , prob=0.8 )
# plot raw data and labels
plot( d$logpop , d$total_tools , col=rangi2 , cex=psize , pch=16 ,
      xlab="log population" , ylab="total tools" )
text( d$logpop , d$total_tools , labels=labels , cex=0.7 ,
      pos=c(4,3,4,2,2,1,4,4,4,2) )
# display posterior predictions
lines( logpop.seq , lambda.median , lty=1)
lines( logpop.seq , lambda.PI80[1,] , lty=2 )
lines( logpop.seq , lambda.PI80[2,] , lty=2 )
# overlay correlations
for( i in 1:10 )
  for ( j in 1:10 )
    if ( i < j )
      lines( c( d$logpop[i],d$logpop[j] ) ,
             c( d$total_tools[i],d$total_tools[j] ) ,
             lwd=3 , col=col.alpha("black",Rho[i,j]^2) )

prediction = data.frame(x = logpop.seq,
                        mean = lambda.median, 
                        upper = lambda.PI80[2,], 
                        lower = lambda.PI80[1,])

p = ggplot(d, aes(logpop, total_tools, size = psize)) 
for( i in 1:10 )
  for ( j in 1:10 )
    if ( i < j )
      p = p + geom_segment(x = d$logpop[i], xend = d$logpop[j],
                           y = d$total_tools[i],  yend = d$total_tools[j], 
                           alpha = Rho[i,j]^2, linewidth = 2)

pdf("figures/island_correlatons_fit.pdf", height = 5, width = 7, 
    bg = "transparent")
p + geom_point(pch = 19, col = 2) + 
  geom_text_repel(aes(label = culture), 
                  size = axis.font.size*0.2) +
  geom_line(data = prediction, aes(x, mean), size = 0.8) +
  geom_line(data = prediction, aes(x, upper), size = 0.8, linetype = 2) +
  geom_line(data = prediction, aes(x, lower), size = 0.8, linetype = 2) +
  scale_y_continuous(limits = c(8, 80)) + 
  theme_minimal() +
  labs(y = "total tools", x = "log population") +  
  theme(legend.position = "none", 
        plot.title = element_text(size = axis.font.size, 
                                  family = figure.font, 
                                  color = "#586E75")
        , axis.title = element_text(size = axis.font.size, 
                                    family = figure.font, 
                                    color = "#586E75"), 
        axis.text = element_text(size = axis.font.size*0.8, 
                                 family = figure.font, 
                                 color = "#586E75"))
dev.off()




## GLM model
data("Kline")
d = Kline
d$P <- scale( log(d$population) )
d$contact_id <- ifelse( d$contact=="high" , 2 , 1 )
dat <- list(
  T = d$total_tools ,
  P = d$P ,
  cid = d$contact_id )
# intercept only
m11.9 <- ulam(
  alist(
    T ~ dpois( lambda ),
    log(lambda) <- a,
    a ~ dnorm( 3 , 0.5 )
  ), data=dat , chains=4 , log_lik=TRUE )
# interaction model
m11.10 <- ulam(
  alist(
    T ~ dpois( lambda ),
    log(lambda) <- a[cid] + b[cid]*P,
    a[cid] ~ dnorm( 3 , 0.5 ),
    b[cid] ~ dnorm( 0 , 0.2 )
  ), data=dat , chains=4 , log_lik=TRUE )


k <- PSIS( m11.10 , pointwise=TRUE )$k


plot( dat$P , dat$T , xlab="log population (std)" , ylab="total tools" ,
      col=rangi2 , pch=ifelse( dat$cid==1 , 1 , 16 ) , lwd=2 ,
      ylim=c(0,75) , cex=1+normalize(k) )
# set up the horizontal axis values to compute predictions at
ns <- 100
P_seq <- seq( from=-1.4 , to=2.4 , length.out=ns )
# predictions for cid=1 (low contact)
lambda <- link( m11.10 , data=data.frame( P=P_seq , cid=1 ) )
lmu <- apply( lambda , 2 , mean )
lci <- apply( lambda , 2 , PI )
lines( P_seq , lmu , lty=2 , lwd=1.5 )
shade( lci , P_seq , xpd=TRUE )

prediction_low = data.frame(x = P_seq,
                        mean = lmu, 
                        upper = lci[2,], 
                        lower = lci[1,])

# predictions for cid=2 (high contact)
lambda <- link( m11.10 , data=data.frame( P=P_seq , cid=2 ) )
lmu <- apply( lambda , 2 , mean )
lci <- apply( lambda , 2 , PI )
lines( P_seq , lmu , lty=1 , lwd=1.5 )
shade( lci , P_seq , xpd=TRUE )

prediction_high = data.frame(x = P_seq,
                            mean = lmu, 
                            upper = lci[2,], 
                            lower = lci[1,])

dev.off()

png("figures/island_tools.png", height = 500, width = 700, 
    bg = "transparent")
p = ggplot(d, aes(P, total_tools)) 
p = p + geom_point(color = d$contact_id, size = 4, aes(shape = contact, fill = contact_id)) + 
  geom_text_repel(aes(label = culture), 
                  size = axis.font.size*0.2) +
  scale_y_continuous(limits = c(8, 80)) + 
  theme_minimal() +
  labs(y = "total tools", x = "log population (std)") +  
  theme(legend.position = "none", 
        plot.title = element_text(size = axis.font.size, 
                                  family = figure.font, 
                                  color = "#586E75")
        , axis.title = element_text(size = axis.font.size, 
                                    family = figure.font, 
                                    color = "#586E75"), 
        axis.text = element_text(size = axis.font.size*0.8, 
                                 family = figure.font, 
                                 color = "#586E75"))
p
dev.off()

png("figures/island_glm_fit.png", height = 500, width = 700, 
    bg = "transparent")
p + geom_line(data = prediction_low, aes(x, mean), size = 0.8) +
  geom_line(data = prediction_low, aes(x, upper), size = 0.8, linetype = 2) +
  geom_line(data = prediction_low, aes(x, lower), size = 0.8, linetype = 2) +
  geom_line(data = prediction_high, aes(x, mean), size = 0.8, col = 2) +
  geom_line(data = prediction_high, aes(x, upper), size = 0.8, linetype = 2, col = 2) +
  geom_line(data = prediction_high, aes(x, lower), size = 0.8, linetype = 2, col = 2)
dev.off()


library(rethinking)
data(Primates301)
data(Primates301_nex)
# plot it using ape package - install.packages('ape') if needed
library(ape)
png("figures/primates_nex.png", height = 700, width = 700, 
    bg = "transparent")
plot( ladderize(Primates301_nex) , type="fan" , font=1 , no.margin=TRUE ,
      label.offset=1 , cex=0.5 )
dev.off()

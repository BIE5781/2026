library(rethinking)

m11.10 <- ulam(
    alist(T~dpois(lambda),
          log(lambda) <- a[cid]+b[cid]*P,
          a[cid]~dnorm(3,0.5),
          b[cid]~dnorm(0,02)),
    data=dat, chains=4, log_lik=TRUE)

dat2 <- list(T=d$total_tools, P = d$population, cid = d$contact_id)

m11.11 <- ulam(
    alist(
        T ~ dpois(lambda),
        lambda <- exp(a[cid])*P^b[cid]/g,
        a[cid] ~ dnorm(1,1),
        b[cid] ~ dexp(1),
        g ~ dexp(1)),
    data = dat2, chains = 4, log_lik=TRUE)
## Meu experimento: o mesmo modelo com log-link
dat3 <- list(T=d$total_tools, P = log(d$population), cid = d$contact_id)
m11.11b <- ulam(
    alist(
        T ~ dpois(lambda),
        log(lambda) <- a[cid] + b[cid]*P - log(g),
        a[cid] ~ dnorm(1,1),
        b[cid] ~ dexp(1),
        g ~ dexp(1)),
    data = dat3, chains = 4, log_lik=TRUE)

##Comparando coeficientes
precis(m11.11, depth=2)
precis(m11.11b, depth=2)
## Graficos
ns <- 100
P_seq <- seq(-5,3,length.out=ns)
pop_seq <- exp(P_seq)
mu1 <- link(m11.11, data.frame(P=pop_seq, cid=1))
mu2 <- link(m11.11, data.frame(P=pop_seq, cid=2))

plot(T ~ P, data = dat2, col=rangi2,
     pch = ifelse(dat2$cid==1, 1,16), lwd=2,
     ylim=c(0,75))

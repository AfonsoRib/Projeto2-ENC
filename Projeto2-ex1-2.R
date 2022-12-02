## 1.2



survivalTimes = c(1552, 627, 884, 2183, 1354, 1354, 1014, 2420,  71, 3725,
	          2195, 2586, 1577, 1766, 1325, 1299, 159, 1825, 965, 695)

B = 100000


### the bootstrap
t.star = numeric(B)
for (i in 1:B){
    x.boot = sample(survivalTimes, length(survivalTimes), replace=T)
    t.star[i] = mean(x.boot)
}

t.boot = mean(t.star)
se.t.boot = sqrt(var(t.star))

###(a)

mu0 = 1020
sd0 = sd(survivalTimes)
n = length(survivalTimes)

t.obs = (mean(survivalTimes)-1020)/(sd(survivalTimes)/sqrt(n)); t.obs
p.value = 1-pt(t.obs,n-1); p.value

t.obs = (mean(survivalTimes)-mu0)/(sd0/sqrt(n))
t.star=numeric(B)
set.seed(123)
z=survivalTimes-mean(survivalTimes)+mu0
for(i in 1:B){
    z.star    = sample(z,n,replace=T)
    sd.z.star = sd(z.star)
    t.star[i] = (mean(z.star)-mu0)/(sd.z.star/sqrt(n))
}
# decision on H0 based on the p.value
p.value <- sum(t.star>t.obs)/B ## TODO verificar se direção do teste está correta
  p.value  

###(b)
alpha = 0.1

##normal
t= mean(survivalTimes)
ci= t - c(qnorm(1-alpha/2),qnorm(alpha/2)) * se.t.boot; ci

### pivotal CI (review)

delta = t.star - t
d = quantile(delta, c(alpha/2,1-alpha/2))
ci.boot = t - c(d[2],d[1]); ci
names(ci.boot) <- c("5%", "95%")
ci.boot

## percentile 90% CI
d = quantile(delta, c(alpha/2,1-alpha/2)); d


## (d)
library(boot)
T <- function(data,indices){return(median(data[indices,]))}
boot.mean <- boot(data=as.data.frame(survivalTimes),statistic = T,R=B)

boot.ci(boot.mean,type=c("basic","norm","perc"))

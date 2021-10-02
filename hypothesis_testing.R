#one Sample Test
sed.seed(1)
iq.dat<-round(rnorm(20,100,15))
mean(iq.dat)
sd(iq.dat)
sort(iq.dat)
hist(iq.dat)

#test the following:
#Ho: sigma^2 = 15^2
#Ha: sigma^2 != 15^2
n <- 20

chisq.star<- (n-1)*var(iq.dat)/15^2
chisq.star

#rejection region at 5% level
##right tailed test:
qchisq(.95,19)#reject if larger than this

##left tailed test:
qchisq(.05,19)#reject if smaller than this

##two tailed test:
qchisq(.025,19)#reject if smaller than this, OR
qchisq(.975,19)#reject if larger than this


#p-val
pchisq(chisq.star,19)#right tailed p-value
1 - pchisq(chisq.star,19)#left tailed p-value
2 * min(pchisq(chisq.star,19),1 - pchisq(chisq.star,19))#two tailed p-value

#test for mean
#Ho: mu <= 101
#Ha: mu > 101
t.star <- (mean(iq.dat) - 101)/(sd(iq.dat)/sqrt(n))
t.star

#rejection region at 5% level
##right tailed test:
qt(.95,19)#reject if larger than this

##left tailed test:
qt(.05,19)#reject if smaller than this

##two tailed test:
qt(.025,19)#reject if smaller than this, OR
qt(.975,19)#reject if larger than this


#p-val
pt(t.star,19)#right tailed p-value
1 - pt(t.star,19)#left tailed p-value
2 * min(pt(t.star,19),1 - pt(t.star,19))#two tailed p-value

#use R function t.test
t.test(iq.dat,alternative = "greater", mu = 101)


##Two Sample Inference

recent.grp <- c(19.5,20.3,19.6,20.2,17.8,17.9,19.1,18.8,17.6,16.8)
past.grp <- c(20.4,21.9,22.1,22.3,20.3,18.8,18.9,19.4,18.4,19.1)

mean(recent.grp);sd(recent.grp)
mean(past.grp);sd(past.grp)

#test on variances, then pooled or non-pooled test
var.test(recent.grp,past.grp)#FTR
t.test(recent.grp,past.grp,alt="less",var.equal=TRUE)

#permutation test on difference of means
choose(20,10)#number of possible permutations
new.dat <- c(recent.grp,past.grp)
obs.mean.diff <- mean(recent.grp) - mean(past.grp)
nsim <- 100000
sim.mean.diff <- rep(NA,length=nsim)
for (i in 1:nsim){
  grps <- sample(c(rep(1,10),rep(2,10)),replace=FALSE)
  sim.mean.diff[i] <- mean(new.dat[grps==1]) - mean(new.dat[grps==2])
}

hist(sim.mean.diff);abline(v=obs.mean.diff,col="red",lty=2)
length(sim.mean.diff[sim.mean.diff<=obs.mean.diff])/nsim #estimated p-value

#WMW test
wilcox.test(recent.grp,past.grp,alt="less",exact=FALSE,correct=TRUE)


#non-parametric BS
data("faithful")

hist(faithful$eruptions)

#pretend it is reasonable to think that these data are normal
t.test(faithful$eruptions)

#pretend it is reasonable to think that these data are from loc-scale family
#get CI for mean
B <- 1000
boot.samp <- rep(NA,times=B)
for (b in 1:B){
  boot.dat <- faithful$eruptions[sample(1:length(faithful$eruptions),length(faithful$eruptions),replace = TRUE)]
  boot.samp[b] <- (mean(boot.dat) - mean(faithful$eruptions))/(sd(boot.samp)/sqrt(length(faithful$eruptions)))
}

mean(faithful$eruptions) - quantile(boot.samp,.975)*sd(faithful$eruptions)/sqrt(length(faithful$eruptions))
mean(faithful$eruptions) + quantile(boot.samp,.975)*sd(faithful$eruptions)/sqrt(length(faithful$eruptions))

#percentile method for median
B <- 1000
boot.samp <- rep(NA,times=B)
for (b in 1:B){
  boot.dat <- faithful$eruptions[sample(1:length(faithful$eruptions),length(faithful$eruptions),replace = TRUE)]
  boot.samp[b] <- median(boot.dat)
}

quantile(boot.samp,.025)
quantile(boot.samp,.975)

#residual method for median
B <- 1000
boot.samp <- rep(NA,times=B)
for (b in 1:B){
  boot.dat <- faithful$eruptions[sample(1:length(faithful$eruptions),length(faithful$eruptions),replace = TRUE)]
  boot.samp[b] <- median(boot.dat) - median(faithful$eruptions)
}

median(faithful$eruptions) - quantile(boot.samp,.975)
median(faithful$eruptions) - quantile(boot.samp,.025)

#use R bootstrap package
library("boot")

boot.out <- boot(data=faithful$eruptions,statistic = function(dat,inds){median(dat[inds])},R=2000)
boot.ci(boot.out,type=c("perc", "bca"))



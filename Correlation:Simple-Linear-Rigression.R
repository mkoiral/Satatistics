#Correlation

# ----------
# example
# ----------
x <- -2:2
y <- x^2
plot(x,y)
cor(x,y)


# ---------------------------------------------------------------------
# simulate from bivariate normal, estimate rho, and create scatterplot
# ---------------------------------------------------------------------
install.packages("mvtnorm") #only need to install a package once!
library(mvtnorm) #load 'mvtnorm' package

n <- 100 #number of bivariate observations
rho <- .75 #true correlation coefficient
sig.mat <- matrix(c(1,rho,rho,1),2,2) #true correlation matrix
#set.seed(2)
dat <- rmvnorm(n,mean=c(0,0),sigma=sig.mat) #get random sample of n bivariate pairs
rho.hat <- cor(dat[,1],dat[,2]) #estimate rho
rho.hat
plot(dat,main=paste("rho hat = ",round(rho.hat,4),sep=""),xlab="x",ylab="y") #create scatterplot
cor.test(dat[,1],dat[,2])

new.dat <- cbind(dat,matrix(rnorm(100*8),100,8))
cor.test(new.dat[,4],new.dat[,5])
choose(10,2)
.05/45#now FTR

# ------------------------------
# small simulation study
# ------------------------------
n <- 10 #number of bivariate observations per simulated data set
rho <- .7 #true correlation coefficient
sig.mat <- matrix(c(1,rho,rho,1),2,2) #true correlation matrix
rhohat.vec <- rep(NA,1000) #create vector to store all of the rho hats

for (i in 1:1000){
  trash.dat <- rmvnorm(n,mean=c(0,0),sigma=sig.mat) #get random sample of n bivariate pairs
  rhohat.vec[i] <- cor(trash.dat[,1],trash.dat[,2]) #estimate rho
}

summary(rhohat.vec)
hist(rhohat.vec,main="Histogram of Sample Correlations",xlab="rho hat")


# critical values and p values in R
qt(.95,6)
1 - pt(2.35,6)


# -----------------------
# simulated SLR example
# -----------------------
set.seed(10) #set the random seed so that everyone gets identical results
x <- runif(50,0,10) #randomly select some x values
y <- 30 + 2*x + rnorm(50,0,17.5) #randomly generate some y values
plot(x,y,pch=17,col="blue",main="Scatterplot of x versus y") #create the scatterplot
cor.test(x,y) #test whether rho != 0
m1 <- lm(y~x) #fit SLR model
abline(m1,col="red",lty=2) #add SLR line to scatterplot
legend(.15,83,legend="Least Squares Line",col="red",lty=2)
summary(m1) #get summary of SLR model
anova(m1)


#inference E(Y_h) for X_h =6
Xh <- 6
Yhhat <- m1$coef[1] + m1$coef[2]*Xh
Yhhat
anova(m1)
MSE <- 250.58# from anova table
Sxx <- sum((x - mean(x))^2)
SYhhat <- sqrt(MSE*(1/50 + (Xh - mean(x))^2/Sxx))
#95% ci for Yhhat
Yhhat - qt(.975,50-2)*SYhhat
Yhhat + qt(.975,50-2)*SYhhat


#predict new response for X_h =6
Xh <- 6
Yhnew <- m1$coef[1] + m1$coef[2]*Xh
Yhnew
MSE <- 250.58# from anova table
Sxx <- sum((x - mean(x))^2)
SYhnew <- sqrt(MSE*(1 + 1/50 + (Xh - mean(x))^2/Sxx))
#95% ci for Yhhat
Yhnew - qt(.975,50-2)*SYhnew
Yhnew + qt(.975,50-2)*SYhnew

#confidence bands for regression line
xseq <- seq(0,10,by=.1)
yhat <- m1$coef[1] + m1$coef[2]*xseq
SYhhat <- sqrt(MSE*(1/50 + (xseq - mean(x))^2/Sxx))
W <- sqrt(2*qf(.95,2,50-2))
lwr.band <- yhat - W*SYhhat
upr.band <- yhat + W*SYhhat
plot(x,y,pch=17,col="blue",main="Scatterplot of x versus y") #create the scatterplot
abline(m1,col="red",lty=1) #add SLR line to scatterplot
lines(xseq,lwr.band,col="purple",lty=3)
lines(xseq,upr.band,col="purple",lty=3)
legend(.15,83,legend=c("Least Squares Line","95% Conf. Bands"),col=c("red","purple"),lty=c(1,3))

#diagnostic plots
# 1) use above data
par(mfrow=c(2,2));plot(x,y,main="Scatterplot of X vs Y");abline(m1);plot(x,m1$resid,main="X vs Raw Residuals");abline(0,0);qqnorm(m1$resid);qqline(m1$resid);acf(m1$resid,main="ACF Plot of Residuals")
stndres <- rstandard(m1)
#install.packages("car")
library(car)
par(mfrow=c(2,2));plot(x,y,main="Scatterplot of X vs Y");abline(m1);plot(x,stndres,main="X vs Standardized Residuals");abline(0,0);qqPlot(stndres);acf(m1$resid,main="ACF Plot of Residuals")
abs(dffits(m1))
abs(dfbetas(m1))
cooks.distance(m1);max(cooks.distance(m1));qf(.5,1,49)
hatvalues(m1)#leverage values
influence.measures(m1)

# 2) use a different data set (autocorrelation)
rho <- 0.5;sig <- 17.5;sig.mat <- diag(50)
sig.mat <- sig * rho^abs(row(sig.mat)-col(sig.mat))
library(mvtnorm)
set.seed(10) #set the random seed so that everyone gets identical results
x <- runif(50,0,10) #randomly select some x values
y2 <- 30 + 2*x + c(rmvnorm(1,sigma=sig.mat))
plot(x,y2)
m2 <- lm(y2~x)
summary(m2)
par(mfrow=c(2,2));plot(x,y2,main="Scatterplot of X vs Y");abline(m2);plot(x,m2$resid,main="X vs Raw Residuals");abline(0,0);qqPlot(m2$resid);acf(m2$resid,main="ACF Plot of Residuals")
par(mfrow=c(2,2));plot(x,y2,main="Scatterplot of X vs Y");abline(m2);plot(1:50,m2$resid,main="Time vs Raw Residuals");abline(0,0);qqPlot(m2$resid);acf(m2$resid,main="ACF Plot of Residuals")
influence.measures(m2)

# 3) use another data set (heteroscedacity)
set.seed(11) #set the random seed so that everyone gets identical results
x <- runif(50,0,10) #randomly select some x values
y3 <- 30 + 2*x + c(rmvnorm(1,sigma=diag(3*x^2)))
plot(x,y3)
m3 <- lm(y3~x)
summary(m3)
par(mfrow=c(2,2));plot(x,y3,main="Scatterplot of X vs Y");abline(m3);plot(x,m3$resid,main="X vs Raw Residuals");abline(0,0);qqPlot(m3$resid);acf(m3$resid,main="ACF Plot of Residuals")
influence.measures(m3)


# 4) use another data set (curvature)
set.seed(10) #set the random seed so that everyone gets identical results
x <- runif(50,0,10) #randomly select some x values
y4 <- 30 + 2*x + 4*x^2 + rnorm(50,0,17.5)
plot(x,y4)
m4 <- lm(y4~x)
summary(m4)
par(mfrow=c(2,2));plot(x,y4,main="Scatterplot of X vs Y");abline(m4);plot(x,m4$resid,main="X vs Raw Residuals");abline(0,0);qqPlot(m4$resid);acf(m4$resid,main="ACF Plot of Residuals")
influence.measures(m4)


# 5) one more data set (outliers)
set.seed(10) #set the random seed so that everyone gets identical results
x <- runif(50,0,10) #randomly select some x values
y5 <- 30 + 2*x + rnorm(50,0,5) #randomly generate some y values
m5 <- lm(y5~x) #fit SLR model
stndres <- rstandard(m5)
summary(m5) #get summary of SLR model
par(mfrow=c(2,2));plot(x,y5,main="Scatterplot of X vs Y");abline(m5);plot(x,stndres,main="X vs Standardized Residuals");abline(0,0);qqPlot(stndres);acf(m5$resid,main="ACF Plot of Residuals")
influence.measures(m5)
#add outlier at 20,70
newy <- c(y5,70);newx <- c(x,20)
m6 <- lm(newy ~ newx)
stndres <- rstandard(m6)
par(mfrow=c(2,2));plot(newx,newy,main="Scatterplot of X vs Y");abline(m6);abline(m5);plot(newx,stndres,main="X vs Standardized Residuals");abline(0,0);qqPlot(stndres);acf(m6$resid,main="ACF Plot of Residuals")
influence.measures(m6)
plot(newx,newy,type="n");text(newx,newy,1:51,cex=.6)
#add outlier at 20,20
newy <- c(y5,20);newx <- c(x,20)
m6 <- lm(newy ~ newx)
stndres <- rstandard(m6)
par(mfrow=c(2,2));plot(newx,newy,main="Scatterplot of X vs Y");abline(m6);abline(m5);plot(newx,stndres,main="X vs Standardized Residuals");abline(0,0);qqPlot(stndres);acf(m6$resid,main="ACF Plot of Residuals")
influence.measures(m6)
#add outlier at 5,70
newy <- c(y5,70);newx <- c(x,5)
m6 <- lm(newy ~ newx)
stndres <- rstandard(m6)
par(mfrow=c(2,2));plot(newx,newy,main="Scatterplot of X vs Y");abline(m6);abline(m5);plot(newx,stndres,main="X vs Standardized Residuals");abline(0,0);qqPlot(stndres);acf(m6$resid,main="ACF Plot of Residuals")
influence.measures(m6)








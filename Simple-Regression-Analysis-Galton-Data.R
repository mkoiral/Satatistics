#Simple Linear Regression (Galton Data)
par(mfrow=c(1,1))
#Give the scatterplot with the simple linear regression line for the Galton data using ONLY fathers’ heights versus sons’ heights (sons’ height is the response variable). You will need to subset the full data set! It may be helpful to know that I used the following line of code.

galton <- read.table("~/Desktop/Data-Analysis//Galton.txt", header=TRUE)
father.son.dat <- galton[galton[,4]=="M",]
father <- father.son.dat[,2]
son <- father.son.dat[,5]
dim(galton)
dim(father.son.dat)

#1: use father/son data from Galton data set
#scatterplot w/ SLR line
plot(father,son)

#On the above scatterplot, add 95% confidence bands for the true regression line for the above model.

m1 <- lm(Height ~ Father,data = father.son.dat)
abline(m1)
anova(m1)
#add confidence bands for regression line
#confidence bands for regression line
MSE <- 5.88
Sxx <- sum((father - mean(father))^2)
xseq <- seq(60,80,by=.1)
yhat <- m1$coef[1] + m1$coef[2]*xseq
SYhhat <- sqrt(MSE*(1/465 + (xseq - mean(father))^2/Sxx))
W <- sqrt(2*qf(.95,2,465-2))
lwr.band <- yhat - W*SYhhat
upr.band <- yhat + W*SYhhat
lines(xseq,lwr.band,col="purple",lty=3)
lines(xseq,upr.band,col="purple",lty=3)

#Give and interpret (in the context of the problem) the slope estimate in the simple linear regression model for the Galton data using ONLY fathers’ heights versus sons’ heights (sons’ height is the response variable). You will need to subset the full data set!
  
m1

Call:
  lm(formula = Height ~ Father, data = father.son.dat)

Coefficients:
  (Intercept)       Father  

#For a one inch increase in a father’s height, the son’s height increases by .448 inches on average.

#For the simple linear regression model for the GALTON data using ONLY fathers’ heights versus sons’ heights (sons’ height is the response variable), 
#give the Normal QQ plot of the residuals and a plot of the standardized residuals. What information do they give you? Does the SLR model seem reasonable?
  
stndres <- rstandard(m1)
par(mfrow=c(1,2))
plot(father,stndres,main="X vs Standardized Residuals");abline(0,0);qqnorm(stndres);qqline(stndres)
  
#There does not appear to be any pattern in the residual plot and the Normal Q-Q plot of the
#residuals looks fairly linear (except perhaps in the tails), so the SLR model assumptions seem to be reasonable here.

#Test whether the true slope in the simple linear regression model for the GALTON data using only the fathers’ heights versus sons’ 
#heights (sons’ height is the response variable) is less than 1 at the 5% level. Give your hypotheses, test statistic, p-value, and conclusion
#(in the context of the problem).
summary(m1)$coefficients
summary(m1)$coefficients[2,2]
m1$coefficients[2]
tstar <- (m1$coefficients[2]  - 1)/summary(m1)$coefficients[2,2]
tstar
pt(tstar,465-2)

#Ho: \beta >= 1
#Ha: \beta < 1

#Since the p-value is less than 0.05, we do have sufficient statistical evidence to 
#conclude that the true slope is less than 1 at the 5% level.

#Using the above model, predict the height of son whose father is 5’10” (70”).

m1$coefficients[1] + 70*m1$coefficients[2]

# Using the above model, give and interpret the 95% confidence interval for E(Y|X=70)

Xh <- 70
Yhhat <- m1$coef[1] + m1$coef[2]*Xh
Yhhat
(Intercept) 
69.60127 
SYhhat <- sqrt(MSE*(1/465 + (Xh - mean(father))^2/Sxx))
#95% ci for Yhhat
Yhhat - qt(.975,465-2)*SYhhat
(Intercept) 
69.36625 
Yhhat + qt(.975,465-2)*SYhhat
(Intercept) 
69.83628 
 predict(m1,newdata = data.frame("Father"=70), interval = "confidence")


#Using the above model, give the 95% prediction interval for the height of a son whose f
#ather is 5’10” (70”).


Xh <- 70
Yhnew <- m1$coef[1] + m1$coef[2]*Xh
Yhnew
(Intercept) 
69.60127 
SYhnew <- sqrt(MSE*(1 + 1/465 + (Xh - mean(father))^2/Sxx))
#95% ci for Yhhat
Yhnew - qt(.975,465-2)*SYhnew
(Intercept) 
64.83036 
Yhnew + qt(.975,465-2)*SYhnew
(Intercept) 
74.37217 
predict(m1,newdata = data.frame("Father"=70), interval = "prediction")

#For this problem, use the ENTIRE Galton data set and consider the variables: father, mother, 
#and child.

 #Give the sample correlation coefficients for all pairs of variables.

cor(galton[,c(2,3,5)])

#Use the R function cor.test to test whether there is sufficient evidence to conclude that 
#each respective true correlation parameter is non-zero at the 5% level. Interpret your results in the context of the problem.

cor.test(galton[,2],galton[,3])$p.val
cor.test(galton[,2],galton[,5])$p.val
cor.test(galton[,3],galton[,5])$p.val


#Since each respective p-value is small, we would conclude that there is evidence that the corresponding true correlation coefficient is non-zero.


#Do the results  abovechange if you use a Bonferroni correction in the tests? Interpret your results in t
#he context of the problem. Make sure to discuss why a Bonferroni correction may be relevant here.

.05/3

#Using a Bonferroni correction where FWER = 5%, we would reject when an individual test has a p-value less than .05/3=.0167. 
#In this example, we would FTR in the test of Father vs Mother.



  
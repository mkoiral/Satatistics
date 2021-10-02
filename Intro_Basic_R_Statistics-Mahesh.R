##Basic Introduction
#loads old faithful data set that is built into R
data(faithful)
#list all objects in R workspace
ls()
#get object type of faithful
class(faithful)
#get dimension of faithful
dim(faithful)
#get variable names assosiated with faithful
names(faithful)
#print faithful output
faithful
#print eruption time variable
faithful$eruptions
#print waitiming variable
faithful$waiting

#basic descriptive statistics
#get average eruptive time
mean(faithful$eruptions)
#get median eruption time
median(faithful$eruptions)
#get 25th, 50th and 75th percent eruption time
quantile(faithful$eruptions, c(0.25,0.50,0.75))

#get IQR
quantile(faithful$eruptions,.75) - quantile(faithful$eruptions,.25)

#get random variance
var(faithful$eruptions)

#get random standard deviation
sd(faithful$eruptions)

#get a bunch of summary statistics
summary(faithful$eruptions)

#some basic plots

#create 1 by 3 plotting window
par(mfrow=c(1,3))
#create boxplot of eruption data
boxplot(faithful$eruptions)
#crate histogram of eruption data
hist(faithful$eruptions)
#get normal QQ plot for eruption data
qqnorm(faithful$eruptions);qqline(faithful$eruptions)

#maybe there is a mixture of distribution
#consider only eruption greater than 3 minutes
par(mfrow=c(1,2))
hist(faithful$eruptions[faithful$eruptions>3])
qqnorm(faithful$eruptions[faithful$eruptions <= 3]);qqline(faithful$eruptions[faithful$eruptions <= 3])

#maybe we could create a categorical variable based on waiting time

#maybe we could create a categorical variable based on waiting time
quantile(faithful$waiting,c(.25,.5,.75)) #get 25th, 50th, and 75th percentiles for waiting time
faithful$CATwaiting <- rep(4,length=nrow(faithful)) #create a categorical waiting time variable
faithful$CATwaiting[faithful$waiting < 58] <- 1
faithful$CATwaiting[faithful$waiting < 76 & faithful$waiting >= 58] <- 2
faithful$CATwaiting[faithful$waiting < 82 & faithful$waiting >= 76] <- 3
par(mfrow=c(1,1)) #return to 1 by 1 plotting window
boxplot(faithful$eruptions ~ faithful$CATwaiting,xlab="Categorical Waiting Time",ylab="Eruption Time")


#look at relationship between eruption time & waiting time
#returns to 1 by 1 plotting window
par(mfrow=c(1,1)) 
plot(faithful$waiting, faithful$eruptions, xlab="waiting Time",ylab="Eruption Time", main ="Scatterplot of Waiting Time versus Eruption Time")

#get probabilities and quantiles from a normal distribution

qnorm(.975,mean=0,sd=1) #find the 0.975 quantile on a N(0,1)
pnorm(103,mean=100,sd=15) #find P(X<103) where X~N(100,15)

#generate random sample from a normal population
#if you use the same random seed , you'll always get the same results
set.seed(12)
#generate a sample of size 100 from a N(100,15)
MySample<- rnorm(n=100, mean =100, sd=15)
sd(MySample)
mean(MySample)
par(mfrow=c(1,2))
#plot a histogram of MySample
hist(MySample)
#Create a Normal QQ of the sample
qqnorm(MySample); qqline(MySample)

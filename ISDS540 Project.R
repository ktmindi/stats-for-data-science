crudecost=read.csv("CrudeCostTraining.csv") 
View(crudecost)

head(crudecost)
tail(crudecost)

x = crudecost[,2]
x

y= crudecost [,3]
y


plot(x,y, type="b", pch=18, xlab="t", ylab="Cost", 
     main="Cost of Crude Oil from 2010-2013")

samplemean=mean(y)  ##xbar = point estimator 
samplemean
median=median(y)   
median

range=max(y)-min(y)   ##RANGE
range

boxplot(y, horizontal=TRUE)
IQR(y)     ##INTERQUARTILE RANGE
boxplot(y)$stats   ## [1,] lower whisker, [2,] Q1, [3,] Q2 MEDIAN, [4,] Q3, [5,] upper whisker

costvar=var(y)   ##VARIANCE
costvar

coststdev=sd(y)    ##STANDARD DEVIATION
coststdev

COEFFV=sd(y)/mean(y)*100   ##COEFFICIENTT OF VARIATION
COEFFV

##average cost of crude oil with 95% confidence level 
##(since the sample size is more than 30 using the Central Limit Theorm 
## we will conclude that xbar will be normally distributed)
alpha=.05
ad2=.05/2
CV=qnorm(1-ad2)      ##CV= z subscript alpha/2 (.05/2)
CV

SE =  coststdev/sqrt(48)          ##SE of xbar = std dev/sqrt(n)
SE

ME = CV*SE
ME

UL=samplemean+ME
LL=samplemean-ME
UL
LL

## we are 95% confident that the average cost of crude oil is between $92.31 and $98.88

##average cost of crude oil with 99% confidence level
alpha2=.01
ad22=.01/2
CV2=qnorm(1-ad22)      ##CV= z subscript alpha/2 (.05/2)
CV2

SE2 =  coststdev/sqrt(48)          ##SE of xbar = std dev/sqrt(n)
SE2

ME2 = CV2*SE2
ME2

UL2=samplemean+ME2
LL2=samplemean-ME2
UL2
LL2
## we are 99% confident that the average cost of crude oil is between $91.27 and $99.90

z= crudecost[,1]


## linear regression model with t and cost 

fit1=lm(y~x)
summary(fit1)

## PREDICTION how much will crude oil cost during time observation
## 41,42,43,44,45,46,47,48 for linear regression model 

testmo <- c(41,42,43,44,45,46,47,48)
newdata <- data.frame(x=testmo)
predict1 <- predict(fit1, newdata, se.fit=T)
predict1


## quadratic regression model with t squared and cost

xsquared=x^2
fit2=lm(y~x+xsquared)
summary(fit2)

## PREDICTION how much will crude oil cost during time observation 
## 41,42,43,44,45,46,47,48 for quadratic regression model 

testmo <- c(41,42,43,44,45,46,47,48)
testmo2 <- c(1681,1764,1849,1936,2025,2116,2209,2304)
newdata2 <- data.frame(x=testmo,xsquared=testmo2)
predict2 <- predict(fit2, newdata2, se.fit=T)
predict2

## test set 

crudecosttest=read.csv("CrudeCostTesting.csv") 
View(crudecosttest)

a = crudecosttest[,2]
a

b= crudecosttest[,3]
b


## Calculate MSE for both models, which one is a better fit

MSEfit1=mean((b-predict1$fit)^2)
MSEfit1

MSEfit2=mean((b-predict2$fit)^2)
MSEfit2

## Using quadratic regression model predict price of crude oil for 
## January 2014

testmo3 <- 49
testmo3s <- 2401
newdata3 <- data.frame(x=testmo3,xsquared=testmo3s)
predict3 <- predict(fit2, newdata3, se.fit=T)
predict3

pred.clim1 <- predict(fit2, newdata3, interval="confidence")
pred.clim1

pred.plim1 <- predict(fit2, newdata3, interval="prediction")
pred.plim1

pred2.clim1 <- predict(fit2, newdata3, interval="confidence", level = 0.99)
pred2.clim1

#datapath <- getwd()
##### part 1 #####
### importing data and computing returns ###

library(readxl) 
#loading data from excel
Stocks <- read_excel("Quoted Assignments/Stocks Eurostoxx50 daily.xlsx", 
                     sheet = "RI", col_types = c("date", "numeric", 
                                                 "numeric", "numeric", "numeric", 
                                                 "numeric", "numeric", "numeric", 
                                                 "numeric", "numeric", "numeric", "numeric", 
                                                 "numeric", "numeric", "numeric", 
                                                 "numeric", "numeric", "numeric", 
                                                 "numeric", "numeric", "numeric", 
                                                 "numeric", "numeric", "numeric", "numeric", 
                                                 "numeric", "numeric", "numeric", "numeric", 
                                                 "numeric", "numeric", "numeric", 
                                                 "numeric", "numeric", "numeric", "numeric", 
                                                 "numeric", "numeric", "numeric", "numeric", 
                                                 "numeric", "numeric", "numeric", 
                                                 "numeric", "numeric", "numeric", 
                                                 "numeric", "numeric", "numeric", 
                                                 "numeric", "numeric", "numeric", 
                                                 "numeric"))
View(Stocks)
library(xts) 
Level <- xts(Stocks[, -1], order.by = Stocks$...1)
#convert to xts without first column which is the data
Level.2015 <- Level['2015-01-01/']
str(Level.2015)
periodicity(Level.2015)
plot(Level.2015)
#taking the time series from 2015, daily frequency
daily.returns <- diff(Level.2015,lag=1, arithmetic = FALSE)-1
saveRDS(daily.returns, paste0("Quoted Assignments/daily.returns.2015.rds"))
plot(daily.returns, xlab='date starting in 2015', ylab='returns in decimals',main='daily returns')

### estimate single index model ###

Returns <- readRDS(paste0("Quoted Assignments/daily.returns.2015.rds"))
library(tidyr)
sim_a <- lm(Returns[,3:52] ~ Returns[,1])
#indicating the 50 Stocks as dependent variables and Return index as independent
sim <- coef(sim_a)
#sim contains beta0 and beta1 coefficients

### descriptive statistics ###

library(PerformanceAnalytics)
summary(sim[1,])
summary(sim[2,])
#transform to vector to run table.Stats
sim.1 <- as.vector(sim[1,])
sim.2 <- as.vector(sim[2,])
table.Stats(sim.1, digits=4)
table.Stats(sim.2, digits=4)

## histogram of betas ##

library(PerformanceAnalytics)
chart.Histogram(sim[2,], methods = c("add.density", "add.normal"), col = c("blue", "red", "green"), xlab='beta 1', main='histogram of beta 1')
chart.Histogram(sim[1,], methods = c("add.density", "add.normal"), col = c("blue", "red", "green"), xlab='intercept', main='histogram of intercept')
#histogram with plotted density and normal distribution

## histogram of R-squared ##
#computing in blocks seems not possible for the R-squared

#for-lus to compute a list of the 50 R-Squares
RSquared <- matrix(0,nrow=50,ncol=1)
counter <- 1
for (val in 3:52)
{
  sim_b <- lm(Returns[,val] ~ Returns[,1])
  RSquared[counter,1] <- summary(sim_b)$r.squared
  counter <- counter+1
}

hist(RSquared,main='histogram of R-Squares',xlab='R-square')
RSquared.vector <- as.vector(RSquared)
table.Stats(RSquared.vector, digits=4)

## computations for weekly and monthly data ##

#to count whole weeks we start at 5/01/2015 to 3/04/2020
Stocks.2015.week <- Stocks[3656:5025,2:53]
Stocks.2015.meanweek <- matrix(0,nrow=274,ncol=52)

# weekly data so computing means per week
day <- 1
for (val in 1:274)
{
comp.mean <- (Stocks.2015.week[day,]+Stocks.2015.week[day+1,]+Stocks.2015.week[day+2,]+Stocks.2015.week[day+3,]+Stocks.2015.week[day+4,])/5
comp.mean <- as.matrix(comp.mean)
Stocks.2015.meanweek[val,] <- comp.mean
day <- day+5
}

#monthly data so loop: month1=5weeks, month2=4weeks, month3=4weeks, and again
# transform means per month
month <- 1
Stocks.2015.meanmonth <- matrix(0,nrow=63,ncol=52)
for (val in seq(1,63,3))
{
  month.mean <- (Stocks.2015.meanweek[month,]+Stocks.2015.week[month+1,]+Stocks.2015.week[month+2,]+Stocks.2015.week[month+3,]+Stocks.2015.week[month+4,])/5
  month.mean <- as.matrix(month.mean)
  Stocks.2015.meanmonth[val,] <- month.mean
  month <- month+5
  
  month.mean <- (Stocks.2015.meanweek[month,]+Stocks.2015.week[month+1,]+Stocks.2015.week[month+2,]+Stocks.2015.week[month+3,])/4
  month.mean <- as.matrix(month.mean)
  Stocks.2015.meanmonth[val+1,] <- month.mean
  month <- month+4
  
  month.mean <- (Stocks.2015.meanweek[month,]+Stocks.2015.week[month+1,]+Stocks.2015.week[month+2,]+Stocks.2015.week[month+3,])/4
  month.mean <- as.matrix(month.mean)
  Stocks.2015.meanmonth[val+2,] <- month.mean
  month <- month+4
}

#making time-series
Week.dat <- xts(Stocks.2015.meanweek, order.by = seq.Date(as.Date("2015-01-05"), by = "week",length.out =274))
str(Week.dat)
periodicity(Week.dat)

Stocks.2015.meanmonth <- Stocks.2015.meanmonth[,-36]
#drop column because of NA objects
Month.dat <- xts(Stocks.2015.meanmonth, order.by = seq.Date(as.Date("2015-01-01"), by = "month",length.out =63))
str(Month.dat)
periodicity(Month.dat)

# time-series are loaded, the division in weeks and months is not exactly the same but the values are more or less equal to the "real" months
# the problem is here is that weeks are broken between two months so I assigned them myself

## returns for monthly and weekly data ##``

plot(Month.dat)
plot(Week.dat)

monthly.returns <- diff(Month.dat,lag=1, arithmetic = FALSE)-1
weekly.returns <- diff(Week.dat,lag=1, arithmetic = FALSE)-1
plot(monthly.returns, xlab='date starting in 2015', ylab='returns in decimals',main='montly returns')
plot(weekly.returns, xlab='date starting in 2015', ylab='returns in decimals',main='weekly returns')


#save returns
saveRDS(monthly.returns, paste0("Quoted Assignments/monthly.returns.2015.rds"))
saveRDS(weekly.returns, paste0("Quoted Assignments/weekly.returns.2015.rds"))

## regression ##

Returns.month <- readRDS(paste0("Quoted Assignments/monthly.returns.2015.rds"))
Returns.week <- readRDS(paste0("Quoted Assignments/weekly.returns.2015.rds"))

library(tidyr)
sim_m <- lm(Returns.month[,3:51] ~ Returns.month[,1])
sim_w <- lm(Returns.week[,3:52] ~ Returns.week[,1])

coef.m <- coef(sim_m)
coef.w <- coef(sim_w)

## descriptive statistics ##

library(PerformanceAnalytics)

#montly
sim.m.1 <- as.vector(coef.m[1,])
sim.m.2 <- as.vector(coef.m[2,])
table.Stats(sim.m.1, digits=4)
table.Stats(sim.m.2, digits=4)

chart.Histogram(coef.m[2,], methods = c("add.density", "add.normal"), col = c("blue", "red", "green"), xlab='beta 1', main='histogram of beta 1 montly data')
chart.Histogram(coef.m[1,], methods = c("add.density", "add.normal"), col = c("blue", "red", "green"), xlab='intercept', main='histogram of intercept montly data')

#weekly
sim.w.1 <- as.vector(coef.w[1,])
sim.w.2 <- as.vector(coef.w[2,])
table.Stats(sim.w.1, digits=4)
table.Stats(sim.w.2, digits=4)

chart.Histogram(coef.w[2,], methods = c("add.density", "add.normal"), col = c("blue", "red", "green"), xlab='beta 1', main='histogram of beta 1 weekly data')
chart.Histogram(coef.w[1,], methods = c("add.density", "add.normal"), col = c("blue", "red", "green"), xlab='intercept', main='histogram of intercept weekly data')

## R-Squares for weekly and monthly

#for-lus to compute a list of the 50 R-Squares
RSquared.month <- matrix(0,nrow=49,ncol=1)
counter <- 1
for (val in 3:51)
{
  sim_b.m <- lm(Returns.month[,val] ~ Returns.month[,1])
  RSquared.month[counter,1] <- summary(sim_b.m)$r.squared
  counter <- counter+1
}

hist(RSquared.month,main='histogram of R-Squares for monthly data',xlab='R-square')
RSquared.vector.m <- as.vector(RSquared.week)
table.Stats(RSquared.vector.m, digits=4)

RSquared.week <- matrix(0,nrow=50,ncol=1)
counter <- 1
for (val in 3:52)
{
  sim_b.w <- lm(Returns.week[,val] ~ Returns.week[,1])
  RSquared.week[counter,1] <- summary(sim_b.w)$r.squared
  counter <- counter+1
}

hist(RSquared.week,main='histogram of R-Squares for weekly data',xlab='R-square')
RSquared.vector.w <- as.vector(RSquared.week)
table.Stats(RSquared.vector.w, digits=4)

##### part 2 #####

# question 1: coefficients from 2018 for weekly returns
library(tidyr)
Returns.week.2018.1 <- Returns.week[,3:52]
Returns.week.2018.2 <- Returns.week[,1]
sim_2018 <- lm(Returns.week.2018.1['2018/'] ~ Returns.week.2018.2['2018/'])
coef.2018 <- coef(sim_2018)

# question 2: alternative beta estimators

## coefficients 2016-2017 for weekly returns ##

Returns.part2 <- diff(Level,lag=1, arithmetic = FALSE)-1
saveRDS(Returns.part2, paste0("Quoted Assignments/returns.part2.rds"))
Returns2 <- readRDS(paste0("Quoted Assignments/returns.part2.rds"))

Returns.1617.1 <- Returns2[,3:52]
Returns.1617.2 <- Returns2[,1]
sim_1617 <- lm(Returns.1617.1['2016/2017'] ~ Returns.1617.2['2016/2017'])
coef.1617 <- coef(sim_1617)

Returns.1415.1 <- Returns2[,3:52]
Returns.1415.1[,34] <- 0
#column is full of NA's so insert 0, S34 cannot be estimated nor ommited because the coefficients won't match anymore in that case
Returns.1415.2 <- Returns2[,1]
sim_1415 <- lm(Returns.1415.1['2014/2015'] ~ Returns.1415.2['2014/2015'])
coef.1415 <- coef(sim_1415)

## explain betas 2016-2017 ##

#dependent variable is coefficient 2016-2017, independent value is coefficient 2014-2015
#y.values <- coef.1617[2,]
#x.value <- coef.1415[2,]

# Blume Betas
sim_explain <- lm(coef.1617[2,] ~ coef.1415[2,])
coef.explain <- coef(sim_explain)
coef(sim_explain)
#take the values to compute beta 2018-2020

coef.Blume <- matrix(0,2,50)
coef.Blume[1,] <- 0.6393447 
coef.Blume[2,] <- 0.3409070*coef.1415[2,]

## Merrill-Lynch approach ##
coef.ML <- matrix(0,2,50)
coef.ML[1,] <- 1/3
coef.ML[2,] <- 2/3*coef.1415[2,]

# question 3: plot three approaches
# as three approaches we have: coef.1617, coef.Blume and coef.ML
# correct numbers from task 1 are: coef.2018

attach(mtcars)
par(mfrow=c(3,1))
plot.new()
plot(coef.1617[2,],coef.2018[2,], col="blue", pch="o",xlab="coefficients 3 approaches", ylab="real coefficients", main="plot of 3 approaches",xlim=c(0,2),ylim=c(0,2))
points(coef.Blume[2,], coef.2018[2,], col="red", pch="*")
points(coef.ML[2,], coef.2018[2,], col="green",pch="+")
lines(0:2,0:2,type="l")



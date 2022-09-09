
library("rstan")

library(rethinking)


data("WaffleDivorce")
waffle_data <- WaffleDivorce

#standardizing the predictors
waffle_data$MedianAgeMarriage.standardized <- (waffle_data$MedianAgeMarriage-mean(waffle_data$MedianAgeMarriage))/sd(waffle_data$MedianAgeMarriage)
waffle_data$Marriage.Standardized <- (waffle_data$Marriage-mean(waffle_data$Marriage))/sd(waffle_data$Marriage)

#Create Waffle houses per million
waffle_data$WHPerMillion <- waffle_data$WaffleHouses/waffle_data$Population

# Write to excel for other visualizations on tableau
library(xlsx)
write.xlsx(data, file = "waffles and divorce.xlsx",
           sheetName = "Waffles", append = FALSE)
#divorce rate on waffle houses
m5.0 <- map(
  alist(
    Divorce ~ dnorm(mu, sigma),
    mu <- a + bA*WHPerMillion ,
    a ~ dnorm(10,10),
    bA ~ dnorm(0,1),
    sigma ~ dunif(0,10)), data = waffle_data)
m5.0
plot(precis(m5.0))

Waffle.seq <- seq(from=0, to=50, length.out=100)
mu <- link(m5.0, data = data.frame(WHPerMillion=Waffle.seq))
mu.PI <- apply(mu, 2, PI)

plot(Divorce ~ WHPerMillion, 
     data = waffle_data, 
     col=col.alpha("navy blue",1),
     las=1,
     main = "Divorce and Waffle Houses
    by State")

abline(m5.0)
shade( mu.PI , Waffle.seq )

#For states that have a non zero number of waffle houses
m5.1 <- map(
  alist(
    Divorce ~ dnorm(mu, sigma),
    mu <- a + bA*WHPerMillion ,
    a ~ dnorm(10,10),
    bA ~ dnorm(0,1),
    sigma ~ dunif(0,10)), data = waffle_data[which(waffle_data$WHPerMillion>0),])
m5.1

Waffle.seq <- seq(from=0, to=50, length.out=100)
mu <- link(m5.1, data = data.frame(WHPerMillion=Waffle.seq))
mu.PI <- apply(mu, 2, PI)

plot(Divorce ~ WHPerMillion, 
     data = data[which(data$WHPerMillion>0),], 
     col=col.alpha("navy blue",1),
     xlab= "Waffle Houses per Million",
     main = "Divorce and Waffle Houses (>0)
    by State")
abline(m5.1)
shade( mu.PI , Waffle.seq )


# Fitting the model - age of marriage and divorce
m5.2 <- map(
  alist(
    Divorce ~ dnorm(mu, sigma),
    mu <- a + bA*MedianAgeMarriage.standardized ,
    a ~ dnorm(10,10),
    bA ~ dnorm(0,1),
    sigma ~ dunif(0,10)), data = waffle_data)
m5.2

MAM.seq <- seq(from=-3, to=3.5, length.out=10)
mu <- link(m5.2, data = data.frame(MedianAgeMarriage.standardized=MAM.seq))
mu.PI <- apply(mu, 2, PI)

plot(Divorce ~ MedianAgeMarriage.standardized, 
     data = waffle_data, 
     col=col.alpha("navy blue",1),
     main= "Divorce and Median Age at Marriage by State")
abline(m5.2)
abline(v=0,col=col.alpha("black", 0.5),lty=4)
shade( mu.PI , MAM.seq )

title(main = "Divorce and Median Age at Marriage by State")

# Fitting the model - marriage and divorce  - linearly 
m5.3 <- map(
  alist(
    Divorce ~ dnorm(mu, sigma),
    mu <- a + bR*Marriage.Standardized,
    a ~ dnorm(10,10),
    bR ~ dnorm(0,1),
    sigma ~ dunif(0,10)
  ), data = waffle_data
)

Marriage.standardized.seq <- seq(from=-2, to=3, length.out=10)
mu <- link(m5.3, data = data.frame(Marriage.Standardized=Marriage.standardized.seq))
mu.PI <- apply(mu, 2, PI)

plot(Divorce ~ Marriage.Standardized, 
     data = waffle_data, 
     col=col.alpha("navy blue",1),
     las = 1,
     main = "Divorce and Marriage Rate by State")
abline(m5.3)
abline(v=0,col=col.alpha("black", 0.5),lty=4)
shade( mu.PI , Marriage.standardized.seq )

### Multivariate model
# Combining for the multivariate model. This shows that there is little predicative power in knowing 
# the marriage rate for a state once we know the median age at marriage
m5.4 <- map(
  alist(
    Divorce ~ dnorm(mu, sigma),
    mu <- a + bR*Marriage.Standardized + bA*MedianAgeMarriage.standardized,
    a ~ dnorm(10,10),
    bR ~ dnorm(0,1),
    bA ~ dnorm(0,1),
    sigma ~ dunif(0,10)
  ), data = waffle_data
)
m5.4
plot(precis(m5.4))

mean(waffle_data$WHPerMillion)

m5.40 <- map(
  alist(
    Divorce ~ dnorm(mu, sigma),
    mu <- a + bR*Marriage.Standardized + bA*MedianAgeMarriage.standardized + bC*WHPerMillion,
    a ~ dnorm(10,10),
    bR ~ dnorm(0,1),
    bA ~ dnorm(0,1),
    bC ~ dnorm(5,10),
    sigma ~ dunif(0,10)
  ), data = waffle_data
)
plot(precis(m5.40))


### Plotting the predictor residuals. 
# In the model of divorce rate we have two predictor variables 1. marriage rate and 
# 2. median age at marriage. To find the predictor residuals, we must use the other predictor for modelling the other variable. 
# Here we predict the residuals for the standardized marriage rate using the standardized median age at marriage and show the difference
# in the predicted vs the observed. 
m5.5 <- map(
  alist(
    Marriage.Standardized ~ dnorm(mu, sigma),
    mu <- a + b*MedianAgeMarriage.standardized ,
    a ~ dnorm(0,10),
    b ~ dnorm(0,1),
    sigma ~ dunif(0,10)
  ), data = waffle_data)
#find expected value at maximum a posteriori probability estimate (MAP). Equals the mode of the posterior distribution 
mu <- coef(m5.5)['a'] + coef(m5.5)['b']*waffle_data$MedianAgeMarriage.standardized

#compute residuals for each State - observed minus the predicted
m.resid <- waffle_data$Marriage.Standardized - mu

#plotting the  residuals
plot(Marriage.Standardized ~ MedianAgeMarriage.standardized, 
     waffle_data, 
     col=col.alpha("navy blue",1),
     las = 1,
     main = "Standardized Marriage Rate and 
  Median Age at Marriage - Residuals")
abline(m5.5)

#looping through states to create the linear distance between predicted and observed outcomes.
for (i in 1:length(m.resid)){
  x <- waffle_data$MedianAgeMarriage.standardized[i] #location of line segment on x axis
  y <- waffle_data$Marriage.Standardized[i] #observed endpoint 
  lines(c(x,x), c(mu[i],y), lwd=0.5, col=col.desat("black",0.5))
}



# Use the residuals from above on the x axis to plot  against divorce rate. Displays the linear relationship between
# divorce and marriage rate after statistically controlling for median age of marriage. 
# Visually shows no relationship between divorce and marriage rate.
m5.6 <- map(
  alist(
    Divorce ~ dnorm(mu, sigma),
    mu <- a + b*m.resid ,
    a ~ dnorm(0,10),
    b ~ dnorm(0,1),
    sigma ~ dunif(0,10)
  ), data = waffle_data)
plot(precis(m5.6))

m.resid.seq <- seq(from=-2, to=2, length.out=10)
mu <- link(m5.6, data = data.frame(m.resid=m.resid.seq))
mu.PI <- apply(mu, 2, PI)

plot(Divorce ~ m.resid, 
     waffle_data, 
     col=col.alpha("navy blue",1), 
     las= 1, 
     xlab = "Marriage Rate Residuals", 
     main = "Divorce and Marriage Rate Residuals")
abline(m5.6)
abline(v=0,col=col.alpha("black", 0.5),lty=4)
shade( mu.PI , m.resid.seq )


# Computing the predictor residuals for median age of marriage so that we may see, after controlling for marriage rate,
# how does median age at marriage truly affect divorce rate. Here we show that states (holding constant the marriage rate) with an older median marriage age
# have a lower divorce rate than states with a younger median marriage age.

m5.7 <- map(
  alist(
    MedianAgeMarriage.standardized ~ dnorm(mu, sigma),
    mu <- a + b*Marriage.Standardized ,
    a ~ dnorm(0,10),
    b ~ dnorm(0,1),
    sigma ~ dunif(0,10)
  ), data = waffle_data)




# Compute median age residuals for each State

mu <- coef(m5.7)['a'] + coef(m5.7)['b']*waffle_data$Marriage.Standardized
age.resid <- waffle_data$MedianAgeMarriage.standardized - mu

# Plotting the residuals of median age for each state, controlling for marriage rate, 
m5.8 <- map(
  alist(
    Divorce ~ dnorm(mu, sigma),
    mu <- a + b*age.resid ,
    a ~ dnorm(0,10),
    b ~ dnorm(0,1),
    sigma ~ dunif(0,10)
  ), data = waffle_data)
plot(precis(m5.8))

age.resid.seq <- seq(from=-2, to=3, length.out=10)
mu <- link(m5.8, data = data.frame(age.resid=age.resid.seq))
mu.PI <- apply(mu, 2, PI)

plot(Divorce ~ age.resid, 
     waffle_data, 
     col=col.alpha("navy blue",1), 
     las = 1, 
     xlab = "Median Age at Marriage Resdiuals", 
     main = "Divorce and Median Age at Marriage Residuals")
abline(m5.8)
abline(v=0,col=col.alpha("black", 0.5),lty=4)
shade( mu.PI , age.resid.seq )



### Counterfactual plots. 
# These inferential plots are useful in seeing how model predictions change when 
# we change one predictor at a time. Can be produced for any value of the predictor variables (even that which is unobserved)
# Extremely useful in determining model implications.

#Here I compute new counterfactual data. Holding median age at marriage constant (at mean value) and allowing marriage to vary.
A.average <- mean(waffle_data$MedianAgeMarriage.standardized)
R.seq <- seq(from=-3, to=3, length.out=30)
predicted.data <- data.frame(
  Marriage.Standardized=R.seq,
  MedianAgeMarriage.standardized=A.average
)

# Compute counterfactual mean divorce (mu)
mu <- link(m5.4, data = predicted.data)
mu.mean <- apply(mu,2,mean)
mu.PI <- apply(mu, 2, PI)

#simulate counterfactual divorce outcomes 
R.sim <- sim(m5.4, data=predicted.data, n=1e4)

R.PI <- apply(R.sim,2, PI)

plot(Divorce ~ Marriage.Standardized, 
     data=waffle_data, 
     type="n",
     las=1)
mtext("medianAgeMarriage.Standardized=0")
lines(R.seq,mu.mean)
shade(mu.PI,R.seq) #89% percentile intervals (narrow/dark)
shade(R.PI, R.seq) #89% prediction interval (wide/light)
abline(v=0,col=col.alpha("black", 0.5),lty=4)

#Doing the same thing as above but holding marriage rate constant and allowing age to vary. 

R.average <-mean(waffle_data$Marriage.Standardized)
A.seq <- seq(from=-3, to=3.5, length.out=30)
predicted.data2 <- data.frame(
  Marriage.Standardized=R.average,
  MedianAgeMarriage.standardized=A.seq
)

mu <- link(m5.4, data=predicted.data2)
mu.mean <- apply(mu,2,mean)
mu.PI <- apply(mu,2,PI)

A.sim <- sim(m5.4, data=predicted.data2, n=1e4)
A.PI <- apply(A.sim,2,PI)

plot(Divorce ~ MedianAgeMarriage.standardized, 
     data=waffle_data, 
     type = "n",
     las=1)
mtext("Marriage.Standardized=0")
lines(A.seq,mu.mean)
shade(mu.PI,A.seq) #89% confidence intervals of mean (narrow/dark)
shade(A.PI, A.seq) #89% prediction interval (wide/light)

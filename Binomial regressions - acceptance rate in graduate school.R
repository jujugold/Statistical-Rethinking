library(rethinking)
library("rstan")
install_cmdstan()
admissions_data <- UCBadmit

# Here we have the results of graduate schools 
admissions_data$male <- ifelse(admissions_data$applicant.gender=="male",1,0)
m10.6 <- map(
  alist(
    admit ~ dbinom(applications, p),
    logit(p) <- a + bM*male,
    a ~ dnorm(0,10),
    bM ~ dnorm(0,10)
  ), 
  data = admissions_data
)

m10.7 <- map(
  alist(
    admit ~ dbinom(applications, p),
    logit(p) <- a,
    a ~ dnorm(0,10)
  ),
  data =admissions_data
)

# Compare the models with WAIC. Shows that gender matters a lot. 
# The male predictor variable improves the deviance found out of sample.
compare(m10.6,m10.7)

# Computing the relative differences in admissions odds. 
# Unnecessary because we want absolute difference in admission probability 
precis(m10.6)
exp(0.61)

# Absolute scale difference. 
post <- extract.samples(m10.6)
probability.admittance.male <- logistic(post$a + post$bM)
probability.admittance.female <- logistic(post$a)
difference.admittance <- probability.admitance.male - probability.admitance.female
quantile(difference.admittance, c(0.025,0.5,0.975)) # shows that men have a a 14% higher chance of admission

#Graph the predicted posterior distribution of model vs the observed distribution
postcheck(m10.6,n=1e4, col=col.alpha("navy blue",1), 
          las= 1, col.lab = "white")
title(xlab = "Admission Case (Odd # is male, Even # is female)",
      ylab = "Proportion Admitted",
      line = 1.6)
title(main = "Binomial model using male as only predictor",
      line = 1)
# Drawing lines connecting points from each department
for (i in 1:6){
  x <- 1 + 2*(i-1)
  y1 <- admissions_data$admit[x]/admissions_data$applications[x]
  y2 <- admissions_data$admit[x+1]/admissions_data$applications[x+1]
  lines(c(x,x+1), c(y1,y2), col=col.alpha("navy blue",.8), lwd = 2)
  text(x + 0.5, (y1+y2)/2 + 0.05, admissions_data$dept[x], cex = 0.8, col=col.alpha("navy blue",1))
}



# However this model isn't strong as it observes the average probabilities of admissions for 
# both genders across all departments.
# It doesn't recognize the significant differences between departments. 
# In this next model lets estimate the difference in probabilities of admissions 
# for both males and females within each department.

admissions_data$dept_ID <- coerce_index(admissions_data$dept) # make index of each department

# First, a model focusing on just department differences:
m10.8 <- map(
  alist(
    admit ~ dbinom(applications, p),
    logit(p) <- a[dept_ID],
    a[dept_ID] ~ dnorm(0,10)
  ), data = admissions_data
)

# Second, a model with both department and gender
m10.9 <- map(
  alist(
    admit ~ dbinom(applications, p),
    logit(p) <- a[dept_ID] + bM*male,
    a[dept_ID] ~ dnorm(0,10),
    bM ~ dnorm(0,10)
  ), data = admissions_data
)
# GLM just for fun
m10.9glm <- glm(cbind(admit,reject) ~ male + dept, data = admissions_data, family = binomial)


summary(m10.9glm)
compare(m10.6,m10.7,m10.8,m10.9) # Here we use the WAIC to compare all the models we made. 
# The most accurate model fitting to the data is  10.8, which makes us believe that 
# the strongest factor determining admissions is the department someone applies to, regardless of gender. 

# However,the model that includes gender and department is also an accurate model given how small the difference in WAIC is.
precis(m10.9, depth = 2) 
exp(-1.26) # comparing within departments the proportional odds, a male has 90% the chance of admission that a female has.

# Checking the quadratic approximation to make sure that department intercepts don't cause problems. 
# Here I am using Markov Chain Monte Carlo (MCMC) built with the map2stan function in the rethinking package
m10.9mcmc <- map2stan(m10.9, chains = 2,iter = 2000, warmup = 500)

precis(m10.9mcmc, depth = 2) # The results of the quadratic fit is good - we are given the same estimates as m10.9 


# We can plot the posterior validation model to see how well the model predicts.
# The model fits well according to the predicted fits and the observed data
postcheck(m10.9,n=1e4, col=col.alpha("navy blue",1), 
          las= 1,
          col.lab = "white")
title(xlab = "Admission Case (Odd # is male, Even # is female)",
      ylab = "Proportion Admitted",
      line = 1.6)
title(main = "Binomial model, male & dept predictors",
      line = 1)

for (i in 1:6){
  x <- 1 + 2*(i-1)
  y1 <- admissions_data$admit[x]/admissions_data$applications[x]
  y2 <- admissions_data$admit[x+1]/admissions_data$applications[x+1]
  lines(c(x,x+1), c(y1,y2), col=col.alpha("navy blue",.8), lwd = 2)
  text(x + 0.5, (y1+y2)/2 + 0.05, admissions_data$dept[x], cex = 0.8, col=col.alpha("navy blue",1))
}

# The straight proportions of admittance
with(admissions_data,sum(admit[male=="0"])/sum(applications[male=="0"])) # Percentage of women admitted is 30.35%
with(admissions_data,sum(admit[male=="1"])/sum(applications[male=="1"])) # Percentage of men admitted is 44.5%

# The proportions for departments that are harder to get into 
with(admissions_data,sum(admit[male=="0" & dept_ID>=3])/sum(applications[male=="0"& dept_ID>=3])) #% of women admitted is 26.5%
with(admissions_data,sum(admit[male=="1" & dept_ID>=3])/sum(applications[male=="1"& dept_ID>=3])) #% of men admitted 25.5%

# The proportions for departments that are easier to get into 
with(admissions_data,sum(admit[male=="0" & dept_ID<3])/sum(applications[male=="0"& dept_ID<3])) #% of women admitted is 79.7%
with(admissions_data,sum(admit[male=="1" & dept_ID<3])/sum(applications[male=="1"& dept_ID<3])) #% of men admitted is 62.5%
with(admissions_data,sum(applications[male=="0"& dept_ID<5]))

# We find that, although men have a higher admittance rate across all departments (44.5% compared to 30.4%), 
# it is clear that this is driven by the fact that females mainly are applying to the harder programs. 
# When broken down, females have a higher likelihood of getting accepted to the easier programs (perhaps because of lack of female applicants to begin with).
# But they have the same likelihood of getting accepted to programs that are relatively harder.
# This is an example of the Simpson's Paradox. A conclusion is reversed when controlling for another important variable.
# Here, individual department acceptance rates.





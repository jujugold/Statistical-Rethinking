# Statistical-Rethinking-Notes
Selected chapters and replication from Richard McElreath's book "Statistical Rethinking"

Here I review and appreciate the author's work, and add some of my own insights.


## Waffles and Divorce R file 
Here I use a simple multivariate model to disprove the theory that the number of waffle houses in a state has any meaningful impact on that state's divorce rate. Instead it is a feature of the younge marriage age in southern states. Marriage rate could also be considered a factor, but I show with models and two different types of plots, predictor residuals plots and counterfactual plots, that marriage age is chiefly responsible.

## Binomial regressions
Here I generate two different binomial regressions to determine if there is bias in the graduate admissions of UC Berkely in 1973. This example is famous for being a known case of the Simpsons's Paradox. Accross departments, it appears that men are accepted at a higher rate. But after controlling for both gender and the within department variance of acceptance rates, this discrepancy is removed.

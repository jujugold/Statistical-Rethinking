# Statistical-Rethinking-Notes
Selected chapters and replication from Richard McElreath's book "Statistical Rethinking"

Here I review and appreciate the author's work, and add some of my own insights and thought.


## Waffles and Divorce R file 
Here I use a simple multivariate model to disprove the theory that the number of waffle houses in a state has any meaningful impact on that state's divorce rate. Instead it is a feature of the younge marriage age in southern states. Marriage rate could also be considered a factor, but I show with models and two different types of plots, predictor residuals plots and counterfactual plots, that marriage age is chiefly responsible.

![Divorce and Waffle Houses by State_page-0001](https://user-images.githubusercontent.com/77739272/191348386-2d8e75d6-ff50-4103-b355-ff1a30b13e15.jpg)
![Divorce and marriage rate residuals](https://user-images.githubusercontent.com/77739272/191352276-f168b156-cf82-4403-9f3b-da419cbc2a43.png)
![Divorce and median age at marriage residuals](https://user-images.githubusercontent.com/77739272/191352308-fc69984b-735e-4638-905a-5cefb5e56534.png)

## Binomial regressions
Here I generate two different binomial regressions to determine if there is bias in the graduate admissions of UC Berkely in 1973. This example is famous for being a known case of the Simpsons's Paradox. Accross departments, it appears that men are accepted at a higher rate. But after controlling for both gender and the within department variance of acceptance rates, this discrepancy is removed.
![Binomial model using only male as predictor](https://user-images.githubusercontent.com/77739272/191349070-e09e62c2-f4ed-418b-ac2a-21091d6ad593.png)
![Posterior validation check - percent admitted on male and department](https://user-images.githubusercontent.com/77739272/191347994-c0f9db28-f480-4333-aac7-2698fdf91cb0.png)

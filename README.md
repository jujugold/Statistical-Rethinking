# Statistical-Rethinking-Notes
Selected chapters and replication from Richard McElreath's book "Statistical Rethinking"

Here I review and appreciate the author's work, and add some of my own insights and thought.


## Waffles and Divorce R file 
Here I use a simple multivariate model to disprove the theory that the number of waffle houses in a state has any meaningful impact on that state's divorce rate. Instead it is a feature of the younge marriage age in southern states. Marriage rate could also be considered a factor, but I show with models and two different types of plots, predictor residuals plots and counterfactual plots, that marriage age is chiefly responsible. I generate many graphs in the R code, but only a few are displayed here.

![Divorce and Waffle Houses by State_page-0001](https://user-images.githubusercontent.com/77739272/191348386-2d8e75d6-ff50-4103-b355-ff1a30b13e15.jpg)
![Predicted Marriage Rate residuals from age](https://user-images.githubusercontent.com/77739272/191355203-1f37a0a1-7623-400c-95a7-df2b1379ec85.png)

Each line segment tracks a residual of marriage rate - the distance between the observed marriage rate and the expected marriage rate, with the prediction coming solely from median age at marriage. Visually demonstrates a relationship between the two variables. States that lie above the regression line have higher rates of marriage as predicted by median age at marriage. Below the line states have lower rates than expected.

![Divorce and marriage rate residuals](https://user-images.githubusercontent.com/77739272/191352276-f168b156-cf82-4403-9f3b-da419cbc2a43.png)

The difference in the observed vs predicted marriage rate as assumed by median age marriage. Similar to the prior graph; however, this visually shows no relationship between marriage rate and divorce.

![Divorce and median age at marriage residuals](https://user-images.githubusercontent.com/77739272/191352308-fc69984b-735e-4638-905a-5cefb5e56534.png)

A counterfactual plot that shows a significant relationship between divorce and median age at marriage.

## Binomial regressions
Here I generate two different binomial regressions to determine if there is bias in the graduate admissions of UC Berkely in 1973. This example is famous for being a known case of the Simpsons's Paradox. Accross departments, the binomial model including just gender as the predictor of admission rate generates a prediction that men are accepted at a higher rate. But after using both gender and the within department variance of acceptance rates, the prediction is much more accurate (the actual observations in blue falls within the boundaries of the predicted admissions rates in black).

![Binomial model using only male as predictor](https://user-images.githubusercontent.com/77739272/191349070-e09e62c2-f4ed-418b-ac2a-21091d6ad593.png)
![Posterior validation check - percent admitted on male and department](https://user-images.githubusercontent.com/77739272/191347994-c0f9db28-f480-4333-aac7-2698fdf91cb0.png)

## Lesson 3 - Thursday 9/15/22

### A. Ambiguity in Assignment Given Last Week 

* Before you can calculate a confidence interval, it is important to calculate the correct standard error.
* Be sure to review the correct formula for the standard error for the problem you are working on.

### B. Consistency

We now consider a situation where a small sample estimator is biased but the bias diminishes as the sample size grows larger. Based on the NCVS, we would expect that rural households have a lower victimization risk. So, let's suppose we have a (1M) population of urban and rural households and a disparity in victimization risk that approximates what we see in the NCVS. Here is the R code:

```R
options(scipen=100000)
set.seed(394320258)
urban <- rbinom(n=1000000,size=1,prob=0.65)
z <- runif(n=1000000,min=0,max=1)
u <- log(z/(1-z))
ystar <- 0.5*urban+u
y <- rep(NA,1000000)
y[ystar>0] <- 1
y[ystar<0] <- 0
pop.df <- data.frame(y,urban)
table(pop.df$y,pop.df$urban)
pop.lr.model <- glm(y~1+urban,data=pop.df,family="binomial")
summary(pop.lr.model)
popoddsratio <- exp(coef(pop.lr.model)[2])
popoddsratio

# now, let's sample from the population [nsamples] times
# using a relatively small sample of size N = 50
# we will call this the small sample size

nsamples <- 10000
sampsize <- 50

urban.coef.small <- vector()

for(i in 1:nsamples)
  {
   sdf <- pop.df[sample(nrow(pop.df),size=sampsize,replace=T), ]
   small.logit <- glm(y~1+urban,data=sdf,family="binomial")
   urban.coef.small[i] <- exp(coef(small.logit)[2])
   }

# let's look at the results for the last sample

summary(small.logit)

# calculate results

mean(urban.coef.small)

# next, we use a sample that is 5x larger (N = 250)
# we will call this a medium sample size

nsamples <- 10000
sampsize <- 250

urban.coef.medium <- vector()

for(i in 1:nsamples)
  {
   sdf <- pop.df[sample(nrow(pop.df),size=sampsize,replace=T), ]
   medium.logit <- glm(y~1+urban,data=sdf,family="binomial")
   urban.coef.medium[i] <- exp(coef(medium.logit)[2])
   }

# let's look at the results for the last sample

summary(medium.logit)

# calculate results

mean(urban.coef.medium)

# now we will use a large sample size of N = 1000

nsamples <- 10000
sampsize <- 1000

urban.coef.large <- vector()

for(i in 1:nsamples)
  {
   sdf <- pop.df[sample(nrow(pop.df),size=sampsize,replace=T), ]
   large.logit <- glm(y~1+urban,data=sdf,family="binomial")
   urban.coef.large[i] <- exp(coef(large.logit)[2])
   }

# let's look at the results for the last sample

summary(large.logit)

# calculate results

mean(urban.coef.large)
```

and here is the output:

```Rout
> options(scipen=100000)
> set.seed(394320258)
> urban <- rbinom(n=1000000,size=1,prob=0.65)
> z <- runif(n=1000000,min=0,max=1)
> u <- log(z/(1-z))
> ystar <- 0.5*urban+u
> y <- rep(NA,1000000)
> y[ystar>0] <- 1
> y[ystar<0] <- 0
> pop.df <- data.frame(y,urban)
> table(pop.df$y,pop.df$urban)
   
         0      1
  0 174762 245504
  1 175537 404197
> pop.lr.model <- glm(y~1+urban,data=pop.df,family="binomial")
> summary(pop.lr.model)

Call:
glm(formula = y ~ 1 + urban, family = "binomial", data = pop.df)

Deviance Residuals: 
    Min       1Q   Median       3Q      Max  
-1.3951  -1.1793   0.9743   0.9743   1.1755  

Coefficients:
            Estimate Std. Error z value            Pr(>|z|)
(Intercept) 0.004425   0.003379   1.309                0.19
urban       0.494164   0.004239 116.585 <0.0000000000000002
               
(Intercept)    
urban       ***
---
Signif. codes:  
0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

(Dispersion parameter for binomial family taken to be 1)

    Null deviance: 1360755  on 999999  degrees of freedom
Residual deviance: 1347136  on 999998  degrees of freedom
AIC: 1347140

Number of Fisher Scoring iterations: 4

> popoddsratio <- exp(coef(pop.lr.model)[2])
> popoddsratio
   urban 
1.639128 
> 
> # now, let's sample from the population [nsamples] times
> # using a relatively small sample of size N = 50
> # we will call this the small sample size
> 
> nsamples <- 10000
> sampsize <- 50
> 
> urban.coef.small <- vector()
> 
> for(i in 1:nsamples)
+   {
+    sdf <- pop.df[sample(nrow(pop.df),size=sampsize,replace=T), ]
+    small.logit <- glm(y~1+urban,data=sdf,family="binomial")
+    urban.coef.small[i] <- exp(coef(small.logit)[2])
+    }
> 
> # let's look at the results for the last sample
> 
> summary(small.logit)

Call:
glm(formula = y ~ 1 + urban, family = "binomial", data = sdf)

Deviance Residuals: 
   Min      1Q  Median      3Q     Max  
-1.302  -1.302   1.058   1.058   1.354  

Coefficients:
            Estimate Std. Error z value Pr(>|z|)
(Intercept)  -0.4055     0.5270  -0.769    0.442
urban         0.6931     0.6280   1.104    0.270

(Dispersion parameter for binomial family taken to be 1)

    Null deviance: 69.235  on 49  degrees of freedom
Residual deviance: 67.994  on 48  degrees of freedom
AIC: 71.994

Number of Fisher Scoring iterations: 4

> 
> # calculate results
> 
> mean(urban.coef.small)
[1] 2.068656
> 
> # next, we use a sample that is 5x larger (N = 250)
> # we will call this a medium sample size
> 
> nsamples <- 10000
> sampsize <- 250
> 
> urban.coef.medium <- vector()
> 
> for(i in 1:nsamples)
+   {
+    sdf <- pop.df[sample(nrow(pop.df),size=sampsize,replace=T), ]
+    medium.logit <- glm(y~1+urban,data=sdf,family="binomial")
+    urban.coef.medium[i] <- exp(coef(medium.logit)[2])
+    }
> 
> # let's look at the results for the last sample
> 
> summary(medium.logit)

Call:
glm(formula = y ~ 1 + urban, family = "binomial", data = sdf)

Deviance Residuals: 
    Min       1Q   Median       3Q      Max  
-1.6325  -1.1097   0.7826   0.7826   1.2466  

Coefficients:
            Estimate Std. Error z value  Pr(>|z|)    
(Intercept)  -0.1613     0.2151  -0.750     0.453    
urban         1.1876     0.2790   4.256 0.0000208 ***
---
Signif. codes:  
0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

(Dispersion parameter for binomial family taken to be 1)

    Null deviance: 326.71  on 249  degrees of freedom
Residual deviance: 308.15  on 248  degrees of freedom
AIC: 312.15

Number of Fisher Scoring iterations: 4

> 
> # calculate results
> 
> mean(urban.coef.medium)
[1] 1.705688
> 
> # now we will use a large sample size of N = 1000
> 
> nsamples <- 10000
> sampsize <- 1000
> 
> urban.coef.large <- vector()
> 
> for(i in 1:nsamples)
+   {
+    sdf <- pop.df[sample(nrow(pop.df),size=sampsize,replace=T), ]
+    large.logit <- glm(y~1+urban,data=sdf,family="binomial")
+    urban.coef.large[i] <- exp(coef(large.logit)[2])
+    }
> 
> # let's look at the results for the last sample
> 
> summary(large.logit)

Call:
glm(formula = y ~ 1 + urban, family = "binomial", data = sdf)

Deviance Residuals: 
    Min       1Q   Median       3Q      Max  
-1.3795  -1.2034   0.9879   0.9879   1.1517  

Coefficients:
            Estimate Std. Error z value Pr(>|z|)   
(Intercept)  0.06093    0.10081   0.604  0.54554   
urban        0.40264    0.13086   3.077  0.00209 **
---
Signif. codes:  
0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

(Dispersion parameter for binomial family taken to be 1)

    Null deviance: 1363.7  on 999  degrees of freedom
Residual deviance: 1354.2  on 998  degrees of freedom
AIC: 1358.2

Number of Fisher Scoring iterations: 4

> 
> # calculate results
> 
> mean(urban.coef.large)
[1] 1.655039
> 
```

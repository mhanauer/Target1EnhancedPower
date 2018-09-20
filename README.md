---
title: "Creating simulated random effects data for two and three level models"
output: html_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```
Library some packages
```{r}
library(lme4)
library(MASS)
```


Here is the model that I am trying to recreate

Level 1: There is the intercept that varies for each person over time.  Then there is the slope for time that varies for each person over time.  Finally there is the error term that is unique for each data point.

$$ Level~1:~~~{y_{ij} = \beta_{0j} + \beta_{1j}Time_{ij} + e_{ij}}~~~ (1.1)$$

Level 2 Intercept: Here the intercept is broken down into the constant plus the effect of the intervention, which is at level 2 in the intercept because it does not vary over time only by person and the error term which varies by person. 

$$ Level~2~Intercept:~~~{\beta_{0j} = \gamma_{00} + \gamma_{01}Intervention2_{j} + \gamma_{02}Intervention3_{j} + u_{0j}} ~~~ (1.2)$$


Then there is level the two slope which has the constant effect, plus the slope for the intervention for each person, plus a random error term that unique to each person.  

$$ Level~2~Slope~Time:~~~{\beta_{1j} = \gamma_{10} + \gamma_{11}Intervention2_{j} + \gamma_{12}Intervention3_{j} + u_{1j}} ~~~ (1.3)$$
Then we have the mixed model, which has all the components combined
$$Mixed~model: ~~~{y_{ij} =   (\gamma_{00}+ \gamma_{01}Intervention2_{j} + \gamma_{02}Intervention3_{j} + u_{0j}) + (\gamma_{10}}+\gamma_{11}*Intervention2_{j}+ \gamma_{11}*Intervention3_{j} +u_{1j})*Time_{ij} + e_{ij} $$

I am basing this example on the example below and extending it by adding an intervention variable: http://m-clark.github.io/docs/sem/latent-growth-curves.html

I am creating a data set with 500 total people across 4-time points (ranging from 0 to 3) totaling 2,000 data points.  
I then create the number of subjects, which are 500 people replicated four times each.

To add an intervention I create a treat variable which I then sample from 500 times and then replicate these values 4 times.

I am assuming I have an outcome that is normally distributed and in standard normal form.     

Then I am setting the intercept to .5, a slope for the variable time to .25, and a slope for the intervention variable to .25.

Then I am creating the random effects for the intercept and time, because each person gets a unique intercept and a unique slope for time.  

I am also creating a slope for the interaction effect between time and intervention, which is also .25

With .5 covariance, we are seeing about a 1 sd, which means that .25 is about a quarter of a standard deviation increase.
```{r}
powerMatt = function(){
library(lmerTest)
n = 90+78+68
timepoints = 2
time = timepoints-1
time = rep(0:time, times=n)
subject = rep(1:n, each=timepoints)
treat = c(0,1,2)
intervention = sample(treat, replace = TRUE, prob = c(.33, .33, .33), n)
intervention = rep(intervention, each = timepoints)
intervention2 = ifelse(intervention == 1, 1, 0)
intervention3 = ifelse(intervention == 2,1,0)

intercept = 0
slopeT = .25
slopeI2 = .25
slopeI3 = .25
slopeTI2 = .25
slopeTI3 = .25

randomEffectsCorr = matrix(c(.5,.2,.2, .5), ncol = 2)
randomEffectsCorr

randomEffects = mvrnonnorm(n, mu = c(0,0), Sigma = randomEffectsCorr, empirical = TRUE)
randomEffects = data.frame(randomEffects)
dim(randomEffects)
colnames(randomEffects) = c("Int", "SlopeT")
dim(randomEffects)

sigma = .05
y1 = (intercept + randomEffects$Int[subject])+(slopeT + randomEffects$SlopeT[subject])*time + slopeI2*intervention2 + slopeI3*intervention3 + slopeTI2*time*intervention2+ slopeTI3*time*intervention3+ rnorm(n*timepoints, mean = 0, sd = sigma)
d = data.frame(subject, time, intervention, y1)
sd(y1)

d$intervention = as.factor(d$intervention)
model1 = lmer(y1 ~ time*intervention + (1|subject), data = d)
model1 = summary(model1)
p_values =  model1$coefficients[,5]
p_values
}
```
Now run this function 10 time first to see if it works and try to see how many times the t-values is above 2
For RAS I want greater than two, because we are expecting a bigger increase.  If this fails can try it the other way
```{r}
reps = 100
p_values = replicate(reps, powerMatt()) 
p_values = as.data.frame(t(p_values)) 
p_values = as.data.frame(apply(p_values, 2, function(x){ifelse(x < .05, 1, 0)}))
p_values = as.data.frame(apply(p_values, 2, sum))
p_values = p_values/reps
colnames(p_values) = c("Power")
p_values
```




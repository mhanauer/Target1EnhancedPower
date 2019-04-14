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
n = 90+88+78
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
effect =  list(.1,.2,.3,.4,.5)
ran_int = rnorm(n = n, mean = .2, sd = .5)
sigma = .5
inter_out_2 = list()
inter_out_3 = list()
y = list()
subject_out = list()
time_out = list()
d_out = list()

for(i in 1:length(effect)){
y[[i]] = (intercept + ran_int[subject])+effect[[i]]*time + effect[[i]]*intervention2 + effect[[i]]*intervention3 + effect[[i]]*time*intervention2+ effect[[i]]*time*intervention3+ rnorm(n*timepoints, mean = 0, sd = sigma)
inter_out_2[[i]] = intervention2
inter_out_3[[i]] = intervention3
subject_out[[i]] = subject
time_out[[i]] = time
d_out[[i]] = data.frame(subject = subject_out[[i]], time = time_out[[i]], intervention2 = inter_out_2[[i]], intervention3 = inter_out_3[[i]], y = y[[i]])
}

model_out = list()
p_values_out = list()
for(i in 1:length(d_out)){
model_out[[i]] = lmer(y ~ time*intervention2 + time*intervention3 + (1|subject), data = d_out[[i]])
model_out[[i]] = summary(model_out[[i]])
p_values_out[[i]] =  model_out[[i]]$coefficients[,5]
p_values_out[[i]] = ifelse(p_values_out[[i]] < .05, 1, 0)
}
p_values_out
}

```
Now run this function 10 time first to see if it works and try to see how many times the t-values is above 2
For RAS I want greater than two, because we are expecting a bigger increase.  If this fails can try it the other way

The final data set needs to be each variable by
Intercept 1...length(effect)
Time 1..length(effect)
Intervention2 1..length(effect)
Intervention3 1...length(effect)
Intervnetion2*time 1:length(effect)
Intervention3*time 1:length(effect)

So stack each 30 on top of each other
```{r}
reps = 10
p_values = rep(powerMatt(), reps) 
p_unlist = unlist(p_values)
length(p_unlist)
power_matrix = matrix(p_unlist, ncol = 6*length(effect), nrow =length(p_unlist)/6*length(effect), byrow = TRUE)
power_matrix

# Figure out how to name them more efficently

p_values = as.data.frame(apply(p_values, 2, sum))
p_values = p_values/reps
colnames(p_values) = c("Power")
p_values
```
Need to make so we can loop over an effect size
```{r}


```




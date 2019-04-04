---
title: "Enhanced Results"
output:
  pdf_document: default
  html_document: default
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

```{r, include=FALSE}
setwd("P:/Evaluation/TN Lives Count_Writing/4_Target1_EnhancedCrisisFollow-up/3_Data & Data Analyses")
datPreAdult = read.csv("Target1EnhancedBaseAdult.csv", header = TRUE)
datPostAdult = read.csv("Target1EnhancedPostAdult.csv", header = TRUE)
datPreYouth = read.csv("Target1EnhancedBaseYouth.csv", header= FALSE, row.names = NULL)
datPostYouth = read.csv("Target1EnhancedPostYouth.csv", header = FALSE, row.names = NULL)
datAdultTreat = read.csv("AdultTreatments.csv", header = TRUE)
library(foreign)
library(nnet)
library(ggplot2)
library(prettyR)
library(nlme)
library(prettyR)
library(descr)
library(Amelia)
library(mitools)
library(BaylorEdPsych)
library(openxlsx)
library(lavaan)
library(lavaan)
library(psych)
library(semTools)
library(dplyr)
library(ltm)
library(prettyR)
library(semTools)
library(GPArotation)
library(lavaan)
library(psych)
library(semTools)
library(dplyr)
library(ltm)
library(lordif)
library(Amelia)
library(plyr)
library(DescTools)
library(MissMech)
library(jtools)
library(paran)
library(effsize)
library(multcomp)
library(MuMIn)






head(datPreAdult)
# subset the variables that you want
datPreAdult =datPreAdult[c(1, 3:4, 6,8,10, 12:13, 15:34, 36:45, 47:51, 53:59)]

head(datPostAdult)
datPostAdult = datPostAdult[c(1, 3:22,24:33, 35:39, 41:47)]
head(datPostAdult)

# Rename added variables otherwise everything else should be the same
colnames(datPreAdult)[colnames(datPreAdult) == "Added.V2..Thinking.of.Ways.to.Kill.Self"] = "Added"   

## Now merge everything
datAdult = merge(datPreAdult, datPostAdult, by = "Adult.ID", sort = TRUE)
head(datAdult)
dim(datAdult)

### 357 is still around above

# Need to er
#datAdultTreat = read.csv("AdultTreatment.csv", header = TRUE)
#Now we need to merge the treatment variable before we transform into long format to avoid duplication.

head(datAdultTreat)

datAdult = merge(datAdult, datAdultTreat, by = "Adult.ID", sort = TRUE)
head(datAdult)
dim(datAdult)



datAdult = reshape(datAdult, varying = list(c("Desire.to.succeed.x", "Desire.to.succeed.y"), c("My.own.plan.to.stay.well.x", "My.own.plan.to.stay.well.y"), c("Goals.in.life.x", "Goals.in.life.y"), c("Believe.I.can.meet.personal.goals.x", "Believe.I.can.meet.personal.goals.y"), c("Purpose.in.life.x", "Purpose.in.life.y"), c("Fear.doesn.t.stop.me......x", "Fear.doesn.t.stop.me......y"), c("I.can.handle.my.life.x", "I.can.handle.my.life.y"), c("I.like.myself.x", "I.like.myself.y"), c("If.people.really.knew.me.......x", "If.people.really.knew.me.......y"), c("Who.I.want.to.become.x", "Who.I.want.to.become.y"), c("Something.good.will.happen.x", "Something.good.will.happen.y"), c("I.m.hopeful.about.future.x", "I.m.hopeful.about.future.y"), c("Continue.to.have.new.interests.x", "Continue.to.have.new.interests.y"), c("Coping.with.mental.illness.not.focus.of.life.x", "Coping.with.mental.illness.not.focus.of.life.y"), c("Symptoms.interfere.less.and.less.x", "Symptoms.interfere.less.and.less.y"), c("Symptoms.problem.for.shorter.periods.x", "Symptoms.problem.for.shorter.periods.y"), c("Know.when.to.ask.for.help.x", "Know.when.to.ask.for.help.y"), c("Willing.to.ask.for.help.x", "Willing.to.ask.for.help.y"), c("I.ask.for.help.when.I.need.it..x", "I.ask.for.help.when.I.need.it..y"), c("I.can.handle.stress..x", "I.can.handle.stress..y"), c("Better.off.if.I.were.gone.x", "Better.off.if.I.were.gone.y"), c("Happier.without.me.x", "Happier.without.me.y"), c("Death.would.be.a.relief.x", "Death.would.be.a.relief.y"), c("Wish.they.could.be.rid.of.me.x", "Wish.they.could.be.rid.of.me.y"), c("Make.things.worse.x", "Make.things.worse.y"), c("Feel.like.I.belong.x", "Feel.like.I.belong.y"), c("Have.many.caring.and.supportive.friends.x", "Have.many.caring.and.supportive.friends.y"), c("Feel.disconnected.x", "Feel.disconnected.y"), c("Feel.like.an.outsider.x", "Feel.like.an.outsider.y"), c("Close.to.other.people.x", "Close.to.other.people.y"), c("Unable.to.take.care.of.self.x", "Unable.to.take.care.of.self.y"), c("Not.recover.or.get.better.x", "Not.recover.or.get.better.y"), c("I.am.to.blame.x", "I.am.to.blame.y"), c("Unpredictable.x", "Unpredictable.y"), c("Dangerous.x", "Dangerous.y"), c("Wish.life.would.end..x", "Wish.life.would.end..y"), c("Life.not.worth.living.x", "Life.not.worth.living.y"), c("Life.so.bad..feel.like.giving.up..x", "Life.so.bad..feel.like.giving.up..y"), c("Better.for.everyone.if.I.were.to.die..x", "Better.for.everyone.if.I.were.to.die..y"), c("Added.x", "Added.y"), c("No.solution.to.my.problems.x", "No.solution.to.my.problems.y"), c("Believe.my.life.will.end.in.suicide..x", "Believe.my.life.will.end.in.suicide..y")), direction = "long", times = c(0,1))



colnames(datAdult) = c("ID", "Age", "Gender", "Race", "SexualOrientation", "RelationshipStatus", "Edu", "Employment", "Treatment", "Time", "RAS1", "RAS2", "RAS3", "RAS4", "RAS5", "RAS6", "RAS7", "RAS8", "RAS9", "RAS10", "RAS11", "RAS12", "RAS13", "RAS14", "RAS15", "RAS16", "RAS17", "RAS18", "RAS19", "RAS20", "INQ1", "INQ2", "INQ3", "INQ4", "INQ5", "INQ6", "INQ7", "INQ8", "INQ9", "INQ10", "SSMI1", "SSMI2", "SSMI3", "SSMI4", "SSMI5", "SIS1", "SIS2", "SIS3", "SIS4", "SIS5", "SIS6", "SIS7")

# Drop last column id 
datAdult = data.frame(datAdult)
head(datAdult)
datAdult$NA. = NULL
head(datAdult)
dim(datAdult)
head(datAdult)



#Checking for issues with adult data set
## In the paper start with 115, because we have three less people later
116*2
summary(datAdult)
dim(datAdult)

dim(subset(datAdult, Time  == 1))
dim(subset(datAdult, Time  == 0))
describe.factor(datAdult$Age)
# One person age is 451 get rid of them
datAdult = subset(datAdult, Age < 450)
dim(datAdult)



# Two treatments have B with space first so try and recode those as just B's
datAdult$Treatment = ifelse(datAdult$Treatment == " B", "B", datAdult$Treatment)
compmeans(datAdult$RAS1, datAdult$Time)

describe.factor(datAdult$Treatment)

describe.factor(datAdult$Treatment)
datAdult$Treatment = ifelse(datAdult$Treatment == 8, 1, ifelse(datAdult$Treatment == 3, 2, ifelse(datAdult$Treatment == 6, 3, ifelse(datAdult$Treatment == "B", 2, datAdult$Treatment))))

describe.factor(datAdult$Treatment)

## If there are NA's for treatment then need to drop them
datAdult = subset(datAdult, Treatment == 1 | Treatment == 2 | Treatment == 3)
dim(datAdult)
compmeans(datAdult$RAS1, datAdult$Time)

# Three items are reversed scored: f = 6, g = 7, j = 10
datAdult$INQ6 = ifelse(datAdult$INQ6 == 1, 5, ifelse(datAdult$INQ6 == 2,4, ifelse(datAdult$INQ6  == 3,3, ifelse(datAdult$INQ6  == 4,2, ifelse(datAdult$INQ6  == 5,1,datAdult$INQ6)))))

datAdult$INQ7= ifelse(datAdult$INQ7== 1, 5, ifelse(datAdult$INQ7== 2,4, ifelse(datAdult$INQ7 == 3,3, ifelse(datAdult$INQ7 == 4,2, ifelse(datAdult$INQ7 == 5,1,datAdult$INQ7)))))

datAdult$INQ10= ifelse(datAdult$INQ10== 1, 5, ifelse(datAdult$INQ10== 2,4, ifelse(datAdult$INQ10 == 3,3, ifelse(datAdult$INQ10 == 4,2, ifelse(datAdult$INQ10 == 5,1,datAdult$INQ10)))))


head(datAdult)



# Create pre data sets for psychometrics
head(datAdult)
RAS = datAdult[c(11:30)]
head(RAS)

head(datAdult)
INQ = datAdult[c(31:40)]
head(INQ)

SSMI = datAdult[c(41:45)]
head(SSMI)

SIS = datAdult[c(46:52)]
SIS

# Subscale one: f =6, g = 7, h = 8, I = 9,  j = 10, k = 11, l = 12, m = 13, t = 20
RASSub1 = RAS[c(6:13, 20)]


# Subscale two q = 17, r= 18, s= 19
head(RAS)
RASSub2 = RAS[c(17:19)]
# Subscale three: a = 1, b = 2, c = 3, d= 4, e = 5
head(RAS)
RASSub3 = RAS[c(1:5)]

# Subscale five: n = 14, o = 15, p = 16
head(RAS)
RASSub5 = RAS[c(14:16)]


#Subscale 1 for INQ: a = 1, b = 2, c = 3, d = 4, e = 5
INQSub1 = INQ[c(1:5)]

#Subscale 2 for INQ: f-j: 6-10
INQSub2 = INQ[c(6:10)]

#Subscale 1 for SIS: a-d: 1:4
SISSub1 = SIS[c(1:4)]

#Subscale 2 for SIS: e-g: 5:7
SISSub2 = SIS[c(5:7)]

SSMIPrePost = datAdult[c(41:45)]
# Creating sum scores for the data analysis that contains all data not just pre data
datAdultDemos = datAdult[c(1:10)]


RASTotalScoreF1 = rowSums(RASSub1)
RASTotalScoreF2 = rowSums(RASSub2)
RASTotalScoreF3 = rowSums(RASSub3)
RASTotalScoreF5 = rowSums(RASSub5)
INQTotalScoreF1 = rowSums(INQSub1)
INQTotalScoreF2 = rowSums(INQSub2)
SISTotalScoreF1 = rowSums(SISSub1)
SISTotalScoreF2 = rowSums(SISSub2)
SSMITotalScore = rowSums(SSMIPrePost)


### Jennifer Lockman full data set
datAdultAnalysisJen = data.frame(datAdultDemos, RASTotalScoreF1, RASTotalScoreF2, RASTotalScoreF3, RASTotalScoreF5, INQTotalScoreF1, INQTotalScoreF2, SISTotalScoreF1, SISTotalScoreF2, SSMITotalScore)
write.csv(datAdultAnalysisJen, "EnhancedDataSet.csv", row.names = FALSE)

datAdultAnalysis = data.frame(datAdultDemos, RASTotalScoreF1, RASTotalScoreF2, RASTotalScoreF3, RASTotalScoreF5, INQTotalScoreF1, INQTotalScoreF2, SISTotalScoreF1, SISTotalScoreF2, SSMITotalScore)
#Need code gender, race, sexual orientation, edu, employment, RelationshipStatus as binary
#Gender: 2 = 1, 1 = 0
#Race: 7 = 0, all else 1
#Sex Orien: 3 = 0, all else 1
#Edu: 2 = 1, all else = 0; high school over lower for one
#Employment 1 = 1 else = 0; unemployed versus everyone else
#Relationship Status: 1,2,3,4 = 1 else = 0


datAdultAnalysis$Gender = ifelse(datAdultAnalysis$Gender == 2,1, 0)
datAdultAnalysis$Race = ifelse(datAdultAnalysis$Race == 7,0, 1)
datAdultAnalysis$SexualOrientation = ifelse(datAdultAnalysis$SexualOrientation == 3,0, 1)
datAdultAnalysis$Edu = ifelse(datAdultAnalysis$Edu <= 2,1, 0)
datAdultAnalysis$Employment = ifelse(datAdultAnalysis$Employment == 1,1, 0)


datAdultAnalysis$RelationshipStatus = ifelse(datAdultAnalysis$RelationshipStatus <= 4, 1, 0)
describe.factor(datAdultAnalysis$RelationshipStatus)


# For the complete data set I need to drop SIS, because there is a ton of missing data
dim(datAdultAnalysis)
datAdultAnalysis$SISTotalScoreF1 = NULL
datAdultAnalysis$SISTotalScoreF2 = NULL
datAdultAnalysisComplete = datAdultAnalysis
datAdultAnalysisComplete = na.omit(datAdultAnalysisComplete)
dim(datAdultAnalysisComplete)

## Getting the percentage of data missing for each variable across

## Calculating how much data is missing
1-(dim(datAdultAnalysisComplete)[1]/dim(datAdultAnalysis)[1])

### Get number of people at each time point
datAdultAnalysisCompletePre = subset(datAdultAnalysisComplete, Time == 0) 

dim(datAdultAnalysisCompletePre)

datAdultAnalysisCompletePost = subset(datAdultAnalysisComplete, Time == 1) 
dim(datAdultAnalysisCompletePost)
datAdultAnalysisCompletePost
setwd("C:/Users/Matthew.Hanauer/Desktop")

write.csv(datAdultAnalysis, "EnhancedDataSet.csv", row.names = FALSE)

describe.factor(datAdultAnalysis$ID)

datAdultAnalysis = datAdultAnalysis[order(datAdultAnalysis$ID),]

## There are some repeats of three people who have the same pre and post twice.
#763, 1131, 1272
datAdultAnalysis = datAdultAnalysis[-c(81:82),]  
datAdultAnalysis[80:86,]

datAdultAnalysis[189:192,]
datAdultAnalysis = datAdultAnalysis[-c(189:190),]

datAdultAnalysis[221:224,]
datAdultAnalysis = datAdultAnalysis[-c(221:221),]

dim(subset(datAdultAnalysis, Time == 0))

m = 10
datAdultAnalysisImpute = amelia(m = m, datAdultAnalysis, noms = c("Gender", "Race", "Edu", "SexualOrientation", "RelationshipStatus", "Employment"), idvars = c("ID", "Treatment"), ts = "Time")

#compare.density(datAdultAnalysisImpute, var = "RASTotalScore")
#compare.density(datAdultAnalysisImpute, var = "INQTotalScore")
#compare.density(datAdultAnalysisImpute, var = "SSMITotalScore")
#compare.density(datAdultAnalysisImpute, var = "SISTotalScore")

summary(datAdultAnalysisImpute)

datAnalysisAll = lapply(1:m, function(x){datAdultAnalysisImpute$imputations[[x]]})

dim(datAnalysisAll[[1]])

head(datAnalysisAll[[1]])
###### Wide vesion here
datWideAnalysis = NULL
for(i in 1:m){
  datWideAnalysis[[i]] = reshape(datAnalysisAll[[i]], v.names = c("RASTotalScoreF1", "RASTotalScoreF2", "RASTotalScoreF3", "RASTotalScoreF5", "INQTotalScoreF1", "INQTotalScoreF2", "SSMITotalScore"),  timevar = "Time", direction = "wide", idvar = "ID")
}

dim(subset(datAnalysisAll[[1]], Time == 0))
dim(subset(datAnalysisAll[[1]], Time == 1))

head(datWideAnalysis[[1]])
for(i in 1:m){
  datWideAnalysis[[i]]$RASDiffF1 =datWideAnalysis[[i]]$RASTotalScoreF1.1-datWideAnalysis[[i]]$RASTotalScoreF1.0
  datWideAnalysis[[i]]$RASDiffF2 =datWideAnalysis[[i]]$RASTotalScoreF2.1-datWideAnalysis[[i]]$RASTotalScoreF2.0
  datWideAnalysis[[i]]$RASDiffF3 =datWideAnalysis[[i]]$RASTotalScoreF3.1-datWideAnalysis[[i]]$RASTotalScoreF3.0
  datWideAnalysis[[i]]$RASDiffF5 =datWideAnalysis[[i]]$RASTotalScoreF5.1-datWideAnalysis[[i]]$RASTotalScoreF5.0
  datWideAnalysis[[i]]$INQDiffF1 =datWideAnalysis[[i]]$INQTotalScoreF1.1-datWideAnalysis[[i]]$INQTotalScoreF1.0  
  datWideAnalysis[[i]]$INQDiffF2 =datWideAnalysis[[i]]$INQTotalScoreF2.1-datWideAnalysis[[i]]$INQTotalScoreF2.0
  datWideAnalysis[[i]]$SSMIDiff =datWideAnalysis[[i]]$SSMITotalScore.1-datWideAnalysis[[i]]$SSMITotalScore.0
}



```
#####################
Checking descriptives at each time point
#####################
```{r}
datAdultAnalysisCompleteBase = subset(datAdultAnalysis, Time == 0)
dim(datAdultAnalysisCompleteBase)
describe(datAdultAnalysisCompleteBase)
describe.factor(datAdultAnalysisCompleteBase$Gender)
describe.factor(datAdultAnalysisCompleteBase$Race)
describe.factor(datAdultAnalysisCompleteBase$RelationshipStatus)
describe.factor(datAdultAnalysisCompleteBase$Edu)
describe.factor(datAdultAnalysisCompleteBase$Employment)
describe.factor(datAdultAnalysisCompleteBase$Treatment)

round(apply(datAdultAnalysisCompleteBase, 2, sd, na.rm = TRUE),2)


# Post
datAdultAnalysisCompletePost = subset(datAdultAnalysis, Time == 1)
dim(datAdultAnalysisCompletePost)
describe(datAdultAnalysisCompletePost)
describe.factor(datAdultAnalysisCompletePost$Gender)
describe.factor(datAdultAnalysisCompletePost$Race)
describe.factor(datAdultAnalysisCompletePost$RelationshipStatus)
describe.factor(datAdultAnalysisCompletePost$Edu)
describe.factor(datAdultAnalysisCompletePost$Employment)

round(apply(datAdultAnalysisCompletePost, 2, sd, na.rm = TRUE),2)


```


Here I am checking the randomization. Using three logisitic regression comparing T1 versus T2 across covariates at baseline, then T1 versus T3 and finally T2 versus T3.

There is significant in relationship status between treatment two and three for relationship status.  Single people are more likely to be in treatment two relative to treatement three 
```{r}
datAdultRandomT12 = subset(datAdultAnalysisComplete, Time == 0 & Treatment == 1  | Treatment == 2)
datAdultRandomT12$Treatment = factor(datAdultRandomT12$Treatment)
datAdultRandomT12$Treatment == ifelse(datAdultRandomT12$Treatment == 1, 1, 0)

modelT12 = glm(Treatment ~  Age + Gender + Race + SexualOrientation + RelationshipStatus + Edu + Employment, data = datAdultRandomT12, family = "binomial")

summary(modelT12)

datAdultRandomT13 = subset(datAdultAnalysisComplete, Time == 0 & Treatment == 1  | Treatment == 3)
datAdultRandomT13$Treatment = factor(datAdultRandomT13$Treatment)
datAdultRandomT13$Treatment == ifelse(datAdultRandomT13$Treatment == 1, 1, 0)


modelT13 = glm(Treatment ~  Age + Gender + Race + SexualOrientation + RelationshipStatus + Edu + Employment, data = datAdultRandomT13, family = "binomial")

summary(modelT13)


datAdultRandomT23 = subset(datAdultAnalysisComplete, Time == 0 & Treatment == 2  | Treatment == 3)
datAdultRandomT23$Treatment = factor(datAdultRandomT23$Treatment)
datAdultRandomT23$Treatment == ifelse(datAdultRandomT23$Treatment == 2, 1, 0)


modelT23 = glm(Treatment ~  Age + Gender + Race + SexualOrientation + RelationshipStatus + Edu + Employment, data = datAdultRandomT23, family = "binomial")

summary(modelT23)
```
########################
Imputed analysese below

Descriptives for base
```{r, echo=FALSE}

descFun = function(x){
  x = data.frame(t(x))
}
datAnalysisAllDes = lapply(1:m, function(x){subset(datAnalysisAll[[x]], Time == 0)})


for(i in 1:m){
  datAnalysisAllDes[[i]]$Treatment= datAnalysisAllDes[[i]]$Treatment =NULL
}


mean.out = NULL
for(i in 1:m) {
  mean.out[[i]] = apply(datAnalysisAllDes[[i]], 2, mean)
  mean.out = data.frame(mean.out)
}

mean.out = descFun(mean.out)


# now get sds
sd.out = NULL
for(i in 1:m) {
  sd.out[[i]] = apply(datAnalysisAllDes[[i]], 2, sd)
  sd.out = data.frame(sd.out)
}
sd.out = descFun(sd.out)
mean.sd.out= mi.meld(mean.out, sd.out)
mean.sd.out
```
Descriptives for post
```{r, echo=FALSE}


descFun = function(x){
  x = data.frame(t(x))
}
datAnalysisAllDes = lapply(1:m, function(x){subset(datAnalysisAll[[x]], Time == 1)})


for(i in 1:m){
  datAnalysisAllDes[[i]]$Treatment= datAnalysisAllDes[[i]]$Treatment =NULL
}


mean.out = NULL
for(i in 1:m) {
  mean.out[[i]] = apply(datAnalysisAllDes[[i]], 2, mean)
  mean.out = data.frame(mean.out)
}

mean.out = descFun(mean.out)


# now get sds
sd.out = NULL
for(i in 1:m) {
  sd.out[[i]] = apply(datAnalysisAllDes[[i]], 2, sd)
  sd.out = data.frame(sd.out)
}
sd.out = descFun(sd.out)
mean.sd.out= mi.meld(mean.out, sd.out)
mean.sd.out= mi.meld(mean.out, sd.out)
mean.sd.out

```
################################################
Multilevel with treatment only with imputed data
################################################
```{r, include=FALSE}
datAnalysisAll = lapply(1:m, function(x){datAdultAnalysisImpute$imputations[[x]]})

datAnalysisT1 = NULL
datAnalysisT2 = NULL
datAnalysisT3 = NULL

for(i in 1:m){
  datAnalysisT1[[i]] = subset(datAnalysisAll[[i]], Treatment == 1)
  datAnalysisT2[[i]] = subset(datAnalysisAll[[i]], Treatment == 2)
  datAnalysisT3[[i]] = subset(datAnalysisAll[[i]], Treatment == 3)
}

```
###############
RASF1 Time and T1
###############
```{r, echo=FALSE}
output = list()
outputReg = list()
coef_output =  NULL
se_output = NULL
rSquared = NULL
stdBeta = NULL
stdCoef = NULL
stdSe = NULL


for(i in 1:m){
  output[[i]] = lmer(RASTotalScoreF1 ~ Time + (1 | ID), data  = datAnalysisT1[[i]])
  outputReg[[i]] = output[[i]]
  stdBeta[[i]] = std.coef(output[[i]], partial.sd = TRUE) 
  stdCoef[[i]] = stdBeta[[i]][2,1]
  stdSe[[i]] = stdBeta[[i]][2,2]
  rSquared[[i]] = r.squaredGLMM(output[[i]])
  output[[i]] = summary(output[[i]])
  coef_output[[i]] = output[[i]]$coefficients[,1]
  se_output[[i]] = output[[i]]$coefficients[,2]
}
coef_output = data.frame(coef_output)

quickTrans = function(x){
  x = data.frame(x)
  x = t(x)
  x = data.frame(x)
}
coef_output = quickTrans(coef_output)
se_output = quickTrans(se_output)

stdBetaOutput = data.frame(stdCoef)
stdSeOutput = data.frame(stdSe)

coef_output = data.frame(coef_output, stdBetaOutput)
se_output = data.frame(se_output, stdSeOutput)

# Figure out the degrees of freedom 

#coefsAll = mi.meld(q = coef_output, se = se_output)

meldAllT_stat = function(x,y){
  coefsAll = mi.meld(q = x, se = y)
  coefs1 = t(data.frame(coefsAll$q.mi))
  ses1 = t(data.frame(coefsAll$se.mi))
  z_stat = coefs1/ses1
  p = 2*pnorm(-abs(z_stat))
  return(data.frame(coefs1, ses1, z_stat, p))
}

results = meldAllT_stat(coef_output, se_output)
round(results,3)
```
################
Cohen's D RAS T1
################
```{r}
datTimeTreat = NULL
for(i in 1:m){
datTimeTreat[[i]] = subset(datAnalysisT1[[i]], Treatment == 1) 
}

library(psych)
cohenDat = NULL
RASTotalT0 = NULL
RASTotalT1 = NULL
for(i in 1:m){
RASTotalT0[[i]] = subset(datTimeTreat[[i]], Time == 0)
RASTotalT1[[i]] = subset(datTimeTreat[[i]], Time == 1)
cohenDat[[i]] = mean(RASTotalT1[[i]]$RASTotalScoreF1-RASTotalT0[[i]]$RASTotalScoreF1)/sqrt((sd(RASTotalT1[[i]]$RASTotalScoreF1)^2+sd(RASTotalT0[[i]]$RASTotalScoreF1)^2)/2)
}


cohenD = data.frame(cohenDat)

meldAllT_stat = function(x,y){
coefsAll = mi.meld(q = x, se = y)
coefs1 = t(data.frame(coefsAll$q.mi))
ses1 = t(data.frame(coefsAll$se.mi))
return(data.frame(coefs1, ses1))
}
y = data.frame(rnorm(10))

meldAllT_stat(cohenD, y)
```
###############
RAS Time and T2
###############
```{r, echo=FALSE}
output = list()
outputReg = list()
coef_output =  NULL
se_output = NULL
rSquared = NULL
stdBeta = NULL
stdCoef = NULL
stdSe = NULL



for(i in 1:m){
output[[i]] = lmer(RASTotalScoreF1 ~ Time  + (1 | ID), data  = datAnalysisT2[[i]])
outputReg[[i]] = output[[i]]
stdBeta[[i]] = std.coef(output[[i]], partial.sd = TRUE) 
stdCoef[[i]] = stdBeta[[i]][2,1]
stdSe[[i]] = stdBeta[[i]][2,2]
rSquared[[i]] = r.squaredGLMM(output[[i]])
output[[i]] = summary(output[[i]])
coef_output[[i]] = output[[i]]$coefficients[,1]
se_output[[i]] = output[[i]]$coefficients[,2]
}
coef_output = data.frame(coef_output)

quickTrans = function(x){
x = data.frame(x)
x = t(x)
x = data.frame(x)
}
coef_output = quickTrans(coef_output)
se_output = quickTrans(se_output)

stdBetaOutput = data.frame(stdCoef)
stdSeOutput = data.frame(stdSe)

coef_output = data.frame(coef_output, stdBetaOutput)
se_output = data.frame(se_output, stdSeOutput)

# Figure out the degrees of freedom 

#coefsAll = mi.meld(q = coef_output, se = se_output)

meldAllT_stat = function(x,y){
coefsAll = mi.meld(q = x, se = y)
coefs1 = t(data.frame(coefsAll$q.mi))
ses1 = t(data.frame(coefsAll$se.mi))
z_stat = coefs1/ses1
p = 2*pnorm(-abs(z_stat))
return(data.frame(coefs1, ses1, z_stat, p))
}

results = meldAllT_stat(coef_output, se_output)
round(results,3)
```
################
Cohen's D RAS T2
################
```{r}
datTimeTreat = datAnalysisT2

library(psych)
cohenDat = NULL
RASTotalT0 = NULL
RASTotalT1 = NULL
for(i in 1:m){
  RASTotalT0[[i]] = subset(datTimeTreat[[i]], Time == 0)
  RASTotalT1[[i]] = subset(datTimeTreat[[i]], Time == 1)
  cohenDat[[i]] = mean(RASTotalT1[[i]]$RASTotalScoreF1-RASTotalT0[[i]]$RASTotalScoreF1)/sqrt((sd(RASTotalT1[[i]]$RASTotalScoreF1)^2+sd(RASTotalT0[[i]]$RASTotalScoreF1)^2)/2)
}

cohenD = data.frame(cohenDat)

meldAllT_stat = function(x,y){
  coefsAll = mi.meld(q = x, se = y)
  coefs1 = t(data.frame(coefsAll$q.mi))
  ses1 = t(data.frame(coefsAll$se.mi))
  return(data.frame(coefs1, ses1))
}
y = data.frame(rnorm(10))

meldAllT_stat(cohenD, y)
```

###############
RAS Time and T3
###############
```{r, echo=FALSE}
output = list()
outputReg = list()
coef_output =  NULL
se_output = NULL
rSquared = NULL
stdBeta = NULL
stdCoef = NULL
stdSe = NULL


for(i in 1:m){
  output[[i]] = lmer(RASTotalScoreF1 ~ Time + (1 | ID), data  = datAnalysisT3[[i]])
  outputReg[[i]] = output[[i]]
  stdBeta[[i]] = std.coef(output[[i]], partial.sd = TRUE) 
  stdCoef[[i]] = stdBeta[[i]][2,1]
  stdSe[[i]] = stdBeta[[i]][2,2]
  rSquared[[i]] = r.squaredGLMM(output[[i]])
  output[[i]] = summary(output[[i]])
  coef_output[[i]] = output[[i]]$coefficients[,1]
  se_output[[i]] = output[[i]]$coefficients[,2]
}
coef_output = data.frame(coef_output)

quickTrans = function(x){
  x = data.frame(x)
  x = t(x)
  x = data.frame(x)
}
coef_output = quickTrans(coef_output)
se_output = quickTrans(se_output)

stdBetaOutput = data.frame(stdCoef)
stdSeOutput = data.frame(stdSe)

coef_output = data.frame(coef_output, stdBetaOutput)
se_output = data.frame(se_output, stdSeOutput)


meldAllT_stat = function(x,y){
  coefsAll = mi.meld(q = x, se = y)
  coefs1 = t(data.frame(coefsAll$q.mi))
  ses1 = t(data.frame(coefsAll$se.mi))
  z_stat = coefs1/ses1
  p = 2*pnorm(-abs(z_stat))
  return(data.frame(coefs1, ses1, z_stat, p))
}

results = meldAllT_stat(coef_output, se_output)
round(results,3)
```
################
Cohen's D RAS T3
################
```{r}
datTimeTreat = datAnalysisT3

library(psych)
cohenDat = NULL
RASTotalT0 = NULL
RASTotalT1 = NULL
for(i in 1:m){
RASTotalT0[[i]] = subset(datTimeTreat[[i]], Time == 0)
RASTotalT1[[i]] = subset(datTimeTreat[[i]], Time == 1)
cohenDat[[i]] = mean(RASTotalT1[[i]]$RASTotalScoreF1-RASTotalT0[[i]]$RASTotalScoreF1)/sqrt((sd(RASTotalT1[[i]]$RASTotalScoreF1)^2+sd(RASTotalT0[[i]]$RASTotalScoreF1)^2)/2)
}

cohenD = data.frame(cohenDat)

meldAllT_stat = function(x,y){
coefsAll = mi.meld(q = x, se = y)
coefs1 = t(data.frame(coefsAll$q.mi))
ses1 = t(data.frame(coefsAll$se.mi))
return(data.frame(coefs1, ses1))
}
y = data.frame(rnorm(10))

meldAllT_stat(cohenD, y)
```
###############
RASF2 Time and T1
###############
```{r, echo=FALSE}
output = list()
outputReg = list()
coef_output =  NULL
se_output = NULL
rSquared = NULL
stdBeta = NULL
stdCoef = NULL
stdSe = NULL


for(i in 1:m){
  output[[i]] = lmer(RASTotalScoreF2 ~ Time + (1 | ID), data  = datAnalysisT1[[i]])
  outputReg[[i]] = output[[i]]
  stdBeta[[i]] = std.coef(output[[i]], partial.sd = TRUE) 
  stdCoef[[i]] = stdBeta[[i]][2,1]
  stdSe[[i]] = stdBeta[[i]][2,2]
  rSquared[[i]] = r.squaredGLMM(output[[i]])
  output[[i]] = summary(output[[i]])
  coef_output[[i]] = output[[i]]$coefficients[,1]
  se_output[[i]] = output[[i]]$coefficients[,2]
}
coef_output = data.frame(coef_output)

quickTrans = function(x){
  x = data.frame(x)
  x = t(x)
  x = data.frame(x)
}
coef_output = quickTrans(coef_output)
se_output = quickTrans(se_output)

stdBetaOutput = data.frame(stdCoef)
stdSeOutput = data.frame(stdSe)

coef_output = data.frame(coef_output, stdBetaOutput)
se_output = data.frame(se_output, stdSeOutput)

# Figure out the degrees of freedom 

#coefsAll = mi.meld(q = coef_output, se = se_output)

meldAllT_stat = function(x,y){
  coefsAll = mi.meld(q = x, se = y)
  coefs1 = t(data.frame(coefsAll$q.mi))
  ses1 = t(data.frame(coefsAll$se.mi))
  z_stat = coefs1/ses1
  p = 2*pnorm(-abs(z_stat))
  return(data.frame(coefs1, ses1, z_stat, p))
}

results = meldAllT_stat(coef_output, se_output)
round(results,3)
```
################
Cohen's D RAS T1
################
```{r}
datTimeTreat = NULL
for(i in 1:m){
datTimeTreat[[i]] = subset(datAnalysisT1[[i]], Treatment == 1) 
}

library(psych)
cohenDat = NULL
RASTotalT0 = NULL
RASTotalT1 = NULL
for(i in 1:m){
RASTotalT0[[i]] = subset(datTimeTreat[[i]], Time == 0)
RASTotalT1[[i]] = subset(datTimeTreat[[i]], Time == 1)
cohenDat[[i]] = mean(RASTotalT1[[i]]$RASTotalScoreF2-RASTotalT0[[i]]$RASTotalScoreF2)/sqrt((sd(RASTotalT1[[i]]$RASTotalScoreF2)^2+sd(RASTotalT0[[i]]$RASTotalScoreF2)^2)/2)
}


cohenD = data.frame(cohenDat)

meldAllT_stat = function(x,y){
coefsAll = mi.meld(q = x, se = y)
coefs1 = t(data.frame(coefsAll$q.mi))
ses1 = t(data.frame(coefsAll$se.mi))
return(data.frame(coefs1, ses1))
}
y = data.frame(rnorm(10))

meldAllT_stat(cohenD, y)
```
###############
RAS Time and T2
###############
```{r, echo=FALSE}
output = list()
outputReg = list()
coef_output =  NULL
se_output = NULL
rSquared = NULL
stdBeta = NULL
stdCoef = NULL
stdSe = NULL


for(i in 1:m){
output[[i]] = lmer(RASTotalScoreF2 ~ Time + (1 | ID), data  = datAnalysisT2[[i]])
outputReg[[i]] = output[[i]]
stdBeta[[i]] = std.coef(output[[i]], partial.sd = TRUE) 
stdCoef[[i]] = stdBeta[[i]][2,1]
stdSe[[i]] = stdBeta[[i]][2,2]
rSquared[[i]] = r.squaredGLMM(output[[i]])
output[[i]] = summary(output[[i]])
coef_output[[i]] = output[[i]]$coefficients[,1]
se_output[[i]] = output[[i]]$coefficients[,2]
}
coef_output = data.frame(coef_output)

quickTrans = function(x){
x = data.frame(x)
x = t(x)
x = data.frame(x)
}
coef_output = quickTrans(coef_output)
se_output = quickTrans(se_output)

stdBetaOutput = data.frame(stdCoef)
stdSeOutput = data.frame(stdSe)

coef_output = data.frame(coef_output, stdBetaOutput)
se_output = data.frame(se_output, stdSeOutput)

# Figure out the degrees of freedom 

#coefsAll = mi.meld(q = coef_output, se = se_output)

meldAllT_stat = function(x,y){
coefsAll = mi.meld(q = x, se = y)
coefs1 = t(data.frame(coefsAll$q.mi))
ses1 = t(data.frame(coefsAll$se.mi))
z_stat = coefs1/ses1
p = 2*pnorm(-abs(z_stat))
return(data.frame(coefs1, ses1, z_stat, p))
}

results = meldAllT_stat(coef_output, se_output)
round(results,3)
```
################
Cohen's D RAS T2
################
```{r}
datTimeTreat = datAnalysisT2

library(psych)
cohenDat = NULL
RASTotalT0 = NULL
RASTotalT1 = NULL
for(i in 1:m){
  RASTotalT0[[i]] = subset(datTimeTreat[[i]], Time == 0)
  RASTotalT1[[i]] = subset(datTimeTreat[[i]], Time == 1)
  cohenDat[[i]] = mean(RASTotalT1[[i]]$RASTotalScoreF2-RASTotalT0[[i]]$RASTotalScoreF2)/sqrt((sd(RASTotalT1[[i]]$RASTotalScoreF2)^2+sd(RASTotalT0[[i]]$RASTotalScoreF2)^2)/2)
}

cohenD = data.frame(cohenDat)

meldAllT_stat = function(x,y){
  coefsAll = mi.meld(q = x, se = y)
  coefs1 = t(data.frame(coefsAll$q.mi))
  ses1 = t(data.frame(coefsAll$se.mi))
  return(data.frame(coefs1, ses1))
}
y = data.frame(rnorm(10))

meldAllT_stat(cohenD, y)
```
###############
RAS Time and T3
###############
```{r, echo=FALSE}
output = list()
outputReg = list()
coef_output =  NULL
se_output = NULL
rSquared = NULL
stdBeta = NULL
stdCoef = NULL
stdSe = NULL


for(i in 1:m){
  output[[i]] = lmer(RASTotalScoreF2 ~ Time + (1 | ID), data  = datAnalysisT3[[i]])
  outputReg[[i]] = output[[i]]
  stdBeta[[i]] = std.coef(output[[i]], partial.sd = TRUE) 
  stdCoef[[i]] = stdBeta[[i]][2,1]
  stdSe[[i]] = stdBeta[[i]][2,2]
  rSquared[[i]] = r.squaredGLMM(output[[i]])
  output[[i]] = summary(output[[i]])
  coef_output[[i]] = output[[i]]$coefficients[,1]
  se_output[[i]] = output[[i]]$coefficients[,2]
}
coef_output = data.frame(coef_output)

quickTrans = function(x){
  x = data.frame(x)
  x = t(x)
  x = data.frame(x)
}
coef_output = quickTrans(coef_output)
se_output = quickTrans(se_output)

stdBetaOutput = data.frame(stdCoef)
stdSeOutput = data.frame(stdSe)

coef_output = data.frame(coef_output, stdBetaOutput)
se_output = data.frame(se_output, stdSeOutput)


meldAllT_stat = function(x,y){
  coefsAll = mi.meld(q = x, se = y)
  coefs1 = t(data.frame(coefsAll$q.mi))
  ses1 = t(data.frame(coefsAll$se.mi))
  z_stat = coefs1/ses1
  p = 2*pnorm(-abs(z_stat))
  return(data.frame(coefs1, ses1, z_stat, p))
}

results = meldAllT_stat(coef_output, se_output)
round(results,3)
```
################
Cohen's D RAS T3
################
```{r}
datTimeTreat = datAnalysisT3

library(psych)
cohenDat = NULL
RASTotalT0 = NULL
RASTotalT1 = NULL
for(i in 1:m){
RASTotalT0[[i]] = subset(datTimeTreat[[i]], Time == 0)
RASTotalT1[[i]] = subset(datTimeTreat[[i]], Time == 1)
cohenDat[[i]] = mean(RASTotalT1[[i]]$RASTotalScoreF2-RASTotalT0[[i]]$RASTotalScoreF2)/sqrt((sd(RASTotalT1[[i]]$RASTotalScoreF2)^2+sd(RASTotalT0[[i]]$RASTotalScoreF2)^2)/2)
}

cohenD = data.frame(cohenDat)

meldAllT_stat = function(x,y){
coefsAll = mi.meld(q = x, se = y)
coefs1 = t(data.frame(coefsAll$q.mi))
ses1 = t(data.frame(coefsAll$se.mi))
return(data.frame(coefs1, ses1))
}
y = data.frame(rnorm(10))

meldAllT_stat(cohenD, y)
```
###############
RASF3 Time and T1
###############
```{r, echo=FALSE}
output = list()
outputReg = list()
coef_output =  NULL
se_output = NULL
rSquared = NULL
stdBeta = NULL
stdCoef = NULL
stdSe = NULL


for(i in 1:m){
  output[[i]] = lmer(RASTotalScoreF3 ~ Time + (1 | ID), data  = datAnalysisT1[[i]])
  outputReg[[i]] = output[[i]]
  stdBeta[[i]] = std.coef(output[[i]], partial.sd = TRUE) 
  stdCoef[[i]] = stdBeta[[i]][2,1]
  stdSe[[i]] = stdBeta[[i]][2,2]
  rSquared[[i]] = r.squaredGLMM(output[[i]])
  output[[i]] = summary(output[[i]])
  coef_output[[i]] = output[[i]]$coefficients[,1]
  se_output[[i]] = output[[i]]$coefficients[,2]
}
coef_output = data.frame(coef_output)

quickTrans = function(x){
  x = data.frame(x)
  x = t(x)
  x = data.frame(x)
}
coef_output = quickTrans(coef_output)
se_output = quickTrans(se_output)

stdBetaOutput = data.frame(stdCoef)
stdSeOutput = data.frame(stdSe)

coef_output = data.frame(coef_output, stdBetaOutput)
se_output = data.frame(se_output, stdSeOutput)

# Figure out the degrees of freedom 

#coefsAll = mi.meld(q = coef_output, se = se_output)

meldAllT_stat = function(x,y){
  coefsAll = mi.meld(q = x, se = y)
  coefs1 = t(data.frame(coefsAll$q.mi))
  ses1 = t(data.frame(coefsAll$se.mi))
  z_stat = coefs1/ses1
  p = 2*pnorm(-abs(z_stat))
  return(data.frame(coefs1, ses1, z_stat, p))
}

results = meldAllT_stat(coef_output, se_output)
round(results,3)
```
################
Cohen's D RAS T1
################
```{r}
datTimeTreat = NULL
for(i in 1:m){
datTimeTreat[[i]] = subset(datAnalysisT1[[i]], Treatment == 1) 
}

library(psych)
cohenDat = NULL
RASTotalT0 = NULL
RASTotalT1 = NULL
for(i in 1:m){
RASTotalT0[[i]] = subset(datTimeTreat[[i]], Time == 0)
RASTotalT1[[i]] = subset(datTimeTreat[[i]], Time == 1)
cohenDat[[i]] = mean(RASTotalT1[[i]]$RASTotalScoreF3-RASTotalT0[[i]]$RASTotalScoreF3)/sqrt((sd(RASTotalT1[[i]]$RASTotalScoreF3)^2+sd(RASTotalT0[[i]]$RASTotalScoreF3)^2)/2)
}


cohenD = data.frame(cohenDat)

meldAllT_stat = function(x,y){
coefsAll = mi.meld(q = x, se = y)
coefs1 = t(data.frame(coefsAll$q.mi))
ses1 = t(data.frame(coefsAll$se.mi))
return(data.frame(coefs1, ses1))
}
y = data.frame(rnorm(10))

meldAllT_stat(cohenD, y)
```
###############
RAS Time and T2
###############
```{r, echo=FALSE}
output = list()
outputReg = list()
coef_output =  NULL
se_output = NULL
rSquared = NULL
stdBeta = NULL
stdCoef = NULL
stdSe = NULL


for(i in 1:m){
output[[i]] = lmer(RASTotalScoreF3 ~ Time + (1 | ID), data  = datAnalysisT2[[i]])
outputReg[[i]] = output[[i]]
stdBeta[[i]] = std.coef(output[[i]], partial.sd = TRUE) 
stdCoef[[i]] = stdBeta[[i]][2,1]
stdSe[[i]] = stdBeta[[i]][2,2]
rSquared[[i]] = r.squaredGLMM(output[[i]])
output[[i]] = summary(output[[i]])
coef_output[[i]] = output[[i]]$coefficients[,1]
se_output[[i]] = output[[i]]$coefficients[,2]
}
coef_output = data.frame(coef_output)

quickTrans = function(x){
x = data.frame(x)
x = t(x)
x = data.frame(x)
}
coef_output = quickTrans(coef_output)
se_output = quickTrans(se_output)

stdBetaOutput = data.frame(stdCoef)
stdSeOutput = data.frame(stdSe)

coef_output = data.frame(coef_output, stdBetaOutput)
se_output = data.frame(se_output, stdSeOutput)

# Figure out the degrees of freedom 

#coefsAll = mi.meld(q = coef_output, se = se_output)

meldAllT_stat = function(x,y){
coefsAll = mi.meld(q = x, se = y)
coefs1 = t(data.frame(coefsAll$q.mi))
ses1 = t(data.frame(coefsAll$se.mi))
z_stat = coefs1/ses1
p = 2*pnorm(-abs(z_stat))
return(data.frame(coefs1, ses1, z_stat, p))
}

results = meldAllT_stat(coef_output, se_output)
round(results,3)
```
################
Cohen's D RAS T2
################
```{r}
datTimeTreat = datAnalysisT2

library(psych)
cohenDat = NULL
RASTotalT0 = NULL
RASTotalT1 = NULL
for(i in 1:m){
  RASTotalT0[[i]] = subset(datTimeTreat[[i]], Time == 0)
  RASTotalT1[[i]] = subset(datTimeTreat[[i]], Time == 1)
  cohenDat[[i]] = mean(RASTotalT1[[i]]$RASTotalScoreF3-RASTotalT0[[i]]$RASTotalScoreF3)/sqrt((sd(RASTotalT1[[i]]$RASTotalScoreF3)^2+sd(RASTotalT0[[i]]$RASTotalScoreF3)^2)/2)
}

cohenD = data.frame(cohenDat)

meldAllT_stat = function(x,y){
  coefsAll = mi.meld(q = x, se = y)
  coefs1 = t(data.frame(coefsAll$q.mi))
  ses1 = t(data.frame(coefsAll$se.mi))
  return(data.frame(coefs1, ses1))
}
y = data.frame(rnorm(10))

meldAllT_stat(cohenD, y)
```
###############
RAS Time and T3
###############
```{r, echo=FALSE}
output = list()
outputReg = list()
coef_output =  NULL
se_output = NULL
rSquared = NULL
stdBeta = NULL
stdCoef = NULL
stdSe = NULL


for(i in 1:m){
  output[[i]] = lmer(RASTotalScoreF3 ~ Time + (1 | ID), data  = datAnalysisT3[[i]])
  outputReg[[i]] = output[[i]]
  stdBeta[[i]] = std.coef(output[[i]], partial.sd = TRUE) 
  stdCoef[[i]] = stdBeta[[i]][2,1]
  stdSe[[i]] = stdBeta[[i]][2,2]
  rSquared[[i]] = r.squaredGLMM(output[[i]])
  output[[i]] = summary(output[[i]])
  coef_output[[i]] = output[[i]]$coefficients[,1]
  se_output[[i]] = output[[i]]$coefficients[,2]
}
coef_output = data.frame(coef_output)

quickTrans = function(x){
  x = data.frame(x)
  x = t(x)
  x = data.frame(x)
}
coef_output = quickTrans(coef_output)
se_output = quickTrans(se_output)

stdBetaOutput = data.frame(stdCoef)
stdSeOutput = data.frame(stdSe)

coef_output = data.frame(coef_output, stdBetaOutput)
se_output = data.frame(se_output, stdSeOutput)


meldAllT_stat = function(x,y){
  coefsAll = mi.meld(q = x, se = y)
  coefs1 = t(data.frame(coefsAll$q.mi))
  ses1 = t(data.frame(coefsAll$se.mi))
  z_stat = coefs1/ses1
  p = 2*pnorm(-abs(z_stat))
  return(data.frame(coefs1, ses1, z_stat, p))
}

results = meldAllT_stat(coef_output, se_output)
round(results,3)
```
################
Cohen's D RAS T3
################
```{r}
datTimeTreat = datAnalysisT3

library(psych)
cohenDat = NULL
RASTotalT0 = NULL
RASTotalT1 = NULL
for(i in 1:m){
RASTotalT0[[i]] = subset(datTimeTreat[[i]], Time == 0)
RASTotalT1[[i]] = subset(datTimeTreat[[i]], Time == 1)
cohenDat[[i]] = mean(RASTotalT1[[i]]$RASTotalScoreF3-RASTotalT0[[i]]$RASTotalScoreF3)/sqrt((sd(RASTotalT1[[i]]$RASTotalScoreF3)^2+sd(RASTotalT0[[i]]$RASTotalScoreF3)^2)/2)
}

cohenD = data.frame(cohenDat)

meldAllT_stat = function(x,y){
coefsAll = mi.meld(q = x, se = y)
coefs1 = t(data.frame(coefsAll$q.mi))
ses1 = t(data.frame(coefsAll$se.mi))
return(data.frame(coefs1, ses1))
}
y = data.frame(rnorm(10))

meldAllT_stat(cohenD, y)
```
###############
RASF5 Time and T1
###############
```{r, echo=FALSE}
output = list()
outputReg = list()
coef_output =  NULL
se_output = NULL
rSquared = NULL
stdBeta = NULL
stdCoef = NULL
stdSe = NULL


for(i in 1:m){
  output[[i]] = lmer(RASTotalScoreF5 ~ Time + (1 | ID), data  = datAnalysisT1[[i]])
  outputReg[[i]] = output[[i]]
  stdBeta[[i]] = std.coef(output[[i]], partial.sd = TRUE) 
  stdCoef[[i]] = stdBeta[[i]][2,1]
  stdSe[[i]] = stdBeta[[i]][2,2]
  rSquared[[i]] = r.squaredGLMM(output[[i]])
  output[[i]] = summary(output[[i]])
  coef_output[[i]] = output[[i]]$coefficients[,1]
  se_output[[i]] = output[[i]]$coefficients[,2]
}
coef_output = data.frame(coef_output)

quickTrans = function(x){
  x = data.frame(x)
  x = t(x)
  x = data.frame(x)
}
coef_output = quickTrans(coef_output)
se_output = quickTrans(se_output)

stdBetaOutput = data.frame(stdCoef)
stdSeOutput = data.frame(stdSe)

coef_output = data.frame(coef_output, stdBetaOutput)
se_output = data.frame(se_output, stdSeOutput)

# Figure out the degrees of freedom 

#coefsAll = mi.meld(q = coef_output, se = se_output)

meldAllT_stat = function(x,y){
  coefsAll = mi.meld(q = x, se = y)
  coefs1 = t(data.frame(coefsAll$q.mi))
  ses1 = t(data.frame(coefsAll$se.mi))
  z_stat = coefs1/ses1
  p = 2*pnorm(-abs(z_stat))
  return(data.frame(coefs1, ses1, z_stat, p))
}

results = meldAllT_stat(coef_output, se_output)
round(results,3)
```
################
Cohen's D RAS T1
################
```{r}
datTimeTreat = NULL
for(i in 1:m){
datTimeTreat[[i]] = subset(datAnalysisT1[[i]], Treatment == 1) 
}

library(psych)
cohenDat = NULL
RASTotalT0 = NULL
RASTotalT1 = NULL
for(i in 1:m){
RASTotalT0[[i]] = subset(datTimeTreat[[i]], Time == 0)
RASTotalT1[[i]] = subset(datTimeTreat[[i]], Time == 1)
cohenDat[[i]] = mean(RASTotalT1[[i]]$RASTotalScoreF5-RASTotalT0[[i]]$RASTotalScoreF5)/sqrt((sd(RASTotalT1[[i]]$RASTotalScoreF5)^2+sd(RASTotalT0[[i]]$RASTotalScoreF5)^2)/2)
}


cohenD = data.frame(cohenDat)

meldAllT_stat = function(x,y){
coefsAll = mi.meld(q = x, se = y)
coefs1 = t(data.frame(coefsAll$q.mi))
ses1 = t(data.frame(coefsAll$se.mi))
return(data.frame(coefs1, ses1))
}
y = data.frame(rnorm(10))

meldAllT_stat(cohenD, y)
```
###############
RAS Time and T2
###############
```{r, echo=FALSE}
output = list()
outputReg = list()
coef_output =  NULL
se_output = NULL
rSquared = NULL
stdBeta = NULL
stdCoef = NULL
stdSe = NULL


for(i in 1:m){
output[[i]] = lmer(RASTotalScoreF5 ~ Time + (1 | ID), data  = datAnalysisT2[[i]])
outputReg[[i]] = output[[i]]
stdBeta[[i]] = std.coef(output[[i]], partial.sd = TRUE) 
stdCoef[[i]] = stdBeta[[i]][2,1]
stdSe[[i]] = stdBeta[[i]][2,2]
rSquared[[i]] = r.squaredGLMM(output[[i]])
output[[i]] = summary(output[[i]])
coef_output[[i]] = output[[i]]$coefficients[,1]
se_output[[i]] = output[[i]]$coefficients[,2]
}
coef_output = data.frame(coef_output)

quickTrans = function(x){
x = data.frame(x)
x = t(x)
x = data.frame(x)
}
coef_output = quickTrans(coef_output)
se_output = quickTrans(se_output)

stdBetaOutput = data.frame(stdCoef)
stdSeOutput = data.frame(stdSe)

coef_output = data.frame(coef_output, stdBetaOutput)
se_output = data.frame(se_output, stdSeOutput)

# Figure out the degrees of freedom 

#coefsAll = mi.meld(q = coef_output, se = se_output)

meldAllT_stat = function(x,y){
coefsAll = mi.meld(q = x, se = y)
coefs1 = t(data.frame(coefsAll$q.mi))
ses1 = t(data.frame(coefsAll$se.mi))
z_stat = coefs1/ses1
p = 2*pnorm(-abs(z_stat))
return(data.frame(coefs1, ses1, z_stat, p))
}

results = meldAllT_stat(coef_output, se_output)
round(results,3)
```
################
Cohen's D RAS T2
################
```{r}
datTimeTreat = datAnalysisT2

library(psych)
cohenDat = NULL
RASTotalT0 = NULL
RASTotalT1 = NULL
for(i in 1:m){
  RASTotalT0[[i]] = subset(datTimeTreat[[i]], Time == 0)
  RASTotalT1[[i]] = subset(datTimeTreat[[i]], Time == 1)
  cohenDat[[i]] = mean(RASTotalT1[[i]]$RASTotalScoreF5-RASTotalT0[[i]]$RASTotalScoreF5)/sqrt((sd(RASTotalT1[[i]]$RASTotalScoreF5)^2+sd(RASTotalT0[[i]]$RASTotalScoreF5)^2)/2)
}

cohenD = data.frame(cohenDat)

meldAllT_stat = function(x,y){
  coefsAll = mi.meld(q = x, se = y)
  coefs1 = t(data.frame(coefsAll$q.mi))
  ses1 = t(data.frame(coefsAll$se.mi))
  return(data.frame(coefs1, ses1))
}
y = data.frame(rnorm(10))

meldAllT_stat(cohenD, y)
```
###############
RAS Time and T3
###############
```{r, echo=FALSE}
output = list()
outputReg = list()
coef_output =  NULL
se_output = NULL
rSquared = NULL
stdBeta = NULL
stdCoef = NULL
stdSe = NULL


for(i in 1:m){
  output[[i]] = lmer(RASTotalScoreF5 ~ Time + (1 | ID), data  = datAnalysisT3[[i]])
  outputReg[[i]] = output[[i]]
  stdBeta[[i]] = std.coef(output[[i]], partial.sd = TRUE) 
  stdCoef[[i]] = stdBeta[[i]][2,1]
  stdSe[[i]] = stdBeta[[i]][2,2]
  rSquared[[i]] = r.squaredGLMM(output[[i]])
  output[[i]] = summary(output[[i]])
  coef_output[[i]] = output[[i]]$coefficients[,1]
  se_output[[i]] = output[[i]]$coefficients[,2]
}
coef_output = data.frame(coef_output)

quickTrans = function(x){
  x = data.frame(x)
  x = t(x)
  x = data.frame(x)
}
coef_output = quickTrans(coef_output)
se_output = quickTrans(se_output)

stdBetaOutput = data.frame(stdCoef)
stdSeOutput = data.frame(stdSe)

coef_output = data.frame(coef_output, stdBetaOutput)
se_output = data.frame(se_output, stdSeOutput)


meldAllT_stat = function(x,y){
  coefsAll = mi.meld(q = x, se = y)
  coefs1 = t(data.frame(coefsAll$q.mi))
  ses1 = t(data.frame(coefsAll$se.mi))
  z_stat = coefs1/ses1
  p = 2*pnorm(-abs(z_stat))
  return(data.frame(coefs1, ses1, z_stat, p))
}

results = meldAllT_stat(coef_output, se_output)
round(results,3)
```
################
Cohen's D RAS T3
################
```{r}
datTimeTreat = datAnalysisT3

library(psych)
cohenDat = NULL
RASTotalT0 = NULL
RASTotalT1 = NULL
for(i in 1:m){
RASTotalT0[[i]] = subset(datTimeTreat[[i]], Time == 0)
RASTotalT1[[i]] = subset(datTimeTreat[[i]], Time == 1)
cohenDat[[i]] = mean(RASTotalT1[[i]]$RASTotalScoreF5-RASTotalT0[[i]]$RASTotalScoreF5)/sqrt((sd(RASTotalT1[[i]]$RASTotalScoreF5)^2+sd(RASTotalT0[[i]]$RASTotalScoreF5)^2)/2)
}

cohenD = data.frame(cohenDat)

meldAllT_stat = function(x,y){
coefsAll = mi.meld(q = x, se = y)
coefs1 = t(data.frame(coefsAll$q.mi))
ses1 = t(data.frame(coefsAll$se.mi))
return(data.frame(coefs1, ses1))
}
y = data.frame(rnorm(10))

meldAllT_stat(cohenD, y)
```

###############
INQF1 Time and T1
###############
```{r, echo=FALSE}
output = list()
outputReg = list()
coef_output =  NULL
se_output = NULL
rSquared = NULL
stdBeta = NULL
stdCoef = NULL
stdSe = NULL


for(i in 1:m){
  output[[i]] = lmer(INQTotalScoreF1 ~ Time + (1 | ID), data  = datAnalysisT1[[i]])
  outputReg[[i]] = output[[i]]
  stdBeta[[i]] = std.coef(output[[i]], partial.sd = TRUE) 
  stdCoef[[i]] = stdBeta[[i]][2,1]
  stdSe[[i]] = stdBeta[[i]][2,2]
  rSquared[[i]] = r.squaredGLMM(output[[i]])
  output[[i]] = summary(output[[i]])
  coef_output[[i]] = output[[i]]$coefficients[,1]
  se_output[[i]] = output[[i]]$coefficients[,2]
}
coef_output = data.frame(coef_output)

quickTrans = function(x){
  x = data.frame(x)
  x = t(x)
  x = data.frame(x)
}
coef_output = quickTrans(coef_output)
se_output = quickTrans(se_output)

stdBetaOutput = data.frame(stdCoef)
stdSeOutput = data.frame(stdSe)

coef_output = data.frame(coef_output, stdBetaOutput)
se_output = data.frame(se_output, stdSeOutput)

# Figure out the degrees of freedom 

#coefsAll = mi.meld(q = coef_output, se = se_output)

meldAllT_stat = function(x,y){
  coefsAll = mi.meld(q = x, se = y)
  coefs1 = t(data.frame(coefsAll$q.mi))
  ses1 = t(data.frame(coefsAll$se.mi))
  z_stat = coefs1/ses1
  p = 2*pnorm(-abs(z_stat))
  return(data.frame(coefs1, ses1, z_stat, p))
}

results = meldAllT_stat(coef_output, se_output)
round(results,3)
```
################
Cohen's D INQ T1
################
```{r}
datTimeTreat = datAnalysisT1

library(psych)
cohenDat = NULL
INQTotalT0 = NULL
INQTotalT1 = NULL
for(i in 1:m){
INQTotalT0[[i]] = subset(datTimeTreat[[i]], Time == 0)
INQTotalT1[[i]] = subset(datTimeTreat[[i]], Time == 1)
cohenDat[[i]] = mean(INQTotalT1[[i]]$INQTotalScoreF1-INQTotalT0[[i]]$INQTotalScoreF1)/sqrt((sd(INQTotalT1[[i]]$INQTotalScoreF1)^2+sd(INQTotalT0[[i]]$INQTotalScoreF1)^2)/2)
}

cohenD = data.frame(cohenDat)

meldAllT_stat = function(x,y){
coefsAll = mi.meld(q = x, se = y)
coefs1 = t(data.frame(coefsAll$q.mi))
ses1 = t(data.frame(coefsAll$se.mi))
return(data.frame(coefs1, ses1))
}
y = data.frame(rnorm(10))

meldAllT_stat(cohenD, y)
```
###############
INQ Time and T2
###############
```{r, echo=FALSE}
output = list()
outputReg = list()
coef_output =  NULL
se_output = NULL
rSquared = NULL
stdBeta = NULL
stdCoef = NULL
stdSe = NULL


for(i in 1:m){
output[[i]] = lmer(INQTotalScoreF1 ~ Time + (1 | ID), data  = datAnalysisT2[[i]])
outputReg[[i]] = output[[i]]
stdBeta[[i]] = std.coef(output[[i]], partial.sd = TRUE) 
stdCoef[[i]] = stdBeta[[i]][2,1]
stdSe[[i]] = stdBeta[[i]][2,2]
rSquared[[i]] = r.squaredGLMM(output[[i]])
output[[i]] = summary(output[[i]])
coef_output[[i]] = output[[i]]$coefficients[,1]
se_output[[i]] = output[[i]]$coefficients[,2]
}
coef_output = data.frame(coef_output)

quickTrans = function(x){
x = data.frame(x)
x = t(x)
x = data.frame(x)
}
coef_output = quickTrans(coef_output)
se_output = quickTrans(se_output)

stdBetaOutput = data.frame(stdCoef)
stdSeOutput = data.frame(stdSe)

coef_output = data.frame(coef_output, stdBetaOutput)
se_output = data.frame(se_output, stdSeOutput)

# Figure out the degrees of freedom 

#coefsAll = mi.meld(q = coef_output, se = se_output)

meldAllT_stat = function(x,y){
coefsAll = mi.meld(q = x, se = y)
coefs1 = t(data.frame(coefsAll$q.mi))
ses1 = t(data.frame(coefsAll$se.mi))
z_stat = coefs1/ses1
p = 2*pnorm(-abs(z_stat))
return(data.frame(coefs1, ses1, z_stat, p))
}

results = meldAllT_stat(coef_output, se_output)
round(results,3)
```
################
Cohen's D INQ T2
################
```{r}
datTimeTreat = datAnalysisT2

library(psych)
cohenDat = NULL
INQTotalT0 = NULL
INQTotalT1 = NULL
for(i in 1:m){
  INQTotalT0[[i]] = subset(datTimeTreat[[i]], Time == 0)
  INQTotalT1[[i]] = subset(datTimeTreat[[i]], Time == 1)
  cohenDat[[i]] = mean(INQTotalT1[[i]]$INQTotalScoreF1-INQTotalT0[[i]]$INQTotalScoreF1)/sqrt((sd(INQTotalT1[[i]]$INQTotalScoreF1)^2+sd(INQTotalT0[[i]]$INQTotalScoreF1)^2)/2)
}

cohenD = data.frame(cohenDat)

meldAllT_stat = function(x,y){
  coefsAll = mi.meld(q = x, se = y)
  coefs1 = t(data.frame(coefsAll$q.mi))
  ses1 = t(data.frame(coefsAll$se.mi))
  return(data.frame(coefs1, ses1))
}
y = data.frame(rnorm(10))

meldAllT_stat(cohenD, y)
```
###############
INQF2 Time and T1
###############
```{r, echo=FALSE}
output = list()
outputReg = list()
coef_output =  NULL
se_output = NULL
rSquared = NULL
stdBeta = NULL
stdCoef = NULL
stdSe = NULL


for(i in 1:m){
  output[[i]] = lmer(INQTotalScoreF2 ~ Time + (1 | ID), data  = datAnalysisT1[[i]])
  outputReg[[i]] = output[[i]]
  stdBeta[[i]] = std.coef(output[[i]], partial.sd = TRUE) 
  stdCoef[[i]] = stdBeta[[i]][2,1]
  stdSe[[i]] = stdBeta[[i]][2,2]
  rSquared[[i]] = r.squaredGLMM(output[[i]])
  output[[i]] = summary(output[[i]])
  coef_output[[i]] = output[[i]]$coefficients[,1]
  se_output[[i]] = output[[i]]$coefficients[,2]
}
coef_output = data.frame(coef_output)

quickTrans = function(x){
  x = data.frame(x)
  x = t(x)
  x = data.frame(x)
}
coef_output = quickTrans(coef_output)
se_output = quickTrans(se_output)

stdBetaOutput = data.frame(stdCoef)
stdSeOutput = data.frame(stdSe)

coef_output = data.frame(coef_output, stdBetaOutput)
se_output = data.frame(se_output, stdSeOutput)

# Figure out the degrees of freedom 

#coefsAll = mi.meld(q = coef_output, se = se_output)

meldAllT_stat = function(x,y){
  coefsAll = mi.meld(q = x, se = y)
  coefs1 = t(data.frame(coefsAll$q.mi))
  ses1 = t(data.frame(coefsAll$se.mi))
  z_stat = coefs1/ses1
  p = 2*pnorm(-abs(z_stat))
  return(data.frame(coefs1, ses1, z_stat, p))
}

results = meldAllT_stat(coef_output, se_output)
round(results,3)
```
################
Cohen's D INQ T1
################
```{r}
datTimeTreat = datAnalysisT1

library(psych)
cohenDat = NULL
INQTotalT0 = NULL
INQTotalT1 = NULL
for(i in 1:m){
INQTotalT0[[i]] = subset(datTimeTreat[[i]], Time == 0)
INQTotalT1[[i]] = subset(datTimeTreat[[i]], Time == 1)
cohenDat[[i]] = mean(INQTotalT1[[i]]$INQTotalScoreF2-INQTotalT0[[i]]$INQTotalScoreF2)/sqrt((sd(INQTotalT1[[i]]$INQTotalScoreF2)^2+sd(INQTotalT0[[i]]$INQTotalScoreF2)^2)/2)
}

cohenD = data.frame(cohenDat)

meldAllT_stat = function(x,y){
coefsAll = mi.meld(q = x, se = y)
coefs1 = t(data.frame(coefsAll$q.mi))
ses1 = t(data.frame(coefsAll$se.mi))
return(data.frame(coefs1, ses1))
}
y = data.frame(rnorm(10))

meldAllT_stat(cohenD, y)
```
###############
INQ Time and T2
###############
```{r, echo=FALSE}
output = list()
outputReg = list()
coef_output =  NULL
se_output = NULL
rSquared = NULL
stdBeta = NULL
stdCoef = NULL
stdSe = NULL


for(i in 1:m){
output[[i]] = lmer(INQTotalScoreF2 ~ Time + (1 | ID), data  = datAnalysisT2[[i]])
outputReg[[i]] = output[[i]]
stdBeta[[i]] = std.coef(output[[i]], partial.sd = TRUE) 
stdCoef[[i]] = stdBeta[[i]][2,1]
stdSe[[i]] = stdBeta[[i]][2,2]
rSquared[[i]] = r.squaredGLMM(output[[i]])
output[[i]] = summary(output[[i]])
coef_output[[i]] = output[[i]]$coefficients[,1]
se_output[[i]] = output[[i]]$coefficients[,2]
}
coef_output = data.frame(coef_output)

quickTrans = function(x){
x = data.frame(x)
x = t(x)
x = data.frame(x)
}
coef_output = quickTrans(coef_output)
se_output = quickTrans(se_output)

stdBetaOutput = data.frame(stdCoef)
stdSeOutput = data.frame(stdSe)

coef_output = data.frame(coef_output, stdBetaOutput)
se_output = data.frame(se_output, stdSeOutput)

# Figure out the degrees of freedom 

#coefsAll = mi.meld(q = coef_output, se = se_output)

meldAllT_stat = function(x,y){
coefsAll = mi.meld(q = x, se = y)
coefs1 = t(data.frame(coefsAll$q.mi))
ses1 = t(data.frame(coefsAll$se.mi))
z_stat = coefs1/ses1
p = 2*pnorm(-abs(z_stat))
return(data.frame(coefs1, ses1, z_stat, p))
}

results = meldAllT_stat(coef_output, se_output)
round(results,3)
```
################
Cohen's D INQ T2
################
```{r}
datTimeTreat = datAnalysisT2

library(psych)
cohenDat = NULL
INQTotalT0 = NULL
INQTotalT1 = NULL
for(i in 1:m){
  INQTotalT0[[i]] = subset(datTimeTreat[[i]], Time == 0)
  INQTotalT1[[i]] = subset(datTimeTreat[[i]], Time == 1)
  cohenDat[[i]] = mean(INQTotalT1[[i]]$INQTotalScoreF2-INQTotalT0[[i]]$INQTotalScoreF2)/sqrt((sd(INQTotalT1[[i]]$INQTotalScoreF2)^2+sd(INQTotalT0[[i]]$INQTotalScoreF2)^2)/2)
}

cohenD = data.frame(cohenDat)

meldAllT_stat = function(x,y){
  coefsAll = mi.meld(q = x, se = y)
  coefs1 = t(data.frame(coefsAll$q.mi))
  ses1 = t(data.frame(coefsAll$se.mi))
  return(data.frame(coefs1, ses1))
}
y = data.frame(rnorm(10))

meldAllT_stat(cohenD, y)
```

###############
INQ Time and T3
###############
```{r, echo=FALSE}
output = list()
outputReg = list()
coef_output =  NULL
se_output = NULL
rSquared = NULL
stdBeta = NULL
stdCoef = NULL
stdSe = NULL


for(i in 1:m){
  output[[i]] = lmer(INQTotalScoreF2 ~ Time + (1 | ID), data  = datAnalysisT3[[i]])
  outputReg[[i]] = output[[i]]
  stdBeta[[i]] = std.coef(output[[i]], partial.sd = TRUE) 
  stdCoef[[i]] = stdBeta[[i]][2,1]
  stdSe[[i]] = stdBeta[[i]][2,2]
  rSquared[[i]] = r.squaredGLMM(output[[i]])
  output[[i]] = summary(output[[i]])
  coef_output[[i]] = output[[i]]$coefficients[,1]
  se_output[[i]] = output[[i]]$coefficients[,2]
}
coef_output = data.frame(coef_output)

quickTrans = function(x){
  x = data.frame(x)
  x = t(x)
  x = data.frame(x)
}
coef_output = quickTrans(coef_output)
se_output = quickTrans(se_output)

stdBetaOutput = data.frame(stdCoef)
stdSeOutput = data.frame(stdSe)

coef_output = data.frame(coef_output, stdBetaOutput)
se_output = data.frame(se_output, stdSeOutput)

# Figure out the degrees of freedom 

#coefsAll = mi.meld(q = coef_output, se = se_output)

meldAllT_stat = function(x,y){
  coefsAll = mi.meld(q = x, se = y)
  coefs1 = t(data.frame(coefsAll$q.mi))
  ses1 = t(data.frame(coefsAll$se.mi))
  z_stat = coefs1/ses1
  p = 2*pnorm(-abs(z_stat))
  return(data.frame(coefs1, ses1, z_stat, p))
}

results = meldAllT_stat(coef_output, se_output)
round(results,3)
```
################
Cohen's D INQ T3
################
```{r}
datTimeTreat = datAnalysisT3

library(psych)
cohenDat = NULL
INQTotalT0 = NULL
INQTotalT1 = NULL
for(i in 1:m){
INQTotalT0[[i]] = subset(datTimeTreat[[i]], Time == 0)
INQTotalT1[[i]] = subset(datTimeTreat[[i]], Time == 1)
cohenDat[[i]] = mean(INQTotalT1[[i]]$INQTotalScoreF2-INQTotalT0[[i]]$INQTotalScoreF2)/sqrt((sd(INQTotalT1[[i]]$INQTotalScoreF2)^2+sd(INQTotalT0[[i]]$INQTotalScoreF2)^2)/2)
}

cohenD = data.frame(cohenDat)

meldAllT_stat = function(x,y){
coefsAll = mi.meld(q = x, se = y)
coefs1 = t(data.frame(coefsAll$q.mi))
ses1 = t(data.frame(coefsAll$se.mi))
return(data.frame(coefs1, ses1))
}
y = data.frame(rnorm(10))

meldAllT_stat(cohenD, y)
```




###############
SSMI Time and T1
###############
```{r, echo=FALSE}
output = list()
outputReg = list()
coef_output =  NULL
se_output = NULL
rSquared = NULL
stdBeta = NULL
stdCoef = NULL
stdSe = NULL


for(i in 1:m){
  output[[i]] = lmer(SSMITotalScore ~ Time + (1 | ID), data  = datAnalysisT1[[i]])
  outputReg[[i]] = output[[i]]
  stdBeta[[i]] = std.coef(output[[i]], partial.sd = TRUE) 
  stdCoef[[i]] = stdBeta[[i]][2,1]
  stdSe[[i]] = stdBeta[[i]][2,2]
  rSquared[[i]] = r.squaredGLMM(output[[i]])
  output[[i]] = summary(output[[i]])
  coef_output[[i]] = output[[i]]$coefficients[,1]
  se_output[[i]] = output[[i]]$coefficients[,2]
}
coef_output = data.frame(coef_output)

quickTrans = function(x){
  x = data.frame(x)
  x = t(x)
  x = data.frame(x)
}
coef_output = quickTrans(coef_output)
se_output = quickTrans(se_output)

stdBetaOutput = data.frame(stdCoef)
stdSeOutput = data.frame(stdSe)

coef_output = data.frame(coef_output, stdBetaOutput)
se_output = data.frame(se_output, stdSeOutput)

# Figure out the degrees of freedom 

#coefsAll = mi.meld(q = coef_output, se = se_output)

meldAllT_stat = function(x,y){
  coefsAll = mi.meld(q = x, se = y)
  coefs1 = t(data.frame(coefsAll$q.mi))
  ses1 = t(data.frame(coefsAll$se.mi))
  z_stat = coefs1/ses1
  p = 2*pnorm(-abs(z_stat))
  return(data.frame(coefs1, ses1, z_stat, p))
}

results = meldAllT_stat(coef_output, se_output)
round(results,3)
```
################
Cohen's D SSMI T1
################
```{r}
datTimeTreat = datAnalysisT1

library(psych)
cohenDat = NULL
SSMITotalT0 = NULL
SSMITotalT1 = NULL
for(i in 1:m){
SSMITotalT0[[i]] = subset(datTimeTreat[[i]], Time == 0)
SSMITotalT1[[i]] = subset(datTimeTreat[[i]], Time == 1)
cohenDat[[i]] = mean(SSMITotalT1[[i]]$SSMITotal-SSMITotalT0[[i]]$SSMITotal)/sqrt((sd(SSMITotalT1[[i]]$SSMITotal)^2+sd(SSMITotalT0[[i]]$SSMITotal)^2)/2)
}

cohenD = data.frame(cohenDat)

meldAllT_stat = function(x,y){
coefsAll = mi.meld(q = x, se = y)
coefs1 = t(data.frame(coefsAll$q.mi))
ses1 = t(data.frame(coefsAll$se.mi))
return(data.frame(coefs1, ses1))
}
y = data.frame(rnorm(10))

meldAllT_stat(cohenD, y)
```

###############
SSMI Time and T2
###############
```{r, echo=FALSE}
output = list()
outputReg = list()
coef_output =  NULL
se_output = NULL
rSquared = NULL
stdBeta = NULL
stdCoef = NULL
stdSe = NULL


for(i in 1:m){
  output[[i]] = lmer(SSMITotalScore ~ Time + (1 | ID), data  = datAnalysisT2[[i]])
  outputReg[[i]] = output[[i]]
  stdBeta[[i]] = std.coef(output[[i]], partial.sd = TRUE) 
  stdCoef[[i]] = stdBeta[[i]][2,1]
  stdSe[[i]] = stdBeta[[i]][2,2]
  rSquared[[i]] = r.squaredGLMM(output[[i]])
  output[[i]] = summary(output[[i]])
  coef_output[[i]] = output[[i]]$coefficients[,1]
  se_output[[i]] = output[[i]]$coefficients[,2]
}
coef_output = data.frame(coef_output)

quickTrans = function(x){
  x = data.frame(x)
  x = t(x)
  x = data.frame(x)
}
coef_output = quickTrans(coef_output)
se_output = quickTrans(se_output)

stdBetaOutput = data.frame(stdCoef)
stdSeOutput = data.frame(stdSe)

coef_output = data.frame(coef_output, stdBetaOutput)
se_output = data.frame(se_output, stdSeOutput)

# Figure out the degrees of freedom 

#coefsAll = mi.meld(q = coef_output, se = se_output)

meldAllT_stat = function(x,y){
  coefsAll = mi.meld(q = x, se = y)
  coefs1 = t(data.frame(coefsAll$q.mi))
  ses1 = t(data.frame(coefsAll$se.mi))
  z_stat = coefs1/ses1
  p = 2*pnorm(-abs(z_stat))
  return(data.frame(coefs1, ses1, z_stat, p))
}

results = meldAllT_stat(coef_output, se_output)
round(results,3)
```
################
Cohen's D SSMI T2
################
```{r}
datTimeTreat = datAnalysisT2

library(psych)
cohenDat = NULL
SSMITotalT0 = NULL
SSMITotalT1 = NULL
for(i in 1:m){
SSMITotalT0[[i]] = subset(datTimeTreat[[i]], Time == 0)
SSMITotalT1[[i]] = subset(datTimeTreat[[i]], Time == 1)
cohenDat[[i]] = mean(SSMITotalT1[[i]]$SSMITotal-SSMITotalT0[[i]]$SSMITotal)/sqrt((sd(SSMITotalT1[[i]]$SSMITotal)^2+sd(SSMITotalT0[[i]]$SSMITotal)^2)/2)
}

cohenD = data.frame(cohenDat)

meldAllT_stat = function(x,y){
coefsAll = mi.meld(q = x, se = y)
coefs1 = t(data.frame(coefsAll$q.mi))
ses1 = t(data.frame(coefsAll$se.mi))
return(data.frame(coefs1, ses1))
}
y = data.frame(rnorm(10))

meldAllT_stat(cohenD, y)
```

###############
SSMI Time and T3
###############
```{r, echo=FALSE}
output = list()
outputReg = list()
coef_output =  NULL
se_output = NULL
rSquared = NULL
stdBeta = NULL
stdCoef = NULL
stdSe = NULL


for(i in 1:m){
  output[[i]] = lmer(SSMITotalScore ~ Time + (1 | ID), data  = datAnalysisT3[[i]])
  outputReg[[i]] = output[[i]]
  stdBeta[[i]] = std.coef(output[[i]], partial.sd = TRUE) 
  stdCoef[[i]] = stdBeta[[i]][2,1]
  stdSe[[i]] = stdBeta[[i]][2,2]
  rSquared[[i]] = r.squaredGLMM(output[[i]])
  output[[i]] = summary(output[[i]])
  coef_output[[i]] = output[[i]]$coefficients[,1]
  se_output[[i]] = output[[i]]$coefficients[,2]
}
coef_output = data.frame(coef_output)

quickTrans = function(x){
  x = data.frame(x)
  x = t(x)
  x = data.frame(x)
}
coef_output = quickTrans(coef_output)
se_output = quickTrans(se_output)

stdBetaOutput = data.frame(stdCoef)
stdSeOutput = data.frame(stdSe)

coef_output = data.frame(coef_output, stdBetaOutput)
se_output = data.frame(se_output, stdSeOutput)

# Figure out the degrees of freedom 

#coefsAll = mi.meld(q = coef_output, se = se_output)

meldAllT_stat = function(x,y){
  coefsAll = mi.meld(q = x, se = y)
  coefs1 = t(data.frame(coefsAll$q.mi))
  ses1 = t(data.frame(coefsAll$se.mi))
  z_stat = coefs1/ses1
  p = 2*pnorm(-abs(z_stat))
  return(data.frame(coefs1, ses1, z_stat, p))
}

results = meldAllT_stat(coef_output, se_output)
round(results,3)
```
################
Cohen's D SSMI T3
################
```{r}
datTimeTreat = datAnalysisT3

library(psych)
cohenDat = NULL
SSMITotalT0 = NULL
SSMITotalT1 = NULL
for(i in 1:m){
SSMITotalT0[[i]] = subset(datTimeTreat[[i]], Time == 0)
SSMITotalT1[[i]] = subset(datTimeTreat[[i]], Time == 1)
cohenDat[[i]] = mean(SSMITotalT1[[i]]$SSMITotal-SSMITotalT0[[i]]$SSMITotal)/sqrt((sd(SSMITotalT1[[i]]$SSMITotal)^2+sd(SSMITotalT0[[i]]$SSMITotal)^2)/2)
}

cohenD = data.frame(cohenDat)

meldAllT_stat = function(x,y){
coefsAll = mi.meld(q = x, se = y)
coefs1 = t(data.frame(coefsAll$q.mi))
ses1 = t(data.frame(coefsAll$se.mi))
return(data.frame(coefs1, ses1))
}
y = data.frame(rnorm(10))

meldAllT_stat(cohenD, y)
```
###############
SISF1 Time and T1
###############
```{r, echo=FALSE}
output = list()
outputReg = list()
coef_output =  NULL
se_output = NULL
rSquared = NULL
stdBeta = NULL
stdCoef = NULL
stdSe = NULL


for(i in 1:m){
  output[[i]] = lmer(SISTotalScoreF1 ~ Time + (1 | ID), data  = datAnalysisT1[[i]])
  outputReg[[i]] = output[[i]]
  stdBeta[[i]] = std.coef(output[[i]], partial.sd = TRUE) 
  stdCoef[[i]] = stdBeta[[i]][2,1]
  stdSe[[i]] = stdBeta[[i]][2,2]
  rSquared[[i]] = r.squaredGLMM(output[[i]])
  output[[i]] = summary(output[[i]])
  coef_output[[i]] = output[[i]]$coefficients[,1]
  se_output[[i]] = output[[i]]$coefficients[,2]
}
coef_output = data.frame(coef_output)

quickTrans = function(x){
  x = data.frame(x)
  x = t(x)
  x = data.frame(x)
}
coef_output = quickTrans(coef_output)
se_output = quickTrans(se_output)

stdBetaOutput = data.frame(stdCoef)
stdSeOutput = data.frame(stdSe)

coef_output = data.frame(coef_output, stdBetaOutput)
se_output = data.frame(se_output, stdSeOutput)

# Figure out the degrees of freedom 

#coefsAll = mi.meld(q = coef_output, se = se_output)

meldAllT_stat = function(x,y){
  coefsAll = mi.meld(q = x, se = y)
  coefs1 = t(data.frame(coefsAll$q.mi))
  ses1 = t(data.frame(coefsAll$se.mi))
  z_stat = coefs1/ses1
  p = 2*pnorm(-abs(z_stat))
  return(data.frame(coefs1, ses1, z_stat, p))
}

results = meldAllT_stat(coef_output, se_output)
round(results,3)
```
################
Cohen's D SIS T1
################
```{r}
datTimeTreat = datAnalysisT1

library(psych)
cohenDat = NULL
SISTotalT0 = NULL
SISTotalT1 = NULL
for(i in 1:m){
SISTotalT0[[i]] = subset(datTimeTreat[[i]], Time == 0)
SISTotalT1[[i]] = subset(datTimeTreat[[i]], Time == 1)
cohenDat[[i]] = mean(SISTotalT1[[i]]$SISTotalScoreF1-SISTotalT0[[i]]$SISTotalScoreF1)/sqrt((sd(SISTotalT1[[i]]$SISTotalScoreF1)^2+sd(SISTotalT0[[i]]$SISTotalScoreF1)^2)/2)
}

cohenD = data.frame(cohenDat)

meldAllT_stat = function(x,y){
coefsAll = mi.meld(q = x, se = y)
coefs1 = t(data.frame(coefsAll$q.mi))
ses1 = t(data.frame(coefsAll$se.mi))
return(data.frame(coefs1, ses1))
}
y = data.frame(rnorm(10))

meldAllT_stat(cohenD, y)
```

###############
SISF1 Time and T2
###############
```{r, echo=FALSE}
output = list()
outputReg = list()
coef_output =  NULL
se_output = NULL
rSquared = NULL
stdBeta = NULL
stdCoef = NULL
stdSe = NULL


for(i in 1:m){
output[[i]] = lmer(SISTotalScoreF1 ~ Time + (1 | ID), data  = datAnalysisT2[[i]])
outputReg[[i]] = output[[i]]
stdBeta[[i]] = std.coef(output[[i]], partial.sd = TRUE) 
stdCoef[[i]] = stdBeta[[i]][2,1]
stdSe[[i]] = stdBeta[[i]][2,2]
rSquared[[i]] = r.squaredGLMM(output[[i]])
output[[i]] = summary(output[[i]])
coef_output[[i]] = output[[i]]$coefficients[,1]
se_output[[i]] = output[[i]]$coefficients[,2]
}
coef_output = data.frame(coef_output)

quickTrans = function(x){
x = data.frame(x)
x = t(x)
x = data.frame(x)
}
coef_output = quickTrans(coef_output)
se_output = quickTrans(se_output)

stdBetaOutput = data.frame(stdCoef)
stdSeOutput = data.frame(stdSe)

coef_output = data.frame(coef_output, stdBetaOutput)
se_output = data.frame(se_output, stdSeOutput)

# Figure out the degrees of freedom 

#coefsAll = mi.meld(q = coef_output, se = se_output)

meldAllT_stat = function(x,y){
coefsAll = mi.meld(q = x, se = y)
coefs1 = t(data.frame(coefsAll$q.mi))
ses1 = t(data.frame(coefsAll$se.mi))
z_stat = coefs1/ses1
p = 2*pnorm(-abs(z_stat))
return(data.frame(coefs1, ses1, z_stat, p))
}

results = meldAllT_stat(coef_output, se_output)
round(results,3)
```
################
Cohen's D SIS T2
################
```{r}
datTimeTreat = datAnalysisT2

library(psych)
cohenDat = NULL
SISTotalT0 = NULL
SISTotalT1 = NULL
for(i in 1:m){
  SISTotalT0[[i]] = subset(datTimeTreat[[i]], Time == 0)
  SISTotalT1[[i]] = subset(datTimeTreat[[i]], Time == 1)
  cohenDat[[i]] = mean(SISTotalT1[[i]]$SISTotalScoreF1-SISTotalT0[[i]]$SISTotalScoreF1)/sqrt((sd(SISTotalT1[[i]]$SISTotalScoreF1)^2+sd(SISTotalT0[[i]]$SISTotalScoreF1)^2)/2)
}

cohenD = data.frame(cohenDat)

meldAllT_stat = function(x,y){
  coefsAll = mi.meld(q = x, se = y)
  coefs1 = t(data.frame(coefsAll$q.mi))
  ses1 = t(data.frame(coefsAll$se.mi))
  return(data.frame(coefs1, ses1))
}
y = data.frame(rnorm(10))

meldAllT_stat(cohenD, y)
```

###############
SIS Time and T3
###############
```{r, echo=FALSE}
output = list()
outputReg = list()
coef_output =  NULL
se_output = NULL
rSquared = NULL
stdBeta = NULL
stdCoef = NULL
stdSe = NULL


for(i in 1:m){
  output[[i]] = lmer(SISTotalScoreF1 ~ Time + (1 | ID), data  = datAnalysisT3[[i]])
  outputReg[[i]] = output[[i]]
  stdBeta[[i]] = std.coef(output[[i]], partial.sd = TRUE) 
  stdCoef[[i]] = stdBeta[[i]][2,1]
  stdSe[[i]] = stdBeta[[i]][2,2]
  rSquared[[i]] = r.squaredGLMM(output[[i]])
  output[[i]] = summary(output[[i]])
  coef_output[[i]] = output[[i]]$coefficients[,1]
  se_output[[i]] = output[[i]]$coefficients[,2]
}
coef_output = data.frame(coef_output)

quickTrans = function(x){
  x = data.frame(x)
  x = t(x)
  x = data.frame(x)
}
coef_output = quickTrans(coef_output)
se_output = quickTrans(se_output)

stdBetaOutput = data.frame(stdCoef)
stdSeOutput = data.frame(stdSe)

coef_output = data.frame(coef_output, stdBetaOutput)
se_output = data.frame(se_output, stdSeOutput)

# Figure out the degrees of freedo 

#coefsAll = mi.meld(q = coef_output, se = se_output)

meldAllT_stat = function(x,y){
  coefsAll = mi.meld(q = x, se = y)
  coefs1 = t(data.frame(coefsAll$q.mi))
  ses1 = t(data.frame(coefsAll$se.mi))
  z_stat = coefs1/ses1
  p = 2*pnorm(-abs(z_stat))
  return(data.frame(coefs1, ses1, z_stat, p))
}

results = meldAllT_stat(coef_output, se_output)
round(results,3)
```
################
Cohen's D SIS T3
################
```{r}
datTimeTreat = datAnalysisT3

library(psych)
cohenDat = NULL
SISTotalT0 = NULL
SISTotalT1 = NULL
for(i in 1:m){
SISTotalT0[[i]] = subset(datTimeTreat[[i]], Time == 0)
SISTotalT1[[i]] = subset(datTimeTreat[[i]], Time == 1)
cohenDat[[i]] = mean(SISTotalT1[[i]]$SISTotalScoreF1-SISTotalT0[[i]]$SISTotalScoreF1)/sqrt((sd(SISTotalT1[[i]]$SISTotalScoreF1)^2+sd(SISTotalT0[[i]]$SISTotalScoreF1)^2)/2)
}

cohenD = data.frame(cohenDat)

meldAllT_stat = function(x,y){
coefsAll = mi.meld(q = x, se = y)
coefs1 = t(data.frame(coefsAll$q.mi))
ses1 = t(data.frame(coefsAll$se.mi))
return(data.frame(coefs1, ses1))
}
y = data.frame(rnorm(10))

meldAllT_stat(cohenD, y)
```
###############
SISF2 Time and T1
###############
```{r, echo=FALSE}
output = list()
outputReg = list()
coef_output =  NULL
se_output = NULL
rSquared = NULL
stdBeta = NULL
stdCoef = NULL
stdSe = NULL


for(i in 1:m){
  output[[i]] = lmer(SISTotalScoreF2 ~ Time + (1 | ID), data  = datAnalysisT1[[i]])
  outputReg[[i]] = output[[i]]
  stdBeta[[i]] = std.coef(output[[i]], partial.sd = TRUE) 
  stdCoef[[i]] = stdBeta[[i]][2,1]
  stdSe[[i]] = stdBeta[[i]][2,2]
  rSquared[[i]] = r.squaredGLMM(output[[i]])
  output[[i]] = summary(output[[i]])
  coef_output[[i]] = output[[i]]$coefficients[,1]
  se_output[[i]] = output[[i]]$coefficients[,2]
}
coef_output = data.frame(coef_output)

quickTrans = function(x){
  x = data.frame(x)
  x = t(x)
  x = data.frame(x)
}
coef_output = quickTrans(coef_output)
se_output = quickTrans(se_output)

stdBetaOutput = data.frame(stdCoef)
stdSeOutput = data.frame(stdSe)

coef_output = data.frame(coef_output, stdBetaOutput)
se_output = data.frame(se_output, stdSeOutput)

# Figure out the degrees of freedom 

#coefsAll = mi.meld(q = coef_output, se = se_output)

meldAllT_stat = function(x,y){
  coefsAll = mi.meld(q = x, se = y)
  coefs1 = t(data.frame(coefsAll$q.mi))
  ses1 = t(data.frame(coefsAll$se.mi))
  z_stat = coefs1/ses1
  p = 2*pnorm(-abs(z_stat))
  return(data.frame(coefs1, ses1, z_stat, p))
}

results = meldAllT_stat(coef_output, se_output)
round(results,3)
```
################
Cohen's D SIS T1
################
```{r}
datTimeTreat = datAnalysisT1

library(psych)
cohenDat = NULL
SISTotalT0 = NULL
SISTotalT1 = NULL
for(i in 1:m){
SISTotalT0[[i]] = subset(datTimeTreat[[i]], Time == 0)
SISTotalT1[[i]] = subset(datTimeTreat[[i]], Time == 1)
cohenDat[[i]] = mean(SISTotalT1[[i]]$SISTotalScoreF2-SISTotalT0[[i]]$SISTotalScoreF2)/sqrt((sd(SISTotalT1[[i]]$SISTotalScoreF2)^2+sd(SISTotalT0[[i]]$SISTotalScoreF2)^2)/2)
}

cohenD = data.frame(cohenDat)

meldAllT_stat = function(x,y){
coefsAll = mi.meld(q = x, se = y)
coefs1 = t(data.frame(coefsAll$q.mi))
ses1 = t(data.frame(coefsAll$se.mi))
return(data.frame(coefs1, ses1))
}
y = data.frame(rnorm(10))

meldAllT_stat(cohenD, y)
```

###############
SISF1 Time and T2
###############
```{r, echo=FALSE}
output = list()
outputReg = list()
coef_output =  NULL
se_output = NULL
rSquared = NULL
stdBeta = NULL
stdCoef = NULL
stdSe = NULL


for(i in 1:m){
output[[i]] = lmer(SISTotalScoreF2 ~ Time + (1 | ID), data  = datAnalysisT2[[i]])
outputReg[[i]] = output[[i]]
stdBeta[[i]] = std.coef(output[[i]], partial.sd = TRUE) 
stdCoef[[i]] = stdBeta[[i]][2,1]
stdSe[[i]] = stdBeta[[i]][2,2]
rSquared[[i]] = r.squaredGLMM(output[[i]])
output[[i]] = summary(output[[i]])
coef_output[[i]] = output[[i]]$coefficients[,1]
se_output[[i]] = output[[i]]$coefficients[,2]
}
coef_output = data.frame(coef_output)

quickTrans = function(x){
x = data.frame(x)
x = t(x)
x = data.frame(x)
}
coef_output = quickTrans(coef_output)
se_output = quickTrans(se_output)

stdBetaOutput = data.frame(stdCoef)
stdSeOutput = data.frame(stdSe)

coef_output = data.frame(coef_output, stdBetaOutput)
se_output = data.frame(se_output, stdSeOutput)

# Figure out the degrees of freedom 

#coefsAll = mi.meld(q = coef_output, se = se_output)

meldAllT_stat = function(x,y){
coefsAll = mi.meld(q = x, se = y)
coefs1 = t(data.frame(coefsAll$q.mi))
ses1 = t(data.frame(coefsAll$se.mi))
z_stat = coefs1/ses1
p = 2*pnorm(-abs(z_stat))
return(data.frame(coefs1, ses1, z_stat, p))
}

results = meldAllT_stat(coef_output, se_output)
round(results,3)
```
################
Cohen's D SIS T2
################
```{r}
datTimeTreat = datAnalysisT2

library(psych)
cohenDat = NULL
SISTotalT0 = NULL
SISTotalT1 = NULL
for(i in 1:m){
  SISTotalT0[[i]] = subset(datTimeTreat[[i]], Time == 0)
  SISTotalT1[[i]] = subset(datTimeTreat[[i]], Time == 1)
  cohenDat[[i]] = mean(SISTotalT1[[i]]$SISTotalScoreF2-SISTotalT0[[i]]$SISTotalScoreF2)/sqrt((sd(SISTotalT1[[i]]$SISTotalScoreF2)^2+sd(SISTotalT0[[i]]$SISTotalScoreF2)^2)/2)
}

cohenD = data.frame(cohenDat)

meldAllT_stat = function(x,y){
  coefsAll = mi.meld(q = x, se = y)
  coefs1 = t(data.frame(coefsAll$q.mi))
  ses1 = t(data.frame(coefsAll$se.mi))
  return(data.frame(coefs1, ses1))
}
y = data.frame(rnorm(10))

meldAllT_stat(cohenD, y)
```

###############
SIS Time and T3
###############
```{r, echo=FALSE}
output = list()
outputReg = list()
coef_output =  NULL
se_output = NULL
rSquared = NULL
stdBeta = NULL
stdCoef = NULL
stdSe = NULL


for(i in 1:m){
  output[[i]] = lmer(SISTotalScoreF2 ~ Time + (1 | ID), data  = datAnalysisT3[[i]])
  outputReg[[i]] = output[[i]]
  stdBeta[[i]] = std.coef(output[[i]], partial.sd = TRUE) 
  stdCoef[[i]] = stdBeta[[i]][2,1]
  stdSe[[i]] = stdBeta[[i]][2,2]
  rSquared[[i]] = r.squaredGLMM(output[[i]])
  output[[i]] = summary(output[[i]])
  coef_output[[i]] = output[[i]]$coefficients[,1]
  se_output[[i]] = output[[i]]$coefficients[,2]
}
coef_output = data.frame(coef_output)

quickTrans = function(x){
  x = data.frame(x)
  x = t(x)
  x = data.frame(x)
}
coef_output = quickTrans(coef_output)
se_output = quickTrans(se_output)

stdBetaOutput = data.frame(stdCoef)
stdSeOutput = data.frame(stdSe)

coef_output = data.frame(coef_output, stdBetaOutput)
se_output = data.frame(se_output, stdSeOutput)

# Figure out the degrees of freedo 

#coefsAll = mi.meld(q = coef_output, se = se_output)

meldAllT_stat = function(x,y){
  coefsAll = mi.meld(q = x, se = y)
  coefs1 = t(data.frame(coefsAll$q.mi))
  ses1 = t(data.frame(coefsAll$se.mi))
  z_stat = coefs1/ses1
  p = 2*pnorm(-abs(z_stat))
  return(data.frame(coefs1, ses1, z_stat, p))
}

results = meldAllT_stat(coef_output, se_output)
round(results,3)
```
################
Cohen's D SIS T3
################
```{r}
datTimeTreat = datAnalysisT3

library(psych)
cohenDat = NULL
SISTotalT0 = NULL
SISTotalT1 = NULL
for(i in 1:m){
SISTotalT0[[i]] = subset(datTimeTreat[[i]], Time == 0)
SISTotalT1[[i]] = subset(datTimeTreat[[i]], Time == 1)
cohenDat[[i]] = mean(SISTotalT1[[i]]$SISTotalScoreF2-SISTotalT0[[i]]$SISTotalScoreF2)/sqrt((sd(SISTotalT1[[i]]$SISTotalScoreF2)^2+sd(SISTotalT0[[i]]$SISTotalScoreF2)^2)/2)
}

cohenD = data.frame(cohenDat)

meldAllT_stat = function(x,y){
coefsAll = mi.meld(q = x, se = y)
coefs1 = t(data.frame(coefsAll$q.mi))
ses1 = t(data.frame(coefsAll$se.mi))
return(data.frame(coefs1, ses1))
}
y = data.frame(rnorm(10))

meldAllT_stat(cohenD, y)
```
##################################
All treatmentments diff Score RASF1
##################################
```{r}
output = NULL
outputSummary = NULL
coef_output = NULL
se_output = NULL


for(i in 1:m){
  output[[i]] = lm(RASDiffF1 ~ factor(Treatment) + Gender + Race + Edu + Employment, data = datWideAnalysis[[i]])
  outputSummary[[i]] = summary(output[[i]])
  coef_output[[i]] = outputSummary[[i]]$coefficients[,1]
  se_output[[i]] = outputSummary[[i]]$coefficients[,2]
}
coef_output = data.frame(t(data.frame(coef_output))) 
se_output = data.frame(t(data.frame(se_output)))

meldAllT_stat = function(x,y){
  coefsAll = mi.meld(q = x, se = y)
  coefs1 = t(data.frame(coefsAll$q.mi))
  ses1 = t(data.frame(coefsAll$se.mi))
  t_stat = coefs1/ses1
  p = 2*(pt(-abs(t_stat), df = outputSummary[[2]]$df[2]))
  return(data.frame(coefs1, ses1, t_stat, p))
}


results = meldAllT_stat(coef_output, se_output)
round(results, 3)

# Make sure things make sense i.e. difference in means
compmeans(datWideAnalysis[[1]]$RASDiffF1, datWideAnalysis[[1]]$Treatment)

```
Contrasts
```{r}
K = matrix(c(0, 1, -1, 0,0,0,0), 1)
library(multcomp)
t = NULL
for(i in 1:m){
  t[[i]] = glht(output[[i]], linfct = K)
  t[[i]] = summary(t[[i]])
}
t
```
############################################
Cohen's D for Treatment 2 versus Treatment 1
############################################
```{r}
datWideAnalysisT12 = NULL

for(i in 1:m){
  datWideAnalysisT12[[i]] = subset(datWideAnalysis[[i]], Treatment == 1 | Treatment == 2)
}


cohenDat = NULL
for(i in 1:m){
  cohenDat[[i]] = cohen.d(datWideAnalysisT12[[i]]$RASDiffF1, datWideAnalysisT12[[i]]$Treatment, hedges.correction = TRUE)
  cohenDat[[i]] = cohenDat[[i]]$estimate
}

cohenD = data.frame(t(data.frame(cohenDat)))

meldAllT_stat = function(x,y){
  coefsAll = mi.meld(q = x, se = y)
  coefs1 = t(data.frame(coefsAll$q.mi))
  ses1 = t(data.frame(coefsAll$se.mi))
  return(data.frame(coefs1, ses1))
}
y = data.frame(rnorm(10))

meldAllT_stat(cohenD, y)
```
############################################
Cohen's D for Treatment 1 and 3
############################################
```{r}
datWideAnalysisT13 = NULL

for(i in 1:m){
  datWideAnalysisT13[[i]] = subset(datWideAnalysis[[i]], Treatment == 1 | Treatment == 3)
}


cohenDat = NULL
for(i in 1:m){
  cohenDat[[i]] = cohen.d(datWideAnalysisT13[[i]]$RASDiffF1, datWideAnalysisT13[[i]]$Treatment, hedges.correction = TRUE)
  cohenDat[[i]] = cohenDat[[i]]$estimate
}

cohenD = data.frame(t(data.frame(cohenDat)))

meldAllT_stat = function(x,y){
  coefsAll = mi.meld(q = x, se = y)
  coefs1 = t(data.frame(coefsAll$q.mi))
  ses1 = t(data.frame(coefsAll$se.mi))
  return(data.frame(coefs1, ses1))
}
y = data.frame(rnorm(10))

meldAllT_stat(cohenD, y)
```
############################################
Cohen's D for Treatment 3 versus Treatment 2
############################################
```{r}
datWideAnalysisT23 = NULL

for(i in 1:m){
  datWideAnalysisT23[[i]] = subset(datWideAnalysis[[i]], Treatment == 2 | Treatment == 3)
}


cohenDat = NULL
for(i in 1:m){
  cohenDat[[i]] = cohen.d(datWideAnalysisT23[[i]]$RASDiffF1, datWideAnalysisT23[[i]]$Treatment, hedges.correction = TRUE)
  cohenDat[[i]] = cohenDat[[i]]$estimate
}

cohenD = data.frame(t(data.frame(cohenDat)))

meldAllT_stat = function(x,y){
  coefsAll = mi.meld(q = x, se = y)
  coefs1 = t(data.frame(coefsAll$q.mi))
  ses1 = t(data.frame(coefsAll$se.mi))
  return(data.frame(coefs1, ses1))
}
y = data.frame(rnorm(10))

meldAllT_stat(cohenD, y)
```
##################################
All treatmentments diff Score RASF2
##################################
```{r}
output = NULL
outputSummary = NULL
coef_output = NULL
se_output = NULL


for(i in 1:m){
  output[[i]] = lm(RASDiffF2 ~ factor(Treatment) + Gender + Race + Edu + Employment, data = datWideAnalysis[[i]])
  outputSummary[[i]] = summary(output[[i]])
  coef_output[[i]] = outputSummary[[i]]$coefficients[,1]
  se_output[[i]] = outputSummary[[i]]$coefficients[,2]
}
coef_output = data.frame(t(data.frame(coef_output))) 
se_output = data.frame(t(data.frame(se_output)))

meldAllT_stat = function(x,y){
  coefsAll = mi.meld(q = x, se = y)
  coefs1 = t(data.frame(coefsAll$q.mi))
  ses1 = t(data.frame(coefsAll$se.mi))
  t_stat = coefs1/ses1
  p = 2*(pt(-abs(t_stat), df = outputSummary[[2]]$df[2]))
  return(data.frame(coefs1, ses1, t_stat, p))
}


results = meldAllT_stat(coef_output, se_output)
round(results, 3)

# Make sure things make sense i.e. difference in means
compmeans(datWideAnalysis[[1]]$RASDiffF2, datWideAnalysis[[1]]$Treatment)

```
Contrasts
```{r}
K = matrix(c(0, 1, -1, 0,0,0,0), 1)
library(multcomp)
t = NULL
for(i in 1:m){
  t[[i]] = glht(output[[i]], linfct = K)
  t[[i]] = summary(t[[i]])
}
t
```
############################################
Cohen's D for Treatment 2 versus Treatment 1
############################################
```{r}
datWideAnalysisT12 = NULL

for(i in 1:m){
datWideAnalysisT12[[i]] = subset(datWideAnalysis[[i]], Treatment == 1 | Treatment == 2)
}


cohenDat = NULL
for(i in 1:m){
cohenDat[[i]] = cohen.d(datWideAnalysisT12[[i]]$RASDiffF2, datWideAnalysisT12[[i]]$Treatment, hedges.correction = TRUE)
cohenDat[[i]] = cohenDat[[i]]$estimate
}

cohenD = data.frame(t(data.frame(cohenDat)))

meldAllT_stat = function(x,y){
coefsAll = mi.meld(q = x, se = y)
coefs1 = t(data.frame(coefsAll$q.mi))
ses1 = t(data.frame(coefsAll$se.mi))
return(data.frame(coefs1, ses1))
}
y = data.frame(rnorm(10))

meldAllT_stat(cohenD, y)
```
############################################
Cohen's D for Treatment 1 and 3
############################################
```{r}
datWideAnalysisT13 = NULL

for(i in 1:m){
  datWideAnalysisT13[[i]] = subset(datWideAnalysis[[i]], Treatment == 1 | Treatment == 3)
}


cohenDat = NULL
for(i in 1:m){
  cohenDat[[i]] = cohen.d(datWideAnalysisT13[[i]]$RASDiffF2, datWideAnalysisT13[[i]]$Treatment, hedges.correction = TRUE)
  cohenDat[[i]] = cohenDat[[i]]$estimate
}

cohenD = data.frame(t(data.frame(cohenDat)))

meldAllT_stat = function(x,y){
  coefsAll = mi.meld(q = x, se = y)
  coefs1 = t(data.frame(coefsAll$q.mi))
  ses1 = t(data.frame(coefsAll$se.mi))
  return(data.frame(coefs1, ses1))
}
y = data.frame(rnorm(10))

meldAllT_stat(cohenD, y)
```
############################################
Cohen's D for Treatment 3 versus Treatment 2
############################################
```{r}
datWideAnalysisT23 = NULL

for(i in 1:m){
datWideAnalysisT23[[i]] = subset(datWideAnalysis[[i]], Treatment == 2 | Treatment == 3)
}


cohenDat = NULL
for(i in 1:m){
cohenDat[[i]] = cohen.d(datWideAnalysisT23[[i]]$RASDiffF2, datWideAnalysisT23[[i]]$Treatment, hedges.correction = TRUE)
cohenDat[[i]] = cohenDat[[i]]$estimate
}

cohenD = data.frame(t(data.frame(cohenDat)))

meldAllT_stat = function(x,y){
coefsAll = mi.meld(q = x, se = y)
coefs1 = t(data.frame(coefsAll$q.mi))
ses1 = t(data.frame(coefsAll$se.mi))
return(data.frame(coefs1, ses1))
}
y = data.frame(rnorm(10))

meldAllT_stat(cohenD, y)
```
##################################
All treatmentments diff Score RASF3
##################################
```{r}
output = NULL
outputSummary = NULL
coef_output = NULL
se_output = NULL


for(i in 1:m){
  output[[i]] = lm(RASDiffF3 ~ factor(Treatment) + Gender + Race + Edu + Employment, data = datWideAnalysis[[i]])
  outputSummary[[i]] = summary(output[[i]])
  coef_output[[i]] = outputSummary[[i]]$coefficients[,1]
  se_output[[i]] = outputSummary[[i]]$coefficients[,2]
}
coef_output = data.frame(t(data.frame(coef_output))) 
se_output = data.frame(t(data.frame(se_output)))

meldAllT_stat = function(x,y){
  coefsAll = mi.meld(q = x, se = y)
  coefs1 = t(data.frame(coefsAll$q.mi))
  ses1 = t(data.frame(coefsAll$se.mi))
  t_stat = coefs1/ses1
  p = 2*(pt(-abs(t_stat), df = outputSummary[[2]]$df[2]))
  return(data.frame(coefs1, ses1, t_stat, p))
}


results = meldAllT_stat(coef_output, se_output)
round(results, 3)

# Make sure things make sense i.e. difference in means
compmeans(datWideAnalysis[[1]]$RASDiffF3, datWideAnalysis[[1]]$Treatment)

```
Contrasts
```{r}
K = matrix(c(0, 1, -1, 0,0,0,0), 1)
library(multcomp)
t = NULL
for(i in 1:m){
  t[[i]] = glht(output[[i]], linfct = K)
  t[[i]] = summary(t[[i]])
}
t
```
############################################
Cohen's D for Treatment 2 versus Treatment 1
############################################
```{r}
datWideAnalysisT12 = NULL

for(i in 1:m){
datWideAnalysisT12[[i]] = subset(datWideAnalysis[[i]], Treatment == 1 | Treatment == 2)
}


cohenDat = NULL
for(i in 1:m){
cohenDat[[i]] = cohen.d(datWideAnalysisT12[[i]]$RASDiffF3, datWideAnalysisT12[[i]]$Treatment, hedges.correction = TRUE)
cohenDat[[i]] = cohenDat[[i]]$estimate
}

cohenD = data.frame(t(data.frame(cohenDat)))

meldAllT_stat = function(x,y){
coefsAll = mi.meld(q = x, se = y)
coefs1 = t(data.frame(coefsAll$q.mi))
ses1 = t(data.frame(coefsAll$se.mi))
return(data.frame(coefs1, ses1))
}
y = data.frame(rnorm(10))

meldAllT_stat(cohenD, y)
```
############################################
Cohen's D for Treatment 1 and 3
############################################
```{r}
datWideAnalysisT13 = NULL

for(i in 1:m){
  datWideAnalysisT13[[i]] = subset(datWideAnalysis[[i]], Treatment == 1 | Treatment == 3)
}


cohenDat = NULL
for(i in 1:m){
  cohenDat[[i]] = cohen.d(datWideAnalysisT13[[i]]$RASDiffF3, datWideAnalysisT13[[i]]$Treatment, hedges.correction = TRUE)
  cohenDat[[i]] = cohenDat[[i]]$estimate
}

cohenD = data.frame(t(data.frame(cohenDat)))

meldAllT_stat = function(x,y){
  coefsAll = mi.meld(q = x, se = y)
  coefs1 = t(data.frame(coefsAll$q.mi))
  ses1 = t(data.frame(coefsAll$se.mi))
  return(data.frame(coefs1, ses1))
}
y = data.frame(rnorm(10))

meldAllT_stat(cohenD, y)
```
############################################
Cohen's D for Treatment 3 versus Treatment 2
############################################
```{r}
datWideAnalysisT23 = NULL

for(i in 1:m){
datWideAnalysisT23[[i]] = subset(datWideAnalysis[[i]], Treatment == 2 | Treatment == 3)
}


cohenDat = NULL
for(i in 1:m){
cohenDat[[i]] = cohen.d(datWideAnalysisT23[[i]]$RASDiffF3, datWideAnalysisT23[[i]]$Treatment, hedges.correction = TRUE)
cohenDat[[i]] = cohenDat[[i]]$estimate
}

cohenD = data.frame(t(data.frame(cohenDat)))

meldAllT_stat = function(x,y){
coefsAll = mi.meld(q = x, se = y)
coefs1 = t(data.frame(coefsAll$q.mi))
ses1 = t(data.frame(coefsAll$se.mi))
return(data.frame(coefs1, ses1))
}
y = data.frame(rnorm(10))

meldAllT_stat(cohenD, y)
```
##################################
All treatmentments diff Score RASF5
##################################
```{r}
output = NULL
outputSummary = NULL
coef_output = NULL
se_output = NULL


for(i in 1:m){
  output[[i]] = lm(RASDiffF5 ~ factor(Treatment) + Gender + Race + Edu + Employment, data = datWideAnalysis[[i]])
  outputSummary[[i]] = summary(output[[i]])
  coef_output[[i]] = outputSummary[[i]]$coefficients[,1]
  se_output[[i]] = outputSummary[[i]]$coefficients[,2]
}
coef_output = data.frame(t(data.frame(coef_output))) 
se_output = data.frame(t(data.frame(se_output)))

meldAllT_stat = function(x,y){
  coefsAll = mi.meld(q = x, se = y)
  coefs1 = t(data.frame(coefsAll$q.mi))
  ses1 = t(data.frame(coefsAll$se.mi))
  t_stat = coefs1/ses1
  p = 2*(pt(-abs(t_stat), df = outputSummary[[2]]$df[2]))
  return(data.frame(coefs1, ses1, t_stat, p))
}


results = meldAllT_stat(coef_output, se_output)
round(results, 3)

# Make sure things make sense i.e. difference in means
compmeans(datWideAnalysis[[1]]$RASDiffF5, datWideAnalysis[[1]]$Treatment)

```
Contrasts
```{r}
K = matrix(c(0, 1, -1, 0,0,0,0), 1)
library(multcomp)
t = NULL
for(i in 1:m){
  t[[i]] = glht(output[[i]], linfct = K)
  t[[i]] = summary(t[[i]])
}
t
```
############################################
Cohen's D for Treatment 2 versus Treatment 1
############################################
```{r}
datWideAnalysisT12 = NULL

for(i in 1:m){
datWideAnalysisT12[[i]] = subset(datWideAnalysis[[i]], Treatment == 1 | Treatment == 2)
}


cohenDat = NULL
for(i in 1:m){
cohenDat[[i]] = cohen.d(datWideAnalysisT12[[i]]$RASDiffF5, datWideAnalysisT12[[i]]$Treatment, hedges.correction = TRUE)
cohenDat[[i]] = cohenDat[[i]]$estimate
}

cohenD = data.frame(t(data.frame(cohenDat)))

meldAllT_stat = function(x,y){
coefsAll = mi.meld(q = x, se = y)
coefs1 = t(data.frame(coefsAll$q.mi))
ses1 = t(data.frame(coefsAll$se.mi))
return(data.frame(coefs1, ses1))
}
y = data.frame(rnorm(10))

meldAllT_stat(cohenD, y)
```
############################################
Cohen's D for Treatment 1 and 3
############################################
```{r}
datWideAnalysisT13 = NULL

for(i in 1:m){
  datWideAnalysisT13[[i]] = subset(datWideAnalysis[[i]], Treatment == 1 | Treatment == 3)
}


cohenDat = NULL
for(i in 1:m){
  cohenDat[[i]] = cohen.d(datWideAnalysisT13[[i]]$RASDiffF5, datWideAnalysisT13[[i]]$Treatment, hedges.correction = TRUE)
  cohenDat[[i]] = cohenDat[[i]]$estimate
}

cohenD = data.frame(t(data.frame(cohenDat)))

meldAllT_stat = function(x,y){
  coefsAll = mi.meld(q = x, se = y)
  coefs1 = t(data.frame(coefsAll$q.mi))
  ses1 = t(data.frame(coefsAll$se.mi))
  return(data.frame(coefs1, ses1))
}
y = data.frame(rnorm(10))

meldAllT_stat(cohenD, y)
```
############################################
Cohen's D for Treatment 3 versus Treatment 2
############################################
```{r}
datWideAnalysisT23 = NULL

for(i in 1:m){
datWideAnalysisT23[[i]] = subset(datWideAnalysis[[i]], Treatment == 2 | Treatment == 3)
}


cohenDat = NULL
for(i in 1:m){
cohenDat[[i]] = cohen.d(datWideAnalysisT23[[i]]$RASDiffF5, datWideAnalysisT23[[i]]$Treatment, hedges.correction = TRUE)
cohenDat[[i]] = cohenDat[[i]]$estimate
}

cohenD = data.frame(t(data.frame(cohenDat)))

meldAllT_stat = function(x,y){
coefsAll = mi.meld(q = x, se = y)
coefs1 = t(data.frame(coefsAll$q.mi))
ses1 = t(data.frame(coefsAll$se.mi))
return(data.frame(coefs1, ses1))
}
y = data.frame(rnorm(10))

meldAllT_stat(cohenD, y)
```
##################################
All treatmentments diff Score INQF1
##################################
```{r}
output = NULL
outputSummary = NULL
coef_output = NULL
se_output = NULL


for(i in 1:m){
  output[[i]] = lm(INQDiffF1 ~ factor(Treatment) + Gender + Race + Edu + Employment, data = datWideAnalysis[[i]])
  outputSummary[[i]] = summary(output[[i]])
  coef_output[[i]] = outputSummary[[i]]$coefficients[,1]
  se_output[[i]] = outputSummary[[i]]$coefficients[,2]
}
coef_output = data.frame(t(data.frame(coef_output))) 
se_output = data.frame(t(data.frame(se_output)))

meldAllT_stat = function(x,y){
  coefsAll = mi.meld(q = x, se = y)
  coefs1 = t(data.frame(coefsAll$q.mi))
  ses1 = t(data.frame(coefsAll$se.mi))
  t_stat = coefs1/ses1
  p = 2*(pt(-abs(t_stat), df = outputSummary[[2]]$df[2]))
  return(data.frame(coefs1, ses1, t_stat, p))
}


results = meldAllT_stat(coef_output, se_output)
round(results, 3)

# Make sure things make sense i.e. difference in means
compmeans(datWideAnalysis[[1]]$INQDiffF1, datWideAnalysis[[1]]$Treatment)

```
Contrasts
```{r}
K = matrix(c(0, 1, -1, 0,0,0,0), 1)
library(multcomp)
t = NULL
for(i in 1:m){
  t[[i]] = glht(output[[i]], linfct = K)
  t[[i]] = summary(t[[i]])
}
t
```
############################################
Cohen's D for Treatment 2 versus Treatment 1
############################################
```{r}
datWideAnalysisT12 = NULL

for(i in 1:m){
datWideAnalysisT12[[i]] = subset(datWideAnalysis[[i]], Treatment == 1 | Treatment == 2)
}


cohenDat = NULL
for(i in 1:m){
cohenDat[[i]] = cohen.d(datWideAnalysisT12[[i]]$INQDiffF1, datWideAnalysisT12[[i]]$Treatment, hedges.correction = TRUE)
cohenDat[[i]] = cohenDat[[i]]$estimate
}

cohenD = data.frame(t(data.frame(cohenDat)))

meldAllT_stat = function(x,y){
coefsAll = mi.meld(q = x, se = y)
coefs1 = t(data.frame(coefsAll$q.mi))
ses1 = t(data.frame(coefsAll$se.mi))
return(data.frame(coefs1, ses1))
}
y = data.frame(rnorm(10))

meldAllT_stat(cohenD, y)
```
############################################
Cohen's D for Treatment 1 and 3
############################################
```{r}
datWideAnalysisT13 = NULL

for(i in 1:m){
  datWideAnalysisT13[[i]] = subset(datWideAnalysis[[i]], Treatment == 1 | Treatment == 3)
}


cohenDat = NULL
for(i in 1:m){
  cohenDat[[i]] = cohen.d(datWideAnalysisT13[[i]]$INQDiffF1, datWideAnalysisT13[[i]]$Treatment, hedges.correction = TRUE)
  cohenDat[[i]] = cohenDat[[i]]$estimate
}

cohenD = data.frame(t(data.frame(cohenDat)))

meldAllT_stat = function(x,y){
  coefsAll = mi.meld(q = x, se = y)
  coefs1 = t(data.frame(coefsAll$q.mi))
  ses1 = t(data.frame(coefsAll$se.mi))
  return(data.frame(coefs1, ses1))
}
y = data.frame(rnorm(10))

meldAllT_stat(cohenD, y)
```
############################################
Cohen's D for Treatment 3 versus Treatment 2
############################################
```{r}
datWideAnalysisT23 = NULL

for(i in 1:m){
datWideAnalysisT23[[i]] = subset(datWideAnalysis[[i]], Treatment == 2 | Treatment == 3)
}


cohenDat = NULL
for(i in 1:m){
cohenDat[[i]] = cohen.d(datWideAnalysisT23[[i]]$INQDiffF1, datWideAnalysisT23[[i]]$Treatment, hedges.correction = TRUE)
cohenDat[[i]] = cohenDat[[i]]$estimate
}

cohenD = data.frame(t(data.frame(cohenDat)))

meldAllT_stat = function(x,y){
coefsAll = mi.meld(q = x, se = y)
coefs1 = t(data.frame(coefsAll$q.mi))
ses1 = t(data.frame(coefsAll$se.mi))
return(data.frame(coefs1, ses1))
}
y = data.frame(rnorm(10))

meldAllT_stat(cohenD, y)
```
##################################
All treatmentments diff Score INQF2
##################################
```{r}
output = NULL
outputSummary = NULL
coef_output = NULL
se_output = NULL


for(i in 1:m){
  output[[i]] = lm(INQDiffF2 ~ factor(Treatment) + Gender + Race + Edu + Employment, data = datWideAnalysis[[i]])
  outputSummary[[i]] = summary(output[[i]])
  coef_output[[i]] = outputSummary[[i]]$coefficients[,1]
  se_output[[i]] = outputSummary[[i]]$coefficients[,2]
}
coef_output = data.frame(t(data.frame(coef_output))) 
se_output = data.frame(t(data.frame(se_output)))

meldAllT_stat = function(x,y){
  coefsAll = mi.meld(q = x, se = y)
  coefs1 = t(data.frame(coefsAll$q.mi))
  ses1 = t(data.frame(coefsAll$se.mi))
  t_stat = coefs1/ses1
  p = 2*(pt(-abs(t_stat), df = outputSummary[[2]]$df[2]))
  return(data.frame(coefs1, ses1, t_stat, p))
}


results = meldAllT_stat(coef_output, se_output)
round(results, 3)

# Make sure things make sense i.e. difference in means
compmeans(datWideAnalysis[[1]]$INQDiffF2, datWideAnalysis[[1]]$Treatment)

```
Contrasts
```{r}
K = matrix(c(0, 1, -1, 0,0,0,0), 1)
library(multcomp)
t = NULL
for(i in 1:m){
  t[[i]] = glht(output[[i]], linfct = K)
  t[[i]] = summary(t[[i]])
}
t
```
############################################
Cohen's D for Treatment 2 versus Treatment 1
############################################
```{r}
datWideAnalysisT12 = NULL

for(i in 1:m){
datWideAnalysisT12[[i]] = subset(datWideAnalysis[[i]], Treatment == 1 | Treatment == 2)
}


cohenDat = NULL
for(i in 1:m){
cohenDat[[i]] = cohen.d(datWideAnalysisT12[[i]]$INQDiffF2, datWideAnalysisT12[[i]]$Treatment, hedges.correction = TRUE)
cohenDat[[i]] = cohenDat[[i]]$estimate
}

cohenD = data.frame(t(data.frame(cohenDat)))

meldAllT_stat = function(x,y){
coefsAll = mi.meld(q = x, se = y)
coefs1 = t(data.frame(coefsAll$q.mi))
ses1 = t(data.frame(coefsAll$se.mi))
return(data.frame(coefs1, ses1))
}
y = data.frame(rnorm(10))

meldAllT_stat(cohenD, y)
```
############################################
Cohen's D for Treatment 1 and 3
############################################
```{r}
datWideAnalysisT13 = NULL

for(i in 1:m){
  datWideAnalysisT13[[i]] = subset(datWideAnalysis[[i]], Treatment == 1 | Treatment == 3)
}


cohenDat = NULL
for(i in 1:m){
  cohenDat[[i]] = cohen.d(datWideAnalysisT13[[i]]$INQDiffF2, datWideAnalysisT13[[i]]$Treatment, hedges.correction = TRUE)
  cohenDat[[i]] = cohenDat[[i]]$estimate
}

cohenD = data.frame(t(data.frame(cohenDat)))

meldAllT_stat = function(x,y){
  coefsAll = mi.meld(q = x, se = y)
  coefs1 = t(data.frame(coefsAll$q.mi))
  ses1 = t(data.frame(coefsAll$se.mi))
  return(data.frame(coefs1, ses1))
}
y = data.frame(rnorm(10))

meldAllT_stat(cohenD, y)
```
############################################
Cohen's D for Treatment 3 versus Treatment 2
############################################
```{r}
datWideAnalysisT23 = NULL

for(i in 1:m){
datWideAnalysisT23[[i]] = subset(datWideAnalysis[[i]], Treatment == 2 | Treatment == 3)
}


cohenDat = NULL
for(i in 1:m){
cohenDat[[i]] = cohen.d(datWideAnalysisT23[[i]]$INQDiffF2, datWideAnalysisT23[[i]]$Treatment, hedges.correction = TRUE)
cohenDat[[i]] = cohenDat[[i]]$estimate
}

cohenD = data.frame(t(data.frame(cohenDat)))

meldAllT_stat = function(x,y){
coefsAll = mi.meld(q = x, se = y)
coefs1 = t(data.frame(coefsAll$q.mi))
ses1 = t(data.frame(coefsAll$se.mi))
return(data.frame(coefs1, ses1))
}
y = data.frame(rnorm(10))

meldAllT_stat(cohenD, y)
```


##################################
All treatmentments diff Score SSMI
##################################
```{r}
output = NULL
outputSummary = NULL
coef_output = NULL
se_output = NULL


for(i in 1:m){
  output[[i]] = lm(SSMIDiff ~ factor(Treatment) + Gender + Race + Edu + Employment, data = datWideAnalysis[[i]])
  outputSummary[[i]] = summary(output[[i]])
  coef_output[[i]] = outputSummary[[i]]$coefficients[,1]
  se_output[[i]] = outputSummary[[i]]$coefficients[,2]
}
coef_output = data.frame(t(data.frame(coef_output))) 
se_output = data.frame(t(data.frame(se_output)))

meldAllT_stat = function(x,y){
  coefsAll = mi.meld(q = x, se = y)
  coefs1 = t(data.frame(coefsAll$q.mi))
  ses1 = t(data.frame(coefsAll$se.mi))
  t_stat = coefs1/ses1
  p = 2*(pt(-abs(t_stat), df = outputSummary[[2]]$df[2]))
  return(data.frame(coefs1, ses1, t_stat, p))
}


results = meldAllT_stat(coef_output, se_output)
round(results, 3)

# Make sure things make sense i.e. difference in means
compmeans(datWideAnalysis[[1]]$SSMIDiff, datWideAnalysis[[1]]$Treatment)

```
Contrasts
```{r}
K = matrix(c(0, 1, -1, 0,0,0,0), 1)
library(multcomp)
t = NULL
for(i in 1:m){
  t[[i]] = glht(output[[i]], linfct = K)
  t[[i]] = summary(t[[i]])
}
t
```
############################################
Cohen's D for Treatment 2 versus Treatment 1
############################################
```{r}
datWideAnalysisT12 = NULL

for(i in 1:m){
datWideAnalysisT12[[i]] = subset(datWideAnalysis[[i]], Treatment == 1 | Treatment == 2)
}


cohenDat = NULL
for(i in 1:m){
cohenDat[[i]] = cohen.d(datWideAnalysisT12[[i]]$SSMIDiff, datWideAnalysisT12[[i]]$Treatment, hedges.correction = TRUE)
cohenDat[[i]] = cohenDat[[i]]$estimate
}

cohenD = data.frame(t(data.frame(cohenDat)))

meldAllT_stat = function(x,y){
coefsAll = mi.meld(q = x, se = y)
coefs1 = t(data.frame(coefsAll$q.mi))
ses1 = t(data.frame(coefsAll$se.mi))
return(data.frame(coefs1, ses1))
}
y = data.frame(rnorm(10))

meldAllT_stat(cohenD, y)
```
############################################
Cohen's D for Treatment 1 and 3
############################################
```{r}
datWideAnalysisT13 = NULL

for(i in 1:m){
  datWideAnalysisT13[[i]] = subset(datWideAnalysis[[i]], Treatment == 1 | Treatment == 3)
}


cohenDat = NULL
for(i in 1:m){
  cohenDat[[i]] = cohen.d(datWideAnalysisT13[[i]]$SSMIDiff, datWideAnalysisT13[[i]]$Treatment, hedges.correction = TRUE)
  cohenDat[[i]] = cohenDat[[i]]$estimate
}

cohenD = data.frame(t(data.frame(cohenDat)))

meldAllT_stat = function(x,y){
  coefsAll = mi.meld(q = x, se = y)
  coefs1 = t(data.frame(coefsAll$q.mi))
  ses1 = t(data.frame(coefsAll$se.mi))
  return(data.frame(coefs1, ses1))
}
y = data.frame(rnorm(10))

meldAllT_stat(cohenD, y)
```
############################################
Cohen's D for Treatment 3 versus Treatment 2
############################################
```{r}
datWideAnalysisT23 = NULL

for(i in 1:m){
datWideAnalysisT23[[i]] = subset(datWideAnalysis[[i]], Treatment == 2 | Treatment == 3)
}


cohenDat = NULL
for(i in 1:m){
cohenDat[[i]] = cohen.d(datWideAnalysisT23[[i]]$SSMIDiff, datWideAnalysisT23[[i]]$Treatment, hedges.correction = TRUE)
cohenDat[[i]] = cohenDat[[i]]$estimate
}

cohenD = data.frame(t(data.frame(cohenDat)))

meldAllT_stat = function(x,y){
coefsAll = mi.meld(q = x, se = y)
coefs1 = t(data.frame(coefsAll$q.mi))
ses1 = t(data.frame(coefsAll$se.mi))
return(data.frame(coefs1, ses1))
}
y = data.frame(rnorm(10))

meldAllT_stat(cohenD, y)
```
##################################
All treatmentments diff Score SISF1
##################################
```{r}
output = NULL
outputSummary = NULL
coef_output = NULL
se_output = NULL

for(i in 1:m){
  output[[i]] = lm(SISDiffF1 ~ factor(Treatment) + Gender + Race + Edu + Employment, data = datWideAnalysis[[i]])
  outputSummary[[i]] = summary(output[[i]])
  coef_output[[i]] = outputSummary[[i]]$coefficients[,1]
  se_output[[i]] = outputSummary[[i]]$coefficients[,2]
}
coef_output = data.frame(t(data.frame(coef_output))) 
se_output = data.frame(t(data.frame(se_output)))

meldAllT_stat = function(x,y){
  coefsAll = mi.meld(q = x, se = y)
  coefs1 = t(data.frame(coefsAll$q.mi))
  ses1 = t(data.frame(coefsAll$se.mi))
  t_stat = coefs1/ses1
  p = 2*(pt(-abs(t_stat), df = outputSummary[[2]]$df[2]))
  return(data.frame(coefs1, ses1, t_stat, p))
}


results = meldAllT_stat(coef_output, se_output)
round(results, 3)

# Make sure things make sense i.e. difference in means
compmeans(datWideAnalysis[[1]]$SISDiffF1, datWideAnalysis[[1]]$Treatment)

```
Contrasts
```{r}
K = matrix(c(0, 1, -1, 0,0,0,0), 1)
library(multcomp)
t = NULL
for(i in 1:m){
  t[[i]] = glht(output[[i]], linfct = K)
  t[[i]] = summary(t[[i]])
}
t
```
############################################
Cohen's D for Treatment 2 versus Treatment 1
############################################
```{r}
datWideAnalysisT12 = NULL

for(i in 1:m){
datWideAnalysisT12[[i]] = subset(datWideAnalysis[[i]], Treatment == 1 | Treatment == 2)
}


cohenDat = NULL
for(i in 1:m){
cohenDat[[i]] = cohen.d(datWideAnalysisT12[[i]]$SISDiffF1, datWideAnalysisT12[[i]]$Treatment, hedges.correction = TRUE)
cohenDat[[i]] = cohenDat[[i]]$estimate
}

cohenD = data.frame(t(data.frame(cohenDat)))

meldAllT_stat = function(x,y){
coefsAll = mi.meld(q = x, se = y)
coefs1 = t(data.frame(coefsAll$q.mi))
ses1 = t(data.frame(coefsAll$se.mi))
return(data.frame(coefs1, ses1))
}
y = data.frame(rnorm(10))

meldAllT_stat(cohenD, y)
```
############################################
Cohen's D for Treatment 1 and 3
############################################
```{r}
datWideAnalysisT13 = NULL

for(i in 1:m){
  datWideAnalysisT13[[i]] = subset(datWideAnalysis[[i]], Treatment == 1 | Treatment == 3)
}


cohenDat = NULL
for(i in 1:m){
  cohenDat[[i]] = cohen.d(datWideAnalysisT13[[i]]$SISDiffF1, datWideAnalysisT13[[i]]$Treatment, hedges.correction = TRUE)
  cohenDat[[i]] = cohenDat[[i]]$estimate
}

cohenD = data.frame(t(data.frame(cohenDat)))

meldAllT_stat = function(x,y){
  coefsAll = mi.meld(q = x, se = y)
  coefs1 = t(data.frame(coefsAll$q.mi))
  ses1 = t(data.frame(coefsAll$se.mi))
  return(data.frame(coefs1, ses1))
}
y = data.frame(rnorm(10))

meldAllT_stat(cohenD, y)
```
############################################
Cohen's D for Treatment 3 versus Treatment 2
############################################
```{r}
datWideAnalysisT23 = NULL

for(i in 1:m){
datWideAnalysisT23[[i]] = subset(datWideAnalysis[[i]], Treatment == 2 | Treatment == 3)
}


cohenDat = NULL
for(i in 1:m){
cohenDat[[i]] = cohen.d(datWideAnalysisT23[[i]]$SISDiffF1, datWideAnalysisT23[[i]]$Treatment, hedges.correction = TRUE)
cohenDat[[i]] = cohenDat[[i]]$estimate
}

cohenD = data.frame(t(data.frame(cohenDat)))

meldAllT_stat = function(x,y){
coefsAll = mi.meld(q = x, se = y)
coefs1 = t(data.frame(coefsAll$q.mi))
ses1 = t(data.frame(coefsAll$se.mi))
return(data.frame(coefs1, ses1))
}
y = data.frame(rnorm(10))

meldAllT_stat(cohenD, y)
```
##################################
All treatmentments diff Score SISF2
##################################
```{r}
output = NULL
outputSummary = NULL
coef_output = NULL
se_output = NULL

for(i in 1:m){
  output[[i]] = lm(SISDiffF2 ~ factor(Treatment) + Gender + Race + Edu + Employment, data = datWideAnalysis[[i]])
  outputSummary[[i]] = summary(output[[i]])
  coef_output[[i]] = outputSummary[[i]]$coefficients[,1]
  se_output[[i]] = outputSummary[[i]]$coefficients[,2]
}
coef_output = data.frame(t(data.frame(coef_output))) 
se_output = data.frame(t(data.frame(se_output)))

meldAllT_stat = function(x,y){
  coefsAll = mi.meld(q = x, se = y)
  coefs1 = t(data.frame(coefsAll$q.mi))
  ses1 = t(data.frame(coefsAll$se.mi))
  t_stat = coefs1/ses1
  p = 2*(pt(-abs(t_stat), df = outputSummary[[2]]$df[2]))
  return(data.frame(coefs1, ses1, t_stat, p))
}


results = meldAllT_stat(coef_output, se_output)
round(results, 3)

# Make sure things make sense i.e. difference in means
compmeans(datWideAnalysis[[1]]$SISDiffF2, datWideAnalysis[[1]]$Treatment)

```
Contrasts
```{r}
K = matrix(c(0, 1, -1, 0,0,0,0), 1)
library(multcomp)
t = NULL
for(i in 1:m){
  t[[i]] = glht(output[[i]], linfct = K)
  t[[i]] = summary(t[[i]])
}
t
```
############################################
Cohen's D for Treatment 2 versus Treatment 1
############################################
```{r}
datWideAnalysisT12 = NULL

for(i in 1:m){
datWideAnalysisT12[[i]] = subset(datWideAnalysis[[i]], Treatment == 1 | Treatment == 2)
}


cohenDat = NULL
for(i in 1:m){
cohenDat[[i]] = cohen.d(datWideAnalysisT12[[i]]$SISDiffF2, datWideAnalysisT12[[i]]$Treatment, hedges.correction = TRUE)
cohenDat[[i]] = cohenDat[[i]]$estimate
}

cohenD = data.frame(t(data.frame(cohenDat)))

meldAllT_stat = function(x,y){
coefsAll = mi.meld(q = x, se = y)
coefs1 = t(data.frame(coefsAll$q.mi))
ses1 = t(data.frame(coefsAll$se.mi))
return(data.frame(coefs1, ses1))
}
y = data.frame(rnorm(10))

meldAllT_stat(cohenD, y)
```
############################################
Cohen's D for Treatment 1 and 3
############################################
```{r}
datWideAnalysisT13 = NULL

for(i in 1:m){
  datWideAnalysisT13[[i]] = subset(datWideAnalysis[[i]], Treatment == 1 | Treatment == 3)
}


cohenDat = NULL
for(i in 1:m){
  cohenDat[[i]] = cohen.d(datWideAnalysisT13[[i]]$SISDiffF2, datWideAnalysisT13[[i]]$Treatment, hedges.correction = TRUE)
  cohenDat[[i]] = cohenDat[[i]]$estimate
}

cohenD = data.frame(t(data.frame(cohenDat)))

meldAllT_stat = function(x,y){
  coefsAll = mi.meld(q = x, se = y)
  coefs1 = t(data.frame(coefsAll$q.mi))
  ses1 = t(data.frame(coefsAll$se.mi))
  return(data.frame(coefs1, ses1))
}
y = data.frame(rnorm(10))

meldAllT_stat(cohenD, y)
```
############################################
Cohen's D for Treatment 3 versus Treatment 2
############################################
```{r}
datWideAnalysisT23 = NULL

for(i in 1:m){
datWideAnalysisT23[[i]] = subset(datWideAnalysis[[i]], Treatment == 2 | Treatment == 3)
}


cohenDat = NULL
for(i in 1:m){
cohenDat[[i]] = cohen.d(datWideAnalysisT23[[i]]$SISDiffF2, datWideAnalysisT23[[i]]$Treatment, hedges.correction = TRUE)
cohenDat[[i]] = cohenDat[[i]]$estimate
}

cohenD = data.frame(t(data.frame(cohenDat)))

meldAllT_stat = function(x,y){
coefsAll = mi.meld(q = x, se = y)
coefs1 = t(data.frame(coefsAll$q.mi))
ses1 = t(data.frame(coefsAll$se.mi))
return(data.frame(coefs1, ses1))
}
y = data.frame(rnorm(10))

meldAllT_stat(cohenD, y)
```








---
title: "Centerstone 2019-2020 Study"
output:
  pdf_document: default
  html_document: default
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```
Load the data
```{r}
# Centerstone study 2019-2020
setwd("S:/Indiana Research & Evaluation/Matthew Hanauer/Centerstone_Study_2019_2020")
center_dat_load = read.csv("BelongGive_DataClean_7.20.csv", header = TRUE)
center_dat=  center_dat_load 
head(center_dat)
library(psych)
library(prettyR)
```
Create total scores for INQ and RAS
```{r}
## Reverse score f = 6,g = 7, j=10
## This means reverse score 1,2,5 for second construct
INQ_1_pre = center_dat[,5:9]
INQ_1_pre = (INQ_1_pre)
INQ_1_pre_des = data.frame(apply(INQ_1_pre, 2, as.factor))
describe(INQ_1_pre_des)

head(INQ_1_pre)
INQ_1_pre = rowMeans(INQ_1_pre, na.rm = TRUE)

INQ_2_pre = center_dat[,10:14]
INQ_2_pre = 8- INQ_2_pre[,c(1,2,5)]
INQ_2_pre_des = data.frame(apply(INQ_2_pre, 2, as.factor))
describe(INQ_2_pre_des)
head(INQ_2_pre)
INQ_2_pre = rowMeans(INQ_2_pre, na.rm = TRUE)


INQ_1_post = center_dat[,100:104]
INQ_1_post_des = data.frame(apply(INQ_1_post, 2, as.factor))
describe(INQ_1_post_des)
head(INQ_1_post)
INQ_1_post = rowMeans(INQ_1_post, na.rm = TRUE)

INQ_2_post = center_dat[,105:109]
INQ_2_post = 8- INQ_2_post[,c(1,2,5)]
INQ_2_post_des = data.frame(apply(INQ_2_post, 2, as.factor))
describe(INQ_2_post_des)
head(INQ_2_post)
INQ_2_post = rowMeans(INQ_2_post, na.rm = TRUE)
head(INQ_1_post)

head(center_dat)
### RAS
## f-m (Personal confidence and hope)    
RAS_1_pre = center_dat[,20:27]
RAS_1_pre_des = data.frame(apply(RAS_1_pre, 2, as.factor))
describe(RAS_1_pre_des)
head(RAS_1_pre)
RAS_1_pre = rowMeans(RAS_1_pre, na.rm = TRUE)
head(RAS_1_pre)

head(center_dat[,15:19])
RAS_1_post = center_dat[,115:122]
head(RAS_1_post)
RAS_1_post_des = data.frame(apply(RAS_1_post, 2, as.factor))
describe(RAS_1_post_des)
head(RAS_1_post)
RAS_1_post = rowMeans(RAS_1_post, na.rm = TRUE)
head(RAS_1_post)


RAS_3_pre = center_dat[,15:19]
RAS_3_pre_des = data.frame(apply(RAS_3_pre, 2, as.factor))
describe(RAS_3_pre_des)
head(RAS_3_pre)
RAS_3_pre = rowMeans(RAS_3_pre, na.rm = TRUE)

RAS_3_post = center_dat[,110:114]
RAS_3_post_des = data.frame(apply(RAS_3_post, 2, as.factor))
describe(RAS_3_post_des)
head(RAS_3_post)
RAS_3_post = rowMeans(RAS_3_post, na.rm = TRUE)

#### RAS 5 willingness to ask for help
RAS_5_pre = center_dat[,28:31]
head(RAS_5_pre)
RAS_5_pre_des = data.frame(apply(RAS_5_pre, 2, as.factor))
describe(RAS_5_pre_des)
head(RAS_5_pre)
RAS_5_pre = rowMeans(RAS_5_pre, na.rm = TRUE)
head(RAS_5_pre)

RAS_5_post = center_dat[,123:126]
head(RAS_5_post)
RAS_5_post_des = data.frame(apply(RAS_5_post, 2, as.factor))
describe(RAS_5_post_des)
head(RAS_5_post)
RAS_5_post = rowMeans(RAS_5_post, na.rm = TRUE)
head(RAS_5_post)

### Lower scores are better
ISLES_1_pre = center_dat[,34:36]
head(center_dat)
ISLES_1_pre_des = data.frame(apply(ISLES_1_pre, 2, as.factor))
describe(ISLES_1_pre_des)
ISLES_1_pre = rowMeans(ISLES_1_pre, na.rm = TRUE)

ISLES_2_pre = center_dat[,37:39]
head(center_dat)
ISLES_2_pre_des = data.frame(apply(ISLES_2_pre, 2, as.factor))
describe(ISLES_2_pre_des)
ISLES_2_pre = rowMeans(ISLES_2_pre, na.rm = TRUE)


ISLES_1_post = center_dat[,132:134]
ISLES_1_post_des = data.frame(apply(ISLES_1_post, 2, as.factor))
describe(ISLES_1_post_des)
ISLES_1_post = rowMeans(ISLES_1_post, na.rm = TRUE)

ISLES_2_post = center_dat[,135:137]
ISLES_2_post_des = data.frame(apply(ISLES_2_post, 2, as.factor))
describe(ISLES_2_post_des)
ISLES_2_post = rowMeans(ISLES_2_post, na.rm = TRUE)


###MILQ higher scores are better factor called Presense
MILQ_pre = center_dat[,40:44]
### Reverse score last question
MILQ_pre[,5] = 8-MILQ_pre[,5]
MILQ_pre_des = data.frame(apply(MILQ_pre, 2, as.factor))
describe(MILQ_pre_des)
head(MILQ_pre)
MILQ_pre = rowMeans(MILQ_pre, na.rm = TRUE)

MILQ_post = center_dat[,127:131]
MILQ_post[,5] = 8-MILQ_post[,5]
MILQ_post_des = data.frame(apply(MILQ_post, 2, as.factor))
describe(MILQ_post_des)
head(MILQ_post)
MILQ_post = rowMeans(MILQ_post, na.rm = TRUE)


### RCS
RCS_pre = center_dat[,56:59]
RCS_pre_des = data.frame(apply(RCS_pre, 2, as.factor))
describe(RCS_pre_des)
head(RCS_pre)
RCS_pre = rowMeans(RCS_pre, na.rm = TRUE)
RCS_pre

RCS_post = center_dat[,151:154]
RCS_post_des = data.frame(apply(RCS_post, 2, as.factor))
describe(RCS_post_des)
head(RCS_post)
RCS_post = rowMeans(RCS_post, na.rm = TRUE)


### SIS first 5 and then next 6

### Demos
demos = center_dat[,c(75:78,80,92,97:98)]
demos_fac = data.frame(apply(demos, 2, as.factor))
describe(demos_fac)
female = ifelse(demos$X10_Sex == 2, 1, 0)
demos
veteran = ifelse(demos$X11_Veteran == 2, 1,0)
sexual_minority = ifelse(demos$X12_SO != 3, 1, 0)
hispanic = ifelse(demos$X13_Hisp.Yes.No. == 1, 1, 0)
non_white = ifelse(demos$X14_Race.e == 1, 0,1)
high_school_greater = ifelse(demos$X16_Education > 2, 1, 0)
employed = ifelse(demos$X17_Employment == 2 | demos$X17_Employment == 3, 1, 0)

demos = data.frame(age = demos$X9_Age, veteran, sexual_minority, hispanic, non_white, high_school_greater, employed)


### SIS 1 just guessing first 5
head(center_dat[,45:55])
SIS_1_pre = center_dat[,45:49]
SIS_1_pre_des = data.frame(apply(SIS_1_pre, 2, as.factor))
describe(SIS_1_pre_des)
head(SIS_1_pre)
SIS_1_pre = rowMeans(SIS_1_pre, na.rm = TRUE)
SIS_1_pre

SIS_2_pre = center_dat[,50:55]
SIS_2_pre_des = data.frame(apply(SIS_2_pre, 2, as.factor))
describe(SIS_2_pre_des)
head(SIS_2_pre)
SIS_2_pre = rowMeans(SIS_2_pre, na.rm = TRUE)
SIS_2_pre

SIS_1_post = center_dat[,155:159]
SIS_1_post_des = data.frame(apply(SIS_1_post, 2, as.factor))
describe(SIS_1_post_des)
head(SIS_1_post)
SIS_1_post = rowMeans(SIS_1_post, na.rm = TRUE)
SIS_1_post

SIS_2_post = center_dat[,160:165]
SIS_2_post_des = data.frame(apply(SIS_2_post, 2, as.factor))
describe(SIS_2_post_des)
head(SIS_2_post)
SIS_2_post = rowMeans(SIS_2_post, na.rm = TRUE)
SIS_2_post


## Only want treat_c, treat_g, treat_i, treat_l, treat_m, because they have more than 25% in each category
#treatments = data.frame( treat_c = center_dat$X9_TREAT.c.Received, treat_g = center_dat$X9_Treat.g.Received, treat_l = center_dat$X9_Treat.L.Received, treat_m = center_dat$X9_Treat.m.Received)

#treatments$treat_b = ifelse(treatments$treat_b == 1, 1, 0)
#treatments_fac = data.frame(apply(treatments, 2, as.factor))
#describe(treatments_fac)


center_dat = data.frame(demos, INQ_1_pre, INQ_2_pre, RAS_1_pre, RAS_3_pre, RAS_5_pre, ISLES_1_pre, ISLES_2_pre, MILQ_pre, RCS_pre, SIS_1_pre, SIS_2_pre, INQ_1_post, INQ_2_post, RAS_1_post, RAS_3_post, RAS_5_post, ISLES_1_post, ISLES_2_post, MILQ_post, RCS_post, SIS_1_post, SIS_2_post)

```
Descriptives with complete data
```{r}
center_dat
center_dat[,2:7] = data.frame(apply(center_dat[,2:7],2, as.factor))
describe(center_dat)

```

Assess missing 
```{r}
head(center_dat)
library(naniar)
miss_var_summary(center_dat)

dim(center_dat)
center_dat_complete = na.omit(center_dat)
dim(center_dat_complete)

quasi_itt =  apply(center_dat, 1, function(x)(sum(is.na(x))))
quasi_itt_dat = data.frame(center_dat,quasi_itt)
describe.factor(quasi_itt_dat$quasi_itt)
### Ten variables and threshold is less than 50% 
dim(quasi_itt_dat)[2]/2
quasi_itt_dat = subset(quasi_itt_dat, quasi_itt < dim(quasi_itt_dat)[2]/2)
dim(center_dat)
dim(quasi_itt_dat)
quasi_itt_dat$quasi_itt = NULL
dim(quasi_itt_dat)

center_dat = quasi_itt_dat
dim(center_dat)
```
Impute data
```{r}
head(center_dat)
library(Amelia)
a.out = amelia(x = center_dat, m = 5, noms = c("veteran", "sexual_minority", "hispanic", "non_white", "high_school_greater", "employed"))
compare.density(a.out, var = "RAS_1_post")
compare.density(a.out, var = "RAS_3_post")
compare.density(a.out, var = "RAS_5_post")
compare.density(a.out, var = "RCS_post")
compare.density(a.out, var = "ISLES_1_post")
compare.density(a.out, var = "ISLES_2_post")
compare.density(a.out, var = "INQ_1_post")
compare.density(a.out, var = "INQ_2_post")
compare.density(a.out, var = "MILQ_post")
compare.density(a.out, var = "SIS_1_post")
compare.density(a.out, var = "SIS_2_post")


impute_dat_loop = a.out$imputations
```
Do diff scores with regression, because not random and want to account
```{r}
dim(impute_dat_loop[[1]])
out_diff_dat = list()
head(impute_dat_loop[[1]][8:18])
head(impute_dat_loop[[1]][19:29])

for(i in 1:length(impute_dat_loop)){
  out_diff_dat[[i]] = impute_dat_loop[[i]][8:18]-impute_dat_loop[[1]][19:29]
  colnames(out_diff_dat[[i]]) = c("INQ_1_diff", "INQ_2_diff", "RAS_1_diff", "RAS_3_diff", "RAS_5_diff", "ISLES_1_diff", "ISLES_2_diff", "MILQ_diff", "RCS_diff", "SIS_1_diff", "SIS_2_diff")
  out_diff_dat[[i]] = scale(out_diff_dat[[i]])
  out_diff_dat[[i]] =cbind(impute_dat_loop[[i]], out_diff_dat[[i]])
}
out_diff_dat
### Evaluate normality
out_diff_dat_norm = out_diff_dat[[1]][30:40]
hist_results = list() 
qq_results = list()
shap_results = list()
for(i in 1:length(out_diff_dat_norm)){
  hist_results[[i]]= hist(out_diff_dat_norm[[i]], main = paste("Histogram of" , names(out_diff_dat_norm)[[i]]))
  qq_results[[i]] = qqnorm(out_diff_dat_norm[[i]], main = names(out_diff_dat_norm)[[i]])
  shap_results[[i]] = shapiro.test(out_diff_dat_norm[[i]])
}
shap_results
```
Research Question #1: Are novel treatment targets (i.e., perceived burdensomeness, thwarted belongingness, meaning made of stress, goal orientation/hope, resilience-based coping) changing from pre-treatment to post-treatment during standard episodes of care? 

Hypothesis 1: There will be no change in novel treatment target scores from the pre-treatment condition to the post-treatment condition. 
```{r}
 
out_diff_dat_d1 = out_diff_dat[[1]]
out_diff_dat_d1_pre = out_diff_dat_d1[,8:18]

out_diff_dat_d2 = out_diff_dat[[2]]
out_diff_dat_d2_pre = out_diff_dat_d2[,8:18]

out_diff_dat_d3 = out_diff_dat[[3]]
out_diff_dat_d3_pre = out_diff_dat_d3[,8:18]

out_diff_dat_d4 = out_diff_dat[[4]]
out_diff_dat_d4_pre = out_diff_dat_d4[,8:18]

out_diff_dat_d5 = out_diff_dat[[5]]
out_diff_dat_d5_pre = out_diff_dat_d5[,8:18]

out_diff_dat_d1 = out_diff_dat[[1]]
out_diff_dat_d1_post = out_diff_dat_d1[,19:29]

out_diff_dat_d2 = out_diff_dat[[2]]
out_diff_dat_d2_post = out_diff_dat_d2[,19:29]

out_diff_dat_d3 = out_diff_dat[[3]]
out_diff_dat_d3_post = out_diff_dat_d3[,19:29]

out_diff_dat_d4 = out_diff_dat[[4]]
out_diff_dat_d4_post = out_diff_dat_d4[,19:29]

out_diff_dat_d5 = out_diff_dat[[5]]
out_diff_dat_d5_post = out_diff_dat_d5[,19:29]

library(effsize)
center_results_d1 = list()
for(i in 1:length(out_diff_dat_d1_post)){
  center_results_d1[[i]]= cohen.d(out_diff_dat_d1_post[[i]], out_diff_dat_d1_pre[[i]], paired = TRUE, conf.level = .95)
  center_results_d1[[i]] = center_results_d1[[i]][c(3,5)]
}
center_results_d1
center_results_d1 = unlist(center_results_d1)
center_results_d1 = matrix(center_results_d1, ncol = 3, byrow = TRUE)
center_results_d1 = data.frame(center_results_d1)
center_results_d1 = round(center_results_d1, 3)
center_results_d1
colnames(center_results_d1) = c("cohen_d", "lower", "upper")
center_results_d1

center_results_d2 = list()
for(i in 1:length(out_diff_dat_d1_post)){
  center_results_d2[[i]]= cohen.d(out_diff_dat_d2_post[[i]], out_diff_dat_d2_pre[[i]], paired = TRUE, conf.level = .95)
  center_results_d2[[i]] = center_results_d2[[i]][c(3,5)]
}
center_results_d2
center_results_d2
center_results_d2 = unlist(center_results_d2)
center_results_d2 = matrix(center_results_d2, ncol = 3, byrow = TRUE)
center_results_d2 = data.frame(center_results_d2)
center_results_d2 = round(center_results_d2, 3)
center_results_d2
colnames(center_results_d2) = c("cohen_d", "lower", "upper")
center_results_d2

center_results_d3 = list()
for(i in 1:length(out_diff_dat_d1_post)){
  center_results_d3[[i]]= cohen.d(out_diff_dat_d3_post[[i]], out_diff_dat_d3_pre[[i]], paired = TRUE, conf.level = .95)
  center_results_d3[[i]] = center_results_d3[[i]][c(3,5)]
}
center_results_d3
center_results_d3
center_results_d3 = unlist(center_results_d3)
center_results_d3 = matrix(center_results_d3, ncol = 3, byrow = TRUE)
center_results_d3 = data.frame(center_results_d3)
center_results_d3 = round(center_results_d3, 3)
center_results_d3
colnames(center_results_d3) = c("cohen_d", "lower", "upper")
center_results_d3

center_results_d4 = list()
for(i in 1:length(out_diff_dat_d1_post)){
  center_results_d4[[i]]= cohen.d(out_diff_dat_d4_post[[i]], out_diff_dat_d4_pre[[i]], paired = TRUE, conf.level = .95)
  center_results_d4[[i]] = center_results_d4[[i]][c(3,5)]
}
center_results_d4
center_results_d4
center_results_d4 = unlist(center_results_d4)
center_results_d4 = matrix(center_results_d4, ncol = 3, byrow = TRUE)
center_results_d4 = data.frame(center_results_d4)
center_results_d4 = round(center_results_d4, 3)
center_results_d4
colnames(center_results_d4) = c("cohen_d", "lower", "upper")
center_results_d4

center_results_d5 = list()
for(i in 1:length(out_diff_dat_d1_post)){
  center_results_d5[[i]]= cohen.d(out_diff_dat_d5_post[[i]], out_diff_dat_d5_pre[[i]], paired = TRUE, conf.level = .95)
  center_results_d5[[i]] = center_results_d5[[i]][c(3,5)]
}
center_results_d5
center_results_d5
center_results_d5 = unlist(center_results_d5)
center_results_d5 = matrix(center_results_d5, ncol = 3, byrow = TRUE)
center_results_d5 = data.frame(center_results_d5)
center_results_d5 = round(center_results_d5, 3)
center_results_d5
colnames(center_results_d5) = c("cohen_d", "lower", "upper")
center_results_d5


center_results_cohen_d = data.frame(cohen_d1 = center_results_d1$cohen_d, cohen_d2 = center_results_d2$cohen_d, cohen_d3 = center_results_d3$cohen_d, cohen_d4 = center_results_d4$cohen_d, cohen_d5 = center_results_d5$cohen_d)
center_results_cohen_d = rowMeans(center_results_cohen_d)
center_results_cohen_d

center_results_upper = data.frame(upper1 = center_results_d1$upper, upper2 = center_results_d2$upper, upper3 = center_results_d3$upper, upper4 = center_results_d4$upper, upper5 = center_results_d5$upper)
center_results_upper = rowMeans(center_results_upper)
center_results_upper

center_results_lower = data.frame(lower1 = center_results_d1$lower, lower2 = center_results_d2$lower, lower3 = center_results_d3$lower, lower4 = center_results_d4$lower, lower5 = center_results_d5$lower)
center_results_lower = rowMeans(center_results_lower)
center_results_lower

center_results = data.frame(cohen_d = center_results_cohen_d, upper = center_results_upper, lower = center_results_lower)
center_results = round(center_results, 2)
center_results$cohen_d = ifelse(center_results$upper > 0 & center_results$lower < 0, center_results$cohen_d, paste0(center_results$cohen_d, "*"))
center_results$ci_95 = paste0(center_results$lower, sep = ",", center_results$upper)
center_results[,2:3] = NULL
head(out_diff_dat)
center_results

outcomes = c("Perceived Burdensomeness", "Thwarted Belongingness", "Personal confidence and hope", "Goal and Success Orientation", "Willingness to ask for help", "Comprehensibility", "Footing in the world", "MILQ", "RCS", "Suicidal Ideation", "Resolved plans and preparations")
center_results = data.frame(outcomes, center_results)

write.csv(center_results, "center_results.csv", row.names = FALSE)


```
Research Question #2: Are novel treatment targets (i.e., perceived burdensomeness, thwarted belongingness, meaning made of stress, meaning in life, goal orientation/hope coping self-efficacy, treatment alliance, treatment satisfaction) associated with episode of care outcomes (i.e., suicide risk, willingness to seek help, intentions to follow-through on discharge plans) at discharge? 

Hypothesis 1: Perceived burdensomeness and thwarted belongingness will be positively and uniquely associated with suicide risk (i.e., ideation, resolved plans/preparation) at discharge. 
```{r}

```


Hypothesis 2: Meaning made of stress, meaning in life, coping self-efficacy, goal orientation/hope, alliance, and treatment satisfaction will be negatively and uniquely associated with suicide risk (i.e., ideation, resolved plans and preparation) at discharge. 

Hypothesis 3: Treatment alliance and treatment satisfaction will be positively and uniquely associated with willingness to seek future help at discharge. 

Hypothesis 4:  Treatment alliance and treatment satisfaction will be positively and uniquely associated with intention to follow-through on discharge plans. 






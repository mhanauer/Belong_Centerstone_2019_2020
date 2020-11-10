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
#install.packages("psych")
#library(psych)
library(prettyR)

```
Get the count and percentage of MDD and PTSD
Using 1a_ICD10 variable
F33 = MDD 
F43.1 = PTSD

https://www.icd10data.com/ICD10CM/Codes/F01-F99/F40-F48/F43-/F43.1

```{r}
describe.factor(center_dat$X1a_ICD10)
center_dat$ICD10_short =  gsub("\\..*","",center_dat$X1a_ICD10)
describe.factor(center_dat$ICD10_short)

```



Create total scores for INQ and RAS
```{r}
## Reverse score f = 6,g = 7, j=10
## This means reverse score 1,2,5 for second construct
INQ_1_pre = center_dat[,5:9]
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

#### RAS 5 No domination by symptoms
RAS_5_pre = center_dat[,28:30]
head(RAS_5_pre)
RAS_5_pre_des = data.frame(apply(RAS_5_pre, 2, as.factor))
describe(RAS_5_pre_des)
head(RAS_5_pre)
RAS_5_pre = rowMeans(RAS_5_pre, na.rm = TRUE)
head(RAS_5_pre)

RAS_5_post = center_dat[,123:125]
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
range(RCS_pre, na.rm = TRUE)

RCS_post = center_dat[,151:154]
RCS_post[RCS_post == 10] = NA
describe(RCS_post)
### overwriting data to get rid of the -99


RCS_post_des = data.frame(apply(RCS_post, 2, as.factor))
describe(RCS_post_des)
head(RCS_post)
RCS_post = rowMeans(RCS_post, na.rm = TRUE)


### BID
BID =  center_dat[,201:206]
BID_des  = data.frame(apply(BID, 2, as.factor))
describe(BID_des)
BID = rowMeans(BID, na.rm = TRUE)
BID

### CSE
center_dat$X5_CSE.a

### TSE_bin

## TSE
### Demos
demos = center_dat[,c(75:78,80, 90,92,97:98)]
demos_fac = data.frame(apply(demos, 2, as.factor))
describe(demos_fac)
female = ifelse(demos$X10_Sex == 2, 1, 0)
demos
demos$X12_SO[demos$X12_SO == 4] = NA 
veteran = ifelse(demos$X11_Veteran == 2, 1,0)
sexual_minority = ifelse(demos$X12_SO != 3, 1, 0)
hispanic = ifelse(demos$X13_Hisp.Yes.No. == 1, 1, 0)
white = ifelse(demos$X14_Race.e == 1, 1,0)
black = ifelse(demos$X14_Race.c == 1, 1, 0)
another_race = ifelse(demos$X14_Race.e == 0 & demos$X14_Race.c == 0,1, 0)

high_school_greater = ifelse(demos$X16_Education < 3,0, ifelse(demos$X16_Education == 3,1,2))


employed = ifelse(demos$X17_Employment == 2 | demos$X17_Employment == 3, 1, 0)

demos = data.frame(age = demos$X9_Age, veteran, sexual_minority, hispanic, white, black, another_race, high_school_greater, employed)


### SIS 1 
# Resolved  = a, b, c, d, g, j, k 
# Suicidal ideation = e, f, h, i
head(center_dat[,c(49:50,52:53)])
SIS_1_pre = center_dat[,c(49:50,52:53)]
SIS_1_pre_des = data.frame(apply(SIS_1_pre, 2, as.factor))
describe(SIS_1_pre_des)
head(SIS_1_pre)
SIS_1_pre = rowMeans(SIS_1_pre, na.rm = TRUE)
SIS_1_pre

head(center_dat[,c(45:48, 51, 54:55)])
SIS_2_pre = center_dat[,c(45:48, 51, 54:55)]
SIS_2_pre_des = data.frame(apply(SIS_2_pre, 2, as.factor))
describe(SIS_2_pre_des)
head(SIS_2_pre)
SIS_2_pre = rowMeans(SIS_2_pre, na.rm = TRUE)
SIS_2_pre

head(center_dat[,c(159:160, 162:163)])
SIS_1_post = center_dat[,c(159:160, 162:163)]
SIS_1_post_des = data.frame(apply(SIS_1_post, 2, as.factor))
describe(SIS_1_post_des)
head(SIS_1_post)
SIS_1_post = rowMeans(SIS_1_post, na.rm = TRUE)
SIS_1_post

head(center_dat[,c(155:158, 161, 164:165)])
SIS_2_post =center_dat[,c(155:158, 161, 164:165)]
SIS_2_post_des = data.frame(apply(SIS_2_post, 2, as.factor))
describe(SIS_2_post_des)
head(SIS_2_post)
SIS_2_post = rowMeans(SIS_2_post, na.rm = TRUE)
SIS_2_post

### treatment satisfaction CSQ
CSQ =  data.frame(X12_CSQ.a = center_dat$X12_CSQ.a,X12_CSQ.b = center_dat$X12_CSQ.b, X12_CSQ.c = center_dat$X12_CSQ.c)
CSQ_des = data.frame(apply(CSQ, 2, as.factor))
describe(CSQ_des)
head(CSQ_des)
CSQ = rowMeans(CSQ, na.rm = TRUE)
CSQ

### Find these vars Hypothesis 3: Treatment alliance and treatment satisfaction will be positively and uniquely associated with willingness to seek future help at discharge.
WAI = center_dat[,166:169]
WAI_desc = data.frame(apply(WAI, 2, as.factor))
describe(WAI_desc)
WAI = rowMeans(WAI, na.rm = TRUE)

### CSE Use problem focused coping 1-6; Stop unpleasant emotions and thoughts 7-10; get support from friends and family 11-13
CSE =  center_dat[,138:150]
CSE_desc = data.frame(apply(CSE,2,as.factor))
describe(CSE_desc)

CSE_1 = CSE[,1:6]
CSE_1 = rowMeans(CSE_1, na.rm = TRUE)
CSE_2 = CSE[,7:10]
CSE_2 = rowMeans(CSE_2, na.rm = TRUE)
CSE_3 = CSE[,11:13]
CSE_3 = rowMeans(CSE_3, na.rm = TRUE)


### Treatment needs post for those who received the treatment what was average
treat_needs_post = center_dat[,170:195]
## Get whether someone died by suicide or not need to add in 0's and missing for number of attempts
suicide = center_dat$X4_AttemptedSuic

center_psycho = center_dat 
center_dat = data.frame(demos, INQ_1_pre, INQ_2_pre, RAS_1_pre, RAS_3_pre, RAS_5_pre, ISLES_1_pre, ISLES_2_pre, MILQ_pre, RCS_pre, SIS_1_pre, SIS_2_pre, INQ_1_post, INQ_2_post, RAS_1_post, RAS_3_post, RAS_5_post, ISLES_1_post, ISLES_2_post, MILQ_post, RCS_post, SIS_1_post, SIS_2_post, CSQ, BID, WAI, CSE_1, CSE_2, CSE_3, treat_needs_post, suicide, female)


demos
```
Psychometrics for BID
```{r}
library(psych)
BID_psycho =  center_psycho[,201:206]
summary(omega(BID_psycho))

efa3 = fa(r = BID_psycho, nfactors = 3, fm = "gls", cor = "poly")
efa3
fa.diagram(efa3)

efa2 = fa(r = BID_psycho, nfactors = 2, fm = "gls", cor = "poly")
efa2
fa.diagram(efa2)

efa1 = fa(r = BID_psycho, nfactors = 1, fm = "gls", cor = "poly")
efa1
fa.diagram(efa1)

anova(efa3,efa1)
anova(efa3,efa2)
anova(efa2,efa1)
# now try VSS
vss(BID_psycho, n = 3, rotate = "oblimin", fm = "mle", cor = "poly")

# now try paran
library(paran)
BID_psycho_Complete = na.omit(BID_psycho)
paran(BID_psycho_Complete, centile = 95, iterations = 1000, graph = TRUE, cfa = TRUE)


```
Get omegas for all non-BID constructs at pre and only post if only collected at post
```{r}
############### Pre
INQ_1_pre_psycho = center_psycho[,5:9]
INQ_2_pre_psycho = center_psycho[,10:14]
RAS_1_pre_psycho = center_psycho[,20:27]
RAS_3_pre_psycho = center_psycho[,15:19]
RAS_5_pre_psycho = center_psycho[,28:30]
ISLES_1_pre_psycho = center_psycho[,34:36]
ISLES_2_pre_psycho = center_psycho[,37:39]
MILQ_pre_psycho = center_psycho[,40:44]
RCS_pre_psycho  = center_dat[,56:59]
SIS_1_pre_psycho = center_psycho[,c(49:50,52:53)]
SIS_2_pre_psycho = center_psycho[,c(45:48, 51, 54:55)]

library(psych)
alpha_pre_list = list(INQ_1_pre_psycho, INQ_2_pre_psycho, RAS_1_pre_psycho, RAS_3_pre_psycho, RAS_5_pre_psycho, ISLES_1_pre_psycho, ISLES_2_pre_psycho, MILQ_pre_psycho, RCS_pre_psycho, SIS_1_pre_psycho, SIS_2_pre_psycho)


alpha_pre = list()
for(i in 1:length(alpha_pre_list)){
  alpha_pre[[i]] = omega(alpha_pre_list[[i]], poly = TRUE)
  alpha_pre[[i]] = alpha_pre[[i]]$alpha
}
alpha_pre

############ Post
INQ_1_post_psycho = center_psycho[,100:104]
INQ_2_post_psycho = center_psycho[,105:109]
RAS_1_post_psycho = center_psycho[,115:122]
RAS_3_post_psycho = center_psycho[,110:114]
RAS_5_post_psycho = center_psycho[,123:125]
ISLES_1_post_psycho = center_psycho[,132:134]
ISLES_2_post_psycho = center_psycho[,135:137]
MILQ_post_psycho = center_psycho[,127:131]
RCS_post_psycho = center_psycho[,151:154]
RCS_post_psycho[RCS_post_psycho == 10] = NA
SIS_1_post_psycho = center_psycho[,c(159:160, 162:163)]
SIS_2_post_psycho =center_psycho[,c(155:158, 161, 164:165)]

alpha_post_list = list(INQ_1_post_psycho, INQ_2_post_psycho, RAS_1_post_psycho, RAS_3_post_psycho, RAS_5_post_psycho, ISLES_1_post_psycho, ISLES_2_post_psycho, MILQ_post_psycho, RCS_post_psycho, SIS_1_post_psycho, SIS_2_post_psycho)

range_out = list()
for(i in 1:length(alpha_post_list)){
  range_out[[i]] = apply(alpha_post_list[[i]], 2, range, na.rm = TRUE)
}
range_out

alpha_post = list()
for(i in 1:length(alpha_post_list)){
  alpha_post[[i]] = omega(alpha_post_list[[i]], poly = TRUE)
  alpha_post[[i]] = alpha_post[[i]]$alpha
}
alpha_post

########## MAP and paran for pre

vss_pre = list()
for(i in 1:length(alpha_pre_list)){
  vss_pre[[i]] = vss(alpha_pre_list[[i]], n = 3, cor = "poly")
  vss_pre[[i]] = vss_pre[[i]]$map
}
vss_pre

vss_post = list()
for(i in 1:length(alpha_post_list)){
  vss_post[[i]] = vss(alpha_post_list[[i]], n = 3, cor = "poly")
  vss_post[[i]] = vss_post[[i]]$map
}
vss_post

### Need complete data for paran
library(paran)
alpha_pre_list_complete = list()
for(i in 1:length(alpha_pre_list)){
 alpha_pre_list_complete[[i]] = na.omit(alpha_pre_list[[i]])
}
alpha_pre_list_complete

paran_pre = list()
for(i in 1:length(alpha_pre_list_complete)){
  paran_pre[[i]] = paran(alpha_pre_list_complete[[1]], centile = 95, iterations = 1000, graph = TRUE, cfa = TRUE)
  paran_pre[[i]] = paran_pre[[i]]$Retained
}
paran_pre

alpha_post_list_complete = list()
for(i in 1:length(alpha_post_list)){
  alpha_post_list_complete[[i]] = na.omit(alpha_post_list[[i]])
}
alpha_post_list_complete
paran_post = list()
for(i in 1:length(alpha_post_list_complete)){
  paran_post[[i]] = paran(alpha_post_list_complete[[1]], centile = 95, iterations = 1000, graph = TRUE, cfa = TRUE)
  paran_post[[i]] = paran_post[[i]]$Retained
}
paran_post

```
Psycho results
```{r}
alpha_pre
alpha_post
vss_pre
vss_post
paran_pre
paran_post
```
Percentage missing with post assessment
```{r}

library(naniar)
center_dat_missing = center_dat[c(1:31, 64:65)]

miss_var_summary(center_dat_missing)


n_total_cases = dim(center_dat_missing)[1]
n_complete_cases = dim(na.omit(center_dat_missing))[1]
p_complete = round(n_complete_cases / n_total_cases,3) 

florida_missing_post_assessments= miss_case_summary(center_dat_missing[21:31])
florida_missing_post_assessments = subset(florida_missing_post_assessments, pct_miss >= 75)
florida_missing_post_assessments
florida_missing_post_assessments$case
remove_75_cases =  paste(florida_missing_post_assessments$case,sep="", collapse=",")
### Copy from remove_75_cases
center_dat_missing_75 = center_dat_missing[-c(3,8,14,18,26,31,49,50,52,55,60,66,71,74,77,81,86,91,97,98,100,101,105,107,108,109,110,115),]
miss_case_summary(center_dat_missing_75)
n_total_cases_75 = dim(center_dat_missing_75)[1]
n_complete_cases_75 = dim(na.omit(center_dat_missing_75))[1]
p_complete_75 = round(n_complete_cases_75 / n_total_cases_75,3)

p_complete_v_75 = round(n_total_cases_75/n_total_cases,3)
p_complete_v_75


```
Descriptives with complete data


```{r}
library(prettyR)
library(Hmisc)
dim(center_dat)
describe.factor(center_dat$X9_TREAT.a.Received)
center_dat$female = as.factor(center_dat$female)
center_dat[,c(2:9,38,40,42,44,46,48,50,52,54,56,58,60, 62, 64, 65)] = data.frame(apply(center_dat[,c(2:9,38,40,42,44,46,48,50,52,54,56,58,60, 62, 64, 65)],2, as.factor))
center_dat[,c(39,41,43,45,47,49,51,53,55,57,59, 61, 63)] = data.frame(apply(center_dat[,c(39,41,43,45,47,49,51,53,55,57,59, 61, 63)],2, as.numeric))
describe.factor(center_dat$X9_TREAT.a.Received)
center_dat
desc_stats = prettyR::describe(center_dat)

desc_stats_numeric = data.frame(desc_stats$Numeric)
desc_stats_numeric = desc_stats_numeric[c(1,4,5),]
desc_stats_numeric
desc_stats_numeric = data.frame(t(desc_stats_numeric))
write.csv(desc_stats_numeric, "desc_stats_numeric.csv", row.names = TRUE)
desc_stats_numeric = read.csv("desc_stats_numeric.csv", header = TRUE)
colnames(desc_stats_numeric)[1] = "variable"
desc_stats_numeric$percent_missing = 1-(as.numeric(desc_stats_numeric$valid.n) / 118)
desc_stats_numeric[,2:5] = format(round(desc_stats_numeric[,2:5], digits=2), nsmall = 2)
desc_stats_numeric
desc_range= center_dat[,-c(2:9,38,40,42,44,46,48,50,52,54,56,58,60, 62, 64, 65)]
desc_range = apply(desc_range, 2, range, na.rm = TRUE)
desc_range = t(desc_range)
desc_range = round(desc_range,2)
desc_range = paste0(desc_range[,1], sep = ",", desc_range[,2])
desc_stats_numeric$desc_range = desc_range
desc_stats_numeric
write.csv(desc_stats_numeric, "desc_stats_numeric.csv", row.names = FALSE)

desc_stats_factor = data.frame(desc_stats$Factor) 
desc_stats_factor = t(desc_stats_factor)
desc_stats_factor = format(round(desc_stats_factor, digits=2), nsmall = 2)
write.csv(desc_stats_factor, "desc_stats_factor.csv", row.names = TRUE)
desc_stats_factor = read.csv("desc_stats_factor.csv", header = TRUE)
colnames(desc_stats_factor)[1] = "variable"
desc_stats_factor$Percent = desc_stats_factor$Percent/100
write.csv(desc_stats_factor, "desc_stats_factor.csv", row.names = FALSE)



library(naniar)
miss_var_summary(center_dat)
library(psych)
```
Get age categories
15-24
25-34
35-44
45-54
55-64
There are four clients with missing data.
```{r}
age_dat = na.omit(center_dat$age)
length(age_dat)
age_cat = ifelse(age_dat <= 24, "15-24", ifelse(age_dat > 24 & age_dat <=  34, "25-34",ifelse(age_dat > 34 & age_dat <= 44, "35-44", ifelse(age_dat > 44 & age_dat <= 54, "45-54", ifelse(age_dat > 54, "55-64","wrong")))))
range(age_dat, na.rm = TRUE)
age_cat
age_test = data.frame(age_dat, age_cat)
age_test
describe.factor(age_cat)


```


Treat variable
Number and percentage who said yes
Mean and sd for the rating
```{r}


treat_all =  center_psycho[,170:195]

### Change treatment med to NA
library(psych)
describe.factor(treat_all$X9_TREAT.b.Received)
treat_all$X9_TREAT.b.Received[treat_all$X9_TREAT.b.Received == 2] = NA
describe.factor(treat_all$X9_TREAT.b.Received)
treat_receive = treat_all[,c(1,3,5,7,9,11,13,15,17,19,21,23,25)]
treat_receive = data.frame(apply(treat_receive, 2, as.factor))


### Have to get the mean and sd for each one, because the data set will be different
saftey_plan =  treat_all[,1:2]
saftey_plan_yes = subset(saftey_plan, X9_TREAT.a.Received == 1)
dim(saftey_plan_yes)
saftey_plan_yes_mean = mean(saftey_plan_yes$X9_TREAT.a.Score, na.rm = TRUE)
saftey_plan_yes_sd = sd(saftey_plan_yes$X9_TREAT.a.Score, na.rm = TRUE)
saftey_plan_yes_range = range(saftey_plan_yes$X9_TREAT.a.Score, na.rm = TRUE)
saftey_plan_percent= na.omit(saftey_plan$X9_TREAT.a.Received)
saftey_plan_percent = describe.factor(saftey_plan_percent)
saftey_plan_percent = round(saftey_plan_percent, 0)
saftey_plan_percent
saftey_plan_percent_yes = paste0(saftey_plan_percent[1],"(",saftey_plan_percent[2], "%", ")") 
saftey_plan_percent_yes
saftey_plan_percent_no = paste0(saftey_plan_percent[3],"(",saftey_plan_percent[4], "%", ")") 
saftey_plan_percent_no

medication =  treat_all[,3:4]
medication_yes = subset(medication, X9_TREAT.b.Received == 1)
dim(medication_yes)
medication_yes_mean = mean(medication_yes$X9_TREAT.b.Score, na.rm = TRUE)
medication_yes_sd = sd(medication_yes$X9_TREAT.b.Score, na.rm = TRUE)
medication_yes_range = range(medication$X9_TREAT.b.Score, na.rm = TRUE)
medication_percent= na.omit(medication$X9_TREAT.b.Received)
medication_percent = describe.factor(medication_percent)
medication_percent = round(medication_percent, 0)
medication_percent
medication_percent_yes = paste0(medication_percent[1],"(",medication_percent[2], "%", ")")
medication_percent_yes
medication_percent_no = paste0(medication_percent[3],"(",medication_percent[4], "%", ")") 
medication_percent_no


ind_therapy =  treat_all[,5:6]
ind_therapy_yes = subset(ind_therapy, X9_TREAT.c.Received == 1)
dim(ind_therapy_yes)
ind_therapy_yes_mean = mean(ind_therapy_yes$X9_TREAT.c.Score, na.rm = TRUE)
ind_therapy_yes_sd = sd(ind_therapy_yes$X9_TREAT.c.Score, na.rm = TRUE)
ind_therapy_yes_range = range(ind_therapy$X9_TREAT.c.Score, na.rm = TRUE)
ind_therapy_percent= na.omit(ind_therapy$X9_TREAT.c.Received)
ind_therapy_percent = describe.factor(ind_therapy_percent)
ind_therapy_percent = round(ind_therapy_percent, 0)
ind_therapy_percent
ind_therapy_percent_yes = paste0(ind_therapy_percent[1],"(",ind_therapy_percent[2], "%", ")") 
ind_therapy_percent_yes
ind_therapy_percent_no = paste0(ind_therapy_percent[3],"(",ind_therapy_percent[4], "%", ")") 
ind_therapy_percent_no


group_therapy =  treat_all[,7:8]
group_therapy_yes = subset(group_therapy, X9_TREAT.d.Received == 1)
dim(group_therapy_yes)
group_therapy_yes_mean = mean(group_therapy_yes$X9_Treat.d.Score, na.rm = TRUE)
group_therapy_yes_sd = sd(group_therapy_yes$X9_Treat.d.Score, na.rm = TRUE)
group_therapy_yes_range = range(group_therapy$X9_Treat.d.Score, na.rm = TRUE)
group_therapy_percent= na.omit(group_therapy$X9_TREAT.d.Received)
group_therapy_percent = describe.factor(group_therapy_percent)
group_therapy_percent = round(group_therapy_percent, 0)
group_therapy_percent
group_therapy_percent_yes = paste0(group_therapy_percent[1],"(",group_therapy_percent[2], "%", ")") 
group_therapy_percent_yes
group_therapy_percent_no = paste0(group_therapy_percent[3],"(",group_therapy_percent[4], "%", ")") 
group_therapy_percent_no


belong =  treat_all[,9:10]
belong_yes = subset(belong, X9_Treat.e.Received == 1)
dim(belong_yes)
belong_yes_mean = mean(belong_yes$X9_Treat.e.Score, na.rm = TRUE)
belong_yes_sd = sd(belong_yes$X9_Treat.e.Score, na.rm = TRUE)
belong_yes_range = range(belong$X9_Treat.e.Score, na.rm = TRUE)
belong_percent= na.omit(belong$X9_Treat.e.Received)
belong_percent = describe.factor(belong_percent)
belong_percent = round(belong_percent, 0)
belong_percent
belong_percent_yes = paste0(belong_percent[1],"(",belong_percent[2], "%", ")") 
belong_percent_yes
belong_percent_no = paste0(belong_percent[3],"(",belong_percent[4], "%", ")") 
belong_percent_no


coping_skills =  treat_all[,11:12]
coping_skills_yes = subset(coping_skills, X9_Treat.f.Received == 1)
dim(coping_skills_yes)
coping_skills_yes_mean = mean(coping_skills_yes$X9_Treat.f.Score, na.rm = TRUE)
coping_skills_yes_sd = sd(coping_skills_yes$X9_Treat.f.Score, na.rm = TRUE)
coping_skills_yes_range = range(coping_skills$X9_Treat.f.Score, na.rm = TRUE)
coping_skills_percent= na.omit(coping_skills$X9_Treat.f.Received)
coping_skills_percent = describe.factor(coping_skills_percent)
coping_skills_percent = round(coping_skills_percent, 0)
coping_skills_percent
coping_skills_percent_yes = paste0(coping_skills_percent[1],"(",coping_skills_percent[2], "%", ")") 
coping_skills_percent_yes
coping_skills_percent_no = paste0(coping_skills_percent[3],"(",coping_skills_percent[4], "%", ")") 
coping_skills_percent_no

meaning =  treat_all[,13:14]
meaning_yes = subset(meaning, X9_Treat.g.Received == 1)
dim(meaning_yes)
meaning_yes_mean = mean(meaning_yes$X9_Treat.g.Score, na.rm = TRUE)
meaning_yes_sd = sd(meaning_yes$X9_Treat.g.Score, na.rm = TRUE)
meaning_yes_range = range(meaning$X9_Treat.g.Score, na.rm = TRUE)
meaning_percent= na.omit(meaning$X9_Treat.g.Received)
meaning_percent = describe.factor(meaning_percent)
meaning_percent = round(meaning_percent, 0)
meaning_percent
meaning_percent_yes = paste0(meaning_percent[1],"(",meaning_percent[2], "%", ")") 
meaning_percent_yes
meaning_percent_no = paste0(meaning_percent[3],"(",meaning_percent[4], "%", ")") 
meaning_percent_no

sense =  treat_all[,15:16]
sense_yes = subset(sense, X9_Treat.h.Received == 1)
dim(sense_yes)
sense_yes_mean = mean(sense_yes$X9_Treat.h.Score, na.rm = TRUE)
sense_yes_sd = sd(sense_yes$X9_Treat.h.Score, na.rm = TRUE)
sense_yes_range = range(sense$X9_Treat.h.Score, na.rm = TRUE)
sense_percent= na.omit(sense$X9_Treat.h.Received)
sense_percent = describe.factor(sense_percent)
sense_percent = round(sense_percent, 0)
sense_percent
sense_percent_yes = paste0(sense_percent[1],"(",sense_percent[2], "%", ")") 
sense_percent_yes
sense_percent_no = paste0(sense_percent[3],"(",sense_percent[4], "%", ")") 
sense_percent_no

burden =  treat_all[,17:18]
burden_yes = subset(burden, X9_Treat.I.Received == 1)
dim(burden_yes)
burden_yes_mean = mean(burden_yes$X9_Treat.I.Score, na.rm = TRUE)
burden_yes_sd = sd(burden_yes$X9_Treat.I.Score, na.rm = TRUE)
burden_yes_range = range(burden$X9_Treat.I.Score, na.rm = TRUE)
burden_percent= na.omit(burden$X9_Treat.I.Received)
burden_percent = describe.factor(burden_percent)
burden_percent = round(burden_percent, 0)
burden_percent
burden_percent_yes = paste0(burden_percent[1],"(",burden_percent[2], "%", ")") 
burden_percent_yes
burden_percent_no = paste0(burden_percent[3],"(",burden_percent[4], "%", ")") 
burden_percent_no

safe =  treat_all[,19:20]
safe_yes = subset(safe, X9_Treat.j.Received == 1)
safe_yes_mean = mean(safe_yes$X9_Treat.j.Score, na.rm = TRUE)
safe_yes_sd = sd(safe_yes$X9_Treat.j.Score, na.rm = TRUE)
safe_yes_range = range(safe$X9_Treat.j.Score, na.rm = TRUE)
safe_percent= na.omit(safe$X9_Treat.j.Received)
safe_percent = describe.factor(safe_percent)
safe_percent = round(safe_percent, 0)
safe_percent
safe_percent_yes = paste0(safe_percent[1],"(",safe_percent[2], "%", ")") 
safe_percent_yes
safe_percent_no = paste0(safe_percent[3],"(",safe_percent[4], "%", ")") 
safe_percent_no

hope =  treat_all[,21:22]
hope_yes = subset(hope, X9_Treat..k.Received == 1)
dim(hope_yes)
hope_yes_mean = mean(hope_yes$X9_Treat.k.Score, na.rm = TRUE)
hope_yes_sd = sd(hope_yes$X9_Treat.k.Score, na.rm = TRUE)
hope_yes_range = range(hope$X9_Treat.k.Score, na.rm = TRUE)
hope_percent= na.omit(hope$X9_Treat..k.Received)
hope_percent = describe.factor(hope_percent)
hope_percent = round(hope_percent, 0)
hope_percent
hope_percent_yes = paste0(hope_percent[1],"(",hope_percent[2], "%", ")") 
hope_percent_yes
hope_percent_no = paste0(hope_percent[3],"(",hope_percent[4], "%", ")") 
hope_percent_no

connect =  treat_all[,23:24]
connect_yes = subset(connect, X9_Treat.L.Received == 1)
dim(connect_yes)
connect_yes_mean = mean(connect_yes$X9_Treat.L.Score, na.rm = TRUE)
connect_yes_sd = sd(connect_yes$X9_Treat.L.Score, na.rm = TRUE)
connect_yes_range = range(connect$X9_Treat.L.Score, na.rm = TRUE)
connect_percent= na.omit(connect$X9_Treat.L.Received)
connect_percent = describe.factor(connect_percent)
connect_percent = round(connect_percent, 0)
connect_percent
connect_percent_yes = paste0(connect_percent[1],"(",connect_percent[2], "%", ")") 
connect_percent_yes
connect_percent_no = paste0(connect_percent[3],"(",connect_percent[4], "%", ")") 
connect_percent_no

needs =  treat_all[,25:26]
needs_yes = subset(needs, X9_Treat.m.Received == 1)
dim(needs_yes)
needs_yes_mean = mean(needs_yes$X9_Treat.m.Score, na.rm = TRUE)
needs_yes_sd = sd(needs_yes$X9_Treat.m.Score, na.rm = TRUE)
needs_yes_range = range(needs$X9_Treat.m.Score, na.rm = TRUE)
needs_percent= na.omit(needs$X9_Treat.m.Received)
needs_percent = describe.factor(needs_percent)
needs_percent = round(needs_percent, 0)
needs_percent
needs_percent_yes = paste0(needs_percent[1],"(",needs_percent[2], "%", ")") 
needs_percent_yes
needs_percent_no = paste0(needs_percent[3],"(",needs_percent[4], "%", ")") 
needs_percent_no

treat_range = data.frame(saftey_plan_yes_range, medication_yes_range, ind_therapy_yes_range, group_therapy_yes_range, belong_yes_range, coping_skills_yes_range, meaning_yes_range, sense_yes_range, burden_yes_range, safe_yes_range, hope_yes_range, connect_yes_range, needs_yes_range)


# treat_range not needed all from 1 to 7 scale make note in table

treat_results = data.frame(saftey_plan_yes_mean, medication_yes_mean, ind_therapy_yes_mean, group_therapy_yes_mean, belong_yes_mean, coping_skills_yes_mean, meaning_yes_mean,sense_yes_mean, burden_yes_mean, safe_yes_mean, hope_yes_mean, connect_yes_mean, needs_yes_mean, saftey_plan_yes_sd, medication_yes_sd, ind_therapy_yes_sd , group_therapy_yes_sd, belong_yes_sd, coping_skills_yes_sd, meaning_yes_sd, sense_yes_sd, burden_yes_sd, safe_yes_sd, hope_yes_sd, connect_yes_sd, needs_yes_sd, saftey_plan_percent_yes, medication_percent_yes, ind_therapy_percent_yes, group_therapy_percent_yes, belong_percent_yes, coping_skills_percent_yes, meaning_percent_yes, sense_percent_yes, burden_percent_yes, safe_percent_yes, hope_percent_yes, connect_percent_yes, needs_percent_yes, saftey_plan_percent_no, medication_percent_no, ind_therapy_percent_no, group_therapy_percent_no, belong_percent_no, coping_skills_percent_no, meaning_percent_no, sense_percent_no, burden_percent_no, safe_percent_no, hope_percent_no, connect_percent_no, needs_percent_no)
treat_results[,1:26] = round(treat_results[,1:26], 2)
treat_results = t(treat_results)
treat_results
treat_results_mean = treat_results[1:13,]
treat_results_sd = treat_results[14:26,]
treat_results_p_yes = treat_results[27:39,]
treat_results_p_no = treat_results[40:52,]

treat_results = data.frame(treat_results_mean, treat_results_sd, treat_results_p_yes, treat_results_p_no)
treat_results
write.csv(treat_results, "treat_results.csv")
```
Effect on suicidal ideation for ind therapy, group therapy, safety plan, burden, meaning, difficultly with life events
Not imputting, because planned missing data (if you did not select anything you selected no)
Difference in standardized average differnece (post-pre) score for suicide for receicing treatment versus not receiving treatment

```{r}
library(descr)
head(center_dat)
center_dat$X9_TREAT.b.Received[center_dat$X9_TREAT.b.Received == 2] = NA
center_dat$SIS_1_diff_extra = center_dat$SIS_1_post-center_dat$SIS_1_pre
center_dat$SIS_1_diff_extra = as.numeric(center_dat$SIS_1_diff_extra)
hist(center_dat$SIS_1_diff_extra)

#g =meaning, l = burden, h = difficult
treatments = data.frame(saftey_plan =center_dat$X9_TREAT.a.Received,  ind_therapy = center_dat$X9_TREAT.c.Received, group_therapy = center_dat$X9_TREAT.d.Received, meaning= center_dat$X9_Treat.g.Received, burden = center_dat$X9_Treat.L.Received, difficult = center_dat$X9_Treat.h.Received)
write.csv(treatments, "treatments.csv", row.names = FALSE)
treatments = read.csv("treatments.csv", header = TRUE)
treatments

## Mean for yes and no of SIS 
mean_sd_list = list()
mean_sd_n_1 = list()
mean_sd_n_0 = list()
mean_sd_list_test = list()

for(i in 1:length(treatments)){
  mean_sd_list[[i]] =  compmeans(center_dat$SIS_1_diff_extra, treatments[[i]])
  mean_sd_list_test[[i]] = mean_sd_list
  mean_sd_list[[i]] = mean_sd_list[[i]][c(1:2,4:5,7:8)]
  #Reorder to mean 1, sd 1, n 1
  mean_sd_list[[i]] = round(mean_sd_list[[i]],2)
  mean_sd_n_1[[i]] = mean_sd_list[[i]][c(2,6,4)]
  mean_sd_n_0[[i]] = mean_sd_list[[i]][c(1,5,3)]
}
mean_sd_list
mean_sd_list_test
mean_sd_n_1[[1]]
mean_sd_n_1 = unlist(mean_sd_n_1)
mean_sd_n_1 = matrix(mean_sd_n_1, ncol= 3, byrow = TRUE)
write.csv(mean_sd_n_1, "mean_sd_n_1.csv", row.names = FALSE)

mean_sd_n_0 = unlist(mean_sd_n_0)
mean_sd_n_0 = matrix(mean_sd_n_0, ncol= 3, byrow = TRUE)
write.csv(mean_sd_n_0, "mean_sd_n_0.csv", row.names = FALSE)


results_list = list()
library(descr)
compmeans(center_dat$SIS_1_diff_extra, center_dat$X9_TREAT.a.Received)
for(i in 1:length(treatments)){
  results_list[[i]] = cohen.d(center_dat$SIS_1_diff_extra, treatments[[i]])
  results_list[[i]] = results_list[[i]]$cohen.d[2]
  
}
results_list[[1]][2]
results_list = unlist(results_list)
results_list = matrix(results_list, ncol = 1, byrow = TRUE)
results_list = data.frame(results_list)
results_list = round(results_list, 3)
results_list
colnames(results_list) = c("cohen_d")
results_list = round(results_list,2)

outcomes = c("Safety Plan", "Individual Therapy", "Group Therapy", "Meaning", "Burden", "Difficult")

results_list = data.frame(outcomes, results_list)

#results_list = results_list[order(abs(results_list$cohen_d), decreasing = TRUE),]

#center_dat$SIS_1_diff_extra = NULL
results_list
write.csv(results_list, "results_list.csv", row.names = FALSE)

### Now t-tests for table five
results_list_t = list()
library(descr)
for(i in 1:length(treatments)){
  results_list_t[[i]] = t.test(center_dat$SIS_1_diff_extra~ treatments[[i]], na.rm = TRUE)
  results_list_t[[i]] = results_list_t[[i]][c(1,3,4)]
}
results_list_t
results_list_t = unlist(results_list_t)
results_list_t = matrix(results_list_t, ncol = 4, byrow = TRUE)
results_list_t = data.frame(results_list_t)
results_list_t = round(results_list_t, 4)
results_list_t
colnames(results_list_t) = c("t_value", "p_value", "lower", "upper")
results_list_t

outcomes = c("Safety Plan", "Individual Therapy", "Group Therapy", "Meaning", "Burden", "Difficult")

results_list_t = data.frame(outcomes, results_list_t)
results_list_t[,2:5] = round(results_list_t[,2:5],2)
results_list_t

results_list_t$outcomes = ifelse(results_list_t$upper > 0 & results_list_t$lower < 0, results_list_t$outcomes, paste0(results_list_t$outcomes, "*"))

results_list_t$ci_95 = paste0(results_list_t$lower, sep = ",", results_list_t$upper)
results_list_t[,4:5] = NULL
results_list_t$p_value = ifelse(results_list_t$p_value <= 0, "<.001", results_list_t$p_value)

#results_list_t$cohen_d = as.numeric(results_list_t$cohen_d)
#results_list_t = results_list_t[order(abs(results_list_t$cohen_d), decreasing = TRUE),]


results_list_t
write.csv(results_list_t, "results_list_t.csv", row.names = FALSE)

describe.factor(treatments$ind_therapy)
compmeans(center_dat$SIS_1_diff_extra, treatments$ind_therapy)
test_diff = data.frame(SIS_1_diff_extra = center_dat$SIS_1_diff_extra, group_therapy = treatments$group_therapy)

test_diff_in = data.frame(SIS_1_diff_extra = center_dat$SIS_1_diff_extra, ind_therapy = treatments$ind_therapy)
### N's are different 
test_diff_in
test_diff
center_dat$SIS_1_diff_extra = NULL
```
Try statistical correction

```{r}
library(konfound)
SIS_1_diff_extra = scale(center_dat$SIS_1_post-center_dat$SIS_1_pre)
center_dat$SIS_1_diff_extra = as.numeric(SIS_1_diff_extra)
treatment_dat = data.frame(saftey_plan =center_dat$X9_TREAT.a.Received,  ind_therapy = center_dat$X9_TREAT.c.Received, group_therapy = center_dat$X9_TREAT.d.Received, meaning= center_dat$X9_Treat.g.Received, burden = center_dat$X9_Treat.L.Received, difficult = center_dat$X9_Treat.h.Received, SIS_1_diff_extra)

test_model = lm(SIS_1_diff_extra ~ saftey_plan + ind_therapy + group_therapy + meaning + burden + difficult,  data = treatment_dat)
library(car)

vif(test_model)
hist(test_model$residuals)
summary(test_model)
konfound(test_model, meaning1)

center_dat$SIS_1_diff_extra = NULL
```
Try predictors of suicide with complete data set
```{r}

head(center_dat)

center_dat_pre = center_dat[,8:18]
center_dat_post = center_dat[,19:29]
diff_scores = center_dat_pre - center_dat_post
diff_scores = data.frame(diff_scores, suicide = center_dat$suicide, BID = center_dat$BID, CSE_1 = center_dat$CSE_1, CSE_2 = center_dat$CSE_2, CSE_3 = center_dat$CSE_3)

reg_suicide = glm(suicide ~ INQ_1_pre + INQ_2_pre + ISLES_1_pre + ISLES_2_pre + MILQ_pre + SIS_2_pre + SIS_1_pre + BID+ CSE_1 + CSE_2 + CSE_3 , data = diff_scores, family = binomial())

summary(reg_suicide)
```


Impute data
```{r}
#head(center_dat)
library(Amelia)

### Change back for analysis 
#center_dat$high_school_greater = ifelse(as.numeric(center_dat$high_school_greater) == 0,0,1)
### Get rid black and another race and reverse white to non-white
##center_dat$another_race = NULL
#center_dat$black = NULL

### Get rid of the treat variables, because they crash R.
### Run descriptives if you want to impute again
#treat_vars  =  center_dat[,c(36:61)]
#center_dat =  data.frame(center_dat[,-c(36:61),])
head(center_dat)
### Create bounds for one var and see what happens
## INQ Post
#dim(center_dat[,8:35])

#range(center_dat$INQ_1_pre, na.rm = TRUE) 
#range(center_dat$RAS_1_pre, na.rm = TRUE) 
#range(center_dat$ISLES_1_post, na.rm = TRUE)
#range(center_dat$MILQ_post, na.rm = TRUE) 
#range(center_dat$RCS_post, na.rm = TRUE) 
#center_dat
bounds = matrix(c(8,1,7, 9,1,7, 10,1,5, 11,1,5, 12,1,5, 13,1,5, 14,1,5, 15,1,7, 16,1,5, 17,1,5, 18,1,5,   19,1,7, 20,1,7, 21,1,5, 22,1,5, 23,1,5, 24,1,5, 25,1,5, 26,1,7, 27,1,5, 28,1,5, 29,1,5, 30,1,4, 31,1,5, 32,1,5, 33,1,10, 34,1,10, 35,1,10),nrow = 28, ncol = 3, byrow = TRUE)
#bounds
dim(center_dat)

#a.out_florida = amelia(x = center_dat_test, m = 5, noms = c("veteran", "sexual_minority", "hispanic", "white", "high_school_greater", "employed", "suicide", "female"), bounds = bounds)
#saveRDS(a.out_florida, file = "a.out_florida.rds")
setwd("S:/Indiana Research & Evaluation/Matthew Hanauer/Centerstone_Study_2019_2020")
a.out_florida = readRDS(file = "a.out_florida.rds")

compare.density(a.out_florida, var = "RAS_1_post")
compare.density(a.out_florida, var = "RAS_3_post")
compare.density(a.out_florida, var = "RAS_3_post")
compare.density(a.out_florida, var = "RAS_5_post")
compare.density(a.out_florida, var = "RCS_post")
compare.density(a.out_florida, var = "ISLES_1_post")
compare.density(a.out_florida, var = "ISLES_2_post")
compare.density(a.out_florida, var = "INQ_1_post")
compare.density(a.out_florida, var = "INQ_2_post")
compare.density(a.out_florida, var = "MILQ_post")
compare.density(a.out_florida, var = "SIS_1_post")
compare.density(a.out_florida, var = "SIS_2_post")


```
Load imputed data
```{r}
library(Amelia)
setwd("S:/Indiana Research & Evaluation/Matthew Hanauer/Centerstone_Study_2019_2020")
a.out_florida = readRDS(file = "a.out_florida.rds")
impute_dat_loop = a.out_florida$imputations
#impute_dat_loop
```
Do diff scores with regression, because not random and want to account
```{r}
dim(impute_dat_loop[[1]])
out_diff_dat = list()
head(impute_dat_loop[[1]][8:18])
head(impute_dat_loop[[1]][19:29])
head(impute_dat_loop[[1]])

for(i in 1:length(impute_dat_loop)){
  out_diff_dat[[i]] =  impute_dat_loop[[i]][19:29] - impute_dat_loop[[i]][8:18]
  colnames(out_diff_dat[[i]]) = c("INQ_1_diff", "INQ_2_diff", "RAS_1_diff", "RAS_3_diff", "RAS_5_diff", "ISLES_1_diff", "ISLES_2_diff", "MILQ_diff", "RCS_diff", "SIS_1_diff", "SIS_2_diff")
  out_diff_dat[[i]] = scale(out_diff_dat[[i]])
  out_diff_dat[[i]] =cbind(impute_dat_loop[[i]], out_diff_dat[[i]])
}
out_diff_dat
### Evaluate normality
out_diff_dat_norm = out_diff_dat[[1]][c(30:32,35,38:48)]
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
Pre convergent and divergent reliability
```{r}
library(Hmisc)
cor_dat_pre_1 = impute_dat_loop[[1]][c(8:18)]
cor_dat_pre_1_results = rcorr(as.matrix(cor_dat_pre_1))
cor_dat_pre_2 = impute_dat_loop[[2]][c(8:18)]
cor_dat_pre_2_results = cor(cor_dat_pre_2)
cor_dat_pre_3 = impute_dat_loop[[3]][c(8:18)]
cor_dat_pre_3_results = cor(cor_dat_pre_3)
cor_dat_pre_4 = impute_dat_loop[[4]][c(8:18)]
cor_dat_pre_4_results = cor(cor_dat_pre_4)
cor_dat_pre_5 = impute_dat_loop[[5]][c(8:18)]
cor_dat_pre_5_results = cor(cor_dat_pre_5)

cor_dat_pre_1

flattenCorrMatrix <- function(cormat, pmat) {
  ut <- upper.tri(cormat)
  data.frame(
    row = rownames(cormat)[row(cormat)[ut]],
    column = rownames(cormat)[col(cormat)[ut]],
    cor  =(cormat)[ut],
    p = pmat[ut]
    )
}
cor_dat_pre_1


cor_dat_pre_1 = rcorr(as.matrix(cor_dat_pre_1))
cor_dat_pre_1 = flattenCorrMatrix(cor_dat_pre_1$r, cor_dat_pre_1$P)
var_names =  cor_dat_pre_1[,1:2]
cor_dat_pre_1 = cor_dat_pre_1$cor
cor_dat_pre_1


cor_dat_pre_2 = rcorr(as.matrix(cor_dat_pre_2))
cor_dat_pre_2 = flattenCorrMatrix(cor_dat_pre_2$r, cor_dat_pre_2$P)
cor_dat_pre_2 = cor_dat_pre_2$cor
cor_dat_pre_2

cor_dat_pre_3 = rcorr(as.matrix(cor_dat_pre_3))
cor_dat_pre_3 = flattenCorrMatrix(cor_dat_pre_3$r, cor_dat_pre_3$P)
cor_dat_pre_3 = cor_dat_pre_3$cor
cor_dat_pre_3

cor_dat_pre_4 = rcorr(as.matrix(cor_dat_pre_4))
cor_dat_pre_4 = flattenCorrMatrix(cor_dat_pre_4$r, cor_dat_pre_4$P)
cor_dat_pre_4 = cor_dat_pre_4$cor
cor_dat_pre_4

cor_dat_pre_5 = rcorr(as.matrix(cor_dat_pre_5))
cor_dat_pre_5 = flattenCorrMatrix(cor_dat_pre_5$r, cor_dat_pre_5$P)
cor_dat_pre_5 = cor_dat_pre_5$cor
cor_dat_pre_5

cor_dat_pre_dat = data.frame(cor_dat_pre_1, cor_dat_pre_2, cor_dat_pre_3, cor_dat_pre_4, cor_dat_pre_5)

cor_dat_pre_dat = apply(cor_dat_pre_dat, 1, mean)
cor_dat_pre_dat = data.frame(var_names, cor_dat_pre_dat)
cor_dat_pre_dat[,3] = round(cor_dat_pre_dat[,3], 2)
cor_dat_pre_dat= cor_dat_pre_dat[order(cor_dat_pre_dat$row),]
cor_dat_pre_dat
#perceived burdensomeness, thwarted belongingness  
#cor_dat_pre_dat$row = ifelse(cor_dat_pre_dat$row == "INQ_1_pre", "perceived burdensomeness", ifelse(cor_dat_pre_dat$row, "INQ_2_pre", "thwarted belongingness", ifelse(cor_dat_pre_dat$row == "ISLES_1_pre", "footing the world", ifelse(cor_dat_pre_dat$row == "ISLES_2_pre"))))
#write.csv(cor_dat_pre_dat, "cor_dat_pre_dat.csv", row.names = FALSE)

```

Correlations
```{r}
cor_dat_pre_1 = impute_dat_loop[[1]][c(8:18, 31)]
cor_dat_pre_1_results = cor(cor_dat_pre_1)
cor_dat_pre_1_results_bid = round(cor_dat_pre_1_results[,12],2)
cor_dat_pre_2 = impute_dat_loop[[2]][c(8:18, 31)]
cor_dat_pre_2_results = cor(cor_dat_pre_2)
cor_dat_pre_2_results_bid = round(cor_dat_pre_2_results[,12],2)
cor_dat_pre_3 = impute_dat_loop[[3]][c(8:18, 31)]
cor_dat_pre_3_results = cor(cor_dat_pre_3)
cor_dat_pre_3_results_bid = round(cor_dat_pre_3_results[,12],2)
cor_dat_pre_4 = impute_dat_loop[[4]][c(8:18, 31)]
cor_dat_pre_4_results = cor(cor_dat_pre_4)
cor_dat_pre_4_results_bid = round(cor_dat_pre_4_results[,12],2)
cor_dat_pre_5 = impute_dat_loop[[5]][c(8:18, 31)]
cor_dat_pre_5_results = cor(cor_dat_pre_5)
cor_dat_pre_5_results_bid = round(cor_dat_pre_5_results[,12],2)

cor_dat_pre_1_results_bid
cor_dat_pre = data.frame(cor_dat_pre_1_results_bid, cor_dat_pre_2_results_bid, cor_dat_pre_3_results_bid, cor_dat_pre_4_results_bid, cor_dat_pre_5_results_bid)
cor_dat_pre = apply(cor_dat_pre, 1, mean)
write.csv(cor_dat_pre, "cor_dat_pre.csv")

cor_dat_post_1 = impute_dat_loop[[1]][c(19:35)]
cor_dat_post_1_results = cor(cor_dat_post_1)
cor_dat_post_1_results_bid = round(cor_dat_post_1_results[,13],2)
cor_dat_post_2 = impute_dat_loop[[2]][c(19:35)]
cor_dat_post_2_results = cor(cor_dat_post_2)
cor_dat_post_2_results_bid = round(cor_dat_post_2_results[,13],2)
cor_dat_post_3 = impute_dat_loop[[3]][c(19:35)]
cor_dat_post_3_results = cor(cor_dat_post_3)
cor_dat_post_3_results_bid = round(cor_dat_post_3_results[,13],2)
cor_dat_post_4 = impute_dat_loop[[4]][c(19:35)]
cor_dat_post_4_results = cor(cor_dat_post_4)
cor_dat_post_4_results_bid = round(cor_dat_post_4_results[,13],2)
cor_dat_post_5 = impute_dat_loop[[5]][c(19:35)]
cor_dat_post_5_results = cor(cor_dat_post_5)
cor_dat_post_5_results_bid = round(cor_dat_post_5_results[,13],2)

cor_dat_post_1_results_bid
cor_dat_post = data.frame(cor_dat_post_1_results_bid, cor_dat_post_2_results_bid, cor_dat_post_3_results_bid, cor_dat_post_4_results_bid, cor_dat_post_5_results_bid)
cor_dat_post = apply(cor_dat_post, 1, mean)
write.csv(cor_dat_post, "cor_dat_post.csv")

```


Get pre and post scores
```{r}
library(Amelia)
mean_out_pre = list()
sd_out_pre = list()
head(impute_dat_loop[[1]][8:18])
head(impute_dat_loop[[1]][19:35])

for(i in 1:length(impute_dat_loop)){
  mean_out_pre[[i]] = apply(impute_dat_loop[[i]][8:18],2,mean)
  sd_out_pre[[i]] = apply(impute_dat_loop[[i]][8:18], 2, sd)
}
dim(impute_dat_loop[[1]][8:18])

parsout_pre = unlist(mean_out_pre) 
parsout_pre = matrix(parsout_pre, ncol = 11, byrow = TRUE)
parsout_pre
write.csv(parsout_pre, "parsout_pre.csv", row.names = FALSE)

sesout_pre = unlist(sd_out_pre)
sesout_pre = matrix(sesout_pre, ncol = 11, byrow = TRUE)
sesout_pre

pars_sesout_pre = mi.meld(parsout_pre, sesout_pre)
pars_sesout_pre
mean_pre = t(pars_sesout_pre$q.mi)
sd_pre = t(pars_sesout_pre$se.mi)
mean_sd_pre = round(data.frame(mean_pre, sd_pre),2)
mean_sd_pre$names = names(impute_dat_loop[[1]][8:18])           
write.csv(mean_sd_pre, "mean_sd_pre.csv", row.names = TRUE)

mean_out_post = list()
sd_out_post = list()
           
for(i in 1:length(impute_dat_loop)){
  mean_out_post[[i]] = apply(impute_dat_loop[[i]][19:35],2,mean)
  sd_out_post[[i]] = apply(impute_dat_loop[[i]][19:35], 2, sd)
}
dim(impute_dat_loop[[1]][19:35])
parsout_post = unlist(mean_out_post) 
parsout_post = matrix(parsout_post, ncol = 17, byrow = TRUE)
parsout_post
write.csv(parsout_post, "parsout_post.csv", row.names = FALSE)
sesout_post = unlist(sd_out_post)
sesout_post = matrix(sesout_post, ncol = 17, byrow = TRUE)
sesout_post

pars_sesout_post = mi.meld(parsout_post, sesout_post)
pars_sesout_post

mean_post = t(pars_sesout_post$q.mi)
sd_post = t(pars_sesout_post$se.mi)
mean_sd_post = round(data.frame(mean_post, sd_post),2)
mean_sd_post$names = names(impute_dat_loop[[1]][19:35])           
write.csv(mean_sd_post, "mean_sd_post.csv", row.names = TRUE)
mean_sd_post
### Get the differnece scores for t-test and cohen's d
## Need difference mean, differene sd
out_dif_t = list()
mean_out_diff = list()
sd_out_diff = list()

for(i in 1:length(impute_dat_loop)){
  out_dif_t[[i]] = impute_dat_loop[[1]][19:29] - impute_dat_loop[[i]][8:18]
  mean_out_diff[[i]] = apply(out_dif_t[[i]],2,mean)
  sd_out_diff[[i]] = apply(out_dif_t[[i]], 2, sd)
}
parsout_diff = unlist(mean_out_diff) 
parsout_diff = matrix(parsout_diff, ncol = 11, byrow = TRUE)
parsout_diff
write.csv(parsout_diff, "parsout_diff.csv", row.names = FALSE)

sesout_diff = unlist(sd_out_diff)
sesout_diff = matrix(sesout_diff, ncol = 11, byrow = TRUE)
sesout_diff

pars_sesout_diff = mi.meld(parsout_diff, sesout_diff)
pars_sesout_diff
mean_diff = t(pars_sesout_diff$q.mi)
sd_diff = t(pars_sesout_diff$se.mi)
mean_sd_diff = round(data.frame(mean_diff, sd_diff),2)
mean_sd_diff$names = names(out_diff_dat[[i]][38:48]) 
mean_sd_diff
write.csv(mean_sd_diff, "mean_sd_diff.csv", row.names = TRUE)

```


Overall effect board member table for T-test and Cohen's D
T-test code
One sample cohen's D is just diff_score / diff_sd
http://sphweb.bumc.bu.edu/otlt/MPH-Modules/BS/SAS/SAS4-OneSampleTtest/SAS4-OneSampleTtest7.html
https://ncss-wpengine.netdna-ssl.com/wp-content/themes/ncss/pdf/Procedures/PASS/One-Sample_T-Tests_using_Effect_Size.pdf
standard error: https://www.bmj.com/about-bmj/resources-readers/publications/statistics-square-one/7-t-tests
P-value: https://www.cyclismo.org/tutorial/R/pValues.html
```{r}
mean_sd_diff = round(mean_sd_diff[,1:2],2)
mean_sd_diff 
## sd_diff / sqrt(n) 
se_diff = mean_sd_diff$sd_diff / sqrt(dim(center_dat)[1])
se_diff
t_stat = round(mean_sd_diff$mean_diff / se_diff,2)
t_stat
p_values_t = round(2*pt(-abs(t_stat), df = dim(out_diff_dat[[1]])[1]-1),3)
p_values_t = ifelse(p_values_t <= 0, "<.001", p_values_t)
p_values_t
critical_t = abs(qt(0.05/2, dim(out_diff_dat[[1]])[1]-1))

upper_reg_t = mean_sd_diff$mean_diff +(critical_t*se_diff)
lower_reg_t =  mean_sd_diff$mean_diff -(critical_t*se_diff)

ci_95_t = paste0(round(lower_reg_t,2), sep = ",", round(upper_reg_t,2))
cohen_d_t = round(mean_sd_diff$mean_diff / mean_sd_diff$sd_diff,2)
outcomes = c("Perceived Burdensomeness", "Thwarted Belongingness", "Personal confidence and hope", "Goal and Success Orientation", "No domination by symptoms", "Comprehensibility", "Footing in the world", "MILQ", "RCS", "Suicidal Ideation", "Resolved plans and preparations")
t_test_results = data.frame(outcomes, t_stat, p_values_t, ci_95_t, cohen_d_t)
write.csv(t_test_results, "t_test_results.csv", row.names = FALSE)
t_test_results
```
Suicidal Ideation regression with difference score
```{r}
library(car)
library(Amelia)
### Regression analysis
regout_sis_1 = list()
regout_sis_1_sum = list()
parsout_sis_1 = list()
sesout_sis_1 = list()
import_sis_1 = list()
vif_sis_1 = list()
library(relaimpo)
head(out_diff_dat[[1]][,35:45])
head(out_diff_dat[[1]])

for(i in 1:length(out_diff_dat)){
  regout_sis_1[[i]] = lm(SIS_1_diff ~ + RAS_1_diff + RAS_3_diff+ RAS_5_diff + ISLES_1_diff + ISLES_2_diff + MILQ_diff + RCS_diff, data = out_diff_dat[[i]])
  regout_sis_1_sum[[i]] = summary(regout_sis_1[[i]])
  import_sis_1[[i]] = calc.relimp(regout_sis_1[[i]])
  import_sis_1[[i]] = import_sis_1[[i]]@lmg
  parsout_sis_1[[i]] = regout_sis_1_sum[[i]]$coefficients[,1]
  sesout_sis_1[[i]] = regout_sis_1_sum[[i]]$coefficients[,2]
  vif_sis_1[[i]] = vif(regout_sis_1[[i]])
}
vif_sis_1
#10 cols
parsout_sis_1 = unlist(parsout_sis_1) 
parsout_sis_1 = matrix(parsout_sis_1, ncol = 8, byrow = TRUE)
parsout_sis_1

sesout_sis_1 = unlist(sesout_sis_1)
sesout_sis_1 = matrix(sesout_sis_1, ncol = 8, byrow = TRUE)
sesout_sis_1

pars_sesout_sis_1 = mi.meld(parsout_sis_1, sesout_sis_1)
t_stat_reg_sis_1 =  pars_sesout_sis_1$q.mi / pars_sesout_sis_1$se.mi
p_values_reg_sis_1 = 2*pt(-abs(t_stat_reg_sis_1), df = dim(out_diff_dat[[1]])[1]-8)
p_values_reg_sis_1 = format(round(p_values_reg_sis_1, digits=3), nsmall = 2)
p_values_reg_sis_1
p_values_reg_sis_1
critical_t_reg_sis_1 = abs(qt(0.05/2, dim(out_diff_dat[[1]])[1]-8))
critical_t_reg_sis_1
upper_reg_sis_1 = pars_sesout_sis_1$q.mi +(critical_t_reg_sis_1*pars_sesout_sis_1$se.mi)
upper_reg_sis_1 = format(round(upper_reg_sis_1, digits=2), nsmall = 2)
upper_reg_sis_1
lower_reg_sis_1 = pars_sesout_sis_1$q.mi - (critical_t_reg_sis_1*pars_sesout_sis_1$se.mi)
lower_reg_sis_1 = format(round(lower_reg_sis_1, digits=2), nsmall = 2)
ci_95_sis_1 = paste0(upper_reg_sis_1, sep = ",", lower_reg_sis_1)
ci_95_sis_1
import_sis_1 = unlist(import_sis_1)
import_sis_1 = matrix(import_sis_1, ncol = 7, byrow = TRUE)
import_sis_1 = colMeans(import_sis_1)
import_sis_1 

reg_results_sis_1 = data.frame(par_est = t(pars_sesout_sis_1$q.mi), se = t(pars_sesout_sis_1$se.mi), p_value = t(p_values_reg_sis_1), ci_95_sis_1)
reg_results_sis_1
reg_results_sis_1[,1:2] = format(round(reg_results_sis_1[,1:2], digits=2), nsmall = 2)
reg_results_sis_1$var_names = c("Intercept", colnames(out_diff_dat[[1]])[40:46])
reg_results_sis_1 = data.frame(var_names = reg_results_sis_1$var_names, reg_results_sis_1[,1:4])
typeof(reg_results_sis_1$p_value)

write.csv(reg_results_sis_1, "reg_results_sis_1.csv", row.names = FALSE)
reg_results_sis_1 = read.csv("reg_results_sis_1.csv", header = TRUE)
reg_results_sis_1$var_names = as.character(reg_results_sis_1$var_names)

reg_results_sis_1$var_names = ifelse(reg_results_sis_1$p_value < .05, paste0(reg_results_sis_1$var_names, sep = "*"), reg_results_sis_1$var_names)
reg_results_sis_1$p_value = ifelse(reg_results_sis_1$p_value ==.000, "<.001", reg_results_sis_1$p_value)
reg_results_sis_1 = reg_results_sis_1[-c(1),]
reg_results_sis_1$import_sis_1 = import_sis_1
reg_results_sis_1$import_sis_1 = format(round(reg_results_sis_1$import_sis_1, digits=2), nsmall = 2)
reg_results_sis_1

write.csv(reg_results_sis_1, "reg_results_sis_1.csv", row.names  = FALSE)
```
Suicidal ideation with pre scores
```{r}
library(car)
library(Amelia)
### Regression analysis
regout_sis_1 = list()
regout_sis_1_sum = list()
parsout_sis_1 = list()
sesout_sis_1 = list()
import_sis_1 = list()
vif_sis_1 = list()
library(relaimpo)

for(i in 1:length(out_diff_dat)){
  regout_sis_1[[i]] = lm(SIS_1_pre ~  RAS_1_pre + RAS_3_pre+ RAS_5_pre + ISLES_1_pre + ISLES_2_pre + MILQ_pre + RCS_pre, data = out_diff_dat[[i]])
  regout_sis_1_sum[[i]] = summary(regout_sis_1[[i]])
  import_sis_1[[i]] = calc.relimp(regout_sis_1[[i]])
  import_sis_1[[i]] = import_sis_1[[i]]@lmg
  parsout_sis_1[[i]] = regout_sis_1_sum[[i]]$coefficients[,1]
  sesout_sis_1[[i]] = regout_sis_1_sum[[i]]$coefficients[,2]
  vif_sis_1[[i]] = vif(regout_sis_1[[i]])
}
vif_sis_1
#10 cols
parsout_sis_1 = unlist(parsout_sis_1) 
parsout_sis_1 = matrix(parsout_sis_1, ncol = 8, byrow = TRUE)
parsout_sis_1

sesout_sis_1 = unlist(sesout_sis_1)
sesout_sis_1 = matrix(sesout_sis_1, ncol = 8, byrow = TRUE)
sesout_sis_1

pars_sesout_sis_1 = mi.meld(parsout_sis_1, sesout_sis_1)
t_stat_reg_sis_1 =  pars_sesout_sis_1$q.mi / pars_sesout_sis_1$se.mi
p_values_reg_sis_1 = 2*pt(-abs(t_stat_reg_sis_1), df = dim(out_diff_dat[[1]])[1]-8)
p_values_reg_sis_1 = format(round(p_values_reg_sis_1, digits=3), nsmall = 2)
p_values_reg_sis_1
p_values_reg_sis_1
critical_t_reg_sis_1 = abs(qt(0.05/2, dim(out_diff_dat[[1]])[1]-8))
critical_t_reg_sis_1
upper_reg_sis_1 = pars_sesout_sis_1$q.mi +(critical_t_reg_sis_1*pars_sesout_sis_1$se.mi)
upper_reg_sis_1 = format(round(upper_reg_sis_1, digits=2), nsmall = 2)
upper_reg_sis_1
lower_reg_sis_1 = pars_sesout_sis_1$q.mi - (critical_t_reg_sis_1*pars_sesout_sis_1$se.mi)
lower_reg_sis_1 = format(round(lower_reg_sis_1, digits=2), nsmall = 2)
ci_95_sis_1 = paste0(upper_reg_sis_1, sep = ",", lower_reg_sis_1)
ci_95_sis_1
import_sis_1 = unlist(import_sis_1)
import_sis_1 = matrix(import_sis_1, ncol = 7, byrow = TRUE)
import_sis_1 = colMeans(import_sis_1)
import_sis_1 

reg_results_sis_1 = data.frame(par_est = t(pars_sesout_sis_1$q.mi), se = t(pars_sesout_sis_1$se.mi), p_value = t(p_values_reg_sis_1), ci_95_sis_1)
reg_results_sis_1
reg_results_sis_1[,1:2] = format(round(reg_results_sis_1[,1:2], digits=2), nsmall = 2)
reg_results_sis_1$var_names = c("Intercept", colnames(out_diff_dat[[1]])[40:46])
reg_results_sis_1 = data.frame(var_names = reg_results_sis_1$var_names, reg_results_sis_1[,1:4])
typeof(reg_results_sis_1$p_value)

write.csv(reg_results_sis_1, "reg_results_sis_1.csv", row.names = FALSE)
reg_results_sis_1 = read.csv("reg_results_sis_1.csv", header = TRUE)
reg_results_sis_1$var_names = as.character(reg_results_sis_1$var_names)

reg_results_sis_1$var_names = ifelse(reg_results_sis_1$p_value < .05, paste0(reg_results_sis_1$var_names, sep = "*"), reg_results_sis_1$var_names)
reg_results_sis_1$p_value = ifelse(reg_results_sis_1$p_value ==.000, "<.001", reg_results_sis_1$p_value)
reg_results_sis_1 = reg_results_sis_1[-c(1),]
reg_results_sis_1$import_sis_1 = import_sis_1
reg_results_sis_1$import_sis_1 = format(round(reg_results_sis_1$import_sis_1, digits=2), nsmall = 2)
reg_results_sis_1

write.csv(reg_results_sis_1, "reg_results_sis_1.csv", row.names  = FALSE)

```
Suicidal ideation with post scores
```{r}
library(car)
library(Amelia)
### Regression analysis
regout_sis_1 = list()
regout_sis_1_sum = list()
parsout_sis_1 = list()
sesout_sis_1 = list()
import_sis_1 = list()
vif_sis_1 = list()
library(relaimpo)

for(i in 1:length(out_diff_dat)){
  regout_sis_1[[i]] = lm(SIS_1_pre ~ RAS_1_post + RAS_3_post+ RAS_5_post + ISLES_1_post + ISLES_2_post + MILQ_post + RCS_post, data = out_diff_dat[[i]])
  regout_sis_1_sum[[i]] = summary(regout_sis_1[[i]])
  import_sis_1[[i]] = calc.relimp(regout_sis_1[[i]])
  import_sis_1[[i]] = import_sis_1[[i]]@lmg
  parsout_sis_1[[i]] = regout_sis_1_sum[[i]]$coefficients[,1]
  sesout_sis_1[[i]] = regout_sis_1_sum[[i]]$coefficients[,2]
  vif_sis_1[[i]] = vif(regout_sis_1[[i]])
}
vif_sis_1
#10 cols
parsout_sis_1 = unlist(parsout_sis_1) 
parsout_sis_1 = matrix(parsout_sis_1, ncol = 8, byrow = TRUE)
parsout_sis_1

sesout_sis_1 = unlist(sesout_sis_1)
sesout_sis_1 = matrix(sesout_sis_1, ncol = 8, byrow = TRUE)
sesout_sis_1

pars_sesout_sis_1 = mi.meld(parsout_sis_1, sesout_sis_1)
t_stat_reg_sis_1 =  pars_sesout_sis_1$q.mi / pars_sesout_sis_1$se.mi
p_values_reg_sis_1 = 2*pt(-abs(t_stat_reg_sis_1), df = dim(out_diff_dat[[1]])[1]-8)
p_values_reg_sis_1 = format(round(p_values_reg_sis_1, digits=3), nsmall = 2)
p_values_reg_sis_1
p_values_reg_sis_1
critical_t_reg_sis_1 = abs(qt(0.05/2, dim(out_diff_dat[[1]])[1]-8))
critical_t_reg_sis_1
upper_reg_sis_1 = pars_sesout_sis_1$q.mi +(critical_t_reg_sis_1*pars_sesout_sis_1$se.mi)
upper_reg_sis_1 = format(round(upper_reg_sis_1, digits=2), nsmall = 2)
upper_reg_sis_1
lower_reg_sis_1 = pars_sesout_sis_1$q.mi - (critical_t_reg_sis_1*pars_sesout_sis_1$se.mi)
lower_reg_sis_1 = format(round(lower_reg_sis_1, digits=2), nsmall = 2)
ci_95_sis_1 = paste0(upper_reg_sis_1, sep = ",", lower_reg_sis_1)
ci_95_sis_1
import_sis_1 = unlist(import_sis_1)
import_sis_1 = matrix(import_sis_1, ncol = 7, byrow = TRUE)
import_sis_1 = colMeans(import_sis_1)
import_sis_1 

reg_results_sis_1 = data.frame(par_est = t(pars_sesout_sis_1$q.mi), se = t(pars_sesout_sis_1$se.mi), p_value = t(p_values_reg_sis_1), ci_95_sis_1)
reg_results_sis_1
reg_results_sis_1[,1:2] = format(round(reg_results_sis_1[,1:2], digits=2), nsmall = 2)
reg_results_sis_1$var_names = c("Intercept", colnames(out_diff_dat[[1]])[40:46])
reg_results_sis_1 = data.frame(var_names = reg_results_sis_1$var_names, reg_results_sis_1[,1:4])
typeof(reg_results_sis_1$p_value)

write.csv(reg_results_sis_1, "reg_results_sis_1.csv", row.names = FALSE)
reg_results_sis_1 = read.csv("reg_results_sis_1.csv", header = TRUE)
reg_results_sis_1$var_names = as.character(reg_results_sis_1$var_names)

reg_results_sis_1$var_names = ifelse(reg_results_sis_1$p_value < .05, paste0(reg_results_sis_1$var_names, sep = "*"), reg_results_sis_1$var_names)
reg_results_sis_1$p_value = ifelse(reg_results_sis_1$p_value ==.000, "<.001", reg_results_sis_1$p_value)
reg_results_sis_1 = reg_results_sis_1[-c(1),]
reg_results_sis_1$import_sis_1 = import_sis_1
reg_results_sis_1$import_sis_1 = format(round(reg_results_sis_1$import_sis_1, digits=2), nsmall = 2)
reg_results_sis_1

write.csv(reg_results_sis_1, "reg_results_sis_1.csv", row.names  = FALSE)

```


Resolved plans difference scores
```{r}
### Regression analysis
regout_sis_2 = list()
regout_sis_2_sum = list()
parsout_sis_2 = list()
sesout_sis_2 = list()
import_sis_2 = list()
vif_sis_2 = list()
library(relaimpo)
head(out_diff_dat[[1]][,35:45])


for(i in 1:length(out_diff_dat)){
  regout_sis_2[[i]] = lm(SIS_2_diff ~ RAS_1_diff + RAS_3_diff+ RAS_5_diff + ISLES_1_diff + ISLES_2_diff + MILQ_diff + RCS_diff, data = out_diff_dat[[i]])
  regout_sis_2_sum[[i]] = summary(regout_sis_2[[i]])
  import_sis_2[[i]] = calc.relimp(regout_sis_2[[i]])
  import_sis_2[[i]] = import_sis_2[[i]]@lmg
  parsout_sis_2[[i]] = regout_sis_2_sum[[i]]$coefficients[,1]
  sesout_sis_2[[i]] = regout_sis_2_sum[[i]]$coefficients[,2]
  vif_sis_2[[i]] = vif(regout_sis_2[[i]])
}
vif_sis_2
#10 cols
parsout_sis_2 = unlist(parsout_sis_2) 
parsout_sis_2 = matrix(parsout_sis_2, ncol = 8, byrow = TRUE)
parsout_sis_2

sesout_sis_2 = unlist(sesout_sis_2)
sesout_sis_2 = matrix(sesout_sis_2, ncol = 8, byrow = TRUE)
sesout_sis_2

pars_sesout_sis_2 = mi.meld(parsout_sis_2, sesout_sis_2)
t_stat_reg_sis_2 =  pars_sesout_sis_2$q.mi / pars_sesout_sis_2$se.mi
p_values_reg_sis_2 = 2*pt(-abs(t_stat_reg_sis_2), df = dim(out_diff_dat[[1]])[1]-8)
p_values_reg_sis_2 = format(round(p_values_reg_sis_2, digits=3), nsmall = 2)
p_values_reg_sis_2
p_values_reg_sis_2
critical_t_reg_sis_2 = abs(qt(0.05/2, dim(out_diff_dat[[1]])[1]-8))
critical_t_reg_sis_2
upper_reg_sis_2 = pars_sesout_sis_2$q.mi +(critical_t_reg_sis_2*pars_sesout_sis_2$se.mi)
upper_reg_sis_2 = format(round(upper_reg_sis_2, digits=2), nsmall = 2)
upper_reg_sis_2
lower_reg_sis_2 = pars_sesout_sis_2$q.mi - (critical_t_reg_sis_2*pars_sesout_sis_2$se.mi)
lower_reg_sis_2 = format(round(lower_reg_sis_2, digits=2), nsmall = 2)
ci_95_sis_2 = paste0(upper_reg_sis_2, sep = ",", lower_reg_sis_2)
ci_95_sis_2
import_sis_2 = unlist(import_sis_2)
import_sis_2 = matrix(import_sis_2, ncol = 7, byrow = TRUE)
import_sis_2 = colMeans(import_sis_2)
import_sis_2 

reg_results_sis_2 = data.frame(par_est = t(pars_sesout_sis_2$q.mi), se = t(pars_sesout_sis_2$se.mi), p_value = t(p_values_reg_sis_2), ci_95_sis_2)
reg_results_sis_2
reg_results_sis_2[,1:2] = format(round(reg_results_sis_2[,1:2], digits=2), nsmall = 2)
reg_results_sis_2$var_names = c("Intercept", colnames(out_diff_dat[[1]])[40:46])
reg_results_sis_2 = data.frame(var_names = reg_results_sis_2$var_names, reg_results_sis_2[,1:4])
typeof(reg_results_sis_2$p_value)

write.csv(reg_results_sis_2, "reg_results_sis_2.csv", row.names = FALSE)
reg_results_sis_2 = read.csv("reg_results_sis_2.csv", header = TRUE)
reg_results_sis_2$var_names = as.character(reg_results_sis_2$var_names)

reg_results_sis_2$var_names = ifelse(reg_results_sis_2$p_value < .05, paste0(reg_results_sis_2$var_names, sep = "*"), reg_results_sis_2$var_names)
reg_results_sis_2$p_value = ifelse(reg_results_sis_2$p_value ==.000, "<.001", reg_results_sis_2$p_value)
reg_results_sis_2 = reg_results_sis_2[-c(1),]
reg_results_sis_2$import_sis_2 = import_sis_2
reg_results_sis_2$import_sis_2 = format(round(reg_results_sis_2$import_sis_2, digits=2), nsmall = 2)
reg_results_sis_2

write.csv(reg_results_sis_2, "reg_results_sis_2.csv", row.names = FALSE)
```
Resolved plans pre scores
```{r}
### Regression analysis
regout_sis_2 = list()
regout_sis_2_sum = list()
parsout_sis_2 = list()
sesout_sis_2 = list()
import_sis_2 = list()
vif_sis_2 = list()
library(relaimpo)
head(out_diff_dat[[1]][,35:45])


for(i in 1:length(out_diff_dat)){
  regout_sis_2[[i]] = lm(SIS_2_diff ~  RAS_1_pre + RAS_3_pre+ RAS_5_pre + ISLES_1_pre + ISLES_2_pre + MILQ_pre + RCS_pre, data = out_diff_dat[[i]])
  regout_sis_2_sum[[i]] = summary(regout_sis_2[[i]])
  import_sis_2[[i]] = calc.relimp(regout_sis_2[[i]])
  import_sis_2[[i]] = import_sis_2[[i]]@lmg
  parsout_sis_2[[i]] = regout_sis_2_sum[[i]]$coefficients[,1]
  sesout_sis_2[[i]] = regout_sis_2_sum[[i]]$coefficients[,2]
  vif_sis_2[[i]] = vif(regout_sis_2[[i]])
}
vif_sis_2
#10 cols
parsout_sis_2 = unlist(parsout_sis_2) 
parsout_sis_2 = matrix(parsout_sis_2, ncol = 8, byrow = TRUE)
parsout_sis_2

sesout_sis_2 = unlist(sesout_sis_2)
sesout_sis_2 = matrix(sesout_sis_2, ncol = 8, byrow = TRUE)
sesout_sis_2

pars_sesout_sis_2 = mi.meld(parsout_sis_2, sesout_sis_2)
t_stat_reg_sis_2 =  pars_sesout_sis_2$q.mi / pars_sesout_sis_2$se.mi
p_values_reg_sis_2 = 2*pt(-abs(t_stat_reg_sis_2), df = dim(out_diff_dat[[1]])[1]-8)
p_values_reg_sis_2 = format(round(p_values_reg_sis_2, digits=3), nsmall = 2)
p_values_reg_sis_2
p_values_reg_sis_2
critical_t_reg_sis_2 = abs(qt(0.05/2, dim(out_diff_dat[[1]])[1]-10))
critical_t_reg_sis_2
upper_reg_sis_2 = pars_sesout_sis_2$q.mi +(critical_t_reg_sis_2*pars_sesout_sis_2$se.mi)
upper_reg_sis_2 = format(round(upper_reg_sis_2, digits=2), nsmall = 2)
upper_reg_sis_2
lower_reg_sis_2 = pars_sesout_sis_2$q.mi - (critical_t_reg_sis_2*pars_sesout_sis_2$se.mi)
lower_reg_sis_2 = format(round(lower_reg_sis_2, digits=2), nsmall = 2)
ci_95_sis_2 = paste0(upper_reg_sis_2, sep = ",", lower_reg_sis_2)
ci_95_sis_2
import_sis_2 = unlist(import_sis_2)
import_sis_2 = matrix(import_sis_2, ncol = 7, byrow = TRUE)
import_sis_2 = colMeans(import_sis_2)
import_sis_2 

reg_results_sis_2 = data.frame(par_est = t(pars_sesout_sis_2$q.mi), se = t(pars_sesout_sis_2$se.mi), p_value = t(p_values_reg_sis_2), ci_95_sis_2)
reg_results_sis_2
reg_results_sis_2[,1:2] = format(round(reg_results_sis_2[,1:2], digits=2), nsmall = 2)
reg_results_sis_2$var_names = c("Intercept", colnames(out_diff_dat[[1]])[40:46])
reg_results_sis_2 = data.frame(var_names = reg_results_sis_2$var_names, reg_results_sis_2[,1:4])
typeof(reg_results_sis_2$p_value)

write.csv(reg_results_sis_2, "reg_results_sis_2.csv", row.names = FALSE)
reg_results_sis_2 = read.csv("reg_results_sis_2.csv", header = TRUE)
reg_results_sis_2$var_names = as.character(reg_results_sis_2$var_names)

reg_results_sis_2$var_names = ifelse(reg_results_sis_2$p_value < .05, paste0(reg_results_sis_2$var_names, sep = "*"), reg_results_sis_2$var_names)
reg_results_sis_2$p_value = ifelse(reg_results_sis_2$p_value ==.000, "<.001", reg_results_sis_2$p_value)
reg_results_sis_2 = reg_results_sis_2[-c(1),]
reg_results_sis_2$import_sis_2 = import_sis_2
reg_results_sis_2$import_sis_2 = format(round(reg_results_sis_2$import_sis_2, digits=2), nsmall = 2)
reg_results_sis_2

write.csv(reg_results_sis_2, "reg_results_sis_2.csv", row.names = FALSE)
```
Resolved plans post scores
```{r}
### Regression analysis
regout_sis_2 = list()
regout_sis_2_sum = list()
parsout_sis_2 = list()
sesout_sis_2 = list()
import_sis_2 = list()
vif_sis_2 = list()
library(relaimpo)
head(out_diff_dat[[1]][,35:45])


for(i in 1:length(out_diff_dat)){
  regout_sis_2[[i]] = lm(SIS_2_diff ~ RAS_1_post + RAS_3_post+ RAS_5_post + ISLES_1_post + ISLES_2_post + MILQ_post + RCS_post, data = out_diff_dat[[i]])
  regout_sis_2_sum[[i]] = summary(regout_sis_2[[i]])
  import_sis_2[[i]] = calc.relimp(regout_sis_2[[i]])
  import_sis_2[[i]] = import_sis_2[[i]]@lmg
  parsout_sis_2[[i]] = regout_sis_2_sum[[i]]$coefficients[,1]
  sesout_sis_2[[i]] = regout_sis_2_sum[[i]]$coefficients[,2]
  vif_sis_2[[i]] = vif(regout_sis_2[[i]])
}
vif_sis_2
#10 cols
parsout_sis_2 = unlist(parsout_sis_2) 
parsout_sis_2 = matrix(parsout_sis_2, ncol = 8, byrow = TRUE)
parsout_sis_2

sesout_sis_2 = unlist(sesout_sis_2)
sesout_sis_2 = matrix(sesout_sis_2, ncol = 8, byrow = TRUE)
sesout_sis_2

pars_sesout_sis_2 = mi.meld(parsout_sis_2, sesout_sis_2)
t_stat_reg_sis_2 =  pars_sesout_sis_2$q.mi / pars_sesout_sis_2$se.mi
p_values_reg_sis_2 = 2*pt(-abs(t_stat_reg_sis_2), df = dim(out_diff_dat[[1]])[1]-8)
p_values_reg_sis_2 = format(round(p_values_reg_sis_2, digits=3), nsmall = 2)
p_values_reg_sis_2
p_values_reg_sis_2
critical_t_reg_sis_2 = abs(qt(0.05/2, dim(out_diff_dat[[1]])[1]-8))
critical_t_reg_sis_2
upper_reg_sis_2 = pars_sesout_sis_2$q.mi +(critical_t_reg_sis_2*pars_sesout_sis_2$se.mi)
upper_reg_sis_2 = format(round(upper_reg_sis_2, digits=2), nsmall = 2)
upper_reg_sis_2
lower_reg_sis_2 = pars_sesout_sis_2$q.mi - (critical_t_reg_sis_2*pars_sesout_sis_2$se.mi)
lower_reg_sis_2 = format(round(lower_reg_sis_2, digits=2), nsmall = 2)
ci_95_sis_2 = paste0(upper_reg_sis_2, sep = ",", lower_reg_sis_2)
ci_95_sis_2
import_sis_2 = unlist(import_sis_2)
import_sis_2 = matrix(import_sis_2, ncol = 7, byrow = TRUE)
import_sis_2 = colMeans(import_sis_2)
import_sis_2 

reg_results_sis_2 = data.frame(par_est = t(pars_sesout_sis_2$q.mi), se = t(pars_sesout_sis_2$se.mi), p_value = t(p_values_reg_sis_2), ci_95_sis_2)
reg_results_sis_2
reg_results_sis_2[,1:2] = format(round(reg_results_sis_2[,1:2], digits=2), nsmall = 2)
reg_results_sis_2$var_names = c("Intercept", colnames(out_diff_dat[[1]])[40:46])
reg_results_sis_2 = data.frame(var_names = reg_results_sis_2$var_names, reg_results_sis_2[,1:4])
typeof(reg_results_sis_2$p_value)

write.csv(reg_results_sis_2, "reg_results_sis_2.csv", row.names = FALSE)
reg_results_sis_2 = read.csv("reg_results_sis_2.csv", header = TRUE)
reg_results_sis_2$var_names = as.character(reg_results_sis_2$var_names)

reg_results_sis_2$var_names = ifelse(reg_results_sis_2$p_value < .05, paste0(reg_results_sis_2$var_names, sep = "*"), reg_results_sis_2$var_names)
reg_results_sis_2$p_value = ifelse(reg_results_sis_2$p_value ==.000, "<.001", reg_results_sis_2$p_value)
reg_results_sis_2 = reg_results_sis_2[-c(1),]
reg_results_sis_2$import_sis_2 = import_sis_2
reg_results_sis_2$import_sis_2 = format(round(reg_results_sis_2$import_sis_2, digits=2), nsmall = 2)
reg_results_sis_2

write.csv(reg_results_sis_2, "reg_results_sis_2.csv", row.names = FALSE)
```

Hypothesis 4:  Treatment alliance and treatment satisfaction will be positively and uniquely associated with intention to follow-through on discharge plans. 
```{r}
### Regression analysis
regout_sis_2 = list()
regout_sis_2_sum = list()
parsout_sis_2 = list()
sesout_sis_2 = list()
import_sis_2 = list()
vif_sis_2 = list()
library(relaimpo)
head(out_diff_dat[[1]][,35:45])


for(i in 1:length(out_diff_dat)){
  regout_sis_2[[i]] = lm(BID ~ RAS_1_diff + RAS_3_diff+ RAS_5_diff + ISLES_1_diff + ISLES_2_diff + MILQ_diff + RCS_diff, data = out_diff_dat[[i]])
  regout_sis_2_sum[[i]] = summary(regout_sis_2[[i]])
  import_sis_2[[i]] = calc.relimp(regout_sis_2[[i]])
  import_sis_2[[i]] = import_sis_2[[i]]@lmg
  parsout_sis_2[[i]] = regout_sis_2_sum[[i]]$coefficients[,1]
  sesout_sis_2[[i]] = regout_sis_2_sum[[i]]$coefficients[,2]
  vif_sis_2[[i]] = vif(regout_sis_2[[i]])
}
vif_sis_2
#10 cols
parsout_sis_2 = unlist(parsout_sis_2) 
parsout_sis_2 = matrix(parsout_sis_2, ncol = 8, byrow = TRUE)
parsout_sis_2

sesout_sis_2 = unlist(sesout_sis_2)
sesout_sis_2 = matrix(sesout_sis_2, ncol = 8, byrow = TRUE)
sesout_sis_2

pars_sesout_sis_2 = mi.meld(parsout_sis_2, sesout_sis_2)
t_stat_reg_sis_2 =  pars_sesout_sis_2$q.mi / pars_sesout_sis_2$se.mi
p_values_reg_sis_2 = 2*pt(-abs(t_stat_reg_sis_2), df = dim(out_diff_dat[[1]])[1]-8)
p_values_reg_sis_2 = format(round(p_values_reg_sis_2, digits=3), nsmall = 2)
p_values_reg_sis_2
p_values_reg_sis_2
critical_t_reg_sis_2 = abs(qt(0.05/2, dim(out_diff_dat[[1]])[1]-8))
critical_t_reg_sis_2
upper_reg_sis_2 = pars_sesout_sis_2$q.mi +(critical_t_reg_sis_2*pars_sesout_sis_2$se.mi)
upper_reg_sis_2 = format(round(upper_reg_sis_2, digits=2), nsmall = 2)
upper_reg_sis_2
lower_reg_sis_2 = pars_sesout_sis_2$q.mi - (critical_t_reg_sis_2*pars_sesout_sis_2$se.mi)
lower_reg_sis_2 = format(round(lower_reg_sis_2, digits=2), nsmall = 2)
ci_95_sis_2 = paste0(upper_reg_sis_2, sep = ",", lower_reg_sis_2)
ci_95_sis_2
import_sis_2 = unlist(import_sis_2)
import_sis_2 = matrix(import_sis_2, ncol = 7, byrow = TRUE)
import_sis_2 = colMeans(import_sis_2)
import_sis_2 

reg_results_sis_2 = data.frame(par_est = t(pars_sesout_sis_2$q.mi), se = t(pars_sesout_sis_2$se.mi), p_value = t(p_values_reg_sis_2), ci_95_sis_2)
reg_results_sis_2
reg_results_sis_2[,1:2] = format(round(reg_results_sis_2[,1:2], digits=2), nsmall = 2)
reg_results_sis_2$var_names = c("Intercept", colnames(out_diff_dat[[1]])[40:46])
reg_results_sis_2 = data.frame(var_names = reg_results_sis_2$var_names, reg_results_sis_2[,1:4])
typeof(reg_results_sis_2$p_value)

write.csv(reg_results_sis_2, "reg_results_sis_2.csv", row.names = FALSE)
reg_results_sis_2 = read.csv("reg_results_sis_2.csv", header = TRUE)
reg_results_sis_2$var_names = as.character(reg_results_sis_2$var_names)

reg_results_sis_2$var_names = ifelse(reg_results_sis_2$p_value < .05, paste0(reg_results_sis_2$var_names, sep = "*"), reg_results_sis_2$var_names)
reg_results_sis_2$p_value = ifelse(reg_results_sis_2$p_value ==.000, "<.001", reg_results_sis_2$p_value)
reg_results_sis_2 = reg_results_sis_2[-c(1),]
reg_results_sis_2$import_sis_2 = import_sis_2
reg_results_sis_2$import_sis_2 = format(round(reg_results_sis_2$import_sis_2, digits=2), nsmall = 2)
reg_results_sis_2

write.csv(reg_results_sis_2, "reg_results_bid.csv", row.names = FALSE)

```


Hypothesis 1: Perceived burdensomeness, thwarted belongingness, and meaning made of stress will contribute the most variance to suicide risk status scores at discharge. 
Going to check all of them

Get point biserial later: library(polycor)


```{r}
library(car)
### Regression analysis
regout_r3_hyp_1 = list()
regout_r3_hyp_1_sum = list()
parsout_r3_hyp_1 = list()
sesout_r3_hyp_1 = list()
import_r3_hyp_1 = list()
vif_r3_hyp_1 = list()

for(i in 1:length(out_diff_dat)){
  regout_r3_hyp_1[[i]] = glm(suicide ~ INQ_1_diff + INQ_2_diff + ISLES_1_diff + ISLES_2_diff + MILQ_diff + SIS_1_diff+SIS_2_diff  + BID+ CSE_1 + CSE_2 + CSE_3 , data = out_diff_dat[[i]], family = binomial()) 
  regout_r3_hyp_1_sum[[i]] = summary(regout_r3_hyp_1[[i]])
  #import_r3_hyp_1[[i]] = calc.relimp(regout_r3_hyp_1[[i]])
  #import_r3_hyp_1[[i]] = import_r3_hyp_1[[i]]@lmg
  parsout_r3_hyp_1[[i]] = regout_r3_hyp_1_sum[[i]]$coefficients[,1]
  parsout_r3_hyp_1[[i]] = exp(parsout_r3_hyp_1[[i]])
  sesout_r3_hyp_1[[i]] = regout_r3_hyp_1_sum[[i]]$coefficients[,2]
  sesout_r3_hyp_1[[i]] = exp(sesout_r3_hyp_1[[i]])
  vif_r3_hyp_1[[i]] = vif(regout_r3_hyp_1[[i]])
}
vif_r3_hyp_1
#3 cols
parsout_r3_hyp_1 = unlist(parsout_r3_hyp_1) 
parsout_r3_hyp_1 = matrix(parsout_r3_hyp_1, ncol = 12, byrow = TRUE)
parsout_r3_hyp_1

sesout_r3_hyp_1 = unlist(sesout_r3_hyp_1)
sesout_r3_hyp_1 = matrix(sesout_r3_hyp_1, ncol = 12, byrow= TRUE)
sesout_r3_hyp_1

pars_sesout_r3_hyp_1 = mi.meld(parsout_r3_hyp_1, sesout_r3_hyp_1)
t_stat_reg_r3_hyp_1 =  pars_sesout_r3_hyp_1$q.mi / pars_sesout_r3_hyp_1$se.mi
p_values_reg_r3_hyp_1 = 2*pt(-abs(t_stat_reg_r3_hyp_1), df = dim(out_diff_dat[[1]])[1]-12)
p_values_reg_r3_hyp_1 = format(round(p_values_reg_r3_hyp_1, digits=3), nsmall = 2)
p_values_reg_r3_hyp_1
p_values_reg_r3_hyp_1
critical_t_reg_r3_hyp_1 = abs(qt(0.05/2, dim(out_diff_dat[[1]])[1]-12))
critical_t_reg_r3_hyp_1
upper_reg_r3_hyp_1 = pars_sesout_r3_hyp_1$q.mi +(critical_t_reg_r3_hyp_1*pars_sesout_r3_hyp_1$se.mi)
upper_reg_r3_hyp_1 = format(round(upper_reg_r3_hyp_1, digits=2), nsmall = 2)
upper_reg_r3_hyp_1
lower_reg_r3_hyp_1 = pars_sesout_r3_hyp_1$q.mi - (critical_t_reg_r3_hyp_1*pars_sesout_r3_hyp_1$se.mi)
lower_reg_r3_hyp_1 = format(round(lower_reg_r3_hyp_1, digits=2), nsmall = 2)
ci_95_r3_hyp_1 = paste0(upper_reg_r3_hyp_1, sep = ",", lower_reg_r3_hyp_1)
ci_95_r3_hyp_1
#import_r3_hyp_1 = unlist(import_r3_hyp_1)
#import_r3_hyp_1 = matrix(import_r3_hyp_1, ncol = 2, byrow = TRUE)
#import_r3_hyp_1 = colMeans(import_r3_hyp_1)
#import_r3_hyp_1 

reg_results_r3_hyp_1 = data.frame(par_est = t(pars_sesout_r3_hyp_1$q.mi), se = t(pars_sesout_r3_hyp_1$se.mi), p_value = t(p_values_reg_r3_hyp_1), ci_95_r3_hyp_1)
reg_results_r3_hyp_1
reg_results_r3_hyp_1[,1:2] = format(round(reg_results_r3_hyp_1[,1:2], digits=2), nsmall = 2)
reg_results_r3_hyp_1$var_names = c(names(regout_r3_hyp_1[[1]]$coefficients))
reg_results_r3_hyp_1 = data.frame(var_names = reg_results_r3_hyp_1$var_names, reg_results_r3_hyp_1[,1:4])
reg_results_r3_hyp_1

write.csv(reg_results_r3_hyp_1, "reg_results_r3_hyp_1.csv", row.names = FALSE)
reg_results_r3_hyp_1 = read.csv("reg_results_r3_hyp_1.csv", header = TRUE)
reg_results_r3_hyp_1$var_names = as.character(reg_results_r3_hyp_1$var_names)
reg_results_r3_hyp_1

reg_results_r3_hyp_1$var_names = ifelse(reg_results_r3_hyp_1$p_value < .05, paste0(reg_results_r3_hyp_1$var_names, sep = "*"), reg_results_r3_hyp_1$var_names)
reg_results_r3_hyp_1$p_value = ifelse(reg_results_r3_hyp_1$p_value ==.000, "<.001", reg_results_r3_hyp_1$p_value)
reg_results_r3_hyp_1 = reg_results_r3_hyp_1[-c(1),]
reg_results_r3_hyp_1$par_est = format(round(reg_results_r3_hyp_1$par_est, digits=2), nsmall = 2) 
reg_results_r3_hyp_1
#reg_results_r3_hyp_1$import_r3_hyp_1 = import_r3_hyp_1
#reg_results_r3_hyp_1$import_r3_hyp_1 = format(round(reg_results_r3_hyp_1$import_r3_hyp_1, digits=2), nsmall = 2)
reg_results_r3_hyp_1

write.csv(reg_results_r3_hyp_1, "reg_results_r3_hyp_1.csv", row.names = FALSE)
```
Original works fine why not simulation?
```{r}
### Just test out
intercept_eff = 2.81
INQ_1_diff_eff = 1.05
INQ_2_diff_eff = .77
ISLES_1_diff_eff = 1.51
ISLES_2_diff_eff = .91
MILQ_diff_eff = 1.15
SIS_2_diff_eff = 2
SIS_1_diff_eff = .63
BID_eff = 1.29
CSE_1_eff = 1.46
CSE_2_eff = .64

y_out = list()
runis_out = list()
test_rep_out = list()
INQ_1_diff_out = list()
INQ_2_diff_out = list()
ISLES_1_diff_out = list()
ISLES_2_diff_out = list()
SIS_1_diff_out = list()
SIS_2_diff_out = list()
MILQ_diff_out = list()
BID_out = list()
CSE_1_out = list()
CSE_2_out = list()
y_prob_out = list()
dat_sim = list()
test_rep = list()
test_rep_sum = list()
n = seq(from = 120, to = 300, by = 20)

for(i in 1:length(n)){

INQ_1_diff_out[[i]] = rnorm(n[[i]],0,1)
INQ_2_diff_out[[i]] = rnorm(n[[i]],0,1)
ISLES_1_diff_out[[i]] = rnorm(n[[i]],0,1)
ISLES_2_diff_out[[i]] = rnorm(n[[i]],0,1)
MILQ_diff_out[[i]] = rnorm(n[[i]],0,1)
SIS_1_diff_out[[i]] = rnorm(n[[i]],0,1)
SIS_2_diff_out[[i]] = rnorm(n[[i]],0,1)
BID_out[[i]] = rnorm(n[[i]],0,1)
CSE_1_out[[i]] = rnorm(n[[i]],0,1)
CSE_2_out[[i]] = rnorm(n[[i]],0,1)

y_out[[i]] = intercept_eff + INQ_1_diff_eff*INQ_1_diff_out[[i]] +INQ_2_diff_eff*INQ_2_diff_out[[i]]+ISLES_1_diff_eff*ISLES_1_diff_out[[i]]+ ISLES_2_diff_eff*ISLES_2_diff_out[[i]]+MILQ_diff_eff*MILQ_diff_out[[i]]+ SIS_1_diff_eff*SIS_1_diff_out[[i]] +SIS_2_diff_eff*SIS_2_diff_out[[i]]+BID_eff*BID_out[[i]]+CSE_1_eff*CSE_1_out[[i]]+CSE_2_eff*CSE_2_out[[i]]

y_prob_out[[i]] = exp(y_out[[i]])/(1+exp(y_out[[i]]))
runis_out[[i]] = runif(length(INQ_1_diff_out[[i]]), 0, 1) # This is the random error part
y_out[[i]] = ifelse(runis_out[[i]] < y_prob_out[[i]], 1, 0)
dat_sim[[i]] = data.frame(y_out = y_out[[i]], INQ_1_diff_out = INQ_1_diff_out[[i]], INQ_2_diff_out = INQ_2_diff_out[[i]], ISLES_1_diff_out = ISLES_1_diff_out[[i]], ISLES_2_diff_out = ISLES_2_diff_out[[i]], MILQ_diff_out = MILQ_diff_out[[i]], SIS_1_diff_out = SIS_1_diff_out[[i]], SIS_2_diff_out = SIS_2_diff_out[[i]], BID_out = BID_out[[i]], CSE_1_out = CSE_1_out[[i]], CSE_2_out = CSE_2_out[[i]])
test_rep[[i]] = glm(y_out ~ ., family = binomial(), data = dat_sim[[i]])
test_rep_sum[[i]] =  summary(test_rep[[i]])
}
#glm(y_out ~ ., family = "binomial", data =dat_sim[[1]])
test_rep_sum[[1]]

dim(dat_sim[[5]])
```
Try with complete data



Now what predicts BID
```{r}
library(relaimpo)
library(car)
### Regression analysis
regout_BID = list()
regout_BID_sum = list()
parsout_BID = list()
sesout_BID = list()
import_BID = list()
vif_BID = list()
hist(log(out_diff_dat[[1]]$BID))
head(out_diff_dat[[1]])


for(i in 1:length(out_diff_dat)){
  regout_BID[[i]] = lm(BID ~ RAS_1_diff + RAS_3_diff + RAS_5_diff + ISLES_1_diff + ISLES_2_diff + MILQ_diff + RCS_diff + SIS_1_diff + SIS_2_diff, data = out_diff_dat[[i]])
  regout_BID_sum[[i]] = summary(regout_BID[[i]])
  import_BID[[i]] = calc.relimp(regout_BID[[i]])
  import_BID[[i]] = import_BID[[i]]@lmg
  parsout_BID[[i]] = regout_BID_sum[[i]]$coefficients[,1]
  sesout_BID[[i]] = regout_BID_sum[[i]]$coefficients[,2]
  vif_BID[[i]] = vif(regout_BID[[i]])
}
vif_BID
#3 cols
parsout_BID = unlist(parsout_BID) 
parsout_BID = matrix(parsout_BID, ncol = 8, byrow = TRUE)
parsout_BID

sesout_BID = unlist(sesout_BID)
sesout_BID = matrix(sesout_BID, ncol = 8, byrow= TRUE)
sesout_BID

pars_sesout_BID = mi.meld(parsout_BID, sesout_BID)
t_stat_reg_BID =  pars_sesout_BID$q.mi / pars_sesout_BID$se.mi
p_values_reg_BID = 2*pt(-abs(t_stat_reg_BID), df = dim(out_diff_dat[[1]])[1]-8)
p_values_reg_BID = format(round(p_values_reg_BID, digits=3), nsmall = 2)
p_values_reg_BID
critical_t_reg_BID = abs(qt(0.05/2, dim(out_diff_dat[[1]])[1]-8))
critical_t_reg_BID
upper_reg_BID = pars_sesout_BID$q.mi +(critical_t_reg_BID*pars_sesout_BID$se.mi)
upper_reg_BID = format(round(upper_reg_BID, digits=2), nsmall = 2)
upper_reg_BID
lower_reg_BID = pars_sesout_BID$q.mi - (critical_t_reg_BID*pars_sesout_BID$se.mi)
lower_reg_BID = format(round(lower_reg_BID, digits=2), nsmall = 2)
ci_95_BID = paste0(upper_reg_BID, sep = ",", lower_reg_BID)
ci_95_BID
import_BID = unlist(import_BID)
import_BID = matrix(import_BID, ncol = 7, byrow = TRUE)
import_BID = colMeans(import_BID)
import_BID 

reg_results_BID = data.frame(par_est = t(pars_sesout_BID$q.mi), se = t(pars_sesout_BID$se.mi), p_value = t(p_values_reg_BID), ci_95_BID)
reg_results_BID[,1:2] = format(round(reg_results_BID[,1:2], digits=2), nsmall = 2)

reg_results_BID$var_names = c("Intercept" ,colnames(out_diff_dat[[1]])[40:48])
reg_results_BID = data.frame(var_names = reg_results_BID$var_names, reg_results_BID[,1:4])
reg_results_BID

write.csv(reg_results_BID, "reg_results_BID.csv", row.names = FALSE)
reg_results_BID = read.csv("reg_results_BID.csv", header = TRUE)
reg_results_BID$var_names = as.character(reg_results_BID$var_names)
reg_results_BID

reg_results_BID$var_names = ifelse(reg_results_BID$p_value < .05, paste0(reg_results_BID$var_names, sep = "*"), reg_results_BID$var_names)
reg_results_BID$p_value = ifelse(reg_results_BID$p_value ==.000, "<.001", reg_results_BID$p_value)
reg_results_BID = reg_results_BID[-c(1),]
reg_results_BID$par_est = format(round(reg_results_BID$par_est, digits=2), nsmall = 2) 
reg_results_BID
reg_results_BID$import_BID = import_BID
reg_results_BID$import_BID = format(round(reg_results_BID$import_BID, digits=2), nsmall = 2)
reg_results_BID = reg_results_BID[order(abs(as.numeric(reg_results_BID$par_est)), decreasing = TRUE),]
reg_results_BID
write.csv(reg_results_BID, "reg_results_BID.csv", row.names = FALSE)
```
############################################
Imputted data for 75% or greater missing
############################################
Impute data
Run the data diagnositics section prior for center_dat_missing_75
```{r}
center_dat_missing_75$high_school_greater = ifelse(as.numeric(center_dat_missing_75$high_school_greater) == 0,0,1)
center_dat_missing_75$another_race = NULL
center_dat_missing_75$black = NULL

center_dat_missing_75

dim(center_dat_missing_75)
head(center_dat_missing_75)

#setwd("S:/Indiana Research & Evaluation/Matthew Hanauer/Centerstone_Study_2019_2020")
#impute_dat_loop = readRDS(file = "impute_dat_loop.rds")

bounds = matrix(c(8,1,7, 9,1,7, 10,1,5, 11,1,5, 12,1,5, 13,1,5, 14,1,5, 15,1,7, 16,1,5, 17,1,5, 18,1,5,   19,1,7, 20,1,7, 21,1,5, 22,1,5, 23,1,5, 24,1,5, 25,1,5, 26,1,7, 27,1,5, 28,1,5, 29,1,5),nrow = 22, ncol = 3, byrow = TRUE)
#bounds
dim(center_dat_missing_75)
m = 5
#a.out_florida_missing_75 = amelia(x = center_dat_missing_75, m = m, noms = c("veteran", "sexual_minority", "hispanic", "white", "high_school_greater", "employed", "suicide", "female"), bounds = bounds)
#saveRDS(a.out_florida_missing_75, file = "a.out_florida_missing_75.rds")
setwd("S:/Indiana Research & Evaluation/Matthew Hanauer/Centerstone_Study_2019_2020")
a.out_florida_missing_75 = readRDS(file = "a.out_florida_missing_75.rds")
compare.density(a.out_florida_missing_75, var = "RAS_1_post")
compare.density(a.out_florida_missing_75, var = "RAS_3_post")
compare.density(a.out_florida_missing_75, var = "RAS_3_post")
compare.density(a.out_florida_missing_75, var = "RAS_5_post")
compare.density(a.out_florida_missing_75, var = "RCS_post")
compare.density(a.out_florida_missing_75, var = "ISLES_1_post")
compare.density(a.out_florida_missing_75, var = "ISLES_2_post")
compare.density(a.out_florida_missing_75, var = "INQ_1_post")
compare.density(a.out_florida_missing_75, var = "INQ_2_post")
compare.density(a.out_florida_missing_75, var = "MILQ_post")
compare.density(a.out_florida_missing_75, var = "SIS_1_post")
compare.density(a.out_florida_missing_75, var = "SIS_2_post")

impute_dat_loop_missing_75 = list()
for(i in 1:m){
  impute_dat_loop_missing_75[[i]] = a.out_florida_missing_75$imputations[[i]]
}
impute_dat_loop_missing_75
```
Get pre and post scores for missing 75
```{r}

mean_out_pre = list()
sd_out_pre = list()
head(impute_dat_loop_missing_75[[1]][8:18])
head(impute_dat_loop_missing_75[[1]][19:29])

for(i in 1:length(impute_dat_loop_missing_75)){
  mean_out_pre[[i]] = apply(impute_dat_loop_missing_75[[i]][8:18],2,mean)
  sd_out_pre[[i]] = apply(impute_dat_loop_missing_75[[i]][8:18], 2, sd)
}
dim(impute_dat_loop_missing_75[[1]][8:18])

parsout_pre = unlist(mean_out_pre) 
parsout_pre = matrix(parsout_pre, ncol = 11, byrow = TRUE)
parsout_pre
write.csv(parsout_pre, "parsout_pre.csv", row.names = FALSE)

sesout_pre = unlist(sd_out_pre)
sesout_pre = matrix(sesout_pre, ncol = 11, byrow = TRUE)
sesout_pre

pars_sesout_pre = mi.meld(parsout_pre, sesout_pre)
pars_sesout_pre
mean_pre = t(pars_sesout_pre$q.mi)
sd_pre = t(pars_sesout_pre$se.mi)
mean_sd_pre = round(data.frame(mean_pre, sd_pre),2)
mean_sd_pre$names = names(impute_dat_loop_missing_75[[1]][8:18])           
write.csv(mean_sd_pre, "mean_sd_pre_75.csv", row.names = TRUE)

mean_out_post = list()
sd_out_post = list()

for(i in 1:length(impute_dat_loop_missing_75)){
  mean_out_post[[i]] = apply(impute_dat_loop_missing_75[[i]][19:29],2,mean)
  sd_out_post[[i]] = apply(impute_dat_loop_missing_75[[i]][19:29], 2, sd)
}
dim(impute_dat_loop_missing_75[[1]][19:29])
parsout_post = unlist(mean_out_post) 
parsout_post = matrix(parsout_post, ncol = 11, byrow = TRUE)
parsout_post
write.csv(parsout_post, "parsout_post.csv", row.names = FALSE)
sesout_post = unlist(sd_out_post)
sesout_post = matrix(sesout_post, ncol = 11, byrow = TRUE)
sesout_post

pars_sesout_post = mi.meld(parsout_post, sesout_post)
pars_sesout_post

mean_post = t(pars_sesout_post$q.mi)
sd_post = t(pars_sesout_post$se.mi)
mean_sd_post = round(data.frame(mean_post, sd_post),2)
mean_sd_post$names = names(impute_dat_loop_missing_75[[1]][19:29])           
write.csv(mean_sd_post, "mean_sd_post_75.csv", row.names = TRUE)
mean_sd_post
### Get the differnece scores for t-test and cohen's d
## Need difference mean, differene sd
out_dif_t = list()
mean_out_diff = list()
sd_out_diff = list()

for(i in 1:length(impute_dat_loop_missing_75)){
  out_dif_t[[i]] = impute_dat_loop_missing_75[[1]][19:29] - impute_dat_loop_missing_75[[i]][8:18]
  mean_out_diff[[i]] = apply(out_dif_t[[i]],2,mean)
  sd_out_diff[[i]] = apply(out_dif_t[[i]], 2, sd)
}
parsout_diff = unlist(mean_out_diff) 
parsout_diff = matrix(parsout_diff, ncol = 11, byrow = TRUE)
parsout_diff
write.csv(parsout_diff, "parsout_diff.csv", row.names = FALSE)

sesout_diff = unlist(sd_out_diff)
sesout_diff = matrix(sesout_diff, ncol = 11, byrow = TRUE)
sesout_diff

pars_sesout_diff = mi.meld(parsout_diff, sesout_diff)
pars_sesout_diff
mean_diff = t(pars_sesout_diff$q.mi)
sd_diff = t(pars_sesout_diff$se.mi)
mean_sd_diff = round(data.frame(mean_diff, sd_diff),2)
mean_sd_diff$names = c("Perceived Burdensomeness", "Thwarted Belongingness", "Personal confidence and hope", "Goal and Success Orientation", "No domination by symptoms", "Comprehensibility", "Footing in the world", "MILQ", "RCS", "Suicidal Ideation", "Resolved plans and preparations")
mean_sd_diff
write.csv(mean_sd_diff, "mean_sd_diff_75.csv", row.names = TRUE)

```


Overall effect board member table for T-test and Cohen's D
T-test code
One sample cohen's D is just diff_score / diff_sd (Cohen, 1988, p. 48)
http://sphweb.bumc.bu.edu/otlt/MPH-Modules/BS/SAS/SAS4-OneSampleTtest/SAS4-OneSampleTtest7.html
https://ncss-wpengine.netdna-ssl.com/wp-content/themes/ncss/pdf/Procedures/PASS/One-Sample_T-Tests_using_Effect_Size.pdf
standard error: https://www.bmj.com/about-bmj/resources-readers/publications/statistics-square-one/7-t-tests
P-value: https://www.cyclismo.org/tutorial/R/pValues.html
```{r}
mean_sd_diff = round(mean_sd_diff[,1:2],2)
mean_sd_diff 
## sd_diff / sqrt(n) 
se_diff = mean_sd_diff$sd_diff / sqrt(dim(impute_dat_loop_missing_75[[1]])[1])
se_diff
t_stat = round(mean_sd_diff$mean_diff / se_diff,2)
t_stat
p_values_t = round(2*pt(-abs(t_stat), df = dim(impute_dat_loop_missing_75[[1]])[1]-1),3)
p_values_t = ifelse(p_values_t <= 0, "<.001", p_values_t)
p_values_t
critical_t = abs(qt(0.05/2, dim(impute_dat_loop_missing_75[[1]])[1]-1))

upper_reg_t = mean_sd_diff$mean_diff +(critical_t*se_diff)
lower_reg_t =  mean_sd_diff$mean_diff -(critical_t*se_diff)

ci_95_t = paste0(round(upper_reg_t,2), sep = ",", round(lower_reg_t,2))
cohen_d_t = round(mean_sd_diff$mean_diff / mean_sd_diff$sd_diff,2)
outcomes = c("Perceived Burdensomeness", "Thwarted Belongingness", "Personal confidence and hope", "Goal and Success Orientation", "No domination by symptoms", "Comprehensibility", "Footing in the world", "MILQ", "RCS", "Suicidal Ideation", "Resolved plans and preparations")
t_test_results_75 = data.frame(outcomes, t_stat, p_values_t, ci_95_t, cohen_d_t)
write.csv(t_test_results_75, "t_test_results_75.csv", row.names = FALSE)
t_test_results_75
```
#################################################
Results for complete data
#################################################
```{r}
center_dat_missing = center_dat[c(1:31, 64:65)]
center_dat_complete_analysis = na.omit(center_dat_missing)
head(center_dat)
dim(center_dat_complete_analysis)
center_dat_complete_analysis$black = NULL
center_dat_complete_analysis$another_race = NULL
mean_pre_complete =  apply(center_dat_complete_analysis[8:18], 2, mean)
sd_pre_complete = apply(center_dat_complete_analysis[8:18], 2, sd)
mean_post_complete = apply(center_dat_complete_analysis[19:29], 2, mean)
sd_post_complete = apply(center_dat_complete_analysis[19:29], 2, sd)
mean_sd_complete = cbind(mean_pre_complete, sd_pre_complete, mean_post_complete, sd_post_complete)
mean_sd_complete = data.frame(mean_sd_complete)
mean_sd_complete = round(mean_sd_complete,2)
mean_sd_complete
write.csv(mean_sd_complete, "mean_sd_pre_complete.csv")
```
Compare differences between the TOT and ITT findings
```{r}
mean_diff_complete = mean_sd_complete$mean_post_complete-mean_sd_complete$mean_pre_complete
se_diff_complete = sd(mean_diff_complete) /sqrt(dim(center_dat_complete_analysis)[1])


t_stat_diff_complete = round(mean_diff_complete / se_diff_complete,2)
t_stat_diff_complete
p_values_t_complete = round(2*pt(-abs(t_stat_diff_complete), df =dim(center_dat_complete_analysis)[1]-1),3)

p_values_t_complete = ifelse(p_values_t_complete <= 0, "<.001", p_values_t_complete)
p_values_t_complete

critical_t_complete = abs(qt(0.05/2, dim(center_dat_complete_analysis)[1]-1))

upper_reg_t_complete = mean_diff_complete +(critical_t_complete*se_diff_complete)
lower_reg_t_complete =  mean_diff_complete -(critical_t_complete*se_diff_complete)

ci_95_t_complete = paste0(round(lower_reg_t_complete,2), sep = ",", round(upper_reg_t_complete,2))
cohen_d_t_complete = round(mean_diff_complete / sd(mean_diff_complete),2)

outcomes = c("Perceived Burdensomeness", "Thwarted Belongingness", "Personal confidence and hope", "Goal and Success Orientation", "No domination by symptoms", "Comprehensibility", "Footing in the world", "MILQ", "RCS", "Suicidal Ideation", "Resolved plans and preparations")
t_test_results_complete = data.frame(outcomes, t_stat_diff_complete, p_values_t_complete, ci_95_t_complete, cohen_d_t_complete)
write.csv(t_test_results_complete, "t_test_results_complete.csv", row.names = FALSE)
t_test_results_complete
```
Difference in Cohen's D because standardized measure on complete and imputted

```{r}
### Run the full imputted results to get this data set
t_test_results
t_test_results_complete

mean(abs(t_test_results_complete$cohen_d_t_complete)-abs(t_test_results$cohen_d_t))


range(abs(t_test_results_complete$cohen_d_t_complete)-abs(t_test_results$cohen_d_t))

ds_impute_complete = data.frame(outcomes = t_test_results_complete$outcomes, cohen_d_t_complete = t_test_results_complete$cohen_d_t_complete, cohen_d_t = t_test_results$cohen_d_t, abs_diff_cohen = abs(t_test_results_complete$cohen_d_t_complete)-abs(t_test_results$cohen_d_t))

ds_impute_complete
mean(ds_impute_complete$abs_diff_cohen)
```
#############################
Discharge data Pre
Rerun first chunk of data cleaning prior to running this model
#############################
```{r}
setwd("P:/Evaluation/TN Lives Count_Writing/florida_results")
discharge_dat = read.csv("Deidentified Lockman data project.csv", header = TRUE, na.strings = c("", " "))
center_dat_discharge = data.frame(CID = center_psycho$CID, demos, INQ_1_pre, INQ_2_pre, RAS_1_pre, RAS_3_pre, RAS_5_pre, ISLES_1_pre, ISLES_2_pre, MILQ_pre, RCS_pre, SIS_1_pre, SIS_2_pre, INQ_1_post, INQ_2_post, RAS_1_post, RAS_3_post, RAS_5_post, ISLES_1_post, ISLES_2_post, MILQ_post, RCS_post, SIS_1_post, SIS_2_post, suicide, female)
head(center_dat)
dim(discharge_dat)
discharge_center_dat = merge(center_dat_discharge, discharge_dat, by = "CID", all.y = TRUE)
dim(discharge_center_dat)
head(discharge_center_dat)
### Change NAs for months, because those are zeros
discharge_center_dat[,37:42][is.na(discharge_center_dat[,37:42])] = 0

### Just keep pre measures
discharge_center_dat = discharge_center_dat[,c(1:19, 33,34,37:42)]
miss_var_summary(discharge_center_dat)
discharge_center_dat_complete = na.omit(discharge_center_dat)
n_discharge = dim(discharge_center_dat_complete)[1]
n_discharge

### Crate a total discharge
discharge_center_dat_complete$total_discharge =  apply(discharge_center_dat_complete[,22:27], 1, sum)
discharge_center_dat_complete
describe.factor(discharge_center_dat_complete$total_discharge)
### Create dataset without outlier
outlier_dat = subset(discharge_center_dat_complete, total_discharge < 15)

```
Participant characteristics
```{r}
dim(discharge_center_dat_complete)
fac_des=apply(discharge_center_dat_complete[,c(3:10, 20:21, 28)], 2, function(x){describe.factor(x)})
fac_des = data.frame(fac_des)
fac_des = t(fac_des)
write.csv(fac_des, "fac_dec.csv")
mean_con = apply(discharge_center_dat_complete[,c(2, 11:19)], 2, mean)
mean_con
sd_con = apply(discharge_center_dat_complete[,c(2, 11:19)], 2, sd)
sd_con
range_con = apply(discharge_center_dat_complete[,c(2, 11:19)], 2, range)
range_con
con_des = rbind(mean_con, sd_con, range_con)
write.csv(con_des,"con_des.csv")
```
Get spearman correlations
```{r}

library(corrplot)
cor_mat = cor(outlier_dat[,c(11:19, 28)], use = "pairwise.complete.obs", method = "spearman")
corrplot(cor_mat, type = "upper", insig = "blank")

```
Try the hurdle model
```{r}
library(pscl)
library(caret)
discharge_center_dat_complete
cor_dat_pre = cor(discharge_center_dat_complete[,c(11:19,28)])
findCorrelation(cor_dat_pre, cutoff = .75)
discharge_hurdle = hurdle(total_discharge ~ INQ_1_pre  +INQ_2_pre +RAS_1_pre + RAS_3_pre + RAS_5_pre  + ISLES_1_pre +ISLES_2_pre + MILQ_pre + RCS_pre, data = discharge_center_dat_complete, dist = "negbin", zero.dist = "binomial")
summary(discharge_hurdle)

```
Sensitivity (get rid of outlier at 16 discharges)
```{r}
outlier_dat = subset(discharge_center_dat_complete, total_discharge < 15)
dim(outlier_dat)
dim(discharge_center_dat_complete)

discharge_hurdle_outlier = hurdle(total_discharge ~ INQ_1_pre  +INQ_2_pre +RAS_1_pre + RAS_3_pre + RAS_5_pre  + ISLES_1_pre +ISLES_2_pre + MILQ_pre + RCS_pre, data = outlier_dat, dist = "negbin", zero.dist = "binomial")
discharge_hurdle_outlier_sum = summary(discharge_hurdle_outlier)
discharge_hurdle_outlier_sum

```
One by one with all data
```{r}

list_vars = as.list(discharge_center_dat_complete[,11:19])
dis_results_out = list()
for(i in 1:length(list_vars)){
  dis_results_out[[i]] = hurdle(total_discharge ~ list_vars[[i]], data = discharge_center_dat_complete, dist = "negbin", zero.dist = "binomial")
  dis_results_out[[i]] = summary(dis_results_out[[i]])
}
dis_results_out

### Try without outlier
list_vars = as.list(discharge_center_dat_complete[,11:19])
dis_results_out = list()
for(i in 1:length(list_vars)){
  dis_results_out[[i]] = hurdle(total_discharge ~ list_vars[[i]], data = discharge_center_dat_complete, dist = "negbin", zero.dist = "binomial")
  dis_results_out[[i]] = summary(dis_results_out[[i]])
}
dis_results_out

### ISLES_1_pre is significant try this variable with some others
list_vars_no_ISLES_1_pre = as.list(discharge_center_dat_complete[,c(11:15, 17:19)])
dis_results_out = list()
for(i in 1:length(list_vars_no_ISLES_1_pre)){
  dis_results_out[[i]] = hurdle(total_discharge ~ list_vars_no_ISLES_1_pre[[i]] +ISLES_1_pre, data = discharge_center_dat_complete, dist = "negbin", zero.dist = "binomial")
  dis_results_out[[i]] = summary(dis_results_out[[i]])
}
dis_results_out
```
#############################
Discharge data Post
#############################
```{r}
setwd("P:/Evaluation/TN Lives Count_Writing/florida_results")
discharge_dat = read.csv("Deidentified Lockman data project.csv", header = TRUE, na.strings = c("", " "))
center_dat_discharge = data.frame(CID = center_psycho$CID, demos, INQ_1_pre, INQ_2_pre, RAS_1_pre, RAS_3_pre, RAS_5_pre, ISLES_1_pre, ISLES_2_pre, MILQ_pre, RCS_pre, SIS_1_pre, SIS_2_pre, INQ_1_post, INQ_2_post, RAS_1_post, RAS_3_post, RAS_5_post, ISLES_1_post, ISLES_2_post, MILQ_post, RCS_post, SIS_1_post, SIS_2_post, suicide, female)
head(center_dat)
dim(discharge_dat)
discharge_center_dat = merge(center_dat_discharge, discharge_dat, by = "CID", all.y = TRUE)
dim(discharge_center_dat)
head(discharge_center_dat)
### Change NAs for months, because those are zeros
discharge_center_dat[,37:42][is.na(discharge_center_dat[,37:42])] = 0
discharge_center_dat
### Just keep post measures
discharge_center_dat = discharge_center_dat[,c(2:10,22:30, 33:34,37:42)]
miss_var_summary(discharge_center_dat)
discharge_center_dat_complete = na.omit(discharge_center_dat)
n_discharge = dim(discharge_center_dat_complete)[1]
n_discharge

### Crate a total discharge
discharge_center_dat_complete$total_discharge =  apply(discharge_center_dat_complete[,21:26], 1, sum)
discharge_center_dat_complete
describe.factor(discharge_center_dat_complete$total_discharge)
```
Participant characteristics
```{r}
fac_des=apply(discharge_center_dat_complete[,c(2:9, 19:20, 27)], 2, function(x){describe.factor(x)})
fac_des = data.frame(fac_des)
fac_des = t(fac_des)
write.csv(fac_des, "fac_des.csv")
mean_con = apply(discharge_center_dat_complete[,c(1, 10:18)], 2, mean)
mean_con
sd_con = apply(discharge_center_dat_complete[,c(1, 10:18)], 2, sd)
sd_con
range_con = apply(discharge_center_dat_complete[,c(1, 10:18)], 2, range)
range_con
con_des = rbind(mean_con, sd_con, range_con)
write.csv(con_des, "con_des.csv")
```

Try the hurdle model
```{r}
library(pscl)
discharge_center_dat_complete

cor_discharge_post =  cor(discharge_center_dat_complete[,c(10:18,27)])
findCorrelation(cor_discharge_post, cutoff = .75, names = TRUE, verbose = TRUE)

discharge_hurdle = hurdle(total_discharge ~ INQ_1_post  +INQ_2_post +RAS_1_post + RAS_3_post + RAS_5_post  + ISLES_1_post +ISLES_2_post + MILQ_post + RCS_post, data = discharge_center_dat_complete, dist = "negbin", zero.dist = "binomial")
summary(discharge_hurdle)
exp(1.42287)
```
Try without outlier
```{r}
library(pscl)

outlier_discharge_dat =  subset(discharge_center_dat_complete, total_discharge < 10)
dim(discharge_center_dat_complete)
dim(outlier_discharge_dat)
cor(discharge_center_dat_complete[,c(10:18,27)])

discharge_hurdle = hurdle(total_discharge ~ INQ_1_post  +INQ_2_post +RAS_1_post + RAS_3_post + RAS_5_post  + ISLES_1_post +ISLES_2_post + MILQ_post + RCS_post, data = outlier_discharge_dat, dist = "negbin", zero.dist = "binomial")
summary(discharge_hurdle)


```
One by one with all data
```{r}

list_vars = as.list(discharge_center_dat_complete[,10:18])
dis_results_out = list()
for(i in 1:length(list_vars)){
  dis_results_out[[i]] = hurdle(total_discharge ~ list_vars[[i]], data = discharge_center_dat_complete, dist = "negbin", zero.dist = "binomial")
  dis_results_out[[i]] = summary(dis_results_out[[i]])
}
dis_results_out

###  try without outlier
list_vars = as.list(outlier_discharge_dat[,10:18])
dis_results_out = list()
for(i in 1:length(list_vars)){
  dis_results_out[[i]] = hurdle(total_discharge ~ list_vars[[i]], data = outlier_discharge_dat, dist = "negbin", zero.dist = "binomial")
  dis_results_out[[i]] = summary(dis_results_out[[i]])
}
dis_results_out


```
###########################################
Simulating pre discharge scores
###########################################
Run first two lines of code to get Florida data
To simulate use: https://uvastatlab.github.io/2019/08/29/simulating-data-for-count-models/
```{r}
setwd("P:/Evaluation/TN Lives Count_Writing/florida_results")
discharge_dat = read.csv("Deidentified Lockman data project.csv", header = TRUE, na.strings = c("", " "))
center_dat_discharge = data.frame(CID = center_psycho$CID, demos, INQ_1_pre, INQ_2_pre, RAS_1_pre, RAS_3_pre, RAS_5_pre, ISLES_1_pre, ISLES_2_pre, MILQ_pre, RCS_pre, SIS_1_pre, SIS_2_pre, INQ_1_post, INQ_2_post, RAS_1_post, RAS_3_post, RAS_5_post, ISLES_1_post, ISLES_2_post, MILQ_post, RCS_post, SIS_1_post, SIS_2_post, suicide, female)
head(center_dat)
dim(discharge_dat)
discharge_center_dat = merge(center_dat_discharge, discharge_dat, by = "CID", all.y = TRUE)
dim(discharge_center_dat)
head(discharge_center_dat)
### Change NAs for months, because those are zeros
discharge_center_dat[,37:42][is.na(discharge_center_dat[,37:42])] = 0

### Just keep pre measures
discharge_center_dat = discharge_center_dat[,c(1:19, 33,34,37:42)]
discharge_center_dat_complete = na.omit(discharge_center_dat)
n_discharge = dim(discharge_center_dat_complete)[1]
n_discharge

### Crate a total discharge
discharge_center_dat_complete$total_discharge =  apply(discharge_center_dat_complete[,22:27], 1, sum)
discharge_center_dat_complete
describe.factor(discharge_center_dat_complete$total_discharge)

outlier_dat = subset(discharge_center_dat_complete, total_discharge < 15)

```
Participant characteristics
```{r}
dim(discharge_center_dat_complete)
fac_des=apply(discharge_center_dat_complete[,c(3:10, 20:21, 28)], 2, function(x){describe.factor(x)})
fac_des = data.frame(fac_des)
fac_des = t(fac_des)
write.csv(fac_des, "fac_dec.csv")
mean_con = apply(discharge_center_dat_complete[,c(2, 11:19)], 2, mean)
mean_con
sd_con = apply(discharge_center_dat_complete[,c(2, 11:19)], 2, sd)
sd_con
range_con = apply(discharge_center_dat_complete[,c(2, 11:19)], 2, range)
range_con
con_des = rbind(mean_con, sd_con, range_con)
write.csv(con_des,"con_des.csv")
```




Simple replication for example
```{r}
dim(outlier_dat)
library(pscl)
discharge_hurdle = zeroinfl(formula = total_discharge ~ RAS_1_pre + RAS_3_pre + RAS_5_pre  + ISLES_1_pre +ISLES_2_pre + MILQ_pre + RCS_pre | RAS_1_pre + RAS_3_pre + RAS_5_pre  + ISLES_1_pre +ISLES_2_pre + MILQ_pre + RCS_pre, data = outlier_dat, dist = "negbin")
summary(discharge_hurdle)
```
Get the distributions of each variable
```{r}
dat_hist = as.list(outlier_dat[13:19])
names = as.list(colnames(outlier_dat[13:19]))
for(i in 1:length(dat_hist)){
  hist(dat_hist[[i]], main = paste("Histogram of" ,names[[i]]))
}

```
Demonstrating replication 
```{r}

```


This replicates
```{r}
power_zero_inflat = function(){
n = rep(c(seq(from = 200, to = 600, by =50)), each =200)
#n = c(300, 400)
n = as.list(n)
RAS_1_pre = list()
RAS_3_pre = list()
RAS_5_pre = list()
ISLES_1_pre = list()
ISLES_2_pre = list()
MILQ_pre = list()
RCS_pre = list()
z = list()
y_sim = list()
discharge_hurdle = list()
discharge_hurdle_sum = list()
count_p = list()
logit_p = list()
for(i in 1:length(n)){

RAS_1_pre[[i]]  = rnorm(n = n[[i]], mean = mean(outlier_dat$RAS_1_pre), sd= sd(outlier_dat$RAS_1_pre))
RAS_1_pre[[i]] = ifelse(RAS_1_pre[[i]]> 5, 5, ifelse(RAS_1_pre[[i]]  < 1, 1, RAS_1_pre[[i]]))

RAS_3_pre[[i]]  = rnorm(n = n[[i]], mean = mean(outlier_dat$RAS_3_pre), sd= sd(outlier_dat$RAS_3_pre))
RAS_3_pre[[i]]  = ifelse(RAS_3_pre[[i]] > 5, 5, ifelse(RAS_3_pre[[i]]  < 1, 1, RAS_3_pre[[i]] ))

RAS_5_pre[[i]] = rnorm(n = n[[i]] , mean = mean(outlier_dat$RAS_5_pre), sd= sd(outlier_dat$RAS_5_pre))
RAS_5_pre[[i]]  = ifelse(RAS_5_pre[[i]] > 5, 5, ifelse(RAS_5_pre[[i]]  < 1, 1, RAS_5_pre[[i]]))

ISLES_1_pre[[i]] =  rnorm(n = n[[i]], mean = mean(outlier_dat$ISLES_1_pre), sd= sd(outlier_dat$ISLES_1_pre))
ISLES_1_pre[[i]] = ifelse(ISLES_1_pre[[i]]> 5, 5, ifelse(ISLES_1_pre[[i]] < 1, 1, ISLES_1_pre[[i]]))

ISLES_2_pre[[i]]= rnorm(n = n[[i]], mean = mean(outlier_dat$ISLES_2_pre), sd= sd(outlier_dat$ISLES_2_pre))
ISLES_2_pre[[i]] = ifelse(ISLES_2_pre[[i]]> 5, 5, ifelse(ISLES_2_pre[[i]] < 1, 1, ISLES_2_pre[[i]]))

MILQ_pre[[i]] = rnorm(n = n[[i]], mean = mean(outlier_dat$MILQ_pre), sd= sd(outlier_dat$MILQ_pre))
MILQ_pre[[i]] = ifelse(MILQ_pre[[i]]> 5, 5, ifelse(MILQ_pre[[i]] < 1, 1, MILQ_pre[[i]]))

RCS_pre[[i]]  =rnorm(n = n[[i]], mean = mean(outlier_dat$RCS_pre), sd= sd(outlier_dat$RCS_pre))   
RCS_pre[[i]] = ifelse(RCS_pre[[i]]> 7, 7, ifelse(RCS_pre[[i]] < 1, 1, RCS_pre[[i]]))


z[[i]] <- rbinom(n = n[[i]], size = 1, prob = 1/(1 + exp((0.6465 + -2.2895 * (RAS_1_pre[[i]]) +-0.1523* (RAS_3_pre[[i]]) + 0.8030*(RAS_5_pre[[i]])+ -0.1993*(ISLES_1_pre[[i]])+ -0.1353*(ISLES_2_pre[[i]])+0.7051*(MILQ_pre[[i]])+0.6645*(RCS_pre[[i]]))))) 

y_sim[[i]] <- ifelse(z[[i]] == 0, 0, rnbinom(n = n[[i]],  mu = exp(0.22739 + -0.49252 * (RAS_1_pre[[i]]) + -0.32546* (RAS_3_pre[[i]])+ 0.32722* (RAS_5_pre[[i]])+ -0.22358* (ISLES_1_pre[[i]])+ 0.16369*(ISLES_2_pre[[i]])+ 0.50411*(MILQ_pre[[i]])+ 0.07625*(RCS_pre[[i]])), size = 1.9818))

discharge_hurdle[[i]] = zeroinfl(formula = y_sim[[i]] ~ RAS_1_pre[[i]] + RAS_3_pre[[i]] + RAS_5_pre[[i]] + ISLES_1_pre[[i]] + ISLES_2_pre[[i]] + MILQ_pre[[i]] + RCS_pre[[i]], dist = "negbin")
discharge_hurdle_sum[[i]] = summary(discharge_hurdle[[i]])
count_p[[i]] = ifelse(discharge_hurdle_sum[[i]]$coefficients$count[c(2:8),4] < .05, 1, 0)
logit_p[[i]] = ifelse(discharge_hurdle_sum[[i]]$coefficients$zero[c(2:8),4] < .05, 1, 0)

}
return(list(count_p, logit_p))
}


```



Test out the function
```{r}
power_rep = power_zero_inflat()

power_unlist= unlist(power_rep)
n = rep(c(seq(from = 200, to = 600, by =50)), each =200)
#n = c(300, 400)
#n = rep(c(seq(from = 200, to = 600, by =50)), each =100)
power_matrix = matrix(power_unlist, ncol = 7, nrow = length(n), byrow = TRUE)
power_matrix = data.frame(power_matrix)
colnames(power_matrix)= colnames(outlier_dat[13:19])
power_matrix
power_matrix$n = n 
power_matrix$model = rep(c("count", "binary"), dim(power_matrix)[1]/2)
power_matrix = na.omit(power_matrix)
### sum by n
library(dplyr)
sum_dat = power_matrix %>% 
  group_by(n, model) %>% 
  summarise_all(funs(sum))
sum_dat

count_dat =  power_matrix %>% group_by(n, model) %>% tally()

power_dat = data.frame(sum_dat, count = count_dat$nn)
power_dat[,3:9] =round(power_dat[,3:9]/ power_dat$count,2)
power_dat$count = NULL
power_dat
library(reshape2)

power_long <- melt(power_dat, id.vars=c(1,2))
power_long
colnames(power_long) = c("n", "model", "predictor", "power")
power_long
```
Then plot the power for count model
```{r}
library(ggplot2)
power_long_count = subset(power_long, model == "count")
power_long_count %>%
  ggplot( aes(x=n, y=power, group=predictor, color=predictor)) +
    geom_line()+
    geom_hline(yintercept=.8, linetype="dashed", color = "red")+
  ggtitle("Figure 1: Count model power analysis for relapse by predictor")

```
Now logit
```{r}
library(ggplot2)
power_long_binary = subset(power_long, model == "binary")
power_long_binary %>%
  ggplot( aes(x=n, y=power, group=predictor, color=predictor)) +
    geom_line()+
    geom_hline(yintercept=.8, linetype="dashed", color = "red")+
  ggtitle("Figure 2: Logit model power analysis for relapse by predictor")


```


This replicates: https://uvastatlab.github.io/2019/08/29/simulating-data-for-count-models/#fnref1
```{r}
set.seed(7)
n <- 10000
male <- sample(c(0,1), size = n, replace = TRUE)
z <- rbinom(n = n, size = 1, 
            prob = 1/(1 + exp((0.8 + 1.8 * (male))))) 
y_sim <- ifelse(z == 0, 0, 
                rnbinom(n = n, 
                        mu = exp(1.3 + 1.5 * (male)), 
                        size = 2))

m6 <- zeroinfl(y_sim ~ male | male, dist = "negbin")
summary(m6)
```
Look at beta function
```{r}
b1 <- seq(0, 1, by = 0.02)    
b1
by1 <- round(dbeta(b1, shape1 = 5, shape2 = 20),2) 
by1
plot(by1)
```





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
#install.packages("psych")
#library(psych)
library(prettyR)
```
Create total scores for INQ and RAS
```{r}
## Reverse score f = 6,g = 7, j=10
## This means reverse score 1,2,5 for second construct
INQ_1_pre = center_dat[,5:9]
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

#### RAS 5 No domination by symptoms
RAS_5_pre = center_dat[,28:30]
head(RAS_5_pre)
RAS_5_pre_des = data.frame(apply(RAS_5_pre, 2, as.factor))
describe(RAS_5_pre_des)
head(RAS_5_pre)
RAS_5_pre = rowMeans(RAS_5_pre, na.rm = TRUE)
head(RAS_5_pre)

RAS_5_post = center_dat[,123:125]
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
range(RCS_pre, na.rm = TRUE)

RCS_post = center_dat[,151:154]
RCS_post[RCS_post == 10] = NA
describe(RCS_post)
### overwriting data to get rid of the -99


RCS_post_des = data.frame(apply(RCS_post, 2, as.factor))
describe(RCS_post_des)
head(RCS_post)
RCS_post = rowMeans(RCS_post, na.rm = TRUE)


### BID
BID =  center_dat[,201:206]
BID_des  = data.frame(apply(BID, 2, as.factor))
describe(BID_des)
BID = rowMeans(BID, na.rm = TRUE)
BID

### CSE
center_dat$X5_CSE.a

### TSE_bin

## TSE
### Demos
demos = center_dat[,c(75:78,80, 90,92,97:98)]
demos_fac = data.frame(apply(demos, 2, as.factor))
describe(demos_fac)
female = ifelse(demos$X10_Sex == 2, 1, 0)
demos
demos$X12_SO[demos$X12_SO == 4] = NA 
veteran = ifelse(demos$X11_Veteran == 2, 1,0)
sexual_minority = ifelse(demos$X12_SO != 3, 1, 0)
hispanic = ifelse(demos$X13_Hisp.Yes.No. == 1, 1, 0)
white = ifelse(demos$X14_Race.e == 1, 1,0)
black = ifelse(demos$X14_Race.c == 1, 1, 0)
another_race = ifelse(demos$X14_Race.e == 0 & demos$X14_Race.c == 0,1, 0)

high_school_greater = ifelse(demos$X16_Education < 3,0, ifelse(demos$X16_Education == 3,1,2))


employed = ifelse(demos$X17_Employment == 2 | demos$X17_Employment == 3, 1, 0)

demos = data.frame(age = demos$X9_Age, veteran, sexual_minority, hispanic, white, black, another_race, high_school_greater, employed)


### SIS 1 
# Resolved  = a, b, c, d, g, j, k 
# Suicidal ideation = e, f, h, i
head(center_dat[,c(49:50,52:53)])
SIS_1_pre = center_dat[,c(49:50,52:53)]
SIS_1_pre_des = data.frame(apply(SIS_1_pre, 2, as.factor))
describe(SIS_1_pre_des)
head(SIS_1_pre)
SIS_1_pre = rowMeans(SIS_1_pre, na.rm = TRUE)
SIS_1_pre

head(center_dat[,c(45:48, 51, 54:55)])
SIS_2_pre = center_dat[,c(45:48, 51, 54:55)]
SIS_2_pre_des = data.frame(apply(SIS_2_pre, 2, as.factor))
describe(SIS_2_pre_des)
head(SIS_2_pre)
SIS_2_pre = rowMeans(SIS_2_pre, na.rm = TRUE)
SIS_2_pre

head(center_dat[,c(159:160, 162:163)])
SIS_1_post = center_dat[,c(159:160, 162:163)]
SIS_1_post_des = data.frame(apply(SIS_1_post, 2, as.factor))
describe(SIS_1_post_des)
head(SIS_1_post)
SIS_1_post = rowMeans(SIS_1_post, na.rm = TRUE)
SIS_1_post

head(center_dat[,c(155:158, 161, 164:165)])
SIS_2_post =center_dat[,c(155:158, 161, 164:165)]
SIS_2_post_des = data.frame(apply(SIS_2_post, 2, as.factor))
describe(SIS_2_post_des)
head(SIS_2_post)
SIS_2_post = rowMeans(SIS_2_post, na.rm = TRUE)
SIS_2_post

### treatment satisfaction CSQ
CSQ =  data.frame(X12_CSQ.a = center_dat$X12_CSQ.a,X12_CSQ.b = center_dat$X12_CSQ.b, X12_CSQ.c = center_dat$X12_CSQ.c)
CSQ_des = data.frame(apply(CSQ, 2, as.factor))
describe(CSQ_des)
head(CSQ_des)
CSQ = rowMeans(CSQ, na.rm = TRUE)
CSQ

### Find these vars Hypothesis 3: Treatment alliance and treatment satisfaction will be positively and uniquely associated with willingness to seek future help at discharge.
WAI = center_dat[,166:169]
WAI_desc = data.frame(apply(WAI, 2, as.factor))
describe(WAI_desc)
WAI = rowMeans(WAI, na.rm = TRUE)

### CSE Use problem focused coping 1-6; Stop unpleasant emotions and thoughts 7-10; get support from friends and family 11-13
CSE =  center_dat[,138:150]
CSE_desc = data.frame(apply(CSE,2,as.factor))
describe(CSE_desc)

CSE_1 = CSE[,1:6]
CSE_1 = rowMeans(CSE_1, na.rm = TRUE)
CSE_2 = CSE[,7:10]
CSE_2 = rowMeans(CSE_2, na.rm = TRUE)
CSE_3 = CSE[,11:13]
CSE_3 = rowMeans(CSE_3, na.rm = TRUE)


### Treatment needs post for those who received the treatment what was average
treat_needs_post = center_dat[,170:195]
## Get whether someone died by suicide or not need to add in 0's and missing for number of attempts
suicide = center_dat$X4_AttemptedSuic

center_psycho = center_dat 
center_dat = data.frame(demos, INQ_1_pre, INQ_2_pre, RAS_1_pre, RAS_3_pre, RAS_5_pre, ISLES_1_pre, ISLES_2_pre, MILQ_pre, RCS_pre, SIS_1_pre, SIS_2_pre, INQ_1_post, INQ_2_post, RAS_1_post, RAS_3_post, RAS_5_post, ISLES_1_post, ISLES_2_post, MILQ_post, RCS_post, SIS_1_post, SIS_2_post, CSQ, BID, WAI, CSE_1, CSE_2, CSE_3, treat_needs_post, suicide, female)


demos
```
Psychometrics for BID
```{r}
library(psych)
BID_psycho =  center_psycho[,201:206]
summary(omega(BID_psycho))

efa3 = fa(r = BID_psycho, nfactors = 3, fm = "gls", cor = "poly")
efa3
fa.diagram(efa3)

efa2 = fa(r = BID_psycho, nfactors = 2, fm = "gls", cor = "poly")
efa2
fa.diagram(efa2)

efa1 = fa(r = BID_psycho, nfactors = 1, fm = "gls", cor = "poly")
efa1
fa.diagram(efa1)

anova(efa3,efa1)
anova(efa3,efa2)
anova(efa2,efa1)
# now try VSS
vss(BID_psycho, n = 3, rotate = "oblimin", fm = "mle", cor = "poly")

# now try paran
library(paran)
BID_psycho_Complete = na.omit(BID_psycho)
paran(BID_psycho_Complete, centile = 95, iterations = 1000, graph = TRUE, cfa = TRUE)


```
Get omegas for all non-BID constructs at pre and only post if only collected at post
```{r}
############### Pre
INQ_1_pre_psycho = center_psycho[,5:9]
INQ_2_pre_psycho = center_psycho[,10:14]
RAS_1_pre_psycho = center_psycho[,20:27]
RAS_3_pre_psycho = center_psycho[,15:19]
RAS_5_pre_psycho = center_psycho[,28:30]
ISLES_1_pre_psycho = center_psycho[,34:36]
ISLES_2_pre_psycho = center_psycho[,37:39]
MILQ_pre_psycho = center_psycho[,40:44]
RCS_pre_psycho  = center_dat[,56:59]
SIS_1_pre_psycho = center_psycho[,c(49:50,52:53)]
SIS_2_pre_psycho = center_psycho[,c(45:48, 51, 54:55)]

library(psych)
alpha_pre_list = list(INQ_1_pre_psycho, INQ_2_pre_psycho, RAS_1_pre_psycho, RAS_3_pre_psycho, RAS_5_pre_psycho, ISLES_1_pre_psycho, ISLES_2_pre_psycho, MILQ_pre_psycho, RCS_pre_psycho, SIS_1_pre_psycho, SIS_2_pre_psycho)


alpha_pre = list()
for(i in 1:length(alpha_pre_list)){
  alpha_pre[[i]] = omega(alpha_pre_list[[i]], poly = TRUE)
  alpha_pre[[i]] = alpha_pre[[i]]$alpha
}
alpha_pre

############ Post
INQ_1_post_psycho = center_psycho[,100:104]
INQ_2_post_psycho = center_psycho[,105:109]
RAS_1_post_psycho = center_psycho[,115:122]
RAS_3_post_psycho = center_psycho[,110:114]
RAS_5_post_psycho = center_psycho[,123:125]
ISLES_1_post_psycho = center_psycho[,132:134]
ISLES_2_post_psycho = center_psycho[,135:137]
MILQ_post_psycho = center_psycho[,127:131]
RCS_post_psycho = center_psycho[,151:154]
RCS_post_psycho[RCS_post_psycho == 10] = NA
SIS_1_post_psycho = center_psycho[,c(159:160, 162:163)]
SIS_2_post_psycho =center_psycho[,c(155:158, 161, 164:165)]

alpha_post_list = list(INQ_1_post_psycho, INQ_2_post_psycho, RAS_1_post_psycho, RAS_3_post_psycho, RAS_5_post_psycho, ISLES_1_post_psycho, ISLES_2_post_psycho, MILQ_post_psycho, RCS_post_psycho, SIS_1_post_psycho, SIS_2_post_psycho)

range_out = list()
for(i in 1:length(alpha_post_list)){
  range_out[[i]] = apply(alpha_post_list[[i]], 2, range, na.rm = TRUE)
}
range_out

alpha_post = list()
for(i in 1:length(alpha_post_list)){
  alpha_post[[i]] = omega(alpha_post_list[[i]], poly = TRUE)
  alpha_post[[i]] = alpha_post[[i]]$alpha
}
alpha_post

########## MAP and paran for pre

vss_pre = list()
for(i in 1:length(alpha_pre_list)){
  vss_pre[[i]] = vss(alpha_pre_list[[i]], n = 3, cor = "poly")
  vss_pre[[i]] = vss_pre[[i]]$map
}
vss_pre

vss_post = list()
for(i in 1:length(alpha_post_list)){
  vss_post[[i]] = vss(alpha_post_list[[i]], n = 3, cor = "poly")
  vss_post[[i]] = vss_post[[i]]$map
}
vss_post

### Need complete data for paran
library(paran)
alpha_pre_list_complete = list()
for(i in 1:length(alpha_pre_list)){
 alpha_pre_list_complete[[i]] = na.omit(alpha_pre_list[[i]])
}
alpha_pre_list_complete

paran_pre = list()
for(i in 1:length(alpha_pre_list_complete)){
  paran_pre[[i]] = paran(alpha_pre_list_complete[[1]], centile = 95, iterations = 1000, graph = TRUE, cfa = TRUE)
  paran_pre[[i]] = paran_pre[[i]]$Retained
}
paran_pre

alpha_post_list_complete = list()
for(i in 1:length(alpha_post_list)){
  alpha_post_list_complete[[i]] = na.omit(alpha_post_list[[i]])
}
alpha_post_list_complete
paran_post = list()
for(i in 1:length(alpha_post_list_complete)){
  paran_post[[i]] = paran(alpha_post_list_complete[[1]], centile = 95, iterations = 1000, graph = TRUE, cfa = TRUE)
  paran_post[[i]] = paran_post[[i]]$Retained
}
paran_post

```
Psycho results
```{r}
alpha_pre
alpha_post
vss_pre
vss_post
paran_pre
paran_post
```
Percentage missing with post assessment
```{r}

library(naniar)
center_dat_missing = center_dat[c(1:31, 64:65)]

miss_var_summary(center_dat_missing)


n_total_cases = dim(center_dat_missing)[1]
n_complete_cases = dim(na.omit(center_dat_missing))[1]
p_complete = round(n_complete_cases / n_total_cases,3) 

florida_missing_post_assessments= miss_case_summary(center_dat_missing[21:31])
florida_missing_post_assessments = subset(florida_missing_post_assessments, pct_miss >= 75)
florida_missing_post_assessments
florida_missing_post_assessments$case
remove_75_cases =  paste(florida_missing_post_assessments$case,sep="", collapse=",")
### Copy from remove_75_cases
center_dat_missing_75 = center_dat_missing[-c(3,8,14,18,26,31,49,50,52,55,60,66,71,74,77,81,86,91,97,98,100,101,105,107,108,109,110,115),]
miss_case_summary(center_dat_missing_75)
n_total_cases_75 = dim(center_dat_missing_75)[1]
n_complete_cases_75 = dim(na.omit(center_dat_missing_75))[1]
p_complete_75 = round(n_complete_cases_75 / n_total_cases_75,3)

p_complete_v_75 = round(n_total_cases_75/n_total_cases,3)
p_complete_v_75


```
Descriptives with complete data


```{r}
library(installr)
library(prettyR)
library(Hmisc)
dim(center_dat)
describe.factor(center_dat$X9_TREAT.a.Received)
center_dat$female = as.factor(center_dat$female)
center_dat[,c(2:9,38,40,42,44,46,48,50,52,54,56,58,60, 62, 64, 65)] = data.frame(apply(center_dat[,c(2:9,38,40,42,44,46,48,50,52,54,56,58,60, 62, 64, 65)],2, as.factor))
center_dat[,c(39,41,43,45,47,49,51,53,55,57,59, 61, 63)] = data.frame(apply(center_dat[,c(39,41,43,45,47,49,51,53,55,57,59, 61, 63)],2, as.numeric))
describe.factor(center_dat$X9_TREAT.a.Received)
center_dat
desc_stats = prettyR::describe(center_dat)

desc_stats_numeric = data.frame(desc_stats$Numeric)
desc_stats_numeric = desc_stats_numeric[c(1,4,5),]
desc_stats_numeric
desc_stats_numeric = data.frame(t(desc_stats_numeric))
write.csv(desc_stats_numeric, "desc_stats_numeric.csv", row.names = TRUE)
desc_stats_numeric = read.csv("desc_stats_numeric.csv", header = TRUE)
colnames(desc_stats_numeric)[1] = "variable"
desc_stats_numeric$percent_missing = 1-(as.numeric(desc_stats_numeric$valid.n) / 118)
desc_stats_numeric[,2:5] = format(round(desc_stats_numeric[,2:5], digits=2), nsmall = 2)
desc_stats_numeric
desc_range= center_dat[,-c(2:9,38,40,42,44,46,48,50,52,54,56,58,60, 62, 64, 65)]
desc_range = apply(desc_range, 2, range, na.rm = TRUE)
desc_range = t(desc_range)
desc_range = round(desc_range,2)
desc_range = paste0(desc_range[,1], sep = ",", desc_range[,2])
desc_stats_numeric$desc_range = desc_range
desc_stats_numeric
write.csv(desc_stats_numeric, "desc_stats_numeric.csv", row.names = FALSE)

desc_stats_factor = data.frame(desc_stats$Factor) 
desc_stats_factor = t(desc_stats_factor)
desc_stats_factor = format(round(desc_stats_factor, digits=2), nsmall = 2)
write.csv(desc_stats_factor, "desc_stats_factor.csv", row.names = TRUE)
desc_stats_factor = read.csv("desc_stats_factor.csv", header = TRUE)
colnames(desc_stats_factor)[1] = "variable"
desc_stats_factor$Percent = desc_stats_factor$Percent/100
write.csv(desc_stats_factor, "desc_stats_factor.csv", row.names = FALSE)



library(naniar)
miss_var_summary(center_dat)
library(psych)
```
Get age categories
15-24
25-34
35-44
45-54
55-64
There are four clients with missing data.
```{r}
age_dat = na.omit(center_dat$age)
length(age_dat)
age_cat = ifelse(age_dat <= 24, "15-24", ifelse(age_dat > 24 & age_dat <=  34, "25-34",ifelse(age_dat > 34 & age_dat <= 44, "35-44", ifelse(age_dat > 44 & age_dat <= 54, "45-54", ifelse(age_dat > 54, "55-64","wrong")))))
range(age_dat, na.rm = TRUE)
age_cat
age_test = data.frame(age_dat, age_cat)
age_test
describe.factor(age_cat)


```


Treat variable
Number and percentage who said yes
Mean and sd for the rating
```{r}


treat_all =  center_psycho[,170:195]

### Change treatment med to NA
library(psych)
describe.factor(treat_all$X9_TREAT.b.Received)
treat_all$X9_TREAT.b.Received[treat_all$X9_TREAT.b.Received == 2] = NA
describe.factor(treat_all$X9_TREAT.b.Received)
treat_receive = treat_all[,c(1,3,5,7,9,11,13,15,17,19,21,23,25)]
treat_receive = data.frame(apply(treat_receive, 2, as.factor))


### Have to get the mean and sd for each one, because the data set will be different
saftey_plan =  treat_all[,1:2]
saftey_plan_yes = subset(saftey_plan, X9_TREAT.a.Received == 1)
dim(saftey_plan_yes)
saftey_plan_yes_mean = mean(saftey_plan_yes$X9_TREAT.a.Score, na.rm = TRUE)
saftey_plan_yes_sd = sd(saftey_plan_yes$X9_TREAT.a.Score, na.rm = TRUE)
saftey_plan_yes_range = range(saftey_plan_yes$X9_TREAT.a.Score, na.rm = TRUE)
saftey_plan_percent= na.omit(saftey_plan$X9_TREAT.a.Received)
saftey_plan_percent = describe.factor(saftey_plan_percent)
saftey_plan_percent = round(saftey_plan_percent, 0)
saftey_plan_percent
saftey_plan_percent_yes = paste0(saftey_plan_percent[1],"(",saftey_plan_percent[2], "%", ")") 
saftey_plan_percent_yes
saftey_plan_percent_no = paste0(saftey_plan_percent[3],"(",saftey_plan_percent[4], "%", ")") 
saftey_plan_percent_no

medication =  treat_all[,3:4]
medication_yes = subset(medication, X9_TREAT.b.Received == 1)
dim(medication_yes)
medication_yes_mean = mean(medication_yes$X9_TREAT.b.Score, na.rm = TRUE)
medication_yes_sd = sd(medication_yes$X9_TREAT.b.Score, na.rm = TRUE)
medication_yes_range = range(medication$X9_TREAT.b.Score, na.rm = TRUE)
medication_percent= na.omit(medication$X9_TREAT.b.Received)
medication_percent = describe.factor(medication_percent)
medication_percent = round(medication_percent, 0)
medication_percent
medication_percent_yes = paste0(medication_percent[1],"(",medication_percent[2], "%", ")")
medication_percent_yes
medication_percent_no = paste0(medication_percent[3],"(",medication_percent[4], "%", ")") 
medication_percent_no


ind_therapy =  treat_all[,5:6]
ind_therapy_yes = subset(ind_therapy, X9_TREAT.c.Received == 1)
dim(ind_therapy_yes)
ind_therapy_yes_mean = mean(ind_therapy_yes$X9_TREAT.c.Score, na.rm = TRUE)
ind_therapy_yes_sd = sd(ind_therapy_yes$X9_TREAT.c.Score, na.rm = TRUE)
ind_therapy_yes_range = range(ind_therapy$X9_TREAT.c.Score, na.rm = TRUE)
ind_therapy_percent= na.omit(ind_therapy$X9_TREAT.c.Received)
ind_therapy_percent = describe.factor(ind_therapy_percent)
ind_therapy_percent = round(ind_therapy_percent, 0)
ind_therapy_percent
ind_therapy_percent_yes = paste0(ind_therapy_percent[1],"(",ind_therapy_percent[2], "%", ")") 
ind_therapy_percent_yes
ind_therapy_percent_no = paste0(ind_therapy_percent[3],"(",ind_therapy_percent[4], "%", ")") 
ind_therapy_percent_no


group_therapy =  treat_all[,7:8]
group_therapy_yes = subset(group_therapy, X9_TREAT.d.Received == 1)
dim(group_therapy_yes)
group_therapy_yes_mean = mean(group_therapy_yes$X9_Treat.d.Score, na.rm = TRUE)
group_therapy_yes_sd = sd(group_therapy_yes$X9_Treat.d.Score, na.rm = TRUE)
group_therapy_yes_range = range(group_therapy$X9_Treat.d.Score, na.rm = TRUE)
group_therapy_percent= na.omit(group_therapy$X9_TREAT.d.Received)
group_therapy_percent = describe.factor(group_therapy_percent)
group_therapy_percent = round(group_therapy_percent, 0)
group_therapy_percent
group_therapy_percent_yes = paste0(group_therapy_percent[1],"(",group_therapy_percent[2], "%", ")") 
group_therapy_percent_yes
group_therapy_percent_no = paste0(group_therapy_percent[3],"(",group_therapy_percent[4], "%", ")") 
group_therapy_percent_no


belong =  treat_all[,9:10]
belong_yes = subset(belong, X9_Treat.e.Received == 1)
dim(belong_yes)
belong_yes_mean = mean(belong_yes$X9_Treat.e.Score, na.rm = TRUE)
belong_yes_sd = sd(belong_yes$X9_Treat.e.Score, na.rm = TRUE)
belong_yes_range = range(belong$X9_Treat.e.Score, na.rm = TRUE)
belong_percent= na.omit(belong$X9_Treat.e.Received)
belong_percent = describe.factor(belong_percent)
belong_percent = round(belong_percent, 0)
belong_percent
belong_percent_yes = paste0(belong_percent[1],"(",belong_percent[2], "%", ")") 
belong_percent_yes
belong_percent_no = paste0(belong_percent[3],"(",belong_percent[4], "%", ")") 
belong_percent_no


coping_skills =  treat_all[,11:12]
coping_skills_yes = subset(coping_skills, X9_Treat.f.Received == 1)
dim(coping_skills_yes)
coping_skills_yes_mean = mean(coping_skills_yes$X9_Treat.f.Score, na.rm = TRUE)
coping_skills_yes_sd = sd(coping_skills_yes$X9_Treat.f.Score, na.rm = TRUE)
coping_skills_yes_range = range(coping_skills$X9_Treat.f.Score, na.rm = TRUE)
coping_skills_percent= na.omit(coping_skills$X9_Treat.f.Received)
coping_skills_percent = describe.factor(coping_skills_percent)
coping_skills_percent = round(coping_skills_percent, 0)
coping_skills_percent
coping_skills_percent_yes = paste0(coping_skills_percent[1],"(",coping_skills_percent[2], "%", ")") 
coping_skills_percent_yes
coping_skills_percent_no = paste0(coping_skills_percent[3],"(",coping_skills_percent[4], "%", ")") 
coping_skills_percent_no

meaning =  treat_all[,13:14]
meaning_yes = subset(meaning, X9_Treat.g.Received == 1)
dim(meaning_yes)
meaning_yes_mean = mean(meaning_yes$X9_Treat.g.Score, na.rm = TRUE)
meaning_yes_sd = sd(meaning_yes$X9_Treat.g.Score, na.rm = TRUE)
meaning_yes_range = range(meaning$X9_Treat.g.Score, na.rm = TRUE)
meaning_percent= na.omit(meaning$X9_Treat.g.Received)
meaning_percent = describe.factor(meaning_percent)
meaning_percent = round(meaning_percent, 0)
meaning_percent
meaning_percent_yes = paste0(meaning_percent[1],"(",meaning_percent[2], "%", ")") 
meaning_percent_yes
meaning_percent_no = paste0(meaning_percent[3],"(",meaning_percent[4], "%", ")") 
meaning_percent_no

sense =  treat_all[,15:16]
sense_yes = subset(sense, X9_Treat.h.Received == 1)
dim(sense_yes)
sense_yes_mean = mean(sense_yes$X9_Treat.h.Score, na.rm = TRUE)
sense_yes_sd = sd(sense_yes$X9_Treat.h.Score, na.rm = TRUE)
sense_yes_range = range(sense$X9_Treat.h.Score, na.rm = TRUE)
sense_percent= na.omit(sense$X9_Treat.h.Received)
sense_percent = describe.factor(sense_percent)
sense_percent = round(sense_percent, 0)
sense_percent
sense_percent_yes = paste0(sense_percent[1],"(",sense_percent[2], "%", ")") 
sense_percent_yes
sense_percent_no = paste0(sense_percent[3],"(",sense_percent[4], "%", ")") 
sense_percent_no

burden =  treat_all[,17:18]
burden_yes = subset(burden, X9_Treat.I.Received == 1)
dim(burden_yes)
burden_yes_mean = mean(burden_yes$X9_Treat.I.Score, na.rm = TRUE)
burden_yes_sd = sd(burden_yes$X9_Treat.I.Score, na.rm = TRUE)
burden_yes_range = range(burden$X9_Treat.I.Score, na.rm = TRUE)
burden_percent= na.omit(burden$X9_Treat.I.Received)
burden_percent = describe.factor(burden_percent)
burden_percent = round(burden_percent, 0)
burden_percent
burden_percent_yes = paste0(burden_percent[1],"(",burden_percent[2], "%", ")") 
burden_percent_yes
burden_percent_no = paste0(burden_percent[3],"(",burden_percent[4], "%", ")") 
burden_percent_no

safe =  treat_all[,19:20]
safe_yes = subset(safe, X9_Treat.j.Received == 1)
safe_yes_mean = mean(safe_yes$X9_Treat.j.Score, na.rm = TRUE)
safe_yes_sd = sd(safe_yes$X9_Treat.j.Score, na.rm = TRUE)
safe_yes_range = range(safe$X9_Treat.j.Score, na.rm = TRUE)
safe_percent= na.omit(safe$X9_Treat.j.Received)
safe_percent = describe.factor(safe_percent)
safe_percent = round(safe_percent, 0)
safe_percent
safe_percent_yes = paste0(safe_percent[1],"(",safe_percent[2], "%", ")") 
safe_percent_yes
safe_percent_no = paste0(safe_percent[3],"(",safe_percent[4], "%", ")") 
safe_percent_no

hope =  treat_all[,21:22]
hope_yes = subset(hope, X9_Treat..k.Received == 1)
dim(hope_yes)
hope_yes_mean = mean(hope_yes$X9_Treat.k.Score, na.rm = TRUE)
hope_yes_sd = sd(hope_yes$X9_Treat.k.Score, na.rm = TRUE)
hope_yes_range = range(hope$X9_Treat.k.Score, na.rm = TRUE)
hope_percent= na.omit(hope$X9_Treat..k.Received)
hope_percent = describe.factor(hope_percent)
hope_percent = round(hope_percent, 0)
hope_percent
hope_percent_yes = paste0(hope_percent[1],"(",hope_percent[2], "%", ")") 
hope_percent_yes
hope_percent_no = paste0(hope_percent[3],"(",hope_percent[4], "%", ")") 
hope_percent_no

connect =  treat_all[,23:24]
connect_yes = subset(connect, X9_Treat.L.Received == 1)
dim(connect_yes)
connect_yes_mean = mean(connect_yes$X9_Treat.L.Score, na.rm = TRUE)
connect_yes_sd = sd(connect_yes$X9_Treat.L.Score, na.rm = TRUE)
connect_yes_range = range(connect$X9_Treat.L.Score, na.rm = TRUE)
connect_percent= na.omit(connect$X9_Treat.L.Received)
connect_percent = describe.factor(connect_percent)
connect_percent = round(connect_percent, 0)
connect_percent
connect_percent_yes = paste0(connect_percent[1],"(",connect_percent[2], "%", ")") 
connect_percent_yes
connect_percent_no = paste0(connect_percent[3],"(",connect_percent[4], "%", ")") 
connect_percent_no

needs =  treat_all[,25:26]
needs_yes = subset(needs, X9_Treat.m.Received == 1)
dim(needs_yes)
needs_yes_mean = mean(needs_yes$X9_Treat.m.Score, na.rm = TRUE)
needs_yes_sd = sd(needs_yes$X9_Treat.m.Score, na.rm = TRUE)
needs_yes_range = range(needs$X9_Treat.m.Score, na.rm = TRUE)
needs_percent= na.omit(needs$X9_Treat.m.Received)
needs_percent = describe.factor(needs_percent)
needs_percent = round(needs_percent, 0)
needs_percent
needs_percent_yes = paste0(needs_percent[1],"(",needs_percent[2], "%", ")") 
needs_percent_yes
needs_percent_no = paste0(needs_percent[3],"(",needs_percent[4], "%", ")") 
needs_percent_no

treat_range = data.frame(saftey_plan_yes_range, medication_yes_range, ind_therapy_yes_range, group_therapy_yes_range, belong_yes_range, coping_skills_yes_range, meaning_yes_range, sense_yes_range, burden_yes_range, safe_yes_range, hope_yes_range, connect_yes_range, needs_yes_range)


# treat_range not needed all from 1 to 7 scale make note in table

treat_results = data.frame(saftey_plan_yes_mean, medication_yes_mean, ind_therapy_yes_mean, group_therapy_yes_mean, belong_yes_mean, coping_skills_yes_mean, meaning_yes_mean,sense_yes_mean, burden_yes_mean, safe_yes_mean, hope_yes_mean, connect_yes_mean, needs_yes_mean, saftey_plan_yes_sd, medication_yes_sd, ind_therapy_yes_sd , group_therapy_yes_sd, belong_yes_sd, coping_skills_yes_sd, meaning_yes_sd, sense_yes_sd, burden_yes_sd, safe_yes_sd, hope_yes_sd, connect_yes_sd, needs_yes_sd, saftey_plan_percent_yes, medication_percent_yes, ind_therapy_percent_yes, group_therapy_percent_yes, belong_percent_yes, coping_skills_percent_yes, meaning_percent_yes, sense_percent_yes, burden_percent_yes, safe_percent_yes, hope_percent_yes, connect_percent_yes, needs_percent_yes, saftey_plan_percent_no, medication_percent_no, ind_therapy_percent_no, group_therapy_percent_no, belong_percent_no, coping_skills_percent_no, meaning_percent_no, sense_percent_no, burden_percent_no, safe_percent_no, hope_percent_no, connect_percent_no, needs_percent_no)
treat_results[,1:26] = round(treat_results[,1:26], 2)
treat_results = t(treat_results)
treat_results
treat_results_mean = treat_results[1:13,]
treat_results_sd = treat_results[14:26,]
treat_results_p_yes = treat_results[27:39,]
treat_results_p_no = treat_results[40:52,]

treat_results = data.frame(treat_results_mean, treat_results_sd, treat_results_p_yes, treat_results_p_no)
treat_results
write.csv(treat_results, "treat_results.csv")
```
Effect on suicidal ideation for ind therapy, group therapy, safety plan, burden, meaning, difficultly with life events
Not imputting, because planned missing data (if you did not select anything you selected no)
Difference in standardized average differnece (post-pre) score for suicide for receicing treatment versus not receiving treatment

```{r}
library(descr)
head(center_dat)
center_dat$X9_TREAT.b.Received[center_dat$X9_TREAT.b.Received == 2] = NA
center_dat$SIS_1_diff_extra = center_dat$SIS_1_post-center_dat$SIS_1_pre
center_dat$SIS_1_diff_extra = as.numeric(center_dat$SIS_1_diff_extra)
hist(center_dat$SIS_1_diff_extra)

#g =meaning, l = burden, h = difficult
treatments = data.frame(saftey_plan =center_dat$X9_TREAT.a.Received,  ind_therapy = center_dat$X9_TREAT.c.Received, group_therapy = center_dat$X9_TREAT.d.Received, meaning= center_dat$X9_Treat.g.Received, burden = center_dat$X9_Treat.L.Received, difficult = center_dat$X9_Treat.h.Received)
write.csv(treatments, "treatments.csv", row.names = FALSE)
treatments = read.csv("treatments.csv", header = TRUE)
treatments

## Mean for yes and no of SIS 
mean_sd_list = list()
mean_sd_n_1 = list()
mean_sd_n_0 = list()
mean_sd_list_test = list()

for(i in 1:length(treatments)){
  mean_sd_list[[i]] =  compmeans(center_dat$SIS_1_diff_extra, treatments[[i]])
  mean_sd_list_test[[i]] = mean_sd_list
  mean_sd_list[[i]] = mean_sd_list[[i]][c(1:2,4:5,7:8)]
  #Reorder to mean 1, sd 1, n 1
  mean_sd_list[[i]] = round(mean_sd_list[[i]],2)
  mean_sd_n_1[[i]] = mean_sd_list[[i]][c(2,6,4)]
  mean_sd_n_0[[i]] = mean_sd_list[[i]][c(1,5,3)]
}
mean_sd_list
mean_sd_list_test
mean_sd_n_1[[1]]
mean_sd_n_1 = unlist(mean_sd_n_1)
mean_sd_n_1 = matrix(mean_sd_n_1, ncol= 3, byrow = TRUE)
write.csv(mean_sd_n_1, "mean_sd_n_1.csv", row.names = FALSE)

mean_sd_n_0 = unlist(mean_sd_n_0)
mean_sd_n_0 = matrix(mean_sd_n_0, ncol= 3, byrow = TRUE)
write.csv(mean_sd_n_0, "mean_sd_n_0.csv", row.names = FALSE)


results_list = list()
library(descr)
compmeans(center_dat$SIS_1_diff_extra, center_dat$X9_TREAT.a.Received)
for(i in 1:length(treatments)){
  results_list[[i]] = cohen.d(center_dat$SIS_1_diff_extra, treatments[[i]])
  results_list[[i]] = results_list[[i]]$cohen.d[2]
  
}
results_list[[1]][2]
results_list = unlist(results_list)
results_list = matrix(results_list, ncol = 1, byrow = TRUE)
results_list = data.frame(results_list)
results_list = round(results_list, 3)
results_list
colnames(results_list) = c("cohen_d")
results_list = round(results_list,2)

outcomes = c("Safety Plan", "Individual Therapy", "Group Therapy", "Meaning", "Burden", "Difficult")

results_list = data.frame(outcomes, results_list)

#results_list = results_list[order(abs(results_list$cohen_d), decreasing = TRUE),]

#center_dat$SIS_1_diff_extra = NULL
results_list
write.csv(results_list, "results_list.csv", row.names = FALSE)

### Now t-tests for table five
results_list_t = list()
library(descr)
for(i in 1:length(treatments)){
  results_list_t[[i]] = t.test(center_dat$SIS_1_diff_extra~ treatments[[i]], na.rm = TRUE)
  results_list_t[[i]] = results_list_t[[i]][c(1,3,4)]
}
results_list_t
results_list_t = unlist(results_list_t)
results_list_t = matrix(results_list_t, ncol = 4, byrow = TRUE)
results_list_t = data.frame(results_list_t)
results_list_t = round(results_list_t, 4)
results_list_t
colnames(results_list_t) = c("t_value", "p_value", "lower", "upper")
results_list_t

outcomes = c("Safety Plan", "Individual Therapy", "Group Therapy", "Meaning", "Burden", "Difficult")

results_list_t = data.frame(outcomes, results_list_t)
results_list_t[,2:5] = round(results_list_t[,2:5],2)
results_list_t

results_list_t$outcomes = ifelse(results_list_t$upper > 0 & results_list_t$lower < 0, results_list_t$outcomes, paste0(results_list_t$outcomes, "*"))

results_list_t$ci_95 = paste0(results_list_t$lower, sep = ",", results_list_t$upper)
results_list_t[,4:5] = NULL
results_list_t$p_value = ifelse(results_list_t$p_value <= 0, "<.001", results_list_t$p_value)

#results_list_t$cohen_d = as.numeric(results_list_t$cohen_d)
#results_list_t = results_list_t[order(abs(results_list_t$cohen_d), decreasing = TRUE),]


results_list_t
write.csv(results_list_t, "results_list_t.csv", row.names = FALSE)

describe.factor(treatments$ind_therapy)
compmeans(center_dat$SIS_1_diff_extra, treatments$ind_therapy)
test_diff = data.frame(SIS_1_diff_extra = center_dat$SIS_1_diff_extra, group_therapy = treatments$group_therapy)

test_diff_in = data.frame(SIS_1_diff_extra = center_dat$SIS_1_diff_extra, ind_therapy = treatments$ind_therapy)
### N's are different 
test_diff_in
test_diff
center_dat$SIS_1_diff_extra = NULL
```
Try statistical correction

```{r}
library(konfound)
SIS_1_diff_extra = scale(center_dat$SIS_1_post-center_dat$SIS_1_pre)
center_dat$SIS_1_diff_extra = as.numeric(SIS_1_diff_extra)
treatment_dat = data.frame(saftey_plan =center_dat$X9_TREAT.a.Received,  ind_therapy = center_dat$X9_TREAT.c.Received, group_therapy = center_dat$X9_TREAT.d.Received, meaning= center_dat$X9_Treat.g.Received, burden = center_dat$X9_Treat.L.Received, difficult = center_dat$X9_Treat.h.Received, SIS_1_diff_extra)

test_model = lm(SIS_1_diff_extra ~ saftey_plan + ind_therapy + group_therapy + meaning + burden + difficult,  data = treatment_dat)
library(car)

vif(test_model)
hist(test_model$residuals)
summary(test_model)
konfound(test_model, meaning1)

center_dat$SIS_1_diff_extra = NULL
```
Try predictors of suicide with complete data set
```{r}

head(center_dat)

center_dat_pre = center_dat[,8:18]
center_dat_post = center_dat[,19:29]
diff_scores = center_dat_pre - center_dat_post
diff_scores = data.frame(diff_scores, suicide = center_dat$suicide, BID = center_dat$BID, CSE_1 = center_dat$CSE_1, CSE_2 = center_dat$CSE_2, CSE_3 = center_dat$CSE_3)

reg_suicide = glm(suicide ~ INQ_1_pre + INQ_2_pre + ISLES_1_pre + ISLES_2_pre + MILQ_pre + SIS_2_pre + SIS_1_pre + BID+ CSE_1 + CSE_2 + CSE_3 , data = diff_scores, family = binomial())

summary(reg_suicide)
```


Impute data
```{r}
#head(center_dat)
library(Amelia)

### Change back for analysis 
#center_dat$high_school_greater = ifelse(as.numeric(center_dat$high_school_greater) == 0,0,1)
### Get rid black and another race and reverse white to non-white
##center_dat$another_race = NULL
#center_dat$black = NULL

### Get rid of the treat variables, because they crash R.
### Run descriptives if you want to impute again
#treat_vars  =  center_dat[,c(36:61)]
#center_dat =  data.frame(center_dat[,-c(36:61),])
head(center_dat)
### Create bounds for one var and see what happens
## INQ Post
#dim(center_dat[,8:35])

#range(center_dat$INQ_1_pre, na.rm = TRUE) 
#range(center_dat$RAS_1_pre, na.rm = TRUE) 
#range(center_dat$ISLES_1_post, na.rm = TRUE)
#range(center_dat$MILQ_post, na.rm = TRUE) 
#range(center_dat$RCS_post, na.rm = TRUE) 
#center_dat
bounds = matrix(c(8,1,7, 9,1,7, 10,1,5, 11,1,5, 12,1,5, 13,1,5, 14,1,5, 15,1,7, 16,1,5, 17,1,5, 18,1,5,   19,1,7, 20,1,7, 21,1,5, 22,1,5, 23,1,5, 24,1,5, 25,1,5, 26,1,7, 27,1,5, 28,1,5, 29,1,5, 30,1,4, 31,1,5, 32,1,5, 33,1,10, 34,1,10, 35,1,10),nrow = 28, ncol = 3, byrow = TRUE)
#bounds
dim(center_dat)

#a.out_florida = amelia(x = center_dat_test, m = 5, noms = c("veteran", "sexual_minority", "hispanic", "white", "high_school_greater", "employed", "suicide", "female"), bounds = bounds)
#saveRDS(a.out_florida, file = "a.out_florida.rds")
setwd("S:/Indiana Research & Evaluation/Matthew Hanauer/Centerstone_Study_2019_2020")
a.out_florida = readRDS(file = "a.out_florida.rds")

compare.density(a.out_florida, var = "RAS_1_post")
compare.density(a.out_florida, var = "RAS_3_post")
compare.density(a.out_florida, var = "RAS_3_post")
compare.density(a.out_florida, var = "RAS_5_post")
compare.density(a.out_florida, var = "RCS_post")
compare.density(a.out_florida, var = "ISLES_1_post")
compare.density(a.out_florida, var = "ISLES_2_post")
compare.density(a.out_florida, var = "INQ_1_post")
compare.density(a.out_florida, var = "INQ_2_post")
compare.density(a.out_florida, var = "MILQ_post")
compare.density(a.out_florida, var = "SIS_1_post")
compare.density(a.out_florida, var = "SIS_2_post")


```
Load imputed data
```{r}
library(Amelia)
setwd("S:/Indiana Research & Evaluation/Matthew Hanauer/Centerstone_Study_2019_2020")
a.out_florida = readRDS(file = "a.out_florida.rds")
impute_dat_loop = a.out_florida$imputations
#impute_dat_loop
```
Do diff scores with regression, because not random and want to account
```{r}
dim(impute_dat_loop[[1]])
out_diff_dat = list()
head(impute_dat_loop[[1]][8:18])
head(impute_dat_loop[[1]][19:29])
head(impute_dat_loop[[1]])

for(i in 1:length(impute_dat_loop)){
  out_diff_dat[[i]] =  impute_dat_loop[[i]][19:29] - impute_dat_loop[[i]][8:18]
  colnames(out_diff_dat[[i]]) = c("INQ_1_diff", "INQ_2_diff", "RAS_1_diff", "RAS_3_diff", "RAS_5_diff", "ISLES_1_diff", "ISLES_2_diff", "MILQ_diff", "RCS_diff", "SIS_1_diff", "SIS_2_diff")
  out_diff_dat[[i]] = scale(out_diff_dat[[i]])
  out_diff_dat[[i]] =cbind(impute_dat_loop[[i]], out_diff_dat[[i]])
}
out_diff_dat
### Evaluate normality
out_diff_dat_norm = out_diff_dat[[1]][c(30:32,35,38:48)]
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
Pre convergent and divergent reliability
```{r}
library(Hmisc)
cor_dat_pre_1 = impute_dat_loop[[1]][c(8:18)]
cor_dat_pre_1_results = rcorr(as.matrix(cor_dat_pre_1))
cor_dat_pre_2 = impute_dat_loop[[2]][c(8:18)]
cor_dat_pre_2_results = cor(cor_dat_pre_2)
cor_dat_pre_3 = impute_dat_loop[[3]][c(8:18)]
cor_dat_pre_3_results = cor(cor_dat_pre_3)
cor_dat_pre_4 = impute_dat_loop[[4]][c(8:18)]
cor_dat_pre_4_results = cor(cor_dat_pre_4)
cor_dat_pre_5 = impute_dat_loop[[5]][c(8:18)]
cor_dat_pre_5_results = cor(cor_dat_pre_5)

cor_dat_pre_1

flattenCorrMatrix <- function(cormat, pmat) {
  ut <- upper.tri(cormat)
  data.frame(
    row = rownames(cormat)[row(cormat)[ut]],
    column = rownames(cormat)[col(cormat)[ut]],
    cor  =(cormat)[ut],
    p = pmat[ut]
    )
}
cor_dat_pre_1


cor_dat_pre_1 = rcorr(as.matrix(cor_dat_pre_1))
cor_dat_pre_1 = flattenCorrMatrix(cor_dat_pre_1$r, cor_dat_pre_1$P)
var_names =  cor_dat_pre_1[,1:2]
cor_dat_pre_1 = cor_dat_pre_1$cor
cor_dat_pre_1


cor_dat_pre_2 = rcorr(as.matrix(cor_dat_pre_2))
cor_dat_pre_2 = flattenCorrMatrix(cor_dat_pre_2$r, cor_dat_pre_2$P)
cor_dat_pre_2 = cor_dat_pre_2$cor
cor_dat_pre_2

cor_dat_pre_3 = rcorr(as.matrix(cor_dat_pre_3))
cor_dat_pre_3 = flattenCorrMatrix(cor_dat_pre_3$r, cor_dat_pre_3$P)
cor_dat_pre_3 = cor_dat_pre_3$cor
cor_dat_pre_3

cor_dat_pre_4 = rcorr(as.matrix(cor_dat_pre_4))
cor_dat_pre_4 = flattenCorrMatrix(cor_dat_pre_4$r, cor_dat_pre_4$P)
cor_dat_pre_4 = cor_dat_pre_4$cor
cor_dat_pre_4

cor_dat_pre_5 = rcorr(as.matrix(cor_dat_pre_5))
cor_dat_pre_5 = flattenCorrMatrix(cor_dat_pre_5$r, cor_dat_pre_5$P)
cor_dat_pre_5 = cor_dat_pre_5$cor
cor_dat_pre_5

cor_dat_pre_dat = data.frame(cor_dat_pre_1, cor_dat_pre_2, cor_dat_pre_3, cor_dat_pre_4, cor_dat_pre_5)

cor_dat_pre_dat = apply(cor_dat_pre_dat, 1, mean)
cor_dat_pre_dat = data.frame(var_names, cor_dat_pre_dat)
cor_dat_pre_dat[,3] = round(cor_dat_pre_dat[,3], 2)
cor_dat_pre_dat= cor_dat_pre_dat[order(cor_dat_pre_dat$row),]
cor_dat_pre_dat
#perceived burdensomeness, thwarted belongingness  
#cor_dat_pre_dat$row = ifelse(cor_dat_pre_dat$row == "INQ_1_pre", "perceived burdensomeness", ifelse(cor_dat_pre_dat$row, "INQ_2_pre", "thwarted belongingness", ifelse(cor_dat_pre_dat$row == "ISLES_1_pre", "footing the world", ifelse(cor_dat_pre_dat$row == "ISLES_2_pre"))))
#write.csv(cor_dat_pre_dat, "cor_dat_pre_dat.csv", row.names = FALSE)

```

Correlations
```{r}
cor_dat_pre_1 = impute_dat_loop[[1]][c(8:18, 31)]
cor_dat_pre_1_results = cor(cor_dat_pre_1)
cor_dat_pre_1_results_bid = round(cor_dat_pre_1_results[,12],2)
cor_dat_pre_2 = impute_dat_loop[[2]][c(8:18, 31)]
cor_dat_pre_2_results = cor(cor_dat_pre_2)
cor_dat_pre_2_results_bid = round(cor_dat_pre_2_results[,12],2)
cor_dat_pre_3 = impute_dat_loop[[3]][c(8:18, 31)]
cor_dat_pre_3_results = cor(cor_dat_pre_3)
cor_dat_pre_3_results_bid = round(cor_dat_pre_3_results[,12],2)
cor_dat_pre_4 = impute_dat_loop[[4]][c(8:18, 31)]
cor_dat_pre_4_results = cor(cor_dat_pre_4)
cor_dat_pre_4_results_bid = round(cor_dat_pre_4_results[,12],2)
cor_dat_pre_5 = impute_dat_loop[[5]][c(8:18, 31)]
cor_dat_pre_5_results = cor(cor_dat_pre_5)
cor_dat_pre_5_results_bid = round(cor_dat_pre_5_results[,12],2)

cor_dat_pre_1_results_bid
cor_dat_pre = data.frame(cor_dat_pre_1_results_bid, cor_dat_pre_2_results_bid, cor_dat_pre_3_results_bid, cor_dat_pre_4_results_bid, cor_dat_pre_5_results_bid)
cor_dat_pre = apply(cor_dat_pre, 1, mean)
write.csv(cor_dat_pre, "cor_dat_pre.csv")

cor_dat_post_1 = impute_dat_loop[[1]][c(19:35)]
cor_dat_post_1_results = cor(cor_dat_post_1)
cor_dat_post_1_results_bid = round(cor_dat_post_1_results[,13],2)
cor_dat_post_2 = impute_dat_loop[[2]][c(19:35)]
cor_dat_post_2_results = cor(cor_dat_post_2)
cor_dat_post_2_results_bid = round(cor_dat_post_2_results[,13],2)
cor_dat_post_3 = impute_dat_loop[[3]][c(19:35)]
cor_dat_post_3_results = cor(cor_dat_post_3)
cor_dat_post_3_results_bid = round(cor_dat_post_3_results[,13],2)
cor_dat_post_4 = impute_dat_loop[[4]][c(19:35)]
cor_dat_post_4_results = cor(cor_dat_post_4)
cor_dat_post_4_results_bid = round(cor_dat_post_4_results[,13],2)
cor_dat_post_5 = impute_dat_loop[[5]][c(19:35)]
cor_dat_post_5_results = cor(cor_dat_post_5)
cor_dat_post_5_results_bid = round(cor_dat_post_5_results[,13],2)

cor_dat_post_1_results_bid
cor_dat_post = data.frame(cor_dat_post_1_results_bid, cor_dat_post_2_results_bid, cor_dat_post_3_results_bid, cor_dat_post_4_results_bid, cor_dat_post_5_results_bid)
cor_dat_post = apply(cor_dat_post, 1, mean)
write.csv(cor_dat_post, "cor_dat_post.csv")

```


Get pre and post scores
```{r}
library(Amelia)
mean_out_pre = list()
sd_out_pre = list()
head(impute_dat_loop[[1]][8:18])
head(impute_dat_loop[[1]][19:35])

for(i in 1:length(impute_dat_loop)){
  mean_out_pre[[i]] = apply(impute_dat_loop[[i]][8:18],2,mean)
  sd_out_pre[[i]] = apply(impute_dat_loop[[i]][8:18], 2, sd)
}
dim(impute_dat_loop[[1]][8:18])

parsout_pre = unlist(mean_out_pre) 
parsout_pre = matrix(parsout_pre, ncol = 11, byrow = TRUE)
parsout_pre
write.csv(parsout_pre, "parsout_pre.csv", row.names = FALSE)

sesout_pre = unlist(sd_out_pre)
sesout_pre = matrix(sesout_pre, ncol = 11, byrow = TRUE)
sesout_pre

pars_sesout_pre = mi.meld(parsout_pre, sesout_pre)
pars_sesout_pre
mean_pre = t(pars_sesout_pre$q.mi)
sd_pre = t(pars_sesout_pre$se.mi)
mean_sd_pre = round(data.frame(mean_pre, sd_pre),2)
mean_sd_pre$names = names(impute_dat_loop[[1]][8:18])           
write.csv(mean_sd_pre, "mean_sd_pre.csv", row.names = TRUE)

mean_out_post = list()
sd_out_post = list()
           
for(i in 1:length(impute_dat_loop)){
  mean_out_post[[i]] = apply(impute_dat_loop[[i]][19:35],2,mean)
  sd_out_post[[i]] = apply(impute_dat_loop[[i]][19:35], 2, sd)
}
dim(impute_dat_loop[[1]][19:35])
parsout_post = unlist(mean_out_post) 
parsout_post = matrix(parsout_post, ncol = 17, byrow = TRUE)
parsout_post
write.csv(parsout_post, "parsout_post.csv", row.names = FALSE)
sesout_post = unlist(sd_out_post)
sesout_post = matrix(sesout_post, ncol = 17, byrow = TRUE)
sesout_post

pars_sesout_post = mi.meld(parsout_post, sesout_post)
pars_sesout_post

mean_post = t(pars_sesout_post$q.mi)
sd_post = t(pars_sesout_post$se.mi)
mean_sd_post = round(data.frame(mean_post, sd_post),2)
mean_sd_post$names = names(impute_dat_loop[[1]][19:35])           
write.csv(mean_sd_post, "mean_sd_post.csv", row.names = TRUE)
mean_sd_post
### Get the differnece scores for t-test and cohen's d
## Need difference mean, differene sd
out_dif_t = list()
mean_out_diff = list()
sd_out_diff = list()

for(i in 1:length(impute_dat_loop)){
  out_dif_t[[i]] = impute_dat_loop[[1]][19:29] - impute_dat_loop[[i]][8:18]
  mean_out_diff[[i]] = apply(out_dif_t[[i]],2,mean)
  sd_out_diff[[i]] = apply(out_dif_t[[i]], 2, sd)
}
parsout_diff = unlist(mean_out_diff) 
parsout_diff = matrix(parsout_diff, ncol = 11, byrow = TRUE)
parsout_diff
write.csv(parsout_diff, "parsout_diff.csv", row.names = FALSE)

sesout_diff = unlist(sd_out_diff)
sesout_diff = matrix(sesout_diff, ncol = 11, byrow = TRUE)
sesout_diff

pars_sesout_diff = mi.meld(parsout_diff, sesout_diff)
pars_sesout_diff
mean_diff = t(pars_sesout_diff$q.mi)
sd_diff = t(pars_sesout_diff$se.mi)
mean_sd_diff = round(data.frame(mean_diff, sd_diff),2)
mean_sd_diff$names = names(out_diff_dat[[i]][38:48]) 
mean_sd_diff
write.csv(mean_sd_diff, "mean_sd_diff.csv", row.names = TRUE)

```


Overall effect board member table for T-test and Cohen's D
T-test code
One sample cohen's D is just diff_score / diff_sd
http://sphweb.bumc.bu.edu/otlt/MPH-Modules/BS/SAS/SAS4-OneSampleTtest/SAS4-OneSampleTtest7.html
https://ncss-wpengine.netdna-ssl.com/wp-content/themes/ncss/pdf/Procedures/PASS/One-Sample_T-Tests_using_Effect_Size.pdf
standard error: https://www.bmj.com/about-bmj/resources-readers/publications/statistics-square-one/7-t-tests
P-value: https://www.cyclismo.org/tutorial/R/pValues.html
```{r}
mean_sd_diff = round(mean_sd_diff[,1:2],2)
mean_sd_diff 
## sd_diff / sqrt(n) 
se_diff = mean_sd_diff$sd_diff / sqrt(dim(center_dat)[1])
se_diff
t_stat = round(mean_sd_diff$mean_diff / se_diff,2)
t_stat
p_values_t = round(2*pt(-abs(t_stat), df = dim(out_diff_dat[[1]])[1]-1),3)
p_values_t = ifelse(p_values_t <= 0, "<.001", p_values_t)
p_values_t
critical_t = abs(qt(0.05/2, dim(out_diff_dat[[1]])[1]-1))

upper_reg_t = mean_sd_diff$mean_diff +(critical_t*se_diff)
lower_reg_t =  mean_sd_diff$mean_diff -(critical_t*se_diff)

ci_95_t = paste0(round(lower_reg_t,2), sep = ",", round(upper_reg_t,2))
cohen_d_t = round(mean_sd_diff$mean_diff / mean_sd_diff$sd_diff,2)
outcomes = c("Perceived Burdensomeness", "Thwarted Belongingness", "Personal confidence and hope", "Goal and Success Orientation", "No domination by symptoms", "Comprehensibility", "Footing in the world", "MILQ", "RCS", "Suicidal Ideation", "Resolved plans and preparations")
t_test_results = data.frame(outcomes, t_stat, p_values_t, ci_95_t, cohen_d_t)
write.csv(t_test_results, "t_test_results.csv", row.names = FALSE)
t_test_results
```
Suicidal Ideation regression with difference score
```{r}
library(car)
library(Amelia)
### Regression analysis
regout_sis_1 = list()
regout_sis_1_sum = list()
parsout_sis_1 = list()
sesout_sis_1 = list()
import_sis_1 = list()
vif_sis_1 = list()
library(relaimpo)
head(out_diff_dat[[1]][,35:45])
head(out_diff_dat[[1]])

for(i in 1:length(out_diff_dat)){
  regout_sis_1[[i]] = lm(SIS_1_diff ~ INQ_1_diff + INQ_2_diff + RAS_1_diff + RAS_3_diff+ RAS_5_diff + ISLES_1_diff + ISLES_2_diff + MILQ_diff + RCS_diff, data = out_diff_dat[[i]])
  regout_sis_1_sum[[i]] = summary(regout_sis_1[[i]])
  import_sis_1[[i]] = calc.relimp(regout_sis_1[[i]])
  import_sis_1[[i]] = import_sis_1[[i]]@lmg
  parsout_sis_1[[i]] = regout_sis_1_sum[[i]]$coefficients[,1]
  sesout_sis_1[[i]] = regout_sis_1_sum[[i]]$coefficients[,2]
  vif_sis_1[[i]] = vif(regout_sis_1[[i]])
}
vif_sis_1
#10 cols
parsout_sis_1 = unlist(parsout_sis_1) 
parsout_sis_1 = matrix(parsout_sis_1, ncol = 10, byrow = TRUE)
parsout_sis_1

sesout_sis_1 = unlist(sesout_sis_1)
sesout_sis_1 = matrix(sesout_sis_1, ncol = 10, byrow = TRUE)
sesout_sis_1

pars_sesout_sis_1 = mi.meld(parsout_sis_1, sesout_sis_1)
t_stat_reg_sis_1 =  pars_sesout_sis_1$q.mi / pars_sesout_sis_1$se.mi
p_values_reg_sis_1 = 2*pt(-abs(t_stat_reg_sis_1), df = dim(out_diff_dat[[1]])[1]-10)
p_values_reg_sis_1 = format(round(p_values_reg_sis_1, digits=3), nsmall = 2)
p_values_reg_sis_1
p_values_reg_sis_1
critical_t_reg_sis_1 = abs(qt(0.05/2, dim(out_diff_dat[[1]])[1]-10))
critical_t_reg_sis_1
upper_reg_sis_1 = pars_sesout_sis_1$q.mi +(critical_t_reg_sis_1*pars_sesout_sis_1$se.mi)
upper_reg_sis_1 = format(round(upper_reg_sis_1, digits=2), nsmall = 2)
upper_reg_sis_1
lower_reg_sis_1 = pars_sesout_sis_1$q.mi - (critical_t_reg_sis_1*pars_sesout_sis_1$se.mi)
lower_reg_sis_1 = format(round(lower_reg_sis_1, digits=2), nsmall = 2)
ci_95_sis_1 = paste0(upper_reg_sis_1, sep = ",", lower_reg_sis_1)
ci_95_sis_1
import_sis_1 = unlist(import_sis_1)
import_sis_1 = matrix(import_sis_1, ncol = 9, byrow = TRUE)
import_sis_1 = colMeans(import_sis_1)
import_sis_1 

reg_results_sis_1 = data.frame(par_est = t(pars_sesout_sis_1$q.mi), se = t(pars_sesout_sis_1$se.mi), p_value = t(p_values_reg_sis_1), ci_95_sis_1)
reg_results_sis_1
reg_results_sis_1[,1:2] = format(round(reg_results_sis_1[,1:2], digits=2), nsmall = 2)
reg_results_sis_1$var_names = c("Intercept", colnames(out_diff_dat[[1]])[38:46])
reg_results_sis_1 = data.frame(var_names = reg_results_sis_1$var_names, reg_results_sis_1[,1:4])
typeof(reg_results_sis_1$p_value)

write.csv(reg_results_sis_1, "reg_results_sis_1.csv", row.names = FALSE)
reg_results_sis_1 = read.csv("reg_results_sis_1.csv", header = TRUE)
reg_results_sis_1$var_names = as.character(reg_results_sis_1$var_names)

reg_results_sis_1$var_names = ifelse(reg_results_sis_1$p_value < .05, paste0(reg_results_sis_1$var_names, sep = "*"), reg_results_sis_1$var_names)
reg_results_sis_1$p_value = ifelse(reg_results_sis_1$p_value ==.000, "<.001", reg_results_sis_1$p_value)
reg_results_sis_1 = reg_results_sis_1[-c(1),]
reg_results_sis_1$import_sis_1 = import_sis_1
reg_results_sis_1$import_sis_1 = format(round(reg_results_sis_1$import_sis_1, digits=2), nsmall = 2)
reg_results_sis_1

write.csv(reg_results_sis_1, "reg_results_sis_1.csv", row.names  = FALSE)
```
Suicidal ideation with pre scores
```{r}
library(car)
library(Amelia)
### Regression analysis
regout_sis_1 = list()
regout_sis_1_sum = list()
parsout_sis_1 = list()
sesout_sis_1 = list()
import_sis_1 = list()
vif_sis_1 = list()
library(relaimpo)

for(i in 1:length(out_diff_dat)){
  regout_sis_1[[i]] = lm(SIS_1_pre ~ INQ_1_pre + INQ_2_pre + RAS_1_pre + RAS_3_pre+ RAS_5_pre + ISLES_1_pre + ISLES_2_pre + MILQ_pre + RCS_pre, data = out_diff_dat[[i]])
  regout_sis_1_sum[[i]] = summary(regout_sis_1[[i]])
  import_sis_1[[i]] = calc.relimp(regout_sis_1[[i]])
  import_sis_1[[i]] = import_sis_1[[i]]@lmg
  parsout_sis_1[[i]] = regout_sis_1_sum[[i]]$coefficients[,1]
  sesout_sis_1[[i]] = regout_sis_1_sum[[i]]$coefficients[,2]
  vif_sis_1[[i]] = vif(regout_sis_1[[i]])
}
vif_sis_1
#10 cols
parsout_sis_1 = unlist(parsout_sis_1) 
parsout_sis_1 = matrix(parsout_sis_1, ncol = 10, byrow = TRUE)
parsout_sis_1

sesout_sis_1 = unlist(sesout_sis_1)
sesout_sis_1 = matrix(sesout_sis_1, ncol = 10, byrow = TRUE)
sesout_sis_1

pars_sesout_sis_1 = mi.meld(parsout_sis_1, sesout_sis_1)
t_stat_reg_sis_1 =  pars_sesout_sis_1$q.mi / pars_sesout_sis_1$se.mi
p_values_reg_sis_1 = 2*pt(-abs(t_stat_reg_sis_1), df = dim(out_diff_dat[[1]])[1]-10)
p_values_reg_sis_1 = format(round(p_values_reg_sis_1, digits=3), nsmall = 2)
p_values_reg_sis_1
p_values_reg_sis_1
critical_t_reg_sis_1 = abs(qt(0.05/2, dim(out_diff_dat[[1]])[1]-10))
critical_t_reg_sis_1
upper_reg_sis_1 = pars_sesout_sis_1$q.mi +(critical_t_reg_sis_1*pars_sesout_sis_1$se.mi)
upper_reg_sis_1 = format(round(upper_reg_sis_1, digits=2), nsmall = 2)
upper_reg_sis_1
lower_reg_sis_1 = pars_sesout_sis_1$q.mi - (critical_t_reg_sis_1*pars_sesout_sis_1$se.mi)
lower_reg_sis_1 = format(round(lower_reg_sis_1, digits=2), nsmall = 2)
ci_95_sis_1 = paste0(upper_reg_sis_1, sep = ",", lower_reg_sis_1)
ci_95_sis_1
import_sis_1 = unlist(import_sis_1)
import_sis_1 = matrix(import_sis_1, ncol = 9, byrow = TRUE)
import_sis_1 = colMeans(import_sis_1)
import_sis_1 

reg_results_sis_1 = data.frame(par_est = t(pars_sesout_sis_1$q.mi), se = t(pars_sesout_sis_1$se.mi), p_value = t(p_values_reg_sis_1), ci_95_sis_1)
reg_results_sis_1
reg_results_sis_1[,1:2] = format(round(reg_results_sis_1[,1:2], digits=2), nsmall = 2)
reg_results_sis_1$var_names = c("Intercept", colnames(out_diff_dat[[1]])[38:46])
reg_results_sis_1 = data.frame(var_names = reg_results_sis_1$var_names, reg_results_sis_1[,1:4])
typeof(reg_results_sis_1$p_value)

write.csv(reg_results_sis_1, "reg_results_sis_1.csv", row.names = FALSE)
reg_results_sis_1 = read.csv("reg_results_sis_1.csv", header = TRUE)
reg_results_sis_1$var_names = as.character(reg_results_sis_1$var_names)

reg_results_sis_1$var_names = ifelse(reg_results_sis_1$p_value < .05, paste0(reg_results_sis_1$var_names, sep = "*"), reg_results_sis_1$var_names)
reg_results_sis_1$p_value = ifelse(reg_results_sis_1$p_value ==.000, "<.001", reg_results_sis_1$p_value)
reg_results_sis_1 = reg_results_sis_1[-c(1),]
reg_results_sis_1$import_sis_1 = import_sis_1
reg_results_sis_1$import_sis_1 = format(round(reg_results_sis_1$import_sis_1, digits=2), nsmall = 2)
reg_results_sis_1

write.csv(reg_results_sis_1, "reg_results_sis_1.csv", row.names  = FALSE)

```
Suicidal ideation with post scores
```{r}
library(car)
library(Amelia)
### Regression analysis
regout_sis_1 = list()
regout_sis_1_sum = list()
parsout_sis_1 = list()
sesout_sis_1 = list()
import_sis_1 = list()
vif_sis_1 = list()
library(relaimpo)

for(i in 1:length(out_diff_dat)){
  regout_sis_1[[i]] = lm(SIS_1_pre ~ INQ_1_post + INQ_2_post + RAS_1_post + RAS_3_post+ RAS_5_post + ISLES_1_post + ISLES_2_post + MILQ_post + RCS_post, data = out_diff_dat[[i]])
  regout_sis_1_sum[[i]] = summary(regout_sis_1[[i]])
  import_sis_1[[i]] = calc.relimp(regout_sis_1[[i]])
  import_sis_1[[i]] = import_sis_1[[i]]@lmg
  parsout_sis_1[[i]] = regout_sis_1_sum[[i]]$coefficients[,1]
  sesout_sis_1[[i]] = regout_sis_1_sum[[i]]$coefficients[,2]
  vif_sis_1[[i]] = vif(regout_sis_1[[i]])
}
vif_sis_1
#10 cols
parsout_sis_1 = unlist(parsout_sis_1) 
parsout_sis_1 = matrix(parsout_sis_1, ncol = 10, byrow = TRUE)
parsout_sis_1

sesout_sis_1 = unlist(sesout_sis_1)
sesout_sis_1 = matrix(sesout_sis_1, ncol = 10, byrow = TRUE)
sesout_sis_1

pars_sesout_sis_1 = mi.meld(parsout_sis_1, sesout_sis_1)
t_stat_reg_sis_1 =  pars_sesout_sis_1$q.mi / pars_sesout_sis_1$se.mi
p_values_reg_sis_1 = 2*pt(-abs(t_stat_reg_sis_1), df = dim(out_diff_dat[[1]])[1]-10)
p_values_reg_sis_1 = format(round(p_values_reg_sis_1, digits=3), nsmall = 2)
p_values_reg_sis_1
p_values_reg_sis_1
critical_t_reg_sis_1 = abs(qt(0.05/2, dim(out_diff_dat[[1]])[1]-10))
critical_t_reg_sis_1
upper_reg_sis_1 = pars_sesout_sis_1$q.mi +(critical_t_reg_sis_1*pars_sesout_sis_1$se.mi)
upper_reg_sis_1 = format(round(upper_reg_sis_1, digits=2), nsmall = 2)
upper_reg_sis_1
lower_reg_sis_1 = pars_sesout_sis_1$q.mi - (critical_t_reg_sis_1*pars_sesout_sis_1$se.mi)
lower_reg_sis_1 = format(round(lower_reg_sis_1, digits=2), nsmall = 2)
ci_95_sis_1 = paste0(upper_reg_sis_1, sep = ",", lower_reg_sis_1)
ci_95_sis_1
import_sis_1 = unlist(import_sis_1)
import_sis_1 = matrix(import_sis_1, ncol = 9, byrow = TRUE)
import_sis_1 = colMeans(import_sis_1)
import_sis_1 

reg_results_sis_1 = data.frame(par_est = t(pars_sesout_sis_1$q.mi), se = t(pars_sesout_sis_1$se.mi), p_value = t(p_values_reg_sis_1), ci_95_sis_1)
reg_results_sis_1
reg_results_sis_1[,1:2] = format(round(reg_results_sis_1[,1:2], digits=2), nsmall = 2)
reg_results_sis_1$var_names = c("Intercept", colnames(out_diff_dat[[1]])[38:46])
reg_results_sis_1 = data.frame(var_names = reg_results_sis_1$var_names, reg_results_sis_1[,1:4])
typeof(reg_results_sis_1$p_value)

write.csv(reg_results_sis_1, "reg_results_sis_1.csv", row.names = FALSE)
reg_results_sis_1 = read.csv("reg_results_sis_1.csv", header = TRUE)
reg_results_sis_1$var_names = as.character(reg_results_sis_1$var_names)

reg_results_sis_1$var_names = ifelse(reg_results_sis_1$p_value < .05, paste0(reg_results_sis_1$var_names, sep = "*"), reg_results_sis_1$var_names)
reg_results_sis_1$p_value = ifelse(reg_results_sis_1$p_value ==.000, "<.001", reg_results_sis_1$p_value)
reg_results_sis_1 = reg_results_sis_1[-c(1),]
reg_results_sis_1$import_sis_1 = import_sis_1
reg_results_sis_1$import_sis_1 = format(round(reg_results_sis_1$import_sis_1, digits=2), nsmall = 2)
reg_results_sis_1

write.csv(reg_results_sis_1, "reg_results_sis_1.csv", row.names  = FALSE)

```


Resolved plans difference scores
```{r}
### Regression analysis
regout_sis_2 = list()
regout_sis_2_sum = list()
parsout_sis_2 = list()
sesout_sis_2 = list()
import_sis_2 = list()
vif_sis_2 = list()
library(relaimpo)
head(out_diff_dat[[1]][,35:45])


for(i in 1:length(out_diff_dat)){
  regout_sis_2[[i]] = lm(SIS_2_diff ~ INQ_1_diff + INQ_2_diff + RAS_1_diff + RAS_3_diff+ RAS_5_diff + ISLES_1_diff + ISLES_2_diff + MILQ_diff + RCS_diff, data = out_diff_dat[[i]])
  regout_sis_2_sum[[i]] = summary(regout_sis_2[[i]])
  import_sis_2[[i]] = calc.relimp(regout_sis_2[[i]])
  import_sis_2[[i]] = import_sis_2[[i]]@lmg
  parsout_sis_2[[i]] = regout_sis_2_sum[[i]]$coefficients[,1]
  sesout_sis_2[[i]] = regout_sis_2_sum[[i]]$coefficients[,2]
  vif_sis_2[[i]] = vif(regout_sis_2[[i]])
}
vif_sis_2
#10 cols
parsout_sis_2 = unlist(parsout_sis_2) 
parsout_sis_2 = matrix(parsout_sis_2, ncol = 10, byrow = TRUE)
parsout_sis_2

sesout_sis_2 = unlist(sesout_sis_2)
sesout_sis_2 = matrix(sesout_sis_2, ncol = 10, byrow = TRUE)
sesout_sis_2

pars_sesout_sis_2 = mi.meld(parsout_sis_2, sesout_sis_2)
t_stat_reg_sis_2 =  pars_sesout_sis_2$q.mi / pars_sesout_sis_2$se.mi
p_values_reg_sis_2 = 2*pt(-abs(t_stat_reg_sis_2), df = dim(out_diff_dat[[1]])[1]-10)
p_values_reg_sis_2 = format(round(p_values_reg_sis_2, digits=3), nsmall = 2)
p_values_reg_sis_2
p_values_reg_sis_2
critical_t_reg_sis_2 = abs(qt(0.05/2, dim(out_diff_dat[[1]])[1]-10))
critical_t_reg_sis_2
upper_reg_sis_2 = pars_sesout_sis_2$q.mi +(critical_t_reg_sis_2*pars_sesout_sis_2$se.mi)
upper_reg_sis_2 = format(round(upper_reg_sis_2, digits=2), nsmall = 2)
upper_reg_sis_2
lower_reg_sis_2 = pars_sesout_sis_2$q.mi - (critical_t_reg_sis_2*pars_sesout_sis_2$se.mi)
lower_reg_sis_2 = format(round(lower_reg_sis_2, digits=2), nsmall = 2)
ci_95_sis_2 = paste0(upper_reg_sis_2, sep = ",", lower_reg_sis_2)
ci_95_sis_2
import_sis_2 = unlist(import_sis_2)
import_sis_2 = matrix(import_sis_2, ncol = 9, byrow = TRUE)
import_sis_2 = colMeans(import_sis_2)
import_sis_2 

reg_results_sis_2 = data.frame(par_est = t(pars_sesout_sis_2$q.mi), se = t(pars_sesout_sis_2$se.mi), p_value = t(p_values_reg_sis_2), ci_95_sis_2)
reg_results_sis_2
reg_results_sis_2[,1:2] = format(round(reg_results_sis_2[,1:2], digits=2), nsmall = 2)
reg_results_sis_2$var_names = c("Intercept", colnames(out_diff_dat[[1]])[38:46])
reg_results_sis_2 = data.frame(var_names = reg_results_sis_2$var_names, reg_results_sis_2[,1:4])
typeof(reg_results_sis_2$p_value)

write.csv(reg_results_sis_2, "reg_results_sis_2.csv", row.names = FALSE)
reg_results_sis_2 = read.csv("reg_results_sis_2.csv", header = TRUE)
reg_results_sis_2$var_names = as.character(reg_results_sis_2$var_names)

reg_results_sis_2$var_names = ifelse(reg_results_sis_2$p_value < .05, paste0(reg_results_sis_2$var_names, sep = "*"), reg_results_sis_2$var_names)
reg_results_sis_2$p_value = ifelse(reg_results_sis_2$p_value ==.000, "<.001", reg_results_sis_2$p_value)
reg_results_sis_2 = reg_results_sis_2[-c(1),]
reg_results_sis_2$import_sis_2 = import_sis_2
reg_results_sis_2$import_sis_2 = format(round(reg_results_sis_2$import_sis_2, digits=2), nsmall = 2)
reg_results_sis_2

write.csv(reg_results_sis_2, "reg_results_sis_2.csv", row.names = FALSE)
```
Resolved plans pre scores
```{r}
### Regression analysis
regout_sis_2 = list()
regout_sis_2_sum = list()
parsout_sis_2 = list()
sesout_sis_2 = list()
import_sis_2 = list()
vif_sis_2 = list()
library(relaimpo)
head(out_diff_dat[[1]][,35:45])


for(i in 1:length(out_diff_dat)){
  regout_sis_2[[i]] = lm(SIS_2_diff ~ INQ_1_pre + INQ_2_pre + RAS_1_pre + RAS_3_pre+ RAS_5_pre + ISLES_1_pre + ISLES_2_pre + MILQ_pre + RCS_pre, data = out_diff_dat[[i]])
  regout_sis_2_sum[[i]] = summary(regout_sis_2[[i]])
  import_sis_2[[i]] = calc.relimp(regout_sis_2[[i]])
  import_sis_2[[i]] = import_sis_2[[i]]@lmg
  parsout_sis_2[[i]] = regout_sis_2_sum[[i]]$coefficients[,1]
  sesout_sis_2[[i]] = regout_sis_2_sum[[i]]$coefficients[,2]
  vif_sis_2[[i]] = vif(regout_sis_2[[i]])
}
vif_sis_2
#10 cols
parsout_sis_2 = unlist(parsout_sis_2) 
parsout_sis_2 = matrix(parsout_sis_2, ncol = 10, byrow = TRUE)
parsout_sis_2

sesout_sis_2 = unlist(sesout_sis_2)
sesout_sis_2 = matrix(sesout_sis_2, ncol = 10, byrow = TRUE)
sesout_sis_2

pars_sesout_sis_2 = mi.meld(parsout_sis_2, sesout_sis_2)
t_stat_reg_sis_2 =  pars_sesout_sis_2$q.mi / pars_sesout_sis_2$se.mi
p_values_reg_sis_2 = 2*pt(-abs(t_stat_reg_sis_2), df = dim(out_diff_dat[[1]])[1]-10)
p_values_reg_sis_2 = format(round(p_values_reg_sis_2, digits=3), nsmall = 2)
p_values_reg_sis_2
p_values_reg_sis_2
critical_t_reg_sis_2 = abs(qt(0.05/2, dim(out_diff_dat[[1]])[1]-10))
critical_t_reg_sis_2
upper_reg_sis_2 = pars_sesout_sis_2$q.mi +(critical_t_reg_sis_2*pars_sesout_sis_2$se.mi)
upper_reg_sis_2 = format(round(upper_reg_sis_2, digits=2), nsmall = 2)
upper_reg_sis_2
lower_reg_sis_2 = pars_sesout_sis_2$q.mi - (critical_t_reg_sis_2*pars_sesout_sis_2$se.mi)
lower_reg_sis_2 = format(round(lower_reg_sis_2, digits=2), nsmall = 2)
ci_95_sis_2 = paste0(upper_reg_sis_2, sep = ",", lower_reg_sis_2)
ci_95_sis_2
import_sis_2 = unlist(import_sis_2)
import_sis_2 = matrix(import_sis_2, ncol = 9, byrow = TRUE)
import_sis_2 = colMeans(import_sis_2)
import_sis_2 

reg_results_sis_2 = data.frame(par_est = t(pars_sesout_sis_2$q.mi), se = t(pars_sesout_sis_2$se.mi), p_value = t(p_values_reg_sis_2), ci_95_sis_2)
reg_results_sis_2
reg_results_sis_2[,1:2] = format(round(reg_results_sis_2[,1:2], digits=2), nsmall = 2)
reg_results_sis_2$var_names = c("Intercept", colnames(out_diff_dat[[1]])[38:46])
reg_results_sis_2 = data.frame(var_names = reg_results_sis_2$var_names, reg_results_sis_2[,1:4])
typeof(reg_results_sis_2$p_value)

write.csv(reg_results_sis_2, "reg_results_sis_2.csv", row.names = FALSE)
reg_results_sis_2 = read.csv("reg_results_sis_2.csv", header = TRUE)
reg_results_sis_2$var_names = as.character(reg_results_sis_2$var_names)

reg_results_sis_2$var_names = ifelse(reg_results_sis_2$p_value < .05, paste0(reg_results_sis_2$var_names, sep = "*"), reg_results_sis_2$var_names)
reg_results_sis_2$p_value = ifelse(reg_results_sis_2$p_value ==.000, "<.001", reg_results_sis_2$p_value)
reg_results_sis_2 = reg_results_sis_2[-c(1),]
reg_results_sis_2$import_sis_2 = import_sis_2
reg_results_sis_2$import_sis_2 = format(round(reg_results_sis_2$import_sis_2, digits=2), nsmall = 2)
reg_results_sis_2

write.csv(reg_results_sis_2, "reg_results_sis_2.csv", row.names = FALSE)
```
Resolved plans post scores
```{r}
### Regression analysis
regout_sis_2 = list()
regout_sis_2_sum = list()
parsout_sis_2 = list()
sesout_sis_2 = list()
import_sis_2 = list()
vif_sis_2 = list()
library(relaimpo)
head(out_diff_dat[[1]][,35:45])


for(i in 1:length(out_diff_dat)){
  regout_sis_2[[i]] = lm(SIS_2_diff ~ INQ_1_post + INQ_2_post + RAS_1_post + RAS_3_post+ RAS_5_post + ISLES_1_post + ISLES_2_post + MILQ_post + RCS_post, data = out_diff_dat[[i]])
  regout_sis_2_sum[[i]] = summary(regout_sis_2[[i]])
  import_sis_2[[i]] = calc.relimp(regout_sis_2[[i]])
  import_sis_2[[i]] = import_sis_2[[i]]@lmg
  parsout_sis_2[[i]] = regout_sis_2_sum[[i]]$coefficients[,1]
  sesout_sis_2[[i]] = regout_sis_2_sum[[i]]$coefficients[,2]
  vif_sis_2[[i]] = vif(regout_sis_2[[i]])
}
vif_sis_2
#10 cols
parsout_sis_2 = unlist(parsout_sis_2) 
parsout_sis_2 = matrix(parsout_sis_2, ncol = 10, byrow = TRUE)
parsout_sis_2

sesout_sis_2 = unlist(sesout_sis_2)
sesout_sis_2 = matrix(sesout_sis_2, ncol = 10, byrow = TRUE)
sesout_sis_2

pars_sesout_sis_2 = mi.meld(parsout_sis_2, sesout_sis_2)
t_stat_reg_sis_2 =  pars_sesout_sis_2$q.mi / pars_sesout_sis_2$se.mi
p_values_reg_sis_2 = 2*pt(-abs(t_stat_reg_sis_2), df = dim(out_diff_dat[[1]])[1]-10)
p_values_reg_sis_2 = format(round(p_values_reg_sis_2, digits=3), nsmall = 2)
p_values_reg_sis_2
p_values_reg_sis_2
critical_t_reg_sis_2 = abs(qt(0.05/2, dim(out_diff_dat[[1]])[1]-10))
critical_t_reg_sis_2
upper_reg_sis_2 = pars_sesout_sis_2$q.mi +(critical_t_reg_sis_2*pars_sesout_sis_2$se.mi)
upper_reg_sis_2 = format(round(upper_reg_sis_2, digits=2), nsmall = 2)
upper_reg_sis_2
lower_reg_sis_2 = pars_sesout_sis_2$q.mi - (critical_t_reg_sis_2*pars_sesout_sis_2$se.mi)
lower_reg_sis_2 = format(round(lower_reg_sis_2, digits=2), nsmall = 2)
ci_95_sis_2 = paste0(upper_reg_sis_2, sep = ",", lower_reg_sis_2)
ci_95_sis_2
import_sis_2 = unlist(import_sis_2)
import_sis_2 = matrix(import_sis_2, ncol = 9, byrow = TRUE)
import_sis_2 = colMeans(import_sis_2)
import_sis_2 

reg_results_sis_2 = data.frame(par_est = t(pars_sesout_sis_2$q.mi), se = t(pars_sesout_sis_2$se.mi), p_value = t(p_values_reg_sis_2), ci_95_sis_2)
reg_results_sis_2
reg_results_sis_2[,1:2] = format(round(reg_results_sis_2[,1:2], digits=2), nsmall = 2)
reg_results_sis_2$var_names = c("Intercept", colnames(out_diff_dat[[1]])[38:46])
reg_results_sis_2 = data.frame(var_names = reg_results_sis_2$var_names, reg_results_sis_2[,1:4])
typeof(reg_results_sis_2$p_value)

write.csv(reg_results_sis_2, "reg_results_sis_2.csv", row.names = FALSE)
reg_results_sis_2 = read.csv("reg_results_sis_2.csv", header = TRUE)
reg_results_sis_2$var_names = as.character(reg_results_sis_2$var_names)

reg_results_sis_2$var_names = ifelse(reg_results_sis_2$p_value < .05, paste0(reg_results_sis_2$var_names, sep = "*"), reg_results_sis_2$var_names)
reg_results_sis_2$p_value = ifelse(reg_results_sis_2$p_value ==.000, "<.001", reg_results_sis_2$p_value)
reg_results_sis_2 = reg_results_sis_2[-c(1),]
reg_results_sis_2$import_sis_2 = import_sis_2
reg_results_sis_2$import_sis_2 = format(round(reg_results_sis_2$import_sis_2, digits=2), nsmall = 2)
reg_results_sis_2

write.csv(reg_results_sis_2, "reg_results_sis_2.csv", row.names = FALSE)
```

Hypothesis 4:  Treatment alliance and treatment satisfaction will be positively and uniquely associated with intention to follow-through on discharge plans. 
```{r}
### Regression analysis
regout_hyp_4 = list()
regout_hyp_4_sum = list()
parsout_hyp_4 = list()
sesout_hyp_4 = list()
import_hyp_4 = list()
vif_hyp_4 = list()
hist(log(out_diff_dat[[1]]$BID))

for(i in 1:length(out_diff_dat)){
  regout_hyp_4[[i]] = lm(BID ~ CSQ + WAI, data = out_diff_dat[[i]])
  regout_hyp_4_sum[[i]] = summary(regout_hyp_4[[i]])
  import_hyp_4[[i]] = calc.relimp(regout_hyp_4[[i]])
  import_hyp_4[[i]] = import_hyp_4[[i]]@lmg
  parsout_hyp_4[[i]] = regout_hyp_4_sum[[i]]$coefficients[,1]
  sesout_hyp_4[[i]] = regout_hyp_4_sum[[i]]$coefficients[,2]
  vif_hyp_4[[i]] = vif(regout_hyp_4[[i]])
}
vif_hyp_4
#3 cols
parsout_hyp_4 = unlist(parsout_hyp_4) 
parsout_hyp_4 = matrix(parsout_hyp_4, ncol = 3, byrow = TRUE)
parsout_hyp_4

sesout_hyp_4 = unlist(sesout_hyp_4)
sesout_hyp_4 = matrix(sesout_hyp_4, ncol = 3, byrow= TRUE)
sesout_hyp_4

pars_sesout_hyp_4 = mi.meld(parsout_hyp_4, sesout_hyp_4)
t_stat_reg_hyp_4 =  pars_sesout_hyp_4$q.mi / pars_sesout_hyp_4$se.mi
p_values_reg_hyp_4 = 2*pt(-abs(t_stat_reg_hyp_4), df = dim(out_diff_dat[[1]])[1]-3)
p_values_reg_hyp_4 = format(round(p_values_reg_hyp_4, digits=3), nsmall = 2)
p_values_reg_hyp_4
p_values_reg_hyp_4
critical_t_reg_hyp_4 = abs(qt(0.05/2, dim(out_diff_dat[[1]])[1]-3))
critical_t_reg_hyp_4
upper_reg_hyp_4 = pars_sesout_hyp_4$q.mi +(critical_t_reg_hyp_4*pars_sesout_hyp_4$se.mi)
upper_reg_hyp_4 = format(round(upper_reg_hyp_4, digits=2), nsmall = 2)
upper_reg_hyp_4
lower_reg_hyp_4 = pars_sesout_hyp_4$q.mi - (critical_t_reg_hyp_4*pars_sesout_hyp_4$se.mi)
lower_reg_hyp_4 = format(round(lower_reg_hyp_4, digits=2), nsmall = 2)
ci_95_hyp_4 = paste0(upper_reg_hyp_4, sep = ",", lower_reg_hyp_4)
ci_95_hyp_4
import_hyp_4 = unlist(import_hyp_4)
import_hyp_4 = matrix(import_hyp_4, ncol = 2, byrow = TRUE)
import_hyp_4 = colMeans(import_hyp_4)
import_hyp_4 

reg_results_hyp_4 = data.frame(par_est = t(pars_sesout_hyp_4$q.mi), se = t(pars_sesout_hyp_4$se.mi), p_value = t(p_values_reg_hyp_4), ci_95_hyp_4)
reg_results_hyp_4
reg_results_hyp_4[,1:2] = format(round(reg_results_hyp_4[,1:2], digits=2), nsmall = 2)
reg_results_hyp_4$var_names = c("Intercept", "CSQ", "WAI")
reg_results_hyp_4 = data.frame(var_names = reg_results_hyp_4$var_names, reg_results_hyp_4[,1:4])
reg_results_hyp_4

write.csv(reg_results_hyp_4, "reg_results_hyp_4.csv", row.names = FALSE)
reg_results_hyp_4 = read.csv("reg_results_hyp_4.csv", header = TRUE)
reg_results_hyp_4$var_names = as.character(reg_results_hyp_4$var_names)
reg_results_hyp_4

reg_results_hyp_4$var_names = ifelse(reg_results_hyp_4$p_value < .05, paste0(reg_results_hyp_4$var_names, sep = "*"), reg_results_hyp_4$var_names)
reg_results_hyp_4$p_value = ifelse(reg_results_hyp_4$p_value ==.000, "<.001", reg_results_hyp_4$p_value)
reg_results_hyp_4 = reg_results_hyp_4[-c(1),]
reg_results_hyp_4$par_est = format(round(reg_results_hyp_4$par_est, digits=2), nsmall = 2) 
reg_results_hyp_4
reg_results_hyp_4$import_hyp_4 = import_hyp_4
reg_results_hyp_4$import_hyp_4 = format(round(reg_results_hyp_4$import_hyp_4, digits=2), nsmall = 2)
reg_results_hyp_4

write.csv(reg_results_hyp_4, "reg_results_hyp_4.csv", row.names = FALSE)

```


Hypothesis 1: Perceived burdensomeness, thwarted belongingness, and meaning made of stress will contribute the most variance to suicide risk status scores at discharge. 
Going to check all of them

Get point biserial later: library(polycor)


```{r}
library(car)
### Regression analysis
regout_r3_hyp_1 = list()
regout_r3_hyp_1_sum = list()
parsout_r3_hyp_1 = list()
sesout_r3_hyp_1 = list()
import_r3_hyp_1 = list()
vif_r3_hyp_1 = list()

for(i in 1:length(out_diff_dat)){
  regout_r3_hyp_1[[i]] = glm(suicide ~ INQ_1_diff + INQ_2_diff + ISLES_1_diff + ISLES_2_diff + MILQ_diff + SIS_1_diff+SIS_2_diff  + BID+ CSE_1 + CSE_2 + CSE_3 , data = out_diff_dat[[i]], family = binomial()) 
  regout_r3_hyp_1_sum[[i]] = summary(regout_r3_hyp_1[[i]])
  #import_r3_hyp_1[[i]] = calc.relimp(regout_r3_hyp_1[[i]])
  #import_r3_hyp_1[[i]] = import_r3_hyp_1[[i]]@lmg
  parsout_r3_hyp_1[[i]] = regout_r3_hyp_1_sum[[i]]$coefficients[,1]
  parsout_r3_hyp_1[[i]] = exp(parsout_r3_hyp_1[[i]])
  sesout_r3_hyp_1[[i]] = regout_r3_hyp_1_sum[[i]]$coefficients[,2]
  sesout_r3_hyp_1[[i]] = exp(sesout_r3_hyp_1[[i]])
  vif_r3_hyp_1[[i]] = vif(regout_r3_hyp_1[[i]])
}
vif_r3_hyp_1
#3 cols
parsout_r3_hyp_1 = unlist(parsout_r3_hyp_1) 
parsout_r3_hyp_1 = matrix(parsout_r3_hyp_1, ncol = 12, byrow = TRUE)
parsout_r3_hyp_1

sesout_r3_hyp_1 = unlist(sesout_r3_hyp_1)
sesout_r3_hyp_1 = matrix(sesout_r3_hyp_1, ncol = 12, byrow= TRUE)
sesout_r3_hyp_1

pars_sesout_r3_hyp_1 = mi.meld(parsout_r3_hyp_1, sesout_r3_hyp_1)
t_stat_reg_r3_hyp_1 =  pars_sesout_r3_hyp_1$q.mi / pars_sesout_r3_hyp_1$se.mi
p_values_reg_r3_hyp_1 = 2*pt(-abs(t_stat_reg_r3_hyp_1), df = dim(out_diff_dat[[1]])[1]-12)
p_values_reg_r3_hyp_1 = format(round(p_values_reg_r3_hyp_1, digits=3), nsmall = 2)
p_values_reg_r3_hyp_1
p_values_reg_r3_hyp_1
critical_t_reg_r3_hyp_1 = abs(qt(0.05/2, dim(out_diff_dat[[1]])[1]-12))
critical_t_reg_r3_hyp_1
upper_reg_r3_hyp_1 = pars_sesout_r3_hyp_1$q.mi +(critical_t_reg_r3_hyp_1*pars_sesout_r3_hyp_1$se.mi)
upper_reg_r3_hyp_1 = format(round(upper_reg_r3_hyp_1, digits=2), nsmall = 2)
upper_reg_r3_hyp_1
lower_reg_r3_hyp_1 = pars_sesout_r3_hyp_1$q.mi - (critical_t_reg_r3_hyp_1*pars_sesout_r3_hyp_1$se.mi)
lower_reg_r3_hyp_1 = format(round(lower_reg_r3_hyp_1, digits=2), nsmall = 2)
ci_95_r3_hyp_1 = paste0(upper_reg_r3_hyp_1, sep = ",", lower_reg_r3_hyp_1)
ci_95_r3_hyp_1
#import_r3_hyp_1 = unlist(import_r3_hyp_1)
#import_r3_hyp_1 = matrix(import_r3_hyp_1, ncol = 2, byrow = TRUE)
#import_r3_hyp_1 = colMeans(import_r3_hyp_1)
#import_r3_hyp_1 

reg_results_r3_hyp_1 = data.frame(par_est = t(pars_sesout_r3_hyp_1$q.mi), se = t(pars_sesout_r3_hyp_1$se.mi), p_value = t(p_values_reg_r3_hyp_1), ci_95_r3_hyp_1)
reg_results_r3_hyp_1
reg_results_r3_hyp_1[,1:2] = format(round(reg_results_r3_hyp_1[,1:2], digits=2), nsmall = 2)
reg_results_r3_hyp_1$var_names = c(names(regout_r3_hyp_1[[1]]$coefficients))
reg_results_r3_hyp_1 = data.frame(var_names = reg_results_r3_hyp_1$var_names, reg_results_r3_hyp_1[,1:4])
reg_results_r3_hyp_1

write.csv(reg_results_r3_hyp_1, "reg_results_r3_hyp_1.csv", row.names = FALSE)
reg_results_r3_hyp_1 = read.csv("reg_results_r3_hyp_1.csv", header = TRUE)
reg_results_r3_hyp_1$var_names = as.character(reg_results_r3_hyp_1$var_names)
reg_results_r3_hyp_1

reg_results_r3_hyp_1$var_names = ifelse(reg_results_r3_hyp_1$p_value < .05, paste0(reg_results_r3_hyp_1$var_names, sep = "*"), reg_results_r3_hyp_1$var_names)
reg_results_r3_hyp_1$p_value = ifelse(reg_results_r3_hyp_1$p_value ==.000, "<.001", reg_results_r3_hyp_1$p_value)
reg_results_r3_hyp_1 = reg_results_r3_hyp_1[-c(1),]
reg_results_r3_hyp_1$par_est = format(round(reg_results_r3_hyp_1$par_est, digits=2), nsmall = 2) 
reg_results_r3_hyp_1
#reg_results_r3_hyp_1$import_r3_hyp_1 = import_r3_hyp_1
#reg_results_r3_hyp_1$import_r3_hyp_1 = format(round(reg_results_r3_hyp_1$import_r3_hyp_1, digits=2), nsmall = 2)
reg_results_r3_hyp_1

write.csv(reg_results_r3_hyp_1, "reg_results_r3_hyp_1.csv", row.names = FALSE)
```
Original works fine why not simulation?
```{r}
### Just test out
intercept_eff = 2.81
INQ_1_diff_eff = 1.05
INQ_2_diff_eff = .77
ISLES_1_diff_eff = 1.51
ISLES_2_diff_eff = .91
MILQ_diff_eff = 1.15
SIS_2_diff_eff = 2
SIS_1_diff_eff = .63
BID_eff = 1.29
CSE_1_eff = 1.46
CSE_2_eff = .64

y_out = list()
runis_out = list()
test_rep_out = list()
INQ_1_diff_out = list()
INQ_2_diff_out = list()
ISLES_1_diff_out = list()
ISLES_2_diff_out = list()
SIS_1_diff_out = list()
SIS_2_diff_out = list()
MILQ_diff_out = list()
BID_out = list()
CSE_1_out = list()
CSE_2_out = list()
y_prob_out = list()
dat_sim = list()
test_rep = list()
test_rep_sum = list()
n = seq(from = 120, to = 300, by = 20)

for(i in 1:length(n)){

INQ_1_diff_out[[i]] = rnorm(n[[i]],0,1)
INQ_2_diff_out[[i]] = rnorm(n[[i]],0,1)
ISLES_1_diff_out[[i]] = rnorm(n[[i]],0,1)
ISLES_2_diff_out[[i]] = rnorm(n[[i]],0,1)
MILQ_diff_out[[i]] = rnorm(n[[i]],0,1)
SIS_1_diff_out[[i]] = rnorm(n[[i]],0,1)
SIS_2_diff_out[[i]] = rnorm(n[[i]],0,1)
BID_out[[i]] = rnorm(n[[i]],0,1)
CSE_1_out[[i]] = rnorm(n[[i]],0,1)
CSE_2_out[[i]] = rnorm(n[[i]],0,1)

y_out[[i]] = intercept_eff + INQ_1_diff_eff*INQ_1_diff_out[[i]] +INQ_2_diff_eff*INQ_2_diff_out[[i]]+ISLES_1_diff_eff*ISLES_1_diff_out[[i]]+ ISLES_2_diff_eff*ISLES_2_diff_out[[i]]+MILQ_diff_eff*MILQ_diff_out[[i]]+ SIS_1_diff_eff*SIS_1_diff_out[[i]] +SIS_2_diff_eff*SIS_2_diff_out[[i]]+BID_eff*BID_out[[i]]+CSE_1_eff*CSE_1_out[[i]]+CSE_2_eff*CSE_2_out[[i]]

y_prob_out[[i]] = exp(y_out[[i]])/(1+exp(y_out[[i]]))
runis_out[[i]] = runif(length(INQ_1_diff_out[[i]]), 0, 1) # This is the random error part
y_out[[i]] = ifelse(runis_out[[i]] < y_prob_out[[i]], 1, 0)
dat_sim[[i]] = data.frame(y_out = y_out[[i]], INQ_1_diff_out = INQ_1_diff_out[[i]], INQ_2_diff_out = INQ_2_diff_out[[i]], ISLES_1_diff_out = ISLES_1_diff_out[[i]], ISLES_2_diff_out = ISLES_2_diff_out[[i]], MILQ_diff_out = MILQ_diff_out[[i]], SIS_1_diff_out = SIS_1_diff_out[[i]], SIS_2_diff_out = SIS_2_diff_out[[i]], BID_out = BID_out[[i]], CSE_1_out = CSE_1_out[[i]], CSE_2_out = CSE_2_out[[i]])
test_rep[[i]] = glm(y_out ~ ., family = binomial(), data = dat_sim[[i]])
test_rep_sum[[i]] =  summary(test_rep[[i]])
}
#glm(y_out ~ ., family = "binomial", data =dat_sim[[1]])
test_rep_sum[[1]]

dim(dat_sim[[5]])
```
Try with complete data



Now what predicts BID
```{r}
library(relaimpo)
library(car)
### Regression analysis
regout_BID = list()
regout_BID_sum = list()
parsout_BID = list()
sesout_BID = list()
import_BID = list()
vif_BID = list()
hist(log(out_diff_dat[[1]]$BID))
head(out_diff_dat[[1]])


for(i in 1:length(out_diff_dat)){
  regout_BID[[i]] = lm(BID ~ INQ_1_diff + INQ_2_diff+ RAS_1_diff + RAS_3_diff + RAS_5_diff + ISLES_1_diff + ISLES_2_diff + MILQ_diff + RCS_diff + SIS_1_diff + SIS_2_diff, data = out_diff_dat[[i]])
  regout_BID_sum[[i]] = summary(regout_BID[[i]])
  import_BID[[i]] = calc.relimp(regout_BID[[i]])
  import_BID[[i]] = import_BID[[i]]@lmg
  parsout_BID[[i]] = regout_BID_sum[[i]]$coefficients[,1]
  sesout_BID[[i]] = regout_BID_sum[[i]]$coefficients[,2]
  vif_BID[[i]] = vif(regout_BID[[i]])
}
vif_BID
#3 cols
parsout_BID = unlist(parsout_BID) 
parsout_BID = matrix(parsout_BID, ncol = 12, byrow = TRUE)
parsout_BID

sesout_BID = unlist(sesout_BID)
sesout_BID = matrix(sesout_BID, ncol = 12, byrow= TRUE)
sesout_BID

pars_sesout_BID = mi.meld(parsout_BID, sesout_BID)
t_stat_reg_BID =  pars_sesout_BID$q.mi / pars_sesout_BID$se.mi
p_values_reg_BID = 2*pt(-abs(t_stat_reg_BID), df = dim(out_diff_dat[[1]])[1]-12)
p_values_reg_BID = format(round(p_values_reg_BID, digits=3), nsmall = 2)
p_values_reg_BID
critical_t_reg_BID = abs(qt(0.05/2, dim(out_diff_dat[[1]])[1]-12))
critical_t_reg_BID
upper_reg_BID = pars_sesout_BID$q.mi +(critical_t_reg_BID*pars_sesout_BID$se.mi)
upper_reg_BID = format(round(upper_reg_BID, digits=2), nsmall = 2)
upper_reg_BID
lower_reg_BID = pars_sesout_BID$q.mi - (critical_t_reg_BID*pars_sesout_BID$se.mi)
lower_reg_BID = format(round(lower_reg_BID, digits=2), nsmall = 2)
ci_95_BID = paste0(upper_reg_BID, sep = ",", lower_reg_BID)
ci_95_BID
import_BID = unlist(import_BID)
import_BID = matrix(import_BID, ncol = 11, byrow = TRUE)
import_BID = colMeans(import_BID)
import_BID 

reg_results_BID = data.frame(par_est = t(pars_sesout_BID$q.mi), se = t(pars_sesout_BID$se.mi), p_value = t(p_values_reg_BID), ci_95_BID)
reg_results_BID[,1:2] = format(round(reg_results_BID[,1:2], digits=2), nsmall = 2)

reg_results_BID$var_names = c("Intercept" ,colnames(out_diff_dat[[1]])[38:48])
reg_results_BID = data.frame(var_names = reg_results_BID$var_names, reg_results_BID[,1:4])
reg_results_BID

write.csv(reg_results_BID, "reg_results_BID.csv", row.names = FALSE)
reg_results_BID = read.csv("reg_results_BID.csv", header = TRUE)
reg_results_BID$var_names = as.character(reg_results_BID$var_names)
reg_results_BID

reg_results_BID$var_names = ifelse(reg_results_BID$p_value < .05, paste0(reg_results_BID$var_names, sep = "*"), reg_results_BID$var_names)
reg_results_BID$p_value = ifelse(reg_results_BID$p_value ==.000, "<.001", reg_results_BID$p_value)
reg_results_BID = reg_results_BID[-c(1),]
reg_results_BID$par_est = format(round(reg_results_BID$par_est, digits=2), nsmall = 2) 
reg_results_BID
reg_results_BID$import_BID = import_BID
reg_results_BID$import_BID = format(round(reg_results_BID$import_BID, digits=2), nsmall = 2)
reg_results_BID = reg_results_BID[order(abs(as.numeric(reg_results_BID$par_est)), decreasing = TRUE),]
reg_results_BID
write.csv(reg_results_BID, "reg_results_BID.csv", row.names = FALSE)
```
############################################
Imputted data for 75% or greater missing
############################################
Impute data
Run the data diagnositics section prior for center_dat_missing_75
```{r}
center_dat_missing_75$high_school_greater = ifelse(as.numeric(center_dat_missing_75$high_school_greater) == 0,0,1)
center_dat_missing_75$another_race = NULL
center_dat_missing_75$black = NULL

center_dat_missing_75

dim(center_dat_missing_75)
head(center_dat_missing_75)

#setwd("S:/Indiana Research & Evaluation/Matthew Hanauer/Centerstone_Study_2019_2020")
#impute_dat_loop = readRDS(file = "impute_dat_loop.rds")

bounds = matrix(c(8,1,7, 9,1,7, 10,1,5, 11,1,5, 12,1,5, 13,1,5, 14,1,5, 15,1,7, 16,1,5, 17,1,5, 18,1,5,   19,1,7, 20,1,7, 21,1,5, 22,1,5, 23,1,5, 24,1,5, 25,1,5, 26,1,7, 27,1,5, 28,1,5, 29,1,5),nrow = 22, ncol = 3, byrow = TRUE)
#bounds
dim(center_dat_missing_75)
m = 5
#a.out_florida_missing_75 = amelia(x = center_dat_missing_75, m = m, noms = c("veteran", "sexual_minority", "hispanic", "white", "high_school_greater", "employed", "suicide", "female"), bounds = bounds)
#saveRDS(a.out_florida_missing_75, file = "a.out_florida_missing_75.rds")
setwd("S:/Indiana Research & Evaluation/Matthew Hanauer/Centerstone_Study_2019_2020")
a.out_florida_missing_75 = readRDS(file = "a.out_florida_missing_75.rds")
compare.density(a.out_florida_missing_75, var = "RAS_1_post")
compare.density(a.out_florida_missing_75, var = "RAS_3_post")
compare.density(a.out_florida_missing_75, var = "RAS_3_post")
compare.density(a.out_florida_missing_75, var = "RAS_5_post")
compare.density(a.out_florida_missing_75, var = "RCS_post")
compare.density(a.out_florida_missing_75, var = "ISLES_1_post")
compare.density(a.out_florida_missing_75, var = "ISLES_2_post")
compare.density(a.out_florida_missing_75, var = "INQ_1_post")
compare.density(a.out_florida_missing_75, var = "INQ_2_post")
compare.density(a.out_florida_missing_75, var = "MILQ_post")
compare.density(a.out_florida_missing_75, var = "SIS_1_post")
compare.density(a.out_florida_missing_75, var = "SIS_2_post")

impute_dat_loop_missing_75 = list()
for(i in 1:m){
  impute_dat_loop_missing_75[[i]] = a.out_florida_missing_75$imputations[[i]]
}
impute_dat_loop_missing_75
```
Get pre and post scores for missing 75
```{r}

mean_out_pre = list()
sd_out_pre = list()
head(impute_dat_loop_missing_75[[1]][8:18])
head(impute_dat_loop_missing_75[[1]][19:29])

for(i in 1:length(impute_dat_loop_missing_75)){
  mean_out_pre[[i]] = apply(impute_dat_loop_missing_75[[i]][8:18],2,mean)
  sd_out_pre[[i]] = apply(impute_dat_loop_missing_75[[i]][8:18], 2, sd)
}
dim(impute_dat_loop_missing_75[[1]][8:18])

parsout_pre = unlist(mean_out_pre) 
parsout_pre = matrix(parsout_pre, ncol = 11, byrow = TRUE)
parsout_pre
write.csv(parsout_pre, "parsout_pre.csv", row.names = FALSE)

sesout_pre = unlist(sd_out_pre)
sesout_pre = matrix(sesout_pre, ncol = 11, byrow = TRUE)
sesout_pre

pars_sesout_pre = mi.meld(parsout_pre, sesout_pre)
pars_sesout_pre
mean_pre = t(pars_sesout_pre$q.mi)
sd_pre = t(pars_sesout_pre$se.mi)
mean_sd_pre = round(data.frame(mean_pre, sd_pre),2)
mean_sd_pre$names = names(impute_dat_loop_missing_75[[1]][8:18])           
write.csv(mean_sd_pre, "mean_sd_pre_75.csv", row.names = TRUE)

mean_out_post = list()
sd_out_post = list()

for(i in 1:length(impute_dat_loop_missing_75)){
  mean_out_post[[i]] = apply(impute_dat_loop_missing_75[[i]][19:29],2,mean)
  sd_out_post[[i]] = apply(impute_dat_loop_missing_75[[i]][19:29], 2, sd)
}
dim(impute_dat_loop_missing_75[[1]][19:29])
parsout_post = unlist(mean_out_post) 
parsout_post = matrix(parsout_post, ncol = 11, byrow = TRUE)
parsout_post
write.csv(parsout_post, "parsout_post.csv", row.names = FALSE)
sesout_post = unlist(sd_out_post)
sesout_post = matrix(sesout_post, ncol = 11, byrow = TRUE)
sesout_post

pars_sesout_post = mi.meld(parsout_post, sesout_post)
pars_sesout_post

mean_post = t(pars_sesout_post$q.mi)
sd_post = t(pars_sesout_post$se.mi)
mean_sd_post = round(data.frame(mean_post, sd_post),2)
mean_sd_post$names = names(impute_dat_loop_missing_75[[1]][19:29])           
write.csv(mean_sd_post, "mean_sd_post_75.csv", row.names = TRUE)
mean_sd_post
### Get the differnece scores for t-test and cohen's d
## Need difference mean, differene sd
out_dif_t = list()
mean_out_diff = list()
sd_out_diff = list()

for(i in 1:length(impute_dat_loop_missing_75)){
  out_dif_t[[i]] = impute_dat_loop_missing_75[[1]][19:29] - impute_dat_loop_missing_75[[i]][8:18]
  mean_out_diff[[i]] = apply(out_dif_t[[i]],2,mean)
  sd_out_diff[[i]] = apply(out_dif_t[[i]], 2, sd)
}
parsout_diff = unlist(mean_out_diff) 
parsout_diff = matrix(parsout_diff, ncol = 11, byrow = TRUE)
parsout_diff
write.csv(parsout_diff, "parsout_diff.csv", row.names = FALSE)

sesout_diff = unlist(sd_out_diff)
sesout_diff = matrix(sesout_diff, ncol = 11, byrow = TRUE)
sesout_diff

pars_sesout_diff = mi.meld(parsout_diff, sesout_diff)
pars_sesout_diff
mean_diff = t(pars_sesout_diff$q.mi)
sd_diff = t(pars_sesout_diff$se.mi)
mean_sd_diff = round(data.frame(mean_diff, sd_diff),2)
mean_sd_diff$names = c("Perceived Burdensomeness", "Thwarted Belongingness", "Personal confidence and hope", "Goal and Success Orientation", "No domination by symptoms", "Comprehensibility", "Footing in the world", "MILQ", "RCS", "Suicidal Ideation", "Resolved plans and preparations")
mean_sd_diff
write.csv(mean_sd_diff, "mean_sd_diff_75.csv", row.names = TRUE)

```


Overall effect board member table for T-test and Cohen's D
T-test code
One sample cohen's D is just diff_score / diff_sd (Cohen, 1988, p. 48)
http://sphweb.bumc.bu.edu/otlt/MPH-Modules/BS/SAS/SAS4-OneSampleTtest/SAS4-OneSampleTtest7.html
https://ncss-wpengine.netdna-ssl.com/wp-content/themes/ncss/pdf/Procedures/PASS/One-Sample_T-Tests_using_Effect_Size.pdf
standard error: https://www.bmj.com/about-bmj/resources-readers/publications/statistics-square-one/7-t-tests
P-value: https://www.cyclismo.org/tutorial/R/pValues.html
```{r}
mean_sd_diff = round(mean_sd_diff[,1:2],2)
mean_sd_diff 
## sd_diff / sqrt(n) 
se_diff = mean_sd_diff$sd_diff / sqrt(dim(impute_dat_loop_missing_75[[1]])[1])
se_diff
t_stat = round(mean_sd_diff$mean_diff / se_diff,2)
t_stat
p_values_t = round(2*pt(-abs(t_stat), df = dim(impute_dat_loop_missing_75[[1]])[1]-1),3)
p_values_t = ifelse(p_values_t <= 0, "<.001", p_values_t)
p_values_t
critical_t = abs(qt(0.05/2, dim(impute_dat_loop_missing_75[[1]])[1]-1))

upper_reg_t = mean_sd_diff$mean_diff +(critical_t*se_diff)
lower_reg_t =  mean_sd_diff$mean_diff -(critical_t*se_diff)

ci_95_t = paste0(round(upper_reg_t,2), sep = ",", round(lower_reg_t,2))
cohen_d_t = round(mean_sd_diff$mean_diff / mean_sd_diff$sd_diff,2)
outcomes = c("Perceived Burdensomeness", "Thwarted Belongingness", "Personal confidence and hope", "Goal and Success Orientation", "No domination by symptoms", "Comprehensibility", "Footing in the world", "MILQ", "RCS", "Suicidal Ideation", "Resolved plans and preparations")
t_test_results_75 = data.frame(outcomes, t_stat, p_values_t, ci_95_t, cohen_d_t)
write.csv(t_test_results_75, "t_test_results_75.csv", row.names = FALSE)
t_test_results_75
```
#################################################
Results for complete data
#################################################
```{r}
center_dat_missing = center_dat[c(1:31, 64:65)]
center_dat_complete_analysis = na.omit(center_dat_missing)
head(center_dat)
dim(center_dat_complete_analysis)
center_dat_complete_analysis$black = NULL
center_dat_complete_analysis$another_race = NULL
mean_pre_complete =  apply(center_dat_complete_analysis[8:18], 2, mean)
sd_pre_complete = apply(center_dat_complete_analysis[8:18], 2, sd)
mean_post_complete = apply(center_dat_complete_analysis[19:29], 2, mean)
sd_post_complete = apply(center_dat_complete_analysis[19:29], 2, sd)
mean_sd_complete = cbind(mean_pre_complete, sd_pre_complete, mean_post_complete, sd_post_complete)
mean_sd_complete = data.frame(mean_sd_complete)
mean_sd_complete = round(mean_sd_complete,2)
mean_sd_complete
write.csv(mean_sd_complete, "mean_sd_pre_complete.csv")
```
Compare differences between the TOT and ITT findings
```{r}
mean_diff_complete = mean_sd_complete$mean_post_complete-mean_sd_complete$mean_pre_complete
se_diff_complete = sd(mean_diff_complete) /sqrt(dim(center_dat_complete_analysis)[1])


t_stat_diff_complete = round(mean_diff_complete / se_diff_complete,2)
t_stat_diff_complete
p_values_t_complete = round(2*pt(-abs(t_stat_diff_complete), df =dim(center_dat_complete_analysis)[1]-1),3)

p_values_t_complete = ifelse(p_values_t_complete <= 0, "<.001", p_values_t_complete)
p_values_t_complete

critical_t_complete = abs(qt(0.05/2, dim(center_dat_complete_analysis)[1]-1))

upper_reg_t_complete = mean_diff_complete +(critical_t_complete*se_diff_complete)
lower_reg_t_complete =  mean_diff_complete -(critical_t_complete*se_diff_complete)

ci_95_t_complete = paste0(round(lower_reg_t_complete,2), sep = ",", round(upper_reg_t_complete,2))
cohen_d_t_complete = round(mean_diff_complete / sd(mean_diff_complete),2)

outcomes = c("Perceived Burdensomeness", "Thwarted Belongingness", "Personal confidence and hope", "Goal and Success Orientation", "No domination by symptoms", "Comprehensibility", "Footing in the world", "MILQ", "RCS", "Suicidal Ideation", "Resolved plans and preparations")
t_test_results_complete = data.frame(outcomes, t_stat_diff_complete, p_values_t_complete, ci_95_t_complete, cohen_d_t_complete)
write.csv(t_test_results_complete, "t_test_results_complete.csv", row.names = FALSE)
t_test_results_complete
```
Difference in Cohen's D because standardized measure on complete and imputted

```{r}
### Run the full imputted results to get this data set
t_test_results
t_test_results_complete

mean(abs(t_test_results_complete$cohen_d_t_complete)-abs(t_test_results$cohen_d_t))


range(abs(t_test_results_complete$cohen_d_t_complete)-abs(t_test_results$cohen_d_t))

ds_impute_complete = data.frame(outcomes = t_test_results_complete$outcomes, cohen_d_t_complete = t_test_results_complete$cohen_d_t_complete, cohen_d_t = t_test_results$cohen_d_t, abs_diff_cohen = abs(t_test_results_complete$cohen_d_t_complete)-abs(t_test_results$cohen_d_t))

ds_impute_complete
mean(ds_impute_complete$abs_diff_cohen)
```
#############################
Discharge data Pre
Rerun first chunk of data cleaning prior to running this model
#############################
```{r}
setwd("P:/Evaluation/TN Lives Count_Writing/florida_results")
discharge_dat = read.csv("Deidentified Lockman data project.csv", header = TRUE, na.strings = c("", " "))
center_dat_discharge = data.frame(CID = center_psycho$CID, demos, INQ_1_pre, INQ_2_pre, RAS_1_pre, RAS_3_pre, RAS_5_pre, ISLES_1_pre, ISLES_2_pre, MILQ_pre, RCS_pre, SIS_1_pre, SIS_2_pre, INQ_1_post, INQ_2_post, RAS_1_post, RAS_3_post, RAS_5_post, ISLES_1_post, ISLES_2_post, MILQ_post, RCS_post, SIS_1_post, SIS_2_post, suicide, female)
head(center_dat)
dim(discharge_dat)
discharge_center_dat = merge(center_dat_discharge, discharge_dat, by = "CID", all.y = TRUE)
dim(discharge_center_dat)
head(discharge_center_dat)
### Change NAs for months, because those are zeros
discharge_center_dat[,37:42][is.na(discharge_center_dat[,37:42])] = 0

### Just keep pre measures
discharge_center_dat = discharge_center_dat[,c(1:19, 33,34,37:42)]
miss_var_summary(discharge_center_dat)
discharge_center_dat_complete = na.omit(discharge_center_dat)
n_discharge = dim(discharge_center_dat_complete)[1]
n_discharge

### Crate a total discharge
discharge_center_dat_complete$total_discharge =  apply(discharge_center_dat_complete[,22:27], 1, sum)
discharge_center_dat_complete
describe.factor(discharge_center_dat_complete$total_discharge)
### Create dataset without outlier
outlier_dat = subset(discharge_center_dat_complete, total_discharge < 15)

```
Participant characteristics
```{r}
dim(discharge_center_dat_complete)
fac_des=apply(discharge_center_dat_complete[,c(3:10, 20:21, 28)], 2, function(x){describe.factor(x)})
fac_des = data.frame(fac_des)
fac_des = t(fac_des)
write.csv(fac_des, "fac_dec.csv")
mean_con = apply(discharge_center_dat_complete[,c(2, 11:19)], 2, mean)
mean_con
sd_con = apply(discharge_center_dat_complete[,c(2, 11:19)], 2, sd)
sd_con
range_con = apply(discharge_center_dat_complete[,c(2, 11:19)], 2, range)
range_con
con_des = rbind(mean_con, sd_con, range_con)
write.csv(con_des,"con_des.csv")
```
Get spearman correlations
```{r}

library(corrplot)
cor_mat = cor(outlier_dat[,c(11:19, 28)], use = "pairwise.complete.obs", method = "spearman")
corrplot(cor_mat, type = "upper", insig = "blank")

```
Try the hurdle model
```{r}
library(pscl)
library(caret)
discharge_center_dat_complete
cor_dat_pre = cor(discharge_center_dat_complete[,c(11:19,28)])
findCorrelation(cor_dat_pre, cutoff = .75)
discharge_hurdle = hurdle(total_discharge ~ INQ_1_pre  +INQ_2_pre +RAS_1_pre + RAS_3_pre + RAS_5_pre  + ISLES_1_pre +ISLES_2_pre + MILQ_pre + RCS_pre, data = discharge_center_dat_complete, dist = "negbin", zero.dist = "binomial")
summary(discharge_hurdle)

```
Sensitivity (get rid of outlier at 16 discharges)
```{r}
outlier_dat = subset(discharge_center_dat_complete, total_discharge < 15)
dim(outlier_dat)
dim(discharge_center_dat_complete)

discharge_hurdle_outlier = hurdle(total_discharge ~ INQ_1_pre  +INQ_2_pre +RAS_1_pre + RAS_3_pre + RAS_5_pre  + ISLES_1_pre +ISLES_2_pre + MILQ_pre + RCS_pre, data = outlier_dat, dist = "negbin", zero.dist = "binomial")
discharge_hurdle_outlier_sum = summary(discharge_hurdle_outlier)
discharge_hurdle_outlier_sum

```
One by one with all data
```{r}

list_vars = as.list(discharge_center_dat_complete[,11:19])
dis_results_out = list()
for(i in 1:length(list_vars)){
  dis_results_out[[i]] = hurdle(total_discharge ~ list_vars[[i]], data = discharge_center_dat_complete, dist = "negbin", zero.dist = "binomial")
  dis_results_out[[i]] = summary(dis_results_out[[i]])
}
dis_results_out

### Try without outlier
list_vars = as.list(discharge_center_dat_complete[,11:19])
dis_results_out = list()
for(i in 1:length(list_vars)){
  dis_results_out[[i]] = hurdle(total_discharge ~ list_vars[[i]], data = discharge_center_dat_complete, dist = "negbin", zero.dist = "binomial")
  dis_results_out[[i]] = summary(dis_results_out[[i]])
}
dis_results_out

### ISLES_1_pre is significant try this variable with some others
list_vars_no_ISLES_1_pre = as.list(discharge_center_dat_complete[,c(11:15, 17:19)])
dis_results_out = list()
for(i in 1:length(list_vars_no_ISLES_1_pre)){
  dis_results_out[[i]] = hurdle(total_discharge ~ list_vars_no_ISLES_1_pre[[i]] +ISLES_1_pre, data = discharge_center_dat_complete, dist = "negbin", zero.dist = "binomial")
  dis_results_out[[i]] = summary(dis_results_out[[i]])
}
dis_results_out
```
#############################
Discharge data Post
#############################
```{r}
setwd("P:/Evaluation/TN Lives Count_Writing/florida_results")
discharge_dat = read.csv("Deidentified Lockman data project.csv", header = TRUE, na.strings = c("", " "))
center_dat_discharge = data.frame(CID = center_psycho$CID, demos, INQ_1_pre, INQ_2_pre, RAS_1_pre, RAS_3_pre, RAS_5_pre, ISLES_1_pre, ISLES_2_pre, MILQ_pre, RCS_pre, SIS_1_pre, SIS_2_pre, INQ_1_post, INQ_2_post, RAS_1_post, RAS_3_post, RAS_5_post, ISLES_1_post, ISLES_2_post, MILQ_post, RCS_post, SIS_1_post, SIS_2_post, suicide, female)
head(center_dat)
dim(discharge_dat)
discharge_center_dat = merge(center_dat_discharge, discharge_dat, by = "CID", all.y = TRUE)
dim(discharge_center_dat)
head(discharge_center_dat)
### Change NAs for months, because those are zeros
discharge_center_dat[,37:42][is.na(discharge_center_dat[,37:42])] = 0
discharge_center_dat
### Just keep post measures
discharge_center_dat = discharge_center_dat[,c(2:10,22:30, 33:34,37:42)]
miss_var_summary(discharge_center_dat)
discharge_center_dat_complete = na.omit(discharge_center_dat)
n_discharge = dim(discharge_center_dat_complete)[1]
n_discharge

### Crate a total discharge
discharge_center_dat_complete$total_discharge =  apply(discharge_center_dat_complete[,21:26], 1, sum)
discharge_center_dat_complete
describe.factor(discharge_center_dat_complete$total_discharge)
```
Participant characteristics
```{r}
fac_des=apply(discharge_center_dat_complete[,c(2:9, 19:20, 27)], 2, function(x){describe.factor(x)})
fac_des = data.frame(fac_des)
fac_des = t(fac_des)
write.csv(fac_des, "fac_des.csv")
mean_con = apply(discharge_center_dat_complete[,c(1, 10:18)], 2, mean)
mean_con
sd_con = apply(discharge_center_dat_complete[,c(1, 10:18)], 2, sd)
sd_con
range_con = apply(discharge_center_dat_complete[,c(1, 10:18)], 2, range)
range_con
con_des = rbind(mean_con, sd_con, range_con)
write.csv(con_des, "con_des.csv")
```

Try the hurdle model
```{r}
library(pscl)
discharge_center_dat_complete

cor_discharge_post =  cor(discharge_center_dat_complete[,c(10:18,27)])
findCorrelation(cor_discharge_post, cutoff = .75, names = TRUE, verbose = TRUE)

discharge_hurdle = hurdle(total_discharge ~ INQ_1_post  +INQ_2_post +RAS_1_post + RAS_3_post + RAS_5_post  + ISLES_1_post +ISLES_2_post + MILQ_post + RCS_post, data = discharge_center_dat_complete, dist = "negbin", zero.dist = "binomial")
summary(discharge_hurdle)
exp(1.42287)
```
Try without outlier
```{r}
library(pscl)

outlier_discharge_dat =  subset(discharge_center_dat_complete, total_discharge < 10)
dim(discharge_center_dat_complete)
dim(outlier_discharge_dat)
cor(discharge_center_dat_complete[,c(10:18,27)])

discharge_hurdle = hurdle(total_discharge ~ INQ_1_post  +INQ_2_post +RAS_1_post + RAS_3_post + RAS_5_post  + ISLES_1_post +ISLES_2_post + MILQ_post + RCS_post, data = outlier_discharge_dat, dist = "negbin", zero.dist = "binomial")
summary(discharge_hurdle)


```
One by one with all data
```{r}

list_vars = as.list(discharge_center_dat_complete[,10:18])
dis_results_out = list()
for(i in 1:length(list_vars)){
  dis_results_out[[i]] = hurdle(total_discharge ~ list_vars[[i]], data = discharge_center_dat_complete, dist = "negbin", zero.dist = "binomial")
  dis_results_out[[i]] = summary(dis_results_out[[i]])
}
dis_results_out

###  try without outlier
list_vars = as.list(outlier_discharge_dat[,10:18])
dis_results_out = list()
for(i in 1:length(list_vars)){
  dis_results_out[[i]] = hurdle(total_discharge ~ list_vars[[i]], data = outlier_discharge_dat, dist = "negbin", zero.dist = "binomial")
  dis_results_out[[i]] = summary(dis_results_out[[i]])
}
dis_results_out


```
###########################################
Simulating pre discharge scores
###########################################
Run first two lines of code to get Florida data
To simulate use: https://uvastatlab.github.io/2019/08/29/simulating-data-for-count-models/
```{r}
setwd("P:/Evaluation/TN Lives Count_Writing/florida_results")
discharge_dat = read.csv("Deidentified Lockman data project.csv", header = TRUE, na.strings = c("", " "))
center_dat_discharge = data.frame(CID = center_psycho$CID, demos, INQ_1_pre, INQ_2_pre, RAS_1_pre, RAS_3_pre, RAS_5_pre, ISLES_1_pre, ISLES_2_pre, MILQ_pre, RCS_pre, SIS_1_pre, SIS_2_pre, INQ_1_post, INQ_2_post, RAS_1_post, RAS_3_post, RAS_5_post, ISLES_1_post, ISLES_2_post, MILQ_post, RCS_post, SIS_1_post, SIS_2_post, suicide, female)
head(center_dat)
dim(discharge_dat)
discharge_center_dat = merge(center_dat_discharge, discharge_dat, by = "CID", all.y = TRUE)
dim(discharge_center_dat)
head(discharge_center_dat)
### Change NAs for months, because those are zeros
discharge_center_dat[,37:42][is.na(discharge_center_dat[,37:42])] = 0

### Just keep pre measures
discharge_center_dat = discharge_center_dat[,c(1:19, 33,34,37:42)]
discharge_center_dat_complete = na.omit(discharge_center_dat)
n_discharge = dim(discharge_center_dat_complete)[1]
n_discharge

### Crate a total discharge
discharge_center_dat_complete$total_discharge =  apply(discharge_center_dat_complete[,22:27], 1, sum)
discharge_center_dat_complete
describe.factor(discharge_center_dat_complete$total_discharge)

outlier_dat = subset(discharge_center_dat_complete, total_discharge < 15)

```
Participant characteristics
```{r}
dim(discharge_center_dat_complete)
fac_des=apply(discharge_center_dat_complete[,c(3:10, 20:21, 28)], 2, function(x){describe.factor(x)})
fac_des = data.frame(fac_des)
fac_des = t(fac_des)
write.csv(fac_des, "fac_dec.csv")
mean_con = apply(discharge_center_dat_complete[,c(2, 11:19)], 2, mean)
mean_con
sd_con = apply(discharge_center_dat_complete[,c(2, 11:19)], 2, sd)
sd_con
range_con = apply(discharge_center_dat_complete[,c(2, 11:19)], 2, range)
range_con
con_des = rbind(mean_con, sd_con, range_con)
write.csv(con_des,"con_des.csv")
```




Simple replication for example
```{r}
dim(outlier_dat)
library(pscl)
discharge_hurdle = zeroinfl(formula = total_discharge ~ RAS_1_pre + RAS_3_pre + RAS_5_pre  + ISLES_1_pre +ISLES_2_pre + MILQ_pre + RCS_pre | RAS_1_pre + RAS_3_pre + RAS_5_pre  + ISLES_1_pre +ISLES_2_pre + MILQ_pre + RCS_pre, data = outlier_dat, dist = "negbin")
summary(discharge_hurdle)
```
Get the distributions of each variable
```{r}
dat_hist = as.list(outlier_dat[13:19])
names = as.list(colnames(outlier_dat[13:19]))
for(i in 1:length(dat_hist)){
  hist(dat_hist[[i]], main = paste("Histogram of" ,names[[i]]))
}

```
Demonstrating replication 
```{r}
power_zero_inflat_rep = function(){
n = rep(10000, 1000)
n = as.list(n)
RAS_1_pre = list()
RAS_3_pre = list()
RAS_5_pre = list()
ISLES_1_pre = list()
ISLES_2_pre = list()
MILQ_pre = list()
RCS_pre = list()
z = list()
y_sim = list()
discharge_hurdle = list()
discharge_hurdle_sum = list()
count_p = list()
logit_p = list()
for(i in 1:length(n)){

RAS_1_pre[[i]]  = rnorm(n = n[[i]], mean = mean(outlier_dat$RAS_1_pre), sd= sd(outlier_dat$RAS_1_pre))
RAS_1_pre[[i]] = ifelse(RAS_1_pre[[i]]> 5, 5, ifelse(RAS_1_pre[[i]]  < 1, 1, RAS_1_pre[[i]]))

RAS_3_pre[[i]]  = rnorm(n = n[[i]], mean = mean(outlier_dat$RAS_3_pre), sd= sd(outlier_dat$RAS_3_pre))
RAS_3_pre[[i]]  = ifelse(RAS_3_pre[[i]] > 5, 5, ifelse(RAS_3_pre[[i]]  < 1, 1, RAS_3_pre[[i]] ))

RAS_5_pre[[i]] = rnorm(n = n[[i]] , mean = mean(outlier_dat$RAS_5_pre), sd= sd(outlier_dat$RAS_5_pre))
RAS_5_pre[[i]]  = ifelse(RAS_5_pre[[i]] > 5, 5, ifelse(RAS_5_pre[[i]]  < 1, 1, RAS_5_pre[[i]]))

ISLES_1_pre[[i]] =  rnorm(n = n[[i]], mean = mean(outlier_dat$ISLES_1_pre), sd= sd(outlier_dat$ISLES_1_pre))
ISLES_1_pre[[i]] = ifelse(ISLES_1_pre[[i]]> 5, 5, ifelse(ISLES_1_pre[[i]] < 1, 1, ISLES_1_pre[[i]]))

ISLES_2_pre[[i]]= rnorm(n = n[[i]], mean = mean(outlier_dat$ISLES_2_pre), sd= sd(outlier_dat$ISLES_2_pre))
ISLES_2_pre[[i]] = ifelse(ISLES_2_pre[[i]]> 5, 5, ifelse(ISLES_2_pre[[i]] < 1, 1, ISLES_2_pre[[i]]))

MILQ_pre[[i]] = rnorm(n = n[[i]], mean = mean(outlier_dat$MILQ_pre), sd= sd(outlier_dat$MILQ_pre))
MILQ_pre[[i]] = ifelse(MILQ_pre[[i]]> 5, 5, ifelse(MILQ_pre[[i]] < 1, 1, MILQ_pre[[i]]))

RCS_pre[[i]]  =rnorm(n = n[[i]], mean = mean(outlier_dat$RCS_pre), sd= sd(outlier_dat$RCS_pre))   
RCS_pre[[i]] = ifelse(RCS_pre[[i]]> 7, 7, ifelse(RCS_pre[[i]] < 1, 1, RCS_pre[[i]]))


z[[i]] <- rbinom(n = n[[i]], size = 1, prob = 1/(1 + exp((0.6465 + -2.2895 * (RAS_1_pre[[i]]) +-0.1523* (RAS_3_pre[[i]]) + 0.8030*(RAS_5_pre[[i]])+ -0.1993*(ISLES_1_pre[[i]])+ -0.1353*(ISLES_2_pre[[i]])+0.7051*(MILQ_pre[[i]])+0.6645*(RCS_pre[[i]]))))) 

y_sim[[i]] <- ifelse(z[[i]] == 0, 0, rnbinom(n = n[[i]],  mu = exp(0.22739 + -0.49252 * (RAS_1_pre[[i]]) + -0.32546* (RAS_3_pre[[i]])+ 0.32722* (RAS_5_pre[[i]])+ -0.22358* (ISLES_1_pre[[i]])+ 0.16369*(ISLES_2_pre[[i]])+ 0.50411*(MILQ_pre[[i]])+ 0.07625*(RCS_pre[[i]])), size = 1.9818))

discharge_hurdle[[i]] = zeroinfl(formula = y_sim[[i]] ~ RAS_1_pre[[i]] + RAS_3_pre[[i]] + RAS_5_pre[[i]] + ISLES_1_pre[[i]] + ISLES_2_pre[[i]] + MILQ_pre[[i]] + RCS_pre[[i]], dist = "negbin")
discharge_hurdle_sum[[i]] = summary(discharge_hurdle[[i]])
count_p[[i]] = discharge_hurdle_sum[[i]]$coefficients$count[,1]
logit_p[[i]] = discharge_hurdle_sum[[i]]$coefficients$zero[,1]
  
}
return(list(count_p, logit_p))
}

```
Replication analysis results
```{r}
power_rep = power_zero_inflat_rep()
power_rep_count= unlist(power_rep[[1]])
power_rep_binary = unlist(power_rep[[2]])

#n = c(300, 400)
#n = rep(c(seq(from = 200, to = 600, by =50)), each =100)
power_matrix_count = matrix(power_rep_count, ncol = 9, nrow = length(n), byrow = TRUE)
power_matrix_count = data.frame(power_matrix_count)
names = c("intercept_count", paste0(colnames(outlier_dat[13:19]), "_", "count"), "theta_count")
colnames(power_matrix_count) = names
power_matrix_count = apply(power_matrix_count, 2, mean)
power_matrix_count = t(data.frame(t(power_matrix_count)))
colnames(power_matrix_count) = "sim_par_ests"
power_matrix_count
## Actual estimates 0.22739 + -0.49252 * (RAS_1_pre[[i]]) + -0.32546* (RAS_3_pre[[i]])+ 0.32722* (RAS_5_pre[[i]])+ -0.22358* (ISLES_1_pre[[i]])+ 0.16369*(ISLES_2_pre[[i]])+ 0.50411*(MILQ_pre[[i]])+ 0.07625*(RCS_pre[[i]])), size = 1.9818
count_pars = t(data.frame(t(c(discharge_hurdle$coefficients$count, discharge_hurdle$theta))))
colnames(count_pars) = "actual_par_ests"
power_matrix_count = cbind(power_matrix_count, count_pars)
power_matrix_count = data.frame(power_matrix_count)
power_matrix_count$abs_bias = abs(power_matrix_count$sim_par_ests-power_matrix_count$actual_par_ests)
power_matrix_count$percent_abs_bias = abs((power_matrix_count$sim_par_ests-power_matrix_count$actual_par_ests)/power_matrix_count$actual_par_ests)
power_matrix_count = round(power_matrix_count,2)
power_matrix_count

#### Binary##############################
power_matrix_binary = matrix(power_rep_binary, ncol = 8, nrow = length(n), byrow = TRUE)
power_matrix_binary = data.frame(power_matrix_binary)
names = c("intercept_binary", paste0(colnames(outlier_dat[13:19]), "_", "binary"))
colnames(power_matrix_binary) = names
power_matrix_binary = apply(power_matrix_binary, 2, mean)
power_matrix_binary = t(data.frame(t(power_matrix_binary)))
colnames(power_matrix_binary) = "sim_par_ests"
power_matrix_binary
binary_pars = t(data.frame(t(c(discharge_hurdle$coefficients$zero))))
colnames(binary_pars) = "actual_par_ests"
power_matrix_binary = cbind(power_matrix_binary, binary_pars)
power_matrix_binary = data.frame(power_matrix_binary)
power_matrix_binary$abs_bias = abs(power_matrix_binary$sim_par_ests-power_matrix_binary$actual_par_ests)
power_matrix_binary$percent_abs_bias = abs((power_matrix_binary$sim_par_ests-power_matrix_binary$actual_par_ests)/power_matrix_binary$actual_par_ests)
power_matrix_binary = round(power_matrix_binary,2)
power_matrix_binary


```
Power analysis function
```{r}
power_zero_inflat = function(){
n = rep(c(seq(from = 200, to = 600, by =50)), each =100)
#n = c(300, 400)
n = as.list(n)
RAS_1_pre = list()
RAS_3_pre = list()
RAS_5_pre = list()
ISLES_1_pre = list()
ISLES_2_pre = list()
MILQ_pre = list()
RCS_pre = list()
z = list()
y_sim = list()
discharge_hurdle = list()
discharge_hurdle_sum = list()
count_p = list()
logit_p = list()
for(i in 1:length(n)){

RAS_1_pre[[i]]  = rnorm(n = n[[i]], mean = mean(outlier_dat$RAS_1_pre), sd= sd(outlier_dat$RAS_1_pre))
RAS_1_pre[[i]] = ifelse(RAS_1_pre[[i]]> 5, 5, ifelse(RAS_1_pre[[i]]  < 1, 1, RAS_1_pre[[i]]))

RAS_3_pre[[i]]  = rnorm(n = n[[i]], mean = mean(outlier_dat$RAS_3_pre), sd= sd(outlier_dat$RAS_3_pre))
RAS_3_pre[[i]]  = ifelse(RAS_3_pre[[i]] > 5, 5, ifelse(RAS_3_pre[[i]]  < 1, 1, RAS_3_pre[[i]] ))

RAS_5_pre[[i]] = rnorm(n = n[[i]] , mean = mean(outlier_dat$RAS_5_pre), sd= sd(outlier_dat$RAS_5_pre))
RAS_5_pre[[i]]  = ifelse(RAS_5_pre[[i]] > 5, 5, ifelse(RAS_5_pre[[i]]  < 1, 1, RAS_5_pre[[i]]))

ISLES_1_pre[[i]] =  rnorm(n = n[[i]], mean = mean(outlier_dat$ISLES_1_pre), sd= sd(outlier_dat$ISLES_1_pre))
ISLES_1_pre[[i]] = ifelse(ISLES_1_pre[[i]]> 5, 5, ifelse(ISLES_1_pre[[i]] < 1, 1, ISLES_1_pre[[i]]))

ISLES_2_pre[[i]]= rnorm(n = n[[i]], mean = mean(outlier_dat$ISLES_2_pre), sd= sd(outlier_dat$ISLES_2_pre))
ISLES_2_pre[[i]] = ifelse(ISLES_2_pre[[i]]> 5, 5, ifelse(ISLES_2_pre[[i]] < 1, 1, ISLES_2_pre[[i]]))

MILQ_pre[[i]] = rnorm(n = n[[i]], mean = mean(outlier_dat$MILQ_pre), sd= sd(outlier_dat$MILQ_pre))
MILQ_pre[[i]] = ifelse(MILQ_pre[[i]]> 5, 5, ifelse(MILQ_pre[[i]] < 1, 1, MILQ_pre[[i]]))

RCS_pre[[i]]  =rnorm(n = n[[i]], mean = mean(outlier_dat$RCS_pre), sd= sd(outlier_dat$RCS_pre))   
RCS_pre[[i]] = ifelse(RCS_pre[[i]]> 7, 7, ifelse(RCS_pre[[i]] < 1, 1, RCS_pre[[i]]))


z[[i]] <- rbinom(n = n[[i]], size = 1, prob = 1/(1 + exp((0.6465 + -2.2895 * (RAS_1_pre[[i]]) +-0.1523* (RAS_3_pre[[i]]) + 0.8030*(RAS_5_pre[[i]])+ -0.1993*(ISLES_1_pre[[i]])+ -0.1353*(ISLES_2_pre[[i]])+0.7051*(MILQ_pre[[i]])+0.6645*(RCS_pre[[i]]))))) 

y_sim[[i]] <- ifelse(z[[i]] == 0, 0, rnbinom(n = n[[i]],  mu = exp(0.22739 + -0.49252 * (RAS_1_pre[[i]]) + -0.32546* (RAS_3_pre[[i]])+ 0.32722* (RAS_5_pre[[i]])+ -0.22358* (ISLES_1_pre[[i]])+ 0.16369*(ISLES_2_pre[[i]])+ 0.50411*(MILQ_pre[[i]])+ 0.07625*(RCS_pre[[i]])), size = 1.9818))

discharge_hurdle[[i]] = zeroinfl(formula = y_sim[[i]] ~ RAS_1_pre[[i]] + RAS_3_pre[[i]] + RAS_5_pre[[i]] + ISLES_1_pre[[i]] + ISLES_2_pre[[i]] + MILQ_pre[[i]] + RCS_pre[[i]], dist = "negbin")
discharge_hurdle_sum[[i]] = summary(discharge_hurdle[[i]])
count_p[[i]] = ifelse(discharge_hurdle_sum[[i]]$coefficients$count[c(2:8),4] < .05, 1, 0)
logit_p[[i]] = ifelse(discharge_hurdle_sum[[i]]$coefficients$zero[c(2:8),4] < .05, 1, 0)

}
return(list(count_p, logit_p))
}


```
Get power analysis results
Not right, the logistic results are in the second unlist
```{r}
power_rep = power_zero_inflat()
power_matrix_count = power_rep
power_matrix_count= unlist(power_matrix_count)
length(power_matrix_count)/7
n = rep(c(seq(from = 200, to = 600, by =50)), each =100)
n = rep(n, 2)
length(n)
power_matrix_count = matrix(power_matrix_count, ncol = 7, nrow = length(n), byrow = TRUE)
power_matrix_count
power_matrix_count = data.frame(power_matrix_count)
colnames(power_matrix_count)= colnames(outlier_dat[13:19])
power_matrix_count
power_matrix_count$n = n 
power_matrix_count$model = c(rep(c("count"), length(n)/2), rep(c("binary"), length(n)/2))
power_matrix_count
### sum by n
power_matrix_count = na.omit(power_matrix_count)
library(dplyr)
sum_dat = power_matrix_count %>% 
  group_by(n, model) %>% 
  summarise_all(funs(sum))
sum_dat

count_dat =  power_matrix_count %>% group_by(n, model) %>% tally()

power_dat = data.frame(sum_dat, count = count_dat$nn)
power_dat[,3:9] =round(power_dat[,3:9]/ power_dat$count,2)
power_dat$count = NULL
power_dat
library(reshape2)

power_long <- melt(power_dat, id.vars=c(1,2))
power_long
colnames(power_long) = c("n", "model", "predictor", "power")
power_long
```
Then plot the power for count model
```{r}
library(ggplot2)
power_long_count = subset(power_long, model == "count")
power_long_count %>%
  ggplot( aes(x=n, y=power, group=predictor, color=predictor)) +
    geom_line()+
    geom_hline(yintercept=.8, linetype="dashed", color = "red")+
  ggtitle("Figure 1: Count model power analysis for relapse by predictor")

```
Now logit
```{r}
library(ggplot2)
power_long_binary = subset(power_long, model == "binary")
power_long_binary %>%
  ggplot( aes(x=n, y=power, group=predictor, color=predictor)) +
    geom_line()+
    geom_hline(yintercept=.8, linetype="dashed", color = "red")+
  ggtitle("Figure 2: Logit model power analysis for relapse by predictor")


```


This replicates: https://uvastatlab.github.io/2019/08/29/simulating-data-for-count-models/#fnref1
```{r}
set.seed(7)
n <- 10000
male <- sample(c(0,1), size = n, replace = TRUE)
z <- rbinom(n = n, size = 1, 
            prob = 1/(1 + exp((0.8 + 1.8 * (male))))) 
y_sim <- ifelse(z == 0, 0, 
                rnbinom(n = n, 
                        mu = exp(1.3 + 1.5 * (male)), 
                        size = 2))

m6 <- zeroinfl(y_sim ~ male | male, dist = "negbin")
summary(m6)
```
Look at beta function
```{r}
b1 <- seq(0, 1, by = 0.02)    
b1
by1 <- round(dbeta(b1, shape1 = 5, shape2 = 20),2) 
by1
plot(by1)
```






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

### Lower scores are better
ISLES_pre = center_dat[,34:39]
ISLES_pre_des = data.frame(apply(ISLES_pre, 2, as.factor))
describe(ISLES_pre_des)
ISLES_pre = rowMeans(ISLES_pre, na.rm = TRUE)

ISLES_post = center_dat[,132:137]
ISLES_post_des = data.frame(apply(ISLES_post, 2, as.factor))
describe(ISLES_post_des)
ISLES_post = rowMeans(ISLES_post, na.rm = TRUE)

###MILQ higher scores are better
MILQ_pre = center_dat[,40:44]
MILQ_pre_des = data.frame(apply(MILQ_pre, 2, as.factor))
describe(MILQ_pre_des)
head(MILQ_pre)
MILQ_pre = rowMeans(MILQ_pre, na.rm = TRUE)

MILQ_post = center_dat[,127:131]
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

## Only want treat_c, treat_g, treat_i, treat_l, treat_m, because they have more than 25% in each category
#treatments = data.frame( treat_c = center_dat$X9_TREAT.c.Received, treat_g = center_dat$X9_Treat.g.Received, treat_l = center_dat$X9_Treat.L.Received, treat_m = center_dat$X9_Treat.m.Received)

#treatments$treat_b = ifelse(treatments$treat_b == 1, 1, 0)
#treatments_fac = data.frame(apply(treatments, 2, as.factor))
#describe(treatments_fac)


center_dat = data.frame(demos, INQ_1_pre, INQ_2_pre, RAS_3_pre, ISLES_pre, MILQ_pre, RCS_pre, INQ_1_post, INQ_2_post, RAS_3_post, ISLES_post, MILQ_post, RCS_post)

```
Descriptives with complete data
```{r}
center_dat
center_dat[,2:7] = data.frame(apply(center_dat[,2:7],2, as.factor))
describe(center_dat)
```
Create difference scores for linear model and assess
```{r}
center_dat
```


Assess missing 
```{r}
head(center_dat)
library(naniar)
miss_var_summary(center_dat)

dim(center_dat)
center_dat_complete = na.omit(center_dat)
dim(center_dat_complete)

```


Just try will complete data set and see what happens
```{r}


```



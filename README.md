---
title: "T_test_code"
output: html_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

Get T-test to work
```{r}
center_results_d1 = list()
for(i in 1:length(out_diff_dat_d1_post)){
  center_results_d1[[i]]= t.test(out_diff_dat_d1_post[[i]], out_diff_dat_d1_pre[[i]], paired = TRUE)
  center_results_d1[[i]] = center_results_d1[[i]][c(1,3,4)]
}

center_results_d1 = unlist(center_results_d1)
center_results_d1 = matrix(center_results_d1, ncol = 4, byrow = TRUE)
center_results_d1 = data.frame(center_results_d1)
center_results_d1 = round(center_results_d1, 3)
center_results_d1
colnames(center_results_d1) = c("t_stat", "p_value", "lower", "upper")
center_results_d1

center_results_d2 = list()
for(i in 1:length(out_diff_dat_d2_post)){
  center_results_d2[[i]]= t.test(out_diff_dat_d2_post[[i]], out_diff_dat_d2_pre[[i]], paired = TRUE)
  center_results_d2[[i]] = center_results_d2[[i]][c(1,3,4)]
}

center_results_d2 = unlist(center_results_d2)
center_results_d2 = matrix(center_results_d2, ncol = 4, byrow = TRUE)
center_results_d2 = data.frame(center_results_d2)
center_results_d2 = round(center_results_d2, 3)
center_results_d2
colnames(center_results_d2) = c("t_stat", "p_value", "lower", "upper")
center_results_d2

center_results_d3 = list()
for(i in 1:length(out_diff_dat_d3_post)){
  center_results_d3[[i]]= t.test(out_diff_dat_d3_post[[i]], out_diff_dat_d3_pre[[i]], paired = TRUE)
  center_results_d3[[i]] = center_results_d3[[i]][c(1,3,4)]
}

center_results_d3 = unlist(center_results_d3)
center_results_d3 = matrix(center_results_d3, ncol = 4, byrow = TRUE)
center_results_d3 = data.frame(center_results_d3)
center_results_d3 = round(center_results_d3, 3)
center_results_d3
colnames(center_results_d3) = c("t_stat", "p_value", "lower", "upper")
center_results_d3

center_results_d4 = list()
for(i in 1:length(out_diff_dat_d4_post)){
  center_results_d4[[i]]= t.test(out_diff_dat_d4_post[[i]], out_diff_dat_d4_pre[[i]], paired = TRUE)
  center_results_d4[[i]] = center_results_d4[[i]][c(1,3,4)]
}

center_results_d4 = unlist(center_results_d4)
center_results_d4 = matrix(center_results_d4, ncol = 4, byrow = TRUE)
center_results_d4 = data.frame(center_results_d4)
center_results_d4 = round(center_results_d4, 3)
center_results_d4
colnames(center_results_d4) = c("t_stat", "p_value", "lower", "upper")
center_results_d4

center_results_d5 = list()
for(i in 1:length(out_diff_dat_d5_post)){
  center_results_d5[[i]]= t.test(out_diff_dat_d5_post[[i]], out_diff_dat_d5_pre[[i]], paired = TRUE)
  center_results_d5[[i]] = center_results_d5[[i]][c(1,3,4)]
}

center_results_d5 = unlist(center_results_d5)
center_results_d5 = matrix(center_results_d5, ncol = 4, byrow = TRUE)
center_results_d5 = data.frame(center_results_d5)
center_results_d5 = round(center_results_d5, 3)
center_results_d5
colnames(center_results_d5) = c("t_stat", "p_value", "lower", "upper")
center_results_d5

center_results_t_stat = data.frame(t_test1 = center_results_d1$t_stat, t_test2 = center_results_d2$t_stat, t_test3 = center_results_d3$t_stat, t_test4 = center_results_d4$t_stat, t_test5 = center_results_d5$t_stat)
center_results_t_stat = rowMeans(center_results_t_stat)
center_results_t_stat

center_results_p_value = data.frame(p_value1 = center_results_d1$p_value, p_value2 = center_results_d2$p_value, p_value3 = center_results_d3$p_value, p_value4 = center_results_d4$p_value, p_value5 = center_results_d5$p_value)
center_results_p_value = rowMeans(center_results_p_value)
center_results_p_value

center_results_upper = data.frame(upper1 = center_results_d1$upper, upper2 = center_results_d2$upper, upper3 = center_results_d3$upper, upper4 = center_results_d4$upper, upper5 = center_results_d5$upper)
center_results_upper = rowMeans(center_results_upper)
center_results_upper

center_results_lower = data.frame(lower1 = center_results_d1$lower, lower2 = center_results_d2$lower, lower3 = center_results_d3$lower, lower4 = center_results_d4$lower, lower5 = center_results_d5$lower)
center_results_lower = rowMeans(center_results_lower)
center_results_lower

center_results_t_test = data.frame(t_stat = center_results_t_stat, p_value = center_results_p_value, upper = center_results_upper, lower = center_results_lower)
center_results_t_test = round(center_results_t_test, 2)

outcomes = c("Perceived Burdensomeness", "Thwarted Belongingness", "Personal confidence and hope", "Goal and Success Orientation", "No domination by symptoms", "Comprehensibility", "Footing in the world", "MILQ", "RCS", "Suicidal Ideation", "Resolved plans and preparations")

center_results_t_test = data.frame(outcomes, center_results_t_test)

center_results_t_test$outcomes = ifelse(center_results_t_test$upper > 0 & center_results_t_test$lower < 0, center_results_t_test$outcomes, paste0(center_results_t_test$outcomes, "*"))
center_results_t_test

center_results_t_test$ci_95 = paste0(center_results_t_test$lower, sep = ",", center_results_t_test$upper)
center_results_t_test[,4:5] = NULL
center_results_t_test

#center_results_t_test$t_stat = as.numeric(center_results_t_test$t_stat)
#center_results_t_test = center_results_t_test[order(abs(center_results_t_test$t_stat), decreasing = TRUE),]

write.csv(center_results_t_test, "center_results_t_test.csv", row.names = FALSE)

```



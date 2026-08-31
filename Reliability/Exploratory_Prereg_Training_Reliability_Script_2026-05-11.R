#############################################
#### Exploratory Prereg Project ####
# Reliability Script
# Created on May 11, 2026, by Moin Syed
# Checked on DATE, by NAME
#############################################

#### Workspace setup ####

library(dplyr)
library(irr)
library(readxl)
library(summarytools)

sessionInfo()

# R version 4.4.2
# RStudio 2026.08.0 Build 187

# dplyr_1.1.4 
# summarytools_1.1.3
# irr_0.84.1  
# readxl_1.4.5   

set.seed(1978)

#############################################

#### Data import ####

# read in each rater's data, select only ID and ratings (plus phase on first one)

dat_m <- read_xlsx("Exploratory_Prereg_Training_Coding_MS.xlsx")
names(dat_m)
dat_m <- dat_m %>% select(1, 2, 6:14)
names(dat_m)

dat_a <- read_xlsx("Exploratory_Prereg_Training_Coding_AP.xlsx")
names(dat_a)
dat_a <- dat_a %>% select(1, 6:14)
names(dat_a)

dat_c <- read_xlsx("Exploratory_Prereg_Training_Coding_CA.xlsx")
names(dat_c)
dat_c <- dat_c %>% select(1, 6:14)
names(dat_c)

dat_e <- read_xlsx("Exploratory_Prereg_Training_Coding_EC.xlsx")
names(dat_e)
dat_e <- dat_e %>% select(1, 6:14)
names(dat_e)

# join these to one data frame

dat <- dat_m %>% 
  full_join(dat_a, by = "id") %>% 
  full_join(dat_c, by = "id") %>% 
  full_join(dat_e, by = "id") 
names(dat)

#############################################

#### Data preparation ####

# difficulty needs to be converted to numeric to run ICCs

dat <- dat %>% mutate(difficulty_ms_num = recode(difficulty_ms, 
                                                 'very_easy' = 1,
                                                 'somewhat_easy' = 2,
                                                 'somewhat_difficult' = 3,
                                                 'very_difficult' = 4))
dat <- dat %>% mutate(difficulty_ap_num = recode(difficulty_ap, 
                                                 'very_easy' = 1,
                                                 'somewhat_easy' = 2,
                                                 'somewhat_difficult' = 3,
                                                 'very_difficult' = 4))
dat <- dat %>% mutate(difficulty_ca_num = recode(difficulty_ca, 
                                                 'very_easy' = 1,
                                                 'somewhat_easy' = 2,
                                                 'somewhat_difficult' = 3,
                                                 'very_difficult' = 4))
dat <- dat %>% mutate(difficulty_ec_num = recode(difficulty_ec, 
                                                 'very_easy' = 1,
                                                 'somewhat_easy' = 2,
                                                 'somewhat_difficult' = 3,
                                                 'very_difficult' = 4))

dat <- dat %>% mutate(difficulty_ms_num = recode(difficulty_ms, 
                                                 'very_easy' = 1,
                                                 'somewhat_easy' = 2,
                                                 'somewhat_difficult' = 3,
                                                 'very_difficult' = 4),
                      difficulty_ap_num = recode(difficulty_ap, 
                                                 'very_easy' = 1,
                                                 'somewhat_easy' = 2,
                                                 'somewhat_difficult' = 3,
                                                 'very_difficult' = 4),
                      difficulty_ca_num = recode(difficulty_ca, 
                                                 'very_easy' = 1,
                                                 'somewhat_easy' = 2,
                                                 'somewhat_difficult' = 3,
                                                 'very_difficult' = 4),
                      difficulty_ec_num = recode(difficulty_ec, 
                                                 'very_easy' = 1,
                                                 'somewhat_easy' = 2,
                                                 'somewhat_difficult' = 3,
                                                 'very_difficult' = 4))


summarytools::ctable(dat$difficulty_ms, dat$difficulty_ms_num)
summarytools::ctable(dat$difficulty_ap, dat$difficulty_ap_num)
summarytools::ctable(dat$difficulty_ca, dat$difficulty_ca_num)
summarytools::ctable(dat$difficulty_ec, dat$difficulty_ec_num)

# filter for only the phase of interest 
# here doing phase 3 to calculate training reliability

dat <- dat %>% filter(phase == 3)

#############################################

#### Reliability Analysis - Training Phase ####

# loops to calculate average pairwise agreeement and kappa

# title

title_ratings <- dat %>% select(title_ms, title_ap, title_ca, title_ec)
pair <- combn(ncol(title_ratings), 2)
pair_agree <- numeric(ncol(pair))
pair_kappa <- numeric(ncol(pair))

for (i in 1:ncol(pair)) {
  rating_1 <- pair[1, i]
  rating_2 <- pair[2, i]
  
  pair_data <- title_ratings[, c(rating_1, rating_2)]
  
  agree <- agree(pair_data)
  pair_agree[i] <- agree$value
  
  kappa <- kappa2(pair_data)
  pair_kappa[i] <- kappa$value
}

title_agree_mean <- mean(pair_agree)
title_agree_mean

title_kappa_mean <- mean(pair_kappa)
title_kappa_mean
# kappa is NA because of 100% non-presence. will fix value to 1.0
title_kappa_mean <- 1.0


# abstract

abs_ratings <- dat %>% select(abstract_ms, abstract_ap, abstract_ca, abstract_ec)
pair <- combn(ncol(abs_ratings), 2)
pair_agree <- numeric(ncol(pair))
pair_kappa <- numeric(ncol(pair))

for (i in 1:ncol(pair)) {
  rating_1 <- pair[1, i]
  rating_2 <- pair[2, i]
  
  pair_data <- abs_ratings[, c(rating_1, rating_2)]
  
  agree <- agree(pair_data)
  pair_agree[i] <- agree$value
  
  kappa <- kappa2(pair_data)
  pair_kappa[i] <- kappa$value
}

abs_agree_mean <- mean(pair_agree)
abs_agree_mean

abs_kappa_mean <- mean(pair_kappa)
abs_kappa_mean


# introduction

intro_ratings <- dat %>% select(intro_ms, intro_ap, intro_ca, intro_ec)
pair <- combn(ncol(intro_ratings), 2)
pair_agree <- numeric(ncol(pair))
pair_kappa <- numeric(ncol(pair))

for (i in 1:ncol(pair)) {
  rating_1 <- pair[1, i]
  rating_2 <- pair[2, i]
  
  pair_data <- intro_ratings[, c(rating_1, rating_2)]
  
  agree <- agree(pair_data)
  pair_agree[i] <- agree$value
  
  kappa <- kappa2(pair_data)
  pair_kappa[i] <- kappa$value
}

intro_agree_mean <- mean(pair_agree)
intro_agree_mean

intro_kappa_mean <- mean(pair_kappa)
intro_kappa_mean


# method

method_ratings <- dat %>% select(method_ms, method_ap, method_ca, method_ec)
pair <- combn(ncol(method_ratings), 2)
pair_agree <- numeric(ncol(pair))
pair_kappa <- numeric(ncol(pair))

for (i in 1:ncol(pair)) {
  rating_1 <- pair[1, i]
  rating_2 <- pair[2, i]
  
  pair_data <- method_ratings[, c(rating_1, rating_2)]
  
  agree <- agree(pair_data)
  pair_agree[i] <- agree$value
  
  kappa <- kappa2(pair_data)
  pair_kappa[i] <- kappa$value
}

method_agree_mean <- mean(pair_agree)
method_agree_mean

method_kappa_mean <- mean(pair_kappa)
method_kappa_mean


# results

results_ratings <- dat %>% select(results_ms, results_ap, results_ca, results_ec)
pair <- combn(ncol(results_ratings), 2)
pair_agree <- numeric(ncol(pair))
pair_kappa <- numeric(ncol(pair))

for (i in 1:ncol(pair)) {
  rating_1 <- pair[1, i]
  rating_2 <- pair[2, i]
  
  pair_data <- results_ratings[, c(rating_1, rating_2)]
  
  agree <- agree(pair_data)
  pair_agree[i] <- agree$value
  
  kappa <- kappa2(pair_data)
  pair_kappa[i] <- kappa$value
}

results_agree_mean <- mean(pair_agree)
results_agree_mean

results_kappa_mean <- mean(pair_kappa)
results_kappa_mean


# discussion

discussion_ratings <- dat %>% select(discussion_ms, discussion_ap, discussion_ca, discussion_ec)
pair <- combn(ncol(discussion_ratings), 2)
pair_agree <- numeric(ncol(pair))
pair_kappa <- numeric(ncol(pair))

for (i in 1:ncol(pair)) {
  rating_1 <- pair[1, i]
  rating_2 <- pair[2, i]
  
  pair_data <- discussion_ratings[, c(rating_1, rating_2)]
  
  agree <- agree(pair_data)
  pair_agree[i] <- agree$value
  
  kappa <- kappa2(pair_data)
  pair_kappa[i] <- kappa$value
}

discussion_agree_mean <- mean(pair_agree)
discussion_agree_mean

discussion_kappa_mean <- mean(pair_kappa)
discussion_kappa_mean


# headings

heading_ratings <- dat %>% select(heading_ms, heading_ap, heading_ca, heading_ec)
pair <- combn(ncol(heading_ratings), 2)
pair_agree <- numeric(ncol(pair))
pair_kappa <- numeric(ncol(pair))

for (i in 1:ncol(pair)) {
  rating_1 <- pair[1, i]
  rating_2 <- pair[2, i]
  
  pair_data <- heading_ratings[, c(rating_1, rating_2)]
  
  agree <- agree(pair_data)
  pair_agree[i] <- agree$value
  
  kappa <- kappa2(pair_data)
  pair_kappa[i] <- kappa$value
}

heading_agree_mean <- mean(pair_agree)
heading_agree_mean

heading_kappa_mean <- mean(pair_kappa)
heading_kappa_mean


# difficulty

difficulty_ratings <- dat %>% select(difficulty_ms, difficulty_ap, difficulty_ca, difficulty_ec)
pair <- combn(ncol(difficulty_ratings), 2)
pair_agree <- numeric(ncol(pair))
pair_kappa <- numeric(ncol(pair))

for (i in 1:ncol(pair)) {
  rating_1 <- pair[1, i]
  rating_2 <- pair[2, i]
  
  pair_data <- difficulty_ratings[, c(rating_1, rating_2)]
  
  agree <- agree(pair_data)
  pair_agree[i] <- agree$value
  
  kappa <- kappa2(pair_data, weight = "squared")
  pair_kappa[i] <- kappa$value
}

difficulty_agree_mean <- mean(pair_agree)
difficulty_agree_mean

difficulty_kappa_mean <- mean(pair_kappa)
difficulty_kappa_mean

# average agreement across the seven dimensions (not difficulty)

agree_mean <- c(title_agree_mean, abs_agree_mean, intro_agree_mean, method_agree_mean,
                results_agree_mean, discussion_agree_mean, heading_agree_mean)
mean(agree_mean) # 90.71%

kappa_mean <- c(title_kappa_mean, abs_kappa_mean, intro_kappa_mean, method_kappa_mean,
                results_kappa_mean, discussion_kappa_mean, heading_kappa_mean)
mean(kappa_mean) # k = .79

# title is kind of a freebie, still ok if removed?

agree_mean_r1 <- c(abs_agree_mean, intro_agree_mean, method_agree_mean,
                results_agree_mean, discussion_agree_mean, heading_agree_mean)
mean(agree_mean_r1) # 89.17%

kappa_mean_r1 <- c(abs_kappa_mean, intro_kappa_mean, method_kappa_mean,
                results_kappa_mean, discussion_kappa_mean, heading_kappa_mean)
mean(kappa_mean_r1) # k = .76

# heading also a bit of a gimme, what if?

agree_mean_r2 <- c(abs_agree_mean, intro_agree_mean, method_agree_mean,
                   results_agree_mean, discussion_agree_mean)
mean(agree_mean_r2) # 87%

kappa_mean_r2 <- c(abs_kappa_mean, intro_kappa_mean, method_kappa_mean,
                   results_kappa_mean, discussion_kappa_mean)
mean(kappa_mean_r2) # k = .71

#############################################

#### Reliability Analysis - Article Coding Phase ####

# read in each rater's data, select only ID and ratings (plus phase on first one)

dat_mf <- read.csv("../Data/Exploratory-Prereg_Lifecycle_RR_Data_Analysis_2026-08-04.csv")
names(dat_mf)

dat_af <- readxl::read_xlsx("../Reliability/Exploratory-Prereg_Lifecycle_RR_Reliability.xlsx")
names(dat_af)

# strip off some columns not needed

dat_af <- dat_af %>% select(1, 2, 9:17)
names(dat_af)

# join moin's coding to abby's

dat_amf <- left_join(dat_af, dat_mf, by = "Key")

names(dat_amf)

# filter for only the batch of interest, if necessary 

# dat_amf <- dat_amf %>% filter(Batch == 1)

rel_title <- dat_amf %>% dplyr::select(title, title_rel)
rel_abstract <- dat_amf %>% dplyr::select(abstract, abstract_rel)
rel_intro <- dat_amf %>% dplyr::select(intro, intro_rel)
rel_method <- dat_amf %>% dplyr::select(method, method_rel)
rel_results <- dat_amf %>% dplyr::select(results, results_rel)
rel_discussion <- dat_amf %>% dplyr::select(discussion, discussion_rel)
rel_heading <- dat_amf %>% dplyr::select(heading, heading_rel)

agree(rel_title)
kappa2(rel_title)

agree(rel_abstract)
kappa2(rel_abstract)

agree(rel_intro)
kappa2(rel_intro)

agree(rel_method)
kappa2(rel_method)

agree(rel_results)
kappa2(rel_results)

agree(rel_discussion)
kappa2(rel_discussion)

agree(rel_heading)
kappa2(rel_heading)

# percent agreement range from 89-100%
# kappa range from .41 (intro) to 1.0. Other than intro, range is .74-1.0

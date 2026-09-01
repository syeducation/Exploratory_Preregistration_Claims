#############################################
#### Exploratory Prereg Project ####
# Script for Simulating Data and Analysis
# Created on April 27, 2026, by Moin Syed
# Checked on DATE, by NAME
#############################################

#### Workspace setup ####

library(dplyr)
library(pwr)
library(htestClust)

sessionInfo()

# R version 4.4.2
# dplyr_1.1.4  
# pwr_1.3-0 
# htestClust_0.2.2

# setting seed for reproducibility

set.seed(1978)

#############################################

## creating a dataset for each article type
## first id, then journal, then presence of exploratory research, the count

## registered reports

# create id and joural index

dat_rr <- data.frame(id = c(1:100), type = "RR")
head(dat_rr)

dat_rr$journal <- sample(1:47, size = 100, replace = TRUE)
table(dat_rr$journal)

dat_rr$explor_bin <- sample(0:1, size = 100, replace = TRUE, prob = c(.25, .75))
table(dat_rr$explor_bin)

dat_rr$explor_count <- sample(0:6, size = 100, replace = TRUE, prob = c(.25, .20, .25, .20, .05, .025, .025))
table(dat_rr$explor_count)

dat_rr$explor_head <- sample(0:1, size = 100, replace = TRUE, prob = c(.25, .75))
table(dat_rr$explor_head)
                
# preregistered reports

dat_pr <- data.frame(id = c(101:200), type = "PR")
head(dat_pr)

dat_pr$journal <- dat_rr$journal
table(dat_pr$journal)

dat_pr$explor_bin <- sample(0:1, size = 100, replace = TRUE, prob = c(.34, .66))
table(dat_pr$explor_bin)

dat_pr$explor_count <- sample(0:6, size = 100, replace = TRUE, prob = c(.34, .23, .23, .10, .05, .025, .025))
table(dat_pr$explor_count)

dat_pr$explor_head <- sample(0:1, size = 100, replace = TRUE, prob = c(.40, .60))
table(dat_pr$explor_head)

# traditional reports - estimated rate from O'Mahony

dat_tr <- data.frame(id = c(201:300), type = "TR")
head(dat_tr)

dat_tr$journal <- dat_rr$journal
table(dat_tr$journal)

dat_tr$explor_bin <- sample(0:1, size = 100, replace = TRUE, prob = c(.43, .57))
table(dat_tr$explor_bin)

dat_tr$explor_count <- sample(0:6, size = 100, replace = TRUE, prob = c(.43, .30, .27, .025, .025, .025, .025))
table(dat_tr$explor_count)

dat_tr$explor_head <- sample(0:1, size = 100, replace = TRUE, prob = c(.75, .25))
table(dat_tr$explor_head)

## combine these three into one dataset

dat_sim <- rbind(dat_rr, dat_pr, dat_tr)
table(dat_sim$type)


#############################################

## analysis using simulated data

## H1 - prereg to traditional

# Test 1 - binary

dat_h1 <- dat_sim %>% filter(type != "RR")
table(dat_h1$type)

# clustering by journal

t1.1 <- chisqtestClust(dat_h1$type, dat_h1$explor_bin, dat_h1$journal)
t1.1

# no clustering

chisq.test(dat_h1$type, dat_h1$explor_bin)

# Test 2 - count

# clustering by journal

t1.2 <- ttestClust(explor_count ~ type, id = journal, data = dat_h1)
t1.2

# no clustering

t.test(explor_count ~ type, data = dat_h1)

# Test 3 - binary

# clustering by journal

t1.3 <- chisqtestClust(dat_h1$type, dat_h1$explor_head, dat_h1$journal)
t1.3

# no clustering

chisq.test(dat_h1$type, dat_h1$explor_head)

## H2 - RR to traditional

# Test 1 - binary

dat_h2 <- dat_sim %>% filter(type != "PR")
table(dat_h2$type)

# clustering by journal

t2.1 <- chisqtestClust(dat_h2$type, dat_h2$explor_bin, dat_h2$journal)
t2.1

# no clustering

chisq.test(dat_h2$type, dat_h2$explor_bin)

# Test 2 - count

# clustering by journal

t2.2 <- ttestClust(explor_count ~ type, id = journal, data = dat_h2)
t2.2

# no clustering

t.test(explor_count ~ type, data = dat_h2)

# Test 3 - binary

# clustering by journal

t2.3 <- chisqtestClust(dat_h2$type, dat_h2$explor_head, dat_h2$journal)
t2.3

# no clustering

chisq.test(dat_h2$type, dat_h2$explor_head)

## H3 - RR to PR

# Test 1 - binary

dat_h3 <- dat_sim %>% filter(type != "TR")
table(dat_h3$type)

# clustering by journal

t3.1 <- chisqtestClust(dat_h3$type, dat_h3$explor_bin, dat_h3$journal)
t3.1

# no clustering

chisq.test(dat_h3$type, dat_h3$explor_bin)

# Test 2 - count

# clustering by journal

t3.2 <- ttestClust(explor_count ~ type, id = journal, data = dat_h3)
t3.2

# no clustering

t.test(explor_count ~ type, data = dat_h3)

# Test 3 - binary

# clustering by journal

t3.3 <- chisqtestClust(dat_h3$type, dat_h3$explor_head, dat_h3$journal)
t3.3

# no clustering

chisq.test(dat_h3$type, dat_h3$explor_head)

## FDR correction for p-values

# create object of p-values
options(scipen = 999)

p_values <- c(t1.1$p.value, t1.2$p.value, t1.3$p.value,
              t2.1$p.value, t2.2$p.value, t2.3$p.value,
              t3.1$p.value, t3.2$p.value, t3.3$p.value)
p_values

p.adjust(p_values, method = "BH")


############# POWER ANALYSIS

# for 2x2 chi squares, phi = .20

pwr.chisq.test(w = .20, power = 0.8, df = 1, sig.level = 0.05)

# for indepedent t-test, d = .40

pwr.t.test(d = .40, power = .8, sig.level = .05, type = "two.sample")




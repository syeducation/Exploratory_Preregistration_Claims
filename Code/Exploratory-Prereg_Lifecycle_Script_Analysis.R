#############################################
#### Exploratory Prereg Project ####
# Analysis Script
# Created on August 10, 2026, by Moin Syed
# Checked on August 31, by Caroline Armstrong
#############################################

#### Workspace setup ####

library(dplyr)
library(summarytools)
library(htestClust)
library(rcompanion)
library(forcats)
library(ggplot2)
library(cowplot)

sessionInfo()

# R version 4.4.2
# RStudio 2026.08.0 Build 187

# dplyr_1.1.4 
# summarytools_1.1.3
# htestClust_0.2.2   
# rcompanion_2.5.2   
# forcats_1.0.0      
# ggplot2_3.5.1      
# cowplot_1.1.3      

set.seed(1978)

#############################################
#### Data import ####

dat <- read.csv("./Data/Exploratory-Prereg_Lifecycle_RR_Data_Analysis_2026-08-04.csv")

names(dat)

# need a numeric indicator of journal for the clustered comparison of means 

dat <- dat %>% mutate(journal_num = recode(journal, 
                                           'Attention, Perception, & Psychophysics' = 1, 
                                           'British Journal of Educational Psychology' = 2,
                                           'British Journal of Health Psychology' = 3,
                                           'British Journal of Psychology' = 4,
                                           'British Journal of Social Psychology' = 5,
                                           'Cognition and Emotion' = 6,
                                           'Cognitive Research: Principles and Implications' = 7,
                                           'Collabra: Psychology' = 8,
                                           'Consciousness and Cognition' = 9,
                                           'Cortex' = 10,
                                           'Developmental Science' = 11,
                                           'European Journal of Personality' = 12,
                                           'Evolution and Human Behavior' = 13,
                                           'Experimental Psychology' = 14,
                                           'Infant and Child Development' = 15,
                                           'International Journal of Psychophysiology' = 16,
                                           'Journal of Experimental Social Psychology' = 17,
                                           'Journal of Personality and Social Psychology' = 18,
                                           'Journal of Research in Personality' = 19,
                                           'Judgment and Decision Making' = 20,
                                           'Law and Human Behavior' = 21,
                                           'Legal and Criminological Psychology' = 22,
                                           'Memory' = 23,
                                           'Nature Human Behaviour' = 24,
                                           'Psychology & Health' = 25,
                                           'Psychology of Addictive Behaviors' = 26,
                                           'Psychology of Sport and Exercise' = 27,
                                           'Psychonomic Bulletin & Review' = 28,
                                           'Psychophysiology' = 29,
                                           'Quarterly Journal of Experimental Psychology' = 30 ))

#############################################
#### Descriptives ####

# journal distribution, check and then make table

table_journal <- dat %>% filter(type_article == "Registered Report") %>% freq(journal, order = "freq")

table_journal <- as_tibble(table_journal, rownames = "journal")
table_journal

write.csv(table_journal, "./Tables/Raw/table_journal_freqs.csv", row.names = FALSE)

# any presence by articles type

ctable(dat$type_article, dat$explore_bin)

ctable(dat$type_article, dat$title)
ctable(dat$type_article, dat$abstract)
ctable(dat$type_article, dat$intro)
ctable(dat$type_article, dat$method)
ctable(dat$type_article, dat$results)
ctable(dat$type_article, dat$discussion)
ctable(dat$type_article, dat$heading)

# counts by article type

dat %>% filter(type_article == "Registered Report") %>%  descr(explore_count)
dat %>% filter(type_article == "Preregistered Match") %>%  descr(explore_count)
dat %>% filter(type_article == "Traditional Match") %>%  descr(explore_count)
dat %>% filter(type_article == "Traditional 2010") %>%  descr(explore_count)


#############################################
#### H1 - prereg to traditional ####

#### Test 1 - binary

dat_h1 <- dat %>% filter(type_article == "Preregistered Match" |
                           type_article == "Traditional Match")
table(dat_h1$type_article)

# distribution by binary total and for each section

ctable(dat_h1$type_article, dat_h1$explore_bin)

ctable(dat_h1$type_article, dat_h1$title)
ctable(dat_h1$type_article, dat_h1$abstract)
ctable(dat_h1$type_article, dat_h1$intro)
ctable(dat_h1$type_article, dat_h1$method)
ctable(dat_h1$type_article, dat_h1$results)
ctable(dat_h1$type_article, dat_h1$discussion)
ctable(dat_h1$type_article, dat_h1$heading)

# main test, clustered by journal

t1.1 <- chisqtestClust(dat_h1$type_article, dat_h1$explore_bin, dat_h1$journal_num)
t1.1

t1.1.es <- table(dat_h1$type_article, dat_h1$explore_bin)
phi(t1.1.es)

# robust 1: no clustering

chisq.test(dat_h1$type_article, dat_h1$explore_bin)

# robust 2: cluster by matching

chisqtestClust(dat_h1$type_article, dat_h1$explore_bin, dat_h1$index)

#### Test 2 - count

# main test, clustered by journal

t1.2 <- ttestClust(explore_count ~ type_article, id = journal_num, data = dat_h1)
t1.2

t1.2.es <- (2 * t1.2$statistic)/sqrt(200)
t1.2.es

# robust 1: no clustering

t.test(explore_count ~ type_article, data = dat_h1)

# robust 2: cluster by matching

ttestClust(explore_count ~ type_article, id = index, data = dat_h1)

#### Test 3 - heading

# main test, clustered by journal

t1.3 <- chisqtestClust(dat_h1$type_article, dat_h1$heading, dat_h1$journal_num)
t1.3

t1.3.es <- table(dat_h1$type_article, dat_h1$heading)
phi(t1.3.es)

# robust 1: no clustering

chisq.test(dat_h1$type_article, dat_h1$heading)

# robust 2: cluster by matching

chisqtestClust(dat_h1$type_article, dat_h1$heading, dat_h1$index)

#############################################
#### H2 - RR to traditional ####

#### Test 1 - binary

dat_h2 <- dat %>% filter(type_article == "Registered Report" |
                           type_article == "Traditional Match")
table(dat_h2$type_article)

# distribution by binary total and for each section

ctable(dat_h2$type_article, dat_h2$explore_bin)

ctable(dat_h2$type_article, dat_h2$title)
ctable(dat_h2$type_article, dat_h2$abstract)
ctable(dat_h2$type_article, dat_h2$intro)
ctable(dat_h2$type_article, dat_h2$method)
ctable(dat_h2$type_article, dat_h2$results)
ctable(dat_h2$type_article, dat_h2$discussion)
ctable(dat_h2$type_article, dat_h2$heading)

# main test, clustered by journal

t2.1 <- chisqtestClust(dat_h2$type_article, dat_h2$explore_bin, dat_h2$journal_num)
t2.1

t2.1.es <- table(dat_h2$type_article, dat_h2$explore_bin)
phi(t2.1.es)

# robust 1: no clustering

chisq.test(dat_h2$type_article, dat_h2$explore_bin)

# robust 2: cluster by matching

chisqtestClust(dat_h2$type_article, dat_h2$explore_bin, dat_h2$index)

#### Test 2 - count

# main test, clustered by journal

t2.2 <- ttestClust(explore_count ~ type_article, id = journal_num, data = dat_h2)
t2.2

t2.2.es <- (2 * t2.2$statistic)/sqrt(200)
t2.2.es

# robust 1: no clustering

t.test(explore_count ~ type_article, data = dat_h2)

# robust 2: cluster by matching

ttestClust(explore_count ~ type_article, id = index, data = dat_h2)

#### Test 3 - heading

# main test, clustered by journal

t2.3 <- chisqtestClust(dat_h2$type_article, dat_h2$heading, dat_h2$journal_num)
t2.3 

t2.3.es <- table(dat_h2$type_article, dat_h2$heading)
phi(t2.3.es)

# robust 1: no clustering

chisq.test(dat_h2$type_article, dat_h2$heading)

# robust 2: cluster by matching

chisqtestClust(dat_h2$type_article, dat_h2$heading, dat_h2$index)

#############################################
#### H3 - RR to PR ####

#### Test 1 - binary

dat_h3 <- dat %>% filter(type_article == "Registered Report" |
                           type_article == "Preregistered Match")
table(dat_h3$type_article)

# distribution by binary total and for each section

ctable(dat_h3$type_article, dat_h3$explore_bin)

ctable(dat_h3$type_article, dat_h3$title)
ctable(dat_h3$type_article, dat_h3$abstract)
ctable(dat_h3$type_article, dat_h3$intro)
ctable(dat_h3$type_article, dat_h3$method)
ctable(dat_h3$type_article, dat_h3$results)
ctable(dat_h3$type_article, dat_h3$discussion)
ctable(dat_h3$type_article, dat_h3$heading)

# main test, clustered by journal

t3.1 <- chisqtestClust(dat_h3$type_article, dat_h3$explore_bin, dat_h3$journal_num)
t3.1

t3.1.es <- table(dat_h3$type_article, dat_h3$explore_bin)
phi(t3.1.es)

# robust 1: no clustering

chisq.test(dat_h3$type_article, dat_h3$explore_bin)

# robust 2: cluster by matching

chisqtestClust(dat_h3$type_article, dat_h3$explore_bin, dat_h3$index)

#### Test 2 - count

# main test, clustered by journal

t3.2 <- ttestClust(explore_count ~ type_article, id = journal_num, data = dat_h3)
t3.2

t3.2.es <- (2 * t3.2$statistic)/sqrt(200)
t3.2.es

# robust 1: no clustering

t.test(explore_count ~ type_article, data = dat_h3)

# robust 2: cluster by matching

ttestClust(explore_count ~ type_article, id = index, data = dat_h3)

#### Test 3 - heading

# main test, clustered by journal

t3.3 <- chisqtestClust(dat_h3$type_article, dat_h3$heading, dat_h3$journal_num)
t3.3

t3.3.es <- table(dat_h3$type_article, dat_h3$heading)
phi(t3.3.es)

# robust 1: no clustering

chisq.test(dat_h3$type_article, dat_h3$heading)

# robust 2: cluster by matching

chisqtestClust(dat_h3$type_article, dat_h3$heading, dat_h3$index)

#############################################
#### FDR correction for p-values

# create object of p-values
options(scipen = 999)

p_values <- c(t1.1$p.value, t1.2$p.value, t1.3$p.value,
              t2.1$p.value, t2.2$p.value, t2.3$p.value,
              t3.1$p.value, t3.2$p.value, t3.3$p.value)
p_values

p.adjust(p_values, method = "BH")

#############################################
#### Plots ####

# reorder factor for x-axis

dat <- dat %>%  mutate(type_article = fct_relevel(type_article, 
                                                  "Traditional 2010",
                                                  "Traditional Match",
                                                  "Preregistered Match",
                                                  "Registered Report"))

#### binary any

plot_b <- ggplot(dat, aes(x = type_article, fill = explore_bin)) +
  geom_bar(position = "fill") +
  scale_fill_manual(values = c("#c1272d", "#00859A"), labels = c("No", "Yes")) +
  labs(title = "Presence of Exploratory Research: Anywhere in Article",
       x = "Article Type", y = "Proportion") +
  scale_x_discrete(labels = c("Traditional 2010", "Traditional", "Preregistered", "Registered Report")) +
  theme_bw() +
  theme(legend.title = element_blank())
plot_b

ggsave("./Figures/figure_binary_any.png", plot_b)

#### doing a binary plot for each section, then will create multipanel

# binary title (this will be omitted from multipanel)

plot_t <- ggplot(dat, aes(x = type_article, fill = title)) +
  geom_bar(position = "fill") +
  scale_fill_manual(values = c("#c1272d", "#00859A"), labels = c("No", "Yes")) +
  labs(title = "Presence of Exploratory Research: \nTitle",
       x = "Article Type", y = "Proportion") +
  scale_x_discrete(labels = c("TA 2010", "TA", "PR", "RR")) +
  theme_bw() +
  theme(legend.title = element_blank())
plot_t

# binary abstract

plot_a <- ggplot(dat, aes(x = type_article, fill = abstract)) +
  geom_bar(position = "fill") +
  scale_fill_manual(values = c("#c1272d", "#00859A"), labels = c("No", "Yes")) +
  labs(title = "Presence of Exploratory Research: \nAbstract",
       x = "Article Type", y = "Proportion") +
  scale_x_discrete(labels = c("TA 2010", "TA", "PR", "RR")) +
  theme_bw() +
  theme(legend.title = element_blank())
plot_a

# binary intro

plot_i <- ggplot(dat, aes(x = type_article, fill = intro)) +
  geom_bar(position = "fill") +
  scale_fill_manual(values = c("#c1272d", "#00859A"), labels = c("No", "Yes")) +
  labs(title = "Presence of Exploratory Research: \nIntroduction",
       x = "Article Type", y = "Proportion") +
  scale_x_discrete(labels = c("TA 2010", "TA", "PR", "RR")) +
  theme_bw() +
  theme(legend.title = element_blank())
plot_i

# binary method

plot_m <- ggplot(dat, aes(x = type_article, fill = method)) +
  geom_bar(position = "fill") +
  scale_fill_manual(values = c("#c1272d", "#00859A"), labels = c("No", "Yes")) +
  labs(title = "Presence of Exploratory Research: \nMethod",
       x = "Article Type", y = "Proportion") +
  scale_x_discrete(labels = c("TA 2010", "TA", "PR", "RR")) +
  theme_bw() +
  theme(legend.title = element_blank())
plot_m

# binary results

plot_r <- ggplot(dat, aes(x = type_article, fill = results)) +
  geom_bar(position = "fill") +
  scale_fill_manual(values = c("#c1272d", "#00859A"), labels = c("No", "Yes")) +
  labs(title = "Presence of Exploratory Research: \nResults",
       x = "Article Type", y = "Proportion") +
  scale_x_discrete(labels = c("TA 2010", "TA", "PR", "RR")) +
  theme_bw() +
  theme(legend.title = element_blank())
plot_r

# binary discussion

plot_d <- ggplot(dat, aes(x = type_article, fill = discussion)) +
  geom_bar(position = "fill") +
  scale_fill_manual(values = c("#c1272d", "#00859A"), labels = c("No", "Yes")) +
  labs(title = "Presence of Exploratory Research: \nDiscussion",
       x = "Article Type", y = "Proportion") +
  scale_x_discrete(labels = c("TA 2010", "TA", "PR", "RR")) +
  theme_bw() +
  theme(legend.title = element_blank())
plot_d

# binary heading

plot_h <- ggplot(dat, aes(x = type_article, fill = heading)) +
  geom_bar(position = "fill") +
  scale_fill_manual(values = c("#c1272d", "#00859A"), labels = c("No", "Yes")) +
  labs(title = "Headers Indicating \nExploratory Research",
       x = "Article Type", y = "Proportion") +
  scale_x_discrete(labels = c("TA 2010", "TA", "PR", "RR")) +
  theme_bw() +
  theme(legend.title = element_blank())
plot_h

#### multi-panel plot, leave out title

figure_bins <- plot_grid(plot_a, plot_i, plot_m, plot_r, plot_d, plot_h,
                         labels = LETTERS[1:6])
figure_bins

ggsave("./Figures/figure_binary_multi.png", figure_bins)

#### count measure across article types

plot_c <-  ggplot(dat, aes(x = type_article, y = explore_count)) +
  geom_violin(aes(fill = factor(type_article), alpha = .80), show.legend = FALSE) + 
  geom_jitter(width = .05, height = .05, alpha = .25) +
  labs(title = "Prominence of Exploratory Research Across Article Sections",
       x = "Article Type", y = "Count") +
  scale_x_discrete(labels = c("Traditional 2010", "Traditional", "Preregistered", "Registered Report")) +
  theme_bw()
plot_c

ggsave("./Figures/figure_count.png", plot_c)



################################################
#### Exploratory Analyses ####

#### Traditional Match to Traditional 2010 ####

dat_exp <- dat %>% filter(type_article == "Traditional Match" |
                            type_article == "Traditional 2010")
table(dat_exp$type_article)

# distribution by binary total and for each section

ctable(dat_exp$type_article, dat_exp$explore_bin)

ctable(dat_exp$type_article, dat_exp$title)
ctable(dat_exp$type_article, dat_exp$abstract)
ctable(dat_exp$type_article, dat_exp$intro)
ctable(dat_exp$type_article, dat_exp$method)
ctable(dat_exp$type_article, dat_exp$results)
ctable(dat_exp$type_article, dat_exp$discussion)
ctable(dat_exp$type_article, dat_exp$heading)

# count

dat_exp %>% filter(type_article == "Traditional 2010") %>%  descr(explore_count)
dat_exp %>% filter(type_article == "Traditional Match") %>%  descr(explore_count)

#### Change over time for all articles ####

# exclude 2010 articles

dat_change <- dat %>% filter(type_article != "Traditional 2010")

# there is one 2018 in there matched with 2019, bin that for clarity

dat_change <- dat_change %>% mutate(year = case_when(year == 2018 ~
                                                       2019,
                                                     TRUE ~ year))

dat_change$year <- as.factor(dat_change$year)

# binary by year, for each article type

plot_change_2010 <- dat %>% filter(type_article == "Traditional 2010") %>%  
  ggplot(aes(x = type_article, fill = explore_bin)) +
  geom_bar(position = "fill") +
  scale_fill_manual(values = c("#c1272d", "#00859A"), labels = c("No", "Yes")) +
  labs(title = "Change Over Time in Presence of Exploratory Research: \nTraditional Articles 2010",
       x = "2010", y = "Proportion") +
  theme_bw() +
  theme(legend.title = element_blank())
plot_change_2010

plot_change_trad <- dat_change %>% filter(type_article == "Traditional Match") %>%  
  ggplot(aes(x = year, fill = explore_bin)) +
  geom_bar(position = "fill") +
  scale_fill_manual(values = c("#c1272d", "#00859A"), labels = c("No", "Yes")) +
  labs(title = "Change Over Time in Presence of Exploratory Research: \nTraditional Articles",
       x = "Publication Year", y = "Proportion") +
  theme_bw() +
  theme(legend.title = element_blank())
plot_change_trad

plot_change_pr <- dat_change %>% filter(type_article == "Preregistered Match") %>%  
  ggplot(aes(x = year, fill = explore_bin)) +
  geom_bar(position = "fill") +
  scale_fill_manual(values = c("#c1272d", "#00859A"), labels = c("No", "Yes")) +
  labs(title = "Change Over Time in Presence of Exploratory Research: \nPreregistered Articles",
       x = "Publication Year", y = "Proportion") +  theme_bw() +
  theme(legend.title = element_blank())
plot_change_pr

plot_change_rr <- dat_change %>% filter(type_article == "Registered Report") %>%  
  ggplot(aes(x = year, fill = explore_bin)) +
  geom_bar(position = "fill") +
  scale_fill_manual(values = c("#c1272d", "#00859A"), labels = c("No", "Yes")) +
  labs(title = "Change Over Time in Presence of Exploratory Research: \nRegistered Reports",
       x = "Publication Year", y = "Proportion") +
  theme_bw() +
  theme(legend.title = element_blank())
plot_change_rr

plot_change_all <- plot_grid(plot_change_2010, plot_change_trad, plot_change_pr, plot_change_rr,
                             labels = LETTERS[1:4])
plot_change_all

ggsave("./Figures/figure_change_all.png", plot_change_all)


plot_change_count <- ggplot(dat_change, aes(x = year, y = explore_count, 
                                          color = type_article, group = type_article)) +
  geom_smooth(formula = y ~ x, method="lm") +
  geom_jitter(width = .10, height = .10, alpha = .30, size = 3.0) +
  labs(title = "Change Over Time in Prominence of Exploratory Research Across Article Sections",
       x = "Publication Year", y = "Count", color = "Article Type") +
  scale_color_discrete(labels = c("Traditional Match" = "Traditional", 
                                  "Preregistered Match" = "Preregistered", 
                                  "Registered Report" = "Registered Report")) +
  theme_bw() +
  theme(legend.position = "bottom")
plot_change_count

ggsave("./Figures/figure_change_count.png", plot_change_count)

#### Comparison of journals that publish a lot of RRs ####

# frequencies by journal (pipe not working here, so code is clunkier....still works)

dat_cortext <- dat %>% filter(journal == "Cortex")
ctable(dat_cortext$type_article, dat_cortext$explore_bin)

dat_nhb <- dat %>% filter(journal == "Nature Human Behaviour")
ctable(dat_nhb$type_article, dat_nhb$explore_bin)

dat_jesp <- dat %>% filter(journal == "Journal of Experimental Social Psychology")
ctable(dat_jesp$type_article, dat_jesp$explore_bin)

# binary presence for the journal cortex

plot_cortex <- dat %>% filter(journal == "Cortex") %>%  
  ggplot(aes(x = type_article, fill = explore_bin)) +
  geom_bar(position = "fill") +
  scale_fill_manual(values = c("#c1272d", "#00859A"), labels = c("No", "Yes")) +
  labs(title = "Presence of Exploratory Research:\nCortex",
       x = "Article Type", y = "Proportion") +
  scale_x_discrete(labels = c("TA 2010", "TA", "PR", "RR")) +
  theme_bw() +
  theme(legend.title = element_blank())
plot_cortex

# binary presence for the journal nature human behavior

plot_nhb <- dat %>% filter(journal == "Nature Human Behaviour") %>%  
  ggplot(aes(x = type_article, fill = explore_bin)) +
  geom_bar(position = "fill") +
  scale_fill_manual(values = c("#c1272d", "#00859A"), labels = c("No", "Yes")) +
  labs(title = "Presence of Exploratory Research:\nNature Human Behaviour",
       x = "Article Type", y = "Proportion") +
  scale_x_discrete(labels = c("TA", "PR", "RR")) +
  theme_bw() +
  theme(legend.title = element_blank())
plot_nhb

# binary presence for the journal of experimental social psychology

plot_jesp <- dat %>% filter(journal == "Journal of Experimental Social Psychology") %>%  
  ggplot(aes(x = type_article, fill = explore_bin)) +
  geom_bar(position = "fill") +
  scale_fill_manual(values = c("#c1272d", "#00859A"), labels = c("No", "Yes")) +
  labs(title = "Presence of Exploratory Research:\nJournal of Experimental Social Psychology",
       x = "Article Type", y = "Proportion") +
  scale_x_discrete(labels = c("TA 2010", "TA", "PR", "RR")) +
  theme_bw() +
  theme(legend.title = element_blank())
plot_jesp

# binary presence for the all other journals

plot_oth <- dat %>% filter(journal != "Journal of Experimental Social Psychology" &
                           journal != "Nature Human Behaviour" & 
                           journal != "Cortex") %>%  
  ggplot(aes(x = type_article, fill = explore_bin)) +
  geom_bar(position = "fill") +
  scale_fill_manual(values = c("#c1272d", "#00859A"), labels = c("No", "Yes")) +
  labs(title = "Presence of Exploratory Research:\nOther Journals",
       x = "Article Type", y = "Proportion") +
  scale_x_discrete(labels = c("TA 2010", "TA", "PR", "RR")) +
  theme_bw() +
  theme(legend.title = element_blank())
plot_oth

figure_journals <- plot_grid(plot_cortex, plot_nhb, plot_jesp, plot_oth,
                         labels = LETTERS[1:4])
figure_journals

ggsave("./Figures/figure_journal_comparison.png", figure_journals)


#### Examination of Journal Policies ####

dat_policies <- read.csv("./Data/Policies/Exploratory-Prereg_Lifecycle_Journal-Policies.csv")
names(dat_policies)

# rename columns for clarity

dat_policies <- dat_policies %>%  rename(journal = journal.name,
                                         guidance_TR = guidance.specified.for.traditional.articles.,
                                         guidance_PR = guidance.specified.for.preregistered.articles.,
                                         guidance_RR = guidance.specified.for.RRs.)

names(dat_policies)

# filter out for only those that were included in the analysis

dat_policies <- dat_policies %>% filter(included == 1)

# now look at frequency of guidance by article type

dat_guidance <- dat_policies %>% select(guidance_TR: guidance_RR)

freq(dat_guidance)

# what journals are requiring it for all types? (n = 2)

dat_required_all <- dat_policies %>% filter(guidance_TR == "required" &
                                        guidance_PR == "required" &
                                        guidance_RR == "required")
freq(dat_required_all$journal)

# what about just PR and RR? (n = 6; 4 unique)

dat_required_two <- dat_policies %>% filter(guidance_PR == "required" &
                                        guidance_RR == "required")
freq(dat_required_two$journal)

# and just for RRs (n = 20; 14 unique)

dat_required_RR <- dat_policies %>% filter(guidance_RR == "required")
freq(dat_required_RR$journal)

# create a variable indicating required for all, two, one, or none

dat_policies <- dat_policies %>% mutate(required = case_when(guidance_TR == "required" &
                                                               guidance_PR == "required" &
                                                               guidance_RR == "required" ~ 3,
                                                             guidance_TR != "required" &
                                                               guidance_PR == "required" &
                                                               guidance_RR == "required" ~ 2,
                                                             guidance_TR != "required" &
                                                               guidance_PR != "required" &
                                                               guidance_RR == "required" ~ 1,
                                                             guidance_TR != "required" &
                                                               guidance_PR != "required" &
                                                               guidance_RR != "required" ~ 0))

freq(dat_policies$required)

# need to merge this file with coding

freq(dat$journal)
freq(dat_policies$journal)

# need to rename one to match

dat_policies <- dat_policies %>% mutate(journal = case_when(journal == "Attention, Perception, and Psychophysics" ~
                                             "Attention, Perception, & Psychophysics",
                                           TRUE ~ journal))
freq(dat_policies$journal)

# now merge the two together

dat_policies_required <- dat_policies %>% select(journal, required)
names(dat_policies_required)

dat_policies_merged <- full_join(dat, dat_policies_required, by = "journal")

freq(dat_policies_merged$required)

# might need to rerun this, depending on starting point

dat_policies_merged <- dat_policies_merged %>%  mutate(type_article = fct_relevel(type_article, 
                                                  "Traditional 2010",
                                                  "Traditional Match",
                                                  "Preregistered Match",
                                                  "Registered Report"))

# now do separate plots for each of the four

plot_req_all <- dat_policies_merged %>% filter(required == 3) %>%  
  ggplot(aes(x = type_article, fill = explore_bin)) +
  geom_bar(position = "fill") +
  scale_fill_manual(values = c("#c1272d", "#00859A"), labels = c("No", "Yes")) +
  labs(title = "Presence of Exploratory Research:\nDistinctions Required for All Articles (n = 16, k =2)",
       x = "Article Type", y = "Proportion") +
  scale_x_discrete(labels = c("TA 2010", "TA", "PR", "RR")) +
  theme_bw() +
  theme(legend.title = element_blank())
plot_req_all

plot_req_two <- dat_policies_merged %>% filter(required == 2) %>%  
  ggplot(aes(x = type_article, fill = explore_bin)) +
  geom_bar(position = "fill") +
  scale_fill_manual(values = c("#c1272d", "#00859A"), labels = c("No", "Yes")) +
  labs(title = "Presence of Exploratory Research:\nDistinctions Required for Registered Articles (n = 32, k = 4)",
       x = "Article Type", y = "Proportion") +
  scale_x_discrete(labels = c("TA 2010", "TA", "PR", "RR")) +
  theme_bw() +
  theme(legend.title = element_blank())
plot_req_two

plot_req_one <- dat_policies_merged %>% filter(required == 1) %>%  
  ggplot(aes(x = type_article, fill = explore_bin)) +
  geom_bar(position = "fill") +
  scale_fill_manual(values = c("#c1272d", "#00859A"), labels = c("No", "Yes")) +
  labs(title = "Presence of Exploratory Research:\nDistinctions Required for Registered Reports Only (n = 238, k = 14)",
       x = "Article Type", y = "Proportion") +
  scale_x_discrete(labels = c("TA 2010", "TA", "PR", "RR")) +
  theme_bw() +
  theme(legend.title = element_blank())
plot_req_one

plot_req_none <- dat_policies_merged %>% filter(required == 0) %>%  
  ggplot(aes(x = type_article, fill = explore_bin)) +
  geom_bar(position = "fill") +
  scale_fill_manual(values = c("#c1272d", "#00859A"), labels = c("No", "Yes")) +
  labs(title = "Presence of Exploratory Research:\nNo Required Distinctions (n = 96, k = 10)",
       x = "Article Type", y = "Proportion") +
  scale_x_discrete(labels = c("TA 2010", "TA", "PR", "RR")) +
  theme_bw() +
  theme(legend.title = element_blank())
plot_req_none

figure_policies <- plot_grid(plot_req_all, plot_req_two, plot_req_one, plot_req_none,
                             labels = LETTERS[1:4])
figure_policies

ggsave("./Figures/figure_policies_comparison.png", figure_policies)


#### generalizability test: comparing trad articles from journals that publish RR and those that don't  ####

# load data with the previously coded generalizability sample of new articles (N = 30)

dat_gen_1 <- readxl::read_xlsx("./Data/Generalizability/Exploratory-Prereg_Lifecycle_Gen_Coding.xlsx")
names(dat_gen_1)

# create count variable 
# intermediate step of converting to numeric

dat_gen_1_analysis <- dat_gen_1 %>% 
  mutate(across(c(title:discussion), ~ case_match(.x, 
                                                  "yes" ~ 1, 
                                                  "no"  ~ 0,))) %>% 
  mutate(explore_count = rowSums(across(c(title:discussion))))

names(dat_gen_1_analysis)

# and from this, create the binary variable

dat_gen_1_analysis <- dat_gen_1_analysis %>% mutate(explore_bin = if_else(explore_count > 0, "yes", "no"))

# check all is ok

freq(dat_gen_1_analysis$explore_bin)
freq(dat_gen_1_analysis$explore_count)
dat_gen_1_analysis %>% group_by(explore_bin) %>% summarise(mean = mean(explore_count))

# load data from primary that they were matched to

dat_gen_2 <- read.csv("./Data/Generalizability/Exploratory-Prereg_Lifecycle_RR_Zotero-Export_Gen-For-Matching_2026-08-19.csv")
names(dat_gen_2)

# just keep the key, index and add indicator

dat_gen_2 <- dat_gen_2[,c(1, 23)]
names(dat_gen_2)

dat_gen_2 <- dat_gen_2 %>%  rename(index = Series)

dat_gen_2 <- dat_gen_2 %>% mutate(gen_sample = 0)
names(dat_gen_2)
head(dat_gen_2)

# now merge in coding from main file 

names(dat)

dat_gen_2_codes <- dat %>% select(Key, type_article:difficulty, explore_count, explore_bin)

dat_gen_2_merged <- left_join(dat_gen_2, dat_gen_2_codes, by = "Key") 
names(dat_gen_2_merged)

# some basic descriptive comparisons

freq(dat_gen_1_analysis$explore_bin)
freq(dat_gen_2_merged$explore_bin)

descr(dat_gen_1_analysis$explore_count)
descr(dat_gen_2_merged$explore_count)



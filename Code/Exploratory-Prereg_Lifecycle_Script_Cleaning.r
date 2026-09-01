#############################################
#### Exploratory Prereg Project ####
# Cleaning Script
# Created on July 30, 2026, by Moin Syed
# Checked on August 31, by Caroline Armstrong
#############################################

#### Workspace setup ####


library(dplyr)
library(tidyr)
library(summarytools) # if using mac, requires install of xquartz (easy)
library(stringr)

sessionInfo()

# R version 4.4.2
# RStudio 2026.08.0 Build 187

# dplyr_1.1.4 
# tidyr_1.3.1
# summarytools_1.1.3
# stringr_1.5.1 

set.seed(1978)

#############################################
#### Data import ####

dat_new <- read.csv("../Data/Exploratory-Prereg_Lifecycle_RR-New-List.csv")
names(dat_new)

# add variable "source"

dat_new <- dat_new %>% mutate(source = "new")
names(dat_new)
head(dat_new)

dat_liu <- read.csv("../Data/Exploratory-Prereg_Lifecycle_RR_Liu_Full-List.csv")
names(dat_liu)

# add variable "source"

dat_liu <- dat_liu %>% mutate(source = "liu")
names(dat_liu)
head(dat_liu)

#############################################
#### Data preparation ####

# merge two datasets together

str(dat_new)
str(dat_liu)

# need to change id in liu data to character before joining

dat_liu$id <- as.character(dat_liu$id)

dat_full <- full_join(dat_new, dat_liu)
names(dat_full)

# look at inclusion numbers and reasons
# N = 188 for sampling

freq(dat_full$include_final)
freq(dat_full$no_reason)

# now reduce to only those that meet inclusion

dat_included <- dat_full %>% filter(include_final == "yes")
freq(dat_included$include_final)

# look at journals, need to convert text first

dat_included$journal <- str_to_title(dat_included$journal)

freq(dat_included$journal)

# remove AMPPS (n = 6) because matches are not really possible given that it is a methods journal

dat_included <- dat_included %>% filter(
  journal != "Advances In Methods And Practices In Psychological Science")

freq(dat_included$journal)

# final N = 182 for sampling
# randomly select 100 from remaining

dat_final <- dat_included[sample(nrow(dat_included), 100), ]

# remove some vars, reorder, and then save this out as working data

names(dat_final)

dat_final <- dat_final[,-c(7:12, 16)]
names(dat_final)

dat_final <- dat_final[,c(1, 9, 2:8)]
names(dat_final)

# write.csv(dat_final, "Exploratory-Prereg_Lifecycle_RR_Data.csv", row.names = FALSE)

# random sampling was supposed to be reproducible
# looks like something got messed up and the random sampling is not reproducible!
# not sure what went wrong there, but this was discovered after all the matches were identified
# so it goes

# n = 8 articles were found ineligible after sampling for various reasons
# need to remove those 8 and resample from the 82 not selected in initial round

# reading back in the data file so that I can resample from those not included

dat_first_run <- read.csv("Exploratory-Prereg_Lifecycle_RR_Data.csv")
head(dat_first_run)

# add a constant indicating that these were included in first run
# then merge back the 82 articles that were not selected

dat_first_run <- dat_first_run %>% mutate(selection = 1)
dat_first_run$selection
names(dat_first_run)

dat_first_run <- dat_first_run[, c(1,10)]
names(dat_first_run)

dat_rejoin <- left_join(dat_included, dat_first_run, by = "id")
names(dat_rejoin)

dat_rejoin$selection[is.na(dat_rejoin$selection)] <- 2
table(dat_rejoin$selection)

# now filter out those already selected

dat_remainder <- dat_rejoin %>% filter(selection == 2)
table(dat_remainder$selection)

# and randomly select 8 articles
# set seed in vain attempt at reproducibility

set.seed(1901)

dat_extra <- dat_remainder[sample(nrow(dat_remainder), 8), ]

# check based on title that these look ok - yep

dat_extra$title

# reorder etc.

names(dat_extra)

dat_extra <- dat_extra[,-c(7:12, 16)]
names(dat_extra)

dat_extra <- dat_extra[,c(1, 9, 2:8, 10)]
names(dat_extra)

# not writing this out because it may not be reproducible
# instead reas in resultant file in next section
# write.csv(dat_extra, "Exploratory-Prereg_Lifecycle_RR_Selection-2_Data.csv", row.names = FALSE)

#############################################
#### Reliability ####

# reading in final article set and selecting set for reliability coding

dat_zot <- read.csv("../Data/Exploratory-Prereg_Lifecycle_RR_Zotero-Export_2026-07-17.csv")
names(dat_zot)

dat_zot <- dat_zot[,c(1, 3:6, 9, 10, 23, 40)]
names(dat_zot)

freq(dat_zot$Manual.Tags)
freq(dat_zot$Series)

# randomly select 80 for reliability coding

set.seed(1992)

dat_reliability <- dat_zot[sample(nrow(dat_zot), 80), ]

# add an indicator variable

dat_reliability <- dat_reliability %>% mutate(rel_sample = 1)
names(dat_reliability)
head(dat_reliability)

dat_reliability <- dat_reliability[,c(1:7)]
names(dat_reliability)

dat_reliability <- dat_reliability %>% mutate(title_rel = NA,
                                              abstract_rel = NA,
                                              intro_rel = NA,
                                              method_rel = NA,
                                              results_rel = NA,
                                              discussion_rel = NA,
                                              heading_rel = NA,
                                              difficulty_rel = NA,
                                              notes_rel = NA)
names(dat_reliability)
head(dat_reliability)

# again, not writing out because of uncertainities of reproducibility 
# write.csv(dat_reliability, "Exploratory-Prereg_Lifecycle_RR_Reliability.csv", row.names = FALSE)

#############################################
#### Coding ####

# and, finally, write out a datafile for coding

dat_coding <- dat_zot %>% mutate(title_rel = NA,
                                              abstract = NA,
                                              intro = NA,
                                              method = NA,
                                              results = NA,
                                              discussion = NA,
                                              heading = NA,
                                              difficulty = NA,
                                              notes = NA)
names(dat_coding)
head(dat_coding)

dat_coding <- dat_coding[,c(1:7, 10:18)]
names(dat_coding)

dat_coding <- dat_coding %>% arrange(Key)

# write.csv(dat_coding, "Exploratory-Prereg_Lifecycle_RR_Coding.csv", row.names = FALSE)

###########################################

## coding is finished, but found some articles that were ineligible. 
## need to merge file with replacements, then delete the ineligibles
## THEN I can read in the coded data 

# first the original Zotero export

dat_z1 <- read.csv("Exploratory-Prereg_Lifecycle_RR_Zotero-Export_2026-07-17.csv")
names(dat_z1)

# now the update

dat_z2 <- read.csv("Exploratory-Prereg_Lifecycle_RR_Zotero-Export_2026-08-04.csv")
names(dat_z2)

# merge together

dat_z_merge <- rbind(dat_z1, dat_z2)
names(dat_z_merge)

# remove the 7 articles that were not eligible

dat_z_reduced <- subset(dat_z_merge, Key != c("PP943BLA", "9FDT5HPG", "VBVMN323", "FYCXAAIM",
                                                "HFB9KKL4", "C7Y99I4H", "D7CLHN8V"))


dat_z_reduced <- dat_z_merge %>% filter(Key != "PP943BLA" 
                                        & Key != "9FDT5HPG"
                                        & Key != "VBVMN323"
                                          & Key != "FYCXAAIM"
                                          & Key != "HFB9KKL4"
                                          & Key != "C7Y99I4H"
                                          & Key != "D7CLHN8V")
                        
# write this one out, it is actual final full record

# write.csv(dat_z_reduced, "Exploratory-Prereg_Lifecycle_RR_Data_Full_Records_2026-08-04.csv", row.names = FALSE)

#############################################
#### Final Data Set ####

# read in coded data

dat_coded <- readxl::read_xlsx("Exploratory-Prereg_Lifecycle_RR_Coding.xlsx")

# get rid of the 7 not eligible

dat_coded_reduced <- dat_coded %>% filter(Key != "PP943BLA" 
                                        & Key != "9FDT5HPG"
                                        & Key != "VBVMN323"
                                        & Key != "FYCXAAIM"
                                        & Key != "HFB9KKL4"
                                        & Key != "C7Y99I4H"
                                        & Key != "D7CLHN8V")

# join the full record with the coded data

# first simplify coded data file
names(dat_coded_reduced)

dat_coded_reduced <- dat_coded_reduced %>% select(Key, title_rel:notes)
names(dat_coded_reduced)

dat_joined <- left_join(dat_z_reduced, dat_coded_reduced, by = "Key")
names(dat_joined)

# reduce data file to variables of interest

dat_joined_reduced <- dat_joined %>% select(Key, Publication.Year:Publication.Title, DOI, Url, 
                                            Series, Manual.Tags, title_rel:notes)
names(dat_joined_reduced)

# rename variables for clarity 

dat_joined_reduced <- dat_joined_reduced %>%  rename(year = Publication.Year,
                                                     journal = Publication.Title,
                                                     author = Author,
                                                     title_article = Title,
                                                     doi = DOI,
                                                     url = Url,
                                                     index = Series,
                                                     type_article = Manual.Tags,
                                                     title = title_rel)
names(dat_joined_reduced)
  
# create count variable 
# intermediate step of converting to numeric

dat_analysis <- dat_joined_reduced %>% 
  mutate(across(c(title:discussion), ~ case_match(.x, 
                                                  "yes" ~ 1, 
                                                  "no"  ~ 0,))) %>% 
  mutate(explore_count = rowSums(across(c(title:discussion))))

names(dat_analysis)

descr(dat_analysis$explore_count)

# and from this, create the binary variable

dat_analysis <- dat_analysis %>% mutate(explore_bin = if_else(explore_count > 0, "yes", "no"))

# check all is ok

dat_analysis %>% group_by(explore_bin) %>% summarise(mean = mean(explore_count))

# and actually now recode back to yes/no

dat_analysis <- dat_joined_reduced %>% 
  mutate(across(c(title:discussion), ~ case_match(.x, 
                                                  1 ~ "yes", 
                                                  0  ~ "no")))
# write this out, final analysis file!

write.csv(dat_analysis, "Exploratory-Prereg_Lifecycle_RR_Data_Analysis_2026-08-04.csv", row.names = FALSE)

###############################################
##############################################

# We're back!
# Prepping the generalizability dataset (n = 30 articles)

# read in the articles that were matched

dat_gen <- read.csv("../Data/Generalizability/Exploratory-Prereg_Lifecycle_RR_Zotero-Export_Gen-Matched_2026-08-19.csv")
names(dat_gen)

dat_gen <- dat_gen[,c(1, 3:6, 9, 10, 23, 40)]
names(dat_gen)

# add indicator

dat_gen <- dat_gen %>% mutate(gen_sample = 1)
names(dat_gen)
head(dat_gen)

# add columns for codes

dat_gen <- dat_gen %>% mutate(title = NA,
                              abstract = NA,
                              intro = NA,
                              method = NA,
                              results = NA,
                              discussion = NA,
                              heading = NA,
                              difficulty = NA,
                              notes = NA)

# rename columns for clarity

dat_gen <- dat_gen %>%  rename(year = Publication.Year,
                               journal = Publication.Title,
                               author = Author,
                               title_article = Title,
                               doi = DOI,
                               url = Url,
                               index = Series,
                               type_article = Manual.Tags)
names(dat_gen)

# save this out for coding

# write.csv(dat_gen, "../Data/Generalizability/Exploratory-Prereg_Lifecycle_Gen_Coding.csv", row.names = FALSE)







#### Exploratory Preregistration Project ####
# Script for Selecting Initial Set of RRs
# Created on 2025-11-22, by Moin Syed
# Major updates on DATE, by NAME (if relevant)
# Checked on DATE, by NAME

#############################################

#### Workspace setup ####

library(groundhog)
groundhog.library(dplyr, "2025-04-01")
groundhog.library(readxl,"2025-04-01")

#############################################

#### Data import ####

# read in data from Liu et al. 

dat <- readxl::read_xlsx("Codebook_for_data_extraction_fin.xlsx")
head(dat)

#############################################

#### Data preparation ####

# file has two headers, remove the first one

dat <- dat[-1,]
head(dat)
names(dat)

# only need article name and type

dat <- dat %>% select(Article_Name, Article_Type)
names(dat)

table(dat$Article_Type)

# one article was retracted, must remove this by name (there is no column for it)

dat <- dat %>% filter(Article_Name != "Event-level risk for negative alcohol consequences in emerging adults: The role of affect, motivation, and context.")

table(dat$Article_Type)

# now only keep the RRs

dat <- dat %>% filter(Article_Type == "RR")

table(dat$Article_Type)

# this is the final set, read out for later use

write.csv(dat, "Procedure/Exploratory-Prereg_Lifecycle_RR-Article-List.csv", row.names = FALSE)




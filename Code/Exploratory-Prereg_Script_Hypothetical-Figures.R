#### Exploratory Preregistration Project ####
# Script for Generating Hypothetical Figures
# Created on 2025-11-28, by Moin Syed
# Major updates on DATE, by NAME (if relevant)
# Checked on DATE, by NAME

########################################################################

#### Workspace setup ####

library(groundhog)
groundhog.library(ggplot2, "2025-04-01")
groundhog.library(cowplot,"2025-04-01")

####### Figure 1a #####################################################

# create data frame for results based on arguments in the literature
# assume N = 100 in each group
# very low rates of exploration in registered studies
# assume a 50/50 rate in traditional reports


dat_1 <- data.frame(type = c("SR", "SR", "RR", "RR"),
                    explore = c("No", "Yes", "No", "Yes"),
                    frequency = c(50, 50, 90, 10))

# plot data for paper, to be Figure 1b

fig_1a <- ggplot(dat_1, aes(x = type, y = frequency, fill = explore)) +
  geom_bar(stat = "identity") +
  labs(title = "If Registration Restricts Exploratory Research",
       x = "Article Type",
       y = "Frequency") +
  scale_x_discrete(labels = c("Registered Articles", "Traditional Articles")) +
  scale_fill_manual(name = "Exporatory Research", 
                    values = c("#c1272d", "#00859A")) +
  theme_classic()
fig_1a

####### Figure 1b #####################################################

# create data frame for results based on O'Mahony (2023)
# assume N = 100 in each group
# 57% exploratory in standard reports
# 75% explortory in registered reports


dat_2 <- data.frame(type = c("SR", "SR", "RR", "RR"),
                           explore = c("No", "Yes", "No", "Yes"),
                           frequency = c(43, 57, 25, 75))


# plot data for paper, to be Figure 1b

fig_1b <- ggplot(dat_2, aes(x = type, y = frequency, fill = explore)) +
  geom_bar(stat = "identity") +
  labs(title = "If Registration Facilitates Exploratory Research",
       x = "Article Type",
       y = "Frequency") +
  scale_x_discrete(labels = c("Registered Articles", "Traditional Articles")) +
  scale_fill_manual(name = "Exporatory Research", 
                    values = c("#c1272d", "#00859A")) +
  theme_classic()
fig_1b

####### Figure 1c #####################################################

# create data frame suggesting that exploratory research increases in traditional articles
# assume N = 100 in each group

dat_3 <- data.frame(year = c("2010", "2012", "2014", "2016",
                             "2018", "2020", "2022", "2024"),
                    frequency = c(30, 30, 35, 40,
                                  45, 50, 55, 58))

# plot data for paper, to be Figure 1c

fig_1c <- ggplot(dat_3, aes(x = year, y = frequency)) +
  geom_point(stat = "identity", size = 3)  +
  labs(title = "If Exploratory Research Increases in Traditional Articles",
       x = "Year",
       y = "Frequency") +
  ylim(0,100) +
  theme_classic()
fig_1c


####### Figure 1d #####################################################

# create data frame suggesting that exploratory research increases in registered articles
# assume N = 100 in each group

dat_4 <- data.frame(year = c("2012", "2014", "2016",
                             "2018", "2020", "2022", "2024"),
                    frequency = c(15, 25, 50,
                                  70, 70, 75, 75))

# plot data for paper, to be Figure 1d

fig_1d <- ggplot(dat_4, aes(x = year, y = frequency)) +
  geom_point(stat = "identity", size = 3)  +
  labs(title = "If Exploratory Research Increases in Registered Articles",
       x = "Year",
       y = "Frequency") +
  ylim(0,100) +
  theme_classic()
fig_1d

####### Combined Figure for Paper ######################################

fig_all <- cowplot::plot_grid(fig_1a, fig_1b, fig_1c, fig_1d, 
                              labels = LETTERS[1:4])
fig_all

# save this out 

ggsave("Exploratory-Prereg_Figure1_Proposal.png", fig_all)



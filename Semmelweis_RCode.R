# R Code

# Data & libraries --------------------------------------------------------

library(tidyverse)
setwd("~/Dr.-Semmelweis-and-the-Discovery-of-Handwashing")
yearly <- read.csv('datasets/yearly_deaths.csv')

# Exploratory Analysis ----------------------------------------------------

yearly <- read.csv('datasets/yearly_deaths.csv')
# Proportion of deaths at clinic 1 vs clinic 2
yearly <- yearly %>% mutate(prop_deaths = deaths/births)
# Plot proportions
ggplot(yearly, aes(x=year, y=prop_deaths, color=clinic)) + geom_line() +
  xlab("Year") + ylab('Proportion of Deaths per Birth')
# Clinic 1 has a much higher rate than clinic 2- why?


# Examining Clinic 1 Data -------------------------------------------------

# Monthly data from clinic 1
monthly <- read_csv("datasets/monthly_deaths.csv")
# Proportion of deaths vs births per month
monthly<- monthly %>%
  mutate(prop_deaths = deaths/births)
# Hand washing decree (date Semmelweis told everyone to wash their hands)
decree <- as.Date('1847-06-01')
# Plot proportion of deaths, red line is when he told everyone to wash their hands
ggplot(monthly, aes(x=date, y=prop_deaths)) +
  geom_line() + geom_vline(xintercept = decree, color='red') +
  xlab("Year") + ylab('Proportion of Deaths per Birth')

# More Hand Washing, Fewer Deaths? ----------------------------------------

# Assign each month with value of id the decree had been given or not
monthly <- monthly %>% 
  mutate(handwashing_started = ifelse(date >= decree, TRUE, FALSE))
# Another plot to visualize this
ggplot(monthly, aes(x = date, y = prop_deaths,
                    colour = handwashing_started )) + geom_line()
# Summarize proportions of deaths before/after decree
monthly %>%
  group_by(handwashing_started) %>%
  summarize(mean_prop_deaths = mean(prop_deaths))

# Statistical Analysis ----------------------------------------------------

t.test(prop_deaths ~ handwashing_started, data = monthly)
# There is a significant difference in proportion of deaths when hands are washed vs when they are not

library(tidyverse)
library(magrittr)
library(ipumsr)
library(modelsummary)
library(broom)
library(car)
library(fixest)
library(estimatr)
library(haven)

# load in R data
load("../Data/acs.rda")
df %<>% select(serial,pernum,year,educvar,loginc,incwage,logincnodef,cpi)

# load in Stata data
stata <- read_dta("../Data/acsM_check.dta") %>% 
         select(serial,pernum,year,stata.incwage=incwage,stata.educvar=educvar,stata.loginc=loginc,stata.logincnodef=logincnodef,stata.loginc2=loginc2)
stata %<>% mutate(stata.educvar = as_factor(stata.educvar))

# join together
together <- left_join(df,stata, by=c("serial","pernum","year"))

table(together$educvar,together$stata.educvar) %>% print
summary(together$incwage-together$stata.incwage) %>% print
summary(together$loginc-together$stata.loginc2) %>% print
summary(together$logincnodef-together$stata.logincnodef) %>% print

summary(lm(loginc ~ educvar, data=df)) %>% print



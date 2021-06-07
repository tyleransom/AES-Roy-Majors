library(plyr)         # cross-tabulation
library(e1071)        # more machine learning functions
library(partykit)     # for conditional inference tree function
library(tidyverse)    # data manipulation and plots
library(Hmisc)        # to format data frame as LaTeX table
library(data.table)   # data table
library(magrittr)     # piping
library(modelsummary) # awesome summary stats and regression output
library(broom)        # handling regression output
library(car)          # joint hypothesis testing
library(fixest)       # fast fixed effects
library(estimatr)     # robust standard errors
library(patchwork)    # combine multiple ggplots together
library(future)       # for parallelization
library(furrr)        # for parallelization
library(ggrepel)      # for better-looking ggplot
library(purrr)        # for mapping
library(abind)        # for N-D arrays

# load in data
load("../Data/acs19.rda")

# seed reproducibility
RNGkind(sample.kind = "Rounding")

#--------------------------------------------
# Overall summary table of estimation sample
#--------------------------------------------
df %<>% mutate(spbornHere     = as.numeric(spbornHere),
               liveWithFamily = as.numeric(liveWithFamily),
               spwork         = as.numeric(spwork),
               childlt5       = as.numeric(childlt5),
               childlt18      = as.numeric(childlt18),
               married        = as.numeric(married),
               birthmig       = as.numeric(birthmig),
               relocc         = as.numeric(relocc)
              )

Count <- function(x) sum(!is.na(x))
datasummary(incwage + loginc + birthmig + relocc + meanStay + residMeanStay + meanRelOcc + meanAggRelOcc + residMeanRelOcc + spbornHere + liveWithFamily + spwork + childlt5 + childlt18 + married ~ Count + Mean + SD + Median + Min + Max, 
             data = df %>% select(incwage,loginc,birthmig,relocc,meanStay,residMeanStay,meanRelOcc,meanAggRelOcc,residMeanRelOcc,spbornHere,liveWithFamily,spwork,childlt5,childlt18,married),
             output = "markdown") %>% print
datasummary(incwage + loginc + birthmig + relocc  ~ educvar * (Count + Mean + SD), 
             data = df %>% select(incwage,loginc,birthmig,relocc,educvar),
             output = "markdown") %>% print

df %<>% mutate(spbornHere     = as.factor(as.logical(spbornHere)),
               liveWithFamily = as.factor(as.logical(liveWithFamily)),
               spwork         = as.factor(as.logical(spwork)),
               married        = as.factor(as.logical(married)),
               childlt5       = as.factor(as.logical(childlt5)),
               childlt18      = as.factor(as.logical(childlt18))
              )
#table(df$spbornHere) %>% print
#table(df$liveWithFamily) %>% print
#table(df$spwork) %>% print
#table(df$childlt5) %>% print
#table(df$childlt18) %>% print

#--------------------------------------------
# basic outcome regressions / LPMs
#--------------------------------------------
source('basic_regressions.R')


#--------------------------------------------
# Table 1
#--------------------------------------------
source('table1.R')
source('table1unwgt.R')
print('Finished with Table 1')


#--------------------------------------------
# bar graphs of transition matrics
#--------------------------------------------
source('fig_transmat.R')
print('Finished with Transition Matrices')


#--------------------------------------------
# bar graphs & tables of occupation distribution
#--------------------------------------------
source('occDistFigsTables.R')
print('Finished with Occupation Distribution Figures & Tables')


#--------------------------------------------
# Example conditional inference tree
#--------------------------------------------
source('tree_example.R')
print('Finished with Example Tree Figure')


#--------------------------------------------
# Table 2 (and appendix table w/accuracies)
#--------------------------------------------
source('table2.R')
print('Finished with Tree Estimation')
save(df,file="afterT2.rda")
source('table2logitBin.R')
print('Finished with Logit and Bin Estimation')


#--------------------------------------------
# Table 3 and Table 4
#--------------------------------------------
source('table3splitBoot.R')
print('Finished with Bootstrapped Wage Estimation')
source('table3and4results.R')



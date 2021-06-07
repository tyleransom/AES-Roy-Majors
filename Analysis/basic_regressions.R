
#--------------------------------------------
# LPM of staying and working in related occ
# want to test exclusion restrictions
#--------------------------------------------
est1 <- lm(birthmig ~                              liveWithFamily + spwork + childlt5 + childlt18, data=df) #, subset=(educvar>3 & !is.na(loginc)))
est2 <- lm(birthmig ~                 spbornHere + liveWithFamily + spwork + childlt5 + childlt18, data=df) #, subset=(educvar>3 & !is.na(loginc)))
est3 <- lm(birthmig ~ meanStay                                                                   , data=df) #, subset=(educvar>3 & !is.na(loginc)))
est4 <- lm(birthmig ~ residMeanStay                                                              , data=df) #, subset=(educvar>3 & !is.na(loginc)))
est5 <- lm(birthmig ~ residMeanStay + spbornHere + liveWithFamily + spwork + childlt5 + childlt18, data=df) #, subset=(educvar>3 & !is.na(loginc)))
modelsummary(list(est1,est2,est3,est4,est5),stars=T,output="markdown") %>% print

linearHypothesis(est5,c("residMeanStay")) %>% print
linearHypothesis(est5,c("spbornHereTRUE","liveWithFamilyTRUE","spworkTRUE","childlt5TRUE","childlt18TRUE")) %>% print

est11 <- lm(relocc ~                                liveWithFamily + spwork + childlt5 + childlt18, data=df) #, subset=(educvar>3 & !is.na(loginc)))
est12 <- lm(relocc ~                   spbornHere + liveWithFamily + spwork + childlt5 + childlt18, data=df) #, subset=(educvar>3 & !is.na(loginc)))
est13 <- lm(relocc ~ meanRelOcc                                                                   , data=df) #, subset=(educvar>3 & !is.na(loginc)))
est14 <- lm(relocc ~ meanAggRelOcc                                                                , data=df) #, subset=(educvar>3 & !is.na(loginc)))
est15 <- lm(as.numeric(relocc) ~ residMeanRelOcc                                                              , data=df) #, subset=(educvar>3 & !is.na(loginc)))
est16 <- lm(as.numeric(relocc) ~ residMeanRelOcc + spbornHere + liveWithFamily + spwork + childlt5 + childlt18, data=df) #, subset=(educvar>3 & !is.na(loginc)))
modelsummary(list(est11,est12,est13,est14,est15,est16),stars=T,output="markdown") %>% print

linearHypothesis(est16,c("residMeanRelOcc")) %>% print
linearHypothesis(est16,c("spbornHereTRUE","liveWithFamilyTRUE","spworkTRUE","childlt5TRUE","childlt18TRUE")) %>% print


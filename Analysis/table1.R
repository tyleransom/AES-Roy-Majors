#--------------------------------------------
# National outcome regressions
#--------------------------------------------
est1 <- feols(loginc    ~ educvar + advdegree + exper + I(exper^2) + I(exper^3) + racer + married | met2013, data=df, weights=df %>% `$`(perwt))
est2 <- feols(birthmig  ~ educvar + advdegree + exper + I(exper^2) + I(exper^3) + racer + married | met2013, data=df, weights=df %>% `$`(perwt))
est3 <- feols(dist      ~ educvar + advdegree + exper + I(exper^2) + I(exper^3) + racer + married | met2013, data=df %>% filter(birthState!=statefip), weights=df %>% filter(birthState!=statefip) %>% `$`(perwt))
est4 <- feols(relocc    ~ educvar + advdegree + exper + I(exper^2) + I(exper^3) + racer + married | met2013, data=df, weights=df %>% `$`(perwt))
est5 <- feols(advdegree ~ educvar             + exper + I(exper^2) + I(exper^3) + racer + married | met2013, data=df, weights=df %>% `$`(perwt))
modelsummary(list("loginc"=est1,"birthmig"=est2,"dist"=est3,"relocc"=est4,"advdegree"=est5), stars=T, output="markdown") %>% print

#--------------------------------------------
# State-level outcome regressions
#--------------------------------------------
outs <- tibble()
for (f in levels(df$statefip)) {
    wage.est <- feols(loginc   ~ educvar + advdegree + exper + I(exper^2) + I(exper^3) + racer + married | met2013, data=df %>% filter(statefip==f), weights=df %>% filter(statefip==f) %>% `$`(perwt))
    migr.est <- feols(birthmig ~ educvar + advdegree + exper + I(exper^2) + I(exper^3) + racer + married | met2013, data=df %>% filter(statefip==f), weights=df %>% filter(statefip==f) %>% `$`(perwt))
    relo.est <- feols(relocc   ~ educvar + advdegree + exper + I(exper^2) + I(exper^3) + racer + married | met2013, data=df %>% filter(statefip==f), weights=df %>% filter(statefip==f) %>% `$`(perwt))
    w.ro.est <- feols(loginc   ~ educvar + advdegree + exper + I(exper^2) + I(exper^3) + racer + married | met2013, data=df %>% filter(statefip==f & relocc==1), weights=df %>% filter(statefip==f & relocc==1) %>% `$`(perwt))
    w.uo.est <- feols(loginc   ~ educvar + advdegree + exper + I(exper^2) + I(exper^3) + racer + married | met2013, data=df %>% filter(statefip==f & relocc==0), weights=df %>% filter(statefip==f & relocc==0) %>% `$`(perwt))
    temp <- tibble(
                   state     = f,
                   w.ed      = 0, 
                   w.ss      = wage.est %>% `$`(coeftable) %>% `$`(Estimate) %>% `[`(1),
                   w.oth     = wage.est %>% `$`(coeftable) %>% `$`(Estimate) %>% `[`(2),
                   w.bus     = wage.est %>% `$`(coeftable) %>% `$`(Estimate) %>% `[`(3),
                   w.stem    = wage.est %>% `$`(coeftable) %>% `$`(Estimate) %>% `[`(4),
                   w.ro.ed   = 0, 
                   w.ro.ss   = w.ro.est %>% `$`(coeftable) %>% `$`(Estimate) %>% `[`(1) - w.uo.est %>% `$`(coeftable) %>% `$`(Estimate) %>% `[`(1),
                   w.ro.oth  = w.ro.est %>% `$`(coeftable) %>% `$`(Estimate) %>% `[`(2) - w.uo.est %>% `$`(coeftable) %>% `$`(Estimate) %>% `[`(2),
                   w.ro.bus  = w.ro.est %>% `$`(coeftable) %>% `$`(Estimate) %>% `[`(3) - w.uo.est %>% `$`(coeftable) %>% `$`(Estimate) %>% `[`(3),
                   w.ro.stem = w.ro.est %>% `$`(coeftable) %>% `$`(Estimate) %>% `[`(4) - w.uo.est %>% `$`(coeftable) %>% `$`(Estimate) %>% `[`(4),
                   m.ed      = 0, 
                   m.ss      = migr.est %>% `$`(coeftable) %>% `$`(Estimate) %>% `[`(1),
                   m.oth     = migr.est %>% `$`(coeftable) %>% `$`(Estimate) %>% `[`(2),
                   m.bus     = migr.est %>% `$`(coeftable) %>% `$`(Estimate) %>% `[`(3),
                   m.stem    = migr.est %>% `$`(coeftable) %>% `$`(Estimate) %>% `[`(4),
                   r.ed      = 0, 
                   r.ss      = relo.est %>% `$`(coeftable) %>% `$`(Estimate) %>% `[`(1),
                   r.oth     = relo.est %>% `$`(coeftable) %>% `$`(Estimate) %>% `[`(2),
                   r.bus     = relo.est %>% `$`(coeftable) %>% `$`(Estimate) %>% `[`(3),
                   r.stem    = relo.est %>% `$`(coeftable) %>% `$`(Estimate) %>% `[`(4)
                  )
    outs %<>% bind_rows(temp)
}

# descriptive stats for opening paragraph of paper
outs %>% select(state,w.bus    ) %>% arrange(-w.bus     ) %>% head %>% print
outs %>% select(state,w.bus    ) %>% arrange( w.bus     ) %>% head %>% print
outs %>% select(state,w.stem   ) %>% arrange(-w.stem    ) %>% head %>% print
outs %>% select(state,w.stem   ) %>% arrange( w.stem    ) %>% head %>% print
outs %>% select(state,w.ro.ss  ) %>% arrange(-w.ro.ss   ) %>% head %>% print
outs %>% select(state,w.ro.ss  ) %>% arrange( w.ro.ss   ) %>% head %>% print
outs %>% select(state,w.ro.oth ) %>% arrange(-w.ro.oth  ) %>% head %>% print
outs %>% select(state,w.ro.oth ) %>% arrange( w.ro.oth  ) %>% head %>% print
outs %>% select(state,w.ro.bus ) %>% arrange(-w.ro.bus  ) %>% head %>% print
outs %>% select(state,w.ro.bus ) %>% arrange( w.ro.bus  ) %>% head %>% print
outs %>% select(state,w.ro.stem) %>% arrange(-w.ro.stem ) %>% head %>% print
outs %>% select(state,w.ro.stem) %>% arrange( w.ro.stem ) %>% head %>% print

# write output to Table 1
fname <- "../Paper/Tables/table1.tex"
cat("\\begin{table}[ht]"                                                                                             ,file=fname,sep="\n")
cat("\\caption{Differences in outcomes by college major, relative to education majors}"                              ,file=fname,sep="\n",append=TRUE)
cat("\\label{tab:samplemeans}"                                                                                       ,file=fname,sep="\n",append=TRUE)
cat("\\centering"                                                                                                    ,file=fname,sep="\n",append=TRUE)
cat("\\begin{threeparttable}"                                                                                        ,file=fname,sep="\n",append=TRUE)
cat("\\begin{tabular}{lcccccc}"                                                                                      ,file=fname,sep="\n",append=TRUE)
cat("\\toprule"                                                                                                      ,file=fname,sep="\n",append=TRUE)
cat("                                & Education & Soc Sci & Other   & Business & STEM     \\\\",file=fname,sep="\n",append=TRUE)
cat("\\midrule"                                                                                                      ,file=fname,sep="\n",append=TRUE)
paste0("Log Earnings                    & 0.00      & ",
       est1 %>% `$`(coeftable) %>% `$`(Estimate) %>% `[`(1) %>% round(digits=3) ,"   & ",
       est1 %>% `$`(coeftable) %>% `$`(Estimate) %>% `[`(2) %>% round(digits=3) ,"   & ",
       est1 %>% `$`(coeftable) %>% `$`(Estimate) %>% `[`(3) %>% round(digits=3) ,"   & ",
       est1 %>% `$`(coeftable) %>% `$`(Estimate) %>% `[`(4) %>% round(digits=3) ,"    \\\\"
      ) %>% cat(file=fname,sep="\n",append=TRUE)
paste0("                                & (---)      & (",
       outs %>% `$`(w.ss)   %>% sd %>% round(digits=3) ,")   & (",
       outs %>% `$`(w.oth)  %>% sd %>% round(digits=3) ,")   & (",
       outs %>% `$`(w.bus)  %>% sd %>% round(digits=3) ,")   & (",
       outs %>% `$`(w.stem) %>% sd %>% round(digits=3) ,")    \\\\"
      ) %>% cat(file=fname,sep="\n",append=TRUE)
paste0("Pr(Lives outside birth state)   & 0.00      & ",
       est2 %>% `$`(coeftable) %>% `$`(Estimate) %>% `[`(1) %>% round(digits=3) ,"   & ",
       est2 %>% `$`(coeftable) %>% `$`(Estimate) %>% `[`(2) %>% round(digits=3) ,"   & ",
       est2 %>% `$`(coeftable) %>% `$`(Estimate) %>% `[`(3) %>% round(digits=3) ,"   & ",
       est2 %>% `$`(coeftable) %>% `$`(Estimate) %>% `[`(4) %>% round(digits=3) ,"    \\\\"
      ) %>% cat(file=fname,sep="\n",append=TRUE)
paste0("                                & (---)      & (",
       outs %>% `$`(m.ss)   %>% sd %>% round(digits=3) ,")   & (",
       outs %>% `$`(m.oth)  %>% sd %>% round(digits=3) ,")   & (",
       outs %>% `$`(m.bus)  %>% sd %>% round(digits=3) ,")   & (",
       outs %>% `$`(m.stem) %>% sd %>% round(digits=3) ,")    \\\\"
      ) %>% cat(file=fname,sep="\n",append=TRUE)
paste0("Pr(Works in related occupation) & 0.00      & ",
       est4 %>% `$`(coeftable) %>% `$`(Estimate) %>% `[`(1) %>% round(digits=3) ,"   & ",
       est4 %>% `$`(coeftable) %>% `$`(Estimate) %>% `[`(2) %>% round(digits=3) ,"   & ",
       est4 %>% `$`(coeftable) %>% `$`(Estimate) %>% `[`(3) %>% round(digits=3) ,"   & ",
       est4 %>% `$`(coeftable) %>% `$`(Estimate) %>% `[`(4) %>% round(digits=3) ,"    \\\\"
      ) %>% cat(file=fname,sep="\n",append=TRUE)
paste0("                                & (---)      & (",
       outs %>% `$`(r.ss)   %>% sd %>% round(digits=3) ,")   & (",
       outs %>% `$`(r.oth)  %>% sd %>% round(digits=3) ,")   & (",
       outs %>% `$`(r.bus)  %>% sd %>% round(digits=3) ,")   & (",
       outs %>% `$`(r.stem) %>% sd %>% round(digits=3) ,")    \\\\"
      ) %>% cat(file=fname,sep="\n",append=TRUE)
cat("\\midrule"                                                                                                      ,file=fname,sep="\n",append=TRUE)
paste0("Frequency (\\%)                  & ",
       df %>% `$`(educvar) %>% table %>% prop.table %>% `[[`(1) %>% `*`(100) %>% round(digits=2) ,"   & ",
       df %>% `$`(educvar) %>% table %>% prop.table %>% `[[`(2) %>% `*`(100) %>% round(digits=2) ,"   & ",
       df %>% `$`(educvar) %>% table %>% prop.table %>% `[[`(3) %>% `*`(100) %>% round(digits=2) ,"   & ",
       df %>% `$`(educvar) %>% table %>% prop.table %>% `[[`(4) %>% `*`(100) %>% round(digits=2) ,"   & ",
       df %>% `$`(educvar) %>% table %>% prop.table %>% `[[`(5) %>% `*`(100) %>% round(digits=2) ," \\\\ "
      ) %>% cat(file=fname,sep="\n",append=TRUE)
paste0("N                               & ",
       df %>% `$`(educvar) %>% table %>% `[[`(1) %>% format(big.mark=",") ," & ",
       df %>% `$`(educvar) %>% table %>% `[[`(2) %>% format(big.mark=",") ," & ",
       df %>% `$`(educvar) %>% table %>% `[[`(3) %>% format(big.mark=",") ," & ",
       df %>% `$`(educvar) %>% table %>% `[[`(4) %>% format(big.mark=",") ," & ",
       df %>% `$`(educvar) %>% table %>% `[[`(5) %>% format(big.mark=",") ," \\\\ "
      ) %>% cat(file=fname,sep="\n",append=TRUE)
cat("\\bottomrule"                                                                                                   ,file=fname,sep="\n",append=TRUE) 
cat("\\end{tabular}"                                                                                                 ,file=fname,sep="\n",append=TRUE) 
cat(paste0("{\\footnotesize {\\raggedright Notes: Regression estimates at national level, controlling for demographics, advanced degree status, CBSA dummies, and a cubic in potential experience. Standard deviation of state-specific estimates reported below in parentheses. All variables except for log earnings and distance are expressed in percentage points and estimated from linear probability models. Sample taken from the 2010-",max(df$year)," American Community Survey and is restricted to males ages 22-54 with a bachelor's degree or higher. Sample weights are included in the computation. Additional details on sample selection can be found in Table \\ref{tab:sampleselection}.}}"),file=fname,sep="\n",append=TRUE)
cat("\\end{threeparttable}"                                                                                          ,file=fname,sep="\n",append=TRUE)  
cat("\\end{table}"                                                                                                   ,file=fname,sep="\n",append=TRUE)   


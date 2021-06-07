#library(tidyverse)
#library(magrittr)
#library(ggrepel)
#library(purrr)
#library(abind)

#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# functions to compute bootstrap SEs and output results for later exporting to a LaTeX table
#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

# bootstrap covariance across all parameters, models, and equations (30 locations, 11 parameters, 2 models = 660 parameters)
booter_cov <- function(df,df2) {
    B <- df$bootnum %>% as.numeric %>% max
    boots <- df %>%
             select(bootnum,choice=loc.occ,name=uncr.name,coefuncr=uncr.coef,coefcorr=corr.coef) %>%
             pivot_longer(
                          cols = starts_with("coef"),
                          names_to = "corrtype",
                          names_prefix = "coef",
                          values_to = "coef",
                          values_drop_na = TRUE
                          ) %>%
             mutate(name = str_replace_all(name, "educvar", ""),
                    parmname = str_c(choice,name,corrtype,sep = "_"),
                    bootnum = as.numeric(bootnum)) %>%
             select(-choice,-name,-corrtype) %>%
             split(.$bootnum) %>%
             map(~mutate(.x,bootnum = NULL)) %>%
             map(~column_to_rownames(.x,var = "parmname")) %>%
             abind(along = 2)

    # now compute bootstrap var-cov matrix
    meanb <- apply(boots, 1, mean)
    vcv <- array(0, c(660, 660))
    for (b in seq(1,B)) {
        vcv <- vcv + (1/(B-1))*( (boots[,b]-meanb) %o% (boots[,b]-meanb) )
    }
    vcv
}

# boostrap SEs
booter <- function(df,df2,vcv) {
    df %>% group_by(loc.occ,uncr.name) %>% 
           dplyr::summarize(
                     pctdiff = mean(100*(corr.coef-uncr.coef)/uncr.coef),
                     uncr.b  = mean(uncr.coef),
                     corr.b  = mean(corr.coef),
                     uncr.se = sd(uncr.coef),
                     corr.se = sd(corr.coef),
                     pctd.se = sd(100*(corr.coef-uncr.coef)/uncr.coef), 
                     nobs1   = mean(uncr.nobs),
                     nobs2   = mean(corr.nobs),
                     r2_1    = mean(uncr.r2),
                     r2_2    = mean(corr.r2),
                     fstat   = mean(corr.f),
                     f.pval  = mean(corr.f<=fcritical),
                     nboot   = n()
                    ) %>%
           mutate(pctdiff = replace(pctdiff,is.na(pctdiff),0),
                  pctd.se = replace(pctd.se,is.na(pctd.se),0)) %>%
           ungroup %>%
           left_join(df2, by=c("loc.occ","uncr.name")) %>%
           mutate(uncr.t    = uncr.coef/uncr.se,
                  corr.t    = corr.coef/corr.se,
                  uncr.p    = 2*pt(-abs(uncr.t), uncr.nobs-30),
                  corr.p    = 2*pt(-abs(corr.t), corr.nobs-30),
                  loc       = loc.occ,
                  loc       = str_replace_all(loc," Related",""),
                  loc       = str_replace_all(loc," Unrelated",""),
                  uncr.star = case_when(
                                        uncr.p < .01 ~ "***",
                                        uncr.p >=.01 & uncr.p<.05 ~ "**",
                                        uncr.p >=.05 & uncr.p<.10 ~ "*",
                                        TRUE ~ ""
                                       ),
                  corr.star = case_when(
                                        corr.p < .01 ~ "***",
                                        corr.p >=.01 & corr.p<.05 ~ "**",
                                        corr.p >=.05 & corr.p<.10 ~ "*",
                                        TRUE ~ ""
                                       ),
                  adv       = case_when(
                                        str_detect(uncr.name,"advdegree") ~ "Advanced Degree",
                                        TRUE ~ ""
                                       ),
                  occ       = case_when(
                                        str_detect(loc.occ,"Related")   ~ "Related",
                                        str_detect(loc.occ,"Unrelated") ~ "Unrelated",
                                        TRUE ~ ""
                                       ),
                  major     = case_when(
                                        str_detect(uncr.name,"Education")     ~ "Education major",
                                        str_detect(uncr.name,"Social Sci")    ~ "Social sciences major",
                                        str_detect(uncr.name,"Other")         ~ "Other major",
                                        str_detect(uncr.name,"Business")      ~ "Business major",
                                        str_detect(uncr.name,"STEM")          ~ "STEM major",
                                        str_detect(uncr.name,"inBirthStTRUE") ~ "Born here",
                                        TRUE                                  ~ "Education major"
                                       ),
                  name      = str_replace_all(uncr.name, "educvar", ""),
                 ) %>%
           rowwise() %>%
           mutate(haus.chi2 = (corr.coef - uncr.coef)^2/abs(vcv[paste(loc.occ,name,"corr",sep="_"),paste(loc.occ,name,"corr",sep="_")]-vcv[paste(loc.occ,name,"uncr",sep="_"),paste(loc.occ,name,"uncr",sep="_")]-0*vcv[paste(loc.occ,name,"corr",sep="_"),paste(loc.occ,name,"uncr",sep="_")])) %>%
           ungroup %>%
           mutate(
                  haus.p    = pchisq(haus.chi2, 1, lower.tail = F),
                  haus.p    = replace(haus.p,is.na(haus.p),1)
                 )
}

# better rounding
specify_decimal <- function(x, k) trimws(format(round(x, k), nsmall=k))

# write output to Table 3
maketable3 <- function(df) {
    fname <- "../Paper/Tables/table3.tex"
    cat("\\begin{table}[ht]"                                                                                             ,file=fname,sep="\n")
    cat("\\caption{Uncorrected vs. corrected earnings equation estimates for select states}"                             ,file=fname,sep="\n",append=TRUE)
    cat("\\label{tab:earnings}"                                                                                          ,file=fname,sep="\n",append=TRUE)
    cat("\\centering"                                                                                                    ,file=fname,sep="\n",append=TRUE)
    cat("\\resizebox{1.3\\textwidth}{!}{"                                                                                ,file=fname,sep="\n",append=TRUE)
    cat("\\begin{threeparttable}"                                                                                        ,file=fname,sep="\n",append=TRUE)
    cat("\\begin{tabular}{lcccccccccccc}"                                                                                ,file=fname,sep="\n",append=TRUE)
    cat("\\toprule"                                                                                                      ,file=fname,sep="\n",append=TRUE)
    cat("                & \\multicolumn{4}{c}{Florida} & \\multicolumn{4}{c}{New York} & \\multicolumn{4}{c}{Texas}\\\\",file=fname,sep="\n",append=TRUE)
    cat("\\cmidrule(lr{.5em}){2-5}\\cmidrule(lr{.5em}){6-9}\\cmidrule(l{.5em}){10-13}"                                   ,file=fname,sep="\n",append=TRUE)
    cat("                        & \\multicolumn{2}{c}{Unrelated Occupation} & \\multicolumn{2}{c}{Related Occupation} & \\multicolumn{2}{c}{Unrelated Occupation} & \\multicolumn{2}{c}{Related Occupation} & \\multicolumn{2}{c}{Unrelated Occupation} & \\multicolumn{2}{c}{Related Occupation} \\\\",file=fname,sep="\n",append=TRUE)
    cat("\\cmidrule(lr{.5em}){2-3}\\cmidrule(lr{.5em}){4-5}\\cmidrule(lr{.5em}){6-7}\\cmidrule(lr{.5em}){8-9}\\cmidrule(lr{.5em}){10-11}\\cmidrule(l{.5em}){12-13}",file=fname,sep="\n",append=TRUE)
    cat("                                     & Uncorrected  & Corrected    & Uncorrected  & Corrected    & Uncorrected  & Corrected    & Uncorrected  & Corrected    & Uncorrected  & Corrected    & Uncorrected  & Corrected   \\\\",file=fname,sep="\n",append=TRUE)
    cat("\\midrule",file=fname,sep="\n",append=TRUE)
    cat("\\emph{Bachelor's degree}             &              &              &              &              &              &              &              &              &              &              &              &             \\\\",file=fname,sep="\n",append=TRUE)
    # education major, no advanced degree
    paste0("\\qquad{}",
           df %>% filter(loc=="Florida"  & adv==""  & occ=="Unrelated" & major=="Education major") %>% `$`(major)     %>% `[`(1)                          ," & ",
           df %>% filter(loc=="Florida"  & adv==""  & occ=="Unrelated" & major=="Education major") %>% `$`(uncr.coef) %>% `[`(1) %>% specify_decimal(.,3) ,
           df %>% filter(loc=="Florida"  & adv==""  & occ=="Unrelated" & major=="Education major") %>% `$`(uncr.star) %>% `[`(1)                          ," & ",
           df %>% filter(loc=="Florida"  & adv==""  & occ=="Unrelated" & major=="Education major") %>% `$`(corr.coef) %>% `[`(1) %>% specify_decimal(.,3) ,
           df %>% filter(loc=="Florida"  & adv==""  & occ=="Unrelated" & major=="Education major") %>% `$`(corr.star) %>% `[`(1)                          ," & ",
           df %>% filter(loc=="Florida"  & adv==""  & occ=="Related"   & major=="Education major") %>% `$`(uncr.coef) %>% `[`(1) %>% specify_decimal(.,3) ,
           df %>% filter(loc=="Florida"  & adv==""  & occ=="Related"   & major=="Education major") %>% `$`(uncr.star) %>% `[`(1)                          ," & ",
           df %>% filter(loc=="Florida"  & adv==""  & occ=="Related"   & major=="Education major") %>% `$`(corr.coef) %>% `[`(1) %>% specify_decimal(.,3) ,
           df %>% filter(loc=="Florida"  & adv==""  & occ=="Related"   & major=="Education major") %>% `$`(corr.star) %>% `[`(1)                          ," & ",
           df %>% filter(loc=="NewYork"  & adv==""  & occ=="Unrelated" & major=="Education major") %>% `$`(uncr.coef) %>% `[`(1) %>% specify_decimal(.,3) ,
           df %>% filter(loc=="NewYork"  & adv==""  & occ=="Unrelated" & major=="Education major") %>% `$`(uncr.star) %>% `[`(1)                          ," & ",
           df %>% filter(loc=="NewYork"  & adv==""  & occ=="Unrelated" & major=="Education major") %>% `$`(corr.coef) %>% `[`(1) %>% specify_decimal(.,3) ,
           df %>% filter(loc=="NewYork"  & adv==""  & occ=="Unrelated" & major=="Education major") %>% `$`(corr.star) %>% `[`(1)                          ," & ",
           df %>% filter(loc=="NewYork"  & adv==""  & occ=="Related"   & major=="Education major") %>% `$`(uncr.coef) %>% `[`(1) %>% specify_decimal(.,3) ,
           df %>% filter(loc=="NewYork"  & adv==""  & occ=="Related"   & major=="Education major") %>% `$`(uncr.star) %>% `[`(1)                          ," & ",
           df %>% filter(loc=="NewYork"  & adv==""  & occ=="Related"   & major=="Education major") %>% `$`(corr.coef) %>% `[`(1) %>% specify_decimal(.,3) ,
           df %>% filter(loc=="NewYork"  & adv==""  & occ=="Related"   & major=="Education major") %>% `$`(corr.star) %>% `[`(1)                          ," & ",
           df %>% filter(loc=="Texas"    & adv==""  & occ=="Unrelated" & major=="Education major") %>% `$`(uncr.coef) %>% `[`(1) %>% specify_decimal(.,3) ,
           df %>% filter(loc=="Texas"    & adv==""  & occ=="Unrelated" & major=="Education major") %>% `$`(uncr.star) %>% `[`(1)                          ," & ",
           df %>% filter(loc=="Texas"    & adv==""  & occ=="Unrelated" & major=="Education major") %>% `$`(corr.coef) %>% `[`(1) %>% specify_decimal(.,3) ,
           df %>% filter(loc=="Texas"    & adv==""  & occ=="Unrelated" & major=="Education major") %>% `$`(corr.star) %>% `[`(1)                          ," & ",
           df %>% filter(loc=="Texas"    & adv==""  & occ=="Related"   & major=="Education major") %>% `$`(uncr.coef) %>% `[`(1) %>% specify_decimal(.,3) ,
           df %>% filter(loc=="Texas"    & adv==""  & occ=="Related"   & major=="Education major") %>% `$`(uncr.star) %>% `[`(1)                          ," & ",
           df %>% filter(loc=="Texas"    & adv==""  & occ=="Related"   & major=="Education major") %>% `$`(corr.coef) %>% `[`(1) %>% specify_decimal(.,3) ,
           df %>% filter(loc=="Texas"    & adv==""  & occ=="Related"   & major=="Education major") %>% `$`(corr.star) %>% `[`(1)                          ,"    \\\\" 
          ) %>% cat(file=fname,sep="\n",append=TRUE)
    paste0(" & (",
           df %>% filter(loc=="Florida"  & adv==""  & occ=="Unrelated" & major=="Education major") %>% `$`(uncr.se)   %>% `[`(1) %>% specify_decimal(.,3) ,") & (",
           df %>% filter(loc=="Florida"  & adv==""  & occ=="Unrelated" & major=="Education major") %>% `$`(corr.se)   %>% `[`(1) %>% specify_decimal(.,3) ,") & (",
           df %>% filter(loc=="Florida"  & adv==""  & occ=="Related"   & major=="Education major") %>% `$`(uncr.se)   %>% `[`(1) %>% specify_decimal(.,3) ,") & (",
           df %>% filter(loc=="Florida"  & adv==""  & occ=="Related"   & major=="Education major") %>% `$`(corr.se)   %>% `[`(1) %>% specify_decimal(.,3) ,") & (",
           df %>% filter(loc=="NewYork"  & adv==""  & occ=="Unrelated" & major=="Education major") %>% `$`(uncr.se)   %>% `[`(1) %>% specify_decimal(.,3) ,") & (",
           df %>% filter(loc=="NewYork"  & adv==""  & occ=="Unrelated" & major=="Education major") %>% `$`(corr.se)   %>% `[`(1) %>% specify_decimal(.,3) ,") & (",
           df %>% filter(loc=="NewYork"  & adv==""  & occ=="Related"   & major=="Education major") %>% `$`(uncr.se)   %>% `[`(1) %>% specify_decimal(.,3) ,") & (",
           df %>% filter(loc=="NewYork"  & adv==""  & occ=="Related"   & major=="Education major") %>% `$`(corr.se)   %>% `[`(1) %>% specify_decimal(.,3) ,") & (",
           df %>% filter(loc=="Texas"    & adv==""  & occ=="Unrelated" & major=="Education major") %>% `$`(uncr.se)   %>% `[`(1) %>% specify_decimal(.,3) ,") & (",
           df %>% filter(loc=="Texas"    & adv==""  & occ=="Unrelated" & major=="Education major") %>% `$`(corr.se)   %>% `[`(1) %>% specify_decimal(.,3) ,") & (",
           df %>% filter(loc=="Texas"    & adv==""  & occ=="Related"   & major=="Education major") %>% `$`(uncr.se)   %>% `[`(1) %>% specify_decimal(.,3) ,") & (",
           df %>% filter(loc=="Texas"    & adv==""  & occ=="Related"   & major=="Education major") %>% `$`(corr.se)   %>% `[`(1) %>% specify_decimal(.,3) ,") \\\\" 
          ) %>% cat(file=fname,sep="\n",append=TRUE)
    # soc sci major, no advanced degree
    paste0("\\qquad{}",
           df %>% filter(loc=="Florida"  & adv==""  & occ=="Unrelated" & major=="Social sciences major") %>% `$`(major)     %>% `[`(1)                          ," & ",
           df %>% filter(loc=="Florida"  & adv==""  & occ=="Unrelated" & major=="Social sciences major") %>% `$`(uncr.coef) %>% `[`(1) %>% specify_decimal(.,3) ,
           df %>% filter(loc=="Florida"  & adv==""  & occ=="Unrelated" & major=="Social sciences major") %>% `$`(uncr.star) %>% `[`(1)                          ," & ",
           df %>% filter(loc=="Florida"  & adv==""  & occ=="Unrelated" & major=="Social sciences major") %>% `$`(corr.coef) %>% `[`(1) %>% specify_decimal(.,3) ,
           df %>% filter(loc=="Florida"  & adv==""  & occ=="Unrelated" & major=="Social sciences major") %>% `$`(corr.star) %>% `[`(1)                          ," & ",
           df %>% filter(loc=="Florida"  & adv==""  & occ=="Related"   & major=="Social sciences major") %>% `$`(uncr.coef) %>% `[`(1) %>% specify_decimal(.,3) ,
           df %>% filter(loc=="Florida"  & adv==""  & occ=="Related"   & major=="Social sciences major") %>% `$`(uncr.star) %>% `[`(1)                          ," & ",
           df %>% filter(loc=="Florida"  & adv==""  & occ=="Related"   & major=="Social sciences major") %>% `$`(corr.coef) %>% `[`(1) %>% specify_decimal(.,3) ,
           df %>% filter(loc=="Florida"  & adv==""  & occ=="Related"   & major=="Social sciences major") %>% `$`(corr.star) %>% `[`(1)                          ," & ",
           df %>% filter(loc=="NewYork"  & adv==""  & occ=="Unrelated" & major=="Social sciences major") %>% `$`(uncr.coef) %>% `[`(1) %>% specify_decimal(.,3) ,
           df %>% filter(loc=="NewYork"  & adv==""  & occ=="Unrelated" & major=="Social sciences major") %>% `$`(uncr.star) %>% `[`(1)                          ," & ",
           df %>% filter(loc=="NewYork"  & adv==""  & occ=="Unrelated" & major=="Social sciences major") %>% `$`(corr.coef) %>% `[`(1) %>% specify_decimal(.,3) ,
           df %>% filter(loc=="NewYork"  & adv==""  & occ=="Unrelated" & major=="Social sciences major") %>% `$`(corr.star) %>% `[`(1)                          ," & ",
           df %>% filter(loc=="NewYork"  & adv==""  & occ=="Related"   & major=="Social sciences major") %>% `$`(uncr.coef) %>% `[`(1) %>% specify_decimal(.,3) ,
           df %>% filter(loc=="NewYork"  & adv==""  & occ=="Related"   & major=="Social sciences major") %>% `$`(uncr.star) %>% `[`(1)                          ," & ",
           df %>% filter(loc=="NewYork"  & adv==""  & occ=="Related"   & major=="Social sciences major") %>% `$`(corr.coef) %>% `[`(1) %>% specify_decimal(.,3) ,
           df %>% filter(loc=="NewYork"  & adv==""  & occ=="Related"   & major=="Social sciences major") %>% `$`(corr.star) %>% `[`(1)                          ," & ",
           df %>% filter(loc=="Texas"    & adv==""  & occ=="Unrelated" & major=="Social sciences major") %>% `$`(uncr.coef) %>% `[`(1) %>% specify_decimal(.,3) ,
           df %>% filter(loc=="Texas"    & adv==""  & occ=="Unrelated" & major=="Social sciences major") %>% `$`(uncr.star) %>% `[`(1)                          ," & ",
           df %>% filter(loc=="Texas"    & adv==""  & occ=="Unrelated" & major=="Social sciences major") %>% `$`(corr.coef) %>% `[`(1) %>% specify_decimal(.,3) ,
           df %>% filter(loc=="Texas"    & adv==""  & occ=="Unrelated" & major=="Social sciences major") %>% `$`(corr.star) %>% `[`(1)                          ," & ",
           df %>% filter(loc=="Texas"    & adv==""  & occ=="Related"   & major=="Social sciences major") %>% `$`(uncr.coef) %>% `[`(1) %>% specify_decimal(.,3) ,
           df %>% filter(loc=="Texas"    & adv==""  & occ=="Related"   & major=="Social sciences major") %>% `$`(uncr.star) %>% `[`(1)                          ," & ",
           df %>% filter(loc=="Texas"    & adv==""  & occ=="Related"   & major=="Social sciences major") %>% `$`(corr.coef) %>% `[`(1) %>% specify_decimal(.,3) ,
           df %>% filter(loc=="Texas"    & adv==""  & occ=="Related"   & major=="Social sciences major") %>% `$`(corr.star) %>% `[`(1)                          ,"    \\\\" 
          ) %>% cat(file=fname,sep="\n",append=TRUE)
    paste0(" & (",
           df %>% filter(loc=="Florida"  & adv==""  & occ=="Unrelated" & major=="Social sciences major") %>% `$`(uncr.se)   %>% `[`(1) %>% specify_decimal(.,3) ,") & (",
           df %>% filter(loc=="Florida"  & adv==""  & occ=="Unrelated" & major=="Social sciences major") %>% `$`(corr.se)   %>% `[`(1) %>% specify_decimal(.,3) ,") & (",
           df %>% filter(loc=="Florida"  & adv==""  & occ=="Related"   & major=="Social sciences major") %>% `$`(uncr.se)   %>% `[`(1) %>% specify_decimal(.,3) ,") & (",
           df %>% filter(loc=="Florida"  & adv==""  & occ=="Related"   & major=="Social sciences major") %>% `$`(corr.se)   %>% `[`(1) %>% specify_decimal(.,3) ,") & (",
           df %>% filter(loc=="NewYork"  & adv==""  & occ=="Unrelated" & major=="Social sciences major") %>% `$`(uncr.se)   %>% `[`(1) %>% specify_decimal(.,3) ,") & (",
           df %>% filter(loc=="NewYork"  & adv==""  & occ=="Unrelated" & major=="Social sciences major") %>% `$`(corr.se)   %>% `[`(1) %>% specify_decimal(.,3) ,") & (",
           df %>% filter(loc=="NewYork"  & adv==""  & occ=="Related"   & major=="Social sciences major") %>% `$`(uncr.se)   %>% `[`(1) %>% specify_decimal(.,3) ,") & (",
           df %>% filter(loc=="NewYork"  & adv==""  & occ=="Related"   & major=="Social sciences major") %>% `$`(corr.se)   %>% `[`(1) %>% specify_decimal(.,3) ,") & (",
           df %>% filter(loc=="Texas"    & adv==""  & occ=="Unrelated" & major=="Social sciences major") %>% `$`(uncr.se)   %>% `[`(1) %>% specify_decimal(.,3) ,") & (",
           df %>% filter(loc=="Texas"    & adv==""  & occ=="Unrelated" & major=="Social sciences major") %>% `$`(corr.se)   %>% `[`(1) %>% specify_decimal(.,3) ,") & (",
           df %>% filter(loc=="Texas"    & adv==""  & occ=="Related"   & major=="Social sciences major") %>% `$`(uncr.se)   %>% `[`(1) %>% specify_decimal(.,3) ,") & (",
           df %>% filter(loc=="Texas"    & adv==""  & occ=="Related"   & major=="Social sciences major") %>% `$`(corr.se)   %>% `[`(1) %>% specify_decimal(.,3) ,") \\\\" 
          ) %>% cat(file=fname,sep="\n",append=TRUE)
    # other major, no advanced degree
    paste0("\\qquad{}",
           df %>% filter(loc=="Florida"  & adv==""  & occ=="Unrelated" & major=="Other major") %>% `$`(major)     %>% `[`(1)                          ," & ",
           df %>% filter(loc=="Florida"  & adv==""  & occ=="Unrelated" & major=="Other major") %>% `$`(uncr.coef) %>% `[`(1) %>% specify_decimal(.,3) ,
           df %>% filter(loc=="Florida"  & adv==""  & occ=="Unrelated" & major=="Other major") %>% `$`(uncr.star) %>% `[`(1)                          ," & ",
           df %>% filter(loc=="Florida"  & adv==""  & occ=="Unrelated" & major=="Other major") %>% `$`(corr.coef) %>% `[`(1) %>% specify_decimal(.,3) ,
           df %>% filter(loc=="Florida"  & adv==""  & occ=="Unrelated" & major=="Other major") %>% `$`(corr.star) %>% `[`(1)                          ," & ",
           df %>% filter(loc=="Florida"  & adv==""  & occ=="Related"   & major=="Other major") %>% `$`(uncr.coef) %>% `[`(1) %>% specify_decimal(.,3) ,
           df %>% filter(loc=="Florida"  & adv==""  & occ=="Related"   & major=="Other major") %>% `$`(uncr.star) %>% `[`(1)                          ," & ",
           df %>% filter(loc=="Florida"  & adv==""  & occ=="Related"   & major=="Other major") %>% `$`(corr.coef) %>% `[`(1) %>% specify_decimal(.,3) ,
           df %>% filter(loc=="Florida"  & adv==""  & occ=="Related"   & major=="Other major") %>% `$`(corr.star) %>% `[`(1)                          ," & ",
           df %>% filter(loc=="NewYork"  & adv==""  & occ=="Unrelated" & major=="Other major") %>% `$`(uncr.coef) %>% `[`(1) %>% specify_decimal(.,3) ,
           df %>% filter(loc=="NewYork"  & adv==""  & occ=="Unrelated" & major=="Other major") %>% `$`(uncr.star) %>% `[`(1)                          ," & ",
           df %>% filter(loc=="NewYork"  & adv==""  & occ=="Unrelated" & major=="Other major") %>% `$`(corr.coef) %>% `[`(1) %>% specify_decimal(.,3) ,
           df %>% filter(loc=="NewYork"  & adv==""  & occ=="Unrelated" & major=="Other major") %>% `$`(corr.star) %>% `[`(1)                          ," & ",
           df %>% filter(loc=="NewYork"  & adv==""  & occ=="Related"   & major=="Other major") %>% `$`(uncr.coef) %>% `[`(1) %>% specify_decimal(.,3) ,
           df %>% filter(loc=="NewYork"  & adv==""  & occ=="Related"   & major=="Other major") %>% `$`(uncr.star) %>% `[`(1)                          ," & ",
           df %>% filter(loc=="NewYork"  & adv==""  & occ=="Related"   & major=="Other major") %>% `$`(corr.coef) %>% `[`(1) %>% specify_decimal(.,3) ,
           df %>% filter(loc=="NewYork"  & adv==""  & occ=="Related"   & major=="Other major") %>% `$`(corr.star) %>% `[`(1)                          ," & ",
           df %>% filter(loc=="Texas"    & adv==""  & occ=="Unrelated" & major=="Other major") %>% `$`(uncr.coef) %>% `[`(1) %>% specify_decimal(.,3) ,
           df %>% filter(loc=="Texas"    & adv==""  & occ=="Unrelated" & major=="Other major") %>% `$`(uncr.star) %>% `[`(1)                          ," & ",
           df %>% filter(loc=="Texas"    & adv==""  & occ=="Unrelated" & major=="Other major") %>% `$`(corr.coef) %>% `[`(1) %>% specify_decimal(.,3) ,
           df %>% filter(loc=="Texas"    & adv==""  & occ=="Unrelated" & major=="Other major") %>% `$`(corr.star) %>% `[`(1)                          ," & ",
           df %>% filter(loc=="Texas"    & adv==""  & occ=="Related"   & major=="Other major") %>% `$`(uncr.coef) %>% `[`(1) %>% specify_decimal(.,3) ,
           df %>% filter(loc=="Texas"    & adv==""  & occ=="Related"   & major=="Other major") %>% `$`(uncr.star) %>% `[`(1)                          ," & ",
           df %>% filter(loc=="Texas"    & adv==""  & occ=="Related"   & major=="Other major") %>% `$`(corr.coef) %>% `[`(1) %>% specify_decimal(.,3) ,
           df %>% filter(loc=="Texas"    & adv==""  & occ=="Related"   & major=="Other major") %>% `$`(corr.star) %>% `[`(1)                          ,"    \\\\" 
          ) %>% cat(file=fname,sep="\n",append=TRUE)
    paste0(" & (",
           df %>% filter(loc=="Florida"  & adv==""  & occ=="Unrelated" & major=="Other major") %>% `$`(uncr.se)   %>% `[`(1) %>% specify_decimal(.,3) ,") & (",
           df %>% filter(loc=="Florida"  & adv==""  & occ=="Unrelated" & major=="Other major") %>% `$`(corr.se)   %>% `[`(1) %>% specify_decimal(.,3) ,") & (",
           df %>% filter(loc=="Florida"  & adv==""  & occ=="Related"   & major=="Other major") %>% `$`(uncr.se)   %>% `[`(1) %>% specify_decimal(.,3) ,") & (",
           df %>% filter(loc=="Florida"  & adv==""  & occ=="Related"   & major=="Other major") %>% `$`(corr.se)   %>% `[`(1) %>% specify_decimal(.,3) ,") & (",
           df %>% filter(loc=="NewYork"  & adv==""  & occ=="Unrelated" & major=="Other major") %>% `$`(uncr.se)   %>% `[`(1) %>% specify_decimal(.,3) ,") & (",
           df %>% filter(loc=="NewYork"  & adv==""  & occ=="Unrelated" & major=="Other major") %>% `$`(corr.se)   %>% `[`(1) %>% specify_decimal(.,3) ,") & (",
           df %>% filter(loc=="NewYork"  & adv==""  & occ=="Related"   & major=="Other major") %>% `$`(uncr.se)   %>% `[`(1) %>% specify_decimal(.,3) ,") & (",
           df %>% filter(loc=="NewYork"  & adv==""  & occ=="Related"   & major=="Other major") %>% `$`(corr.se)   %>% `[`(1) %>% specify_decimal(.,3) ,") & (",
           df %>% filter(loc=="Texas"    & adv==""  & occ=="Unrelated" & major=="Other major") %>% `$`(uncr.se)   %>% `[`(1) %>% specify_decimal(.,3) ,") & (",
           df %>% filter(loc=="Texas"    & adv==""  & occ=="Unrelated" & major=="Other major") %>% `$`(corr.se)   %>% `[`(1) %>% specify_decimal(.,3) ,") & (",
           df %>% filter(loc=="Texas"    & adv==""  & occ=="Related"   & major=="Other major") %>% `$`(uncr.se)   %>% `[`(1) %>% specify_decimal(.,3) ,") & (",
           df %>% filter(loc=="Texas"    & adv==""  & occ=="Related"   & major=="Other major") %>% `$`(corr.se)   %>% `[`(1) %>% specify_decimal(.,3) ,") \\\\" 
          ) %>% cat(file=fname,sep="\n",append=TRUE)
    # business major, no advanced degree
    paste0("\\qquad{}",
           df %>% filter(loc=="Florida"  & adv==""  & occ=="Unrelated" & major=="Business major") %>% `$`(major)     %>% `[`(1)                          ," & ",
           df %>% filter(loc=="Florida"  & adv==""  & occ=="Unrelated" & major=="Business major") %>% `$`(uncr.coef) %>% `[`(1) %>% specify_decimal(.,3) ,
           df %>% filter(loc=="Florida"  & adv==""  & occ=="Unrelated" & major=="Business major") %>% `$`(uncr.star) %>% `[`(1)                          ," & ",
           df %>% filter(loc=="Florida"  & adv==""  & occ=="Unrelated" & major=="Business major") %>% `$`(corr.coef) %>% `[`(1) %>% specify_decimal(.,3) ,
           df %>% filter(loc=="Florida"  & adv==""  & occ=="Unrelated" & major=="Business major") %>% `$`(corr.star) %>% `[`(1)                          ," & ",
           df %>% filter(loc=="Florida"  & adv==""  & occ=="Related"   & major=="Business major") %>% `$`(uncr.coef) %>% `[`(1) %>% specify_decimal(.,3) ,
           df %>% filter(loc=="Florida"  & adv==""  & occ=="Related"   & major=="Business major") %>% `$`(uncr.star) %>% `[`(1)                          ," & ",
           df %>% filter(loc=="Florida"  & adv==""  & occ=="Related"   & major=="Business major") %>% `$`(corr.coef) %>% `[`(1) %>% specify_decimal(.,3) ,
           df %>% filter(loc=="Florida"  & adv==""  & occ=="Related"   & major=="Business major") %>% `$`(corr.star) %>% `[`(1)                          ," & ",
           df %>% filter(loc=="NewYork"  & adv==""  & occ=="Unrelated" & major=="Business major") %>% `$`(uncr.coef) %>% `[`(1) %>% specify_decimal(.,3) ,
           df %>% filter(loc=="NewYork"  & adv==""  & occ=="Unrelated" & major=="Business major") %>% `$`(uncr.star) %>% `[`(1)                          ," & ",
           df %>% filter(loc=="NewYork"  & adv==""  & occ=="Unrelated" & major=="Business major") %>% `$`(corr.coef) %>% `[`(1) %>% specify_decimal(.,3) ,
           df %>% filter(loc=="NewYork"  & adv==""  & occ=="Unrelated" & major=="Business major") %>% `$`(corr.star) %>% `[`(1)                          ," & ",
           df %>% filter(loc=="NewYork"  & adv==""  & occ=="Related"   & major=="Business major") %>% `$`(uncr.coef) %>% `[`(1) %>% specify_decimal(.,3) ,
           df %>% filter(loc=="NewYork"  & adv==""  & occ=="Related"   & major=="Business major") %>% `$`(uncr.star) %>% `[`(1)                          ," & ",
           df %>% filter(loc=="NewYork"  & adv==""  & occ=="Related"   & major=="Business major") %>% `$`(corr.coef) %>% `[`(1) %>% specify_decimal(.,3) ,
           df %>% filter(loc=="NewYork"  & adv==""  & occ=="Related"   & major=="Business major") %>% `$`(corr.star) %>% `[`(1)                          ," & ",
           df %>% filter(loc=="Texas"    & adv==""  & occ=="Unrelated" & major=="Business major") %>% `$`(uncr.coef) %>% `[`(1) %>% specify_decimal(.,3) ,
           df %>% filter(loc=="Texas"    & adv==""  & occ=="Unrelated" & major=="Business major") %>% `$`(uncr.star) %>% `[`(1)                          ," & ",
           df %>% filter(loc=="Texas"    & adv==""  & occ=="Unrelated" & major=="Business major") %>% `$`(corr.coef) %>% `[`(1) %>% specify_decimal(.,3) ,
           df %>% filter(loc=="Texas"    & adv==""  & occ=="Unrelated" & major=="Business major") %>% `$`(corr.star) %>% `[`(1)                          ," & ",
           df %>% filter(loc=="Texas"    & adv==""  & occ=="Related"   & major=="Business major") %>% `$`(uncr.coef) %>% `[`(1) %>% specify_decimal(.,3) ,
           df %>% filter(loc=="Texas"    & adv==""  & occ=="Related"   & major=="Business major") %>% `$`(uncr.star) %>% `[`(1)                          ," & ",
           df %>% filter(loc=="Texas"    & adv==""  & occ=="Related"   & major=="Business major") %>% `$`(corr.coef) %>% `[`(1) %>% specify_decimal(.,3) ,
           df %>% filter(loc=="Texas"    & adv==""  & occ=="Related"   & major=="Business major") %>% `$`(corr.star) %>% `[`(1)                          ,"    \\\\" 
          ) %>% cat(file=fname,sep="\n",append=TRUE)
    paste0(" & (",
           df %>% filter(loc=="Florida"  & adv==""  & occ=="Unrelated" & major=="Business major") %>% `$`(uncr.se)   %>% `[`(1) %>% specify_decimal(.,3) ,") & (",
           df %>% filter(loc=="Florida"  & adv==""  & occ=="Unrelated" & major=="Business major") %>% `$`(corr.se)   %>% `[`(1) %>% specify_decimal(.,3) ,") & (",
           df %>% filter(loc=="Florida"  & adv==""  & occ=="Related"   & major=="Business major") %>% `$`(uncr.se)   %>% `[`(1) %>% specify_decimal(.,3) ,") & (",
           df %>% filter(loc=="Florida"  & adv==""  & occ=="Related"   & major=="Business major") %>% `$`(corr.se)   %>% `[`(1) %>% specify_decimal(.,3) ,") & (",
           df %>% filter(loc=="NewYork"  & adv==""  & occ=="Unrelated" & major=="Business major") %>% `$`(uncr.se)   %>% `[`(1) %>% specify_decimal(.,3) ,") & (",
           df %>% filter(loc=="NewYork"  & adv==""  & occ=="Unrelated" & major=="Business major") %>% `$`(corr.se)   %>% `[`(1) %>% specify_decimal(.,3) ,") & (",
           df %>% filter(loc=="NewYork"  & adv==""  & occ=="Related"   & major=="Business major") %>% `$`(uncr.se)   %>% `[`(1) %>% specify_decimal(.,3) ,") & (",
           df %>% filter(loc=="NewYork"  & adv==""  & occ=="Related"   & major=="Business major") %>% `$`(corr.se)   %>% `[`(1) %>% specify_decimal(.,3) ,") & (",
           df %>% filter(loc=="Texas"    & adv==""  & occ=="Unrelated" & major=="Business major") %>% `$`(uncr.se)   %>% `[`(1) %>% specify_decimal(.,3) ,") & (",
           df %>% filter(loc=="Texas"    & adv==""  & occ=="Unrelated" & major=="Business major") %>% `$`(corr.se)   %>% `[`(1) %>% specify_decimal(.,3) ,") & (",
           df %>% filter(loc=="Texas"    & adv==""  & occ=="Related"   & major=="Business major") %>% `$`(uncr.se)   %>% `[`(1) %>% specify_decimal(.,3) ,") & (",
           df %>% filter(loc=="Texas"    & adv==""  & occ=="Related"   & major=="Business major") %>% `$`(corr.se)   %>% `[`(1) %>% specify_decimal(.,3) ,") \\\\" 
          ) %>% cat(file=fname,sep="\n",append=TRUE)
    # STEM major, no advanced degree
    paste0("\\qquad{}",
           df %>% filter(loc=="Florida"  & adv==""  & occ=="Unrelated" & major=="STEM major") %>% `$`(major)     %>% `[`(1)                          ," & ",
           df %>% filter(loc=="Florida"  & adv==""  & occ=="Unrelated" & major=="STEM major") %>% `$`(uncr.coef) %>% `[`(1) %>% specify_decimal(.,3) ,
           df %>% filter(loc=="Florida"  & adv==""  & occ=="Unrelated" & major=="STEM major") %>% `$`(uncr.star) %>% `[`(1)                          ," & ",
           df %>% filter(loc=="Florida"  & adv==""  & occ=="Unrelated" & major=="STEM major") %>% `$`(corr.coef) %>% `[`(1) %>% specify_decimal(.,3) ,
           df %>% filter(loc=="Florida"  & adv==""  & occ=="Unrelated" & major=="STEM major") %>% `$`(corr.star) %>% `[`(1)                          ," & ",
           df %>% filter(loc=="Florida"  & adv==""  & occ=="Related"   & major=="STEM major") %>% `$`(uncr.coef) %>% `[`(1) %>% specify_decimal(.,3) ,
           df %>% filter(loc=="Florida"  & adv==""  & occ=="Related"   & major=="STEM major") %>% `$`(uncr.star) %>% `[`(1)                          ," & ",
           df %>% filter(loc=="Florida"  & adv==""  & occ=="Related"   & major=="STEM major") %>% `$`(corr.coef) %>% `[`(1) %>% specify_decimal(.,3) ,
           df %>% filter(loc=="Florida"  & adv==""  & occ=="Related"   & major=="STEM major") %>% `$`(corr.star) %>% `[`(1)                          ," & ",
           df %>% filter(loc=="NewYork"  & adv==""  & occ=="Unrelated" & major=="STEM major") %>% `$`(uncr.coef) %>% `[`(1) %>% specify_decimal(.,3) ,
           df %>% filter(loc=="NewYork"  & adv==""  & occ=="Unrelated" & major=="STEM major") %>% `$`(uncr.star) %>% `[`(1)                          ," & ",
           df %>% filter(loc=="NewYork"  & adv==""  & occ=="Unrelated" & major=="STEM major") %>% `$`(corr.coef) %>% `[`(1) %>% specify_decimal(.,3) ,
           df %>% filter(loc=="NewYork"  & adv==""  & occ=="Unrelated" & major=="STEM major") %>% `$`(corr.star) %>% `[`(1)                          ," & ",
           df %>% filter(loc=="NewYork"  & adv==""  & occ=="Related"   & major=="STEM major") %>% `$`(uncr.coef) %>% `[`(1) %>% specify_decimal(.,3) ,
           df %>% filter(loc=="NewYork"  & adv==""  & occ=="Related"   & major=="STEM major") %>% `$`(uncr.star) %>% `[`(1)                          ," & ",
           df %>% filter(loc=="NewYork"  & adv==""  & occ=="Related"   & major=="STEM major") %>% `$`(corr.coef) %>% `[`(1) %>% specify_decimal(.,3) ,
           df %>% filter(loc=="NewYork"  & adv==""  & occ=="Related"   & major=="STEM major") %>% `$`(corr.star) %>% `[`(1)                          ," & ",
           df %>% filter(loc=="Texas"    & adv==""  & occ=="Unrelated" & major=="STEM major") %>% `$`(uncr.coef) %>% `[`(1) %>% specify_decimal(.,3) ,
           df %>% filter(loc=="Texas"    & adv==""  & occ=="Unrelated" & major=="STEM major") %>% `$`(uncr.star) %>% `[`(1)                          ," & ",
           df %>% filter(loc=="Texas"    & adv==""  & occ=="Unrelated" & major=="STEM major") %>% `$`(corr.coef) %>% `[`(1) %>% specify_decimal(.,3) ,
           df %>% filter(loc=="Texas"    & adv==""  & occ=="Unrelated" & major=="STEM major") %>% `$`(corr.star) %>% `[`(1)                          ," & ",
           df %>% filter(loc=="Texas"    & adv==""  & occ=="Related"   & major=="STEM major") %>% `$`(uncr.coef) %>% `[`(1) %>% specify_decimal(.,3) ,
           df %>% filter(loc=="Texas"    & adv==""  & occ=="Related"   & major=="STEM major") %>% `$`(uncr.star) %>% `[`(1)                          ," & ",
           df %>% filter(loc=="Texas"    & adv==""  & occ=="Related"   & major=="STEM major") %>% `$`(corr.coef) %>% `[`(1) %>% specify_decimal(.,3) ,
           df %>% filter(loc=="Texas"    & adv==""  & occ=="Related"   & major=="STEM major") %>% `$`(corr.star) %>% `[`(1)                          ,"    \\\\" 
          ) %>% cat(file=fname,sep="\n",append=TRUE)
    paste0(" & (",
           df %>% filter(loc=="Florida"  & adv==""  & occ=="Unrelated" & major=="STEM major") %>% `$`(uncr.se)   %>% `[`(1) %>% specify_decimal(.,3) ,") & (",
           df %>% filter(loc=="Florida"  & adv==""  & occ=="Unrelated" & major=="STEM major") %>% `$`(corr.se)   %>% `[`(1) %>% specify_decimal(.,3) ,") & (",
           df %>% filter(loc=="Florida"  & adv==""  & occ=="Related"   & major=="STEM major") %>% `$`(uncr.se)   %>% `[`(1) %>% specify_decimal(.,3) ,") & (",
           df %>% filter(loc=="Florida"  & adv==""  & occ=="Related"   & major=="STEM major") %>% `$`(corr.se)   %>% `[`(1) %>% specify_decimal(.,3) ,") & (",
           df %>% filter(loc=="NewYork"  & adv==""  & occ=="Unrelated" & major=="STEM major") %>% `$`(uncr.se)   %>% `[`(1) %>% specify_decimal(.,3) ,") & (",
           df %>% filter(loc=="NewYork"  & adv==""  & occ=="Unrelated" & major=="STEM major") %>% `$`(corr.se)   %>% `[`(1) %>% specify_decimal(.,3) ,") & (",
           df %>% filter(loc=="NewYork"  & adv==""  & occ=="Related"   & major=="STEM major") %>% `$`(uncr.se)   %>% `[`(1) %>% specify_decimal(.,3) ,") & (",
           df %>% filter(loc=="NewYork"  & adv==""  & occ=="Related"   & major=="STEM major") %>% `$`(corr.se)   %>% `[`(1) %>% specify_decimal(.,3) ,") & (",
           df %>% filter(loc=="Texas"    & adv==""  & occ=="Unrelated" & major=="STEM major") %>% `$`(uncr.se)   %>% `[`(1) %>% specify_decimal(.,3) ,") & (",
           df %>% filter(loc=="Texas"    & adv==""  & occ=="Unrelated" & major=="STEM major") %>% `$`(corr.se)   %>% `[`(1) %>% specify_decimal(.,3) ,") & (",
           df %>% filter(loc=="Texas"    & adv==""  & occ=="Related"   & major=="STEM major") %>% `$`(uncr.se)   %>% `[`(1) %>% specify_decimal(.,3) ,") & (",
           df %>% filter(loc=="Texas"    & adv==""  & occ=="Related"   & major=="STEM major") %>% `$`(corr.se)   %>% `[`(1) %>% specify_decimal(.,3) ,") \\\\" 
          ) %>% cat(file=fname,sep="\n",append=TRUE)
    cat("\\emph{Advanced degree (interaction)}             &              &              &              &              &              &              &              &              &              &              &              &             \\\\",file=fname,sep="\n",append=TRUE)
     # education major, advanced degree
    paste0("\\qquad{}",
           df %>% filter(loc=="Florida"  & adv=="Advanced Degree"  & occ=="Unrelated" & major=="Education major") %>% `$`(major)     %>% `[`(1)                          ," & ",
           df %>% filter(loc=="Florida"  & adv=="Advanced Degree"  & occ=="Unrelated" & major=="Education major") %>% `$`(uncr.coef) %>% `[`(1) %>% specify_decimal(.,3) ,
           df %>% filter(loc=="Florida"  & adv=="Advanced Degree"  & occ=="Unrelated" & major=="Education major") %>% `$`(uncr.star) %>% `[`(1)                          ," & ",
           df %>% filter(loc=="Florida"  & adv=="Advanced Degree"  & occ=="Unrelated" & major=="Education major") %>% `$`(corr.coef) %>% `[`(1) %>% specify_decimal(.,3) ,
           df %>% filter(loc=="Florida"  & adv=="Advanced Degree"  & occ=="Unrelated" & major=="Education major") %>% `$`(corr.star) %>% `[`(1)                          ," & ",
           df %>% filter(loc=="Florida"  & adv=="Advanced Degree"  & occ=="Related"   & major=="Education major") %>% `$`(uncr.coef) %>% `[`(1) %>% specify_decimal(.,3) ,
           df %>% filter(loc=="Florida"  & adv=="Advanced Degree"  & occ=="Related"   & major=="Education major") %>% `$`(uncr.star) %>% `[`(1)                          ," & ",
           df %>% filter(loc=="Florida"  & adv=="Advanced Degree"  & occ=="Related"   & major=="Education major") %>% `$`(corr.coef) %>% `[`(1) %>% specify_decimal(.,3) ,
           df %>% filter(loc=="Florida"  & adv=="Advanced Degree"  & occ=="Related"   & major=="Education major") %>% `$`(corr.star) %>% `[`(1)                          ," & ",
           df %>% filter(loc=="NewYork"  & adv=="Advanced Degree"  & occ=="Unrelated" & major=="Education major") %>% `$`(uncr.coef) %>% `[`(1) %>% specify_decimal(.,3) ,
           df %>% filter(loc=="NewYork"  & adv=="Advanced Degree"  & occ=="Unrelated" & major=="Education major") %>% `$`(uncr.star) %>% `[`(1)                          ," & ",
           df %>% filter(loc=="NewYork"  & adv=="Advanced Degree"  & occ=="Unrelated" & major=="Education major") %>% `$`(corr.coef) %>% `[`(1) %>% specify_decimal(.,3) ,
           df %>% filter(loc=="NewYork"  & adv=="Advanced Degree"  & occ=="Unrelated" & major=="Education major") %>% `$`(corr.star) %>% `[`(1)                          ," & ",
           df %>% filter(loc=="NewYork"  & adv=="Advanced Degree"  & occ=="Related"   & major=="Education major") %>% `$`(uncr.coef) %>% `[`(1) %>% specify_decimal(.,3) ,
           df %>% filter(loc=="NewYork"  & adv=="Advanced Degree"  & occ=="Related"   & major=="Education major") %>% `$`(uncr.star) %>% `[`(1)                          ," & ",
           df %>% filter(loc=="NewYork"  & adv=="Advanced Degree"  & occ=="Related"   & major=="Education major") %>% `$`(corr.coef) %>% `[`(1) %>% specify_decimal(.,3) ,
           df %>% filter(loc=="NewYork"  & adv=="Advanced Degree"  & occ=="Related"   & major=="Education major") %>% `$`(corr.star) %>% `[`(1)                          ," & ",
           df %>% filter(loc=="Texas"    & adv=="Advanced Degree"  & occ=="Unrelated" & major=="Education major") %>% `$`(uncr.coef) %>% `[`(1) %>% specify_decimal(.,3) ,
           df %>% filter(loc=="Texas"    & adv=="Advanced Degree"  & occ=="Unrelated" & major=="Education major") %>% `$`(uncr.star) %>% `[`(1)                          ," & ",
           df %>% filter(loc=="Texas"    & adv=="Advanced Degree"  & occ=="Unrelated" & major=="Education major") %>% `$`(corr.coef) %>% `[`(1) %>% specify_decimal(.,3) ,
           df %>% filter(loc=="Texas"    & adv=="Advanced Degree"  & occ=="Unrelated" & major=="Education major") %>% `$`(corr.star) %>% `[`(1)                          ," & ",
           df %>% filter(loc=="Texas"    & adv=="Advanced Degree"  & occ=="Related"   & major=="Education major") %>% `$`(uncr.coef) %>% `[`(1) %>% specify_decimal(.,3) ,
           df %>% filter(loc=="Texas"    & adv=="Advanced Degree"  & occ=="Related"   & major=="Education major") %>% `$`(uncr.star) %>% `[`(1)                          ," & ",
           df %>% filter(loc=="Texas"    & adv=="Advanced Degree"  & occ=="Related"   & major=="Education major") %>% `$`(corr.coef) %>% `[`(1) %>% specify_decimal(.,3) ,
           df %>% filter(loc=="Texas"    & adv=="Advanced Degree"  & occ=="Related"   & major=="Education major") %>% `$`(corr.star) %>% `[`(1)                          ,"    \\\\" 
          ) %>% cat(file=fname,sep="\n",append=TRUE)
    paste0(" & (",
           df %>% filter(loc=="Florida"  & adv=="Advanced Degree"  & occ=="Unrelated" & major=="Education major") %>% `$`(uncr.se)   %>% `[`(1) %>% specify_decimal(.,3) ,") & (",
           df %>% filter(loc=="Florida"  & adv=="Advanced Degree"  & occ=="Unrelated" & major=="Education major") %>% `$`(corr.se)   %>% `[`(1) %>% specify_decimal(.,3) ,") & (",
           df %>% filter(loc=="Florida"  & adv=="Advanced Degree"  & occ=="Related"   & major=="Education major") %>% `$`(uncr.se)   %>% `[`(1) %>% specify_decimal(.,3) ,") & (",
           df %>% filter(loc=="Florida"  & adv=="Advanced Degree"  & occ=="Related"   & major=="Education major") %>% `$`(corr.se)   %>% `[`(1) %>% specify_decimal(.,3) ,") & (",
           df %>% filter(loc=="NewYork"  & adv=="Advanced Degree"  & occ=="Unrelated" & major=="Education major") %>% `$`(uncr.se)   %>% `[`(1) %>% specify_decimal(.,3) ,") & (",
           df %>% filter(loc=="NewYork"  & adv=="Advanced Degree"  & occ=="Unrelated" & major=="Education major") %>% `$`(corr.se)   %>% `[`(1) %>% specify_decimal(.,3) ,") & (",
           df %>% filter(loc=="NewYork"  & adv=="Advanced Degree"  & occ=="Related"   & major=="Education major") %>% `$`(uncr.se)   %>% `[`(1) %>% specify_decimal(.,3) ,") & (",
           df %>% filter(loc=="NewYork"  & adv=="Advanced Degree"  & occ=="Related"   & major=="Education major") %>% `$`(corr.se)   %>% `[`(1) %>% specify_decimal(.,3) ,") & (",
           df %>% filter(loc=="Texas"    & adv=="Advanced Degree"  & occ=="Unrelated" & major=="Education major") %>% `$`(uncr.se)   %>% `[`(1) %>% specify_decimal(.,3) ,") & (",
           df %>% filter(loc=="Texas"    & adv=="Advanced Degree"  & occ=="Unrelated" & major=="Education major") %>% `$`(corr.se)   %>% `[`(1) %>% specify_decimal(.,3) ,") & (",
           df %>% filter(loc=="Texas"    & adv=="Advanced Degree"  & occ=="Related"   & major=="Education major") %>% `$`(uncr.se)   %>% `[`(1) %>% specify_decimal(.,3) ,") & (",
           df %>% filter(loc=="Texas"    & adv=="Advanced Degree"  & occ=="Related"   & major=="Education major") %>% `$`(corr.se)   %>% `[`(1) %>% specify_decimal(.,3) ,") \\\\" 
          ) %>% cat(file=fname,sep="\n",append=TRUE)
    # soc sci major, advanced degree
    paste0("\\qquad{}",
           df %>% filter(loc=="Florida"  & adv=="Advanced Degree"  & occ=="Unrelated" & major=="Social sciences major") %>% `$`(major)     %>% `[`(1)                          ," & ",
           df %>% filter(loc=="Florida"  & adv=="Advanced Degree"  & occ=="Unrelated" & major=="Social sciences major") %>% `$`(uncr.coef) %>% `[`(1) %>% specify_decimal(.,3) ,
           df %>% filter(loc=="Florida"  & adv=="Advanced Degree"  & occ=="Unrelated" & major=="Social sciences major") %>% `$`(uncr.star) %>% `[`(1)                          ," & ",
           df %>% filter(loc=="Florida"  & adv=="Advanced Degree"  & occ=="Unrelated" & major=="Social sciences major") %>% `$`(corr.coef) %>% `[`(1) %>% specify_decimal(.,3) ,
           df %>% filter(loc=="Florida"  & adv=="Advanced Degree"  & occ=="Unrelated" & major=="Social sciences major") %>% `$`(corr.star) %>% `[`(1)                          ," & ",
           df %>% filter(loc=="Florida"  & adv=="Advanced Degree"  & occ=="Related"   & major=="Social sciences major") %>% `$`(uncr.coef) %>% `[`(1) %>% specify_decimal(.,3) ,
           df %>% filter(loc=="Florida"  & adv=="Advanced Degree"  & occ=="Related"   & major=="Social sciences major") %>% `$`(uncr.star) %>% `[`(1)                          ," & ",
           df %>% filter(loc=="Florida"  & adv=="Advanced Degree"  & occ=="Related"   & major=="Social sciences major") %>% `$`(corr.coef) %>% `[`(1) %>% specify_decimal(.,3) ,
           df %>% filter(loc=="Florida"  & adv=="Advanced Degree"  & occ=="Related"   & major=="Social sciences major") %>% `$`(corr.star) %>% `[`(1)                          ," & ",
           df %>% filter(loc=="NewYork"  & adv=="Advanced Degree"  & occ=="Unrelated" & major=="Social sciences major") %>% `$`(uncr.coef) %>% `[`(1) %>% specify_decimal(.,3) ,
           df %>% filter(loc=="NewYork"  & adv=="Advanced Degree"  & occ=="Unrelated" & major=="Social sciences major") %>% `$`(uncr.star) %>% `[`(1)                          ," & ",
           df %>% filter(loc=="NewYork"  & adv=="Advanced Degree"  & occ=="Unrelated" & major=="Social sciences major") %>% `$`(corr.coef) %>% `[`(1) %>% specify_decimal(.,3) ,
           df %>% filter(loc=="NewYork"  & adv=="Advanced Degree"  & occ=="Unrelated" & major=="Social sciences major") %>% `$`(corr.star) %>% `[`(1)                          ," & ",
           df %>% filter(loc=="NewYork"  & adv=="Advanced Degree"  & occ=="Related"   & major=="Social sciences major") %>% `$`(uncr.coef) %>% `[`(1) %>% specify_decimal(.,3) ,
           df %>% filter(loc=="NewYork"  & adv=="Advanced Degree"  & occ=="Related"   & major=="Social sciences major") %>% `$`(uncr.star) %>% `[`(1)                          ," & ",
           df %>% filter(loc=="NewYork"  & adv=="Advanced Degree"  & occ=="Related"   & major=="Social sciences major") %>% `$`(corr.coef) %>% `[`(1) %>% specify_decimal(.,3) ,
           df %>% filter(loc=="NewYork"  & adv=="Advanced Degree"  & occ=="Related"   & major=="Social sciences major") %>% `$`(corr.star) %>% `[`(1)                          ," & ",
           df %>% filter(loc=="Texas"    & adv=="Advanced Degree"  & occ=="Unrelated" & major=="Social sciences major") %>% `$`(uncr.coef) %>% `[`(1) %>% specify_decimal(.,3) ,
           df %>% filter(loc=="Texas"    & adv=="Advanced Degree"  & occ=="Unrelated" & major=="Social sciences major") %>% `$`(uncr.star) %>% `[`(1)                          ," & ",
           df %>% filter(loc=="Texas"    & adv=="Advanced Degree"  & occ=="Unrelated" & major=="Social sciences major") %>% `$`(corr.coef) %>% `[`(1) %>% specify_decimal(.,3) ,
           df %>% filter(loc=="Texas"    & adv=="Advanced Degree"  & occ=="Unrelated" & major=="Social sciences major") %>% `$`(corr.star) %>% `[`(1)                          ," & ",
           df %>% filter(loc=="Texas"    & adv=="Advanced Degree"  & occ=="Related"   & major=="Social sciences major") %>% `$`(uncr.coef) %>% `[`(1) %>% specify_decimal(.,3) ,
           df %>% filter(loc=="Texas"    & adv=="Advanced Degree"  & occ=="Related"   & major=="Social sciences major") %>% `$`(uncr.star) %>% `[`(1)                          ," & ",
           df %>% filter(loc=="Texas"    & adv=="Advanced Degree"  & occ=="Related"   & major=="Social sciences major") %>% `$`(corr.coef) %>% `[`(1) %>% specify_decimal(.,3) ,
           df %>% filter(loc=="Texas"    & adv=="Advanced Degree"  & occ=="Related"   & major=="Social sciences major") %>% `$`(corr.star) %>% `[`(1)                          ,"    \\\\" 
          ) %>% cat(file=fname,sep="\n",append=TRUE)
    paste0(" & (",
           df %>% filter(loc=="Florida"  & adv=="Advanced Degree"  & occ=="Unrelated" & major=="Social sciences major") %>% `$`(uncr.se)   %>% `[`(1) %>% specify_decimal(.,3) ,") & (",
           df %>% filter(loc=="Florida"  & adv=="Advanced Degree"  & occ=="Unrelated" & major=="Social sciences major") %>% `$`(corr.se)   %>% `[`(1) %>% specify_decimal(.,3) ,") & (",
           df %>% filter(loc=="Florida"  & adv=="Advanced Degree"  & occ=="Related"   & major=="Social sciences major") %>% `$`(uncr.se)   %>% `[`(1) %>% specify_decimal(.,3) ,") & (",
           df %>% filter(loc=="Florida"  & adv=="Advanced Degree"  & occ=="Related"   & major=="Social sciences major") %>% `$`(corr.se)   %>% `[`(1) %>% specify_decimal(.,3) ,") & (",
           df %>% filter(loc=="NewYork"  & adv=="Advanced Degree"  & occ=="Unrelated" & major=="Social sciences major") %>% `$`(uncr.se)   %>% `[`(1) %>% specify_decimal(.,3) ,") & (",
           df %>% filter(loc=="NewYork"  & adv=="Advanced Degree"  & occ=="Unrelated" & major=="Social sciences major") %>% `$`(corr.se)   %>% `[`(1) %>% specify_decimal(.,3) ,") & (",
           df %>% filter(loc=="NewYork"  & adv=="Advanced Degree"  & occ=="Related"   & major=="Social sciences major") %>% `$`(uncr.se)   %>% `[`(1) %>% specify_decimal(.,3) ,") & (",
           df %>% filter(loc=="NewYork"  & adv=="Advanced Degree"  & occ=="Related"   & major=="Social sciences major") %>% `$`(corr.se)   %>% `[`(1) %>% specify_decimal(.,3) ,") & (",
           df %>% filter(loc=="Texas"    & adv=="Advanced Degree"  & occ=="Unrelated" & major=="Social sciences major") %>% `$`(uncr.se)   %>% `[`(1) %>% specify_decimal(.,3) ,") & (",
           df %>% filter(loc=="Texas"    & adv=="Advanced Degree"  & occ=="Unrelated" & major=="Social sciences major") %>% `$`(corr.se)   %>% `[`(1) %>% specify_decimal(.,3) ,") & (",
           df %>% filter(loc=="Texas"    & adv=="Advanced Degree"  & occ=="Related"   & major=="Social sciences major") %>% `$`(uncr.se)   %>% `[`(1) %>% specify_decimal(.,3) ,") & (",
           df %>% filter(loc=="Texas"    & adv=="Advanced Degree"  & occ=="Related"   & major=="Social sciences major") %>% `$`(corr.se)   %>% `[`(1) %>% specify_decimal(.,3) ,") \\\\" 
          ) %>% cat(file=fname,sep="\n",append=TRUE)
    # other major, advanced degree
    paste0("\\qquad{}",
           df %>% filter(loc=="Florida"  & adv=="Advanced Degree"  & occ=="Unrelated" & major=="Other major") %>% `$`(major)     %>% `[`(1)                          ," & ",
           df %>% filter(loc=="Florida"  & adv=="Advanced Degree"  & occ=="Unrelated" & major=="Other major") %>% `$`(uncr.coef) %>% `[`(1) %>% specify_decimal(.,3) ,
           df %>% filter(loc=="Florida"  & adv=="Advanced Degree"  & occ=="Unrelated" & major=="Other major") %>% `$`(uncr.star) %>% `[`(1)                          ," & ",
           df %>% filter(loc=="Florida"  & adv=="Advanced Degree"  & occ=="Unrelated" & major=="Other major") %>% `$`(corr.coef) %>% `[`(1) %>% specify_decimal(.,3) ,
           df %>% filter(loc=="Florida"  & adv=="Advanced Degree"  & occ=="Unrelated" & major=="Other major") %>% `$`(corr.star) %>% `[`(1)                          ," & ",
           df %>% filter(loc=="Florida"  & adv=="Advanced Degree"  & occ=="Related"   & major=="Other major") %>% `$`(uncr.coef) %>% `[`(1) %>% specify_decimal(.,3) ,
           df %>% filter(loc=="Florida"  & adv=="Advanced Degree"  & occ=="Related"   & major=="Other major") %>% `$`(uncr.star) %>% `[`(1)                          ," & ",
           df %>% filter(loc=="Florida"  & adv=="Advanced Degree"  & occ=="Related"   & major=="Other major") %>% `$`(corr.coef) %>% `[`(1) %>% specify_decimal(.,3) ,
           df %>% filter(loc=="Florida"  & adv=="Advanced Degree"  & occ=="Related"   & major=="Other major") %>% `$`(corr.star) %>% `[`(1)                          ," & ",
           df %>% filter(loc=="NewYork"  & adv=="Advanced Degree"  & occ=="Unrelated" & major=="Other major") %>% `$`(uncr.coef) %>% `[`(1) %>% specify_decimal(.,3) ,
           df %>% filter(loc=="NewYork"  & adv=="Advanced Degree"  & occ=="Unrelated" & major=="Other major") %>% `$`(uncr.star) %>% `[`(1)                          ," & ",
           df %>% filter(loc=="NewYork"  & adv=="Advanced Degree"  & occ=="Unrelated" & major=="Other major") %>% `$`(corr.coef) %>% `[`(1) %>% specify_decimal(.,3) ,
           df %>% filter(loc=="NewYork"  & adv=="Advanced Degree"  & occ=="Unrelated" & major=="Other major") %>% `$`(corr.star) %>% `[`(1)                          ," & ",
           df %>% filter(loc=="NewYork"  & adv=="Advanced Degree"  & occ=="Related"   & major=="Other major") %>% `$`(uncr.coef) %>% `[`(1) %>% specify_decimal(.,3) ,
           df %>% filter(loc=="NewYork"  & adv=="Advanced Degree"  & occ=="Related"   & major=="Other major") %>% `$`(uncr.star) %>% `[`(1)                          ," & ",
           df %>% filter(loc=="NewYork"  & adv=="Advanced Degree"  & occ=="Related"   & major=="Other major") %>% `$`(corr.coef) %>% `[`(1) %>% specify_decimal(.,3) ,
           df %>% filter(loc=="NewYork"  & adv=="Advanced Degree"  & occ=="Related"   & major=="Other major") %>% `$`(corr.star) %>% `[`(1)                          ," & ",
           df %>% filter(loc=="Texas"    & adv=="Advanced Degree"  & occ=="Unrelated" & major=="Other major") %>% `$`(uncr.coef) %>% `[`(1) %>% specify_decimal(.,3) ,
           df %>% filter(loc=="Texas"    & adv=="Advanced Degree"  & occ=="Unrelated" & major=="Other major") %>% `$`(uncr.star) %>% `[`(1)                          ," & ",
           df %>% filter(loc=="Texas"    & adv=="Advanced Degree"  & occ=="Unrelated" & major=="Other major") %>% `$`(corr.coef) %>% `[`(1) %>% specify_decimal(.,3) ,
           df %>% filter(loc=="Texas"    & adv=="Advanced Degree"  & occ=="Unrelated" & major=="Other major") %>% `$`(corr.star) %>% `[`(1)                          ," & ",
           df %>% filter(loc=="Texas"    & adv=="Advanced Degree"  & occ=="Related"   & major=="Other major") %>% `$`(uncr.coef) %>% `[`(1) %>% specify_decimal(.,3) ,
           df %>% filter(loc=="Texas"    & adv=="Advanced Degree"  & occ=="Related"   & major=="Other major") %>% `$`(uncr.star) %>% `[`(1)                          ," & ",
           df %>% filter(loc=="Texas"    & adv=="Advanced Degree"  & occ=="Related"   & major=="Other major") %>% `$`(corr.coef) %>% `[`(1) %>% specify_decimal(.,3) ,
           df %>% filter(loc=="Texas"    & adv=="Advanced Degree"  & occ=="Related"   & major=="Other major") %>% `$`(corr.star) %>% `[`(1)                          ,"    \\\\" 
          ) %>% cat(file=fname,sep="\n",append=TRUE)
    paste0(" & (",
           df %>% filter(loc=="Florida"  & adv=="Advanced Degree"  & occ=="Unrelated" & major=="Other major") %>% `$`(uncr.se)   %>% `[`(1) %>% specify_decimal(.,3) ,") & (",
           df %>% filter(loc=="Florida"  & adv=="Advanced Degree"  & occ=="Unrelated" & major=="Other major") %>% `$`(corr.se)   %>% `[`(1) %>% specify_decimal(.,3) ,") & (",
           df %>% filter(loc=="Florida"  & adv=="Advanced Degree"  & occ=="Related"   & major=="Other major") %>% `$`(uncr.se)   %>% `[`(1) %>% specify_decimal(.,3) ,") & (",
           df %>% filter(loc=="Florida"  & adv=="Advanced Degree"  & occ=="Related"   & major=="Other major") %>% `$`(corr.se)   %>% `[`(1) %>% specify_decimal(.,3) ,") & (",
           df %>% filter(loc=="NewYork"  & adv=="Advanced Degree"  & occ=="Unrelated" & major=="Other major") %>% `$`(uncr.se)   %>% `[`(1) %>% specify_decimal(.,3) ,") & (",
           df %>% filter(loc=="NewYork"  & adv=="Advanced Degree"  & occ=="Unrelated" & major=="Other major") %>% `$`(corr.se)   %>% `[`(1) %>% specify_decimal(.,3) ,") & (",
           df %>% filter(loc=="NewYork"  & adv=="Advanced Degree"  & occ=="Related"   & major=="Other major") %>% `$`(uncr.se)   %>% `[`(1) %>% specify_decimal(.,3) ,") & (",
           df %>% filter(loc=="NewYork"  & adv=="Advanced Degree"  & occ=="Related"   & major=="Other major") %>% `$`(corr.se)   %>% `[`(1) %>% specify_decimal(.,3) ,") & (",
           df %>% filter(loc=="Texas"    & adv=="Advanced Degree"  & occ=="Unrelated" & major=="Other major") %>% `$`(uncr.se)   %>% `[`(1) %>% specify_decimal(.,3) ,") & (",
           df %>% filter(loc=="Texas"    & adv=="Advanced Degree"  & occ=="Unrelated" & major=="Other major") %>% `$`(corr.se)   %>% `[`(1) %>% specify_decimal(.,3) ,") & (",
           df %>% filter(loc=="Texas"    & adv=="Advanced Degree"  & occ=="Related"   & major=="Other major") %>% `$`(uncr.se)   %>% `[`(1) %>% specify_decimal(.,3) ,") & (",
           df %>% filter(loc=="Texas"    & adv=="Advanced Degree"  & occ=="Related"   & major=="Other major") %>% `$`(corr.se)   %>% `[`(1) %>% specify_decimal(.,3) ,") \\\\" 
          ) %>% cat(file=fname,sep="\n",append=TRUE)
    # business major, advanced degree
    paste0("\\qquad{}",
           df %>% filter(loc=="Florida"  & adv=="Advanced Degree"  & occ=="Unrelated" & major=="Business major") %>% `$`(major)     %>% `[`(1)                          ," & ",
           df %>% filter(loc=="Florida"  & adv=="Advanced Degree"  & occ=="Unrelated" & major=="Business major") %>% `$`(uncr.coef) %>% `[`(1) %>% specify_decimal(.,3) ,
           df %>% filter(loc=="Florida"  & adv=="Advanced Degree"  & occ=="Unrelated" & major=="Business major") %>% `$`(uncr.star) %>% `[`(1)                          ," & ",
           df %>% filter(loc=="Florida"  & adv=="Advanced Degree"  & occ=="Unrelated" & major=="Business major") %>% `$`(corr.coef) %>% `[`(1) %>% specify_decimal(.,3) ,
           df %>% filter(loc=="Florida"  & adv=="Advanced Degree"  & occ=="Unrelated" & major=="Business major") %>% `$`(corr.star) %>% `[`(1)                          ," & ",
           df %>% filter(loc=="Florida"  & adv=="Advanced Degree"  & occ=="Related"   & major=="Business major") %>% `$`(uncr.coef) %>% `[`(1) %>% specify_decimal(.,3) ,
           df %>% filter(loc=="Florida"  & adv=="Advanced Degree"  & occ=="Related"   & major=="Business major") %>% `$`(uncr.star) %>% `[`(1)                          ," & ",
           df %>% filter(loc=="Florida"  & adv=="Advanced Degree"  & occ=="Related"   & major=="Business major") %>% `$`(corr.coef) %>% `[`(1) %>% specify_decimal(.,3) ,
           df %>% filter(loc=="Florida"  & adv=="Advanced Degree"  & occ=="Related"   & major=="Business major") %>% `$`(corr.star) %>% `[`(1)                          ," & ",
           df %>% filter(loc=="NewYork"  & adv=="Advanced Degree"  & occ=="Unrelated" & major=="Business major") %>% `$`(uncr.coef) %>% `[`(1) %>% specify_decimal(.,3) ,
           df %>% filter(loc=="NewYork"  & adv=="Advanced Degree"  & occ=="Unrelated" & major=="Business major") %>% `$`(uncr.star) %>% `[`(1)                          ," & ",
           df %>% filter(loc=="NewYork"  & adv=="Advanced Degree"  & occ=="Unrelated" & major=="Business major") %>% `$`(corr.coef) %>% `[`(1) %>% specify_decimal(.,3) ,
           df %>% filter(loc=="NewYork"  & adv=="Advanced Degree"  & occ=="Unrelated" & major=="Business major") %>% `$`(corr.star) %>% `[`(1)                          ," & ",
           df %>% filter(loc=="NewYork"  & adv=="Advanced Degree"  & occ=="Related"   & major=="Business major") %>% `$`(uncr.coef) %>% `[`(1) %>% specify_decimal(.,3) ,
           df %>% filter(loc=="NewYork"  & adv=="Advanced Degree"  & occ=="Related"   & major=="Business major") %>% `$`(uncr.star) %>% `[`(1)                          ," & ",
           df %>% filter(loc=="NewYork"  & adv=="Advanced Degree"  & occ=="Related"   & major=="Business major") %>% `$`(corr.coef) %>% `[`(1) %>% specify_decimal(.,3) ,
           df %>% filter(loc=="NewYork"  & adv=="Advanced Degree"  & occ=="Related"   & major=="Business major") %>% `$`(corr.star) %>% `[`(1)                          ," & ",
           df %>% filter(loc=="Texas"    & adv=="Advanced Degree"  & occ=="Unrelated" & major=="Business major") %>% `$`(uncr.coef) %>% `[`(1) %>% specify_decimal(.,3) ,
           df %>% filter(loc=="Texas"    & adv=="Advanced Degree"  & occ=="Unrelated" & major=="Business major") %>% `$`(uncr.star) %>% `[`(1)                          ," & ",
           df %>% filter(loc=="Texas"    & adv=="Advanced Degree"  & occ=="Unrelated" & major=="Business major") %>% `$`(corr.coef) %>% `[`(1) %>% specify_decimal(.,3) ,
           df %>% filter(loc=="Texas"    & adv=="Advanced Degree"  & occ=="Unrelated" & major=="Business major") %>% `$`(corr.star) %>% `[`(1)                          ," & ",
           df %>% filter(loc=="Texas"    & adv=="Advanced Degree"  & occ=="Related"   & major=="Business major") %>% `$`(uncr.coef) %>% `[`(1) %>% specify_decimal(.,3) ,
           df %>% filter(loc=="Texas"    & adv=="Advanced Degree"  & occ=="Related"   & major=="Business major") %>% `$`(uncr.star) %>% `[`(1)                          ," & ",
           df %>% filter(loc=="Texas"    & adv=="Advanced Degree"  & occ=="Related"   & major=="Business major") %>% `$`(corr.coef) %>% `[`(1) %>% specify_decimal(.,3) ,
           df %>% filter(loc=="Texas"    & adv=="Advanced Degree"  & occ=="Related"   & major=="Business major") %>% `$`(corr.star) %>% `[`(1)                          ,"    \\\\" 
          ) %>% cat(file=fname,sep="\n",append=TRUE)
    paste0(" & (",
           df %>% filter(loc=="Florida"  & adv=="Advanced Degree"  & occ=="Unrelated" & major=="Business major") %>% `$`(uncr.se)   %>% `[`(1) %>% specify_decimal(.,3) ,") & (",
           df %>% filter(loc=="Florida"  & adv=="Advanced Degree"  & occ=="Unrelated" & major=="Business major") %>% `$`(corr.se)   %>% `[`(1) %>% specify_decimal(.,3) ,") & (",
           df %>% filter(loc=="Florida"  & adv=="Advanced Degree"  & occ=="Related"   & major=="Business major") %>% `$`(uncr.se)   %>% `[`(1) %>% specify_decimal(.,3) ,") & (",
           df %>% filter(loc=="Florida"  & adv=="Advanced Degree"  & occ=="Related"   & major=="Business major") %>% `$`(corr.se)   %>% `[`(1) %>% specify_decimal(.,3) ,") & (",
           df %>% filter(loc=="NewYork"  & adv=="Advanced Degree"  & occ=="Unrelated" & major=="Business major") %>% `$`(uncr.se)   %>% `[`(1) %>% specify_decimal(.,3) ,") & (",
           df %>% filter(loc=="NewYork"  & adv=="Advanced Degree"  & occ=="Unrelated" & major=="Business major") %>% `$`(corr.se)   %>% `[`(1) %>% specify_decimal(.,3) ,") & (",
           df %>% filter(loc=="NewYork"  & adv=="Advanced Degree"  & occ=="Related"   & major=="Business major") %>% `$`(uncr.se)   %>% `[`(1) %>% specify_decimal(.,3) ,") & (",
           df %>% filter(loc=="NewYork"  & adv=="Advanced Degree"  & occ=="Related"   & major=="Business major") %>% `$`(corr.se)   %>% `[`(1) %>% specify_decimal(.,3) ,") & (",
           df %>% filter(loc=="Texas"    & adv=="Advanced Degree"  & occ=="Unrelated" & major=="Business major") %>% `$`(uncr.se)   %>% `[`(1) %>% specify_decimal(.,3) ,") & (",
           df %>% filter(loc=="Texas"    & adv=="Advanced Degree"  & occ=="Unrelated" & major=="Business major") %>% `$`(corr.se)   %>% `[`(1) %>% specify_decimal(.,3) ,") & (",
           df %>% filter(loc=="Texas"    & adv=="Advanced Degree"  & occ=="Related"   & major=="Business major") %>% `$`(uncr.se)   %>% `[`(1) %>% specify_decimal(.,3) ,") & (",
           df %>% filter(loc=="Texas"    & adv=="Advanced Degree"  & occ=="Related"   & major=="Business major") %>% `$`(corr.se)   %>% `[`(1) %>% specify_decimal(.,3) ,") \\\\" 
          ) %>% cat(file=fname,sep="\n",append=TRUE)
    # STEM major, advanced degree
    paste0("\\qquad{}",
           df %>% filter(loc=="Florida"  & adv=="Advanced Degree"  & occ=="Unrelated" & major=="STEM major") %>% `$`(major)     %>% `[`(1)                          ," & ",
           df %>% filter(loc=="Florida"  & adv=="Advanced Degree"  & occ=="Unrelated" & major=="STEM major") %>% `$`(uncr.coef) %>% `[`(1) %>% specify_decimal(.,3) ,
           df %>% filter(loc=="Florida"  & adv=="Advanced Degree"  & occ=="Unrelated" & major=="STEM major") %>% `$`(uncr.star) %>% `[`(1)                          ," & ",
           df %>% filter(loc=="Florida"  & adv=="Advanced Degree"  & occ=="Unrelated" & major=="STEM major") %>% `$`(corr.coef) %>% `[`(1) %>% specify_decimal(.,3) ,
           df %>% filter(loc=="Florida"  & adv=="Advanced Degree"  & occ=="Unrelated" & major=="STEM major") %>% `$`(corr.star) %>% `[`(1)                          ," & ",
           df %>% filter(loc=="Florida"  & adv=="Advanced Degree"  & occ=="Related"   & major=="STEM major") %>% `$`(uncr.coef) %>% `[`(1) %>% specify_decimal(.,3) ,
           df %>% filter(loc=="Florida"  & adv=="Advanced Degree"  & occ=="Related"   & major=="STEM major") %>% `$`(uncr.star) %>% `[`(1)                          ," & ",
           df %>% filter(loc=="Florida"  & adv=="Advanced Degree"  & occ=="Related"   & major=="STEM major") %>% `$`(corr.coef) %>% `[`(1) %>% specify_decimal(.,3) ,
           df %>% filter(loc=="Florida"  & adv=="Advanced Degree"  & occ=="Related"   & major=="STEM major") %>% `$`(corr.star) %>% `[`(1)                          ," & ",
           df %>% filter(loc=="NewYork"  & adv=="Advanced Degree"  & occ=="Unrelated" & major=="STEM major") %>% `$`(uncr.coef) %>% `[`(1) %>% specify_decimal(.,3) ,
           df %>% filter(loc=="NewYork"  & adv=="Advanced Degree"  & occ=="Unrelated" & major=="STEM major") %>% `$`(uncr.star) %>% `[`(1)                          ," & ",
           df %>% filter(loc=="NewYork"  & adv=="Advanced Degree"  & occ=="Unrelated" & major=="STEM major") %>% `$`(corr.coef) %>% `[`(1) %>% specify_decimal(.,3) ,
           df %>% filter(loc=="NewYork"  & adv=="Advanced Degree"  & occ=="Unrelated" & major=="STEM major") %>% `$`(corr.star) %>% `[`(1)                          ," & ",
           df %>% filter(loc=="NewYork"  & adv=="Advanced Degree"  & occ=="Related"   & major=="STEM major") %>% `$`(uncr.coef) %>% `[`(1) %>% specify_decimal(.,3) ,
           df %>% filter(loc=="NewYork"  & adv=="Advanced Degree"  & occ=="Related"   & major=="STEM major") %>% `$`(uncr.star) %>% `[`(1)                          ," & ",
           df %>% filter(loc=="NewYork"  & adv=="Advanced Degree"  & occ=="Related"   & major=="STEM major") %>% `$`(corr.coef) %>% `[`(1) %>% specify_decimal(.,3) ,
           df %>% filter(loc=="NewYork"  & adv=="Advanced Degree"  & occ=="Related"   & major=="STEM major") %>% `$`(corr.star) %>% `[`(1)                          ," & ",
           df %>% filter(loc=="Texas"    & adv=="Advanced Degree"  & occ=="Unrelated" & major=="STEM major") %>% `$`(uncr.coef) %>% `[`(1) %>% specify_decimal(.,3) ,
           df %>% filter(loc=="Texas"    & adv=="Advanced Degree"  & occ=="Unrelated" & major=="STEM major") %>% `$`(uncr.star) %>% `[`(1)                          ," & ",
           df %>% filter(loc=="Texas"    & adv=="Advanced Degree"  & occ=="Unrelated" & major=="STEM major") %>% `$`(corr.coef) %>% `[`(1) %>% specify_decimal(.,3) ,
           df %>% filter(loc=="Texas"    & adv=="Advanced Degree"  & occ=="Unrelated" & major=="STEM major") %>% `$`(corr.star) %>% `[`(1)                          ," & ",
           df %>% filter(loc=="Texas"    & adv=="Advanced Degree"  & occ=="Related"   & major=="STEM major") %>% `$`(uncr.coef) %>% `[`(1) %>% specify_decimal(.,3) ,
           df %>% filter(loc=="Texas"    & adv=="Advanced Degree"  & occ=="Related"   & major=="STEM major") %>% `$`(uncr.star) %>% `[`(1)                          ," & ",
           df %>% filter(loc=="Texas"    & adv=="Advanced Degree"  & occ=="Related"   & major=="STEM major") %>% `$`(corr.coef) %>% `[`(1) %>% specify_decimal(.,3) ,
           df %>% filter(loc=="Texas"    & adv=="Advanced Degree"  & occ=="Related"   & major=="STEM major") %>% `$`(corr.star) %>% `[`(1)                          ,"    \\\\" 
          ) %>% cat(file=fname,sep="\n",append=TRUE)
    paste0(" & (",
           df %>% filter(loc=="Florida"  & adv=="Advanced Degree"  & occ=="Unrelated" & major=="STEM major") %>% `$`(uncr.se)   %>% `[`(1) %>% specify_decimal(.,3) ,") & (",
           df %>% filter(loc=="Florida"  & adv=="Advanced Degree"  & occ=="Unrelated" & major=="STEM major") %>% `$`(corr.se)   %>% `[`(1) %>% specify_decimal(.,3) ,") & (",
           df %>% filter(loc=="Florida"  & adv=="Advanced Degree"  & occ=="Related"   & major=="STEM major") %>% `$`(uncr.se)   %>% `[`(1) %>% specify_decimal(.,3) ,") & (",
           df %>% filter(loc=="Florida"  & adv=="Advanced Degree"  & occ=="Related"   & major=="STEM major") %>% `$`(corr.se)   %>% `[`(1) %>% specify_decimal(.,3) ,") & (",
           df %>% filter(loc=="NewYork"  & adv=="Advanced Degree"  & occ=="Unrelated" & major=="STEM major") %>% `$`(uncr.se)   %>% `[`(1) %>% specify_decimal(.,3) ,") & (",
           df %>% filter(loc=="NewYork"  & adv=="Advanced Degree"  & occ=="Unrelated" & major=="STEM major") %>% `$`(corr.se)   %>% `[`(1) %>% specify_decimal(.,3) ,") & (",
           df %>% filter(loc=="NewYork"  & adv=="Advanced Degree"  & occ=="Related"   & major=="STEM major") %>% `$`(uncr.se)   %>% `[`(1) %>% specify_decimal(.,3) ,") & (",
           df %>% filter(loc=="NewYork"  & adv=="Advanced Degree"  & occ=="Related"   & major=="STEM major") %>% `$`(corr.se)   %>% `[`(1) %>% specify_decimal(.,3) ,") & (",
           df %>% filter(loc=="Texas"    & adv=="Advanced Degree"  & occ=="Unrelated" & major=="STEM major") %>% `$`(uncr.se)   %>% `[`(1) %>% specify_decimal(.,3) ,") & (",
           df %>% filter(loc=="Texas"    & adv=="Advanced Degree"  & occ=="Unrelated" & major=="STEM major") %>% `$`(corr.se)   %>% `[`(1) %>% specify_decimal(.,3) ,") & (",
           df %>% filter(loc=="Texas"    & adv=="Advanced Degree"  & occ=="Related"   & major=="STEM major") %>% `$`(uncr.se)   %>% `[`(1) %>% specify_decimal(.,3) ,") & (",
           df %>% filter(loc=="Texas"    & adv=="Advanced Degree"  & occ=="Related"   & major=="STEM major") %>% `$`(corr.se)   %>% `[`(1) %>% specify_decimal(.,3) ,") \\\\" 
          ) %>% cat(file=fname,sep="\n",append=TRUE)
    # born here
    paste0("",
           df %>% filter(loc=="Florida"  & adv==""                 & occ=="Unrelated" & major=="Born here") %>% `$`(major)     %>% `[`(1)                          ," & ",
           df %>% filter(loc=="Florida"  & adv==""                 & occ=="Unrelated" & major=="Born here") %>% `$`(uncr.coef) %>% `[`(1) %>% specify_decimal(.,3) ,
           df %>% filter(loc=="Florida"  & adv==""                 & occ=="Unrelated" & major=="Born here") %>% `$`(uncr.star) %>% `[`(1)                          ," & ",
           df %>% filter(loc=="Florida"  & adv==""                 & occ=="Unrelated" & major=="Born here") %>% `$`(corr.coef) %>% `[`(1) %>% specify_decimal(.,3) ,
           df %>% filter(loc=="Florida"  & adv==""                 & occ=="Unrelated" & major=="Born here") %>% `$`(corr.star) %>% `[`(1)                          ," & ",
           df %>% filter(loc=="Florida"  & adv==""                 & occ=="Related"   & major=="Born here") %>% `$`(uncr.coef) %>% `[`(1) %>% specify_decimal(.,3) ,
           df %>% filter(loc=="Florida"  & adv==""                 & occ=="Related"   & major=="Born here") %>% `$`(uncr.star) %>% `[`(1)                          ," & ",
           df %>% filter(loc=="Florida"  & adv==""                 & occ=="Related"   & major=="Born here") %>% `$`(corr.coef) %>% `[`(1) %>% specify_decimal(.,3) ,
           df %>% filter(loc=="Florida"  & adv==""                 & occ=="Related"   & major=="Born here") %>% `$`(corr.star) %>% `[`(1)                          ," & ",
           df %>% filter(loc=="NewYork"  & adv==""                 & occ=="Unrelated" & major=="Born here") %>% `$`(uncr.coef) %>% `[`(1) %>% specify_decimal(.,3) ,
           df %>% filter(loc=="NewYork"  & adv==""                 & occ=="Unrelated" & major=="Born here") %>% `$`(uncr.star) %>% `[`(1)                          ," & ",
           df %>% filter(loc=="NewYork"  & adv==""                 & occ=="Unrelated" & major=="Born here") %>% `$`(corr.coef) %>% `[`(1) %>% specify_decimal(.,3) ,
           df %>% filter(loc=="NewYork"  & adv==""                 & occ=="Unrelated" & major=="Born here") %>% `$`(corr.star) %>% `[`(1)                          ," & ",
           df %>% filter(loc=="NewYork"  & adv==""                 & occ=="Related"   & major=="Born here") %>% `$`(uncr.coef) %>% `[`(1) %>% specify_decimal(.,3) ,
           df %>% filter(loc=="NewYork"  & adv==""                 & occ=="Related"   & major=="Born here") %>% `$`(uncr.star) %>% `[`(1)                          ," & ",
           df %>% filter(loc=="NewYork"  & adv==""                 & occ=="Related"   & major=="Born here") %>% `$`(corr.coef) %>% `[`(1) %>% specify_decimal(.,3) ,
           df %>% filter(loc=="NewYork"  & adv==""                 & occ=="Related"   & major=="Born here") %>% `$`(corr.star) %>% `[`(1)                          ," & ",
           df %>% filter(loc=="Texas"    & adv==""                 & occ=="Unrelated" & major=="Born here") %>% `$`(uncr.coef) %>% `[`(1) %>% specify_decimal(.,3) ,
           df %>% filter(loc=="Texas"    & adv==""                 & occ=="Unrelated" & major=="Born here") %>% `$`(uncr.star) %>% `[`(1)                          ," & ",
           df %>% filter(loc=="Texas"    & adv==""                 & occ=="Unrelated" & major=="Born here") %>% `$`(corr.coef) %>% `[`(1) %>% specify_decimal(.,3) ,
           df %>% filter(loc=="Texas"    & adv==""                 & occ=="Unrelated" & major=="Born here") %>% `$`(corr.star) %>% `[`(1)                          ," & ",
           df %>% filter(loc=="Texas"    & adv==""                 & occ=="Related"   & major=="Born here") %>% `$`(uncr.coef) %>% `[`(1) %>% specify_decimal(.,3) ,
           df %>% filter(loc=="Texas"    & adv==""                 & occ=="Related"   & major=="Born here") %>% `$`(uncr.star) %>% `[`(1)                          ," & ",
           df %>% filter(loc=="Texas"    & adv==""                 & occ=="Related"   & major=="Born here") %>% `$`(corr.coef) %>% `[`(1) %>% specify_decimal(.,3) ,
           df %>% filter(loc=="Texas"    & adv==""                 & occ=="Related"   & major=="Born here") %>% `$`(corr.star) %>% `[`(1)                          ,"    \\\\" 
          ) %>% cat(file=fname,sep="\n",append=TRUE)
    paste0(" & (",
           df %>% filter(loc=="Florida"  & adv==""                 & occ=="Unrelated" & major=="Born here") %>% `$`(uncr.se)   %>% `[`(1) %>% specify_decimal(.,3) ,") & (",
           df %>% filter(loc=="Florida"  & adv==""                 & occ=="Unrelated" & major=="Born here") %>% `$`(corr.se)   %>% `[`(1) %>% specify_decimal(.,3) ,") & (",
           df %>% filter(loc=="Florida"  & adv==""                 & occ=="Related"   & major=="Born here") %>% `$`(uncr.se)   %>% `[`(1) %>% specify_decimal(.,3) ,") & (",
           df %>% filter(loc=="Florida"  & adv==""                 & occ=="Related"   & major=="Born here") %>% `$`(corr.se)   %>% `[`(1) %>% specify_decimal(.,3) ,") & (",
           df %>% filter(loc=="NewYork"  & adv==""                 & occ=="Unrelated" & major=="Born here") %>% `$`(uncr.se)   %>% `[`(1) %>% specify_decimal(.,3) ,") & (",
           df %>% filter(loc=="NewYork"  & adv==""                 & occ=="Unrelated" & major=="Born here") %>% `$`(corr.se)   %>% `[`(1) %>% specify_decimal(.,3) ,") & (",
           df %>% filter(loc=="NewYork"  & adv==""                 & occ=="Related"   & major=="Born here") %>% `$`(uncr.se)   %>% `[`(1) %>% specify_decimal(.,3) ,") & (",
           df %>% filter(loc=="NewYork"  & adv==""                 & occ=="Related"   & major=="Born here") %>% `$`(corr.se)   %>% `[`(1) %>% specify_decimal(.,3) ,") & (",
           df %>% filter(loc=="Texas"    & adv==""                 & occ=="Unrelated" & major=="Born here") %>% `$`(uncr.se)   %>% `[`(1) %>% specify_decimal(.,3) ,") & (",
           df %>% filter(loc=="Texas"    & adv==""                 & occ=="Unrelated" & major=="Born here") %>% `$`(corr.se)   %>% `[`(1) %>% specify_decimal(.,3) ,") & (",
           df %>% filter(loc=="Texas"    & adv==""                 & occ=="Related"   & major=="Born here") %>% `$`(uncr.se)   %>% `[`(1) %>% specify_decimal(.,3) ,") & (",
           df %>% filter(loc=="Texas"    & adv==""                 & occ=="Related"   & major=="Born here") %>% `$`(corr.se)   %>% `[`(1) %>% specify_decimal(.,3) ,") \\\\" 
          ) %>% cat(file=fname,sep="\n",append=TRUE)
    cat("Cubic in experience & $\\checkmark$ & $\\checkmark$ & $\\checkmark$ & $\\checkmark$ & $\\checkmark$ & $\\checkmark$ & $\\checkmark$ & $\\checkmark$ & $\\checkmark$ & $\\checkmark$ & $\\checkmark$ & $\\checkmark$\\\\",file=fname,sep="\n",append=TRUE)
    cat("Demographics        & $\\checkmark$ & $\\checkmark$ & $\\checkmark$ & $\\checkmark$ & $\\checkmark$ & $\\checkmark$ & $\\checkmark$ & $\\checkmark$ & $\\checkmark$ & $\\checkmark$ & $\\checkmark$ & $\\checkmark$\\\\",file=fname,sep="\n",append=TRUE)
    cat("CBSA fixed effects  & $\\checkmark$ & $\\checkmark$ & $\\checkmark$ & $\\checkmark$ & $\\checkmark$ & $\\checkmark$ & $\\checkmark$ & $\\checkmark$ & $\\checkmark$ & $\\checkmark$ & $\\checkmark$ & $\\checkmark$\\\\",file=fname,sep="\n",append=TRUE)
    paste0("$F$ test for $\\lambda$ terms   & & ",
           df %>% filter(loc=="Florida"  & occ=="Unrelated") %>% `$`(corr.f) %>% `[`(1) %>% specify_decimal(.,3) ," & & ",
           df %>% filter(loc=="Florida"  & occ=="Related"  ) %>% `$`(corr.f) %>% `[`(1) %>% specify_decimal(.,3) ," & & ",
           df %>% filter(loc=="NewYork"  & occ=="Unrelated") %>% `$`(corr.f) %>% `[`(1) %>% specify_decimal(.,3) ," & & ",
           df %>% filter(loc=="NewYork"  & occ=="Related"  ) %>% `$`(corr.f) %>% `[`(1) %>% specify_decimal(.,3) ," & & ",
           df %>% filter(loc=="Texas"    & occ=="Unrelated") %>% `$`(corr.f) %>% `[`(1) %>% specify_decimal(.,3) ," & & ",
           df %>% filter(loc=="Texas"    & occ=="Related"  ) %>% `$`(corr.f) %>% `[`(1) %>% specify_decimal(.,3) ," \\\\" 
          ) %>% cat(file=fname,sep="\n",append=TRUE)
    paste0("                                & & {[}",
           df %>% filter(loc=="Florida"  & occ=="Unrelated") %>% `$`(f.pval) %>% `[`(1) %>% specify_decimal(.,3) ,"{]} & & {[}",
           df %>% filter(loc=="Florida"  & occ=="Related"  ) %>% `$`(f.pval) %>% `[`(1) %>% specify_decimal(.,3) ,"{]} & & {[}",
           df %>% filter(loc=="NewYork"  & occ=="Unrelated") %>% `$`(f.pval) %>% `[`(1) %>% specify_decimal(.,3) ,"{]} & & {[}",
           df %>% filter(loc=="NewYork"  & occ=="Related"  ) %>% `$`(f.pval) %>% `[`(1) %>% specify_decimal(.,3) ,"{]} & & {[}",
           df %>% filter(loc=="Texas"    & occ=="Unrelated") %>% `$`(f.pval) %>% `[`(1) %>% specify_decimal(.,3) ,"{]} & & {[}",
           df %>% filter(loc=="Texas"    & occ=="Related"  ) %>% `$`(f.pval) %>% `[`(1) %>% specify_decimal(.,3) ,"{]} \\\\" 
          ) %>% cat(file=fname,sep="\n",append=TRUE)
    cat("\\midrule"                                                                                                      ,file=fname,sep="\n",append=TRUE)
    paste0("$R^2$                           & ",
           df %>% filter(loc=="Florida"  & occ=="Unrelated") %>% `$`(uncr.r2) %>% `[`(1) %>% specify_decimal(.,3) ," & ",
           df %>% filter(loc=="Florida"  & occ=="Unrelated") %>% `$`(corr.r2) %>% `[`(1) %>% specify_decimal(.,3) ," & ",
           df %>% filter(loc=="Florida"  & occ=="Related"  ) %>% `$`(uncr.r2) %>% `[`(1) %>% specify_decimal(.,3) ," & ",
           df %>% filter(loc=="Florida"  & occ=="Related"  ) %>% `$`(corr.r2) %>% `[`(1) %>% specify_decimal(.,3) ," & ",
           df %>% filter(loc=="NewYork"  & occ=="Unrelated") %>% `$`(uncr.r2) %>% `[`(1) %>% specify_decimal(.,3) ," & ",
           df %>% filter(loc=="NewYork"  & occ=="Unrelated") %>% `$`(corr.r2) %>% `[`(1) %>% specify_decimal(.,3) ," & ",
           df %>% filter(loc=="NewYork"  & occ=="Related"  ) %>% `$`(uncr.r2) %>% `[`(1) %>% specify_decimal(.,3) ," & ",
           df %>% filter(loc=="NewYork"  & occ=="Related"  ) %>% `$`(corr.r2) %>% `[`(1) %>% specify_decimal(.,3) ," & ",
           df %>% filter(loc=="Texas"    & occ=="Unrelated") %>% `$`(uncr.r2) %>% `[`(1) %>% specify_decimal(.,3) ," & ",
           df %>% filter(loc=="Texas"    & occ=="Unrelated") %>% `$`(corr.r2) %>% `[`(1) %>% specify_decimal(.,3) ," & ",
           df %>% filter(loc=="Texas"    & occ=="Related"  ) %>% `$`(uncr.r2) %>% `[`(1) %>% specify_decimal(.,3) ," & ",
           df %>% filter(loc=="Texas"    & occ=="Related"  ) %>% `$`(corr.r2) %>% `[`(1) %>% specify_decimal(.,3) ," \\\\" 
          ) %>% cat(file=fname,sep="\n",append=TRUE)
    paste0("Observations                    & ",
           df %>% filter(loc=="Florida"  & occ=="Unrelated") %>% `$`(uncr.nobs) %>% `[`(1) %>% format(big.mark=",") ," & ",
           df %>% filter(loc=="Florida"  & occ=="Unrelated") %>% `$`(corr.nobs) %>% `[`(1) %>% format(big.mark=",") ," & ",
           df %>% filter(loc=="Florida"  & occ=="Related"  ) %>% `$`(uncr.nobs) %>% `[`(1) %>% format(big.mark=",") ," & ",
           df %>% filter(loc=="Florida"  & occ=="Related"  ) %>% `$`(corr.nobs) %>% `[`(1) %>% format(big.mark=",") ," & ",
           df %>% filter(loc=="NewYork"  & occ=="Unrelated") %>% `$`(uncr.nobs) %>% `[`(1) %>% format(big.mark=",") ," & ",
           df %>% filter(loc=="NewYork"  & occ=="Unrelated") %>% `$`(corr.nobs) %>% `[`(1) %>% format(big.mark=",") ," & ",
           df %>% filter(loc=="NewYork"  & occ=="Related"  ) %>% `$`(uncr.nobs) %>% `[`(1) %>% format(big.mark=",") ," & ",
           df %>% filter(loc=="NewYork"  & occ=="Related"  ) %>% `$`(corr.nobs) %>% `[`(1) %>% format(big.mark=",") ," & ",
           df %>% filter(loc=="Texas"    & occ=="Unrelated") %>% `$`(uncr.nobs) %>% `[`(1) %>% format(big.mark=",") ," & ",
           df %>% filter(loc=="Texas"    & occ=="Unrelated") %>% `$`(corr.nobs) %>% `[`(1) %>% format(big.mark=",") ," & ",
           df %>% filter(loc=="Texas"    & occ=="Related"  ) %>% `$`(uncr.nobs) %>% `[`(1) %>% format(big.mark=",") ," & ",
           df %>% filter(loc=="Texas"    & occ=="Related"  ) %>% `$`(corr.nobs) %>% `[`(1) %>% format(big.mark=",") ," \\\\" 
          ) %>% cat(file=fname,sep="\n",append=TRUE)
    cat("\\bottomrule"                                                                                                   ,file=fname,sep="\n",append=TRUE) 
    cat("\\end{tabular}"                                                                                                 ,file=fname,sep="\n",append=TRUE) 
    cat(paste0("{\\footnotesize {\\raggedright Notes: Cubic in experience is fully interacted with advanced degree status. The wage return for an advanced degree holder is the sum of the bachelor's degree coefficient and the advanced degree interaction. Bootstrapped standard errors (",df$nboot[1]," replicates) are listed below coefficients in parentheses. $p$-values of statistical tests are listed below test statistics in brackets. *** p<0.01; ** p<0.05; * p<0.10.}}"),file=fname,sep="\n",append=TRUE)
    cat("\\end{threeparttable}"                                                                                          ,file=fname,sep="\n",append=TRUE)  
    cat("}"                                                                                                              ,file=fname,sep="\n",append=TRUE)
    cat("\\end{table}"                                                                                                   ,file=fname,sep="\n",append=TRUE)   
}

# write output to Table 4
maketable4 <- function(df) {
    df %<>% mutate(
                   major    = str_replace_all(major," major",""),
                   major    = str_replace_all(major,"Social sciences","Social Science"),
                   haus.sig = haus.p<0.05 & uncr.p<.05 & corr.p<.05 & abs(pctdiff/pctd.se)>=1.96 & abs(pctdiff)>10
                  ) %>%
            filter(major   != "Born here")
    fname <- "../Paper/Tables/table4.tex"
    cat("\\begin{table}[ht]"                                                                                             ,file=fname,sep="\n")
    cat("\\caption{Percentage change in returns when correcting for selection}"                                          ,file=fname,sep="\n",append=TRUE)
    cat("\\label{tab:aggpctreturn}"                                                                                      ,file=fname,sep="\n",append=TRUE)
    cat("\\centering"                                                                                                    ,file=fname,sep="\n",append=TRUE)
    cat("\\begin{threeparttable}"                                                                                        ,file=fname,sep="\n",append=TRUE)
    cat("\\begin{tabular}{lcccccccc}"                                                                                    ,file=fname,sep="\n",append=TRUE)
    cat("\\toprule"                                                                                                      ,file=fname,sep="\n",append=TRUE)
    cat("                & \\multicolumn{4}{c}{Unrelated occupation} & \\multicolumn{4}{c}{Related occupation} \\\\"     ,file=fname,sep="\n",append=TRUE) 
    cat("\\cmidrule(lr{.5em}){2-5}\\cmidrule(lr{.5em}){6-9}"                                                             ,file=fname,sep="\n",append=TRUE)
    cat("Major           & p10   & Median & p90 & No. significant  & p10   & Median & p90 & No. significant  \\\\ ",file=fname,sep="\n",append=TRUE)
    cat("\\midrule",file=fname,sep="\n",append=TRUE)
    cat("\\emph{Bachelor's degrees}       &&&&&& \\\\",file=fname,sep="\n",append=TRUE)
    # education major, no advanced degree
    paste0("\\qquad{}",
           df %>% filter(adv==""  & occ=="Unrelated" & major=="Education") %>% `$`(major)    %>% `[`(1)                                  ," & ",
           df %>% filter(adv==""  & occ=="Unrelated" & major=="Education") %>% `$`(pctdiff)  %>% quantile(0.1)  %>% specify_decimal(.,1) ," & ",
           df %>% filter(adv==""  & occ=="Unrelated" & major=="Education") %>% `$`(pctdiff)  %>% quantile(0.5)  %>% specify_decimal(.,1) ," & ",
           df %>% filter(adv==""  & occ=="Unrelated" & major=="Education") %>% `$`(pctdiff)  %>% quantile(0.9)  %>% specify_decimal(.,1) ," & ",
           df %>% filter(adv==""  & occ=="Unrelated" & major=="Education") %>% `$`(haus.sig) %>% sum(.)         %>% specify_decimal(.,0) ," & ",
           df %>% filter(adv==""  & occ=="Related"   & major=="Education") %>% `$`(pctdiff)  %>% quantile(0.1)  %>% specify_decimal(.,1) ," & ",
           df %>% filter(adv==""  & occ=="Related"   & major=="Education") %>% `$`(pctdiff)  %>% quantile(0.5)  %>% specify_decimal(.,1) ," & ",
           df %>% filter(adv==""  & occ=="Related"   & major=="Education") %>% `$`(pctdiff)  %>% quantile(0.9)  %>% specify_decimal(.,1) ," & ",
           df %>% filter(adv==""  & occ=="Related"   & major=="Education") %>% `$`(haus.sig) %>% sum(.)         %>% specify_decimal(.,0) ,"    \\\\"  
          ) %>% cat(file=fname,sep="\n",append=TRUE)
    # social science major, no advanced degree
    paste0("\\qquad{}",
           df %>% filter(adv==""  & occ=="Unrelated" & major=="Social Science") %>% `$`(major)    %>% `[`(1)                                  ," & ",
           df %>% filter(adv==""  & occ=="Unrelated" & major=="Social Science") %>% `$`(pctdiff)  %>% quantile(0.1)  %>% specify_decimal(.,1) ," & ",
           df %>% filter(adv==""  & occ=="Unrelated" & major=="Social Science") %>% `$`(pctdiff)  %>% quantile(0.5)  %>% specify_decimal(.,1) ," & ",
           df %>% filter(adv==""  & occ=="Unrelated" & major=="Social Science") %>% `$`(pctdiff)  %>% quantile(0.9)  %>% specify_decimal(.,1) ," & ",
           df %>% filter(adv==""  & occ=="Unrelated" & major=="Social Science") %>% `$`(haus.sig) %>% sum(.)         %>% specify_decimal(.,0) ," & ",
           df %>% filter(adv==""  & occ=="Related"   & major=="Social Science") %>% `$`(pctdiff)  %>% quantile(0.1)  %>% specify_decimal(.,1) ," & ",
           df %>% filter(adv==""  & occ=="Related"   & major=="Social Science") %>% `$`(pctdiff)  %>% quantile(0.5)  %>% specify_decimal(.,1) ," & ",
           df %>% filter(adv==""  & occ=="Related"   & major=="Social Science") %>% `$`(pctdiff)  %>% quantile(0.9)  %>% specify_decimal(.,1) ," & ",
           df %>% filter(adv==""  & occ=="Related"   & major=="Social Science") %>% `$`(haus.sig) %>% sum(.)         %>% specify_decimal(.,0) ,"    \\\\"  
          ) %>% cat(file=fname,sep="\n",append=TRUE)
    # other major, no advanced degree
    paste0("\\qquad{}",
           df %>% filter(adv==""  & occ=="Unrelated" & major=="Other") %>% `$`(major)    %>% `[`(1)                                  ," & ",
           df %>% filter(adv==""  & occ=="Unrelated" & major=="Other") %>% `$`(pctdiff)  %>% quantile(0.1)  %>% specify_decimal(.,1) ," & ",
           df %>% filter(adv==""  & occ=="Unrelated" & major=="Other") %>% `$`(pctdiff)  %>% quantile(0.5)  %>% specify_decimal(.,1) ," & ",
           df %>% filter(adv==""  & occ=="Unrelated" & major=="Other") %>% `$`(pctdiff)  %>% quantile(0.9)  %>% specify_decimal(.,1) ," & ",
           df %>% filter(adv==""  & occ=="Unrelated" & major=="Other") %>% `$`(haus.sig) %>% sum(.)         %>% specify_decimal(.,0) ," & ",
           df %>% filter(adv==""  & occ=="Related"   & major=="Other") %>% `$`(pctdiff)  %>% quantile(0.1)  %>% specify_decimal(.,1) ," & ",
           df %>% filter(adv==""  & occ=="Related"   & major=="Other") %>% `$`(pctdiff)  %>% quantile(0.5)  %>% specify_decimal(.,1) ," & ",
           df %>% filter(adv==""  & occ=="Related"   & major=="Other") %>% `$`(pctdiff)  %>% quantile(0.9)  %>% specify_decimal(.,1) ," & ",
           df %>% filter(adv==""  & occ=="Related"   & major=="Other") %>% `$`(haus.sig) %>% sum(.)         %>% specify_decimal(.,0) ,"    \\\\"  
          ) %>% cat(file=fname,sep="\n",append=TRUE)
    # business major, no advanced degree
    paste0("\\qquad{}",
           df %>% filter(adv==""  & occ=="Unrelated" & major=="Business") %>% `$`(major)    %>% `[`(1)                                  ," & ",
           df %>% filter(adv==""  & occ=="Unrelated" & major=="Business") %>% `$`(pctdiff)  %>% quantile(0.1)  %>% specify_decimal(.,1) ," & ",
           df %>% filter(adv==""  & occ=="Unrelated" & major=="Business") %>% `$`(pctdiff)  %>% quantile(0.5)  %>% specify_decimal(.,1) ," & ",
           df %>% filter(adv==""  & occ=="Unrelated" & major=="Business") %>% `$`(pctdiff)  %>% quantile(0.9)  %>% specify_decimal(.,1) ," & ",
           df %>% filter(adv==""  & occ=="Unrelated" & major=="Business") %>% `$`(haus.sig) %>% sum(.)         %>% specify_decimal(.,0) ," & ",
           df %>% filter(adv==""  & occ=="Related"   & major=="Business") %>% `$`(pctdiff)  %>% quantile(0.1)  %>% specify_decimal(.,1) ," & ",
           df %>% filter(adv==""  & occ=="Related"   & major=="Business") %>% `$`(pctdiff)  %>% quantile(0.5)  %>% specify_decimal(.,1) ," & ",
           df %>% filter(adv==""  & occ=="Related"   & major=="Business") %>% `$`(pctdiff)  %>% quantile(0.9)  %>% specify_decimal(.,1) ," & ",
           df %>% filter(adv==""  & occ=="Related"   & major=="Business") %>% `$`(haus.sig) %>% sum(.)         %>% specify_decimal(.,0) ,"    \\\\"  
          ) %>% cat(file=fname,sep="\n",append=TRUE)
    # STEM major, no advanced degree
    paste0("\\qquad{}",
           df %>% filter(adv==""  & occ=="Unrelated" & major=="STEM") %>% `$`(major)    %>% `[`(1)                                  ," & ",
           df %>% filter(adv==""  & occ=="Unrelated" & major=="STEM") %>% `$`(pctdiff)  %>% quantile(0.1)  %>% specify_decimal(.,1) ," & ",
           df %>% filter(adv==""  & occ=="Unrelated" & major=="STEM") %>% `$`(pctdiff)  %>% quantile(0.5)  %>% specify_decimal(.,1) ," & ",
           df %>% filter(adv==""  & occ=="Unrelated" & major=="STEM") %>% `$`(pctdiff)  %>% quantile(0.9)  %>% specify_decimal(.,1) ," & ",
           df %>% filter(adv==""  & occ=="Unrelated" & major=="STEM") %>% `$`(haus.sig) %>% sum(.)         %>% specify_decimal(.,0) ," & ",
           df %>% filter(adv==""  & occ=="Related"   & major=="STEM") %>% `$`(pctdiff)  %>% quantile(0.1)  %>% specify_decimal(.,1) ," & ",
           df %>% filter(adv==""  & occ=="Related"   & major=="STEM") %>% `$`(pctdiff)  %>% quantile(0.5)  %>% specify_decimal(.,1) ," & ",
           df %>% filter(adv==""  & occ=="Related"   & major=="STEM") %>% `$`(pctdiff)  %>% quantile(0.9)  %>% specify_decimal(.,1) ," & ",
           df %>% filter(adv==""  & occ=="Related"   & major=="STEM") %>% `$`(haus.sig) %>% sum(.)         %>% specify_decimal(.,0) ,"    \\\\"  
          ) %>% cat(file=fname,sep="\n",append=TRUE)
    cat("\\emph{Advanced degrees}       &&&&&& \\\\",file=fname,sep="\n",append=TRUE)
    # education major, advanced degree
    paste0("\\qquad{}",
           df %>% filter(adv!=""  & occ=="Unrelated" & major=="Education") %>% `$`(major)    %>% `[`(1)                                  ," & ",
           df %>% filter(adv!=""  & occ=="Unrelated" & major=="Education") %>% `$`(pctdiff)  %>% quantile(0.1)  %>% specify_decimal(.,1) ," & ",
           df %>% filter(adv!=""  & occ=="Unrelated" & major=="Education") %>% `$`(pctdiff)  %>% quantile(0.5)  %>% specify_decimal(.,1) ," & ",
           df %>% filter(adv!=""  & occ=="Unrelated" & major=="Education") %>% `$`(pctdiff)  %>% quantile(0.9)  %>% specify_decimal(.,1) ," & ",
           df %>% filter(adv!=""  & occ=="Unrelated" & major=="Education") %>% `$`(haus.sig) %>% sum(.)         %>% specify_decimal(.,0) ," & ",
           df %>% filter(adv!=""  & occ=="Related"   & major=="Education") %>% `$`(pctdiff)  %>% quantile(0.1)  %>% specify_decimal(.,1) ," & ",
           df %>% filter(adv!=""  & occ=="Related"   & major=="Education") %>% `$`(pctdiff)  %>% quantile(0.5)  %>% specify_decimal(.,1) ," & ",
           df %>% filter(adv!=""  & occ=="Related"   & major=="Education") %>% `$`(pctdiff)  %>% quantile(0.9)  %>% specify_decimal(.,1) ," & ",
           df %>% filter(adv!=""  & occ=="Related"   & major=="Education") %>% `$`(haus.sig) %>% sum(.)         %>% specify_decimal(.,0) ,"    \\\\"  
          ) %>% cat(file=fname,sep="\n",append=TRUE)
    # social science major, advanced degree
    paste0("\\qquad{}",
           df %>% filter(adv!=""  & occ=="Unrelated" & major=="Social Science") %>% `$`(major)    %>% `[`(1)                                  ," & ",
           df %>% filter(adv!=""  & occ=="Unrelated" & major=="Social Science") %>% `$`(pctdiff)  %>% quantile(0.1)  %>% specify_decimal(.,1) ," & ",
           df %>% filter(adv!=""  & occ=="Unrelated" & major=="Social Science") %>% `$`(pctdiff)  %>% quantile(0.5)  %>% specify_decimal(.,1) ," & ",
           df %>% filter(adv!=""  & occ=="Unrelated" & major=="Social Science") %>% `$`(pctdiff)  %>% quantile(0.9)  %>% specify_decimal(.,1) ," & ",
           df %>% filter(adv!=""  & occ=="Unrelated" & major=="Social Science") %>% `$`(haus.sig) %>% sum(.)         %>% specify_decimal(.,0) ," & ",
           df %>% filter(adv!=""  & occ=="Related"   & major=="Social Science") %>% `$`(pctdiff)  %>% quantile(0.1)  %>% specify_decimal(.,1) ," & ",
           df %>% filter(adv!=""  & occ=="Related"   & major=="Social Science") %>% `$`(pctdiff)  %>% quantile(0.5)  %>% specify_decimal(.,1) ," & ",
           df %>% filter(adv!=""  & occ=="Related"   & major=="Social Science") %>% `$`(pctdiff)  %>% quantile(0.9)  %>% specify_decimal(.,1) ," & ",
           df %>% filter(adv!=""  & occ=="Related"   & major=="Social Science") %>% `$`(haus.sig) %>% sum(.)         %>% specify_decimal(.,0) ,"    \\\\"  
          ) %>% cat(file=fname,sep="\n",append=TRUE)
    # other major, advanced degree
    paste0("\\qquad{}",
           df %>% filter(adv!=""  & occ=="Unrelated" & major=="Other") %>% `$`(major)    %>% `[`(1)                                  ," & ",
           df %>% filter(adv!=""  & occ=="Unrelated" & major=="Other") %>% `$`(pctdiff)  %>% quantile(0.1)  %>% specify_decimal(.,1) ," & ",
           df %>% filter(adv!=""  & occ=="Unrelated" & major=="Other") %>% `$`(pctdiff)  %>% quantile(0.5)  %>% specify_decimal(.,1) ," & ",
           df %>% filter(adv!=""  & occ=="Unrelated" & major=="Other") %>% `$`(pctdiff)  %>% quantile(0.9)  %>% specify_decimal(.,1) ," & ",
           df %>% filter(adv!=""  & occ=="Unrelated" & major=="Other") %>% `$`(haus.sig) %>% sum(.)         %>% specify_decimal(.,0) ," & ",
           df %>% filter(adv!=""  & occ=="Related"   & major=="Other") %>% `$`(pctdiff)  %>% quantile(0.1)  %>% specify_decimal(.,1) ," & ",
           df %>% filter(adv!=""  & occ=="Related"   & major=="Other") %>% `$`(pctdiff)  %>% quantile(0.5)  %>% specify_decimal(.,1) ," & ",
           df %>% filter(adv!=""  & occ=="Related"   & major=="Other") %>% `$`(pctdiff)  %>% quantile(0.9)  %>% specify_decimal(.,1) ," & ",
           df %>% filter(adv!=""  & occ=="Related"   & major=="Other") %>% `$`(haus.sig) %>% sum(.)         %>% specify_decimal(.,0) ,"    \\\\"  
          ) %>% cat(file=fname,sep="\n",append=TRUE)
    # business major, advanced degree
    paste0("\\qquad{}",
           df %>% filter(adv!=""  & occ=="Unrelated" & major=="Business") %>% `$`(major)    %>% `[`(1)                                  ," & ",
           df %>% filter(adv!=""  & occ=="Unrelated" & major=="Business") %>% `$`(pctdiff)  %>% quantile(0.1)  %>% specify_decimal(.,1) ," & ",
           df %>% filter(adv!=""  & occ=="Unrelated" & major=="Business") %>% `$`(pctdiff)  %>% quantile(0.5)  %>% specify_decimal(.,1) ," & ",
           df %>% filter(adv!=""  & occ=="Unrelated" & major=="Business") %>% `$`(pctdiff)  %>% quantile(0.9)  %>% specify_decimal(.,1) ," & ",
           df %>% filter(adv!=""  & occ=="Unrelated" & major=="Business") %>% `$`(haus.sig) %>% sum(.)         %>% specify_decimal(.,0) ," & ",
           df %>% filter(adv!=""  & occ=="Related"   & major=="Business") %>% `$`(pctdiff)  %>% quantile(0.1)  %>% specify_decimal(.,1) ," & ",
           df %>% filter(adv!=""  & occ=="Related"   & major=="Business") %>% `$`(pctdiff)  %>% quantile(0.5)  %>% specify_decimal(.,1) ," & ",
           df %>% filter(adv!=""  & occ=="Related"   & major=="Business") %>% `$`(pctdiff)  %>% quantile(0.9)  %>% specify_decimal(.,1) ," & ",
           df %>% filter(adv!=""  & occ=="Related"   & major=="Business") %>% `$`(haus.sig) %>% sum(.)         %>% specify_decimal(.,0) ,"    \\\\"  
          ) %>% cat(file=fname,sep="\n",append=TRUE)
    # STEM major, advanced degree
    paste0("\\qquad{}",
           df %>% filter(adv!=""  & occ=="Unrelated" & major=="STEM") %>% `$`(major)    %>% `[`(1)                                  ," & ",
           df %>% filter(adv!=""  & occ=="Unrelated" & major=="STEM") %>% `$`(pctdiff)  %>% quantile(0.1)  %>% specify_decimal(.,1) ," & ",
           df %>% filter(adv!=""  & occ=="Unrelated" & major=="STEM") %>% `$`(pctdiff)  %>% quantile(0.5)  %>% specify_decimal(.,1) ," & ",
           df %>% filter(adv!=""  & occ=="Unrelated" & major=="STEM") %>% `$`(pctdiff)  %>% quantile(0.9)  %>% specify_decimal(.,1) ," & ",
           df %>% filter(adv!=""  & occ=="Unrelated" & major=="STEM") %>% `$`(haus.sig) %>% sum(.)         %>% specify_decimal(.,0) ," & ",
           df %>% filter(adv!=""  & occ=="Related"   & major=="STEM") %>% `$`(pctdiff)  %>% quantile(0.1)  %>% specify_decimal(.,1) ," & ",
           df %>% filter(adv!=""  & occ=="Related"   & major=="STEM") %>% `$`(pctdiff)  %>% quantile(0.5)  %>% specify_decimal(.,1) ," & ",
           df %>% filter(adv!=""  & occ=="Related"   & major=="STEM") %>% `$`(pctdiff)  %>% quantile(0.9)  %>% specify_decimal(.,1) ," & ",
           df %>% filter(adv!=""  & occ=="Related"   & major=="STEM") %>% `$`(haus.sig) %>% sum(.)         %>% specify_decimal(.,0) ,"    \\\\"  
          ) %>% cat(file=fname,sep="\n",append=TRUE)
    cat("\\bottomrule"                                                                                                   ,file=fname,sep="\n",append=TRUE) 
    cat("\\end{tabular}"                                                                                                 ,file=fname,sep="\n",append=TRUE) 
    cat(paste0("{\\footnotesize {\\raggedright Notes: Summary statistics of the 15-location distribution of the percentage change between uncorrected and corrected returns to majors. ``No. significant'' counts the number of locations satisfying the following conditions: $(i)$ the Hausman test statistic overturns the null hypothesis of no difference between corrected and uncorrected at the 5\\% level; $(ii)$ both the uncorrected and corrected coefficients are statistically different from zero at the 5\\% level; $(iii)$ the percentage difference between the corrected and uncorrected returns is significantly different from zero at the 5\\% level; and $(iv)$ the percentage difference exceeds 10\\% in magnitude. Percentage changes are least informative for education, social science, and other majors because these majors have bases (i.e. uncorrected returns) that may be very close to zero.}}"),file=fname,sep="\n",append=TRUE)
    cat("\\end{threeparttable}"                                                                                          ,file=fname,sep="\n",append=TRUE)  
    cat("\\end{table}"                                                                                                   ,file=fname,sep="\n",append=TRUE)   
}

# write output to appendix tables
makeapptables <- function(df,shortrel,shortmaj,shortadv) {
    rel <- case_when(
                     shortrel=="Rel"  ~ "Related",
                     shortrel=="Unr"  ~ "Unrelated"
                    )
    Adv <- case_when(
                     shortadv=="Adv"  ~ "Advanced Degree",
                     shortadv==""     ~ ""
                    )
    maj <- case_when(
                     shortmaj=="Edu"  ~ "Education",
                     shortmaj=="Soc"  ~ "Social Science",
                     shortmaj=="Oth"  ~ "Other",
                     shortmaj=="Bus"  ~ "Business",
                     shortmaj=="STEM" ~ "STEM"
                    )
    df %<>% mutate(
                   major    = str_replace_all(major," major",""),
                   major    = str_replace_all(major,"Social sciences","Social Science"),
                   loc      = str_replace_all(loc,"DMV"        ,"WV, VA, DC, MD, DE"),
                   loc      = str_replace_all(loc,"ESCentral"  ,"E S Central Div"),
                   loc      = str_replace_all(loc,"Mountain"   ,"Mountain Div"),
                   loc      = str_replace_all(loc,"NC/SC/GA"   ,"NC, SC, GA"),
                   loc      = str_replace_all(loc,"NewEngland" ,"New England"),
                   loc      = str_replace_all(loc,"NewYork"    ,"New York"),
                   loc      = str_replace_all(loc,"NJ/PA"      ,"New Jersey and Penn."),
                   loc      = str_replace_all(loc,"OH/IN/MI/WI","OH, IN, MI, WI"),
                   loc      = str_replace_all(loc,"OK/AR/LA"   ,"OK, AR, LA"),
                   loc      = str_replace_all(loc,"Pacific"    ,"OR, WA, AK, HI"),
                   loc      = str_replace_all(loc,"WNCentral"  ,"W N Central Div")
                  ) %>%
            filter(major   != "Born here")
    fname <- paste0("../Paper/Tables/",shortmaj,"_",shortrel,"AllStatesA",as.numeric(shortadv=="Adv"),".tex")
    cat("\\begin{landscape}"                                                                                             ,file=fname,sep="\n")
    cat("\\begin{table}[ht]"                                                                                             ,file=fname,sep="\n",append=TRUE)
    if (shortadv=="Adv") {
        cat(paste0("\\caption{Return to Adv. Deg. ",maj," majors in ",rel," occupation, by state (uncorrected and corrected)}")        ,file=fname,sep="\n",append=TRUE)
    } else {
        cat(paste0("\\caption{Return to Bach. Deg. ",maj," majors in ",rel," occupation, by state (uncorrected and corrected)}")        ,file=fname,sep="\n",append=TRUE)
    }
    #\caption{Return to STEM majors in related occupation, by state (uncorrected and corrected)}
    cat(paste0("\\label{tab:",shortmaj,shortrel,"A",as.numeric(shortadv=="Adv"),"All}")                                  ,file=fname,sep="\n",append=TRUE)
    cat("\\centering"                                                                                                    ,file=fname,sep="\n",append=TRUE)
    cat("\\resizebox{1.1\\textwidth}{!}{"                                                                                ,file=fname,sep="\n",append=TRUE)
    cat("\\begin{threeparttable}"                                                                                        ,file=fname,sep="\n",append=TRUE)
    cat("\\begin{tabular}{lccccc}"                                                                                       ,file=fname,sep="\n",append=TRUE)
    cat("\\toprule"                                                                                                      ,file=fname,sep="\n",append=TRUE)
    cat("      & Uncorrected  & Corrected & Percentage   & $\\chi^{2}$ Test & $F$ Test for               \\\\"           ,file=fname,sep="\n",append=TRUE) 
    cat(paste0("Location & ",maj," Return  & ",maj," Return & Change & for Difference   & Correction Terms  \\\\")       ,file=fname,sep="\n",append=TRUE) 
    cat("\\midrule"                                                                                                      ,file=fname,sep="\n",append=TRUE)
    for (jj in names(table(df$loc)) ) {
        tmpy <- df %>% filter(loc==jj & adv==Adv & occ==rel & major==maj)
        paste0(
               tmpy %>% `$`(loc) %>% `[`(1) ," & ",
               tmpy %>% `$`(uncr.coef) %>% `[`(1) %>% specify_decimal(.,3) ," & ",
               tmpy %>% `$`(corr.coef) %>% `[`(1) %>% specify_decimal(.,3) ," & ",
               tmpy %>% `$`(pctdiff)   %>% `[`(1) %>% specify_decimal(.,1) ," & ",
               tmpy %>% `$`(haus.chi2) %>% `[`(1) %>% specify_decimal(.,3) ," & ",
               tmpy %>% `$`(corr.f)    %>% `[`(1) %>% specify_decimal(.,3) ," \\\\ "
              )  %>% cat(file=fname,sep="\n",append=TRUE)
        paste0(
               " & (",
               tmpy %>% `$`(uncr.se) %>% `[`(1) %>% specify_decimal(.,3) ,") & (",
               tmpy %>% `$`(corr.se) %>% `[`(1) %>% specify_decimal(.,3) ,") & (",
               tmpy %>% `$`(pctd.se) %>% `[`(1) %>% specify_decimal(.,1) ,") & [",
               tmpy %>% `$`(haus.p)  %>% `[`(1) %>% specify_decimal(.,3) ,"] & [",
               tmpy %>% `$`(f.pval)  %>% `[`(1) %>% specify_decimal(.,3) ,"] \\\\ "
              )  %>% cat(file=fname,sep="\n",append=TRUE)
    }
    cat("\\bottomrule"                                                                                                   ,file=fname,sep="\n",append=TRUE) 
    cat("\\end{tabular}"                                                                                                 ,file=fname,sep="\n",append=TRUE) 
    cat(paste0("{\\footnotesize {\\raggedright Notes: Bootstrapped standard errors (500 replications) below coefficients in parentheses. $p$-values below test statistics in brackets. See Appendix \\ref{sec:hypothAppendix} for further details.}}"),file=fname,sep="\n",append=TRUE)
    cat("\\end{threeparttable}"                                                                                          ,file=fname,sep="\n",append=TRUE)  
    cat("}"                                                                                                              ,file=fname,sep="\n",append=TRUE)
    cat("\\end{table}"                                                                                                   ,file=fname,sep="\n",append=TRUE)   
    cat("\\end{landscape}"                                                                                               ,file=fname,sep="\n",append=TRUE)   
}

# function to plot corrected and uncorrected returns
coefplots <- function(df1,rr,mm,aa) {
    df1 %<>% filter(major!="Born here")
    df1 %<>% mutate(
                    major    = str_replace_all(major," major",""),
                    major    = str_replace_all(major,"Education","Edu"),
                    major    = str_replace_all(major,"Social sciences","Soc"),
                    major    = str_replace_all(major,"Other","Oth"),
                    major    = str_replace_all(major,"Business","Bus"),
                    occ      = str_replace_all(occ,"ated",""),
                    occ      = str_replace_all(occ,"Unrel","Unr"),
                    adv      = str_replace_all(adv,"Advanced Degree","Adv"),
                    loc      = str_replace_all(loc,"Texas","TX"),
                    loc      = str_replace_all(loc,"NewYork","NY"),
                    loc      = str_replace_all(loc,"California","CA"),
                    loc      = str_replace_all(loc,"Florida","FL"),
                    loc      = str_replace_all(loc,"Illinois","IL"),
                    haus.sig = haus.p<0.05 & uncr.p<.05 & corr.p<.05 & abs(pctdiff/pctd.se)>=1.96 & abs(pctdiff)>10
                   )

    p <- ggplot(
                df1 %>% filter(occ==rr & major==mm & adv==aa), 
                aes(x=uncr.coef, y=corr.coef, color=factor(haus.sig))
               ) + 
    geom_point() +
    scale_color_manual(values=c("gray", "black")) +
    geom_text_repel(aes(label=loc),hjust=0, vjust=0) +
    geom_abline(intercept = 0, slope = 1) +
    labs(y = "Corrected Return", x = "Uncorrected Return") + theme_minimal() + theme(legend.position="none")
    ggsave(paste0("../Paper/Graphics/Scatter",aa,mm,rr,"SeparateM.eps"), device = "eps", width=5, height = 4, units = "in")
}

# 500-replicate results
load("wageresults.rda")
load("bootoutput500.rda")
boot.cov <- booter_cov(boot_out)
boot.se.500 <- booter(boot_out,wagest,boot.cov)
print("summary stats of bootstrap estimates")
boot.se.500 %>% summary %>% print
maketable3(boot.se.500)
maketable4(boot.se.500)

# 1000-replicate results
#load("wageresults.rda")
#load("bootoutput1000.rda")
#boot.se.1000 <- booter(boot_out,wagest)
#maketable3(boot.se.1000)

# call the functions for each major/adv degree combo
for (rrr in c("Rel","Unr")) {
    for (aaa in c("","Adv")) {
        for (mmm in c("Edu","Soc","Oth","Bus","STEM")) {
            coefplots(boot.se.500,rrr,mmm,aaa)
            makeapptables(boot.se.500,rrr,mmm,aaa)
        }
    }
}


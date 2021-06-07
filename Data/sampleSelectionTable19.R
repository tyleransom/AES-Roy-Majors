# write output to Appendix Sample Selection Table
fname <- "../Paper/Tables/appendixTableSampleSelection19.tex"
cat("\\begin{table}[ht]"                                                                                 ,file=fname,sep="\n")
cat("\\caption{Sample selection details}"                                                                ,file=fname,sep="\n",append=TRUE)
cat("\\label{tab:sampleselection}"                                                                       ,file=fname,sep="\n",append=TRUE)
cat("\\centering"                                                                                        ,file=fname,sep="\n",append=TRUE)
cat("\\begin{threeparttable}"                                                                            ,file=fname,sep="\n",append=TRUE)
cat("\\begin{tabular}{lrr}"                                                                              ,file=fname,sep="\n",append=TRUE)
cat("\\toprule"                                                                                          ,file=fname,sep="\n",append=TRUE)
cat("Criterion                                                 & No. obs deleted  & Remaining obs.  \\\\",file=fname,sep="\n",append=TRUE)
cat("\\midrule"                                                                                          ,file=fname,sep="\n",append=TRUE)
paste0("Respondents in 2010-",max(df$year), " ACS                              &      ---         & ",
       bbb %>% format(big.mark=",") ," \\\\ "
      ) %>% cat(file=fname,sep="\n",append=TRUE)
paste0("Drop those without a bachelor's degree or higher          & ",
       (bbb-ccc) %>% format(big.mark=",") ," & ",
       ccc %>% format(big.mark=",") ," \\\\ "
      ) %>% cat(file=fname,sep="\n",append=TRUE)
paste0("Drop those outside of 22-54 age range                     & ",
       (ccc-ddd) %>% format(big.mark=",") ," & ",
       ddd %>% format(big.mark=",") ," \\\\ "
      ) %>% cat(file=fname,sep="\n",append=TRUE)
paste0("Drop those currently enrolled in school or living in group quarters & ",
       (ddd-fff) %>% format(big.mark=",") ," & ",
       fff %>% format(big.mark=",") ," \\\\ "
      ) %>% cat(file=fname,sep="\n",append=TRUE)
paste0("Drop those not born in the US                             & ",
       (fff-ggg) %>% format(big.mark=",") ," & ",
       ggg %>% format(big.mark=",") ," \\\\ "
      ) %>% cat(file=fname,sep="\n",append=TRUE)
paste0("Drop those with missing annual earnings                   & ",
       (ggg-hhh) %>% format(big.mark=",") ," & ",
       hhh %>% format(big.mark=",") ," \\\\ "
      ) %>% cat(file=fname,sep="\n",append=TRUE)
paste0("Drop those with positive annual earnings below \\$20,000   & ",
       (hhh-iii) %>% format(big.mark=",") ," & ",
       iii %>% format(big.mark=",") ," \\\\ "
      ) %>% cat(file=fname,sep="\n",append=TRUE)
paste0("Drop those with annual earnings above \\$600,000           & ",
       (iii-jjj) %>% format(big.mark=",") ," & ",
       jjj %>% format(big.mark=",") ," \\\\ "
      ) %>% cat(file=fname,sep="\n",append=TRUE)
paste0("Drop those with zero annual earnings                      & ",
       (jjj-kkk) %>% format(big.mark=",") ," & ",
       kkk %>% format(big.mark=",") ," \\\\ "
      ) %>% cat(file=fname,sep="\n",append=TRUE)
paste0("Drop those with missing occupation                        & ",
       (kkk-lll) %>% format(big.mark=",") ," & ",
       lll %>% format(big.mark=",") ," \\\\ "
      ) %>% cat(file=fname,sep="\n",append=TRUE)
paste0("Drop females                                              & ",
       (lll-mmm) %>% format(big.mark=",") ," & ",
       mmm %>% format(big.mark=",") ," \\\\ "
      ) %>% cat(file=fname,sep="\n",append=TRUE)
paste0("Drop those with imputed earnings or occupations           & ",
       (mmm-nnn) %>% format(big.mark=",") ," & ",
       nnn %>% format(big.mark=",") ," \\\\ "
      ) %>% cat(file=fname,sep="\n",append=TRUE)
paste0("Drop those with imputed labor force status                & ",
       (nnn-ooo) %>% format(big.mark=",") ," & ",
       ooo %>% format(big.mark=",") ," \\\\ "
      ) %>% cat(file=fname,sep="\n",append=TRUE)
cat("\\midrule"                                                                                          ,file=fname,sep="\n",append=TRUE)
paste0("Final analysis sample                                     &      ---         &     ",
       ooo %>% format(big.mark=",") ," \\\\ "
      ) %>% cat(file=fname,sep="\n",append=TRUE)
cat("\\bottomrule"                                                                                                   ,file=fname,sep="\n",append=TRUE)
cat("\\end{tabular}"                                                                                                 ,file=fname,sep="\n",append=TRUE)
cat("%{\\footnotesize {\\raggedright Notes: }}",file=fname,sep="\n",append=TRUE)
cat("\\end{threeparttable}"                                                                                          ,file=fname,sep="\n",append=TRUE)
cat("\\end{table}"                                                                                                   ,file=fname,sep="\n",append=TRUE)
fname <- NULL 

# aggregate the data
dfagg <- df %>% group_by(advdegree,educvar,birthState,loc1occ) %>% 
                dplyr::summarize(ctng = sum(perwt)) %>% 
                group_by(advdegree,educvar,birthState) %>% 
                mutate(proba = 100*ctng/sum(ctng),
                       relocc = 1-str_detect(loc1occ,"Unrelated"),
                       relocc = as.factor(relocc),
                       relocc = fct_recode(relocc, "Related" = "1", "Unrelated" = "0"),
                       educvar = fct_recode(educvar, "Edu" = "Education", "Soc Sci" = "Social Science", "Oth" = "Other", "Bus" = "Business", "STEM" = "STEM"),
                       loc1 = case_when(as.numeric(loc1occ)>15 ~ as.numeric(loc1occ)-15, as.numeric(loc1occ)<=15 ~ as.numeric(loc1occ)),
                       loc1 = as.factor(loc1),
                       loc1 = fct_recode(loc1, "CA" = "1", "TX" = "2", "FL" = "3", "IL" = "4", "NY" = "5", "New England" = "6", "NJ/PA" = "7", "DMV" = "8", "NC/SC/GA" = "9", "E S Central" = "10", "OH/IN/MI/WI" = "11", "W N Central" = "12", "OK/AR/LA" = "13", "Mountain" = "14", "Pacific not CA" = "15")
                       ) %>%
                ungroup






dfnoadv <- dfagg %>% filter(advdegree==0 & (birthState %in% c("CA","FL","IL","NY","TX")) & (loc1 %in% c("CA","FL","IL","NY","TX"))) %>% 
                 rename(`Related Occupation` = relocc) %>% 
                 mutate(loc1ord    = fct_relevel(loc1,c("CA","FL","IL","NY","TX"))) %>% 
                 mutate(loc1ord    = fct_recode(loc1ord,   "Lives in California" = "CA","Lives in Florida" = "FL","Lives in Illinois" = "IL","Lives in New York" = "NY","Lives in Texas" = "TX")) %>%
                 mutate(birthState = fct_recode(birthState,"Born in California"  = "CA","Born in Florida"  = "FL","Born in Illinois"  = "IL","Born in New York"  = "NY","Born in Texas"  = "TX"))
# Transition matrix for those without an advanced degree
p11 <- ggplot(dfnoadv %>% filter(birthState=="Born in California" & loc1ord=="Lives in California"), 
       aes(x = educvar, y = proba, fill = `Related Occupation`)
      ) +
    geom_col(color = "#1A476F", size=0.05) +
    labs(y = "Born in California\n\nProbability (%)", x = "Major") + theme_minimal() +
    theme(panel.grid.minor = element_blank(), panel.grid.major.y = element_line(size = 0.05), panel.grid.major.x = element_line(size = 0.05), axis.text.y = element_text(color = "black"), axis.text.x = element_text(color = "black"), axis.title.x = element_text(color = "black"), axis.title.y = element_text(color = "black"), legend.position="bottom", panel.border = element_rect(color="black", fill=NA)) + ggtitle("Lives in California") + theme(plot.title = element_text(hjust = 0.5)) +
    scale_fill_manual(values=c("#D7D29E","#1A476F")) + scale_y_continuous(limits = c(0,85), breaks = seq(0,80,by=20))
p12 <- ggplot(dfnoadv %>%  filter(birthState=="Born in California" & loc1ord=="Lives in Florida"), 
       aes(x = educvar, y = proba, fill = `Related Occupation`)
      ) +
    geom_col(color = "#1A476F", size=0.05) +
    labs(y = NULL, x = "Major") + theme_minimal() +
    theme(panel.grid.minor = element_blank(), panel.grid.major.y = element_line(size = 0.05), panel.grid.major.x = element_line(size = 0.05), axis.text.y = element_text(color = "black"), axis.text.x = element_text(color = "black"), axis.title.x = element_text(color = "black"), axis.title.y = element_text(color = "black"), legend.position="bottom", panel.border = element_rect(color="black", fill=NA)) +  ggtitle("Lives in Florida") + theme(plot.title = element_text(hjust = 0.5)) +
    scale_fill_manual(values=c("#D7D29E","#1A476F")) + scale_y_continuous(limits = c(0,12), breaks = seq(0,10,by=2))
p13 <- ggplot(dfnoadv %>%  filter(birthState=="Born in California" & loc1ord=="Lives in Illinois"), 
       aes(x = educvar, y = proba, fill = `Related Occupation`)
      ) +
    geom_col(color = "#1A476F", size=0.05) +
    labs(y = NULL, x = "Major") + theme_minimal() +
    theme(panel.grid.minor = element_blank(), panel.grid.major.y = element_line(size = 0.05), panel.grid.major.x = element_line(size = 0.05), axis.text.y = element_text(color = "black"), axis.text.x = element_text(color = "black"), axis.title.x = element_text(color = "black"), axis.title.y = element_text(color = "black"), legend.position="bottom", panel.border = element_rect(color="black", fill=NA)) +  ggtitle("Lives in Illinois") + theme(plot.title = element_text(hjust = 0.5)) +
    scale_fill_manual(values=c("#D7D29E","#1A476F")) + scale_y_continuous(limits = c(0,12), breaks = seq(0,10,by=2))
p14 <- ggplot(dfnoadv %>%  filter(birthState=="Born in California" & loc1ord=="Lives in New York"), 
       aes(x = educvar, y = proba, fill = `Related Occupation`)
      ) +
    geom_col(color = "#1A476F", size=0.05) +
    labs(y = NULL, x = "Major") + theme_minimal() +
    theme(panel.grid.minor = element_blank(), panel.grid.major.y = element_line(size = 0.05), panel.grid.major.x = element_line(size = 0.05), axis.text.y = element_text(color = "black"), axis.text.x = element_text(color = "black"), axis.title.x = element_text(color = "black"), axis.title.y = element_text(color = "black"), legend.position="bottom", panel.border = element_rect(color="black", fill=NA)) +  ggtitle("Lives in New York") + theme(plot.title = element_text(hjust = 0.5)) +
    scale_fill_manual(values=c("#D7D29E","#1A476F")) + scale_y_continuous(limits = c(0,12), breaks = seq(0,10,by=2))
p15 <- ggplot(dfnoadv %>%   filter(birthState=="Born in California" & loc1ord=="Lives in Texas"), 
       aes(x = educvar, y = proba, fill = `Related Occupation`)
      ) +
    geom_col(color = "#1A476F", size=0.05) +
    labs(y = NULL, x = "Major") + theme_minimal() +
    theme(panel.grid.minor = element_blank(), panel.grid.major.y = element_line(size = 0.05), panel.grid.major.x = element_line(size = 0.05), axis.text.y = element_text(color = "black"), axis.text.x = element_text(color = "black"), axis.title.x = element_text(color = "black"), axis.title.y = element_text(color = "black"), legend.position="bottom", panel.border = element_rect(color="black", fill=NA)) + ggtitle("Lives in Texas") + theme(plot.title = element_text(hjust = 0.5)) +
    scale_fill_manual(values=c("#D7D29E","#1A476F")) + scale_y_continuous(limits = c(0,12), breaks = seq(0,10,by=2))
p21 <- ggplot(dfnoadv %>% filter(birthState=="Born in Florida" & loc1ord=="Lives in California"), 
       aes(x = educvar, y = proba, fill = `Related Occupation`)
      ) +
    geom_col(color = "#1A476F", size=0.05) +
    labs(y = "Born in Florida\n\nProbability (%)", x = "Major") + theme_minimal() +
    theme(panel.grid.minor = element_blank(), panel.grid.major.y = element_line(size = 0.05), panel.grid.major.x = element_line(size = 0.05), axis.text.y = element_text(color = "black"), axis.text.x = element_text(color = "black"), axis.title.x = element_text(color = "black"), axis.title.y = element_text(color = "black"), legend.position="bottom", panel.border = element_rect(color="black", fill=NA)) +
    scale_fill_manual(values=c("#D7D29E","#1A476F")) + scale_y_continuous(limits = c(0,12), breaks = seq(0,10,by=2))
p22 <- ggplot(dfnoadv %>% filter(birthState=="Born in Florida" & loc1ord=="Lives in Florida"), 
       aes(x = educvar, y = proba, fill = `Related Occupation`)
      ) +
    geom_col(color = "#1A476F", size=0.05) +
    labs(y = NULL, x = "Major") + theme_minimal() +
    theme(panel.grid.minor = element_blank(), panel.grid.major.y = element_line(size = 0.05), panel.grid.major.x = element_line(size = 0.05), axis.text.y = element_text(color = "black"), axis.text.x = element_text(color = "black"), axis.title.x = element_text(color = "black"), axis.title.y = element_text(color = "black"), legend.position="bottom", panel.border = element_rect(color="black", fill=NA)) +
    scale_fill_manual(values=c("#D7D29E","#1A476F")) + scale_y_continuous(limits = c(0,85), breaks = seq(0,80,by=20))
p23 <- ggplot(dfnoadv %>% filter(birthState=="Born in Florida" & loc1ord=="Lives in Illinois"), 
       aes(x = educvar, y = proba, fill = `Related Occupation`)
      ) +
    geom_col(color = "#1A476F", size=0.05) +
    labs(y = NULL, x = "Major") + theme_minimal() +
    theme(panel.grid.minor = element_blank(), panel.grid.major.y = element_line(size = 0.05), panel.grid.major.x = element_line(size = 0.05), axis.text.y = element_text(color = "black"), axis.text.x = element_text(color = "black"), axis.title.x = element_text(color = "black"), axis.title.y = element_text(color = "black"), legend.position="bottom", panel.border = element_rect(color="black", fill=NA)) +
    scale_fill_manual(values=c("#D7D29E","#1A476F")) + scale_y_continuous(limits = c(0,12), breaks = seq(0,10,by=2))
p24 <- ggplot(dfnoadv %>% filter(birthState=="Born in Florida" & loc1ord=="Lives in New York"), 
       aes(x = educvar, y = proba, fill = `Related Occupation`)
      ) +
    geom_col(color = "#1A476F", size=0.05) +
    labs(y = NULL, x = "Major") + theme_minimal() +
    theme(panel.grid.minor = element_blank(), panel.grid.major.y = element_line(size = 0.05), panel.grid.major.x = element_line(size = 0.05), axis.text.y = element_text(color = "black"), axis.text.x = element_text(color = "black"), axis.title.x = element_text(color = "black"), axis.title.y = element_text(color = "black"), legend.position="bottom", panel.border = element_rect(color="black", fill=NA)) +
    scale_fill_manual(values=c("#D7D29E","#1A476F")) + scale_y_continuous(limits = c(0,12), breaks = seq(0,10,by=2))
p25 <- ggplot(dfnoadv %>% filter(birthState=="Born in Florida" & loc1ord=="Lives in Texas"), 
       aes(x = educvar, y = proba, fill = `Related Occupation`)
      ) +
    geom_col(color = "#1A476F", size=0.05) +
    labs(y = NULL, x = "Major") + theme_minimal() +
    theme(panel.grid.minor = element_blank(), panel.grid.major.y = element_line(size = 0.05), panel.grid.major.x = element_line(size = 0.05), axis.text.y = element_text(color = "black"), axis.text.x = element_text(color = "black"), axis.title.x = element_text(color = "black"), axis.title.y = element_text(color = "black"), legend.position="bottom", panel.border = element_rect(color="black", fill=NA)) +
    scale_fill_manual(values=c("#D7D29E","#1A476F")) + scale_y_continuous(limits = c(0,12), breaks = seq(0,10,by=2))
p31 <- ggplot(dfnoadv %>% filter(birthState=="Born in Illinois" & loc1ord=="Lives in California"), 
       aes(x = educvar, y = proba, fill = `Related Occupation`)
      ) +
    geom_col(color = "#1A476F", size=0.05) +
    labs(y = "Born in Illinois\n\nProbability (%)", x = "Major") + theme_minimal() +
    theme(panel.grid.minor = element_blank(), panel.grid.major.y = element_line(size = 0.05), panel.grid.major.x = element_line(size = 0.05), axis.text.y = element_text(color = "black"), axis.text.x = element_text(color = "black"), axis.title.x = element_text(color = "black"), axis.title.y = element_text(color = "black"), legend.position="bottom", panel.border = element_rect(color="black", fill=NA)) +
    scale_fill_manual(values=c("#D7D29E","#1A476F")) + scale_y_continuous(limits = c(0,12), breaks = seq(0,10,by=2))
p32 <- ggplot(dfnoadv %>% filter(birthState=="Born in Illinois" & loc1ord=="Lives in Florida"), 
       aes(x = educvar, y = proba, fill = `Related Occupation`)
      ) +
    geom_col(color = "#1A476F", size=0.05) +
    labs(y = NULL, x = "Major") + theme_minimal() +
    theme(panel.grid.minor = element_blank(), panel.grid.major.y = element_line(size = 0.05), panel.grid.major.x = element_line(size = 0.05), axis.text.y = element_text(color = "black"), axis.text.x = element_text(color = "black"), axis.title.x = element_text(color = "black"), axis.title.y = element_text(color = "black"), legend.position="bottom", panel.border = element_rect(color="black", fill=NA)) +
    scale_fill_manual(values=c("#D7D29E","#1A476F")) + scale_y_continuous(limits = c(0,12), breaks = seq(0,10,by=2))
p33 <- ggplot(dfnoadv %>% filter(birthState=="Born in Illinois" & loc1ord=="Lives in Illinois"), 
       aes(x = educvar, y = proba, fill = `Related Occupation`)
      ) +
    geom_col(color = "#1A476F", size=0.05) +
    labs(y = NULL, x = "Major") + theme_minimal() +
    theme(panel.grid.minor = element_blank(), panel.grid.major.y = element_line(size = 0.05), panel.grid.major.x = element_line(size = 0.05), axis.text.y = element_text(color = "black"), axis.text.x = element_text(color = "black"), axis.title.x = element_text(color = "black"), axis.title.y = element_text(color = "black"), legend.position="bottom", panel.border = element_rect(color="black", fill=NA)) +
    scale_fill_manual(values=c("#D7D29E","#1A476F")) + scale_y_continuous(limits = c(0,85), breaks = seq(0,80,by=20))
p34 <- ggplot(dfnoadv %>% filter(birthState=="Born in Illinois" & loc1ord=="Lives in New York"), 
       aes(x = educvar, y = proba, fill = `Related Occupation`)
      ) +
    geom_col(color = "#1A476F", size=0.05) +
    labs(y = NULL, x = "Major") + theme_minimal() +
    theme(panel.grid.minor = element_blank(), panel.grid.major.y = element_line(size = 0.05), panel.grid.major.x = element_line(size = 0.05), axis.text.y = element_text(color = "black"), axis.text.x = element_text(color = "black"), axis.title.x = element_text(color = "black"), axis.title.y = element_text(color = "black"), legend.position="bottom", panel.border = element_rect(color="black", fill=NA)) +
    scale_fill_manual(values=c("#D7D29E","#1A476F")) + scale_y_continuous(limits = c(0,12), breaks = seq(0,10,by=2))
p35 <- ggplot(dfnoadv %>% filter(birthState=="Born in Illinois" & loc1ord=="Lives in Texas"), 
       aes(x = educvar, y = proba, fill = `Related Occupation`)
      ) +
    geom_col(color = "#1A476F", size=0.05) +
    labs(y = NULL, x = "Major") + theme_minimal() +
    theme(panel.grid.minor = element_blank(), panel.grid.major.y = element_line(size = 0.05), panel.grid.major.x = element_line(size = 0.05), axis.text.y = element_text(color = "black"), axis.text.x = element_text(color = "black"), axis.title.x = element_text(color = "black"), axis.title.y = element_text(color = "black"), legend.position="bottom", panel.border = element_rect(color="black", fill=NA)) +
    scale_fill_manual(values=c("#D7D29E","#1A476F")) + scale_y_continuous(limits = c(0,12), breaks = seq(0,10,by=2))
p41 <- ggplot(dfnoadv %>% filter(birthState=="Born in New York" & loc1ord=="Lives in California"), 
       aes(x = educvar, y = proba, fill = `Related Occupation`)
      ) +
    geom_col(color = "#1A476F", size=0.05) +
    labs(y = "Born in New York\n\nProbability (%)", x = "Major") + theme_minimal() +
    theme(panel.grid.minor = element_blank(), panel.grid.major.y = element_line(size = 0.05), panel.grid.major.x = element_line(size = 0.05), axis.text.y = element_text(color = "black"), axis.text.x = element_text(color = "black"), axis.title.x = element_text(color = "black"), axis.title.y = element_text(color = "black"), legend.position="bottom", panel.border = element_rect(color="black", fill=NA)) +
    scale_fill_manual(values=c("#D7D29E","#1A476F")) + scale_y_continuous(limits = c(0,12), breaks = seq(0,10,by=2))
p42 <- ggplot(dfnoadv %>% filter(birthState=="Born in New York" & loc1ord=="Lives in Florida"), 
       aes(x = educvar, y = proba, fill = `Related Occupation`)
      ) +
    geom_col(color = "#1A476F", size=0.05) +
    labs(y = NULL, x = "Major") + theme_minimal() +
    theme(panel.grid.minor = element_blank(), panel.grid.major.y = element_line(size = 0.05), panel.grid.major.x = element_line(size = 0.05), axis.text.y = element_text(color = "black"), axis.text.x = element_text(color = "black"), axis.title.x = element_text(color = "black"), axis.title.y = element_text(color = "black"), legend.position="bottom", panel.border = element_rect(color="black", fill=NA)) +
    scale_fill_manual(values=c("#D7D29E","#1A476F")) + scale_y_continuous(limits = c(0,12), breaks = seq(0,10,by=2))
p43 <- ggplot(dfnoadv %>% filter(birthState=="Born in New York" & loc1ord=="Lives in Illinois"), 
       aes(x = educvar, y = proba, fill = `Related Occupation`)
      ) +
    geom_col(color = "#1A476F", size=0.05) +
    labs(y = NULL, x = "Major") + theme_minimal() +
    theme(panel.grid.minor = element_blank(), panel.grid.major.y = element_line(size = 0.05), panel.grid.major.x = element_line(size = 0.05), axis.text.y = element_text(color = "black"), axis.text.x = element_text(color = "black"), axis.title.x = element_text(color = "black"), axis.title.y = element_text(color = "black"), legend.position="bottom", panel.border = element_rect(color="black", fill=NA)) +
    scale_fill_manual(values=c("#D7D29E","#1A476F")) + scale_y_continuous(limits = c(0,12), breaks = seq(0,10,by=2))
p44 <- ggplot(dfnoadv %>% filter(birthState=="Born in New York" & loc1ord=="Lives in New York"), 
       aes(x = educvar, y = proba, fill = `Related Occupation`)
      ) +
    geom_col(color = "#1A476F", size=0.05) +
    labs(y = NULL, x = "Major") + theme_minimal() +
    theme(panel.grid.minor = element_blank(), panel.grid.major.y = element_line(size = 0.05), panel.grid.major.x = element_line(size = 0.05), axis.text.y = element_text(color = "black"), axis.text.x = element_text(color = "black"), axis.title.x = element_text(color = "black"), axis.title.y = element_text(color = "black"), legend.position="bottom", panel.border = element_rect(color="black", fill=NA)) +
    scale_fill_manual(values=c("#D7D29E","#1A476F")) + scale_y_continuous(limits = c(0,85), breaks = seq(0,80,by=20))
p45 <- ggplot(dfnoadv %>% filter(birthState=="Born in New York" & loc1ord=="Lives in Texas"), 
       aes(x = educvar, y = proba, fill = `Related Occupation`)
      ) +
    geom_col(color = "#1A476F", size=0.05) +
    labs(y = NULL, x = "Major") + theme_minimal() +
    theme(panel.grid.minor = element_blank(), panel.grid.major.y = element_line(size = 0.05), panel.grid.major.x = element_line(size = 0.05), axis.text.y = element_text(color = "black"), axis.text.x = element_text(color = "black"), axis.title.x = element_text(color = "black"), axis.title.y = element_text(color = "black"), legend.position="bottom", panel.border = element_rect(color="black", fill=NA)) +
    scale_fill_manual(values=c("#D7D29E","#1A476F")) + scale_y_continuous(limits = c(0,12), breaks = seq(0,10,by=2))
p51 <- ggplot(dfnoadv %>% filter(birthState=="Born in Texas" & loc1ord=="Lives in California"), 
       aes(x = educvar, y = proba, fill = `Related Occupation`)
      ) +
    geom_col(color = "#1A476F", size=0.05) +
    labs(y = "Born in Texas\n\nProbability (%)", x = "Major") + theme_minimal() +
    theme(panel.grid.minor = element_blank(), panel.grid.major.y = element_line(size = 0.05), panel.grid.major.x = element_line(size = 0.05), axis.text.y = element_text(color = "black"), axis.text.x = element_text(color = "black"), axis.title.x = element_text(color = "black"), axis.title.y = element_text(color = "black"), legend.position="bottom", panel.border = element_rect(color="black", fill=NA)) +
    scale_fill_manual(values=c("#D7D29E","#1A476F")) + scale_y_continuous(limits = c(0,12), breaks = seq(0,10,by=2))
p52 <- ggplot(dfnoadv %>% filter(birthState=="Born in Texas" & loc1ord=="Lives in Florida"), 
       aes(x = educvar, y = proba, fill = `Related Occupation`)
      ) +
    geom_col(color = "#1A476F", size=0.05) +
    labs(y = NULL, x = "Major") + theme_minimal() +
    theme(panel.grid.minor = element_blank(), panel.grid.major.y = element_line(size = 0.05), panel.grid.major.x = element_line(size = 0.05), axis.text.y = element_text(color = "black"), axis.text.x = element_text(color = "black"), axis.title.x = element_text(color = "black"), axis.title.y = element_text(color = "black"), legend.position="bottom", panel.border = element_rect(color="black", fill=NA)) +
    scale_fill_manual(values=c("#D7D29E","#1A476F")) + scale_y_continuous(limits = c(0,12), breaks = seq(0,10,by=2))
p53 <- ggplot(dfnoadv %>% filter(birthState=="Born in Texas" & loc1ord=="Lives in Illinois"), 
       aes(x = educvar, y = proba, fill = `Related Occupation`)
      ) +
    geom_col(color = "#1A476F", size=0.05) +
    labs(y = NULL, x = "Major") + theme_minimal() +
    theme(panel.grid.minor = element_blank(), panel.grid.major.y = element_line(size = 0.05), panel.grid.major.x = element_line(size = 0.05), axis.text.y = element_text(color = "black"), axis.text.x = element_text(color = "black"), axis.title.x = element_text(color = "black"), axis.title.y = element_text(color = "black"), legend.position="bottom", panel.border = element_rect(color="black", fill=NA)) +
    scale_fill_manual(values=c("#D7D29E","#1A476F")) + scale_y_continuous(limits = c(0,12), breaks = seq(0,10,by=2))
p54 <- ggplot(dfnoadv %>% filter(birthState=="Born in Texas" & loc1ord=="Lives in New York"), 
       aes(x = educvar, y = proba, fill = `Related Occupation`)
      ) +
    geom_col(color = "#1A476F", size=0.05) +
    labs(y = NULL, x = "Major") + theme_minimal() +
    theme(panel.grid.minor = element_blank(), panel.grid.major.y = element_line(size = 0.05), panel.grid.major.x = element_line(size = 0.05), axis.text.y = element_text(color = "black"), axis.text.x = element_text(color = "black"), axis.title.x = element_text(color = "black"), axis.title.y = element_text(color = "black"), legend.position="bottom", panel.border = element_rect(color="black", fill=NA)) +
    scale_fill_manual(values=c("#D7D29E","#1A476F")) + scale_y_continuous(limits = c(0,12), breaks = seq(0,10,by=2))
p55 <- ggplot(dfnoadv %>% filter(birthState=="Born in Texas" & loc1ord=="Lives in Texas"), 
       aes(x = educvar, y = proba, fill = `Related Occupation`)
      ) +
    geom_col(color = "#1A476F", size=0.05) +
    labs(y = NULL, x = "Major") + theme_minimal() +
    theme(panel.grid.minor = element_blank(), panel.grid.major.y = element_line(size = 0.05), panel.grid.major.x = element_line(size = 0.05), axis.text.y = element_text(color = "black"), axis.text.x = element_text(color = "black"), axis.title.x = element_text(color = "black"), axis.title.y = element_text(color = "black"), legend.position="bottom", panel.border = element_rect(color="black", fill=NA)) +
    scale_fill_manual(values=c("#D7D29E","#1A476F")) + scale_y_continuous(limits = c(0,85), breaks = seq(0,80,by=20))
(p11 | p12 | p13 | p14 | p15) / (p21 | p22 | p23 | p24 | p25) / (p31 | p32 | p33 | p34 | p35) / (p41 | p42 | p43 | p44 | p45) / (p51 | p52 | p53 | p54 | p55) + plot_layout(guides = "collect") & theme(legend.position='bottom') 
ggsave("../Paper/Graphics/barTranMatBA.eps", device = "eps", width=11.6, height = 9, units = "in")









dfadv <- dfagg %>% filter(advdegree==1 & (birthState %in% c("CA","FL","IL","NY","TX")) & (loc1 %in% c("CA","FL","IL","NY","TX"))) %>% 
                 rename(`Related Occupation` = relocc) %>% 
                 mutate(loc1ord    = fct_relevel(loc1,c("CA","FL","IL","NY","TX"))) %>% 
                 mutate(loc1ord    = fct_recode(loc1ord,   "Lives in California" = "CA","Lives in Florida" = "FL","Lives in Illinois" = "IL","Lives in New York" = "NY","Lives in Texas" = "TX")) %>%
                 mutate(birthState = fct_recode(birthState,"Born in California"  = "CA","Born in Florida"  = "FL","Born in Illinois"  = "IL","Born in New York"  = "NY","Born in Texas"  = "TX"))
# Transition matrix for those without an advanced degree
p11 <- ggplot(dfadv %>% filter(birthState=="Born in California" & loc1ord=="Lives in California"), 
       aes(x = educvar, y = proba, fill = `Related Occupation`)
      ) +
    geom_col(color = "#1A476F", size=0.05) +
    labs(y = "Born in California\n\nProbability (%)", x = "Major") + theme_minimal() +
    theme(panel.grid.minor = element_blank(), panel.grid.major.y = element_line(size = 0.05), panel.grid.major.x = element_line(size = 0.05), axis.text.y = element_text(color = "black"), axis.text.x = element_text(color = "black"), axis.title.x = element_text(color = "black"), axis.title.y = element_text(color = "black"), legend.position="bottom", panel.border = element_rect(color="black", fill=NA)) + ggtitle("Lives in California") + theme(plot.title = element_text(hjust = 0.5)) +
    scale_fill_manual(values=c("#D7D29E","#1A476F")) + scale_y_continuous(limits = c(0,85), breaks = seq(0,80,by=20))
p12 <- ggplot(dfadv %>%  filter(birthState=="Born in California" & loc1ord=="Lives in Florida"), 
       aes(x = educvar, y = proba, fill = `Related Occupation`)
      ) +
    geom_col(color = "#1A476F", size=0.05) +
    labs(y = NULL, x = "Major") + theme_minimal() +
    theme(panel.grid.minor = element_blank(), panel.grid.major.y = element_line(size = 0.05), panel.grid.major.x = element_line(size = 0.05), axis.text.y = element_text(color = "black"), axis.text.x = element_text(color = "black"), axis.title.x = element_text(color = "black"), axis.title.y = element_text(color = "black"), legend.position="bottom", panel.border = element_rect(color="black", fill=NA)) +  ggtitle("Lives in Florida") + theme(plot.title = element_text(hjust = 0.5)) +
    scale_fill_manual(values=c("#D7D29E","#1A476F")) + scale_y_continuous(limits = c(0,12), breaks = seq(0,10,by=2))
p13 <- ggplot(dfadv %>%  filter(birthState=="Born in California" & loc1ord=="Lives in Illinois"), 
       aes(x = educvar, y = proba, fill = `Related Occupation`)
      ) +
    geom_col(color = "#1A476F", size=0.05) +
    labs(y = NULL, x = "Major") + theme_minimal() +
    theme(panel.grid.minor = element_blank(), panel.grid.major.y = element_line(size = 0.05), panel.grid.major.x = element_line(size = 0.05), axis.text.y = element_text(color = "black"), axis.text.x = element_text(color = "black"), axis.title.x = element_text(color = "black"), axis.title.y = element_text(color = "black"), legend.position="bottom", panel.border = element_rect(color="black", fill=NA)) +  ggtitle("Lives in Illinois") + theme(plot.title = element_text(hjust = 0.5)) +
    scale_fill_manual(values=c("#D7D29E","#1A476F")) + scale_y_continuous(limits = c(0,12), breaks = seq(0,10,by=2))
p14 <- ggplot(dfadv %>%  filter(birthState=="Born in California" & loc1ord=="Lives in New York"), 
       aes(x = educvar, y = proba, fill = `Related Occupation`)
      ) +
    geom_col(color = "#1A476F", size=0.05) +
    labs(y = NULL, x = "Major") + theme_minimal() +
    theme(panel.grid.minor = element_blank(), panel.grid.major.y = element_line(size = 0.05), panel.grid.major.x = element_line(size = 0.05), axis.text.y = element_text(color = "black"), axis.text.x = element_text(color = "black"), axis.title.x = element_text(color = "black"), axis.title.y = element_text(color = "black"), legend.position="bottom", panel.border = element_rect(color="black", fill=NA)) +  ggtitle("Lives in New York") + theme(plot.title = element_text(hjust = 0.5)) +
    scale_fill_manual(values=c("#D7D29E","#1A476F")) + scale_y_continuous(limits = c(0,12), breaks = seq(0,10,by=2))
p15 <- ggplot(dfadv %>%   filter(birthState=="Born in California" & loc1ord=="Lives in Texas"), 
       aes(x = educvar, y = proba, fill = `Related Occupation`)
      ) +
    geom_col(color = "#1A476F", size=0.05) +
    labs(y = NULL, x = "Major") + theme_minimal() +
    theme(panel.grid.minor = element_blank(), panel.grid.major.y = element_line(size = 0.05), panel.grid.major.x = element_line(size = 0.05), axis.text.y = element_text(color = "black"), axis.text.x = element_text(color = "black"), axis.title.x = element_text(color = "black"), axis.title.y = element_text(color = "black"), legend.position="bottom", panel.border = element_rect(color="black", fill=NA)) + ggtitle("Lives in Texas") + theme(plot.title = element_text(hjust = 0.5)) +
    scale_fill_manual(values=c("#D7D29E","#1A476F")) + scale_y_continuous(limits = c(0,12), breaks = seq(0,10,by=2))
p21 <- ggplot(dfadv %>% filter(birthState=="Born in Florida" & loc1ord=="Lives in California"), 
       aes(x = educvar, y = proba, fill = `Related Occupation`)
      ) +
    geom_col(color = "#1A476F", size=0.05) +
    labs(y = "Born in Florida\n\nProbability (%)", x = "Major") + theme_minimal() +
    theme(panel.grid.minor = element_blank(), panel.grid.major.y = element_line(size = 0.05), panel.grid.major.x = element_line(size = 0.05), axis.text.y = element_text(color = "black"), axis.text.x = element_text(color = "black"), axis.title.x = element_text(color = "black"), axis.title.y = element_text(color = "black"), legend.position="bottom", panel.border = element_rect(color="black", fill=NA)) +
    scale_fill_manual(values=c("#D7D29E","#1A476F")) + scale_y_continuous(limits = c(0,12), breaks = seq(0,10,by=2))
p22 <- ggplot(dfadv %>% filter(birthState=="Born in Florida" & loc1ord=="Lives in Florida"), 
       aes(x = educvar, y = proba, fill = `Related Occupation`)
      ) +
    geom_col(color = "#1A476F", size=0.05) +
    labs(y = NULL, x = "Major") + theme_minimal() +
    theme(panel.grid.minor = element_blank(), panel.grid.major.y = element_line(size = 0.05), panel.grid.major.x = element_line(size = 0.05), axis.text.y = element_text(color = "black"), axis.text.x = element_text(color = "black"), axis.title.x = element_text(color = "black"), axis.title.y = element_text(color = "black"), legend.position="bottom", panel.border = element_rect(color="black", fill=NA)) +
    scale_fill_manual(values=c("#D7D29E","#1A476F")) + scale_y_continuous(limits = c(0,85), breaks = seq(0,80,by=20))
p23 <- ggplot(dfadv %>% filter(birthState=="Born in Florida" & loc1ord=="Lives in Illinois"), 
       aes(x = educvar, y = proba, fill = `Related Occupation`)
      ) +
    geom_col(color = "#1A476F", size=0.05) +
    labs(y = NULL, x = "Major") + theme_minimal() +
    theme(panel.grid.minor = element_blank(), panel.grid.major.y = element_line(size = 0.05), panel.grid.major.x = element_line(size = 0.05), axis.text.y = element_text(color = "black"), axis.text.x = element_text(color = "black"), axis.title.x = element_text(color = "black"), axis.title.y = element_text(color = "black"), legend.position="bottom", panel.border = element_rect(color="black", fill=NA)) +
    scale_fill_manual(values=c("#D7D29E","#1A476F")) + scale_y_continuous(limits = c(0,12), breaks = seq(0,10,by=2))
p24 <- ggplot(dfadv %>% filter(birthState=="Born in Florida" & loc1ord=="Lives in New York"), 
       aes(x = educvar, y = proba, fill = `Related Occupation`)
      ) +
    geom_col(color = "#1A476F", size=0.05) +
    labs(y = NULL, x = "Major") + theme_minimal() +
    theme(panel.grid.minor = element_blank(), panel.grid.major.y = element_line(size = 0.05), panel.grid.major.x = element_line(size = 0.05), axis.text.y = element_text(color = "black"), axis.text.x = element_text(color = "black"), axis.title.x = element_text(color = "black"), axis.title.y = element_text(color = "black"), legend.position="bottom", panel.border = element_rect(color="black", fill=NA)) +
    scale_fill_manual(values=c("#D7D29E","#1A476F")) + scale_y_continuous(limits = c(0,12), breaks = seq(0,10,by=2))
p25 <- ggplot(dfadv %>% filter(birthState=="Born in Florida" & loc1ord=="Lives in Texas"), 
       aes(x = educvar, y = proba, fill = `Related Occupation`)
      ) +
    geom_col(color = "#1A476F", size=0.05) +
    labs(y = NULL, x = "Major") + theme_minimal() +
    theme(panel.grid.minor = element_blank(), panel.grid.major.y = element_line(size = 0.05), panel.grid.major.x = element_line(size = 0.05), axis.text.y = element_text(color = "black"), axis.text.x = element_text(color = "black"), axis.title.x = element_text(color = "black"), axis.title.y = element_text(color = "black"), legend.position="bottom", panel.border = element_rect(color="black", fill=NA)) +
    scale_fill_manual(values=c("#D7D29E","#1A476F")) + scale_y_continuous(limits = c(0,12), breaks = seq(0,10,by=2))
p31 <- ggplot(dfadv %>% filter(birthState=="Born in Illinois" & loc1ord=="Lives in California"), 
       aes(x = educvar, y = proba, fill = `Related Occupation`)
      ) +
    geom_col(color = "#1A476F", size=0.05) +
    labs(y = "Born in Illinois\n\nProbability (%)", x = "Major") + theme_minimal() +
    theme(panel.grid.minor = element_blank(), panel.grid.major.y = element_line(size = 0.05), panel.grid.major.x = element_line(size = 0.05), axis.text.y = element_text(color = "black"), axis.text.x = element_text(color = "black"), axis.title.x = element_text(color = "black"), axis.title.y = element_text(color = "black"), legend.position="bottom", panel.border = element_rect(color="black", fill=NA)) +
    scale_fill_manual(values=c("#D7D29E","#1A476F")) + scale_y_continuous(limits = c(0,12), breaks = seq(0,10,by=2))
p32 <- ggplot(dfadv %>% filter(birthState=="Born in Illinois" & loc1ord=="Lives in Florida"), 
       aes(x = educvar, y = proba, fill = `Related Occupation`)
      ) +
    geom_col(color = "#1A476F", size=0.05) +
    labs(y = NULL, x = "Major") + theme_minimal() +
    theme(panel.grid.minor = element_blank(), panel.grid.major.y = element_line(size = 0.05), panel.grid.major.x = element_line(size = 0.05), axis.text.y = element_text(color = "black"), axis.text.x = element_text(color = "black"), axis.title.x = element_text(color = "black"), axis.title.y = element_text(color = "black"), legend.position="bottom", panel.border = element_rect(color="black", fill=NA)) +
    scale_fill_manual(values=c("#D7D29E","#1A476F")) + scale_y_continuous(limits = c(0,12), breaks = seq(0,10,by=2))
p33 <- ggplot(dfadv %>% filter(birthState=="Born in Illinois" & loc1ord=="Lives in Illinois"), 
       aes(x = educvar, y = proba, fill = `Related Occupation`)
      ) +
    geom_col(color = "#1A476F", size=0.05) +
    labs(y = NULL, x = "Major") + theme_minimal() +
    theme(panel.grid.minor = element_blank(), panel.grid.major.y = element_line(size = 0.05), panel.grid.major.x = element_line(size = 0.05), axis.text.y = element_text(color = "black"), axis.text.x = element_text(color = "black"), axis.title.x = element_text(color = "black"), axis.title.y = element_text(color = "black"), legend.position="bottom", panel.border = element_rect(color="black", fill=NA)) +
    scale_fill_manual(values=c("#D7D29E","#1A476F")) + scale_y_continuous(limits = c(0,85), breaks = seq(0,80,by=20))
p34 <- ggplot(dfadv %>% filter(birthState=="Born in Illinois" & loc1ord=="Lives in New York"), 
       aes(x = educvar, y = proba, fill = `Related Occupation`)
      ) +
    geom_col(color = "#1A476F", size=0.05) +
    labs(y = NULL, x = "Major") + theme_minimal() +
    theme(panel.grid.minor = element_blank(), panel.grid.major.y = element_line(size = 0.05), panel.grid.major.x = element_line(size = 0.05), axis.text.y = element_text(color = "black"), axis.text.x = element_text(color = "black"), axis.title.x = element_text(color = "black"), axis.title.y = element_text(color = "black"), legend.position="bottom", panel.border = element_rect(color="black", fill=NA)) +
    scale_fill_manual(values=c("#D7D29E","#1A476F")) + scale_y_continuous(limits = c(0,12), breaks = seq(0,10,by=2))
p35 <- ggplot(dfadv %>% filter(birthState=="Born in Illinois" & loc1ord=="Lives in Texas"), 
       aes(x = educvar, y = proba, fill = `Related Occupation`)
      ) +
    geom_col(color = "#1A476F", size=0.05) +
    labs(y = NULL, x = "Major") + theme_minimal() +
    theme(panel.grid.minor = element_blank(), panel.grid.major.y = element_line(size = 0.05), panel.grid.major.x = element_line(size = 0.05), axis.text.y = element_text(color = "black"), axis.text.x = element_text(color = "black"), axis.title.x = element_text(color = "black"), axis.title.y = element_text(color = "black"), legend.position="bottom", panel.border = element_rect(color="black", fill=NA)) +
    scale_fill_manual(values=c("#D7D29E","#1A476F")) + scale_y_continuous(limits = c(0,12), breaks = seq(0,10,by=2))
p41 <- ggplot(dfadv %>% filter(birthState=="Born in New York" & loc1ord=="Lives in California"), 
       aes(x = educvar, y = proba, fill = `Related Occupation`)
      ) +
    geom_col(color = "#1A476F", size=0.05) +
    labs(y = "Born in New York\n\nProbability (%)", x = "Major") + theme_minimal() +
    theme(panel.grid.minor = element_blank(), panel.grid.major.y = element_line(size = 0.05), panel.grid.major.x = element_line(size = 0.05), axis.text.y = element_text(color = "black"), axis.text.x = element_text(color = "black"), axis.title.x = element_text(color = "black"), axis.title.y = element_text(color = "black"), legend.position="bottom", panel.border = element_rect(color="black", fill=NA)) +
    scale_fill_manual(values=c("#D7D29E","#1A476F")) + scale_y_continuous(limits = c(0,12), breaks = seq(0,10,by=2))
p42 <- ggplot(dfadv %>% filter(birthState=="Born in New York" & loc1ord=="Lives in Florida"), 
       aes(x = educvar, y = proba, fill = `Related Occupation`)
      ) +
    geom_col(color = "#1A476F", size=0.05) +
    labs(y = NULL, x = "Major") + theme_minimal() +
    theme(panel.grid.minor = element_blank(), panel.grid.major.y = element_line(size = 0.05), panel.grid.major.x = element_line(size = 0.05), axis.text.y = element_text(color = "black"), axis.text.x = element_text(color = "black"), axis.title.x = element_text(color = "black"), axis.title.y = element_text(color = "black"), legend.position="bottom", panel.border = element_rect(color="black", fill=NA)) +
    scale_fill_manual(values=c("#D7D29E","#1A476F")) + scale_y_continuous(limits = c(0,12), breaks = seq(0,10,by=2))
p43 <- ggplot(dfadv %>% filter(birthState=="Born in New York" & loc1ord=="Lives in Illinois"), 
       aes(x = educvar, y = proba, fill = `Related Occupation`)
      ) +
    geom_col(color = "#1A476F", size=0.05) +
    labs(y = NULL, x = "Major") + theme_minimal() +
    theme(panel.grid.minor = element_blank(), panel.grid.major.y = element_line(size = 0.05), panel.grid.major.x = element_line(size = 0.05), axis.text.y = element_text(color = "black"), axis.text.x = element_text(color = "black"), axis.title.x = element_text(color = "black"), axis.title.y = element_text(color = "black"), legend.position="bottom", panel.border = element_rect(color="black", fill=NA)) +
    scale_fill_manual(values=c("#D7D29E","#1A476F")) + scale_y_continuous(limits = c(0,12), breaks = seq(0,10,by=2))
p44 <- ggplot(dfadv %>% filter(birthState=="Born in New York" & loc1ord=="Lives in New York"), 
       aes(x = educvar, y = proba, fill = `Related Occupation`)
      ) +
    geom_col(color = "#1A476F", size=0.05) +
    labs(y = NULL, x = "Major") + theme_minimal() +
    theme(panel.grid.minor = element_blank(), panel.grid.major.y = element_line(size = 0.05), panel.grid.major.x = element_line(size = 0.05), axis.text.y = element_text(color = "black"), axis.text.x = element_text(color = "black"), axis.title.x = element_text(color = "black"), axis.title.y = element_text(color = "black"), legend.position="bottom", panel.border = element_rect(color="black", fill=NA)) +
    scale_fill_manual(values=c("#D7D29E","#1A476F")) + scale_y_continuous(limits = c(0,85), breaks = seq(0,80,by=20))
p45 <- ggplot(dfadv %>% filter(birthState=="Born in New York" & loc1ord=="Lives in Texas"), 
       aes(x = educvar, y = proba, fill = `Related Occupation`)
      ) +
    geom_col(color = "#1A476F", size=0.05) +
    labs(y = NULL, x = "Major") + theme_minimal() +
    theme(panel.grid.minor = element_blank(), panel.grid.major.y = element_line(size = 0.05), panel.grid.major.x = element_line(size = 0.05), axis.text.y = element_text(color = "black"), axis.text.x = element_text(color = "black"), axis.title.x = element_text(color = "black"), axis.title.y = element_text(color = "black"), legend.position="bottom", panel.border = element_rect(color="black", fill=NA)) +
    scale_fill_manual(values=c("#D7D29E","#1A476F")) + scale_y_continuous(limits = c(0,12), breaks = seq(0,10,by=2))
p51 <- ggplot(dfadv %>% filter(birthState=="Born in Texas" & loc1ord=="Lives in California"), 
       aes(x = educvar, y = proba, fill = `Related Occupation`)
      ) +
    geom_col(color = "#1A476F", size=0.05) +
    labs(y = "Born in Texas\n\nProbability (%)", x = "Major") + theme_minimal() +
    theme(panel.grid.minor = element_blank(), panel.grid.major.y = element_line(size = 0.05), panel.grid.major.x = element_line(size = 0.05), axis.text.y = element_text(color = "black"), axis.text.x = element_text(color = "black"), axis.title.x = element_text(color = "black"), axis.title.y = element_text(color = "black"), legend.position="bottom", panel.border = element_rect(color="black", fill=NA)) +
    scale_fill_manual(values=c("#D7D29E","#1A476F")) + scale_y_continuous(limits = c(0,12), breaks = seq(0,10,by=2))
p52 <- ggplot(dfadv %>% filter(birthState=="Born in Texas" & loc1ord=="Lives in Florida"), 
       aes(x = educvar, y = proba, fill = `Related Occupation`)
      ) +
    geom_col(color = "#1A476F", size=0.05) +
    labs(y = NULL, x = "Major") + theme_minimal() +
    theme(panel.grid.minor = element_blank(), panel.grid.major.y = element_line(size = 0.05), panel.grid.major.x = element_line(size = 0.05), axis.text.y = element_text(color = "black"), axis.text.x = element_text(color = "black"), axis.title.x = element_text(color = "black"), axis.title.y = element_text(color = "black"), legend.position="bottom", panel.border = element_rect(color="black", fill=NA)) +
    scale_fill_manual(values=c("#D7D29E","#1A476F")) + scale_y_continuous(limits = c(0,12), breaks = seq(0,10,by=2))
p53 <- ggplot(dfadv %>% filter(birthState=="Born in Texas" & loc1ord=="Lives in Illinois"), 
       aes(x = educvar, y = proba, fill = `Related Occupation`)
      ) +
    geom_col(color = "#1A476F", size=0.05) +
    labs(y = NULL, x = "Major") + theme_minimal() +
    theme(panel.grid.minor = element_blank(), panel.grid.major.y = element_line(size = 0.05), panel.grid.major.x = element_line(size = 0.05), axis.text.y = element_text(color = "black"), axis.text.x = element_text(color = "black"), axis.title.x = element_text(color = "black"), axis.title.y = element_text(color = "black"), legend.position="bottom", panel.border = element_rect(color="black", fill=NA)) +
    scale_fill_manual(values=c("#D7D29E","#1A476F")) + scale_y_continuous(limits = c(0,12), breaks = seq(0,10,by=2))
p54 <- ggplot(dfadv %>% filter(birthState=="Born in Texas" & loc1ord=="Lives in New York"), 
       aes(x = educvar, y = proba, fill = `Related Occupation`)
      ) +
    geom_col(color = "#1A476F", size=0.05) +
    labs(y = NULL, x = "Major") + theme_minimal() +
    theme(panel.grid.minor = element_blank(), panel.grid.major.y = element_line(size = 0.05), panel.grid.major.x = element_line(size = 0.05), axis.text.y = element_text(color = "black"), axis.text.x = element_text(color = "black"), axis.title.x = element_text(color = "black"), axis.title.y = element_text(color = "black"), legend.position="bottom", panel.border = element_rect(color="black", fill=NA)) +
    scale_fill_manual(values=c("#D7D29E","#1A476F")) + scale_y_continuous(limits = c(0,12), breaks = seq(0,10,by=2))
p55 <- ggplot(dfadv %>% filter(birthState=="Born in Texas" & loc1ord=="Lives in Texas"), 
       aes(x = educvar, y = proba, fill = `Related Occupation`)
      ) +
    geom_col(color = "#1A476F", size=0.05) +
    labs(y = NULL, x = "Major") + theme_minimal() +
    theme(panel.grid.minor = element_blank(), panel.grid.major.y = element_line(size = 0.05), panel.grid.major.x = element_line(size = 0.05), axis.text.y = element_text(color = "black"), axis.text.x = element_text(color = "black"), axis.title.x = element_text(color = "black"), axis.title.y = element_text(color = "black"), legend.position="bottom", panel.border = element_rect(color="black", fill=NA)) +
    scale_fill_manual(values=c("#D7D29E","#1A476F")) + scale_y_continuous(limits = c(0,85), breaks = seq(0,80,by=20))
(p11 | p12 | p13 | p14 | p15) / (p21 | p22 | p23 | p24 | p25) / (p31 | p32 | p33 | p34 | p35) / (p41 | p42 | p43 | p44 | p45) / (p51 | p52 | p53 | p54 | p55) + plot_layout(guides = "collect") & theme(legend.position='bottom') 
ggsave("../Paper/Graphics/barTranMatAdvDeg.eps", device = "eps", width=11.6, height = 9, units = "in")


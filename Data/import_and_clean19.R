library(tidyverse)    # data manipulation and plots
library(magrittr)     # piping
library(ipumsr)       # manage IPUMS data
library(dtplyr)       # faster data manipulation
library(data.table)   # faster data manipulation 

ddi45 <- read_ipums_ddi("usa_00045.xml")
df   <- read_ipums_micro(ddi45)

#::::::::::::::::::::::::::::::::::::::::::::::
# adjust data set properties
#::::::::::::::::::::::::::::::::::::::::::::::
names(df) <- tolower(names(df))
for (i in colnames(df)){
    df[[i]] <- zap_labels(df[[i]])
}
#df %<>% mutate_if(is.labelled, ~as_factor(lbl_clean(.))) # use this if you want to convert val labels to factors (I'll do that myself later)

#::::::::::::::::::::::::::::::::::::::::::::::
# create various variables of interest
#::::::::::::::::::::::::::::::::::::::::::::::
# weights
df %<>% mutate(hhwt  = hhwt  / 100)
df %<>% mutate(perwt = perwt / 100)

# education completed
df %<>% mutate(highDeg = case_when(
                                   educd >= 000 & educd <= 061           ~ 1, # Dropout
                                   educd >= 064 & educd <= 064           ~ 2, # GED
                                   educd %in% c(062,063,065,070,071,080) ~ 3, # HS
                                   educd >= 081 & educd <= 100           ~ 4, # AA 
                                   educd >= 101 & educd <= 113           ~ 5, # BA  
                                   educd >= 114 & educd <= 114           ~ 6, # MA  
                                   educd >= 115 & educd <= 115           ~ 7, # Prof
                                   educd >= 116 & educd <= 116           ~ 8  # PhD 
                                  )
              ) %>%
        mutate(hgc = case_when(
                               educd >= 000 & educd <= 012 ~ 0,
                               educd == 013 ~ 2 ,
                               educd == 014 ~ 1 ,
                               educd == 015 ~ 2 ,
                               educd == 016 ~ 3 ,
                               educd == 017 ~ 4 ,
                               educd == 020 ~ 6.5 ,
                               educd == 021 ~ 5.5 ,
                               educd == 022 ~ 5 ,
                               educd == 023 ~ 6 ,
                               educd == 024 ~ 7.5 ,
                               educd == 025 ~ 7 ,
                               educd == 026 ~ 8 ,
                               educd == 030 ~ 9 ,
                               educd == 040 ~ 10,
                               educd == 050 ~ 11,
                               educd == 060 ~ 12,
                               educd == 061 ~ 11.5,
                               educd == 062 ~ 12,
                               educd == 063 ~ 12,
                               educd == 064 ~ 12,
                               educd == 065 ~ 12.5,
                               educd == 070 ~ 13,
                               educd == 071 ~ 13.5,
                               educd == 080 ~ 14,
                               educd == 081 ~ 14,
                               educd == 082 ~ 14,
                               educd == 083 ~ 14,
                               educd == 090 ~ 15,
                               educd == 100 ~ 16,
                               educd == 101 ~ 16,
                               educd == 110 ~ 18,
                               educd == 111 ~ 18,
                               educd == 112 ~ 19,
                               educd == 113 ~ 20,
                               educd == 114 ~ 18,
                               educd == 115 ~ 19,
                               educd == 116 ~ 20,
                               educd == 999 ~ NA_real_
                              )
              ) %>%
# race
        mutate(racer = case_when(
                                 race==1 & hispan==0             ~ 1, # white
                                 race==2 & hispan==0             ~ 2, # black
                                 hispan>0                        ~ 3, # hispanic
                                 !(race %in% c(1,2)) & hispan==0 ~ 4  # other
                                ),
               racer = as.factor(racer),
               racer = fct_recode(racer, "White" = "1","Black" = "2","Hispanic" = "3","Other" = "4")
              ) %>%
# sex
        mutate(female = sex==2) %>%
# Aggregate majors a la Altonji, Kahn, and Speer:
        mutate(major = case_when(
                                 degfieldd %in% c(3702,3700,3701,4005) ~  1,
                                 degfieldd %in% c(5007) ~  2,
                                 degfieldd %in% c(2410,2411,2415,2416,2417,2418,2419,2499,2400,2401,2402,2403,2404,2412,2413,2501,2407,5008) ~  3,
                                 degfieldd %in% c(2414) ~  4,
                                 degfieldd %in% c(2408) ~  5,
                                 degfieldd %in% c(5003) ~  6,
                                 degfieldd %in% c(2102,2100,2105,2106,2107) ~  7,
                                 degfieldd %in% c(2406) ~  8,
                                 degfieldd %in% c(2405) ~  9,
                                 degfieldd %in% c(2502,2503,2504,2599,2500,5102) ~ 10,
                                 degfieldd %in% c(5000,5005,5006,5004,5002,5001,2409) ~ 11,
                                 degfieldd %in% c(2309) ~ 12,
                                 degfieldd %in% c(2101) ~ 13,
                                 degfieldd %in% c(3699,3606,3609,3611,3603,3602,3604,3600,3605,3601,4006,3607,3608,4003) ~ 14,
                                 degfieldd %in% c(5801,5601,5701,6000,5901) ~ 15,
                                 degfieldd %in% c(5501) ~ 16,
                                 degfieldd %in% c(6207) ~ 17,
                                 degfieldd %in% c(1301,1302,1303) ~ 18,
                                 degfieldd %in% c(6104,6106,6299,6200,6204,6205,6210) ~ 19,
                                 degfieldd %in% c(4008,5098,4000) ~ 20,
                                 degfieldd %in% c(6201,6202,6212) ~ 21,
                                 degfieldd %in% c(6206) ~ 22,
                                 degfieldd %in% c(1401) ~ 23,
                                 degfieldd %in% c(2313,2311,2306,2305,3501,2300,2301,2304,2307,2310,2312,2399,2314,2308) ~ 24,
                                 degfieldd %in% c(6203) ~ 25,
                                 degfieldd %in% c(1100,1101,1102,1103,1105,1106,1199,1104) ~ 26,
                                 degfieldd %in% c(2901) ~ 27,
                                 degfieldd %in% c(6004) ~ 28,
                                 degfieldd %in% c(5200,5201,5202,5203,5205,5206,5299) ~ 29,
                                 degfieldd %in% c(2602,2603) ~ 30,
                                 degfieldd %in% c(6002,6001) ~ 31,
                                 degfieldd %in% c(4002,4101) ~ 32,
                                 degfieldd %in% c(6211,2201) ~ 33,
                                 degfieldd %in% c(1901,1903,1904,2001) ~ 34,
                                 degfieldd %in% c(6108,6103,6100,6199,6109,6102)~ 35,
                                 degfieldd %in% c(5599,4007,5507,5500,5502,5504,5503) ~ 36,
                                 degfieldd %in% c(1501) ~ 37,
                                 degfieldd %in% c(3301,3401,3402,2601,3302) ~ 38,
                                 degfieldd %in% c(5506) ~ 39,
                                 degfieldd %in% c(5301) ~ 40,
                                 degfieldd %in% c(6402,6403) ~ 41,
                                 degfieldd %in% c(4001,5505) ~ 42,
                                 degfieldd %in% c(6000,6006,6099)~ 43,
                                 degfieldd %in% c(3202,5401,5402) ~ 44,
                                 degfieldd %in% c(5403,5404,6209) ~ 45,
                                 degfieldd %in% c(4801,4901) ~ 46,
                                 degfieldd %in% c(1902) ~ 47,
                                 degfieldd %in% c(6105) ~ 48,
                                 degfieldd %in% c(6110,2303) ~ 49,
                                 degfieldd %in% c(6003,6005,6007) ~ 50,
                                 degfieldd %in% c(6107) ~ 51,
                                 degfieldd %in% c(3201,3801) ~ NA_real_,
                                 TRUE ~ NA_real_
                                )
              ) %>%
        mutate(major2 = case_when(
                                  degfield2d %in% c(3702,3700,3701,4005) ~  1,
                                  degfield2d %in% c(5007) ~  2,
                                  degfield2d %in% c(2410,2411,2415,2416,2417,2418,2419,2499,2400,2401,2402,2403,2404,2412,2413,2501,2407,5008) ~  3,
                                  degfield2d %in% c(2414) ~  4,
                                  degfield2d %in% c(2408) ~  5,
                                  degfield2d %in% c(5003) ~  6,
                                  degfield2d %in% c(2102,2100,2105,2106,2107) ~  7,
                                  degfield2d %in% c(2406) ~  8,
                                  degfield2d %in% c(2405) ~  9,
                                  degfield2d %in% c(2502,2503,2504,2599,2500,5102) ~ 10,
                                  degfield2d %in% c(5000,5005,5006,5004,5002,5001,2409) ~ 11,
                                  degfield2d %in% c(2309) ~ 12,
                                  degfield2d %in% c(2101) ~ 13,
                                  degfield2d %in% c(3699,3606,3609,3611,3603,3602,3604,3600,3605,3601,4006,3607,3608,4003) ~ 14,
                                  degfield2d %in% c(5801,5601,5701,6000,5901) ~ 15,
                                  degfield2d %in% c(5501) ~ 16,
                                  degfield2d %in% c(6207) ~ 17,
                                  degfield2d %in% c(1301,1302,1303) ~ 18,
                                  degfield2d %in% c(6104,6106,6299,6200,6204,6205,6210) ~ 19,
                                  degfield2d %in% c(4008,5098,4000) ~ 20,
                                  degfield2d %in% c(6201,6202,6212) ~ 21,
                                  degfield2d %in% c(6206) ~ 22,
                                  degfield2d %in% c(1401) ~ 23,
                                  degfield2d %in% c(2313,2311,2306,2305,3501,2300,2301,2304,2307,2310,2312,2399,2314,2308) ~ 24,
                                  degfield2d %in% c(6203) ~ 25,
                                  degfield2d %in% c(1100,1101,1102,1103,1105,1106,1199,1104) ~ 26,
                                  degfield2d %in% c(2901) ~ 27,
                                  degfield2d %in% c(6004) ~ 28,
                                  degfield2d %in% c(5200,5201,5202,5203,5205,5206,5299) ~ 29,
                                  degfield2d %in% c(2602,2603) ~ 30,
                                  degfield2d %in% c(6002,6001) ~ 31,
                                  degfield2d %in% c(4002,4101) ~ 32,
                                  degfield2d %in% c(6211,2201) ~ 33,
                                  degfield2d %in% c(1901,1903,1904,2001) ~ 34,
                                  degfield2d %in% c(6108,6103,6100,6199,6109,6102)~ 35,
                                  degfield2d %in% c(5599,4007,5507,5500,5502,5504,5503) ~ 36,
                                  degfield2d %in% c(1501) ~ 37,
                                  degfield2d %in% c(3301,3401,3402,2601,3302) ~ 38,
                                  degfield2d %in% c(5506) ~ 39,
                                  degfield2d %in% c(5301) ~ 40,
                                  degfield2d %in% c(6402,6403) ~ 41,
                                  degfield2d %in% c(4001,5505) ~ 42,
                                  degfield2d %in% c(6000,6006,6099)~ 43,
                                  degfield2d %in% c(3202,5401,5402) ~ 44,
                                  degfield2d %in% c(5403,5404,6209) ~ 45,
                                  degfield2d %in% c(4801,4901) ~ 46,
                                  degfield2d %in% c(1902) ~ 47,
                                  degfield2d %in% c(6105) ~ 48,
                                  degfield2d %in% c(6110,2303) ~ 49,
                                  degfield2d %in% c(6003,6005,6007) ~ 50,
                                  degfield2d %in% c(6107) ~ 51,
                                  degfield2d %in% c(3201,3801) ~ NA_real_,
                                  TRUE ~ NA_real_
                                )
              ) %>%
        mutate(majorAgg = recode(
                                 major,
                                       `21` = 2L,
                                       `26` = 10L,
                                       `3`  = 5L,
                                       `23` = 1L,
                                       `37` = 6L,
                                       `43` = 1L,
                                       `14` = 10L,
                                       `25` = 2L,
                                       `9`  = 5L,
                                       `6`  = 10L,
                                       `8`  = 5L,
                                       `28` = 1L,
                                       `34` = 3L,
                                       `7`  = 7L,
                                       `13` = 7L,
                                       `11` = 10L,
                                       `16` = 2L,
                                       `5`  = 5L,
                                       `10` = 5L,
                                       `18` = 10L,
                                       `27` = 11L,
                                       `50` = 1L,
                                       `17` = 2L,
                                       `32` = 8L,
                                       `30` = 6L,
                                       `41` = 6L,
                                       `42` = 11L,
                                       `47` = 3L,
                                       `33` = 12L,
                                       `38` = 6L,
                                       `24` = 4L,
                                       `22` = 2L,
                                       `1`  = 7L,
                                       `4`  = 5L,
                                       `48` = 8L,
                                       `19` = 2L,
                                       `20` = 10L,
                                       `31` = 1L,
                                       `51` = 8L,
                                       `35` = 8L,
                                       `36` = 11L,
                                       `46` = 11L,
                                       `2`  = 10L,
                                       `39` = 11L,
                                       `15` = 12L,
                                       `40` = 9L,
                                       `29` = 11L,
                                       `44` = 9L,
                                       `49` = 9L,
                                       `12` = 4L,
                                       `45` = 11L
                                )
              ) %>%
# migration / birth location
        mutate(bpl = replace(bpl,bpl==110,72), migplac1 = replace(migplac1,migplac1==110,72)) %>%
        mutate(bpl = replace(bpl,bpl==0  ,NA), migplac1 = replace(migplac1,migplac1==0  ,NA)) %>%
        mutate(bpl = replace(bpl,bpl==999,NA), migplac1 = replace(migplac1,migplac1==999,NA)) %>%
        mutate(foreignBorn   = bpl>=90 | is.na(bpl)) %>%
        mutate(birthState    = bpl) %>%
        mutate(currState     = statefip) %>%
        mutate(prevState     = migplac1, 
               statemove     = migrate1==3,
               statemove1    = statefip!=migplac1 & !(statefip %in% c(000,999)) & !(migplac1 %in% c(000,999)) & migplac1>=1 & migplac1<=110,
               statemove1    = replace(statemove1,migplac1 %in% c(000,999),NA),
               inBirthSt     = statefip==bpl & !(statefip %in% c(000,999)) & !(bpl %in% c(000,999)),
               prevInBirthSt = migplac1==bpl & !(migplac1 %in% c(000,999)) & !(bpl %in% c(000,999))
              )
print('cross-tab of statemove and statemove1')
table(df$statemove,df$statemove1) %>% print

# employment status
df %<>% mutate(emp   = empstat==1,
               unemp = empstat==2,
               nilf  = empstat==3)

# Spouse employment status, major, education, etc.
df %<>% arrange(year,serial,pernum) %>%
          group_by(year,serial)
df.sp <- df
df.sp %<>% select(year,serial,pernum,empstat,educd,major,majorAgg,incwage,age,occ1990,birthState) %>% 
           rename(spempstat = empstat, speducd = educd, spmajor = major, spmajorAgg = majorAgg, spincwage = incwage, spage = age, spocc = occ1990, spbirthState = birthState)
df %<>% left_join(df.sp, by=c('year','serial','sploc'='pernum'))
df.sp <- NULL

df %>% select(year,serial,pernum,sploc,incwage,spincwage,birthState,spbirthState,age,spage) %>% print(n=30)

# Create id
df %<>% group_by(serial,pernum,year) %>% mutate(id = cur_group_id()) %>% ungroup()
# check that this is a unique identifier
print(paste0('Number of rows in data: ',dim(df)[1]/1e6))
print(paste0('Number of rows in uniqued data: ',df %>% distinct(serial,pernum,year) %>% dim %>% `[`(1)/1e6))

# Create family structure and co-residence
df %<>% mutate(isSpouse = relate==2)
df %<>% group_by(serial,year) %>% mutate(anySp = max(isSpouse)) %>% ungroup()
df %<>% mutate(liveWithSpouse = (relate==2) | (anySp==1 & relate==1))
df %<>% mutate(isChild = relate %in% c(3,4))
df %<>% group_by(serial,year) %>% mutate(anyCh = max(isChild)) %>% ungroup()
df %<>% mutate(liveWithChildren = (relate %in% c(5,6)) | (anyCh==1 & relate %in% c(1,2)))
df %<>% mutate(isParent = relate %in% c(5,6))
df %<>% group_by(serial,year) %>% mutate(anyPar = max(isParent)) %>% ungroup()
df %<>% mutate(liveWithParents = (relate %in% c(3,4)) | (anyPar==1 & relate %in% c(1,2)))
df %<>% mutate(isSibling = relate %in% c(7,8))
df %<>% group_by(serial,year) %>% mutate(anySib = max(isSibling)) %>% ungroup()
df %<>% mutate(liveWithSiblings = (relate %in% c(7,8)) | (anySib==1 & relate %in% c(1,2)))
df %<>% mutate(isRelative = relate %in% c(9,10))
df %<>% group_by(serial,year) %>% mutate(anyRel = max(isRelative)) %>% ungroup()
df %<>% mutate(liveWithRelatives = (relate %in% c(9,10)) | (anyRel==1 & relate %in% c(1,2)))
df %<>% mutate(liveWithFamily = liveWithSpouse==1 | liveWithChildren==1 | liveWithParents==1 | liveWithSiblings==1 | liveWithRelatives==1)



#::::::::::::::::::::::::::::::::::::::::::::::
# sample selection behavior
#::::::::::::::::::::::::::::::::::::::::::::::
print(paste0('full sample size: ',dim(df)[1]/1e6))
aaa <- dim(df)[1]

df %<>% filter(year>2009)
print(paste0('drop year 2009 and prior: ',dim(df)[1]/1e6))
bbb <- dim(df)[1]

df %<>% filter(majorAgg!=0 & !is.na(majorAgg))
print(paste0('Drop people who are not exactly college graduates or who do not report major: ',dim(df)[1]/1e6))
ccc <- dim(df)[1]

df %<>% filter(age>=22 & age<=54)
print(paste0('Drop people outside of the age range 22-54: ',dim(df)[1]/1e6))
ddd <- dim(df)[1]

df %<>% filter(school==1)
print(paste0('Drop people currently in school: ',dim(df)[1]/1e6))
eee <- dim(df)[1]

df %<>% filter(!(gq %in% c(3,4)))
print(paste0('Drop people living in group quarters: ',dim(df)[1]/1e6))
fff <- dim(df)[1]

df %<>% filter(birthState>=1 & birthState<=56)
print(paste0('Drop immigrants: ',dim(df)[1]/1e6))
ggg <- dim(df)[1]

df %<>% filter( !is.na(incwage) )
print(paste0('Drop missing earnings: ',dim(df)[1]/1e6))
hhh <- dim(df)[1]

df %<>% filter( (incwage<=0 | incwage>=20000) )
print(paste0('Drop low earnings: ',dim(df)[1]/1e6))
iii <- dim(df)[1]

df %<>% filter(incwage<600000)
print(paste0('Drop high earnings: ',dim(df)[1]/1e6))
jjj <- dim(df)[1]

df %<>% filter(incwage!=0)
print(paste0('Drop zero earnings: ',dim(df)[1]/1e6))
kkk <- dim(df)[1]

df %<>% filter(!is.na(occ1990))
print(paste0('Drop missing occupations: ',dim(df)[1]/1e6))
lll <- dim(df)[1]

df %<>% filter(female==0)
print(paste0('Drop women: ',dim(df)[1]/1e6))
mmm <- dim(df)[1]

df %<>% filter(qincwage==0 & qocc==0)
print(paste0('Drop people with imputed earnings/occupations: ',dim(df)[1]/1e6))
nnn <- dim(df)[1]

df %<>% filter(qempstat==0) # qmarst==0 & qschool==0 & qeduc==0 & qrace==0 & qhispan==0 & qdegfield==0
print(paste0('Drop people with imputed other variables: ',dim(df)[1]/1e6))
ooo <- dim(df)[1]

source('sampleSelectionTable19.R')

#df %<>% filter(!(educd %in% c(114,115,116)))
#print(paste0('Drop advanced degrees',dim(df)[1]/1e6))

df %<>% mutate(sphgc = case_when(
                                 is.na(speducd) ~ NA_real_,
                                 speducd >= 000 & educd <= 012 ~ 0,
                                 speducd == 013 ~ 2 ,
                                 speducd == 014 ~ 1 ,
                                 speducd == 015 ~ 2 ,
                                 speducd == 016 ~ 3 ,
                                 speducd == 017 ~ 4 ,
                                 speducd == 020 ~ 6.5 ,
                                 speducd == 021 ~ 5.5 ,
                                 speducd == 022 ~ 5 ,
                                 speducd == 023 ~ 6 ,
                                 speducd == 024 ~ 7.5 ,
                                 speducd == 025 ~ 7 ,
                                 speducd == 026 ~ 8 ,
                                 speducd == 030 ~ 9 ,
                                 speducd == 040 ~ 10,
                                 speducd == 050 ~ 11,
                                 speducd == 060 ~ 12,
                                 speducd == 061 ~ 11.5,
                                 speducd == 062 ~ 12,
                                 speducd == 063 ~ 12,
                                 speducd == 064 ~ 12,
                                 speducd == 065 ~ 12.5,
                                 speducd == 070 ~ 13,
                                 speducd == 071 ~ 13.5,
                                 speducd == 080 ~ 14,
                                 speducd == 081 ~ 14,
                                 speducd == 082 ~ 14,
                                 speducd == 083 ~ 14,
                                 speducd == 090 ~ 15,
                                 speducd == 100 ~ 16,
                                 speducd == 101 ~ 16,
                                 speducd == 110 ~ 18,
                                 speducd == 111 ~ 18,
                                 speducd == 112 ~ 19,
                                 speducd == 113 ~ 20,
                                 speducd == 114 ~ 18,
                                 speducd == 115 ~ 19,
                                 speducd == 116 ~ 20,
                                 speducd == 999 ~ NA_real_
                                )
              )

df %<>% mutate(majorAggAgg = recode(           #  1 "Education" 2 "Social Sciences" 3 "Other" 4 "Business" 5 "STEM"
                                    majorAgg,
                                          `0`  = 0L,
                                          `4`  = 1L,
                                          `11` = 2L,
                                          `1`  = 3L,
                                          `3`  = 3L,
                                          `6`  = 3L,
                                          `9`  = 3L,
                                          `12` = 3L,
                                          `2`  = 4L,
                                          `5`  = 5L,
                                          `7`  = 5L,
                                          `8`  = 5L,
                                          `10` = 5L
                                   )
              )
table(df$majorAggAgg) %>% print

df %<>% mutate(spmajorAggAgg = recode(           #  1 "Education" 2 "Social Sciences" 3 "Other" 4 "Business" 5 "STEM"
                                      spmajorAgg,
                                              `0`  = 0L,
                                              `4`  = 1L,
                                              `11` = 2L,
                                              `1`  = 3L,
                                              `3`  = 3L,
                                              `6`  = 3L,
                                              `9`  = 3L,
                                              `12` = 3L,
                                              `2`  = 4L,
                                              `5`  = 5L,
                                              `7`  = 5L,
                                              `8`  = 5L,
                                              `10` = 5L
                                     )
               )

#--------------------------------------------
# Minor cleaning: generate education level,
# household co-residence structure,
# potential experience, marital status,
# drop outlier earnings, condense ind/occ,
# construct various geographic measures
#--------------------------------------------
df %<>% mutate(  advdegree = (  educd>=114 &   educd<=116))
df %<>% mutate(spadvdegree = (speducd>=114 & speducd<=116))

df %<>% mutate(educvar = case_when(
                                   (majorAgg %in% c(0)) & (educd>=002 & educd<=061) ~ 1, # "Dropout"
                                   (majorAgg %in% c(0)) & (educd>=062 & educd<=064) ~ 2, # "HS"
                                   (majorAgg %in% c(0)) & (educd>=065 & educd<=100) ~ 3, # "Some College"
                                   (majorAgg %in% c(4))                             ~ 4, # "Education Major"
                                   (majorAgg %in% c(11))                            ~ 5, # "Social Sciences Major"
                                   (majorAgg %in% c(1,3,6,9,12))                    ~ 6, # "Other Major"
                                   (majorAgg %in% c(2))                             ~ 7, # "Business Major"
                                   (majorAgg %in% c(5,7,8,10))                      ~ 8, # "STEM Major"
                                   TRUE                                             ~ 6  #remaining cases are classified as "Other Major"
                                  )
)
table(df$educvar) %>% print

df %<>% mutate(speducvar = case_when(
                                     (spmajorAgg %in% c(0)) & (speducd>=002 & speducd<=061) ~ 1, # "Dropout"
                                     (spmajorAgg %in% c(0)) & (speducd>=062 & speducd<=064) ~ 2, # "HS"
                                     (spmajorAgg %in% c(0)) & (speducd>=065 & speducd<=100) ~ 3, # "Some College"
                                     (spmajorAgg %in% c(4))                                 ~ 4, # "Education Major"
                                     (spmajorAgg %in% c(11))                                ~ 5, # "Social Sciences Major"
                                     (spmajorAgg %in% c(1,3,6,9,12))                        ~ 6, # "Other Major"
                                     (spmajorAgg %in% c(2))                                 ~ 7, # "Business Major"
                                     (spmajorAgg %in% c(5,7,8,10))                          ~ 8, # "STEM Major"
                                     TRUE                                                   ~ 6  #remaining cases are classified as "Other Major"
                                    )
)


# household co-residence structure
df %<>% mutate(childlt5  = (nchlt5>0 & !is.na(nchlt5)),
               child518  = (nchild>0 & childlt5==0),
               childlt18 = nchild>0,
               childcat = 1*childlt5 + 2*child518)


df %<>% mutate(married  = (marst %in% c(1,2)),
               divorced = (marst %in% c(3,4)),
               widowed  = (marst %in% c(5)  ),
               single   = (marst %in% c(6)  ),
               marital = 1*married + 2*divorced + 3*(single | widowed))

df %<>% mutate(spwork     = spempstat==1,
               spwork     = replace(spwork,is.na(spwork),0))
df %<>% mutate(spbornHere = spbirthState==birthState,
               spbornHere = replace(spbornHere,is.na(spbornHere),0))
df %<>% mutate(exper      = age-hgc-6,
               exper      = replace(exper,exper<0,0))

summary(df$incwage) %>% print
sum(!is.na(df$incwage)) %>% print
summary(log(df$incwage)) %>% print
sum(!is.na(log(df$incwage))) %>% print
df %<>% mutate(cpi = case_when(year==2009 ~ 0.98, year==2010 ~ 1, year==2011 ~ 1.03, year==2012 ~ 1.05, year==2013 ~ 1.07, year==2014 ~ 1.09, year==2015 ~ 1.10, year==2016 ~ 1.12, year==2017 ~ 1.14, year==2018 ~ 1.17, year==2019 ~ 1.19))
df %<>% mutate(loginc      = log(incwage/cpi),
               logincnodef = log(incwage))
summary(df$loginc) %>% print
sum(!is.na(df$loginc)) %>% print
summary(df$logincnodef) %>% print
sum(!is.na(df$logincnodef)) %>% print

df %<>% mutate(birthState = replace(birthState,birthState>56,99))
df %<>% mutate(birthState = na_if(birthState,birthState==99))
# 1 "Alabama"  2 "Alaska"  4 "Arizona"  5 "Arkansas"  6 "California"  8 "Colorado"  9 "Connecticut" 10 "Delaware" 11 "DistrictOfColumbia" 12 "Florida" 13 "Georgia" 15 "Hawaii" 16 "Idaho" 17 "Illinois" 18 "Indiana" 19 "Iowa" 20 "Kansas" 21 "Kentucky" 22 "Louisiana" 23 "Maine" 24 "Maryland" 25 "Massachusetts" 26 "Michigan" 27 "Minnesota" 28 "Mississippi" 29 "Missouri" 30 "Montana" 31 "Nebraska" 32 "Nevada" 33 "NewHampshire" 34 "NewJersey" 35 "NewMexico" 36 "NewYork" 37 "NorthCarolina" 38 "NorthDakota" 39 "Ohio" 40 "Oklahoma" 41 "Oregon" 42 "Pennsylvania" 44 "RhodeIsland" 45 "SouthCarolina" 46 "SouthDakota" 47 "Tennessee" 48 "Texas" 49 "Utah" 50 "Vermont" 51 "Virginia" 53 "Washington" 54 "WestVirginia" 55 "Wisconsin" 56 "Wyoming" 99 "Foreign born"

df %<>% mutate(birthmig = !inBirthSt)
df %<>% mutate(mover    = birthmig)
df %<>% mutate(stayer   = !mover)

# aggregate residence state to 15 areas
# 1 "CA" 2 "TX" 3 "FL" 4 "IL" 5 "NY" 6 "New England" 7 "NJ/PA" 8 "DMV" 9 "NC/SC/GA" 10 "E S Central" 11 "OH/IN/MI/WI" 12 "W N Central" 13 "OK/AR/LA" 14 "Mountain" 15 "Pacific \ {CA}"
df %<>% mutate(loc1 = case_when(
                                statefip==1  ~ 10,
                                statefip==2  ~ 15,
                                statefip==4  ~ 14,
                                statefip==5  ~ 13,
                                statefip==6  ~ 1 ,
                                statefip==8  ~ 14,
                                statefip==9  ~ 6 ,
                                statefip==10 ~ 8 ,
                                statefip==11 ~ 8 ,
                                statefip==12 ~ 3 ,
                                statefip==13 ~ 9 ,
                                statefip==15 ~ 15,
                                statefip==16 ~ 14,
                                statefip==17 ~ 4 ,
                                statefip==18 ~ 11,
                                statefip==19 ~ 12,
                                statefip==20 ~ 12,
                                statefip==21 ~ 10,
                                statefip==22 ~ 13,
                                statefip==23 ~ 6 ,
                                statefip==24 ~ 8 ,
                                statefip==25 ~ 6 ,
                                statefip==26 ~ 11,
                                statefip==27 ~ 12,
                                statefip==28 ~ 10,
                                statefip==29 ~ 12,
                                statefip==30 ~ 14,
                                statefip==31 ~ 12,
                                statefip==32 ~ 14,
                                statefip==33 ~ 6 ,
                                statefip==34 ~ 7 ,
                                statefip==35 ~ 14,
                                statefip==36 ~ 5 ,
                                statefip==37 ~ 9 ,
                                statefip==38 ~ 12,
                                statefip==39 ~ 11,
                                statefip==40 ~ 13,
                                statefip==41 ~ 15,
                                statefip==42 ~ 7 ,
                                statefip==44 ~ 6 ,
                                statefip==45 ~ 9 ,
                                statefip==46 ~ 12,
                                statefip==47 ~ 10,
                                statefip==48 ~ 2 ,
                                statefip==49 ~ 14,
                                statefip==50 ~ 6 ,
                                statefip==51 ~ 8 ,
                                statefip==53 ~ 15,
                                statefip==54 ~ 8 ,
                                statefip==55 ~ 11,
                                statefip==56 ~ 14
                               )
              )

# aggregate birth state to 15 areas
# 1 "CA" 2 "TX" 3 "FL" 4 "IL" 5 "NY" 6 "New England" 7 "NJ/PA" 8 "DMV" 9 "NC/SC/GA" 10 "E S Central" 11 "OH/IN/MI/WI" 12 "W N Central" 13 "OK/AR/LA" 14 "Mountain" 15 "Pacific \ {CA}"
df %<>% mutate(birthloc = case_when(
                                    birthState==1  ~ 10,
                                    birthState==2  ~ 15,
                                    birthState==4  ~ 14,
                                    birthState==5  ~ 13,
                                    birthState==6  ~ 1 ,
                                    birthState==8  ~ 14,
                                    birthState==9  ~ 6 ,
                                    birthState==10 ~ 8 ,
                                    birthState==11 ~ 8 ,
                                    birthState==12 ~ 3 ,
                                    birthState==13 ~ 9 ,
                                    birthState==15 ~ 15,
                                    birthState==16 ~ 14,
                                    birthState==17 ~ 4 ,
                                    birthState==18 ~ 11,
                                    birthState==19 ~ 12,
                                    birthState==20 ~ 12,
                                    birthState==21 ~ 10,
                                    birthState==22 ~ 13,
                                    birthState==23 ~ 6 ,
                                    birthState==24 ~ 8 ,
                                    birthState==25 ~ 6 ,
                                    birthState==26 ~ 11,
                                    birthState==27 ~ 12,
                                    birthState==28 ~ 10,
                                    birthState==29 ~ 12,
                                    birthState==30 ~ 14,
                                    birthState==31 ~ 12,
                                    birthState==32 ~ 14,
                                    birthState==33 ~ 6 ,
                                    birthState==34 ~ 7 ,
                                    birthState==35 ~ 14,
                                    birthState==36 ~ 5 ,
                                    birthState==37 ~ 9 ,
                                    birthState==38 ~ 12,
                                    birthState==39 ~ 11,
                                    birthState==40 ~ 13,
                                    birthState==41 ~ 15,
                                    birthState==42 ~ 7 ,
                                    birthState==44 ~ 6 ,
                                    birthState==45 ~ 9 ,
                                    birthState==46 ~ 12,
                                    birthState==47 ~ 10,
                                    birthState==48 ~ 2 ,
                                    birthState==49 ~ 14,
                                    birthState==50 ~ 6 ,
                                    birthState==51 ~ 8 ,
                                    birthState==53 ~ 15,
                                    birthState==54 ~ 8 ,
                                    birthState==55 ~ 11,
                                    birthState==56 ~ 14
                                   )
              )

# aggregate spouse's birth state to 15 areas
# 1 "CA" 2 "TX" 3 "FL" 4 "IL" 5 "NY" 6 "New England" 7 "NJ/PA" 8 "DMV" 9 "NC/SC/GA" 10 "E S Central" 11 "OH/IN/MI/WI" 12 "W N Central" 13 "OK/AR/LA" 14 "Mountain" 15 "Pacific \ {CA}"
df %<>% mutate(spbirthloc = case_when(
                                      spbirthState==1  ~ 10,
                                      spbirthState==2  ~ 15,
                                      spbirthState==4  ~ 14,
                                      spbirthState==5  ~ 13,
                                      spbirthState==6  ~ 1 ,
                                      spbirthState==8  ~ 14,
                                      spbirthState==9  ~ 6 ,
                                      spbirthState==10 ~ 8 ,
                                      spbirthState==11 ~ 8 ,
                                      spbirthState==12 ~ 3 ,
                                      spbirthState==13 ~ 9 ,
                                      spbirthState==15 ~ 15,
                                      spbirthState==16 ~ 14,
                                      spbirthState==17 ~ 4 ,
                                      spbirthState==18 ~ 11,
                                      spbirthState==19 ~ 12,
                                      spbirthState==20 ~ 12,
                                      spbirthState==21 ~ 10,
                                      spbirthState==22 ~ 13,
                                      spbirthState==23 ~ 6 ,
                                      spbirthState==24 ~ 8 ,
                                      spbirthState==25 ~ 6 ,
                                      spbirthState==26 ~ 11,
                                      spbirthState==27 ~ 12,
                                      spbirthState==28 ~ 10,
                                      spbirthState==29 ~ 12,
                                      spbirthState==30 ~ 14,
                                      spbirthState==31 ~ 12,
                                      spbirthState==32 ~ 14,
                                      spbirthState==33 ~ 6 ,
                                      spbirthState==34 ~ 7 ,
                                      spbirthState==35 ~ 14,
                                      spbirthState==36 ~ 5 ,
                                      spbirthState==37 ~ 9 ,
                                      spbirthState==38 ~ 12,
                                      spbirthState==39 ~ 11,
                                      spbirthState==40 ~ 13,
                                      spbirthState==41 ~ 15,
                                      spbirthState==42 ~ 7 ,
                                      spbirthState==44 ~ 6 ,
                                      spbirthState==45 ~ 9 ,
                                      spbirthState==46 ~ 12,
                                      spbirthState==47 ~ 10,
                                      spbirthState==48 ~ 2 ,
                                      spbirthState==49 ~ 14,
                                      spbirthState==50 ~ 6 ,
                                      spbirthState==51 ~ 8 ,
                                      spbirthState==53 ~ 15,
                                      spbirthState==54 ~ 8 ,
                                      spbirthState==55 ~ 11,
                                      spbirthState==56 ~ 14
                                     )
              )

# double check that states got aggregated correctly
table(df$statefip,df$loc1) %>% print
table(df$birthState,df$birthloc) %>% print

# Generate birth states (matrix)
df %<>% mutate(bornHere1  = birthloc==1 )
df %<>% mutate(bornHere2  = birthloc==2 )
df %<>% mutate(bornHere3  = birthloc==3 )
df %<>% mutate(bornHere4  = birthloc==4 )
df %<>% mutate(bornHere5  = birthloc==5 )
df %<>% mutate(bornHere6  = birthloc==6 )
df %<>% mutate(bornHere7  = birthloc==7 )
df %<>% mutate(bornHere8  = birthloc==8 )
df %<>% mutate(bornHere9  = birthloc==9 )
df %<>% mutate(bornHere10 = birthloc==10)
df %<>% mutate(bornHere11 = birthloc==11)
df %<>% mutate(bornHere12 = birthloc==12)
df %<>% mutate(bornHere13 = birthloc==13)
df %<>% mutate(bornHere14 = birthloc==14)
df %<>% mutate(bornHere15 = birthloc==15)

# Generate spouse birth states (matrix)
df %<>% mutate(spbornHere1  = spbirthloc==1 ,
               spbornHere1  = replace(spbornHere1, is.na(spbornHere1), 0))
df %<>% mutate(spbornHere2  = spbirthloc==2 ,
               spbornHere2  = replace(spbornHere2, is.na(spbornHere2), 0))
df %<>% mutate(spbornHere3  = spbirthloc==3 ,
               spbornHere3  = replace(spbornHere3, is.na(spbornHere3), 0))
df %<>% mutate(spbornHere4  = spbirthloc==4 ,      
               spbornHere4  = replace(spbornHere4, is.na(spbornHere4), 0))
df %<>% mutate(spbornHere5  = spbirthloc==5 ,
               spbornHere5  = replace(spbornHere5, is.na(spbornHere5), 0))
df %<>% mutate(spbornHere6  = spbirthloc==6 ,
               spbornHere6  = replace(spbornHere6, is.na(spbornHere6), 0))
df %<>% mutate(spbornHere7  = spbirthloc==7 ,
               spbornHere7  = replace(spbornHere7, is.na(spbornHere7), 0))
df %<>% mutate(spbornHere8  = spbirthloc==8 ,                          
               spbornHere8  = replace(spbornHere8, is.na(spbornHere8), 0))
df %<>% mutate(spbornHere9  = spbirthloc==9 ,
               spbornHere9  = replace(spbornHere9, is.na(spbornHere9), 0))
df %<>% mutate(spbornHere10 = spbirthloc==10,
               spbornHere10 = replace(spbornHere10,is.na(spbornHere10),0))
df %<>% mutate(spbornHere11 = spbirthloc==11,
               spbornHere11 = replace(spbornHere11,is.na(spbornHere11),0))
df %<>% mutate(spbornHere12 = spbirthloc==12,
               spbornHere12 = replace(spbornHere12,is.na(spbornHere12),0))
df %<>% mutate(spbornHere13 = spbirthloc==13,
               spbornHere13 = replace(spbornHere13,is.na(spbornHere13),0))
df %<>% mutate(spbornHere14 = spbirthloc==14,
               spbornHere14 = replace(spbornHere14,is.na(spbornHere14),0))
df %<>% mutate(spbornHere15 = spbirthloc==15,
               spbornHere15 = replace(spbornHere15,is.na(spbornHere15),0))

# Generate same-division states
df %<>% mutate(bornDiv1  = birthloc %in% c(1,15) )
df %<>% mutate(bornDiv2  = birthloc %in% c(2,13) )
df %<>% mutate(bornDiv3  = birthloc %in% c(3,8,9))
df %<>% mutate(bornDiv4  = birthloc %in% c(4,11) )
df %<>% mutate(bornDiv5  = birthloc %in% c(5,7)  )
df %<>% mutate(bornDiv6  = birthloc %in% c(6)    )
df %<>% mutate(bornDiv7  = birthloc %in% c(5,7)  )
df %<>% mutate(bornDiv8  = birthloc %in% c(3,8,9))
df %<>% mutate(bornDiv9  = birthloc %in% c(3,8,9))
df %<>% mutate(bornDiv10 = birthloc %in% c(10)   )
df %<>% mutate(bornDiv11 = birthloc %in% c(4,11) )
df %<>% mutate(bornDiv12 = birthloc %in% c(12)   )
df %<>% mutate(bornDiv13 = birthloc %in% c(2,13) )
df %<>% mutate(bornDiv14 = birthloc %in% c(14)   )
df %<>% mutate(bornDiv15 = birthloc %in% c(1,15) )

df %<>% mutate(bornReg1  = birthloc %in% c(1,14,15)      )
df %<>% mutate(bornReg2  = birthloc %in% c(2,3,8,9,10,13))
df %<>% mutate(bornReg3  = birthloc %in% c(2,3,8,9,10,13))
df %<>% mutate(bornReg4  = birthloc %in% c(4,11,12)      )
df %<>% mutate(bornReg5  = birthloc %in% c(5,6,7)        )
df %<>% mutate(bornReg6  = birthloc %in% c(5,6,7)        )
df %<>% mutate(bornReg7  = birthloc %in% c(5,6,7)        )
df %<>% mutate(bornReg8  = birthloc %in% c(2,3,8,9,10,13))
df %<>% mutate(bornReg9  = birthloc %in% c(2,3,8,9,10,13))
df %<>% mutate(bornReg10 = birthloc %in% c(2,3,8,9,10,13))
df %<>% mutate(bornReg11 = birthloc %in% c(4,11,12)      )
df %<>% mutate(bornReg12 = birthloc %in% c(4,11,12)      )
df %<>% mutate(bornReg13 = birthloc %in% c(2,3,8,9,10,13))
df %<>% mutate(bornReg14 = birthloc %in% c(1,14,15)      )
df %<>% mutate(bornReg15 = birthloc %in% c(1,14,15)      )

#--------------------------------------------
# Merge distance migrated
#--------------------------------------------
statedist <- read_csv("stateDistances.csv") %>% select(birthState,statefip,dist)
df %<>% left_join(statedist, by=c("birthState","statefip"))

# add non-college grads as categories of "major"
df %<>% mutate(major = replace(major,educvar==3,52),
               major = replace(major,educvar==2,53),
               major = replace(major,educvar==1,54))

#--------------------------------------------
# Related occupation
#--------------------------------------------
df.nmaj      <- df %>% group_by(major,advdegree)         %>% filter(!is.na(loginc)) %>% summarize(N_maj     = sum(perwt))
df.n.maj.occ <- df %>% group_by(major,advdegree,occ1990) %>% filter(!is.na(loginc)) %>% summarize(N_maj_occ = sum(perwt))
df.occ       <- left_join(df.n.maj.occ,df.nmaj, by=c("major","advdegree")) %>% mutate(occ_shr = N_maj_occ/N_maj)
df         %<>% left_join(df.occ, by=c("major","advdegree","occ1990"))

df %<>% mutate(relocc = occ_shr>.02 & !is.na(occ_shr))
table(df$major,df$relocc) #[aw=perwt] if !mi(loginc), row nofreq
table(df$majorAggAgg,df$relocc) #[aw=perwt] if !mi(loginc), row nofreq

df %<>% mutate(loc1occ = case_when(
                                   relocc==1 ~ loc1,
                                   relocc==0 ~ loc1+15
                                  )
)
# 1 "CaliforniaRelated" 2 "TexasRelated" 3 "FloridaRelated" 4 "IllinoisRelated" 5 "NewYorkRelated" 6 "NewEnglandRelated" 7 "NJ/PARelated" 8 "DMVRelated" 9 "NC/SC/GARelated" 10 "ESCentralRelated" 11 "OH/IN/MI/WIRelated" 12 "WNCentralRelated" 13 "OK/AR/LARelated" 14 "MountainRelated" 15 "PacificRelated" 16 "CaliforniaUnrelated" 17 "TexasUnrelated" 18 "FloridaUnrelated" 19 "IllinoisUnrelated" 20 "NewYorkUnrelated" 21 "NewEnglandUnrelated" 22 "NJ/PAUnrelated" 23 "DMVUnrelated" 24 "NC/SC/GAUnrelated" 25 "ESCentralUnrelated" 26 "OH/IN/MI/WIUnrelated" 27 "WNCentralUnrelated" 28 "OK/AR/LAUnrelated" 29 "MountainUnrelated" 30 "PacificUnrelated" 

# get stayer and related occupation rates by various groups (for use as exclusion restrictions)
df %<>% group_by(educvar,advdegree,female,age,racer,birthState) %>% mutate(meanStay = weighted.mean(stayer,perwt),
                                                                           meanRelOcc = weighted.mean(relocc,perwt)) %>% ungroup
df %<>% group_by(educvar,advdegree,female,age,racer) %>% mutate(meanAggStay = weighted.mean(stayer,perwt),
                                                                meanAggRelOcc = weighted.mean(relocc,perwt)) %>% ungroup
#df %<>% group_by(educvar,advdegree,female,age,racer,loc1) %>% mutate(meanRelOccA = weighted.mean(relocc,perwt)) %>% ungroup
df %<>% mutate(residMeanRelOcc = meanRelOcc-meanAggRelOcc,residMeanStay = meanStay-meanAggStay)
df %<>% mutate(status = case_when(
                                  loc1==birthloc & relocc==1 ~ 1,
                                  loc1==birthloc & relocc==0 ~ 2,
                                  loc1!=birthloc & relocc==1 ~ 3,
                                  loc1!=birthloc & relocc==0 ~ 4))

#--------------------------------------------
# create factors
#--------------------------------------------
df %<>% mutate(across(c(educvar,birthState,statefip,birthloc,loc1,loc1occ,met2013,status),~as.factor(.))) %>% 
        mutate(educvar = fct_recode(educvar, "Education" = "4", "Social Science" = "5", "Other" = "6", "Business" = "7", "STEM" = "8"),
               status  = fct_recode(status, "stayerRelOcc" = "1", "stayerUnrelOcc" = "2", "moverRelOcc" = "3", "moverUnrelOcc" = "4"),
               birthloc = fct_recode(birthloc, "CA" = "1", "TX" = "2", "FL" = "3", "IL" = "4", "NY" = "5", "New England" = "6", "NJ/PA" = "7", "DMV" = "8", "NC/SC/GA" = "9", "E S Central" = "10", "OH/IN/MI/WI" = "11", "W N Central" = "12", "OK/AR/LA" = "13", "Mountain" = "14", "Pacific not CA" = "15"),
               loc1 = fct_recode(loc1, "CA" = "1", "TX" = "2", "FL" = "3", "IL" = "4", "NY" = "5", "New England" = "6", "NJ/PA" = "7", "DMV" = "8", "NC/SC/GA" = "9", "E S Central" = "10", "OH/IN/MI/WI" = "11", "W N Central" = "12", "OK/AR/LA" = "13", "Mountain" = "14", "Pacific not CA" = "15"),
               loc1occ = fct_recode(loc1occ, "California Related" = "1", "Texas Related" = "2", "Florida Related" = "3", "Illinois Related" = "4", "NewYork Related" = "5", "NewEngland Related" = "6", "NJ/PA Related" = "7", "DMV Related" = "8", "NC/SC/GA Related" = "9", "ESCentral Related" = "10", "OH/IN/MI/WI Related" = "11", "WNCentral Related" = "12", "OK/AR/LA Related" = "13", "Mountain Related" = "14", "Pacific Related" = "15", "California Unrelated" = "16", "Texas Unrelated" = "17", "Florida Unrelated" = "18", "Illinois Unrelated" = "19", "NewYork Unrelated" = "20", "NewEngland Unrelated" = "21", "NJ/PA Unrelated" = "22", "DMV Unrelated" = "23", "NC/SC/GA Unrelated" = "24", "ESCentral Unrelated" = "25", "OH/IN/MI/WI Unrelated" = "26", "WNCentral Unrelated" = "27", "OK/AR/LA Unrelated" = "28", "Mountain Unrelated" = "29", "Pacific Unrelated" = "30"),
               birthState = fct_recode(birthState, "AL" = "1", "AK" = "2", "AZ" = "4", "AR" = "5", "CA" = "6", "CO" = "8", "CT" = "9", "DE" = "10", "DC" = "11", "FL" = "12", "GA" = "13", "HI" = "15", "ID" = "16", "IL" = "17", "IN" = "18", "IA" = "19", "KS" = "20", "KY" = "21", "LA" = "22", "ME" = "23", "MD" = "24", "MA" = "25", "MI" = "26", "MN" = "27", "MS" = "28", "MO" = "29", "MT" = "30", "NE" = "31", "NV" = "32", "NH" = "33", "NJ" = "34", "NM" = "35", "NY" = "36", "NC" = "37", "ND" = "38", "OH" = "39", "OK" = "40", "OR" = "41", "PA" = "42", "RI" = "44", "SC" = "45", "SD" = "46", "TN" = "47", "TX" = "48", "UT" = "49", "VT" = "50", "VA" = "51", "WA" = "53", "WI" = "54", "WV" = "55", "WY" = "56"),
               statefip = fct_recode(statefip, "AL" = "1", "AK" = "2", "AZ" = "4", "AR" = "5", "CA" = "6", "CO" = "8", "CT" = "9", "DE" = "10", "DC" = "11", "FL" = "12", "GA" = "13", "HI" = "15", "ID" = "16", "IL" = "17", "IN" = "18", "IA" = "19", "KS" = "20", "KY" = "21", "LA" = "22", "ME" = "23", "MD" = "24", "MA" = "25", "MI" = "26", "MN" = "27", "MS" = "28", "MO" = "29", "MT" = "30", "NE" = "31", "NV" = "32", "NH" = "33", "NJ" = "34", "NM" = "35", "NY" = "36", "NC" = "37", "ND" = "38", "OH" = "39", "OK" = "40", "OR" = "41", "PA" = "42", "RI" = "44", "SC" = "45", "SD" = "46", "TN" = "47", "TX" = "48", "UT" = "49", "VT" = "50", "VA" = "51", "WA" = "53", "WI" = "54", "WV" = "55", "WY" = "56"),
               met2013 = fct_recode(met2013,"Not in identifiable area" = "0", "Akron, OH" = "10420", "Albany-Schenectady-Troy, NY" = "10580", "Albuquerque, NM" = "10740", "Alexandria, LA" = "10780", "Allentown-Bethlehem-Easton, PA-NJ" = "10900", "Altoona, PA" = "11020", "Amarillo, TX" = "11100", "Anchorage, AK" = "11260", "Ann Arbor, MI" = "11460", "Anniston-Oxford-Jacksonville, AL" = "11500", "Asheville, NC" = "11700", "Athens-Clarke County, GA" = "12020", "Atlanta-Sandy Springs-Roswell, GA" = "12060", "Atlantic City-Hammonton, NJ" = "12100", "Auburn-Opelika, AL" = "12220", "Augusta-Richmond County, GA-SC" = "12260", "Austin-Round Rock, TX" = "12420", "Bakersfield, CA" = "12540", "Baltimore-Columbia-Towson, MD" = "12580", "Bangor, ME" = "12620", "Barnstable Town, MA" = "12700", "Baton Rouge, LA" = "12940", "Battle Creek, MI" = "12980", "Beaumont-Port Arthur, TX" = "13140", "Bellingham, WA" = "13380", "Bend-Redmond, OR" = "13460", "Billings, MT" = "13740", "Binghamton, NY" = "13780", "Birmingham-Hoover, AL" = "13820", "Bismarck, ND" = "13900", "Blacksburg-Christiansburg-Radford, VA" = "13980", "Bloomington, IL" = "14010", "Bloomington, IN" = "14020", "Boise City, ID" = "14260", "Boston-Cambridge-Newton, MA-NH" = "14460", "Bremerton-Silverdale, WA" = "14740", "Bridgeport-Stamford-Norwalk, CT" = "14860", "Brownsville-Harlingen, TX" = "15180", "Buffalo-Cheektowaga-Niagara Falls, NY" = "15380", "Burlington, NC" = "15500", "Burlington-South Burlington, VT" = "15540", "Canton-Massillon, OH" = "15940", "Cape Coral-Fort Myers, FL" = "15980", "Champaign-Urbana, IL" = "16580", "Charleston, WV" = "16620", "Charleston-North Charleston, SC" = "16700", "Charlotte-Concord-Gastonia, NC-SC" = "16740", "Charlottesville, VA" = "16820", "Chattanooga, TN-GA" = "16860", "Chicago-Naperville-Elgin, IL-IN-WI" = "16980", "Chico, CA" = "17020", "Cincinnati, OH-KY-IN" = "17140", "Clarksville, TN-KY" = "17300", "Cleveland-Elyria, OH" = "17460", "Coeur d'Alene, ID" = "17660", "College Station-Bryan, TX" = "17780", "Colorado Springs, CO" = "17820", "Columbia, MO" = "17860", "Columbia, SC" = "17900", "Columbus, OH" = "18140", "Corpus Christi, TX" = "18580", "Dallas-Fort Worth-Arlington, TX" = "19100", "Daphne-Fairhope-Foley, AL" = "19300", "Davenport-Moline-Rock Island, IA-IL" = "19340", "Dayton, OH" = "19380", "Decatur, AL" = "19460", "Decatur, IL" = "19500", "Deltona-Daytona Beach-Ormond Beach, FL" = "19660", "Denver-Aurora-Lakewood, CO" = "19740", "Des Moines-West Des Moines, IA" = "19780", "Detroit-Warren-Dearborn, MI" = "19820", "Dover, DE" = "20100", "Durham-Chapel Hill, NC" = "20500", "East Stroudsburg, PA" = "20700", "Eau Claire, WI" = "20740", "El Centro, CA" = "20940", "Elizabethtown-Fort Knox, KY" = "21060", "Elkhart-Goshen, IN" = "21140", "El Paso, TX" = "21340", "Erie, PA" = "21500", "Eugene, OR" = "21660", "Evansville, IN-KY" = "21780", "Farmington, NM" = "22140", "Fayetteville, NC" = "22180", "Fayetteville-Springdale-Rogers, AR-MO" = "22220"), 
               met2013 = fct_recode(met2013,"Flagstaff, AZ" = "22380", "Flint, MI" = "22420", "Florence, SC" = "22500", "Florence-Muscle Shoals, AL" = "22520", "Fort Collins, CO" = "22660", "Fort Wayne, IN" = "23060", "Fresno, CA" = "23420", "Gadsden, AL" = "23460", "Gainesville, FL" = "23540", "Gainesville, GA" = "23580", "Glens Falls, NY" = "24020", "Goldsboro, NC" = "24140", "Grand Junction, CO" = "24300", "Grand Rapids-Wyoming, MI" = "24340", "Greeley, CO" = "24540", "Greensboro-High Point, NC" = "24660", "Greenville, NC" = "24780", "Greenville-Anderson-Mauldin, SC" = "24860", "Gulfport-Biloxi-Pascagoula, MS" = "25060", "Hammond, LA" = "25220", "Hanford-Corcoran, CA" = "25260", "Harrisburg-Carlisle, PA" = "25420", "Harrisonburg, VA" = "25500", "Hartford-West Hartford-East Hartford, CT" = "25540", "Hattiesburg, MS" = "25620", "Hickory-Lenoir-Morganton, NC" = "25860", "Hilton Head Island-Bluffton-Beaufort, SC" = "25940", "Homosassa Springs, FL" = "26140", "Houma-Thibodaux, LA" = "26380", "Houston-The Woodlands-Sugar Land, TX" = "26420", "Huntsville, AL" = "26620", "Indianapolis-Carmel-Anderson, IN" = "26900", "Iowa City, IA" = "26980", "Ithaca, NY" = "27060", "Jackson, MI" = "27100", "Jackson, MS" = "27140", "Jackson, TN" = "27180", "Jacksonville, FL" = "27260", "Jacksonville, NC" = "27340", "Janesville-Beloit, WI" = "27500", "Jefferson City, MO" = "27620", "Johnstown, PA" = "27780", "Joplin, MO" = "27900", "Kalamazoo-Portage, MI" = "28020", "Kankakee, IL" = "28100", "Kansas City, MO-KS" = "28140", "Kennewick-Richland, WA" = "28420", "Killeen-Temple, TX" = "28660", "Kingsport-Bristol-Bristol, TN-VA" = "28700", "Knoxville, TN" = "28940", "La Crosse-Onalaska, WI-MN" = "29100", "Lafayette, LA" = "29180", "Lafayette-West Lafayette, IN" = "29200", "Lake Charles, LA" = "29340", "Lake Havasu City-Kingman, AZ" = "29420", "Lakeland-Winter Haven, FL" = "29460", "Lancaster, PA" = "29540", "Lansing-East Lansing, MI" = "29620", "Laredo, TX" = "29700", "Las Cruces, NM" = "29740", "Las Vegas-Henderson-Paradise, NV" = "29820", "Lawrence, KS" = "29940", "Lebanon, PA" = "30140", "Lewiston-Auburn, ME" = "30340", "Lima, OH" = "30620", "Lincoln, NE" = "30700", "Little Rock-North Little Rock-Conway, AR" = "30780", "Los Angeles-Long Beach-Anaheim, CA" = "31080", "Louisville/Jefferson County, KY-IN" = "31140", "Lubbock, TX" = "31180", "Lynchburg, VA" = "31340", "Madera, CA" = "31460", "Manchester-Nashua, NH" = "31700", "Mansfield, OH" = "31900", "Mayagüez, PR" = "32420", "McAllen-Edinburg-Mission, TX" = "32580", "Medford, OR" = "32780", "Memphis, TN-MS-AR" = "32820", "Merced, CA" = "32900", "Miami-Fort Lauderdale-West Palm Beach, FL" = "33100", "Michigan City-La Porte, IN" = "33140", "Midland, TX" = "33260", "Milwaukee-Waukesha-West Allis, WI" = "33340", "Minneapolis-St. Paul-Bloomington, MN-WI" = "33460", "Mobile, AL" = "33660", "Modesto, CA" = "33700", "Monroe, LA" = "33740", "Monroe, MI" = "33780", "Montgomery, AL" = "33860", "Morgantown, WV" = "34060"), 
               met2013 = fct_recode(met2013,"Muncie, IN" = "34620", "Muskegon, MI" = "34740", "Myrtle Beach-Conway-North Myrtle Beach, SC-NC" = "34820", "Napa, CA" = "34900", "Naples-Immokalee-Marco Island, FL" = "34940", "Nashville-Davidson--Murfreesboro--Franklin, TN" = "34980", "New Haven-Milford, CT" = "35300", "New Orleans-Metairie, LA" = "35380", "New York-Newark-Jersey City, NY-NJ-PA" = "35620", "Niles-Benton Harbor, MI" = "35660", "North Port-Sarasota-Bradenton, FL" = "35840", "Norwich-New London, CT" = "35980", "Ocala, FL" = "36100", "Ocean City, NJ" = "36140", "Odessa, TX" = "36220", "Ogden-Clearfield, UT" = "36260", "Oklahoma City, OK" = "36420", "Olympia-Tumwater, WA" = "36500", "Omaha-Council Bluffs, NE-IA" = "36540", "Orlando-Kissimmee-Sanford, FL" = "36740", "Oshkosh-Neenah, WI" = "36780", "Owensboro, KY" = "36980", "Oxnard-Thousand Oaks-Ventura, CA" = "37100", "Palm Bay-Melbourne-Titusville, FL" = "37340", "Panama City, FL" = "37460", "Parkersburg-Vienna, WV" = "37620", "Pensacola-Ferry Pass-Brent, FL" = "37860", "Peoria, IL" = "37900", "Philadelphia-Camden-Wilmington, PA-NJ-DE-MD" = "37980", "Phoenix-Mesa-Scottsdale, AZ" = "38060", "Pittsburgh, PA" = "38300", "Pittsfield, MA" = "38340", "Ponce, PR" = "38660", "Portland-South Portland, ME" = "38860", "Portland-Vancouver-Hillsboro, OR-WA" = "38900", "Port St. Lucie, FL" = "38940", "Prescott, AZ" = "39140", "Providence-Warwick, RI-MA" = "39300", "Provo-Orem, UT" = "39340", "Pueblo, CO" = "39380", "Punta Gorda, FL" = "39460", "Racine, WI" = "39540", "Raleigh, NC" = "39580", "Reading, PA" = "39740", "Redding, CA" = "39820", "Reno, NV" = "39900", "Richmond, VA" = "40060", "Riverside-San Bernardino-Ontario, CA" = "40140", "Roanoke, VA" = "40220", "Rochester, NY" = "40380", "Rockford, IL" = "40420", "Rocky Mount, NC" = "40580", "Sacramento--Roseville--Arden-Arcade, CA" = "40900", "Saginaw, MI" = "40980", "St. Cloud, MN" = "41060", "St. George, UT" = "41100", "St. Joseph, MO-KS" = "41140", "St. Louis, MO-IL" = "41180", "Salinas, CA" = "41500", "Salisbury, MD-DE" = "41540", "Salt Lake City, UT" = "41620", "San Angelo, TX" = "41660", "San Antonio-New Braunfels, TX" = "41700", "San Diego-Carlsbad, CA" = "41740", "San Francisco-Oakland-Hayward, CA" = "41860", "San Germán, PR" = "41900", "San Jose-Sunnyvale-Santa Clara, CA" = "41940", "San Juan-Carolina-Caguas, PR" = "41980", "San Luis Obispo-Paso Robles-Arroyo Grande, CA" = "42020", "Santa Cruz-Watsonville, CA" = "42100", "Santa Fe, NM" = "42140", "Santa Maria-Santa Barbara, CA" = "42200", "Santa Rosa, CA" = "42220", "Scranton--Wilkes-Barre--Hazleton, PA" = "42540", "Seattle-Tacoma-Bellevue, WA" = "42660", "Sebastian-Vero Beach, FL" = "42680", "Sheboygan, WI" = "43100", "Shreveport-Bossier City, LA" = "43340", "Spartanburg, SC" = "43900", "Spokane-Spokane Valley, WA" = "44060", "Springfield, IL" = "44100", "Springfield, MA" = "44140", "Springfield, MO" = "44180", "Springfield, OH" = "44220", "State College, PA" = "44300"), 
               met2013 = fct_recode(met2013,"Stockton-Lodi, CA" = "44700", "Sumter, SC" = "44940", "Syracuse, NY" = "45060", "Tallahassee, FL" = "45220", "Tampa-St. Petersburg-Clearwater, FL" = "45300", "Terre Haute, IN" = "45460", "Toledo, OH" = "45780", "Topeka, KS" = "45820", "Trenton, NJ" = "45940", "Tucson, AZ" = "46060", "Tuscaloosa, AL" = "46220", "Tyler, TX" = "46340", "Urban Honolulu, HI" = "46520", "Utica-Rome, NY" = "46540", "Valdosta, GA" = "46660", "Vallejo-Fairfield, CA" = "46700", "Vineland-Bridgeton, NJ" = "47220", "Virginia Beach-Norfolk-Newport News, VA-NC" = "47260", "Visalia-Porterville, CA" = "47300", "Waco, TX" = "47380", "Washington-Arlington-Alexandria, DC-VA-MD-WV" = "47900", "Wausau, WI" = "48140", "Wenatchee, WA" = "48300", "Wichita, KS" = "48620", "Wichita Falls, TX" = "48660", "Williamsport, PA" = "48700", "Wilmington, NC" = "48900", "Winston-Salem, NC" = "49180", "Worcester, MA-CT" = "49340", "Yakima, WA" = "49420", "York-Hanover, PA" = "49620", "Youngstown-Warren-Boardman, OH-PA" = "49660", "Yuba City, CA" = "49700", "Yuma, AZ" = "49740") 
              )

save(df,file="acs19.rda")


library(magrittr)

# import transition matrix data (2019, obtained from https://www.census.gov/data/tables/time-series/demo/geographic-mobility/state-to-state-migration.html)
transmat <- read.csv("statemigtransmat.csv", sep=",", row.names=1) %>%
            as.matrix %>%
            `[`(,1:51)

# how many cross-state moves total?
totmoves <- sum(transmat)

# now compute number of cross-state moves within each of my 15 aggregate locations
vec1 <- c("Ohio","Indiana","Michigan","Wisconsin")
within1 <- sum(transmat[vec1,vec1])

# NC/SC/GA
vec2a <- c("North Carolina","South Carolina","Georgia")
vec2b <- c("North.Carolina","South.Carolina","Georgia")
within2 <- sum(transmat[vec2a,vec2b])

# Mountain Census Division
vec3a <- c("Utah","Nevada","Arizona","New Mexico","Colorado","Wyoming","Idaho","Montana")
vec3b <- c("Utah","Nevada","Arizona","New.Mexico","Colorado","Wyoming","Idaho","Montana")
within3 <- sum(transmat[vec3a,vec3b])

# NJ/PA
vec4a <- c("New Jersey","Pennsylvania")
vec4b <- c("New.Jersey","Pennsylvania")
within4 <- sum(transmat[vec4a,vec4b])

# W N Central Census Division
vec5a <- c("North Dakota","South Dakota","Minnesota","Nebraska","Iowa","Kansas","Missouri")
vec5b <- c("North.Dakota","South.Dakota","Minnesota","Nebraska","Iowa","Kansas","Missouri")
within5 <- sum(transmat[vec5a,vec5b])

# E S Central Census Division
vec6 <- c("Kentucky","Tennessee","Arkansas","Mississippi")
within6 <- sum(transmat[vec6,vec6])

# DMV
vec7a <- c("West Virginia","Delaware","District of Columbia ","Maryland","Virginia")
vec7b <- c("West.Virginia","Delaware","District.of.Columbia","Maryland","Virginia")
within7 <- sum(transmat[vec7a,vec7b])

# New England Census Division
vec8a <- c("Maine","New Hampshire","Vermont","Massachusetts","Rhode Island","Connecticut")
vec8b <- c("Maine","New.Hampshire","Vermont","Massachusetts","Rhode.Island","Connecticut")
within8 <- sum(transmat[vec8a,vec8b])

# Pacific \ {CA}
vec9 <- c("Alaska","Hawaii","Oregon","Washington")
within9 <- sum(transmat[vec9,vec9])

# OK/AR/LA
vec10 <- c("Oklahoma","Louisiana","Arkansas")
within10 <- sum(transmat[vec10,vec10])

# now subtract total inter-state, within-location moves from total number of inter-state moves
mymoves <- totmoves - within1 - within2 - within3 - within4 - within5 - within6 - within7 - within8 - within9 - within10
mymoves %>% print
totmoves %>% print

# what fraction of cross-state moves do I observe?
(mymoves/totmoves) %>% print

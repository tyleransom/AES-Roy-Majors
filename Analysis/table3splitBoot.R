# library(plyr)         # cross-tabulation
# library(caret)        # for some machine learning functions
# library(e1071)        # more machine learning functions
# library(partykit)     # for conditional inference tree function
# library(tidyverse)    # for tidy evaluation
# library(magrittr)     # for piping
# library(fixest)       # for fast fixed effects estimation
# library(car)          # for join hypothesis tests
# library(future)       # for parallelization
# library(furrr)        # for parallelization
# 
#load("afterT2.rda")
#load("../Data/acs.rda")

df <- NULL
load("../Data/acs19.rda")
df %<>% mutate(spbornHere     = as.factor(as.logical(spbornHere)),
               liveWithFamily = as.factor(as.logical(liveWithFamily)),
               spwork         = as.factor(as.logical(spwork)),
               married        = as.factor(as.logical(married)),
               childlt5       = as.factor(as.logical(childlt5)),
               childlt18      = as.factor(as.logical(childlt18))
              )

#-----------------------------------------------
# function to estimate tree model
#-----------------------------------------------
treefun <- function(ddff) {

    # Randomly spilt the data set into training (j=1) and test (j=0) sets
    ddff$d <- runif(dim(ddff)[1],0,1)
    ddff$d[ddff$d>=0.20]                <- 1
#    ddff$d[ddff$d<=0.35 & ddff$d >=0.2] <- 2
    ddff$d[ddff$d<0.20]                 <- 0 

    # Define Categorical Variables
    ddff$stayer1 <- (ddff$birthloc==ddff$loc1)
    J            <- nlevels(ddff$loc1occ)
    for (j in 1:(J/2)) {
        ddff[,c(paste0("spbornHere",j))] <- factor(ifelse(ddff[,c(paste0("spbornHere",j))]==1,"spbornHere","spbornElsewhere"))
        ddff[,c(paste0("bornHere",j))]   <- factor(ifelse(ddff[,c(paste0("bornHere",j))]  ==1,"bornHere"  ,"bornElsewhere"))
        ddff[,c(paste0("bornDiv",j))]    <- factor(ifelse(ddff[,c(paste0("bornDiv",j))]   ==1,"bornDiv"   ,"bornNonDiv"))
        ddff[,c(paste0("bornReg",j))]    <- factor(ifelse(ddff[,c(paste0("bornReg",j))]   ==1,"bornReg"   ,"bornNonReg"))
    }
    head(ddff)

    #::::::::::::::::::::::::::::::::::::::::::
    # Main Estimation
    #::::::::::::::::::::::::::::::::::::::::::
    # Initialize variables
    ddff <- cbind(ddff,matrix(0,dim(ddff)[1],J))
    colnames(ddff)[(length(ddff) - J + 1):length(ddff)]<- paste0("pTree", seq(1,J))
    ddff <- cbind(ddff,matrix(1,dim(ddff)[1],J))
    colnames(ddff)[(length(ddff) - J + 1):length(ddff)]<- paste0("clusterID", seq(1,J))
    ddff$clusterID <- matrix(0,dim(ddff)[1],1)

    # Create training and test sets
    train <- ddff %>% filter(d==1) #%>% as_tibble
    tset  <- ddff %>% filter(d==0) #%>% as_tibble

    #print(dim(train))
    #print(dim(tset))
    #print(dim(ddff))

    for (j in 1:J) {
        train$tempdv <- factor(ifelse(train$loc1occ==levels(train$loc1occ)[j],levels(train$loc1occ)[j],"Else"))
        #print(levels(train$tempdv))
        if (j<=(J/2)) {
            train$tempSpBornHere <- train[,c(paste0("spbornHere",j))]
            train$tempBornHere   <- train[,c(paste0("bornHere",j))]
            train$tempBornDiv    <- train[,c(paste0("bornDiv",j))]
            train$tempBornReg    <- train[,c(paste0("bornReg",j))]
            tset$tempSpBornHere  <- tset[,c(paste0("spbornHere",j))]
            tset$tempBornHere    <- tset[,c(paste0("bornHere",j))]
            tset$tempBornDiv     <- tset[,c(paste0("bornDiv",j))]
            tset$tempBornReg     <- tset[,c(paste0("bornReg",j))]
            ddff$tempSpBornHere  <- ddff[,c(paste0("spbornHere",j))]
            ddff$tempBornHere    <- ddff[,c(paste0("bornHere",j))]
            ddff$tempBornDiv     <- ddff[,c(paste0("bornDiv",j))]
            ddff$tempBornReg     <- ddff[,c(paste0("bornReg",j))]
        } else {
            train$tempSpBornHere <- train[,c(paste0("spbornHere",j-(J/2)))]
            train$tempBornHere   <- train[,c(paste0("bornHere",j-(J/2)))]
            train$tempBornDiv    <- train[,c(paste0("bornDiv",j-(J/2)))]
            train$tempBornReg    <- train[,c(paste0("bornReg",j-(J/2)))]
            tset$tempSpBornHere  <- tset[,c(paste0("spbornHere",j-(J/2)))]
            tset$tempBornHere    <- tset[,c(paste0("bornHere",j-(J/2)))]
            tset$tempBornDiv     <- tset[,c(paste0("bornDiv",j-(J/2)))]
            tset$tempBornReg     <- tset[,c(paste0("bornReg",j-(J/2)))]
            ddff$tempSpBornHere  <- ddff[,c(paste0("spbornHere",j-(J/2)))]
            ddff$tempBornHere    <- ddff[,c(paste0("bornHere",j-(J/2)))]
            ddff$tempBornDiv     <- ddff[,c(paste0("bornDiv",j-(J/2)))]
            ddff$tempBornReg     <- ddff[,c(paste0("bornReg",j-(J/2)))]
        }
        #print(sum(train$loc1occ==levels(train$loc1occ)[j]))
                                                                           
        # perform the tree classification algorithm
        model.ctree <- ctree(tempdv~tempBornHere+tempSpBornHere+tempBornDiv+tempBornReg+educvar+as.factor(advdegree)+married+racer+liveWithFamily+spwork+childlt5+childlt18+age+residMeanStay+residMeanRelOcc,data=train,control=ctree_control(minbucket=500,minsplit=500,testtype="Bonferroni",mincriterion=0.99))
        # predict class probabilities in training set
        preddy <- predict(model.ctree,newdata=train,type="prob")
        train[, c(paste0("pTree",j))] <- as.matrix(preddy[,c(levels(train$loc1occ)[j])])

        # predict classes in ddff set (for estimation later)
        preddyCat <- predict(model.ctree,newdata=ddff,type="prob")
        ddff[, c(paste0("pTree",j))] <- as.matrix(preddyCat[,c(levels(ddff$loc1occ)[j])])
        # return node IDs in ddff set
        preddy <- predict(model.ctree,newdata=ddff,type="node")
        tempvar <- 100000*j+preddy
        ddff[,c(paste0("clusterID",j))] <- tempvar
        #print(paste("Number of clusters for DV ",j,": ",max(preddy),sep=""))
        #print(plyr::count(tempvar))

        # predict classes in test (for accuracy measurement)
        preddyCat <- predict(model.ctree,newdata=tset,type="prob")
        tset[, c(paste0("pTree",j))] <- as.matrix(preddyCat[,c(levels(tset$loc1occ)[j])])
    }

    #print(head(ddff[,c("loc1occ",paste0("pTree",seq(1,J)))]))

    # Assign cluster to first-best location
    for (j in 1:J) {
        ddff$clusterID[ddff$loc1occ==levels(ddff$loc1occ)[j],1]<-ddff[ddff$loc1occ==levels(ddff$loc1occ)[j],c(paste0("clusterID",j))]
    }

    # Normalize probabilities to sum to 1:
    ddff$denom     <- rowSums(ddff[    ,c(paste0("pTree", seq(1,J)))], dims=1)
    train$denom   <- rowSums(train[  ,c(paste0("pTree", seq(1,J)))], dims=1)
    tset$denom    <- rowSums(tset[   ,c(paste0("pTree", seq(1,J)))], dims=1)
    ddff[    ,c(paste0("pTree", seq(1,J)),"denom")] <- ddff[    ,c(paste0("pTree", seq(1,J)),"denom")]/ddff$denom
    train[  ,c(paste0("pTree", seq(1,J)),"denom")] <- train[  ,c(paste0("pTree", seq(1,J)),"denom")]/train$denom
    tset[   ,c(paste0("pTree", seq(1,J)),"denom")] <- tset[   ,c(paste0("pTree", seq(1,J)),"denom")]/tset$denom

    head(ddff[    ,c(paste0("pTree", seq(1,J)),"denom")])

    # Create predicted location for classification performance measurement
    train$predloc1occ   <- apply(train[  ,c(paste0("pTree", seq(1,J)))], 1, which.max)
    tset$predloc1occ    <- apply(tset[   ,c(paste0("pTree", seq(1,J)))], 1, which.max)

    ddffps <- ddff[,c(paste0("pTree", seq(1,J)))]

    # Fix labeling on predicted classification
    train %<>% mutate(predloc1occ = as.factor(predloc1occ))
    train %<>% mutate(predloc1occ = fct_recode(predloc1occ, "California Related" = "1", "Texas Related" = "2", "Florida Related" = "3", "Illinois Related" = "4", "NewYork Related" = "5", "NewEngland Related" = "6", "NJ/PA Related" = "7", "DMV Related" = "8", "NC/SC/GA Related" = "9", "ESCentral Related" = "10", "OH/IN/MI/WI Related" = "11", "WNCentral Related" = "12", "OK/AR/LA Related" = "13", "Mountain Related" = "14", "Pacific Related" = "15", "California Unrelated" = "16", "Texas Unrelated" = "17", "Florida Unrelated" = "18", "Illinois Unrelated" = "19", "NewYork Unrelated" = "20", "NewEngland Unrelated" = "21", "NJ/PA Unrelated" = "22", "DMV Unrelated" = "23", "NC/SC/GA Unrelated" = "24", "ESCentral Unrelated" = "25", "OH/IN/MI/WI Unrelated" = "26", "WNCentral Unrelated" = "27", "OK/AR/LA Unrelated" = "28", "Mountain Unrelated" = "29", "Pacific Unrelated" = "30"))
    tset %<>% mutate(predloc1occ = as.factor(predloc1occ),
                     predloc1occ = fct_recode(predloc1occ, "California Related" = "1", "Texas Related" = "2", "Florida Related" = "3", "Illinois Related" = "4", "NewYork Related" = "5", "NewEngland Related" = "6", "NJ/PA Related" = "7", "DMV Related" = "8", "NC/SC/GA Related" = "9", "ESCentral Related" = "10", "OH/IN/MI/WI Related" = "11", "WNCentral Related" = "12", "OK/AR/LA Related" = "13", "Mountain Related" = "14", "Pacific Related" = "15", "California Unrelated" = "16", "Texas Unrelated" = "17", "Florida Unrelated" = "18", "Illinois Unrelated" = "19", "NewYork Unrelated" = "20", "NewEngland Unrelated" = "21", "NJ/PA Unrelated" = "22", "DMV Unrelated" = "23", "NC/SC/GA Unrelated" = "24", "ESCentral Unrelated" = "25", "OH/IN/MI/WI Unrelated" = "26", "WNCentral Unrelated" = "27", "OK/AR/LA Unrelated" = "28", "Mountain Unrelated" = "29", "Pacific Unrelated" = "30"))

    # Calculate prediction accuracy in training set (train) [kappa, etc.]
    #print(class(train$predloc1occ))
    #print(class(train$loc1occ))
    #print(plyr::count(train, 'loc1occ'))
    #print(plyr::count(train, 'predloc1occ'))
    tester <- plyr::count(train, 'predloc1occ')
    train$predloc1occ <- factor(train$predloc1occ,labels=levels(train$loc1occ)[tester[,1]])
    #table(train$loc1occ) %>% print
    #table(train$predloc1occ) %>% print

    #print(caret::confusionMatrix(train$predloc1occ,train$loc1occ)$overall[1:2])
    #print(head(as.matrix(cbind(train$predloc1occ,train$loc1occ))))
    #print(tail(as.matrix(cbind(train$predloc1occ,train$loc1occ))))
    #print(dim(as.matrix(cbind(train$predloc1occ,train$loc1occ))))

    # Calculate prediction accuracy in test set (test)
    #print(class(tset$predloc1occ))
    #print(class(tset$loc1occ))
    #print(plyr::count(tset, 'loc1occ'))
    #print(plyr::count(tset, 'predloc1occ'))
    tester <- plyr::count(tset, 'predloc1occ')
    tset$predloc1occ <- factor(tset$predloc1occ,labels=levels(tset$loc1occ)[tester[,1]])
    #print(caret::confusionMatrix(tset$predloc1occ,tset$loc1occ)$overall[1:2])
    #print(head(as.matrix(cbind(tset$predloc1occ,tset$loc1occ))))
    #print(tail(as.matrix(cbind(tset$predloc1occ,tset$loc1occ))))
    #print(dim(as.matrix(cbind(tset$predloc1occ,tset$loc1occ))))

    # create first-best, second-best, and retention probabilities:
    maxn <- function(n) function(x) order(x, decreasing = TRUE)[n]
    ddff$tree.prob.first.best  <- matrix(data=0,nrow=dim(ddff)[1],ncol=1)
    ddff$tree.prob.occ         <- matrix(data=0,nrow=dim(ddff)[1],ncol=1)
    ddff$tree.prob.loc         <- matrix(data=0,nrow=dim(ddff)[1],ncol=1)
    ddff$tree.prob.second.best <- matrix(data=0,nrow=dim(ddff)[1],ncol=1)
    ddff$tree.prob.third.best  <- matrix(data=0,nrow=dim(ddff)[1],ncol=1)
    ddff$tree.prob.fourth.best <- matrix(data=0,nrow=dim(ddff)[1],ncol=1)
    ddff$tree.prob.retention   <- matrix(data=0,nrow=dim(ddff)[1],ncol=1)
    ddff$tree.prob.retention[,1] <- ddffps[cbind(1:nrow(ddffps), as.numeric(ddff$birthloc))]+ddffps[cbind(1:nrow(ddffps), as.numeric(ddff$birthloc)+J/2)]
    for (j in 1:J) {
        ddff$tree.prob.first.best[as.numeric(ddff$loc1occ)==j,1] <- ddffps[as.numeric(ddff$loc1occ)==j,j]
        if (j<=J/2) {
            ddff$tree.prob.loc[as.numeric(ddff$loc1occ)==j,1] <- ddffps[as.numeric(ddff$loc1occ)==j,j+J/2]
            tempy1 <- ddffps[as.numeric(ddff$loc1occ)==j,-((J/2)+1):-J]
            ddff$tree.prob.occ[as.numeric(ddff$loc1occ)==j,1] <- rowSums(tempy1, dims=1)-ddff$tree.prob.first.best[as.numeric(ddff$loc1occ)==j,1]
        } else {
            ddff$tree.prob.loc[as.numeric(ddff$loc1occ)==j,1] <- ddffps[as.numeric(ddff$loc1occ)==j,j-J/2]
            tempy1 <- ddffps[as.numeric(ddff$loc1occ)==j,-1:-(J/2)]
            ddff$tree.prob.occ[as.numeric(ddff$loc1occ)==j,1] <- rowSums(tempy1, dims=1)-ddff$tree.prob.first.best[as.numeric(ddff$loc1occ)==j,1]
        }
        tempy <- ddffps[as.numeric(ddff$loc1occ)==j,-j]
        ddff$tree.prob.second.best[as.numeric(ddff$loc1occ)==j,1] <- apply(tempy,1,max)
        ddff$tree.prob.third.best[as.numeric(ddff$loc1occ)==j,1] <- apply(tempy,1,function(x)x[maxn(2)(x)])
        ddff$tree.prob.fourth.best[as.numeric(ddff$loc1occ)==j,1] <- apply(tempy,1,function(x)x[maxn(3)(x)])
    }
    ddff %<>% select(-starts_with("temp"))
}

#-----------------------------------------------
# function to estimate log earnings regressions: 
# (uncorrected and corrected)
#-----------------------------------------------
regfun <- function(ddff) {
    uncr.est <- feols(loginc ~ (educvar + poly(exper, degree=3, raw=TRUE))*advdegree + racer + married + inBirthSt                                                                | met2013, data=ddff, weights=ddff$perwt)
    corr.est <- feols(loginc ~ (educvar + poly(exper, degree=3, raw=TRUE))*advdegree + racer + married + inBirthSt + poly(tree.prob.first.best,tree.prob.loc, degree=3, raw=TRUE) | met2013, data=ddff, weights=ddff$perwt) 
    uncr.adv <- uncr.est %>% `$`(coeftable) %>% `$`(Estimate) %>% `[`( which(rownames(uncr.est$coeftable)=="advdegreeTRUE") )
    corr.adv <- corr.est %>% `$`(coeftable) %>% `$`(Estimate) %>% `[`( which(rownames(corr.est$coeftable)=="advdegreeTRUE") )
    #fstat <- linearHypothesis(corr.est, matchCoefs(corr.est,"prob"))
    numdf <- length(matchCoefs(corr.est,"prob"))
    demdf <- uncr.est %>% `$`(nobs) - ( uncr.est %>% `$`(coeftable) %>% dim %>% `[`(1) ) 
    fstat <- ((uncr.est$ssr-corr.est$ssr)/numdf)/(corr.est$ssr/demdf)
    fcrit <- qf(.05, numdf, demdf, lower.tail=TRUE)
    outs <- tibble(
                   uncr.name = c("educvarEducation"                   , 
                                 "educvarSocial Science"              ,
                                 "educvarOther"                       ,
                                 "educvarBusiness"                    ,
                                 "educvarSTEM"                        ,
                                 "advdegreeTRUE"                      ,
                                 "educvarSocial Science:advdegreeTRUE",
                                 "educvarOther:advdegreeTRUE"         ,
                                 "educvarBusiness:advdegreeTRUE"      ,
                                 "educvarSTEM:advdegreeTRUE"          ,
                                 "inBirthStTRUE"           
                               ),
                   uncr.coef = c(0, 
                                 uncr.est %>% `$`(coeftable) %>% `$`(Estimate) %>% `[`( which(rownames(uncr.est$coeftable)=="educvarSocial Science"              ) ),
                                 uncr.est %>% `$`(coeftable) %>% `$`(Estimate) %>% `[`( which(rownames(uncr.est$coeftable)=="educvarOther"                       ) ),
                                 uncr.est %>% `$`(coeftable) %>% `$`(Estimate) %>% `[`( which(rownames(uncr.est$coeftable)=="educvarBusiness"                    ) ),
                                 uncr.est %>% `$`(coeftable) %>% `$`(Estimate) %>% `[`( which(rownames(uncr.est$coeftable)=="educvarSTEM"                        ) ),
                                 uncr.est %>% `$`(coeftable) %>% `$`(Estimate) %>% `[`( which(rownames(uncr.est$coeftable)=="advdegreeTRUE"                      ) ), 
                                 uncr.est %>% `$`(coeftable) %>% `$`(Estimate) %>% `[`( which(rownames(uncr.est$coeftable)=="educvarSocial Science:advdegreeTRUE") )+uncr.adv,
                                 uncr.est %>% `$`(coeftable) %>% `$`(Estimate) %>% `[`( which(rownames(uncr.est$coeftable)=="educvarOther:advdegreeTRUE"         ) )+uncr.adv,
                                 uncr.est %>% `$`(coeftable) %>% `$`(Estimate) %>% `[`( which(rownames(uncr.est$coeftable)=="educvarBusiness:advdegreeTRUE"      ) )+uncr.adv,
                                 uncr.est %>% `$`(coeftable) %>% `$`(Estimate) %>% `[`( which(rownames(uncr.est$coeftable)=="educvarSTEM:advdegreeTRUE"          ) )+uncr.adv,
                                 uncr.est %>% `$`(coeftable) %>% `$`(Estimate) %>% `[`( which(rownames(uncr.est$coeftable)=="inBirthStTRUE"                      ) )
                               ),
                   corr.name = c("educvarEducation"                   , 
                                 "educvarSocial Science"              ,
                                 "educvarOther"                       ,
                                 "educvarBusiness"                    ,
                                 "educvarSTEM"                        ,
                                 "advdegreeTRUE"                      ,
                                 "educvarSocial Science:advdegreeTRUE",
                                 "educvarOther:advdegreeTRUE"         ,
                                 "educvarBusiness:advdegreeTRUE"      ,
                                 "educvarSTEM:advdegreeTRUE"          ,
                                 "inBirthStTRUE"           
                               ),
                   corr.coef = c(0, 
                                 corr.est %>% `$`(coeftable) %>% `$`(Estimate) %>% `[`( which(rownames(corr.est$coeftable)=="educvarSocial Science"              ) ),
                                 corr.est %>% `$`(coeftable) %>% `$`(Estimate) %>% `[`( which(rownames(corr.est$coeftable)=="educvarOther"                       ) ),
                                 corr.est %>% `$`(coeftable) %>% `$`(Estimate) %>% `[`( which(rownames(corr.est$coeftable)=="educvarBusiness"                    ) ),
                                 corr.est %>% `$`(coeftable) %>% `$`(Estimate) %>% `[`( which(rownames(corr.est$coeftable)=="educvarSTEM"                        ) ),
                                 corr.est %>% `$`(coeftable) %>% `$`(Estimate) %>% `[`( which(rownames(corr.est$coeftable)=="advdegreeTRUE"                      ) ), 
                                 corr.est %>% `$`(coeftable) %>% `$`(Estimate) %>% `[`( which(rownames(corr.est$coeftable)=="educvarSocial Science:advdegreeTRUE") )+corr.adv,
                                 corr.est %>% `$`(coeftable) %>% `$`(Estimate) %>% `[`( which(rownames(corr.est$coeftable)=="educvarOther:advdegreeTRUE"         ) )+corr.adv,
                                 corr.est %>% `$`(coeftable) %>% `$`(Estimate) %>% `[`( which(rownames(corr.est$coeftable)=="educvarBusiness:advdegreeTRUE"      ) )+corr.adv,
                                 corr.est %>% `$`(coeftable) %>% `$`(Estimate) %>% `[`( which(rownames(corr.est$coeftable)=="educvarSTEM:advdegreeTRUE"          ) )+corr.adv,
                                 corr.est %>% `$`(coeftable) %>% `$`(Estimate) %>% `[`( which(rownames(corr.est$coeftable)=="inBirthStTRUE"                      ) )
                               ),
                   uncr.r2   = rep(uncr.est %>% `$`(sq.cor), 11),
                   corr.r2   = rep(corr.est %>% `$`(sq.cor), 11),
                   uncr.nobs = rep(uncr.est %>% `$`(nobs)  , 11),
                   corr.nobs = rep(corr.est %>% `$`(nobs)  , 11),
                   corr.f    = rep(fstat                   , 11),
                   fcritical = rep(fcrit                   , 11)
                  ) %>% mutate(corr.name = NULL) #%>% rename(varname = uncr.name)
}

#-----------------------------------------------
# function that does the bootstrap sampling
# and calls each stage of the estimation
#-----------------------------------------------
bootwrap <- function(N,tdf) {
    # sample the data with replacement
    dff <- tdf[sample(1:N, N, replace = T), ]
    # estimate the tree models in serial
    fdf <- dff %>% treefun
    fdf %<>% select(perwt,loc1occ,loginc,educvar,
                    exper,advdegree,racer,married,
                    inBirthSt,met2013,
                    tree.prob.first.best,tree.prob.loc)
    # estimate the log wage models in serial
    out <- fdf %>%
           split(.$loc1occ) %>%
           map_dfr(~ regfun(.x), .id = "loc.occ")
}

#-----------------------------------------------
# set up bootstrap parameters and execute
#-----------------------------------------------
df.sv <- df %>% mutate(inBirthSt = as.factor(inBirthSt)) %>%
                select(
                       # variables needed for wage regressions
                       perwt,loc1occ,loginc,educvar,exper,
                       advdegree,racer,married,inBirthSt,met2013,
                       # variables needed for tree model
                       birthloc,loc1,age,liveWithFamily,
                       spwork,childlt5,childlt18,
                       residMeanStay,residMeanRelOcc,
                       starts_with("bornHere"),
                       starts_with("spbornHere"),
                       starts_with("bornDiv"),
                       starts_with("bornReg")
                      )

# takes about 6 hours for 100 replicates
plan(multisession, workers = 2)

# do the tree and wage estimation once (no bootstrapping)
set.seed(12345)
fdf <- df.sv %>% treefun

wagest <- fdf %>%
          split(.$loc1occ) %>%
          future_map_dfr(~ regfun(.x), .id = "loc.occ")
save(fdf,wagest, file="wageresults.rda")

# now do the bootstrap
N  <- dim(df.sv)[1]
B  <- 500
set.seed(12345)
iterator = rep(N,B)
boot_out <- future_map_dfr(
                           # Repeat the sample size B times
                           iterator,
                           # Our function
                           ~bootwrap(N,df.sv),
                           # Let furrr know we want to set a seed
                           .options = furrr_options(seed = T),
                           # Save results as new column
                           .id      = "bootnum"
                          ) %>% as_tibble
 
save(boot_out, file=paste0("bootoutput",B,".rda"))


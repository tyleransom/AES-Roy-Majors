#-----------------------------------------------
# function to estimate probabilities by logit
#-----------------------------------------------
logitfun <- function(ddff) {

    # Randomly spilt the data set into training (j=1) and test (j=0) sets
    ddff$d <- runif(dim(ddff)[1],0,1)
    ddff$d[ddff$d>=0.20]                <- 1
    #ddff$d[ddff$d<=0.35 & ddff$d >=0.2] <- 2
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
    colnames(ddff)[(length(ddff) - J + 1):length(ddff)]<- paste0("pLogit", seq(1,J))
    ddff <- cbind(ddff,matrix(1,dim(ddff)[1],J))
    colnames(ddff)[(length(ddff) - J + 1):length(ddff)]<- paste0("clusterID", seq(1,J))
    ddff$clusterID <- matrix(0,dim(ddff)[1],1)

    # Create training and test sets
    train <- ddff %>% filter(d==1) #%>% as_tibble
    tset  <- ddff %>% filter(d==0) #%>% as_tibble

    for (j in 1:J) {
        train$tempdv <- ifelse(train$loc1occ==levels(train$loc1occ)[j],TRUE,FALSE)
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
                                                                           
        # perform the logit classification algorithm
        model.logit <- glm(tempdv~tempBornHere+tempSpBornHere+tempBornReg+educvar*as.factor(advdegree)+married+racer+liveWithFamily+spwork+childlt5+childlt18+married:childlt5+married:childlt18+married:spwork+educvar:racer+educvar:married+educvar:liveWithFamily+poly(age,degree=3,raw=TRUE)+poly(residMeanStay,degree=2,raw=TRUE)+poly(residMeanRelOcc,degree=2,raw=TRUE)+residMeanStay:residMeanRelOcc,data=train,family = "binomial") 
        # predict class probabilities in training set
        preddy <- predict(model.logit,newdata=train,type="response")
        train[, c(paste0("pLogit",j))] <- as.matrix(preddy)
        # predict classes in test (for accuracy measurement)
        preddy <- predict(model.logit,newdata=tset,type="response")
        tset[, c(paste0("pLogit",j))] <- as.matrix(preddy)
        # predict classes in ddff
        preddy <- predict(model.logit,newdata=ddff,type="response")
        ddff[, c(paste0("pLogit",j))] <- as.matrix(preddy)
    }

    # Normalize probabilities to sum to 1:
    ddff$denom     <- rowSums(ddff[    ,c(paste0("pLogit", seq(1,J)))], dims=1)
    train$denom   <- rowSums(train[  ,c(paste0("pLogit", seq(1,J)))], dims=1)
    tset$denom    <- rowSums(tset[   ,c(paste0("pLogit", seq(1,J)))], dims=1)
    ddff[    ,c(paste0("pLogit", seq(1,J)),"denom")] <- ddff[    ,c(paste0("pLogit", seq(1,J)),"denom")]/ddff$denom
    train[  ,c(paste0("pLogit", seq(1,J)),"denom")] <- train[  ,c(paste0("pLogit", seq(1,J)),"denom")]/train$denom
    tset[   ,c(paste0("pLogit", seq(1,J)),"denom")] <- tset[   ,c(paste0("pLogit", seq(1,J)),"denom")]/tset$denom

    # Create predicted location for classification performance measurement
    train$predloc1occ   <- apply(train[  ,c(paste0("pLogit", seq(1,J)))], 1, which.max)
    tset$predloc1occ    <- apply(tset[   ,c(paste0("pLogit", seq(1,J)))], 1, which.max)

    ddffps <- ddff[,c(paste0("pLogit", seq(1,J)))]

    # Fix labeling on predicted classification
    train %<>% mutate(predloc1occ = as.factor(predloc1occ))
    train %<>% mutate(predloc1occ = fct_recode(predloc1occ, "California Related" = "1", "Texas Related" = "2", "Florida Related" = "3", "Illinois Related" = "4", "NewYork Related" = "5", "NewEngland Related" = "6", "NJ/PA Related" = "7", "DMV Related" = "8", "NC/SC/GA Related" = "9", "ESCentral Related" = "10", "OH/IN/MI/WI Related" = "11", "WNCentral Related" = "12", "OK/AR/LA Related" = "13", "Mountain Related" = "14", "Pacific Related" = "15", "California Unrelated" = "16", "Texas Unrelated" = "17", "Florida Unrelated" = "18", "Illinois Unrelated" = "19", "NewYork Unrelated" = "20", "NewEngland Unrelated" = "21", "NJ/PA Unrelated" = "22", "DMV Unrelated" = "23", "NC/SC/GA Unrelated" = "24", "ESCentral Unrelated" = "25", "OH/IN/MI/WI Unrelated" = "26", "WNCentral Unrelated" = "27", "OK/AR/LA Unrelated" = "28", "Mountain Unrelated" = "29", "Pacific Unrelated" = "30"))
    tset %<>% mutate(predloc1occ = as.factor(predloc1occ),
                     predloc1occ = fct_recode(predloc1occ, "California Related" = "1", "Texas Related" = "2", "Florida Related" = "3", "Illinois Related" = "4", "NewYork Related" = "5", "NewEngland Related" = "6", "NJ/PA Related" = "7", "DMV Related" = "8", "NC/SC/GA Related" = "9", "ESCentral Related" = "10", "OH/IN/MI/WI Related" = "11", "WNCentral Related" = "12", "OK/AR/LA Related" = "13", "Mountain Related" = "14", "Pacific Related" = "15", "California Unrelated" = "16", "Texas Unrelated" = "17", "Florida Unrelated" = "18", "Illinois Unrelated" = "19", "NewYork Unrelated" = "20", "NewEngland Unrelated" = "21", "NJ/PA Unrelated" = "22", "DMV Unrelated" = "23", "NC/SC/GA Unrelated" = "24", "ESCentral Unrelated" = "25", "OH/IN/MI/WI Unrelated" = "26", "WNCentral Unrelated" = "27", "OK/AR/LA Unrelated" = "28", "Mountain Unrelated" = "29", "Pacific Unrelated" = "30"))

    # Calculate prediction accuracy in training set (train) [kappa, etc.]
    tester <- plyr::count(train, 'predloc1occ')
    train$predloc1occ <- factor(train$predloc1occ,labels=levels(train$loc1occ)[tester[,1]])
    print("Logit accuracy in training set")
    print(caret::confusionMatrix(train$predloc1occ,train$loc1occ)$overall[1:2])
    print(head(as.matrix(cbind(train$predloc1occ,train$loc1occ))))
    print(tail(as.matrix(cbind(train$predloc1occ,train$loc1occ))))
    print(dim(as.matrix(cbind(train$predloc1occ,train$loc1occ))))

    # Calculate prediction accuracy in test set (test)
    tester <- plyr::count(tset, 'predloc1occ')
    tset$predloc1occ <- factor(tset$predloc1occ,labels=levels(tset$loc1occ)[tester[,1]])
    print("Logit accuracy in test set")
    print(caret::confusionMatrix(tset$predloc1occ,tset$loc1occ)$overall[1:2])
    print(head(as.matrix(cbind(tset$predloc1occ,tset$loc1occ))))
    print(tail(as.matrix(cbind(tset$predloc1occ,tset$loc1occ))))
    print(dim(as.matrix(cbind(tset$predloc1occ,tset$loc1occ))))

    # create first-best, second-best, and retention probabilities:
    maxn <- function(n) function(x) order(x, decreasing = TRUE)[n]
    ddff$logit.prob.first.best  <- matrix(data=0,nrow=dim(ddff)[1],ncol=1)
    ddff$logit.prob.occ         <- matrix(data=0,nrow=dim(ddff)[1],ncol=1)
    ddff$logit.prob.loc         <- matrix(data=0,nrow=dim(ddff)[1],ncol=1)
    ddff$logit.prob.second.best <- matrix(data=0,nrow=dim(ddff)[1],ncol=1)
    ddff$logit.prob.third.best  <- matrix(data=0,nrow=dim(ddff)[1],ncol=1)
    ddff$logit.prob.fourth.best <- matrix(data=0,nrow=dim(ddff)[1],ncol=1)
    ddff$logit.prob.retention   <- matrix(data=0,nrow=dim(ddff)[1],ncol=1)
    ddff$logit.prob.retention[,1] <- ddffps[cbind(1:nrow(ddffps), as.numeric(ddff$birthloc))]+ddffps[cbind(1:nrow(ddffps), as.numeric(ddff$birthloc)+J/2)]
    for (j in 1:J) {
        ddff$logit.prob.first.best[as.numeric(ddff$loc1occ)==j,1] <- ddffps[as.numeric(ddff$loc1occ)==j,j]
        if (j<=J/2) {
            ddff$logit.prob.loc[as.numeric(ddff$loc1occ)==j,1] <- ddffps[as.numeric(ddff$loc1occ)==j,j+J/2]
            tempy1 <- ddffps[as.numeric(ddff$loc1occ)==j,-((J/2)+1):-J]
            ddff$logit.prob.occ[as.numeric(ddff$loc1occ)==j,1] <- rowSums(tempy1, dims=1)-ddff$logit.prob.first.best[as.numeric(ddff$loc1occ)==j,1]
        } else {
            ddff$logit.prob.loc[as.numeric(ddff$loc1occ)==j,1] <- ddffps[as.numeric(ddff$loc1occ)==j,j-J/2]
            tempy1 <- ddffps[as.numeric(ddff$loc1occ)==j,-1:-(J/2)]
            ddff$logit.prob.occ[as.numeric(ddff$loc1occ)==j,1] <- rowSums(tempy1, dims=1)-ddff$logit.prob.first.best[as.numeric(ddff$loc1occ)==j,1]
        }
        tempy <- ddffps[as.numeric(ddff$loc1occ)==j,-j]
        ddff$logit.prob.second.best[as.numeric(ddff$loc1occ)==j,1] <- apply(tempy,1,max)
        ddff$logit.prob.third.best[as.numeric(ddff$loc1occ)==j,1] <- apply(tempy,1,function(x)x[maxn(2)(x)])
        ddff$logit.prob.fourth.best[as.numeric(ddff$loc1occ)==j,1] <- apply(tempy,1,function(x)x[maxn(3)(x)])
    }
    ddff %<>% select(-starts_with("temp"))
}

#-----------------------------------------------
# function to estimate bin probabilities
#-----------------------------------------------
binfun <- function(ddff) {

    # Randomly spilt the data set into training (j=1) and test (j=0) sets
    ddff$d <- runif(dim(ddff)[1],0,1)
    ddff$d[ddff$d>=0.20]                <- 1
    #ddff$d[ddff$d<=0.35 & ddff$d >=0.2] <- 2
    ddff$d[ddff$d<0.20]                 <- 0 

    # Define Categorical Variables
    ddff$age                  <- as.numeric(ddff$age)
    ddff$ageQuant             <- gtools::quantcut(ddff$age, q = seq(0,1,by=0.5))
    ddff$residMeanStayQuant   <- gtools::quantcut(ddff$residMeanStay, q = seq(0,1,by=0.2))
    ddff$residMeanRelOccQuant <- gtools::quantcut(ddff$residMeanRelOcc, q = seq(0,1,by=0.2))
    ddff$ageFac               <- as.factor(ddff$age)
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
    colnames(ddff)[(length(ddff) - J + 1):length(ddff)]<- paste0("pBin", seq(1,J))
    ddff <- cbind(ddff,matrix(1,dim(ddff)[1],J))
    colnames(ddff)[(length(ddff) - J + 1):length(ddff)]<- paste0("clusterID", seq(1,J))
    ddff$clusterID <- matrix(0,dim(ddff)[1],1)

    # Create training and test sets
    train <- ddff %>% filter(d==1) #%>% as_tibble
    tset  <- ddff %>% filter(d==0) #%>% as_tibble

    for (j in 1:J) {
        train$tempdv <- (train$loc1occ==levels(train$loc1occ)[j])
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

        # collapse to take averages
        collapseN <- doBy::summaryBy(tempdv~tempBornHere+educvar+married+residMeanRelOccQuant+residMeanStayQuant,FUN=c(length),data=train)
        collapse1 <- doBy::summaryBy(tempdv~tempBornHere+educvar+married+residMeanRelOccQuant+residMeanStayQuant,data=train)
        collapse1 <- cbind(collapse1,collapseN[,length(collapseN)])
        colnames(collapse1)[length(collapse1)] <- "clusterSize"
        collapse1$clusterID <- 1000000+seq(1, dim(collapse1)[1], 1)
                                                                           
        # perform the bin classification (i.e. merge in the bin probabilities)
        preddy <- join(ddff, collapse1, by=c("tempBornHere","educvar","married","residMeanStayQuant","residMeanRelOccQuant"), type='left', match='all')
        preddy1<- preddy
        preddy <- preddy[,(length(preddy)-2)]
        ddff[ , c(paste0("pBin",j))] <- preddy
        ddff[ , c("clusterSize")] <- preddy1[,(length(preddy1)-1)]
        ddff[ , c("clusterID")]   <- preddy1[,length(preddy1)]

        # merge into training data
        preddy <- join(train, collapse1, by=c("tempBornHere","educvar","married","residMeanStayQuant","residMeanRelOccQuant"), type='left', match='all')
        preddy1<- preddy
        preddy <- preddy[,(length(preddy)-2)]
        train[ , c(paste0("pBin",j))] <- preddy
        train[ , c("clusterSize")] <- preddy1[,(length(preddy1)-1)]
        train[ , c("clusterID")]   <- preddy1[,length(preddy1)]

        # merge into test data
        preddy <- join(tset, collapse1, by=c("tempBornHere","educvar","married","residMeanStayQuant","residMeanRelOccQuant"), type='left', match='all')
        preddy1<- preddy
        preddy <- preddy[,(length(preddy)-2)]
        tset[ , c(paste0("pBin",j))] <- preddy
        tset[ , c("clusterSize")] <- preddy1[,(length(preddy1)-1)]
        tset[ , c("clusterID")]   <- preddy1[,length(preddy1)]
    }

    # set NAs in data to be close to zero
    for (j in 1:J) {
        tset[is.na(tset[, c(paste0("pBin",j))]), c(paste0("pBin",j))] <- 1e-4
        ddff[is.na(ddff[, c(paste0("pBin",j))]), c(paste0("pBin",j))] <- 1e-4
    }

    # Assign cluster to first-best location
    for (j in 1:J) {
        ddff$clusterID[ddff$loc1occ==levels(ddff$loc1occ)[j]]<-ddff[ddff$loc1occ==levels(ddff$loc1occ)[j],c(paste0("clusterID",j))]
    }

    # Normalize probabilities to sum to 1:
    ddff$denom     <- rowSums(ddff[    ,c(paste0("pBin", seq(1,J)))], dims=1)
    train$denom   <- rowSums(train[  ,c(paste0("pBin", seq(1,J)))], dims=1)
    tset$denom    <- rowSums(tset[   ,c(paste0("pBin", seq(1,J)))], dims=1)
    ddff[    ,c(paste0("pBin", seq(1,J)),"denom")] <- ddff[    ,c(paste0("pBin", seq(1,J)),"denom")]/ddff$denom
    train[  ,c(paste0("pBin", seq(1,J)),"denom")] <- train[  ,c(paste0("pBin", seq(1,J)),"denom")]/train$denom
    tset[   ,c(paste0("pBin", seq(1,J)),"denom")] <- tset[   ,c(paste0("pBin", seq(1,J)),"denom")]/tset$denom

    head(ddff[    ,c(paste0("pBin", seq(1,J)),"denom")])

    # Create predicted location for classification performance measurement
    train$predloc1occ   <- apply(train[  ,c(paste0("pBin", seq(1,J)))], 1, which.max)
    tset$predloc1occ    <- apply(tset[   ,c(paste0("pBin", seq(1,J)))], 1, which.max)

    ddffps <- ddff[,c(paste0("pBin", seq(1,J)))]

    # Fix labeling on predicted classification
    train %<>% mutate(predloc1occ = as.factor(predloc1occ))
    train %<>% mutate(predloc1occ = fct_recode(predloc1occ, "California Related" = "1", "Texas Related" = "2", "Florida Related" = "3", "Illinois Related" = "4", "NewYork Related" = "5", "NewEngland Related" = "6", "NJ/PA Related" = "7", "DMV Related" = "8", "NC/SC/GA Related" = "9", "ESCentral Related" = "10", "OH/IN/MI/WI Related" = "11", "WNCentral Related" = "12", "OK/AR/LA Related" = "13", "Mountain Related" = "14", "Pacific Related" = "15", "California Unrelated" = "16", "Texas Unrelated" = "17", "Florida Unrelated" = "18", "Illinois Unrelated" = "19", "NewYork Unrelated" = "20", "NewEngland Unrelated" = "21", "NJ/PA Unrelated" = "22", "DMV Unrelated" = "23", "NC/SC/GA Unrelated" = "24", "ESCentral Unrelated" = "25", "OH/IN/MI/WI Unrelated" = "26", "WNCentral Unrelated" = "27", "OK/AR/LA Unrelated" = "28", "Mountain Unrelated" = "29", "Pacific Unrelated" = "30"))
    tset %<>% mutate(predloc1occ = as.factor(predloc1occ),
                     predloc1occ = fct_recode(predloc1occ, "California Related" = "1", "Texas Related" = "2", "Florida Related" = "3", "Illinois Related" = "4", "NewYork Related" = "5", "NewEngland Related" = "6", "NJ/PA Related" = "7", "DMV Related" = "8", "NC/SC/GA Related" = "9", "ESCentral Related" = "10", "OH/IN/MI/WI Related" = "11", "WNCentral Related" = "12", "OK/AR/LA Related" = "13", "Mountain Related" = "14", "Pacific Related" = "15", "California Unrelated" = "16", "Texas Unrelated" = "17", "Florida Unrelated" = "18", "Illinois Unrelated" = "19", "NewYork Unrelated" = "20", "NewEngland Unrelated" = "21", "NJ/PA Unrelated" = "22", "DMV Unrelated" = "23", "NC/SC/GA Unrelated" = "24", "ESCentral Unrelated" = "25", "OH/IN/MI/WI Unrelated" = "26", "WNCentral Unrelated" = "27", "OK/AR/LA Unrelated" = "28", "Mountain Unrelated" = "29", "Pacific Unrelated" = "30"))

    # Calculate prediction accuracy in training set (train) [kappa, etc.]
    tester <- plyr::count(train, 'predloc1occ')
    train$predloc1occ <- factor(train$predloc1occ,labels=levels(train$loc1occ)[tester[,1]])
    print("Bin accuracy in training set")
    print(caret::confusionMatrix(train$predloc1occ,train$loc1occ)$overall[1:2])
    print(head(as.matrix(cbind(train$predloc1occ,train$loc1occ))))
    print(tail(as.matrix(cbind(train$predloc1occ,train$loc1occ))))
    print(dim(as.matrix(cbind(train$predloc1occ,train$loc1occ))))

    # Calculate prediction accuracy in test set (test)
    tester <- plyr::count(tset, 'predloc1occ')
    tset$predloc1occ <- factor(tset$predloc1occ,labels=levels(tset$loc1occ)[tester[,1]])
    print("Bin accuracy in test set")
    print(caret::confusionMatrix(tset$predloc1occ,tset$loc1occ)$overall[1:2])
    print(head(as.matrix(cbind(tset$predloc1occ,tset$loc1occ))))
    print(tail(as.matrix(cbind(tset$predloc1occ,tset$loc1occ))))
    print(dim(as.matrix(cbind(tset$predloc1occ,tset$loc1occ))))

    # create first-best, second-best, and retention probabilities:
    maxn <- function(n) function(x) order(x, decreasing = TRUE)[n]
    ddff$bin.prob.first.best  <- matrix(data=0,nrow=dim(ddff)[1],ncol=1)
    ddff$bin.prob.occ         <- matrix(data=0,nrow=dim(ddff)[1],ncol=1)
    ddff$bin.prob.loc         <- matrix(data=0,nrow=dim(ddff)[1],ncol=1)
    ddff$bin.prob.second.best <- matrix(data=0,nrow=dim(ddff)[1],ncol=1)
    ddff$bin.prob.third.best  <- matrix(data=0,nrow=dim(ddff)[1],ncol=1)
    ddff$bin.prob.fourth.best <- matrix(data=0,nrow=dim(ddff)[1],ncol=1)
    ddff$bin.prob.retention   <- matrix(data=0,nrow=dim(ddff)[1],ncol=1)
    ddff$bin.prob.retention[,1] <- ddffps[cbind(1:nrow(ddffps), as.numeric(ddff$birthloc))]+ddffps[cbind(1:nrow(ddffps), as.numeric(ddff$birthloc)+J/2)]
    for (j in 1:J) {
        ddff$bin.prob.first.best[as.numeric(ddff$loc1occ)==j,1] <- ddffps[as.numeric(ddff$loc1occ)==j,j]
        if (j<=J/2) {
            ddff$bin.prob.loc[as.numeric(ddff$loc1occ)==j,1] <- ddffps[as.numeric(ddff$loc1occ)==j,j+J/2]
            tempy1 <- ddffps[as.numeric(ddff$loc1occ)==j,-((J/2)+1):-J]
            ddff$bin.prob.occ[as.numeric(ddff$loc1occ)==j,1] <- rowSums(tempy1, dims=1)-ddff$bin.prob.first.best[as.numeric(ddff$loc1occ)==j,1]
        } else {
            ddff$bin.prob.loc[as.numeric(ddff$loc1occ)==j,1] <- ddffps[as.numeric(ddff$loc1occ)==j,j-J/2]
            tempy1 <- ddffps[as.numeric(ddff$loc1occ)==j,-1:-(J/2)]
            ddff$bin.prob.occ[as.numeric(ddff$loc1occ)==j,1] <- rowSums(tempy1, dims=1)-ddff$bin.prob.first.best[as.numeric(ddff$loc1occ)==j,1]
        }
        tempy <- ddffps[as.numeric(ddff$loc1occ)==j,-j]
        ddff$bin.prob.second.best[as.numeric(ddff$loc1occ)==j,1] <- apply(tempy,1,max)
        ddff$bin.prob.third.best[as.numeric(ddff$loc1occ)==j,1] <- apply(tempy,1,function(x)x[maxn(2)(x)])
        ddff$bin.prob.fourth.best[as.numeric(ddff$loc1occ)==j,1] <- apply(tempy,1,function(x)x[maxn(3)(x)])
    }
    ddff %<>% select(-starts_with("temp"))
}



# Execute the functions defined above
df <- NULL
load("../Data/acs19.rda")
df %<>% mutate(spbornHere     = as.factor(as.logical(spbornHere)),
               liveWithFamily = as.factor(as.logical(liveWithFamily)),
               spwork         = as.factor(as.logical(spwork)),
               married        = as.factor(as.logical(married)),
               childlt5       = as.factor(as.logical(childlt5)),
               childlt18      = as.factor(as.logical(childlt18))
              )
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

# do the tree and wage estimation once (no bootstrapping)
set.seed(12345)
nulll <- logitfun(df.sv)
nullb <- binfun(df.sv)

nulll %<>% filter(loc1occ=="NewYork Related")
nullb %<>% filter(loc1occ=="NewYork Related")

# new york estimates using logit and bin results (compare these with what is in Table 2 in the paper)
# logit
uncr.est <- feols(loginc ~ (educvar + poly(exper, degree=3, raw=TRUE))*advdegree + racer + married + inBirthSt                                                                  | met2013, data=nulll, weights=nulll$perwt)
corr.est <- feols(loginc ~ (educvar + poly(exper, degree=3, raw=TRUE))*advdegree + racer + married + inBirthSt + poly(logit.prob.first.best,logit.prob.loc, degree=3, raw=TRUE) | met2013, data=nulll, weights=nulll$perwt) 
uncr.est %>% summary %>% print
corr.est %>% summary %>% print

# bin
uncr.est <- feols(loginc ~ (educvar + poly(exper, degree=3, raw=TRUE))*advdegree + racer + married + inBirthSt                                                              | met2013, data=nullb, weights=nullb$perwt)
corr.est <- feols(loginc ~ (educvar + poly(exper, degree=3, raw=TRUE))*advdegree + racer + married + inBirthSt + poly(bin.prob.first.best,bin.prob.loc, degree=3, raw=TRUE) | met2013, data=nullb, weights=nullb$perwt) 
uncr.est %>% summary %>% print
corr.est %>% summary %>% print
 

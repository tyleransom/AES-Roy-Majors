###################################################
# R code for descriptive regressions and conditional
# inference tree classification algorithm
# Tyler Ransom
###################################################


#------------------------------------------
# Load Required Packages and Functions
#------------------------------------------
# source("initialize.R")

# Set Run Parameters
# If test is TRUE, this will subset the data by 20% and run the code
test <- FALSE

# for reproducibility
set.seed(12345)


#------------------------------------------
# Read and Prepare Data
#------------------------------------------
if (test == TRUE) {
  #subset the data frame to test the code
  df$sub <- runif(dim(df)[1],0,1)
  df <- subset(df,sub<=0.2)
  df <- subset(df,select=-sub)
}
today <- Sys.Date()

# For 3-way cross-validation: Randomly spilt the data set into training (j=1), test (j=0) set and validation (j=2) set
df$d <- runif(dim(df)[1],0,1)
df$d[df$d>=0.20]              <- 1
#df$d[df$d<=0.35 & df$d >=0.2] <- 2
df$d[df$d<0.20]               <- 0 

# Define Categorical Variables
df$stayer1 <- (df$birthloc==df$loc1)
J          <- nlevels(df$loc1occ)
for (j in 1:(J/2)) {
    df[,c(paste0("spbornHere",j))]      <- factor(ifelse(df[,c(paste0("spbornHere",j))]  ==1,"spbornHere"  ,"spbornElsewhere"))
    df[,c(paste0("bornHere",j))]        <- factor(ifelse(df[,c(paste0("bornHere",j))]    ==1,"bornHere"    ,"bornElsewhere"))
    df[,c(paste0("bornDiv",j))]         <- factor(ifelse(df[,c(paste0("bornDiv",j))]     ==1,"bornDiv"     ,"bornNonDiv"))
    df[,c(paste0("bornReg",j))]         <- factor(ifelse(df[,c(paste0("bornReg",j))]     ==1,"bornReg"     ,"bornNonReg"))
}
head(df)

#------------------------------------------
# Main Estimation
#------------------------------------------
# Initialize variables
df <- cbind(df,matrix(0,dim(df)[1],J))
colnames(df)[(length(df) - J + 1):length(df)]<- paste0("pTree", seq(1,J))
df <- cbind(df,matrix(1,dim(df)[1],J))
colnames(df)[(length(df) - J + 1):length(df)]<- paste0("clusterID", seq(1,J))
df$clusterID <- matrix(0,dim(df)[1],1)
varsbig <- c("loc1occ","birthloc","educvar","marst","race","female","liveWithFamily","spwork","childlt5","childlt18","age","ownershp","spbornHere","meanRelOcc")
vars    <- c("loc1occ","educvar","marst","race","female","liveWithFamily","spwork","childlt5","childlt18","age","ownershp","spbornHere","meanRelOcc")

# Create training, validation, and test sets
train <- df %>% filter(d==1) #%>% as_tibble
tset  <- df %>% filter(d==0) #%>% as_tibble
#xval  <- df %>% filter(d==2) #%>% as_tibble

print(dim(train))
print(dim(tset))
#print(dim(xval))
print(dim(df))

for (j in 1:J) {
    train$tempdv <- factor(ifelse(train$loc1occ==levels(train$loc1occ)[j],levels(train$loc1occ)[j],"Else"))
    print(levels(train$tempdv))
    if (j<=(J/2)) {
        train$tempSpBornHere <- train[,c(paste0("spbornHere",j))]
        train$tempBornHere   <- train[,c(paste0("bornHere",j))]
        train$tempBornDiv    <- train[,c(paste0("bornDiv",j))]
        train$tempBornReg    <- train[,c(paste0("bornReg",j))]
        tset$tempSpBornHere  <- tset[,c(paste0("spbornHere",j))]
        tset$tempBornHere    <- tset[,c(paste0("bornHere",j))]
        tset$tempBornDiv     <- tset[,c(paste0("bornDiv",j))]
        tset$tempBornReg     <- tset[,c(paste0("bornReg",j))]
        #xval$tempSpBornHere  <- xval[,c(paste0("spbornHere",j))]
        #xval$tempBornHere    <- xval[,c(paste0("bornHere",j))]
        #xval$tempBornDiv     <- xval[,c(paste0("bornDiv",j))]
        #xval$tempBornReg     <- xval[,c(paste0("bornReg",j))]
        df$tempSpBornHere    <- df[,c(paste0("spbornHere",j))]
        df$tempBornHere      <- df[,c(paste0("bornHere",j))]
        df$tempBornDiv       <- df[,c(paste0("bornDiv",j))]
        df$tempBornReg       <- df[,c(paste0("bornReg",j))]
    } else {
        train$tempSpBornHere <- train[,c(paste0("spbornHere",j-(J/2)))]
        train$tempBornHere   <- train[,c(paste0("bornHere",j-(J/2)))]
        train$tempBornDiv    <- train[,c(paste0("bornDiv",j-(J/2)))]
        train$tempBornReg    <- train[,c(paste0("bornReg",j-(J/2)))]
        tset$tempSpBornHere  <- tset[,c(paste0("spbornHere",j-(J/2)))]
        tset$tempBornHere    <- tset[,c(paste0("bornHere",j-(J/2)))]
        tset$tempBornDiv     <- tset[,c(paste0("bornDiv",j-(J/2)))]
        tset$tempBornReg     <- tset[,c(paste0("bornReg",j-(J/2)))]
        #xval$tempSpBornHere  <- xval[,c(paste0("spbornHere",j-(J/2)))]
        #xval$tempBornHere    <- xval[,c(paste0("bornHere",j-(J/2)))]
        #xval$tempBornDiv     <- xval[,c(paste0("bornDiv",j-(J/2)))]
        #xval$tempBornReg     <- xval[,c(paste0("bornReg",j-(J/2)))]
        df$tempSpBornHere    <- df[,c(paste0("spbornHere",j-(J/2)))]
        df$tempBornHere      <- df[,c(paste0("bornHere",j-(J/2)))]
        df$tempBornDiv       <- df[,c(paste0("bornDiv",j-(J/2)))]
        df$tempBornReg       <- df[,c(paste0("bornReg",j-(J/2)))]
    }
    print(sum(train$loc1occ==levels(train$loc1occ)[j]))
    
    # perform the tree classification algorithm
    model.ctree <- ctree(tempdv~tempBornHere+tempSpBornHere+tempBornDiv+tempBornReg+educvar+as.factor(advdegree)+married+racer+liveWithFamily+spwork+childlt5+childlt18+age+residMeanStay+residMeanRelOcc,data=train,control=ctree_control(minbucket=500,minsplit=500,testtype="Bonferroni",mincriterion=0.99))
    # export the tree to EPS file
    setEPS()
    postscript(paste0("../Paper/Graphics/occOVA",j,".eps"))
    plot(model.ctree)
    dev.off()
    # predict class probabilities in training set
    preddy <- predict(model.ctree,newdata=train,type="prob")
    train[, c(paste0("pTree",j))] <- as.matrix(preddy[,c(levels(train$loc1occ)[j])])

    # predict classes in df set (for estimation later)
    preddyCat <- predict(model.ctree,newdata=df,type="prob")
    df[, c(paste0("pTree",j))] <- as.matrix(preddyCat[,c(levels(df$loc1occ)[j])])
    # return node IDs in df set
    preddy <- predict(model.ctree,newdata=df,type="node")
    tempvar <- 100000*j+preddy
    df[,c(paste0("clusterID",j))] <- tempvar
    print(paste("Number of clusters for DV ",j,": ",max(preddy),sep=""))
    print(plyr::count(tempvar))

    # predict classes in validation set (for model selection)
    #preddyCat <- predict(model.ctree,newdata=xval,type="prob")
    #xval[, c(paste0("pTree",j))] <- as.matrix(preddyCat[,c(levels(xval$loc1occ)[j])])

    # predict classes in test (for accuracy measurement)
    preddyCat <- predict(model.ctree,newdata=tset,type="prob")
    tset[, c(paste0("pTree",j))] <- as.matrix(preddyCat[,c(levels(tset$loc1occ)[j])])
}

print(head(df[,c("loc1occ",paste0("pTree",seq(1,J)))]))

# Assign cluster to first-best location
for (j in 1:J) {
    df$clusterID[df$loc1occ==levels(df$loc1occ)[j],1]<-df[df$loc1occ==levels(df$loc1occ)[j],c(paste0("clusterID",j))]
}

# Normalize probabilities to sum to 1:
df$denom     <- rowSums(df[    ,c(paste0("pTree", seq(1,J)))], dims=1)
train$denom   <- rowSums(train[  ,c(paste0("pTree", seq(1,J)))], dims=1)
#xval$denom    <- rowSums(xval[   ,c(paste0("pTree", seq(1,J)))], dims=1)
tset$denom    <- rowSums(tset[   ,c(paste0("pTree", seq(1,J)))], dims=1)
df[    ,c(paste0("pTree", seq(1,J)),"denom")] <- df[    ,c(paste0("pTree", seq(1,J)),"denom")]/df$denom
train[  ,c(paste0("pTree", seq(1,J)),"denom")] <- train[  ,c(paste0("pTree", seq(1,J)),"denom")]/train$denom
#xval[   ,c(paste0("pTree", seq(1,J)),"denom")] <- xval[   ,c(paste0("pTree", seq(1,J)),"denom")]/xval$denom
tset[   ,c(paste0("pTree", seq(1,J)),"denom")] <- tset[   ,c(paste0("pTree", seq(1,J)),"denom")]/tset$denom

head(df[    ,c(paste0("pTree", seq(1,J)),"denom")])

# Create predicted location for classification performance measurement
train$predloc1occ   <- apply(train[  ,c(paste0("pTree", seq(1,J)))], 1, which.max)
#xval$predloc1occ    <- apply(xval[   ,c(paste0("pTree", seq(1,J)))], 1, which.max)
tset$predloc1occ    <- apply(tset[   ,c(paste0("pTree", seq(1,J)))], 1, which.max)

dfps <- df[,c(paste0("pTree", seq(1,J)))]

print(dim(df))
print(dim(dfps))

# Fix labeling on predicted classification
train %<>% mutate(predloc1occ = as.factor(predloc1occ))
save(train,tset,file="TEST.rda")
print('printing levels of predloc1oc')
print(levels(train$predloc1occ))
train %<>% mutate(predloc1occ = fct_recode(predloc1occ, "California Related" = "1", "Texas Related" = "2", "Florida Related" = "3", "Illinois Related" = "4", "NewYork Related" = "5", "NewEngland Related" = "6", "NJ/PA Related" = "7", "DMV Related" = "8", "NC/SC/GA Related" = "9", "ESCentral Related" = "10", "OH/IN/MI/WI Related" = "11", "WNCentral Related" = "12", "OK/AR/LA Related" = "13", "Mountain Related" = "14", "Pacific Related" = "15", "California Unrelated" = "16", "Texas Unrelated" = "17", "Florida Unrelated" = "18", "Illinois Unrelated" = "19", "NewYork Unrelated" = "20", "NewEngland Unrelated" = "21", "NJ/PA Unrelated" = "22", "DMV Unrelated" = "23", "NC/SC/GA Unrelated" = "24", "ESCentral Unrelated" = "25", "OH/IN/MI/WI Unrelated" = "26", "WNCentral Unrelated" = "27", "OK/AR/LA Unrelated" = "28", "Mountain Unrelated" = "29", "Pacific Unrelated" = "30"))
tset %<>% mutate(predloc1occ = as.factor(predloc1occ),
                 predloc1occ = fct_recode(predloc1occ, "California Related" = "1", "Texas Related" = "2", "Florida Related" = "3", "Illinois Related" = "4", "NewYork Related" = "5", "NewEngland Related" = "6", "NJ/PA Related" = "7", "DMV Related" = "8", "NC/SC/GA Related" = "9", "ESCentral Related" = "10", "OH/IN/MI/WI Related" = "11", "WNCentral Related" = "12", "OK/AR/LA Related" = "13", "Mountain Related" = "14", "Pacific Related" = "15", "California Unrelated" = "16", "Texas Unrelated" = "17", "Florida Unrelated" = "18", "Illinois Unrelated" = "19", "NewYork Unrelated" = "20", "NewEngland Unrelated" = "21", "NJ/PA Unrelated" = "22", "DMV Unrelated" = "23", "NC/SC/GA Unrelated" = "24", "ESCentral Unrelated" = "25", "OH/IN/MI/WI Unrelated" = "26", "WNCentral Unrelated" = "27", "OK/AR/LA Unrelated" = "28", "Mountain Unrelated" = "29", "Pacific Unrelated" = "30"))

# Calculate prediction accuracy in training set (train) [kappa, etc.]
print(class(train$predloc1occ))
print(class(train$loc1occ))
print(plyr::count(train, 'loc1occ'))
print(plyr::count(train, 'predloc1occ'))
tester <- plyr::count(train, 'predloc1occ')
train$predloc1occ <- factor(train$predloc1occ,labels=levels(train$loc1occ)[tester[,1]])
table(train$loc1occ) %>% print
table(train$predloc1occ) %>% print

print("Tree accuracy in training set")
print(caret::confusionMatrix(train$predloc1occ,train$loc1occ)$overall[1:2])
print(head(as.matrix(cbind(train$predloc1occ,train$loc1occ))))
print(tail(as.matrix(cbind(train$predloc1occ,train$loc1occ))))
print(dim(as.matrix(cbind(train$predloc1occ,train$loc1occ))))

# # Calculate prediction accuracy in validation set (xval)
# print(class(xval$predloc1occ))
# print(class(xval$loc1occ))
# print(plyr::count(xval, 'loc1occ'))
# print(plyr::count(xval, 'predloc1occ'))
# tester <- plyr::count(xval, 'predloc1occ')
# xval$predloc1occ <- factor(xval$predloc1occ,labels=levels(xval$loc1occ)[tester[,1]])
# print(caret::confusionMatrix(xval$predloc1occ,xval$loc1occ)$overall[1:2])
# print(head(as.matrix(cbind(xval$predloc1occ,xval$loc1occ))))
# print(tail(as.matrix(cbind(xval$predloc1occ,xval$loc1occ))))
# print(dim(as.matrix(cbind(xval$predloc1occ,xval$loc1occ))))

# Calculate prediction accuracy in test set (test)
print(class(tset$predloc1occ))
print(class(tset$loc1occ))
print(plyr::count(tset, 'loc1occ'))
print(plyr::count(tset, 'predloc1occ'))
tester <- plyr::count(tset, 'predloc1occ')
tset$predloc1occ <- factor(tset$predloc1occ,labels=levels(tset$loc1occ)[tester[,1]])
print("Tree accuracy in test set")
print(caret::confusionMatrix(tset$predloc1occ,tset$loc1occ)$overall[1:2])
print(head(as.matrix(cbind(tset$predloc1occ,tset$loc1occ))))
print(tail(as.matrix(cbind(tset$predloc1occ,tset$loc1occ))))
print(dim(as.matrix(cbind(tset$predloc1occ,tset$loc1occ))))

# create first-best, second-best, and retention probabilities:
maxn <- function(n) function(x) order(x, decreasing = TRUE)[n]
df$tree.prob.first.best  <- matrix(data=0,nrow=dim(df)[1],ncol=1)
df$tree.prob.occ         <- matrix(data=0,nrow=dim(df)[1],ncol=1)
df$tree.prob.loc         <- matrix(data=0,nrow=dim(df)[1],ncol=1)
df$tree.prob.second.best <- matrix(data=0,nrow=dim(df)[1],ncol=1)
df$tree.prob.third.best  <- matrix(data=0,nrow=dim(df)[1],ncol=1)
df$tree.prob.fourth.best <- matrix(data=0,nrow=dim(df)[1],ncol=1)
df$tree.prob.retention   <- matrix(data=0,nrow=dim(df)[1],ncol=1)
head(df$tree.prob.first.best,20)
head(df$tree.prob.occ,20)
head(df$tree.prob.loc,20)
head(df$tree.prob.second.best)
head(df$tree.prob.third.best)
head(df$tree.prob.fourth.best)
head(df$tree.prob.retention)
df$tree.prob.retention[,1] <- dfps[cbind(1:nrow(dfps), as.numeric(df$birthloc))]+dfps[cbind(1:nrow(dfps), as.numeric(df$birthloc)+J/2)]
for (j in 1:J) {
    df$tree.prob.first.best[as.numeric(df$loc1occ)==j,1] <- dfps[as.numeric(df$loc1occ)==j,j]
    if (j<=J/2) {
        df$tree.prob.loc[as.numeric(df$loc1occ)==j,1] <- dfps[as.numeric(df$loc1occ)==j,j+J/2]
        tempy1 <- dfps[as.numeric(df$loc1occ)==j,-((J/2)+1):-J]
        df$tree.prob.occ[as.numeric(df$loc1occ)==j,1] <- rowSums(tempy1, dims=1)-df$tree.prob.first.best[as.numeric(df$loc1occ)==j,1]
    } else {
        df$tree.prob.loc[as.numeric(df$loc1occ)==j,1] <- dfps[as.numeric(df$loc1occ)==j,j-J/2]
        tempy1 <- dfps[as.numeric(df$loc1occ)==j,-1:-(J/2)]
        df$tree.prob.occ[as.numeric(df$loc1occ)==j,1] <- rowSums(tempy1, dims=1)-df$tree.prob.first.best[as.numeric(df$loc1occ)==j,1]
    }
    tempy <- dfps[as.numeric(df$loc1occ)==j,-j]
    df$tree.prob.second.best[as.numeric(df$loc1occ)==j,1] <- apply(tempy,1,max)
    df$tree.prob.third.best[as.numeric(df$loc1occ)==j,1] <- apply(tempy,1,function(x)x[maxn(2)(x)])
    df$tree.prob.fourth.best[as.numeric(df$loc1occ)==j,1] <- apply(tempy,1,function(x)x[maxn(3)(x)])
}
head(dfps)
head(df$tree.prob.first.best,20)
head(df$tree.prob.occ,20)
head(df$tree.prob.loc,20)
head(df$tree.prob.second.best,20)
head(df$tree.prob.third.best,20)
head(df$tree.prob.fourth.best,20)
print(df[is.na(df$tree.prob.first.best),])
head(df$tree.prob.retention)
str(df$tree.prob.first.best)
str(df$tree.prob.occ)
str(df$tree.prob.loc)
str(df$tree.prob.second.best)
str(df$tree.prob.third.best)
str(df$tree.prob.fourth.best)
str(df$tree.prob.retention)
summary(df$tree.prob.first.best)
summary(df$tree.prob.occ)
summary(df$tree.prob.loc)
summary(df$tree.prob.second.best)
summary(df$tree.prob.third.best)
summary(df$tree.prob.fourth.best)
summary(df$tree.prob.retention)
df$stayer.prob.first.best  <- df$tree.prob.first.best*(df$stayer==TRUE)
df$stayer.prob.second.best <- df$tree.prob.second.best*(df$stayer==TRUE)
df$mover.prob.first.best   <- df$tree.prob.first.best*(df$stayer==FALSE)
df$mover.prob.second.best  <- df$tree.prob.second.best*(df$stayer==FALSE)
df$mover.prob.retention    <- df$tree.prob.retention*(df$stayer==FALSE)

# Migration Probability Summary Statistics by education level [all groups together]
dfdt <- data.table(df[,c("id","clusterID","educvar","tree.prob.first.best")])
freqs0 <- as.data.frame(dfdt[, length(unique(id)), by = 'educvar'])
freqs  <- as.data.frame(dfdt[, length(unique(clusterID)), by = 'educvar']) # 1 other way to do this: dfdt[, count := length(unique(clusterID)), by=educvar]
freqs2 <- as.data.frame(dfdt[, mean(tree.prob.first.best), by = 'educvar'])
freqs3 <- as.data.frame(dfdt[, sd(tree.prob.first.best), by = 'educvar'])
freqs4 <- as.data.frame(dfdt[, quantile(tree.prob.first.best,c(.1)), by = 'educvar'])
freqs5 <- as.data.frame(dfdt[, quantile(tree.prob.first.best,c(.9)), by = 'educvar'])
names(freqs0)[names(freqs0)=="V1"] <- "Individuals"
names(freqs)[names(freqs)=="V1"]   <- "Cells"
names(freqs2)[names(freqs2)=="V1"] <- "Mean"
names(freqs3)[names(freqs3)=="V1"] <- "Std. Dev."
names(freqs4)[names(freqs4)=="V1"] <- "10th Percentile"
names(freqs5)[names(freqs5)=="V1"] <- "90th Percentile"
freqtable <- merge(freqs,freqs0,by="educvar")
freqtable <- merge(freqtable,freqs2,by="educvar")
freqtable <- merge(freqtable,freqs3,by="educvar")
freqtable <- merge(freqtable,freqs4,by="educvar")
freqtable <- merge(freqtable,freqs5,by="educvar")
names(freqtable)[names(freqtable)=="educvar"] <- "Education Level"
print(freqtable)
save(freqtable,file="freqtableOccMXvalOVA.Rda")
levels(freqtable$'Education Level')<-c("Education Major","Social Sciences Major","Other Major","Business Major","STEM Major")
freqtable$sorty <- c(4,1,3,2,5)
freqtable <- freqtable[order(freqtable$sorty),]
#freqtable <- freqtable[c(4,1,3,2,5)]
freqtable <- subset(freqtable,select=-sorty)
freqtable[,c("Mean","Std. Dev.","10th Percentile","90th Percentile")] <- Hmisc::format.df(freqtable[,c("Mean","Std. Dev.","10th Percentile","90th Percentile")],dec=4)
freqtable[,c("Individuals","Cells")] <- Hmisc::format.df(freqtable[,c("Individuals","Cells")],dec=0,big.mark=",")
freqtable[,c("Education Level")] <- c("Education Major","Social Sciences Major","Other Major","Business Major","STEM Major")
#w<-latex(freqtable, file="../Paper/Tables/migProbsOccMXvalOVA.tex", booktabs=TRUE,caption="Summary of Migration Probabilities",label="tab:migprobs",multicol=FALSE,rowname=NULL,where="ht",col.just=c("l","c","c","c","c","c","c"))
#w<-latex(freqtable, file="../Paper/Tables/migProbsOccTabularOnlyMXvalOVA.tex", booktabs=TRUE,table.env=FALSE,multicol=FALSE,rowname=NULL,center="none",where="ht",col.just=c("l","c","c","c","c","c","c"))

# Migration Probability Summary Statistics by education level [stayers in related occ only]
dfdt <- data.table(df[df$stayer==TRUE & df$relocc==TRUE,c("id","clusterID","educvar","tree.prob.first.best")])
freqs0 <- as.data.frame(dfdt[, length(unique(id)), by = 'educvar'])
freqs  <- as.data.frame(dfdt[, length(unique(clusterID)), by = 'educvar']) # 1 other way to do this: dfdt[, count := length(unique(clusterID)), by=educvar]
freqs2 <- as.data.frame(dfdt[, mean(tree.prob.first.best), by = 'educvar'])
freqs3 <- as.data.frame(dfdt[, sd(tree.prob.first.best), by = 'educvar'])
freqs4 <- as.data.frame(dfdt[, quantile(tree.prob.first.best,c(.1)), by = 'educvar'])
freqs5 <- as.data.frame(dfdt[, quantile(tree.prob.first.best,c(.9)), by = 'educvar'])
names(freqs0)[names(freqs0)=="V1"] <- "Individuals"
names(freqs)[names(freqs)=="V1"] <- "Cells"
names(freqs2)[names(freqs2)=="V1"] <- "Mean"
names(freqs3)[names(freqs3)=="V1"] <- "Std. Dev."
names(freqs4)[names(freqs4)=="V1"] <- "10th Percentile"
names(freqs5)[names(freqs5)=="V1"] <- "90th Percentile"
freqtable <- merge(freqs,freqs0,by="educvar")
freqtable <- merge(freqtable,freqs2,by="educvar")
freqtable <- merge(freqtable,freqs3,by="educvar")
freqtable <- merge(freqtable,freqs4,by="educvar")
freqtable <- merge(freqtable,freqs5,by="educvar")
names(freqtable)[names(freqtable)=="educvar"] <- "Education Level"
print(freqtable)
save(freqtable,file="freqtableMoverOccMXvalOVA.Rda")
levels(freqtable$'Education Level')<-c("Education Major","Social Sciences Major","Other Major","Business Major","STEM Major")
freqtable$sorty <- c(4,1,3,2,5)
freqtable <- freqtable[order(freqtable$sorty),]
#freqtable <- freqtable[c(4,1,3,2,5)]
freqtable <- subset(freqtable,select=-sorty)
freqtable[,c("Mean","Std. Dev.","10th Percentile","90th Percentile")] <- Hmisc::format.df(freqtable[,c("Mean","Std. Dev.","10th Percentile","90th Percentile")],dec=4)
freqtable[,c("Individuals","Cells")] <- Hmisc::format.df(freqtable[,c("Individuals","Cells")],dec=0,big.mark=",")
freqtable[,c("Education Level")] <- c("Education Major","Social Sciences Major","Other Major","Business Major","STEM Major")
#w<-latex(freqtable, file="../Paper/Tables/stayerMigProbsRelOccMXvalOVA.tex", booktabs=TRUE,caption="Summary of Migration Probabilities",label="tab:migprobs",multicol=FALSE,rowname=NULL,where="ht",col.just=c("l","c","c","c","c","c","c"))
w<-latex(freqtable, file="../Paper/Tables/stayerMigProbsRelOccTabularOnlyMXvalOVA.tex", booktabs=TRUE,table.env=FALSE,multicol=FALSE,rowname=NULL,center="none",where="ht",col.just=c("l","c","c","c","c","c","c"))

# Migration Probability Summary Statistics by education level [stayers not in related occ only]
dfdt <- data.table(df[df$stayer==TRUE & df$relocc==FALSE,c("id","clusterID","educvar","tree.prob.first.best")])
freqs0 <- as.data.frame(dfdt[, length(unique(id)), by = 'educvar'])
freqs  <- as.data.frame(dfdt[, length(unique(clusterID)), by = 'educvar']) # 1 other way to do this: dfdt[, count := length(unique(clusterID)), by=educvar]
freqs2 <- as.data.frame(dfdt[, mean(tree.prob.first.best), by = 'educvar'])
freqs3 <- as.data.frame(dfdt[, sd(tree.prob.first.best), by = 'educvar'])
freqs4 <- as.data.frame(dfdt[, quantile(tree.prob.first.best,c(.1)), by = 'educvar'])
freqs5 <- as.data.frame(dfdt[, quantile(tree.prob.first.best,c(.9)), by = 'educvar'])
names(freqs0)[names(freqs0)=="V1"] <- "Individuals"
names(freqs)[names(freqs)=="V1"] <- "Cells"
names(freqs2)[names(freqs2)=="V1"] <- "Mean"
names(freqs3)[names(freqs3)=="V1"] <- "Std. Dev."
names(freqs4)[names(freqs4)=="V1"] <- "10th Percentile"
names(freqs5)[names(freqs5)=="V1"] <- "90th Percentile"
freqtable <- merge(freqs,freqs0,by="educvar")
freqtable <- merge(freqtable,freqs2,by="educvar")
freqtable <- merge(freqtable,freqs3,by="educvar")
freqtable <- merge(freqtable,freqs4,by="educvar")
freqtable <- merge(freqtable,freqs5,by="educvar")
names(freqtable)[names(freqtable)=="educvar"] <- "Education Level"
print(freqtable)
save(freqtable,file="freqtableMoverOccMXvalOVA.Rda")
levels(freqtable$'Education Level')<-c("Education Major","Social Sciences Major","Other Major","Business Major","STEM Major")
freqtable$sorty <- c(4,1,3,2,5)
freqtable <- freqtable[order(freqtable$sorty),]
#freqtable <- freqtable[c(4,1,3,2,5)]
freqtable <- subset(freqtable,select=-sorty)
freqtable[,c("Mean","Std. Dev.","10th Percentile","90th Percentile")] <- Hmisc::format.df(freqtable[,c("Mean","Std. Dev.","10th Percentile","90th Percentile")],dec=4)
freqtable[,c("Individuals","Cells")] <- Hmisc::format.df(freqtable[,c("Individuals","Cells")],dec=0,big.mark=",")
freqtable[,c("Education Level")] <- c("Education Major","Social Sciences Major","Other Major","Business Major","STEM Major")
#w<-latex(freqtable, file="../Paper/Tables/stayerMigProbsNotRelOccMXvalOVA.tex", booktabs=TRUE,caption="Summary of Migration Probabilities",label="tab:migprobs",multicol=FALSE,rowname=NULL,where="ht",col.just=c("l","c","c","c","c","c","c"))
w<-latex(freqtable, file="../Paper/Tables/stayerMigProbsNotRelOccTabularOnlyMXvalOVA.tex", booktabs=TRUE,table.env=FALSE,multicol=FALSE,rowname=NULL,center="none",where="ht",col.just=c("l","c","c","c","c","c","c"))

# Migration Probability Summary Statistics by education level [movers in related occ only]
dfdt <- data.table(df[df$stayer==FALSE & df$relocc==TRUE,c("id","clusterID","educvar","tree.prob.first.best")])
freqs0 <- as.data.frame(dfdt[, length(unique(id)), by = 'educvar'])
freqs  <- as.data.frame(dfdt[, length(unique(clusterID)), by = 'educvar']) # 1 other way to do this: dfdt[, count := length(unique(clusterID)), by=educvar]
freqs2 <- as.data.frame(dfdt[, mean(tree.prob.first.best), by = 'educvar'])
freqs3 <- as.data.frame(dfdt[, sd(tree.prob.first.best), by = 'educvar'])
freqs4 <- as.data.frame(dfdt[, quantile(tree.prob.first.best,c(.1)), by = 'educvar'])
freqs5 <- as.data.frame(dfdt[, quantile(tree.prob.first.best,c(.9)), by = 'educvar'])
names(freqs0)[names(freqs0)=="V1"] <- "Individuals"
names(freqs)[names(freqs)=="V1"] <- "Cells"
names(freqs2)[names(freqs2)=="V1"] <- "Mean"
names(freqs3)[names(freqs3)=="V1"] <- "Std. Dev."
names(freqs4)[names(freqs4)=="V1"] <- "10th Percentile"
names(freqs5)[names(freqs5)=="V1"] <- "90th Percentile"
freqtable <- merge(freqs,freqs0,by="educvar")
freqtable <- merge(freqtable,freqs2,by="educvar")
freqtable <- merge(freqtable,freqs3,by="educvar")
freqtable <- merge(freqtable,freqs4,by="educvar")
freqtable <- merge(freqtable,freqs5,by="educvar")
names(freqtable)[names(freqtable)=="educvar"] <- "Education Level"
print(freqtable)
save(freqtable,file="freqtableStayerOccMXvalOVA.Rda")
levels(freqtable$'Education Level')<-c("Education Major","Social Sciences Major","Other Major","Business Major","STEM Major")
freqtable$sorty <- c(4,1,3,2,5)
freqtable <- freqtable[order(freqtable$sorty),]
#freqtable <- freqtable[c(4,1,3,2,5)]
freqtable <- subset(freqtable,select=-sorty)
freqtable[,c("Mean","Std. Dev.","10th Percentile","90th Percentile")] <- Hmisc::format.df(freqtable[,c("Mean","Std. Dev.","10th Percentile","90th Percentile")],dec=4)
freqtable[,c("Individuals","Cells")] <- Hmisc::format.df(freqtable[,c("Individuals","Cells")],dec=0,big.mark=",")
freqtable[,c("Education Level")] <- c("Education Major","Social Sciences Major","Other Major","Business Major","STEM Major")
#w<-latex(freqtable, file="../Paper/Tables/moverMigProbsRelOccMXvalOVA.tex", booktabs=TRUE,caption="Summary of Migration Probabilities",label="tab:migprobs",multicol=FALSE,rowname=NULL,where="ht",col.just=c("l","c","c","c","c","c","c"))
w<-latex(freqtable, file="../Paper/Tables/moverMigProbsRelOccTabularOnlyMXvalOVA.tex", booktabs=TRUE,table.env=FALSE,multicol=FALSE,rowname=NULL,center="none",where="ht",col.just=c("l","c","c","c","c","c","c"))

# Migration Probability Summary Statistics by education level [movers not in related occ only]
dfdt <- data.table(df[df$stayer==FALSE & df$relocc==FALSE,c("id","clusterID","educvar","tree.prob.first.best")])
freqs0 <- as.data.frame(dfdt[, length(unique(id)), by = 'educvar'])
freqs  <- as.data.frame(dfdt[, length(unique(clusterID)), by = 'educvar']) # 1 other way to do this: dfdt[, count := length(unique(clusterID)), by=educvar]
freqs2 <- as.data.frame(dfdt[, mean(tree.prob.first.best), by = 'educvar'])
freqs3 <- as.data.frame(dfdt[, sd(tree.prob.first.best), by = 'educvar'])
freqs4 <- as.data.frame(dfdt[, quantile(tree.prob.first.best,c(.1)), by = 'educvar'])
freqs5 <- as.data.frame(dfdt[, quantile(tree.prob.first.best,c(.9)), by = 'educvar'])
names(freqs0)[names(freqs0)=="V1"] <- "Individuals"
names(freqs)[names(freqs)=="V1"] <- "Cells"
names(freqs2)[names(freqs2)=="V1"] <- "Mean"
names(freqs3)[names(freqs3)=="V1"] <- "Std. Dev."
names(freqs4)[names(freqs4)=="V1"] <- "10th Percentile"
names(freqs5)[names(freqs5)=="V1"] <- "90th Percentile"
freqtable <- merge(freqs,freqs0,by="educvar")
freqtable <- merge(freqtable,freqs2,by="educvar")
freqtable <- merge(freqtable,freqs3,by="educvar")
freqtable <- merge(freqtable,freqs4,by="educvar")
freqtable <- merge(freqtable,freqs5,by="educvar")
names(freqtable)[names(freqtable)=="educvar"] <- "Education Level"
print(freqtable)
save(freqtable,file="freqtableStayerOccMXvalOVA.Rda")
levels(freqtable$'Education Level')<-c("Education Major","Social Sciences Major","Other Major","Business Major","STEM Major")
levels(freqtable$'Education Level')<-c("Education Major","Social Sciences Major","Other Major","Business Major","STEM Major")
freqtable$sorty <- c(4,1,3,2,5)
freqtable <- freqtable[order(freqtable$sorty),]
#freqtable <- freqtable[c(4,1,3,2,5)]
freqtable <- subset(freqtable,select=-sorty)
freqtable[,c("Mean","Std. Dev.","10th Percentile","90th Percentile")] <- Hmisc::format.df(freqtable[,c("Mean","Std. Dev.","10th Percentile","90th Percentile")],dec=4)
freqtable[,c("Individuals","Cells")] <- Hmisc::format.df(freqtable[,c("Individuals","Cells")],dec=0,big.mark=",")
freqtable[,c("Education Level")] <- c("Education Major","Social Sciences Major","Other Major","Business Major","STEM Major")
#w<-latex(freqtable, file="../Paper/Tables/moverMigProbsNotRelOccMXvalOVA.tex", booktabs=TRUE,caption="Summary of Migration Probabilities",label="tab:migprobs",multicol=FALSE,rowname=NULL,where="ht",col.just=c("l","c","c","c","c","c","c"))
w<-latex(freqtable, file="../Paper/Tables/moverMigProbsNotRelOccTabularOnlyMXvalOVA.tex", booktabs=TRUE,table.env=FALSE,multicol=FALSE,rowname=NULL,center="none",where="ht",col.just=c("l","c","c","c","c","c","c"))


###################################################
# R code for Monte Carlo simulation of selection
# correction model using Dahl's index sufficiency
# Tyler Ransom
###################################################


#------------------------------------------
# Load Required Packages and Functions
#------------------------------------------
source("../initialize.R")

# for reproducibility
RNGkind(sample.kind = "Rounding")
set.seed(12345)

# draw discrete uniform
rdu <- function(en,ka,kb) sample(ka:kb,en,replace=T)

#------------------------------------------
# Setup of the simulation
#------------------------------------------
# Primitives:
# y_k  = s*beta_k + tau_k * a_i + b_ik
# u_jk = z*gamma_jk + eps_jk
# V_jk = y_k + u_jk
# y_k observed iff V_jk > V_mn for all (j,k) \neq (m,n)

# alternatives ordered as follows:
# 1   through L: 1st occupation in each location
# L+1 through L*K: 2nd occupation in each location

# Parameters of the simulation (sample size, choice set size, etc.)
M <- 2    # no. of majors
K <- 2    # no. of occupations
L <- 15   # no. of locations
J <- 15   # no. of birth locations
stopifnot(J==L) # Number of birth locations must be same as number of residence locations
N <- 1e3*L*K

jay <- 23

Nsim <- 100

varY   <- data.frame(matrix(0,Nsim,2))[,1]
vareta <- data.frame(matrix(0,Nsim,2))[,1]
coeffs <- data.frame(matrix(0,Nsim,17))
RMSEs  <- data.frame(matrix(0,Nsim,17))
devs   <- data.frame(matrix(0,Nsim,17))
Ns     <- data.frame(matrix(0,Nsim,17))
fits   <- data.frame(matrix(0,Nsim,8))
colnames(coeffs) <- c("OLS","bin1B3","bin1B4","bin2B3","bin2B4","tree1B3","tree1B4","tree2B3","tree2B4","logit1B3","logit1B4","logit2B3","logit2B4","binRough1B3","binRough1B4","binRough2B3","binRough2B4")
colnames(RMSEs ) <- c("OLS","bin1B3","bin1B4","bin2B3","bin2B4","tree1B3","tree1B4","tree2B3","tree2B4","logit1B3","logit1B4","logit2B3","logit2B4","binRough1B3","binRough1B4","binRough2B3","binRough2B4")
colnames(devs  ) <- c("OLS","bin1B3","bin1B4","bin2B3","bin2B4","tree1B3","tree1B4","tree2B3","tree2B4","logit1B3","logit1B4","logit2B3","logit2B4","binRough1B3","binRough1B4","binRough2B3","binRough2B4")
colnames(Ns    ) <- c("OLS","bin1B3","bin1B4","bin2B3","bin2B4","tree1B3","tree1B4","tree2B3","tree2B4","logit1B3","logit1B4","logit2B3","logit2B4","binRough1B3","binRough1B4","binRough2B3","binRough2B4")
colnames(fits  ) <- c("AccuracyBin","KappaBin","AccuracyTree","KappaTree","AccuracyLogit","KappaLogit","AccuracybinRough","KappabinRough")

# Parameters of wage equation
Beta <- array(sample(1:4,5*L*K,TRUE), dim=c(5,L,K)) / array(sample(1:4,5*L*K,TRUE), dim=c(5,L,K))
Beta[4, , ] <- -.1*array(runif(L*K), dim=c(1,L,K))
Beta[5, , ] <- .0001*array(runif(L*K), dim=c(1,L,K))
Beta[1,(jay-L),1] <- 1
Beta[1,(jay-L),2] <- 2
cons <- matrix(sample(-2:2,L*K,TRUE), ncol=K)

# Parameters of selection equation
gammaBPL              <- array(rdu(1*L*K, 0,1), dim=c(1,L,K))
gamma0                <- array(rdu(6*L*K,-2,2), dim=c(6,L,K))
# counter             <- array(c( 1, 2, 3, 4, 5, 6, 7, 8, 9,10,11,12,13,14,15,16,17,18,19,20, 21,22,23,24,25,26,27,28,29,30), dim=c(1,L,K))
gamma0[1, , ]         <- array(c( 8, 8, 0,10,10, 8,-9, 8, 2, 1, 8, 1, 1, 0,-1, 7, 3, 2, 3, 5,-14,-9, 8, 2, 2, 0, 2, 8, 0, 5), dim=c(1,L,K))
gamma1                <- array(rdu(9*L*K,-2,2), dim=c(9,L,K))
gamma0[2, ,2]         <- abs(gamma0[2, ,2])           # make sure occ shifter actually is correlated with an occupation
gamma1[seq(7,9), ,2]  <- abs(gamma1[seq(7,9), ,2])    # make sure occ shifter actually is correlated with an occupation
gamma0[2, ,1]         <- -1*abs(gamma0[2, ,1])        # make sure occ shifter actually is correlated with an occupation
gamma1[seq(7,9), ,1]  <- -1*abs(gamma1[seq(7,9), ,1]) # make sure occ shifter actually is correlated with an occupation
# Gamma <- abind(gammaBPL[1, , ],gamma1,gamma0[seq(1,dim(gamma0)[1]), , ],along=1)
Gamma <- abind(gammaBPL,gamma0[1, , ],gamma1,gamma0[seq(2,dim(gamma0)[1]), , ],along=1)

for (sim in 1:Nsim) {
try({
print(paste0("simulation number: ",sim))
#------------------------------------------
# Simulate the data
#------------------------------------------
# Generate exogenous X's and Z's:
# race
# marital status / fertility
# birth location
# major
# work experience
# occupational preference shifter
black          <- 1*(runif(N,0.0,1.0)>.8)
married        <- 1*(runif(N,0.0,1.0)>.45)
kids           <- 1*(runif(N,0.0,1.0)<.22)
birthloc       <- sample(1:J,N,TRUE)
major          <- 1*(runif(N,0.0,1.0)>.4)
exper          <- 15+4*rnorm(N)
exper[exper<0] <- 0
migShifter     <- 2*runif(N,0.0,1.0)-1
occShifter     <- 2*runif(N,0.0,1.0)-1
X <- data.frame(rep.int(1,N),birthloc,migShifter,occShifter,major,black,married,kids,exper,exper^2,exper^3)
Z <- data.frame(rep.int(1,N),X$major,X$black,X$black*X$major,X$kids,X$married,X$married*X$kids,X$occShifter,X$occShifter*X$married,X$occShifter*X$major,X$migShifter*X$occShifter,X$migShifter*X$major,X$migShifter*X$married,X$migShifter*X$kids,X$migShifter*X$married)
colnames(X)[1] <- "constant"
colnames(Z)[1] <- "constant"
rm(black)
rm(married)
rm(kids)
rm(birthloc)
rm(major)
rm(exper)
rm(occShifter)



# Preference shocks: distributed mv normal (could do mixture of (mv)normals)
# D    = J*L*K
# MU1  = rep.int(0,D)
# temp = matrix(runif(D*D), ncol=D)
# SIG1 = (t(temp)%*%temp)/5
# print('got here')
# # epsB = mvrnorm(N,MU1,SIG1)
# # browser()
# # epsB = reshape(epsB,[N J L K]);
# # eps  = zeros(N,L,K);
# # for j=1:J
# # eps(birthloc==j,:,:) = epsB(birthloc==j,j,:,:);
# # end

# Preference shocks: distributed iid normal (for now)
epsB <- array(rnorm(N*J*L*K, 0, 1), dim=c(N,J,L,K))
eps <- array(rep.int(0,N*L*K), dim=c(N,L,K))
for (j in 1:J) {
	eps[X$birthloc==j, , ] <- epsB[X$birthloc==j,j, , ]
}


# Earnings shocks: distributed iid normal
eta <- array(rnorm(N*L*K), dim=c(N,L,K))


# Generate earnings and utility
y <- array(rep.int(0,N*L*K), dim=c(N,L,K))
for (l in 1:L) {
	for (k in 1:K) {
		y[ ,l,k] = cons[l,k]*X$cons + Beta[1,l,k]*X$major + Beta[2,l,k]*X$black + Beta[3,l,k]*X$married +
		Beta[4,l,k]*X$exper + Beta[5,l,k]*X$exper.2 + Beta[5,l,k]*X$exper.3 + eta[ ,l,k]
	}
}

# print(summary(as.vector(y)))

u <- array(rep.int(0,N*L*K), dim=c(N,L,K))
for (l in 1:L) {
	for (k in 1:K) {
		u[ ,l,k] <- (X$birthloc==l)*Gamma[1,l,k]+eps[, l,k]
		# u[ ,l,k] <- (X$birthloc==l)*Gamma[1,l,k]+Z[,2 ]*Gamma[2 ,l,k]+Z[,3 ]*Gamma[3 ,l,k]+Z[,4 ]*Gamma[4 ,l,k]+
		            # Z[,5 ]*Gamma[5 ,l,k]+Z[,6 ]*Gamma[6 ,l,k]+Z[,7 ]*Gamma[7 ,l,k]+Z[,8 ]*Gamma[8 ,l,k]+
					# Z[,9 ]*Gamma[9 ,l,k]+Z[,10]*Gamma[10,l,k]+Z[,11]*Gamma[11,l,k]+Z[,12]*Gamma[12,l,k]+
					# Z[,13]*Gamma[13,l,k]+Z[,14]*Gamma[14,l,k]+Z[,15]*Gamma[15,l,k]+eps[, l,k]
		for (j in 1:length(Z)) {
			u[ ,l,k] <- u[ ,l,k]+(Z[ ,j]*Gamma[j,l,k])
		}
	}
}

V <- y+u
Vrand <- array(runif(N*L*K), dim=c(N,L,K))


# Find utility-maximizing option
Vresh = cbind(V[ , ,1],V[ , ,2])
inder <- apply(Vresh, 1, which.max)
inder1 <- matrix(rep.int(0,N*K), ncol=K)
inder1[inder<=L,1] = inder[inder<=L]
inder1[inder> L,1] = inder[inder> L]-L
inder1[inder<=L,2] = 1
inder1[inder> L,2] = 2

print(table(inder))
# browser()

# Observe earnings only in utility-maximized option
obsY   = rep.int(0,N)
obseta = rep.int(0,N)
for (i in 1:N) {
    obsY[i]   = y[i,inder1[i,1],inder1[i,2]]
    obseta[i] = eta[i,inder1[i,1],inder1[i,2]]
}

# Find utility-maximizing option if utility were random
Vresh = cbind(Vrand[ , ,1],Vrand[ , ,2])
inderRand <- apply(Vresh, 1, which.max)
inderRand1 <- matrix(rep.int(0,N*K), ncol=K)
inderRand1[inderRand<=L,1] = inderRand[inderRand<=L]
inderRand1[inderRand> L,1] = inderRand[inderRand> L]-L
inderRand1[inderRand<=L,2] = 1
inderRand1[inderRand> L,2] = 2

# Observe earnings randomly
obsYrand = rep.int(0,N)
for (i in 1:N) {
    obsYrand[i] = y[i,inderRand1[i,1],inderRand1[i,2]]
}

dat <- cbind(inder,inderRand,obsY,obsYrand,obseta,X)
colnames(dat)[1:5] <- c("choice","choiceRand","wage","wageRand","eta")
dat$resloc                       <- dat$choice
dat$resloc[dat$resloc>L]         <- dat$resloc[dat$resloc>L]-L
dat$reslocRand                   <- dat$choiceRand
dat$reslocRand[dat$reslocRand>L] <- dat$reslocRand[dat$reslocRand>L]-L
dat$relocc                       <- dat$choice<=L
dat$reloccRand                   <- dat$choiceRand<=L

# factorize categorical variables for estimation
dat$major      <- as.factor(dat$major)
dat$black      <- as.factor(dat$black)
dat$married    <- as.factor(dat$married)
dat$kids       <- as.factor(dat$kids)
dat$birthloc   <- as.factor(dat$birthloc)
dat$choice     <- as.factor(dat$choice)
dat$choiceRand <- as.factor(dat$choiceRand)
dat$resloc     <- as.factor(dat$resloc)
dat$reslocRand <- as.factor(dat$reslocRand)
dat$relocc     <- as.factor(dat$relocc)
dat$reloccRand <- as.factor(dat$reloccRand)
head(dat)



X$black    <- as.factor(X$black)
X$married  <- as.factor(X$married)
X$kids     <- as.factor(X$kids)
X$birthloc <- as.factor(X$birthloc)
X$major    <- as.factor(X$major)
#------------------------------------------
# Run naive regressions
#------------------------------------------
# struth <- lm(wageRand~major+black+married+poly(exper,degree=3,raw=T),subset=choiceRand==16,data=dat)
# print(summary(struth))
# biased <- lm(wage    ~major+black+married+poly(exper,degree=3,raw=T),subset=choice    ==16,data=dat)
# print(summary(biased))
# struth <- lm(wageRand~major+black+married+poly(exper,degree=3,raw=T),subset=choiceRand==1,data=dat)
# print(summary(struth))
# biased <- lm(wage    ~major+black+married+poly(exper,degree=3,raw=T),subset=choice    ==1,data=dat)
# print(summary(biased))

# Note: pooling occupations within a location and estimating fully interacted specification does not
# give same results as estimating separately ... why???
# struth <- lm(wageRand~major+black+married+poly(exper,degree=3,raw=T),subset=choiceRand==1,data=dat)
# print(summary(struth))
# struth <- lm(wageRand~major+black+married+poly(exper,degree=3,raw=T),subset=choiceRand==16,data=dat)
# print(summary(struth))
# struth <- lm(wageRand~relocc*(major+black+married+poly(exper,degree=3,raw=T)),subset=(choiceRand==1) | (choiceRand==L+1),data=dat)
# print(summary(struth))

print("bin")
tic()
#----------------------------------------
# Bin estimation
#----------------------------------------
# create choice dummies
dat1 <- dummy.data.frame(dat, names="choice", all=FALSE)
colnames(dat1)[(length(dat1) - (L*K) + 1):length(dat1)]<- paste0("d", seq(1,(L*K)))
dat <- cbind(dat,dat1)

# discretize occupation shifter
dat$occShifterQuant <- quantcut(dat$occShifter, q = seq(0,1,by=0.25))
dat$migShifterQuant <- quantcut(dat$migShifter, q = seq(0,1,by=0.25))

# Compute the bin probabilities
for (j in 1:(L*K)) {
	dat$tempdv <- 1*(dat$choice==j)
	# print(paste0("Location ",j,": ",levels(dat$tempdv)))
	if (j<=L) {
		dat$tempBornHere <- 1*(dat$birthloc==j)
	} else {
		dat$tempBornHere <- 1*(dat$birthloc==(j-L))
	}
	# print(sum(dat$choice==j))

	# collapse to take averages
	collapse1 <- summaryBy(tempdv~tempBornHere+major+black+married+kids+migShifterQuant+occShifterQuant,data=dat)

	# merge into main data to get predictions
	preddy <- join(dat, collapse1, by=c("tempBornHere","major","black","married","kids","migShifterQuant","occShifterQuant"), type='left', match='all')
	dat[ , c(paste0("pBin",j))] <- preddy[,length(preddy)]
}

dat$denom <- rowSums(dat[ ,c(paste0("pBin", seq(1,(L*K))))], dims=1)
dat[ ,c(paste0("pBin", seq(1,(L*K))),"denom")] <- dat[ ,c(paste0("pBin", seq(1,(L*K))),"denom")]/dat$denom
datps <- dat[,c(paste0("pBin", seq(1,(L*K))))]

# Create predicted location for classification performance measurement
dat$predchoice <- apply(dat[,c(paste0("pBin", seq(1,(L*K))))], 1, which.max)

# Calculate prediction accuracy
# print(class(dat$predchoice))
# print(class(dat$choice))
# print(count(dat, 'choice'))
# print(count(dat, 'predchoice'))
tester <-count(dat, 'predchoice')
dat$predchoice <- as.factor(dat$predchoice)
fits[sim,1]<- confusionMatrix(dat$predchoice,dat$choice)$overall[1]
fits[sim,2]<- confusionMatrix(dat$predchoice,dat$choice)$overall[2]

# print(head(datps))
# print(head(dat))

dat$bin.prob.first.best <- matrix(data=0,nrow=dim(dat)[1],ncol=1)
dat$bin.prob.loc        <- matrix(data=0,nrow=dim(dat)[1],ncol=1)

for (j in 1:(L*K)) {
	dat$bin.prob.first.best[as.numeric(dat$choice)==j,1] <- datps[as.numeric(dat$choice)==j,j]
	if (j<=L) {
		dat$bin.prob.loc[as.numeric(dat$choice)==j,1] <- datps[as.numeric(dat$choice)==j,j+L]
	} else {
		dat$bin.prob.loc[as.numeric(dat$choice)==j,1] <- datps[as.numeric(dat$choice)==j,j-L]
	}
}
toc()


print("binRough")
tic()
#----------------------------------------
# Bin estimation
#----------------------------------------
# create choice dummies
dat1 <- dummy.data.frame(dat, names="choice", all=FALSE)
colnames(dat1)[(length(dat1) - (L*K) + 1):length(dat1)]<- paste0("d", seq(1,(L*K)))
dat <- cbind(dat,dat1)

# discretize occupation shifter
dat$occShifterQuant <- quantcut(dat$occShifter, q = seq(0,1,by=0.25))
dat$migShifterQuant <- quantcut(dat$migShifter, q = seq(0,1,by=0.25))

# Compute the bin probabilities
for (j in 1:(L*K)) {
	dat$tempdv <- 1*(dat$choice==j)
	# print(paste0("Location ",j,": ",levels(dat$tempdv)))
	if (j<=L) {
		dat$tempBornHere <- 1*(dat$birthloc==j)
	} else {
		dat$tempBornHere <- 1*(dat$birthloc==(j-L))
	}
	# print(sum(dat$choice==j))

	# collapse to take averages
	collapse1 <- summaryBy(tempdv~tempBornHere+major+migShifterQuant+occShifterQuant,data=dat)

	# merge into main data to get predictions
	preddy <- join(dat, collapse1, by=c("tempBornHere","major","migShifterQuant","occShifterQuant"), type='left', match='all')
	dat[ , c(paste0("pBinRough",j))] <- preddy[,length(preddy)]
}

dat$denom <- rowSums(dat[ ,c(paste0("pBinRough", seq(1,(L*K))))], dims=1)
dat[ ,c(paste0("pBinRough", seq(1,(L*K))),"denom")] <- dat[ ,c(paste0("pBinRough", seq(1,(L*K))),"denom")]/dat$denom
datps <- dat[,c(paste0("pBinRough", seq(1,(L*K))))]

# Create predicted location for classification performance measurement
dat$predchoice <- apply(dat[,c(paste0("pBinRough", seq(1,(L*K))))], 1, which.max)

# Calculate prediction accuracy
# print(class(dat$predchoice))
# print(class(dat$choice))
# print(count(dat, 'choice'))
# print(count(dat, 'predchoice'))
tester <-count(dat, 'predchoice')
dat$predchoice <- as.factor(dat$predchoice)
fits[sim,1]<- confusionMatrix(dat$predchoice,dat$choice)$overall[1]
fits[sim,2]<- confusionMatrix(dat$predchoice,dat$choice)$overall[2]

# print(head(datps))
# print(head(dat))

dat$binRough.prob.first.best <- matrix(data=0,nrow=dim(dat)[1],ncol=1)
dat$binRough.prob.loc        <- matrix(data=0,nrow=dim(dat)[1],ncol=1)

for (j in 1:(L*K)) {
	dat$binRough.prob.first.best[as.numeric(dat$choice)==j,1] <- datps[as.numeric(dat$choice)==j,j]
	if (j<=L) {
		dat$binRough.prob.loc[as.numeric(dat$choice)==j,1] <- datps[as.numeric(dat$choice)==j,j+L]
	} else {
		dat$binRough.prob.loc[as.numeric(dat$choice)==j,1] <- datps[as.numeric(dat$choice)==j,j-L]
	}
}
toc()


print("ctree")
tic()
#----------------------------------------
# Tree estimation
#----------------------------------------
for (j in 1:(L*K)) {
	dat$tempdv <- as.factor(dat$choice==j)
	# print(paste0("Location ",j,": ",levels(dat$tempdv)))
	if (j<=L) {
		dat$tempBornHere <- as.factor(dat$birthloc==j)
	} else {
		dat$tempBornHere <- as.factor(dat$birthloc==(j-L))
	}
	# print(sum(dat$choice==j))
	
	# perform the tree classification algorithm
	model.ctree <- ctree(tempdv~tempBornHere+major+black+married+kids+migShifter+occShifter,data=dat,control=ctree_control(minbucket=50,mincriterion=0.95))
	if ((j==jay) & (sim==1)) {
		# export the tree to EPS file
		setEPS()
		postscript(paste0("MCtree",j,".eps"))
		plot(model.ctree)
		dev.off()
	}
	# predict class probabilities
	preddy <- predict(model.ctree,type="prob")
	dat[, c(paste0("pTree",j))] <- as.matrix(preddy[,c("TRUE")])
}

# print(head(dat))

dat$denom <- rowSums(dat[ ,c(paste0("pTree", seq(1,(L*K))))], dims=1)
dat[ ,c(paste0("pTree", seq(1,(L*K))),"denom")] <- dat[ ,c(paste0("pTree", seq(1,(L*K))),"denom")]/dat$denom
datps <- dat[,c(paste0("pTree", seq(1,(L*K))))]

# Create predicted location for classification performance measurement
dat$predchoice <- apply(dat[,c(paste0("pTree", seq(1,(L*K))))], 1, which.max)

# Calculate prediction accuracy
# print(class(dat$predchoice))
# print(class(dat$choice))
# print(count(dat, 'choice'))
# print(count(dat, 'predchoice'))
tester <-count(dat, 'predchoice')
dat$predchoice <- as.factor(dat$predchoice)
fits[sim,3]<- confusionMatrix(dat$predchoice,dat$choice)$overall[1]
fits[sim,4]<- confusionMatrix(dat$predchoice,dat$choice)$overall[2]

# print(head(datps))
# print(head(dat))

dat$tree.prob.first.best <- matrix(data=0,nrow=dim(dat)[1],ncol=1)
dat$tree.prob.loc        <- matrix(data=0,nrow=dim(dat)[1],ncol=1)

for (j in 1:(L*K)) {
	dat$tree.prob.first.best[as.numeric(dat$choice)==j,1] <- datps[as.numeric(dat$choice)==j,j]
	if (j<=L) {
		dat$tree.prob.loc[as.numeric(dat$choice)==j,1] <- datps[as.numeric(dat$choice)==j,j+L]
	} else {
		dat$tree.prob.loc[as.numeric(dat$choice)==j,1] <- datps[as.numeric(dat$choice)==j,j-L]
	}
}
toc()

print("logit")
tic()
#----------------------------------------
# Logit estimation
#----------------------------------------
for (j in 1:(L*K)) {
	dat$tempdv <- as.factor(dat$choice==j)
	# print(paste0("Location ",j,": ",levels(dat$tempdv)))
	if (j<=L) {
		dat$tempBornHere <- as.factor(dat$birthloc==j)
	} else {
		dat$tempBornHere <- as.factor(dat$birthloc==(j-L))
	}
	# perform the logit classification algorithm
	model.logit <- glm(tempdv~tempBornHere+(major+black+married+kids+migShifter+occShifter)^2+migShifter^2+occShifter^2,family=binomial(link="logit"),data=dat)
	if ((j==jay) & (sim==1)) {
		print(summary(model.logit))
	}
	# predict class probabilities
	preddy <- predict(model.logit,newdata=dat,type="response")
	dat[, c(paste0("pLogit",j))] <- as.matrix(preddy)
}

# print(head(dat))

dat$denom <- rowSums(dat[ ,c(paste0("pLogit", seq(1,(L*K))))], dims=1)
dat[ ,c(paste0("pLogit", seq(1,(L*K))),"denom")] <- dat[ ,c(paste0("pLogit", seq(1,(L*K))),"denom")]/dat$denom
datps <- dat[,c(paste0("pLogit", seq(1,(L*K))))]

# Create predicted location for classification performance measurement
dat$predchoice <- apply(dat[,c(paste0("pLogit", seq(1,(L*K))))], 1, which.max)

# Calculate prediction accuracy
print(class(dat$predchoice))
print(class(dat$choice))
print(count(dat, 'choice'))
print(count(dat, 'predchoice'))
tester <-count(dat, 'predchoice')
dat$predchoice <- as.factor(dat$predchoice)
fits[sim,5]<- confusionMatrix(dat$predchoice,dat$choice)$overall[1]
fits[sim,6]<- confusionMatrix(dat$predchoice,dat$choice)$overall[2]

# print(head(datps))
# print(head(dat))

dat$logit.prob.first.best <- matrix(data=0,nrow=dim(dat)[1],ncol=1)
dat$logit.prob.loc        <- matrix(data=0,nrow=dim(dat)[1],ncol=1)

for (j in 1:(L*K)) {
	dat$logit.prob.first.best[as.numeric(dat$choice)==j,1] <- datps[as.numeric(dat$choice)==j,j]
	if (j<=L) {
		dat$logit.prob.loc[as.numeric(dat$choice)==j,1] <- datps[as.numeric(dat$choice)==j,j+L]
	} else {
		dat$logit.prob.loc[as.numeric(dat$choice)==j,1] <- datps[as.numeric(dat$choice)==j,j-L]
	}
}
toc()

# print("NB")
# #----------------------------------------
# # Naive Bayes estimation
# #----------------------------------------
# for (j in 1:(L*K)) {
	# dat$tempdv <- as.factor(dat$choice==j)
	# # print(paste0("Location ",j,": ",levels(dat$tempdv)))
	# if (j<=L) {
		# dat$tempBornHere <- as.factor(dat$birthloc==j)
	# } else {
		# dat$tempBornHere <- as.factor(dat$birthloc==(j-L))
	# }
	
	# # perform the NB classification algorithm
	# tic()
	# print(sapply(dat[,c("tempdv","tempBornHere","major","black","married","kids","migShifter","occShifter")],class))
	# model.nb <- naiveBayes(tempdv~tempBornHere+major+black+married+kids+migShifter+occShifter,data=dat)
	# toc()
	# # predict class probabilities
	# tic()
	# preddy <- predict(model.nb,newdata=dat,type="raw")
	# toc()
	# tic()
	# dat[, c(paste0("pNB",j))] <- as.matrix(preddy[,c("TRUE")])
	# toc()
# }


# dat$denom <- rowSums(dat[ ,c(paste0("pNB", seq(1,(L*K))))], dims=1)
# dat[ ,c(paste0("pNB", seq(1,(L*K))),"denom")] <- dat[ ,c(paste0("pNB", seq(1,(L*K))),"denom")]/dat$denom
# datps <- dat[,c(paste0("pNB", seq(1,(L*K))))]

# # Create predicted location for classification performance measurement
# dat$predchoice <- apply(dat[,c(paste0("pNB", seq(1,(L*K))))], 1, which.max)

# # Calculate prediction accuracy
# print(class(dat$predchoice))
# print(class(dat$choice))
# print(count(dat, 'choice'))
# print(count(dat, 'predchoice'))
# tester <-count(dat, 'predchoice')
# dat$predchoice <- as.factor(dat$predchoice)
# fits[sim,7]<- confusionMatrix(dat$predchoice,dat$choice)$overall[1]
# fits[sim,8]<- confusionMatrix(dat$predchoice,dat$choice)$overall[2]

# dat$nb.prob.first.best <- matrix(data=0,nrow=dim(dat)[1],ncol=1)
# dat$nb.prob.loc        <- matrix(data=0,nrow=dim(dat)[1],ncol=1)

# for (j in 1:(L*K)) {
	# dat$nb.prob.first.best[as.numeric(dat$choice)==j,1] <- datps[as.numeric(dat$choice)==j,j]
	# if (j<=L) {
		# dat$nb.prob.loc[as.numeric(dat$choice)==j,1] <- datps[as.numeric(dat$choice)==j,j+L]
	# } else {
		# dat$nb.prob.loc[as.numeric(dat$choice)==j,1] <- datps[as.numeric(dat$choice)==j,j-L]
	# }
# }

# # print(head(dat[,c("nb.prob.first.best","nb.prob.loc")]))

varY[sim]   <- var(dat[dat$choice==jay,c("wage")])*(sum(dat$choice == jay)-1)
vareta[sim] <- var(dat[dat$choice==jay,c("eta")])

biased <- lm(wage    ~major+black+married+poly(exper,degree=3,raw=T),subset=choice    ==jay,data=dat)
coeffs[sim,1] <- summary(biased)$coef[2,1]
RMSEs[sim,1]  <- summary(biased)$sigma
devs[sim,1]   <- deviance(biased)
Ns[sim,1]     <- nobs(biased)

fixed  <- lm(wage    ~major+black+married+poly(exper,degree=3,raw=T)+poly(bin.prob.first.best,degree=3,raw=T),subset=choice    ==jay,data=dat)
coeffs[sim,2] <- summary(fixed)$coef[2,1]
RMSEs[sim,2]  <- summary(fixed)$sigma
devs[sim,2]   <- deviance(fixed)
Ns[sim,2]     <- nobs(fixed)

fixed  <- lm(wage    ~major+black+married+poly(exper,degree=3,raw=T)+poly(bin.prob.first.best,degree=4,raw=T),subset=choice    ==jay,data=dat)
coeffs[sim,3] <- summary(fixed)$coef[2,1]
RMSEs[sim,3]  <- summary(fixed)$sigma
devs[sim,3]   <- deviance(fixed)
Ns[sim,3]     <- nobs(fixed)

fixed  <- lm(wage    ~major+black+married+poly(exper,degree=3,raw=T)+poly(bin.prob.first.best,degree=3,raw=T)*poly(bin.prob.loc,degree=3,raw=T),subset=choice    ==jay,data=dat)
coeffs[sim,4] <- summary(fixed)$coef[2,1]
RMSEs[sim,4]  <- summary(fixed)$sigma
devs[sim,4]   <- deviance(fixed)
Ns[sim,4]     <- nobs(fixed)

fixed  <- lm(wage    ~major+black+married+poly(exper,degree=4,raw=T)+poly(bin.prob.first.best,degree=4,raw=T)*poly(bin.prob.loc,degree=4,raw=T),subset=choice    ==jay,data=dat)
coeffs[sim,5] <- summary(fixed)$coef[2,1]
RMSEs[sim,5]  <- summary(fixed)$sigma
devs[sim,5]   <- deviance(fixed)
Ns[sim,5]     <- nobs(fixed)

fixed  <- lm(wage    ~major+black+married+poly(exper,degree=3,raw=T)+poly(tree.prob.first.best,degree=3,raw=T),subset=choice    ==jay,data=dat)
coeffs[sim,6] <- summary(fixed)$coef[2,1]
RMSEs[sim,6]  <- summary(fixed)$sigma
devs[sim,6]   <- deviance(fixed)
Ns[sim,6]     <- nobs(fixed)

fixed  <- lm(wage    ~major+black+married+poly(exper,degree=4,raw=T)+poly(tree.prob.first.best,degree=4,raw=T),subset=choice    ==jay,data=dat)
coeffs[sim,7] <- summary(fixed)$coef[2,1]
RMSEs[sim,7]  <- summary(fixed)$sigma
devs[sim,7]   <- deviance(fixed)
Ns[sim,7]     <- nobs(fixed)

fixed  <- lm(wage    ~major+black+married+poly(exper,degree=3,raw=T)+poly(tree.prob.first.best,degree=3,raw=T)*poly(tree.prob.loc,degree=3,raw=T),subset=choice    ==jay,data=dat)
coeffs[sim,8] <- summary(fixed)$coef[2,1]
RMSEs[sim,8]  <- summary(fixed)$sigma
devs[sim,8]   <- deviance(fixed)
Ns[sim,8]     <- nobs(fixed)

fixed  <- lm(wage    ~major+black+married+poly(exper,degree=4,raw=T)+poly(tree.prob.first.best,degree=4,raw=T)*poly(tree.prob.loc,degree=4,raw=T),subset=choice    ==jay,data=dat)
coeffs[sim,9] <- summary(fixed)$coef[2,1]
RMSEs[sim,9]  <- summary(fixed)$sigma
devs[sim,9]   <- deviance(fixed)
Ns[sim,9]     <- nobs(fixed)

fixed  <- lm(wage    ~major+black+married+poly(exper,degree=3,raw=T)+poly(logit.prob.first.best,degree=3,raw=T),subset=choice    ==jay,data=dat)
coeffs[sim,10] <- summary(fixed)$coef[2,1]
RMSEs[sim,10]  <- summary(fixed)$sigma
devs[sim,10]   <- deviance(fixed)
Ns[sim,10]     <- nobs(fixed)

fixed  <- lm(wage    ~major+black+married+poly(exper,degree=4,raw=T)+poly(logit.prob.first.best,degree=4,raw=T),subset=choice    ==jay,data=dat)
coeffs[sim,11] <- summary(fixed)$coef[2,1]
RMSEs[sim,11]  <- summary(fixed)$sigma
devs[sim,11]   <- deviance(fixed)
Ns[sim,11]     <- nobs(fixed)

fixed  <- lm(wage    ~major+black+married+poly(exper,degree=3,raw=T)+poly(logit.prob.first.best,degree=3,raw=T)*poly(logit.prob.loc,degree=3,raw=T),subset=choice    ==jay,data=dat)
coeffs[sim,12] <- summary(fixed)$coef[2,1]
RMSEs[sim,12]  <- summary(fixed)$sigma
devs[sim,12]   <- deviance(fixed)
Ns[sim,12]     <- nobs(fixed)

fixed  <- lm(wage    ~major+black+married+poly(exper,degree=4,raw=T)+poly(logit.prob.first.best,degree=4,raw=T)*poly(logit.prob.loc,degree=4,raw=T),subset=choice    ==jay,data=dat)
coeffs[sim,13] <- summary(fixed)$coef[2,1]
RMSEs[sim,13]  <- summary(fixed)$sigma
devs[sim,13]   <- deviance(fixed)
Ns[sim,13]     <- nobs(fixed)

fixed  <- lm(wage    ~major+black+married+poly(exper,degree=3,raw=T)+poly(binRough.prob.first.best,degree=3,raw=T),subset=choice    ==jay,data=dat)
coeffs[sim,14] <- summary(fixed)$coef[2,1]
RMSEs[sim,14]  <- summary(fixed)$sigma
devs[sim,14]   <- deviance(fixed)
Ns[sim,14]     <- nobs(fixed)

fixed  <- lm(wage    ~major+black+married+poly(exper,degree=4,raw=T)+poly(binRough.prob.first.best,degree=4,raw=T),subset=choice    ==jay,data=dat)
coeffs[sim,15] <- summary(fixed)$coef[2,1]
RMSEs[sim,15]  <- summary(fixed)$sigma
devs[sim,15]   <- deviance(fixed)
Ns[sim,15]     <- nobs(fixed)

fixed  <- lm(wage    ~major+black+married+poly(exper,degree=3,raw=T)+poly(binRough.prob.first.best,degree=3,raw=T)*poly(binRough.prob.loc,degree=3,raw=T),subset=choice    ==jay,data=dat)
coeffs[sim,16] <- summary(fixed)$coef[2,1]
RMSEs[sim,16]  <- summary(fixed)$sigma
devs[sim,16]   <- deviance(fixed)
Ns[sim,16]     <- nobs(fixed)

fixed  <- lm(wage    ~major+black+married+poly(exper,degree=4,raw=T)+poly(binRough.prob.first.best,degree=4,raw=T)*poly(binRough.prob.loc,degree=4,raw=T),subset=choice    ==jay,data=dat)
coeffs[sim,17] <- summary(fixed)$coef[2,1]
RMSEs[sim,17]  <- summary(fixed)$sigma
devs[sim,17]   <- deviance(fixed)
Ns[sim,17]     <- nobs(fixed)

# fixed  <- lm(wage    ~major+black+married+poly(exper,degree=3,raw=T)+poly(tree.prob.first.best,tree.prob.loc,degree=3,raw=T),subset=choice    ==jay,data=dat)
# print(summary(fixed))
})

if (coeffs[sim,13]==0) {
	coeffs[sim, ] <- c(NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA)
	RMSEs[sim, ]  <- c(NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA)
    devs[sim, ]   <- c(NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA)
	Ns[sim, ]     <- c(NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA) 
	fits[sim, ]   <- c(NA,NA,NA,NA,NA,NA,NA,NA)
}  

save(varY,file="varY1e3.Rda")
save(vareta,file="vareta1e3.Rda")
save(coeffs,file="coeffs1e3.Rda")
save(RMSEs,file="RMSEs1e3.Rda")
save(devs,file="devs1e3.Rda")
save(Ns,file="Ns1e3.Rda")
save(fits,file="fits1e3.Rda")
}

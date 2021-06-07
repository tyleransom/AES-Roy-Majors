# Remove everything from the environment and clear out memories
rm(list = ls())
gc()

# Print library path
.libPaths()

# Require R version 3.3.0
ver<-R.Version()
if (as.numeric(ver$minor)<3) {
	#stop("Version incompatibility")
}

# Matrix Packages
require(abind)

# Statistic Packages
require(gtools)
require(doBy)
require(dummies)
require(foreign)
require(caret)
require(nnet)
require(stats)
require(lattice)
require(Matrix)
# require(HH) #vif
# require(quadprog) #Qudratic Programming: Constrained Regression
require(pastecs) #Summary Statistics
require(plyr) #Tabluation for factor variables
#require(sm) #Kernel density package
require(Hmisc) #Package to export matrices to LaTeX
require(texreg) #Package to export regression estimates to LaTeX
require(lmtest) #Lagrange multiplier test
require(ggplot2) #Plotting
require(ggthemes) #Plotting
require(reshape2) #data management
require(data.table) #data management

# Machine Learning Pacakges
# require(lars)
require(e1071) #SVM, Naive Bayes
require(klaR) #Naive Bayes w/kernel density estimation
# require(randomForest)
# require(gbm) 
# require(dismo) #Gradient Boosting Model
# require(ipred) #Bagging
# require(party) #Tree classification (including Breiman's random forests)
require(partykit) #Add-on for -party- to enable multivariate classification
require(rpart) #Tree regression
require(evd) # take draws from extreme value dist'n

# Sys.setenv(http_proxy="http://servername:80")
# list.of.packages <- c("party", "pastecs")
# new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
# if(length(new.packages)) install.packages(new.packages, repos='http://cran.us.r-project.org')

# custom functions to be used later
matequal <- function(x, y) {
	is.matrix(x) && is.matrix(y) && dim(x) == dim(y) && all(x == y)
}

build.ror <- function(final.rnames, name.map){
	keep <- final.rnames %in% names(name.map)
	mapper <- function(x){
	mp <- name.map[[x]] 
	ifelse(is.null(mp), x, mp)
	}
	newnames <- sapply(final.rnames, mapper)
	omit <- paste0(final.rnames[!keep], collapse="|")
	reorder <- na.omit(match(unlist(name.map), newnames[keep]))

	list(ccn=newnames, oc=omit, rc=reorder)
}

all.varnames.list <- function(model.list){
	mods <- texreg:::get.data(model.list)
	gofers <- texreg:::get.gof(mods)
	mm <- texreg:::aggregate.matrix(mods, gofers, digits=3)
	rownames(mm)
}

# Function: tic/toc function to time the execution
tic <- function(gcFirst = TRUE, type=c("elapsed", "user.self", "sys.self"))
{
  type <- match.arg(type)
  assign(".type", type, envir=baseenv())
  if(gcFirst) gc(FALSE)
  tic <- proc.time()[type]         
  assign(".tic", tic, envir=baseenv())
  invisible(tic)
}

toc <- function()
{
  type <- get(".type", envir=baseenv())
  toc <- proc.time()[type]
  tic <- get(".tic", envir=baseenv())
  print(toc - tic)
  invisible(toc)
}

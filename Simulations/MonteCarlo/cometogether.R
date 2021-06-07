# Remove everything from the environment and clear out memories
rm(list = ls())
gc()

require(xtable)

# Load baseline results (iid taste shocks, bin quartiles for occupation excl. restr.)
load('coeffs.Rda')
load('Ns.Rda')
load('fits.Rda')
load('RMSEs.Rda')

print(" ")
print("iid tastes, quartiles")
print(summary(coeffs[,c("OLS","bin1B3","tree1B3","logit1B3","binRough1B3","bin2B3","tree2B3","logit2B3","binRough2B3")]))
print(summary(Ns[,c("OLS","bin1B3","tree1B3","logit1B3","binRough1B3","bin2B3","tree2B3","logit2B3","binRough2B3")]))
print(summary(fits))
print(summary(RMSEs[,c("OLS","bin1B3","tree1B3","logit1B3","binRough1B3","bin2B3","tree2B3","logit2B3","binRough2B3")]))

# # Load median results (iid taste shocks, bin two-iles for occupation excl. restr.)
# load('coeffsMed.Rda')
# load('NsMed.Rda')
# load('fitsMed.Rda')
# load('RMSEsMed.Rda')

# print(" ")
# print("iid tastes, two-iles")
# print(summary(coeffs[,c("OLS","bin1B3","tree1B3","logit1B3","binRough1B3","bin2B3","tree2B3","logit2B3","binRough2B3")]))
# print(summary(Ns[,c("OLS","bin1B3","tree1B3","logit1B3","binRough1B3","bin2B3","tree2B3","logit2B3","binRough2B3")]))
# print(summary(fits))
# print(summary(RMSEs[,c("OLS","bin1B3","tree1B3","logit1B3","binRough1B3","bin2B3","tree2B3","logit2B3","binRough2B3")]))

# Load mvn results (bin quartiles for occupation excl. restr.)
load('coeffsCorrEps.Rda')
load('NsCorrEps.Rda')
load('fitsCorrEps.Rda')
load('RMSEsCorrEps.Rda')

print(" ")
print("corr tastes, quartiles")
print(summary(coeffs[,c("OLS","bin1B3","tree1B3","logit1B3","binRough1B3","bin2B3","tree2B3","logit2B3","binRough2B3")]))
print(summary(Ns[,c("OLS","bin1B3","tree1B3","logit1B3","binRough1B3","bin2B3","tree2B3","logit2B3","binRough2B3")]))
print(summary(fits))
print(summary(RMSEs[,c("OLS","bin1B3","tree1B3","logit1B3","binRough1B3","bin2B3","tree2B3","logit2B3","binRough2B3")]))

# # Load mvn median results (bin two-iles for occupation excl. restr.)
# load('coeffsMedCorrEps.Rda')
# load('NsMedCorrEps.Rda')
# load('fitsMedCorrEps.Rda')
# load('RMSEsMedCorrEps.Rda')

# print(" ")
# print("corr tastes, two-iles")
# print(summary(coeffs[,c("OLS","bin1B3","tree1B3","logit1B3","binRough1B3","bin2B3","tree2B3","logit2B3","binRough2B3")]))
# print(summary(Ns[,c("OLS","bin1B3","tree1B3","logit1B3","binRough1B3","bin2B3","tree2B3","logit2B3","binRough2B3")]))
# print(summary(fits))
# print(summary(RMSEs[,c("OLS","bin1B3","tree1B3","logit1B3","binRough1B3","bin2B3","tree2B3","logit2B3","binRough2B3")]))

# Load baseline results (iid taste shocks, bin quartiles for occupation excl. restr.)
load('coeffs1e3.Rda')
load('Ns1e3.Rda')
load('fits1e3.Rda')
load('RMSEs1e3.Rda')

print(" ")
print("iid tastes, quartiles")
print(summary(coeffs[,c("OLS","bin1B3","tree1B3","logit1B3","binRough1B3","bin2B3","tree2B3","logit2B3","binRough2B3")]))
print(summary(Ns[,c("OLS","bin1B3","tree1B3","logit1B3","binRough1B3","bin2B3","tree2B3","logit2B3","binRough2B3")]))
print(summary(fits))
print(summary(RMSEs[,c("OLS","bin1B3","tree1B3","logit1B3","binRough1B3","bin2B3","tree2B3","logit2B3","binRough2B3")]))

# # Load median results (iid taste shocks, bin two-iles for occupation excl. restr.)
# load('coeffs1e3Med.Rda')
# load('Ns1e3Med.Rda')
# load('fits1e3Med.Rda')
# load('RMSEs1e3Med.Rda')

# print(" ")
# print("iid tastes, two-iles")
# print(summary(coeffs[,c("OLS","bin1B3","tree1B3","logit1B3","binRough1B3","bin2B3","tree2B3","logit2B3","binRough2B3")]))
# print(summary(Ns[,c("OLS","bin1B3","tree1B3","logit1B3","binRough1B3","bin2B3","tree2B3","logit2B3","binRough2B3")]))
# print(summary(fits))
# print(summary(RMSEs[,c("OLS","bin1B3","tree1B3","logit1B3","binRough1B3","bin2B3","tree2B3","logit2B3","binRough2B3")]))

# Load mvn results (bin quartiles for occupation excl. restr.)
load('coeffs1e3CorrEps.Rda')
load('Ns1e3CorrEps.Rda')
load('fits1e3CorrEps.Rda')
load('RMSEs1e3CorrEps.Rda')

print(" ")
print("corr tastes, quartiles")
print(summary(coeffs[,c("OLS","bin1B3","tree1B3","logit1B3","binRough1B3","bin2B3","tree2B3","logit2B3","binRough2B3")]))
print(summary(Ns[,c("OLS","bin1B3","tree1B3","logit1B3","binRough1B3","bin2B3","tree2B3","logit2B3","binRough2B3")]))
print(summary(fits))
print(summary(RMSEs[,c("OLS","bin1B3","tree1B3","logit1B3","binRough1B3","bin2B3","tree2B3","logit2B3","binRough2B3")]))

# Load mvn median results (bin two-iles for occupation excl. restr.)
# load('coeffs1e3MedCorrEps.Rda')
# load('Ns1e3MedCorrEps.Rda')
# load('fits1e3MedCorrEps.Rda')
# load('RMSEs1e3MedCorrEps.Rda')

# print(" ")
# print("corr tastes, two-iles")
# print(summary(coeffs[,c("OLS","bin1B3","tree1B3","logit1B3","binRough1B3","bin2B3","tree2B3","logit2B3","binRough2B3")]))
# print(summary(Ns[,c("OLS","bin1B3","tree1B3","logit1B3","binRough1B3","bin2B3","tree2B3","logit2B3","binRough2B3")]))
# print(summary(fits))
# print(summary(RMSEs[,c("OLS","bin1B3","tree1B3","logit1B3","binRough1B3","bin2B3","tree2B3","logit2B3","binRough2B3")]))

# Load mvn results (bin quartiles for occupation excl. restr.)
load('coeffs1e3CorrEpsCorrEta.Rda')
load('Ns1e3CorrEpsCorrEta.Rda')
load('fits1e3CorrEpsCorrEta.Rda')
load('RMSEs1e3CorrEpsCorrEta.Rda')

print(" ")
print("corr tastes corr wages, quartiles")
print(summary(coeffs[,c("OLS","bin1B3","tree1B3","logit1B3","binRough1B3","bin2B3","tree2B3","logit2B3","binRough2B3")]))
print(summary(Ns[,c("OLS","bin1B3","tree1B3","logit1B3","binRough1B3","bin2B3","tree2B3","logit2B3","binRough2B3")]))
print(summary(fits))
print(summary(RMSEs[,c("OLS","bin1B3","tree1B3","logit1B3","binRough1B3","bin2B3","tree2B3","logit2B3","binRough2B3")]))

# Create matrix of results
baseline   <- data.frame(matrix(0,ncol=8,nrow=9))
corrEps    <- data.frame(matrix(0,ncol=8,nrow=9))
corrEpsEta <- data.frame(matrix(0,ncol=8,nrow=9))
T1EV       <- data.frame(matrix(0,ncol=8,nrow=9))
FE         <- data.frame(matrix(0,ncol=8,nrow=9))
row.names(baseline  ) <- c("[\\]quad OLS","[\\]quad 1st Best Bin","[\\]quad 1st Best Tree","[\\]quad 1st Best Logit","[\\]quad 1st Best Coarse Bin","[\\]quad 1st+2nd Best Bin","[\\]quad 1st+2nd Best Tree","[\\]quad 1st+2nd Best Logit","[\\]quad 1st+2nd Best Coarse Bin")
row.names(corrEps   ) <- c("[\\]quad OLS","[\\]quad 1st Best Bin","[\\]quad 1st Best Tree","[\\]quad 1st Best Logit","[\\]quad 1st Best Coarse Bin","[\\]quad 1st+2nd Best Bin","[\\]quad 1st+2nd Best Tree","[\\]quad 1st+2nd Best Logit","[\\]quad 1st+2nd Best Coarse Bin")
row.names(corrEpsEta) <- c("[\\]quad OLS","[\\]quad 1st Best Bin","[\\]quad 1st Best Tree","[\\]quad 1st Best Logit","[\\]quad 1st Best Coarse Bin","[\\]quad 1st+2nd Best Bin","[\\]quad 1st+2nd Best Tree","[\\]quad 1st+2nd Best Logit","[\\]quad 1st+2nd Best Coarse Bin")
row.names(T1EV)       <- c("[\\]quad OLS","[\\]quad 1st Best Bin","[\\]quad 1st Best Tree","[\\]quad 1st Best Logit","[\\]quad 1st Best Coarse Bin","[\\]quad 1st+2nd Best Bin","[\\]quad 1st+2nd Best Tree","[\\]quad 1st+2nd Best Logit","[\\]quad 1st+2nd Best Coarse Bin")
row.names(FE)         <- c("[\\]quad OLS","[\\]quad 1st Best Bin","[\\]quad 1st Best Tree","[\\]quad 1st Best Logit","[\\]quad 1st Best Coarse Bin","[\\]quad 1st+2nd Best Bin","[\\]quad 1st+2nd Best Tree","[\\]quad 1st+2nd Best Logit","[\\]quad 1st+2nd Best Coarse Bin")


#-----------------------
# Baseline simulation
#-----------------------
load('coeffs.Rda')
load('RMSEs.Rda')
load('Ns.Rda')
# OLS
baseline[1,1] = mean(coeffs[,c("OLS")], na.rm=TRUE)-2
baseline[1,2] = sd(  coeffs[,c("OLS")], na.rm=TRUE)
baseline[1,3] = mean( RMSEs[,c("OLS")], na.rm=TRUE)
baseline[1,4] = round(mean(    Ns[,c("OLS")], na.rm=TRUE), digits=0)
# Bin 1B
baseline[2,1] = mean(coeffs[,c("bin1B3")], na.rm=TRUE)-2
baseline[2,2] = sd(  coeffs[,c("bin1B3")], na.rm=TRUE)
baseline[2,3] = mean( RMSEs[,c("bin1B3")], na.rm=TRUE)
baseline[2,4] = "" #mean(    Ns[,c("bin1B3")], na.rm=TRUE)
# Tree 1B
baseline[3,1] = mean(coeffs[,c("tree1B3")], na.rm=TRUE)-2
baseline[3,2] = sd(  coeffs[,c("tree1B3")], na.rm=TRUE)
baseline[3,3] = mean( RMSEs[,c("tree1B3")], na.rm=TRUE)
baseline[3,4] = "" #mean(    Ns[,c("tree1B3")], na.rm=TRUE)
# Logit 1B
baseline[4,1] = mean(coeffs[,c("logit1B3")], na.rm=TRUE)-2
baseline[4,2] = sd(  coeffs[,c("logit1B3")], na.rm=TRUE)
baseline[4,3] = mean( RMSEs[,c("logit1B3")], na.rm=TRUE)
baseline[4,4] = "" #mean(    Ns[,c("logit1B3")], na.rm=TRUE)
# Bin Coarse 1B
baseline[5,1] = mean(coeffs[,c("binRough1B3")], na.rm=TRUE)-2
baseline[5,2] = sd(  coeffs[,c("binRough1B3")], na.rm=TRUE)
baseline[5,3] = mean( RMSEs[,c("binRough1B3")], na.rm=TRUE)
baseline[5,4] = "" #mean(    Ns[,c("binRough1B3")], na.rm=TRUE)
# Bin 2B
baseline[6,1] = mean(coeffs[,c("bin2B3")], na.rm=TRUE)-2
baseline[6,2] = sd(  coeffs[,c("bin2B3")], na.rm=TRUE)
baseline[6,3] = mean( RMSEs[,c("bin2B3")], na.rm=TRUE)
baseline[6,4] = "" #mean(    Ns[,c("bin2B3")], na.rm=TRUE)
# Tree 2B
baseline[7,1] = mean(coeffs[,c("tree2B3")], na.rm=TRUE)-2
baseline[7,2] = sd(  coeffs[,c("tree2B3")], na.rm=TRUE)
baseline[7,3] = mean( RMSEs[,c("tree2B3")], na.rm=TRUE)
baseline[7,4] = "" #mean(    Ns[,c("tree2B3")], na.rm=TRUE)
# Logit 2B
baseline[8,1] = mean(coeffs[,c("logit2B3")], na.rm=TRUE)-2
baseline[8,2] = sd(  coeffs[,c("logit2B3")], na.rm=TRUE)
baseline[8,3] = mean( RMSEs[,c("logit2B3")], na.rm=TRUE)
baseline[8,4] = "" #mean(    Ns[,c("logit2B3")], na.rm=TRUE)
# Bin Coarse 2B
baseline[9,1] = mean(coeffs[,c("binRough2B3")], na.rm=TRUE)-2
baseline[9,2] = sd(  coeffs[,c("binRough2B3")], na.rm=TRUE)
baseline[9,3] = mean( RMSEs[,c("binRough2B3")], na.rm=TRUE)
baseline[9,4] = "" #mean(    Ns[,c("binRough2B3")], na.rm=TRUE)
print(baseline)

load('coeffs1e3.Rda')
load('RMSEs1e3.Rda')
load('Ns1e3.Rda')
# OLS
baseline[1,5] = mean(coeffs[,c("OLS")], na.rm=TRUE)-2
baseline[1,6] = sd(  coeffs[,c("OLS")], na.rm=TRUE)
baseline[1,7] = mean( RMSEs[,c("OLS")], na.rm=TRUE)
baseline[1,8] = round(mean(    Ns[,c("OLS")], na.rm=TRUE), digits=0)
# Bin 1B
baseline[2,5] = mean(coeffs[,c("bin1B3")], na.rm=TRUE)-2
baseline[2,6] = sd(  coeffs[,c("bin1B3")], na.rm=TRUE)
baseline[2,7] = mean( RMSEs[,c("bin1B3")], na.rm=TRUE)
baseline[2,8] = "" #mean(    Ns[,c("bin1B3")], na.rm=TRUE)
# Tree 1B
baseline[3,5] = mean(coeffs[,c("tree1B3")], na.rm=TRUE)-2
baseline[3,6] = sd(  coeffs[,c("tree1B3")], na.rm=TRUE)
baseline[3,7] = mean( RMSEs[,c("tree1B3")], na.rm=TRUE)
baseline[3,8] = "" #mean(    Ns[,c("tree1B3")], na.rm=TRUE)
# Logit 1B
baseline[4,5] = mean(coeffs[,c("logit1B3")], na.rm=TRUE)-2
baseline[4,6] = sd(  coeffs[,c("logit1B3")], na.rm=TRUE)
baseline[4,7] = mean( RMSEs[,c("logit1B3")], na.rm=TRUE)
baseline[4,8] = "" #mean(    Ns[,c("logit1B3")], na.rm=TRUE)
# Bin Coarse 1B
baseline[5,5] = mean(coeffs[,c("binRough1B3")], na.rm=TRUE)-2
baseline[5,6] = sd(  coeffs[,c("binRough1B3")], na.rm=TRUE)
baseline[5,7] = mean( RMSEs[,c("binRough1B3")], na.rm=TRUE)
baseline[5,8] = "" #mean(    Ns[,c("binRough1B3")], na.rm=TRUE)
# Bin 2B
baseline[6,5] = mean(coeffs[,c("bin2B3")], na.rm=TRUE)-2
baseline[6,6] = sd(  coeffs[,c("bin2B3")], na.rm=TRUE)
baseline[6,7] = mean( RMSEs[,c("bin2B3")], na.rm=TRUE)
baseline[6,8] = "" #mean(    Ns[,c("bin2B3")], na.rm=TRUE)
# Tree 2B
baseline[7,5] = mean(coeffs[,c("tree2B3")], na.rm=TRUE)-2
baseline[7,6] = sd(  coeffs[,c("tree2B3")], na.rm=TRUE)
baseline[7,7] = mean( RMSEs[,c("tree2B3")], na.rm=TRUE)
baseline[7,8] = "" #mean(    Ns[,c("tree2B3")], na.rm=TRUE)
# Logit 2B
baseline[8,5] = mean(coeffs[,c("logit2B3")], na.rm=TRUE)-2
baseline[8,6] = sd(  coeffs[,c("logit2B3")], na.rm=TRUE)
baseline[8,7] = mean( RMSEs[,c("logit2B3")], na.rm=TRUE)
baseline[8,8] = "" #mean(    Ns[,c("logit2B3")], na.rm=TRUE)
# Bin Coarse 2B
baseline[9,5] = mean(coeffs[,c("binRough2B3")], na.rm=TRUE)-2
baseline[9,6] = sd(  coeffs[,c("binRough2B3")], na.rm=TRUE)
baseline[9,7] = mean( RMSEs[,c("binRough2B3")], na.rm=TRUE)
baseline[9,8] = "" #mean(    Ns[,c("binRough2B3")], na.rm=TRUE)


#-----------------------
# Correlated pref shocks
#-----------------------
load('coeffsCorrEps.Rda')
load('RMSEsCorrEps.Rda')
load('NsCorrEps.Rda')
# OLS
corrEps[1,1] = mean(coeffs[,c("OLS")], na.rm=TRUE)-2
corrEps[1,2] = sd(  coeffs[,c("OLS")], na.rm=TRUE)
corrEps[1,3] = mean( RMSEs[,c("OLS")], na.rm=TRUE)
corrEps[1,4] = round(mean(    Ns[,c("OLS")], na.rm=TRUE),digits=0)
# Bin 1B
corrEps[2,1] = mean(coeffs[,c("bin1B3")], na.rm=TRUE)-2
corrEps[2,2] = sd(  coeffs[,c("bin1B3")], na.rm=TRUE)
corrEps[2,3] = mean( RMSEs[,c("bin1B3")], na.rm=TRUE)
corrEps[2,4] = "" #mean(    Ns[,c("bin1B3")], na.rm=TRUE)
# Tree 1B
corrEps[3,1] = mean(coeffs[,c("tree1B3")], na.rm=TRUE)-2
corrEps[3,2] = sd(  coeffs[,c("tree1B3")], na.rm=TRUE)
corrEps[3,3] = mean( RMSEs[,c("tree1B3")], na.rm=TRUE)
corrEps[3,4] = "" #mean(    Ns[,c("tree1B3")], na.rm=TRUE)
# Logit 1B
corrEps[4,1] = mean(coeffs[,c("logit1B3")], na.rm=TRUE)-2
corrEps[4,2] = sd(  coeffs[,c("logit1B3")], na.rm=TRUE)
corrEps[4,3] = mean( RMSEs[,c("logit1B3")], na.rm=TRUE)
corrEps[4,4] = "" #mean(    Ns[,c("logit1B3")], na.rm=TRUE)
# Bin Coarse 1B
corrEps[5,1] = mean(coeffs[,c("binRough1B3")], na.rm=TRUE)-2
corrEps[5,2] = sd(  coeffs[,c("binRough1B3")], na.rm=TRUE)
corrEps[5,3] = mean( RMSEs[,c("binRough1B3")], na.rm=TRUE)
corrEps[5,4] = "" #mean(    Ns[,c("binRough1B3")], na.rm=TRUE)
# Bin 2B
corrEps[6,1] = mean(coeffs[,c("bin2B3")], na.rm=TRUE)-2
corrEps[6,2] = sd(  coeffs[,c("bin2B3")], na.rm=TRUE)
corrEps[6,3] = mean( RMSEs[,c("bin2B3")], na.rm=TRUE)
corrEps[6,4] = "" #mean(    Ns[,c("bin2B3")], na.rm=TRUE)
# Tree 2B
corrEps[7,1] = mean(coeffs[,c("tree2B3")], na.rm=TRUE)-2
corrEps[7,2] = sd(  coeffs[,c("tree2B3")], na.rm=TRUE)
corrEps[7,3] = mean( RMSEs[,c("tree2B3")], na.rm=TRUE)
corrEps[7,4] = "" #mean(    Ns[,c("tree2B3")], na.rm=TRUE)
# Logit 2B
corrEps[8,1] = mean(coeffs[,c("logit2B3")], na.rm=TRUE)-2
corrEps[8,2] = sd(  coeffs[,c("logit2B3")], na.rm=TRUE)
corrEps[8,3] = mean( RMSEs[,c("logit2B3")], na.rm=TRUE)
corrEps[8,4] = "" #mean(    Ns[,c("logit2B3")], na.rm=TRUE)
# Bin Coarse 2B
corrEps[9,1] = mean(coeffs[,c("binRough2B3")], na.rm=TRUE)-2
corrEps[9,2] = sd(  coeffs[,c("binRough2B3")], na.rm=TRUE)
corrEps[9,3] = mean( RMSEs[,c("binRough2B3")], na.rm=TRUE)
corrEps[9,4] = "" #mean(    Ns[,c("binRough2B3")], na.rm=TRUE)

load('coeffs1e3CorrEps.Rda')
load('RMSEs1e3CorrEps.Rda')
load('Ns1e3CorrEps.Rda')
# OLS
corrEps[1,5] = mean(coeffs[,c("OLS")], na.rm=TRUE)-2
corrEps[1,6] = sd(  coeffs[,c("OLS")], na.rm=TRUE)
corrEps[1,7] = mean( RMSEs[,c("OLS")], na.rm=TRUE)
corrEps[1,8] = round(mean(    Ns[,c("OLS")], na.rm=TRUE), digits=0)
# Bin 1B
corrEps[2,5] = mean(coeffs[,c("bin1B3")], na.rm=TRUE)-2
corrEps[2,6] = sd(  coeffs[,c("bin1B3")], na.rm=TRUE)
corrEps[2,7] = mean( RMSEs[,c("bin1B3")], na.rm=TRUE)
corrEps[2,8] = "" #mean(    Ns[,c("bin1B3")], na.rm=TRUE)
# Tree 1B
corrEps[3,5] = mean(coeffs[,c("tree1B3")], na.rm=TRUE)-2
corrEps[3,6] = sd(  coeffs[,c("tree1B3")], na.rm=TRUE)
corrEps[3,7] = mean( RMSEs[,c("tree1B3")], na.rm=TRUE)
corrEps[3,8] = "" #mean(    Ns[,c("tree1B3")], na.rm=TRUE)
# Logit 1B
corrEps[4,5] = mean(coeffs[,c("logit1B3")], na.rm=TRUE)-2
corrEps[4,6] = sd(  coeffs[,c("logit1B3")], na.rm=TRUE)
corrEps[4,7] = mean( RMSEs[,c("logit1B3")], na.rm=TRUE)
corrEps[4,8] = "" #mean(    Ns[,c("logit1B3")], na.rm=TRUE)
# Bin Coarse 1B
corrEps[5,5] = mean(coeffs[,c("binRough1B3")], na.rm=TRUE)-2
corrEps[5,6] = sd(  coeffs[,c("binRough1B3")], na.rm=TRUE)
corrEps[5,7] = mean( RMSEs[,c("binRough1B3")], na.rm=TRUE)
corrEps[5,8] = "" #mean(    Ns[,c("binRough1B3")], na.rm=TRUE)
# Bin 2B
corrEps[6,5] = mean(coeffs[,c("bin2B3")], na.rm=TRUE)-2
corrEps[6,6] = sd(  coeffs[,c("bin2B3")], na.rm=TRUE)
corrEps[6,7] = mean( RMSEs[,c("bin2B3")], na.rm=TRUE)
corrEps[6,8] = "" #mean(    Ns[,c("bin2B3")], na.rm=TRUE)
# Tree 2B
corrEps[7,5] = mean(coeffs[,c("tree2B3")], na.rm=TRUE)-2
corrEps[7,6] = sd(  coeffs[,c("tree2B3")], na.rm=TRUE)
corrEps[7,7] = mean( RMSEs[,c("tree2B3")], na.rm=TRUE)
corrEps[7,8] = "" #mean(    Ns[,c("tree2B3")], na.rm=TRUE)
# Logit 2B
corrEps[8,5] = mean(coeffs[,c("logit2B3")], na.rm=TRUE)-2
corrEps[8,6] = sd(  coeffs[,c("logit2B3")], na.rm=TRUE)
corrEps[8,7] = mean( RMSEs[,c("logit2B3")], na.rm=TRUE)
corrEps[8,8] = "" #mean(    Ns[,c("logit2B3")], na.rm=TRUE)
# Bin Coarse 2B
corrEps[9,5] = mean(coeffs[,c("binRough2B3")], na.rm=TRUE)-2
corrEps[9,6] = sd(  coeffs[,c("binRough2B3")], na.rm=TRUE)
corrEps[9,7] = mean( RMSEs[,c("binRough2B3")], na.rm=TRUE)
corrEps[9,8] = "" #mean(    Ns[,c("binRough2B3")], na.rm=TRUE)


#-----------------------
# Correlated pref shocks and earnings shocks
#-----------------------
load('coeffsCorrEpsCorrEta.Rda')
load('RMSEsCorrEpsCorrEta.Rda')
load('NsCorrEpsCorrEta.Rda')
# OLS
corrEpsEta[1,1] = mean(coeffs[,c("OLS")], na.rm=TRUE)-2
corrEpsEta[1,2] = sd(  coeffs[,c("OLS")], na.rm=TRUE)
corrEpsEta[1,3] = mean( RMSEs[,c("OLS")], na.rm=TRUE)
corrEpsEta[1,4] = round(mean(    Ns[,c("OLS")], na.rm=TRUE), digits=0)
# Bin 1B
corrEpsEta[2,1] = mean(coeffs[,c("bin1B3")], na.rm=TRUE)-2
corrEpsEta[2,2] = sd(  coeffs[,c("bin1B3")], na.rm=TRUE)
corrEpsEta[2,3] = mean( RMSEs[,c("bin1B3")], na.rm=TRUE)
corrEpsEta[2,4] = "" #mean(    Ns[,c("bin1B3")], na.rm=TRUE)
# Tree 1B
corrEpsEta[3,1] = mean(coeffs[,c("tree1B3")], na.rm=TRUE)-2
corrEpsEta[3,2] = sd(  coeffs[,c("tree1B3")], na.rm=TRUE)
corrEpsEta[3,3] = mean( RMSEs[,c("tree1B3")], na.rm=TRUE)
corrEpsEta[3,4] = "" #mean(    Ns[,c("tree1B3")], na.rm=TRUE)
# Logit 1B
corrEpsEta[4,1] = mean(coeffs[,c("logit1B3")], na.rm=TRUE)-2
corrEpsEta[4,2] = sd(  coeffs[,c("logit1B3")], na.rm=TRUE)
corrEpsEta[4,3] = mean( RMSEs[,c("logit1B3")], na.rm=TRUE)
corrEpsEta[4,4] = "" #mean(    Ns[,c("logit1B3")], na.rm=TRUE)
# Bin Coarse 1B
corrEpsEta[5,1] = mean(coeffs[,c("binRough1B3")], na.rm=TRUE)-2
corrEpsEta[5,2] = sd(  coeffs[,c("binRough1B3")], na.rm=TRUE)
corrEpsEta[5,3] = mean( RMSEs[,c("binRough1B3")], na.rm=TRUE)
corrEpsEta[5,4] = "" #mean(    Ns[,c("binRough1B3")], na.rm=TRUE)
# Bin 2B
corrEpsEta[6,1] = mean(coeffs[,c("bin2B3")], na.rm=TRUE)-2
corrEpsEta[6,2] = sd(  coeffs[,c("bin2B3")], na.rm=TRUE)
corrEpsEta[6,3] = mean( RMSEs[,c("bin2B3")], na.rm=TRUE)
corrEpsEta[6,4] = "" #mean(    Ns[,c("bin2B3")], na.rm=TRUE)
# Tree 2B
corrEpsEta[7,1] = mean(coeffs[,c("tree2B3")], na.rm=TRUE)-2
corrEpsEta[7,2] = sd(  coeffs[,c("tree2B3")], na.rm=TRUE)
corrEpsEta[7,3] = mean( RMSEs[,c("tree2B3")], na.rm=TRUE)
corrEpsEta[7,4] = "" #mean(    Ns[,c("tree2B3")], na.rm=TRUE)
# Logit 2B
corrEpsEta[8,1] = mean(coeffs[,c("logit2B3")], na.rm=TRUE)-2
corrEpsEta[8,2] = sd(  coeffs[,c("logit2B3")], na.rm=TRUE)
corrEpsEta[8,3] = mean( RMSEs[,c("logit2B3")], na.rm=TRUE)
corrEpsEta[8,4] = "" #mean(    Ns[,c("logit2B3")], na.rm=TRUE)
# Bin Coarse 2B
corrEpsEta[9,1] = mean(coeffs[,c("binRough2B3")], na.rm=TRUE)-2
corrEpsEta[9,2] = sd(  coeffs[,c("binRough2B3")], na.rm=TRUE)
corrEpsEta[9,3] = mean( RMSEs[,c("binRough2B3")], na.rm=TRUE)
corrEpsEta[9,4] = "" #mean(    Ns[,c("binRough2B3")], na.rm=TRUE)

load('coeffs1e3CorrEpsCorrEta.Rda')
load('RMSEs1e3CorrEpsCorrEta.Rda')
load('Ns1e3CorrEpsCorrEta.Rda')
# OLS
corrEpsEta[1,5] = mean(coeffs[,c("OLS")], na.rm=TRUE)-2
corrEpsEta[1,6] = sd(  coeffs[,c("OLS")], na.rm=TRUE)
corrEpsEta[1,7] = mean( RMSEs[,c("OLS")], na.rm=TRUE)
corrEpsEta[1,8] = round(mean(    Ns[,c("OLS")], na.rm=TRUE), digits=0)
# Bin 1B
corrEpsEta[2,5] = mean(coeffs[,c("bin1B3")], na.rm=TRUE)-2
corrEpsEta[2,6] = sd(  coeffs[,c("bin1B3")], na.rm=TRUE)
corrEpsEta[2,7] = mean( RMSEs[,c("bin1B3")], na.rm=TRUE)
corrEpsEta[2,8] = "" #mean(    Ns[,c("bin1B3")], na.rm=TRUE)
# Tree 1B
corrEpsEta[3,5] = mean(coeffs[,c("tree1B3")], na.rm=TRUE)-2
corrEpsEta[3,6] = sd(  coeffs[,c("tree1B3")], na.rm=TRUE)
corrEpsEta[3,7] = mean( RMSEs[,c("tree1B3")], na.rm=TRUE)
corrEpsEta[3,8] = "" #mean(    Ns[,c("tree1B3")], na.rm=TRUE)
# Logit 1B
corrEpsEta[4,5] = mean(coeffs[,c("logit1B3")], na.rm=TRUE)-2
corrEpsEta[4,6] = sd(  coeffs[,c("logit1B3")], na.rm=TRUE)
corrEpsEta[4,7] = mean( RMSEs[,c("logit1B3")], na.rm=TRUE)
corrEpsEta[4,8] = "" #mean(    Ns[,c("logit1B3")], na.rm=TRUE)
# Bin Coarse 1B
corrEpsEta[5,5] = mean(coeffs[,c("binRough1B3")], na.rm=TRUE)-2
corrEpsEta[5,6] = sd(  coeffs[,c("binRough1B3")], na.rm=TRUE)
corrEpsEta[5,7] = mean( RMSEs[,c("binRough1B3")], na.rm=TRUE)
corrEpsEta[5,8] = "" #mean(    Ns[,c("binRough1B3")], na.rm=TRUE)
# Bin 2B
corrEpsEta[6,5] = mean(coeffs[,c("bin2B3")], na.rm=TRUE)-2
corrEpsEta[6,6] = sd(  coeffs[,c("bin2B3")], na.rm=TRUE)
corrEpsEta[6,7] = mean( RMSEs[,c("bin2B3")], na.rm=TRUE)
corrEpsEta[6,8] = "" #mean(    Ns[,c("bin2B3")], na.rm=TRUE)
# Tree 2B
corrEpsEta[7,5] = mean(coeffs[,c("tree2B3")], na.rm=TRUE)-2
corrEpsEta[7,6] = sd(  coeffs[,c("tree2B3")], na.rm=TRUE)
corrEpsEta[7,7] = mean( RMSEs[,c("tree2B3")], na.rm=TRUE)
corrEpsEta[7,8] = "" #mean(    Ns[,c("tree2B3")], na.rm=TRUE)
# Logit 2B
corrEpsEta[8,5] = mean(coeffs[,c("logit2B3")], na.rm=TRUE)-2
corrEpsEta[8,6] = sd(  coeffs[,c("logit2B3")], na.rm=TRUE)
corrEpsEta[8,7] = mean( RMSEs[,c("logit2B3")], na.rm=TRUE)
corrEpsEta[8,8] = "" #mean(    Ns[,c("logit2B3")], na.rm=TRUE)
# Bin Coarse 2B
corrEpsEta[9,5] = mean(coeffs[,c("binRough2B3")], na.rm=TRUE)-2
corrEpsEta[9,6] = sd(  coeffs[,c("binRough2B3")], na.rm=TRUE)
corrEpsEta[9,7] = mean( RMSEs[,c("binRough2B3")], na.rm=TRUE)
corrEpsEta[9,8] = "" #mean(    Ns[,c("binRough2B3")], na.rm=TRUE)


# #-----------------------
# # Logit preference shocks
# #-----------------------
# load('coeffsT1EV.Rda')
# load('RMSEsT1EV.Rda')
# load('NsT1EV.Rda')
# # OLS
# T1EV[1,1] = mean(coeffs[,c("OLS")], na.rm=TRUE)-2
# T1EV[1,2] = sd(  coeffs[,c("OLS")], na.rm=TRUE)
# T1EV[1,3] = mean( RMSEs[,c("OLS")], na.rm=TRUE)
# T1EV[1,4] = round(mean(    Ns[,c("OLS")], na.rm=TRUE), digits=0)
# # Bin 1B
# T1EV[2,1] = mean(coeffs[,c("bin1B3")], na.rm=TRUE)-2
# T1EV[2,2] = sd(  coeffs[,c("bin1B3")], na.rm=TRUE)
# T1EV[2,3] = mean( RMSEs[,c("bin1B3")], na.rm=TRUE)
# T1EV[2,4] = "" #mean(    Ns[,c("bin1B3")], na.rm=TRUE)
# # Tree 1B
# T1EV[3,1] = mean(coeffs[,c("tree1B3")], na.rm=TRUE)-2
# T1EV[3,2] = sd(  coeffs[,c("tree1B3")], na.rm=TRUE)
# T1EV[3,3] = mean( RMSEs[,c("tree1B3")], na.rm=TRUE)
# T1EV[3,4] = "" #mean(    Ns[,c("tree1B3")], na.rm=TRUE)
# # Logit 1B
# T1EV[4,1] = mean(coeffs[,c("logit1B3")], na.rm=TRUE)-2
# T1EV[4,2] = sd(  coeffs[,c("logit1B3")], na.rm=TRUE)
# T1EV[4,3] = mean( RMSEs[,c("logit1B3")], na.rm=TRUE)
# T1EV[4,4] = "" #mean(    Ns[,c("logit1B3")], na.rm=TRUE)
# # Bin Coarse 1B
# T1EV[5,1] = mean(coeffs[,c("binRough1B3")], na.rm=TRUE)-2
# T1EV[5,2] = sd(  coeffs[,c("binRough1B3")], na.rm=TRUE)
# T1EV[5,3] = mean( RMSEs[,c("binRough1B3")], na.rm=TRUE)
# T1EV[5,4] = "" #mean(    Ns[,c("binRough1B3")], na.rm=TRUE)
# # Bin 2B
# T1EV[6,1] = mean(coeffs[,c("bin2B3")], na.rm=TRUE)-2
# T1EV[6,2] = sd(  coeffs[,c("bin2B3")], na.rm=TRUE)
# T1EV[6,3] = mean( RMSEs[,c("bin2B3")], na.rm=TRUE)
# T1EV[6,4] = "" #mean(    Ns[,c("bin2B3")], na.rm=TRUE)
# # Tree 2B
# T1EV[7,1] = mean(coeffs[,c("tree2B3")], na.rm=TRUE)-2
# T1EV[7,2] = sd(  coeffs[,c("tree2B3")], na.rm=TRUE)
# T1EV[7,3] = mean( RMSEs[,c("tree2B3")], na.rm=TRUE)
# T1EV[7,4] = "" #mean(    Ns[,c("tree2B3")], na.rm=TRUE)
# # Logit 2B
# T1EV[8,1] = mean(coeffs[,c("logit2B3")], na.rm=TRUE)-2
# T1EV[8,2] = sd(  coeffs[,c("logit2B3")], na.rm=TRUE)
# T1EV[8,3] = mean( RMSEs[,c("logit2B3")], na.rm=TRUE)
# T1EV[8,4] = "" #mean(    Ns[,c("logit2B3")], na.rm=TRUE)
# # Bin Coarse 2B
# T1EV[9,1] = mean(coeffs[,c("binRough2B3")], na.rm=TRUE)-2
# T1EV[9,2] = sd(  coeffs[,c("binRough2B3")], na.rm=TRUE)
# T1EV[9,3] = mean( RMSEs[,c("binRough2B3")], na.rm=TRUE)
# T1EV[9,4] = "" #mean(    Ns[,c("binRough2B3")], na.rm=TRUE)
# 
# load('coeffs1e3T1EV.Rda')
# load('RMSEs1e3T1EV.Rda')
# load('Ns1e3T1EV.Rda')
# # OLS
# T1EV[1,5] = mean(coeffs[,c("OLS")], na.rm=TRUE)-2
# T1EV[1,6] = sd(  coeffs[,c("OLS")], na.rm=TRUE)
# T1EV[1,7] = mean( RMSEs[,c("OLS")], na.rm=TRUE)
# T1EV[1,8] = round(mean(    Ns[,c("OLS")], na.rm=TRUE), digits=0)
# # Bin 1B
# T1EV[2,5] = mean(coeffs[,c("bin1B3")], na.rm=TRUE)-2
# T1EV[2,6] = sd(  coeffs[,c("bin1B3")], na.rm=TRUE)
# T1EV[2,7] = mean( RMSEs[,c("bin1B3")], na.rm=TRUE)
# T1EV[2,8] = "" #mean(    Ns[,c("bin1B3")], na.rm=TRUE)
# # Tree 1B
# T1EV[3,5] = mean(coeffs[,c("tree1B3")], na.rm=TRUE)-2
# T1EV[3,6] = sd(  coeffs[,c("tree1B3")], na.rm=TRUE)
# T1EV[3,7] = mean( RMSEs[,c("tree1B3")], na.rm=TRUE)
# T1EV[3,8] = "" #mean(    Ns[,c("tree1B3")], na.rm=TRUE)
# # Logit 1B
# T1EV[4,5] = mean(coeffs[,c("logit1B3")], na.rm=TRUE)-2
# T1EV[4,6] = sd(  coeffs[,c("logit1B3")], na.rm=TRUE)
# T1EV[4,7] = mean( RMSEs[,c("logit1B3")], na.rm=TRUE)
# T1EV[4,8] = "" #mean(    Ns[,c("logit1B3")], na.rm=TRUE)
# # Bin Coarse 1B
# T1EV[5,5] = mean(coeffs[,c("binRough1B3")], na.rm=TRUE)-2
# T1EV[5,6] = sd(  coeffs[,c("binRough1B3")], na.rm=TRUE)
# T1EV[5,7] = mean( RMSEs[,c("binRough1B3")], na.rm=TRUE)
# T1EV[5,8] = "" #mean(    Ns[,c("binRough1B3")], na.rm=TRUE)
# # Bin 2B
# T1EV[6,5] = mean(coeffs[,c("bin2B3")], na.rm=TRUE)-2
# T1EV[6,6] = sd(  coeffs[,c("bin2B3")], na.rm=TRUE)
# T1EV[6,7] = mean( RMSEs[,c("bin2B3")], na.rm=TRUE)
# T1EV[6,8] = "" #mean(    Ns[,c("bin2B3")], na.rm=TRUE)
# # Tree 2B
# T1EV[7,5] = mean(coeffs[,c("tree2B3")], na.rm=TRUE)-2
# T1EV[7,6] = sd(  coeffs[,c("tree2B3")], na.rm=TRUE)
# T1EV[7,7] = mean( RMSEs[,c("tree2B3")], na.rm=TRUE)
# T1EV[7,8] = "" #mean(    Ns[,c("tree2B3")], na.rm=TRUE)
# # Logit 2B
# T1EV[8,5] = mean(coeffs[,c("logit2B3")], na.rm=TRUE)-2
# T1EV[8,6] = sd(  coeffs[,c("logit2B3")], na.rm=TRUE)
# T1EV[8,7] = mean( RMSEs[,c("logit2B3")], na.rm=TRUE)
# T1EV[8,8] = "" #mean(    Ns[,c("logit2B3")], na.rm=TRUE)
# # Bin Coarse 2B
# T1EV[9,5] = mean(coeffs[,c("binRough2B3")], na.rm=TRUE)-2
# T1EV[9,6] = sd(  coeffs[,c("binRough2B3")], na.rm=TRUE)
# T1EV[9,7] = mean( RMSEs[,c("binRough2B3")], na.rm=TRUE)
# T1EV[9,8] = "" #mean(    Ns[,c("binRough2B3")], na.rm=TRUE)
# 
# 
# #-----------------------
# # FE factor loading
# #-----------------------
# load('coeffsFE.Rda')
# load('RMSEsFE.Rda')
# load('NsFE.Rda')
# # OLS
# FE[1,1] = mean(coeffs[,c("OLS")], na.rm=TRUE)-2
# FE[1,2] = sd(  coeffs[,c("OLS")], na.rm=TRUE)
# FE[1,3] = mean( RMSEs[,c("OLS")], na.rm=TRUE)
# FE[1,4] = round(mean(    Ns[,c("OLS")], na.rm=TRUE), digits=0)
# # Bin 1B
# FE[2,1] = mean(coeffs[,c("bin1B3")], na.rm=TRUE)-2
# FE[2,2] = sd(  coeffs[,c("bin1B3")], na.rm=TRUE)
# FE[2,3] = mean( RMSEs[,c("bin1B3")], na.rm=TRUE)
# FE[2,4] = "" #mean(    Ns[,c("bin1B3")], na.rm=TRUE)
# # Tree 1B
# FE[3,1] = mean(coeffs[,c("tree1B3")], na.rm=TRUE)-2
# FE[3,2] = sd(  coeffs[,c("tree1B3")], na.rm=TRUE)
# FE[3,3] = mean( RMSEs[,c("tree1B3")], na.rm=TRUE)
# FE[3,4] = "" #mean(    Ns[,c("tree1B3")], na.rm=TRUE)
# # Logit 1B
# FE[4,1] = mean(coeffs[,c("logit1B3")], na.rm=TRUE)-2
# FE[4,2] = sd(  coeffs[,c("logit1B3")], na.rm=TRUE)
# FE[4,3] = mean( RMSEs[,c("logit1B3")], na.rm=TRUE)
# FE[4,4] = "" #mean(    Ns[,c("logit1B3")], na.rm=TRUE)
# # Bin Coarse 1B
# FE[5,1] = mean(coeffs[,c("binRough1B3")], na.rm=TRUE)-2
# FE[5,2] = sd(  coeffs[,c("binRough1B3")], na.rm=TRUE)
# FE[5,3] = mean( RMSEs[,c("binRough1B3")], na.rm=TRUE)
# FE[5,4] = "" #mean(    Ns[,c("binRough1B3")], na.rm=TRUE)
# # Bin 2B
# FE[6,1] = mean(coeffs[,c("bin2B3")], na.rm=TRUE)-2
# FE[6,2] = sd(  coeffs[,c("bin2B3")], na.rm=TRUE)
# FE[6,3] = mean( RMSEs[,c("bin2B3")], na.rm=TRUE)
# FE[6,4] = "" #mean(    Ns[,c("bin2B3")], na.rm=TRUE)
# # Tree 2B
# FE[7,1] = mean(coeffs[,c("tree2B3")], na.rm=TRUE)-2
# FE[7,2] = sd(  coeffs[,c("tree2B3")], na.rm=TRUE)
# FE[7,3] = mean( RMSEs[,c("tree2B3")], na.rm=TRUE)
# FE[7,4] = "" #mean(    Ns[,c("tree2B3")], na.rm=TRUE)
# # Logit 2B
# FE[8,1] = mean(coeffs[,c("logit2B3")], na.rm=TRUE)-2
# FE[8,2] = sd(  coeffs[,c("logit2B3")], na.rm=TRUE)
# FE[8,3] = mean( RMSEs[,c("logit2B3")], na.rm=TRUE)
# FE[8,4] = "" #mean(    Ns[,c("logit2B3")], na.rm=TRUE)
# # Bin Coarse 2B
# FE[9,1] = mean(coeffs[,c("binRough2B3")], na.rm=TRUE)-2
# FE[9,2] = sd(  coeffs[,c("binRough2B3")], na.rm=TRUE)
# FE[9,3] = mean( RMSEs[,c("binRough2B3")], na.rm=TRUE)
# FE[9,4] = "" #mean(    Ns[,c("binRough2B3")], na.rm=TRUE)
# 
# load('coeffs1e3FE.Rda')
# load('RMSEs1e3FE.Rda')
# load('Ns1e3FE.Rda')
# # OLS
# FE[1,5] = mean(coeffs[,c("OLS")], na.rm=TRUE)-2
# FE[1,6] = sd(  coeffs[,c("OLS")], na.rm=TRUE)
# FE[1,7] = mean( RMSEs[,c("OLS")], na.rm=TRUE)
# FE[1,8] = round(mean(    Ns[,c("OLS")], na.rm=TRUE), digits=0)
# # Bin 1B
# FE[2,5] = mean(coeffs[,c("bin1B3")], na.rm=TRUE)-2
# FE[2,6] = sd(  coeffs[,c("bin1B3")], na.rm=TRUE)
# FE[2,7] = mean( RMSEs[,c("bin1B3")], na.rm=TRUE)
# FE[2,8] = "" #mean(    Ns[,c("bin1B3")], na.rm=TRUE)
# # Tree 1B
# FE[3,5] = mean(coeffs[,c("tree1B3")], na.rm=TRUE)-2
# FE[3,6] = sd(  coeffs[,c("tree1B3")], na.rm=TRUE)
# FE[3,7] = mean( RMSEs[,c("tree1B3")], na.rm=TRUE)
# FE[3,8] = "" #mean(    Ns[,c("tree1B3")], na.rm=TRUE)
# # Logit 1B
# FE[4,5] = mean(coeffs[,c("logit1B3")], na.rm=TRUE)-2
# FE[4,6] = sd(  coeffs[,c("logit1B3")], na.rm=TRUE)
# FE[4,7] = mean( RMSEs[,c("logit1B3")], na.rm=TRUE)
# FE[4,8] = "" #mean(    Ns[,c("logit1B3")], na.rm=TRUE)
# # Bin Coarse 1B
# FE[5,5] = mean(coeffs[,c("binRough1B3")], na.rm=TRUE)-2
# FE[5,6] = sd(  coeffs[,c("binRough1B3")], na.rm=TRUE)
# FE[5,7] = mean( RMSEs[,c("binRough1B3")], na.rm=TRUE)
# FE[5,8] = "" #mean(    Ns[,c("binRough1B3")], na.rm=TRUE)
# # Bin 2B
# FE[6,5] = mean(coeffs[,c("bin2B3")], na.rm=TRUE)-2
# FE[6,6] = sd(  coeffs[,c("bin2B3")], na.rm=TRUE)
# FE[6,7] = mean( RMSEs[,c("bin2B3")], na.rm=TRUE)
# FE[6,8] = "" #mean(    Ns[,c("bin2B3")], na.rm=TRUE)
# # Tree 2B
# FE[7,5] = mean(coeffs[,c("tree2B3")], na.rm=TRUE)-2
# FE[7,6] = sd(  coeffs[,c("tree2B3")], na.rm=TRUE)
# FE[7,7] = mean( RMSEs[,c("tree2B3")], na.rm=TRUE)
# FE[7,8] = "" #mean(    Ns[,c("tree2B3")], na.rm=TRUE)
# # Logit 2B
# FE[8,5] = mean(coeffs[,c("logit2B3")], na.rm=TRUE)-2
# FE[8,6] = sd(  coeffs[,c("logit2B3")], na.rm=TRUE)
# FE[8,7] = mean( RMSEs[,c("logit2B3")], na.rm=TRUE)
# FE[8,8] = "" #mean(    Ns[,c("logit2B3")], na.rm=TRUE)
# # Bin Coarse 2B
# FE[9,5] = mean(coeffs[,c("binRough2B3")], na.rm=TRUE)-2
# FE[9,6] = sd(  coeffs[,c("binRough2B3")], na.rm=TRUE)
# FE[9,7] = mean( RMSEs[,c("binRough2B3")], na.rm=TRUE)
# FE[9,8] = "" #mean(    Ns[,c("binRough2B3")], na.rm=TRUE)


#-----------------------
# Print results
#-----------------------
print(xtable(baseline  , digits=4),include.rownames=T, sanitize.rownames.function=identity)
print(xtable(corrEps   , digits=4),include.rownames=T, sanitize.rownames.function=identity)
print(xtable(corrEpsEta, digits=4),include.rownames=T, sanitize.rownames.function=identity)
# print(xtable(T1EV      , digits=4),include.rownames=T, sanitize.rownames.function=identity)
# print(xtable(FE        , digits=4),include.rownames=T, sanitize.rownames.function=identity)
 
# print(xtable(df, display = c("s","e","e")),format.args = list(digits = 3, big.mark = " ", decimal.mark = ","))

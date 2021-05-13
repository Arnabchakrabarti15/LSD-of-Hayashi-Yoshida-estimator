# library

library(MASS)
library(matrixcalc)
library(gtools)
library(Matrix)
# INTRODUCTION

p = 30 # number of observations
n = 60 # number of observations per variable 
T1 = 1000

# GENERATION OF SYNCHRONOUS ARRIVAL TIMES

# Preparation of arrival times for p stocks
N <- p*n # Number of random samples for synced data i.e. size of synchronized data
# p stocks and in each stocks n observations 
ll = 0 #initialize the combined arrival times
for(i in 1:p){
  l = sort(runif(n, min = 0, max = T1))
  ll = c(ll,l)
}
ll = sort(ll[-1]) 


# GENERATION OF SYNCHRONOUS DATA

## FIXING THE TRUE COVARIANCE MATRIX (PXP) FOR SIMULATING DATA
Sigma = diag(rep(1,p)) # read.csv("AVG_SUBSAMPLE_CORR_10sec_Day1.csv", header=FALSE)


## GENERATING SYNCED DATA (over the interarrivals i.e. non-cumulative)

sync.data = mvrnorm(N, mu= rep(0,p), Sigma)

## CUMULATIVE SYNCED DATA 

sync.cumsum.data = cumsum(data.frame(sync.data))

# Nonsynchronous data

## indicating function

indicator0 = rep(1:p, n) 
indicator = permute(permute(permute(indicator0)))

## Initialization

nonsynch.cum.data = matrix(rep(0,p*n), n, p)
location = matrix(rep(0,n*p), n, p)

## Preparation of cumulative log-price

for(i in 1:p){
  nonsynch.cum.data[,i] = subset(sync.cumsum.data[,i], indicator == i)
}

## Preparation of location

for(i in 1:p){
  location[,i] = subset(ll, indicator == i)
}


# Hayashi-Yoshida Calculation: 

## Initialization
sum = 0
HY = matrix(rep(0,p*p), p)

## Calculation
for(k in 1:p){
  for(l in 1:p){
    for(i in 1:(n-1)){
      for(j in 1:(n-1)){
        if(location[i,k]>=location[j+1,l]){ #St1[i,2]>=St2[j+1,2]){
          HY[k,l] = HY[k,l] + 0 
        }else if(location[i+1,k]<=location[j,l]){ # if(St1[i+1,2]<=St2[j,2]){
          HY[k,l] = HY[k,l] + 0
        }else {
          HY[k,l] = HY[k,l] + (nonsynch.cum.data[i+1,k] - nonsynch.cum.data[i,k])*(nonsynch.cum.data[j+1,l] - nonsynch.cum.data[j,l])
        }
      }
    }
  }
}

# Scaling

HY = HY/N
## spectrun of HY covariance matrix
eig.HY.cov = eigen(HY)$values  

##############################################
# Limiting spectral distribution
p = 300 # number of observations
n = 600 # number of observations per variable 
T1 = 1000

# GENERATION OF SYNCHRONOUS ARRIVAL TIMES

# Preparation of arrival times for p stocks
N <- p*n # Number of random samples for synced data i.e. size of synchronized data
# p stocks and in each stocks 1000 observations 
ll = 0 #initialize the combined arrival times
for(i in 1:p){
  l = sort(runif(n, min = 0, max = T1))
  ll = c(ll,l)
}
ll = sort(ll[-1]) # combined arrival times of length 1000*40


# GENERATION OF SYNCHRONOUS DATA

## FIXING THE TRUE COVARIANCE MATRIX (PXP) FOR SIMULATING DATA
Sigma = diag(rep(1,p)) # read.csv("AVG_SUBSAMPLE_CORR_10sec_Day1.csv", header=FALSE)

## GENERATING SYNCED DATA (over the interarrivals i.e. non-cumulative)

sync.data = mvrnorm(N, mu= rep(0,p), Sigma)

## CUMULATIVE SYNCED DATA 

sync.cumsum.data = cumsum(data.frame(sync.data))

# Nonsynchronous data

## indicating function

indicator0 = rep(1:p, n) 
indicator = permute(permute(permute(indicator0)))

## Initialization

nonsynch.cum.data = matrix(rep(0,p*n), n, p)
location = matrix(rep(0,n*p), n, p)

## Preparation of cumulative log-price

for(i in 1:p){
  nonsynch.cum.data[,i] = subset(sync.cumsum.data[,i], indicator == i)
}

## Preparation of location

for(i in 1:p){
  location[,i] = subset(ll, indicator == i)
}


# Hayashi-Yoshida Calculation: 

## Initialization
sum = 0
LHY = matrix(rep(0,p*p), p)

## Calculation
for(k in 1:p){
  for(l in 1:p){
    for(i in 1:(n-1)){
      for(j in 1:(n-1)){
        if(location[i,k]>=location[j+1,l]){ #St1[i,2]>=St2[j+1,2]){
          LHY[k,l] = LHY[k,l] + 0 
        }else if(location[i+1,k]<=location[j,l]){ # if(St1[i+1,2]<=St2[j,2]){
          LHY[k,l] = LHY[k,l] + 0
        }else {
          LHY[k,l] = LHY[k,l] + (nonsynch.cum.data[i+1,k] - nonsynch.cum.data[i,k])*(nonsynch.cum.data[j+1,l] - nonsynch.cum.data[j,l])
        }
      }
    }
  }
}

# Scaling

LHY = LHY/N




##############################################

eig.LHY.cov = eigen(LHY)$values

# Plotting the ecdf
library(plyr)
library(ggplot2)
d.f <- data.frame(
  grp = as.factor(c(rep("HY",30),rep("LHY",300)) ),
  val = c( eig.HY.cov, eig.LHY.cov) )

d.f <- arrange(d.f,grp,val)
d.f.ecdf <- ddply(d.f, .(grp), transform, ecdf=ecdf(val)(val) )

p1 <- ggplot( d.f.ecdf, aes(val, ecdf, colour = grp) )
p1 + geom_step()



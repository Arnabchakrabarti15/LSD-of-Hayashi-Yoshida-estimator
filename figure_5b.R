# library

library(MASS)
library(matrixcalc)
library(gtools)
library(Matrix)
# INTRODUCTION

p = 30 # number of observations
n = 500 # number of observations per variable 
T1 = 1000

# GENERATION OF SYNCHRONOUS ARRIVAL TIMES

# Preparation of arrival times for p stocks
N <- p*n # Number of random samples for synced data i.e. size of synchronized data
ll = 0 #initialize the combined arrival times
for(i in 1:p){
  l = sort(runif(n, min = 0, max = T1))
  ll = c(ll,l)
}
ll = sort(ll[-1]) # combined arrival times of length 1000*40


# GENERATION OF SYNCHRONOUS DATA

## FIXING THE TRUE COVARIANCE MATRIX (PXP) FOR SIMULATING DATA
Sigma = read.csv("CORR_Day1.csv", header=FALSE)
Sigma = Sigma[1:p,1:p]

## GENERATING SYNCED DATA (over the interarrivals i.e. non-cumulative)

sync.data = mvrnorm(N, mu= rep(0,p), Sigma)

## CUMULATIVE SYNCED DATA 

sync.cumsum.data = cumsum(data.frame(sync.data))

# Realized cov matrix

## Initialise 

real.cov = matrix(rep(0,p*p),p)

## Calculation 

for(i in 1:N){
  so   = sync.data[i,] %*% t(sync.data[i,])
  real.cov = real.cov + so
}

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


real.cov = real.cov/N

## spectrun of realized covariance matrix
eig.realized.cov = eigen(real.cov)$values

## spectrun of HY covariance matrix
eig.HY.cov = eigen(HY)$values  

# Plotting the ecdf
library(plyr)
library(ggplot2)
d.f <- data.frame(
  grp = as.factor(c(rep("HYCV",p-1),rep("RCV",p-1)) ),
  val = c( eig.HY.cov[-1], eig.realized.cov[-1]) )
  
d.f <- arrange(d.f,grp,val)
d.f.ecdf <- ddply(d.f, .(grp), transform, ecdf=ecdf(val)(val) )

p1 <- ggplot( d.f.ecdf, aes(val, ecdf, colour = grp) )
p1 + geom_step()

library(plyr)
library(ggplot2)
d.f <- data.frame(
  grp = as.factor(c(rep("HYCV",p),rep("RCV",p)) ),
  val = c( eig.HY.cov, eig.realized.cov) )
  
d.f <- arrange(d.f,grp,val)
d.f.ecdf <- ddply(d.f, .(grp), transform, ecdf=ecdf(val)(val) )

p2 <- ggplot( d.f.ecdf, aes(val, ecdf, colour = grp) )
p2 + geom_step()


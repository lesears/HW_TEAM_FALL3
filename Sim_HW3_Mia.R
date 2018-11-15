##################
#   Sim & Risk   #
#      HW3       #
#  Orange Team 6 #
##################


#We will assume only two of these can actually change. Figure 1 shows the probability distributions for
#each factor’s Monte Carlo assumption.
#Dry-Hole Risk
#Factor
#Mean Standard Dev. Min. Max.
#Hydrocarbons 99% 5% 0% 100%
#Structure 100% 0% 100% 100%
#Reservoir 80% 10% 0% 100%
#Seal 100% 0% 100% 100%
# The total number of planned wells is assumed a Uniform
#distribution between 10 and 30.


#𝑃𝑃𝑊 = 𝑃𝐻 × 𝑃𝑅 × 𝑃𝑆𝑒𝑎𝑙 ×𝑃𝑆𝑡𝑟𝑢𝑐𝑡𝑢𝑟𝑒

# #simulate the total count of wells with uniform distribution 
# count<-runif(n=1, min=10, max=30)
# 
# #simulate the probability of hydrocarbons (PH) with truncated normal distribution 
# #rtruncnorm(n, a=-Inf, b=Inf, mean = 0, sd = 1)
# ph<-rtruncnorm(1,a=0, b=1,mean=0.99,sd=0.05)
# 
# 
# #simulate the probability of Reservoir (PR) with trunncated normal distribution 
# pr<-rtruncnorm(1,a=0, b=1,mean=0.8, sd=0.1)
# 
# 
# #calculate the producing probability for each well
# pw<-ph*pr*pseal*pstructure
# 
# #simulate the dry or wet well with Bernolli distribution with inputing producing probability
# #rbinom(n, size, prob); size=number of trails--->what does this mean?
# results<-rbinom(1, 1, pw)


#-----------------------------------------------------------------------------------------
# #since the structure and seal don't have any risk, does that mean Pseal=1 Pstructure=1?
pseal<-1
pstructure<-1

#----------------------------------------------------------------------------------------
#load the library 
library(truncnorm)
#-----------------------------------------------------------------------------------------
# Set null vector
pw <- c()
#producing probability of wells

ph <-c()
#probability of hydrocarbon

pr <-c()
#probability of reservior 


results = rep(0,10000)
#whether the well is dry or wet 

count <- rep(0,10000)
#the count of wells 

phvalues<-c()

k<-0

#----------------------------------combine all of those into one loop to simulate 10000 times-------------------------

for(i in 1:10000){
  #uniform distribution to simulate the count of wells 
  count[i]<-sample(10:30,1)
      #nested loop to simulate the producing probability of each well and with bernoli 
      #to determine if that well is dry or wet
      for (j in 1:count[i]){ 
        k<-k+1
        ph[k]<-rtruncnorm(1,a=0, b=1,mean=0.99,sd=0.05)
        
        pr[k]<-rtruncnorm(1,a=0, b=1,mean=0.8,sd=0.1)
        pw[k]<-ph[k]*pr[k]*pseal*pstructure
        results[i]<-rbinom(1, 1, pw[k])/count[i] + results[i]
      }
}

hist(results,main='distribution of proportion of wells that is producing', xlab='proportion')
hist(ph,main='distribution of probability of hydrocarbon', xlab='probability')
hist(pr,main='distribution of probability of reservior', xlab='probability')


VaR.results = quantile(results, 0.05)
CVaR = mean(results[results<=0.591])

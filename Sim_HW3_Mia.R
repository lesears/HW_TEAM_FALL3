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


#----------------------------------------------------------------------------------------
#load the library 
library(truncnorm)
#-----------------------------------------------------------------------------------------
# Set null vector
pw <- rep(0,10000)
#producing probability of wells

ph <-rep(0,10000)
#probability of hydrocarbon

pr <-rep(0,10000)
#probability of reservior 


results = rep(0,10000)
#where the well is dry or wet 

count <- rep(0,10000)
#the count of wells 




#-----------------------------------------------------------------------------------------

# #since the structure and seal don't have any risk, does that mean Pseal=1 Pstructure=1?
# pseal<-1
# pstructure<-1
# 
# 
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

#----------------------------------combine all of those into one loop to simulate 10000 times-------------------------

for(i in 1:10000){
  #uniform distribution to simulate the count of wells 
  count[i]<-runif(n=1, min=10, max=30)
      #nested loop to simulate the producing probability of each well and with bernoli 
      #to determine if that well is dry or wet
      for (j in 1:count[i]){
        ph[j]<-rtruncnorm(1,a=0, b=1,mean=0.99,sd=0.05)
        pr[j]<-rtruncnorm(1,a=0, b=1,mean=0.8, sd=0.1)
        pw[j]<-ph[j]*pr[j]*pseal*pstructure
        results[j]<-rbinom(1, 1, pw[j])
      }
}

hist(results)
hist(pw)

VaR = quantile(pw, 0.05)
CVaR = mean(pw[pw<=10.97145])

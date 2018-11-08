# need these packages
library(survival)
library(survminer)
library(flexsurv)
library(dplyr)


#setting directory

setwd("/Users/miawu/Documents/NC State/Fall 3/Survival Analysis/survivalcsv")


#import data
katrina<-read_csv('katrina.csv', col_names = TRUE)

View(katrina)

#checking distributions
#--weibull
fit_wb <- flexsurvreg(Surv(hour, reason ==1 ) ~ backup+bridgecrane+servo+trashrack+elevation+slope+age, 
                      data = katrina, dist = "weibull")
# plot looks good 
plot(fit_wb, type = "cumhaz", ci = TRUE, conf.int = FALSE, las = 1, bty = "n",
     xlab = "Hour", ylab = "cumulative hazard", main = "weibull distribution")


#--exponential
fit_exp<-flexsurvreg(Surv(hour, reason ==1 ) ~ backup+ bridgecrane+servo+trashrack+elevation+slope+age, 
                      data = katrina, dist = "exponential")

#plot doesnt look good 
plot(fit_exp, type = "cumhaz", ci = TRUE, conf.int = FALSE, las = 1, bty = "n", 
     xlab = "week", ylab = "cumulative hazard",
     main = "exponential distribution")


#--lognormal 
fit_lnorm<-flexsurvreg(Surv(hour, reason ==1 ) ~ backup+ bridgecrane+servo+trashrack+elevation+slope+age, 
                      data = katrina, dist = "lognorm")

#plot looks good 
plot(fit_lnorm, type = "cumhaz", ci = TRUE, conf.int = FALSE, las = 1, bty = "n", 
     xlab = "week", ylab = "cumulative hazard",
     main = "lognorm distribution")


#--log-logistic
fit_llogit<-flexsurvreg(Surv(hour, reason ==1 ) ~ backup+ bridgecrane+servo+trashrack+elevation+slope+age, 
                      data = katrina, dist = "llogis")

#plot doesnt look good 
plot(fit_llogit, type = "cumhaz", ci = TRUE, conf.int = FALSE, las = 1, bty = "n", 
     xlab = "week", ylab = "cumulative hazard",
     main = "log-logistic distribution")

#---------based on the distribution selection, weibull is the best for this case
#now lets check the summary from the model to determine 


fit <- survreg(Surv(hour, reason == 1) ~ backup+ bridgecrane + servo + trashrack + elevation + slope + age, 
                      data = katrina, dist = "weibull") 
summary(fit)

# based on the pvalues, backup, servo and slope are significant

#exponentiate estimates
exp(coef(fit))
#based on the estimates, backup, servo can make pumps survive longer
#(since the estimate of these two variables are greater than 1)




#------------------predict-------------------------
#survprob_50 <- predict(fit, type = "quantile", se.fit = TRUE,
                            # p = (0.5))
# note that these are the quantiles for EVENT times, not survival times
# so 0.25 means this is the time where events have happened to 25% of people
# so S(t) = 0.75 at whatever this time is
#head(survprob_50$fit)



#pred_time <- predict(fit, type = "response", se.fit = TRUE)
#with(pred_time, head(cbind(fit, se.fit)))



#-------------------------now lets look at how much time can servo and backup upgrade can save for each pump------

#-----------servo ---------------------
katrina_noservo <- katrina %>%
  # first, we need to get the linear predictor from the old model
  mutate(old_lp = predict(fit, type = "lp"),
         # add ID variable so we know which subjects they are
         ID = row_number()) %>%
  # next, we're only interested in those who had the event AND no financial aid
  dplyr::filter(reason == 1, servo == 0) %>%
  # now i'm doing two things:
  # 1. find the survival prob at the time of their event
  # 2. pretending they did have financial aid (change fin from 0 to 1)
  mutate(old_time = hour,
         surv_prob = 1 - psurvreg(old_time,
                                  mean = old_lp,
                                  scale = fit$scale,
                                  distribution = fit$dist),
         old_servo = servo,
         servo = old_servo + 1)

# now with that dataset, i need to find their new time
results_servo <- katrina_noservo %>%
  # estimate their new linear predictor value if they had financial aid
  mutate(new_lp = predict(fit, newdata = katrina_noservo, type = "lp"),
         # now, keeping the same survival probability as before, get the time
         # corresponding to the new_lp value
         new_time = qsurvreg(1 - surv_prob,
                             mean = new_lp,
                             scale = fit$scale,
                             distribution = fit$dist),
         # and compute the difference
         pred_time_diff = new_time - old_time) %>%
  select(ID, surv_prob, old_time, new_time, pred_time_diff)
head(results_servo)

#sort the results_servo by variable 'pred_time_diff' descending. this way we can see the pumps that could save 
#most time with servo upgrade

results_servo<- results_servo[order(-results_servo$pred_time_diff),] 
View(results_servo)


#--------------------------------backup -----------------

katrina_nobackup <- katrina %>%
  # first, we need to get the linear predictor from the old model
  mutate(old_lp = predict(fit, type = "lp"),
         # add ID variable so we know which subjects they are
         ID = row_number()) %>%
  # next, we're only interested in those who had the event AND no financial aid
  dplyr::filter(reason == 1, backup == 0) %>%
  # now i'm doing two things:
  # 1. find the survival prob at the time of their event
  # 2. pretending they did have financial aid (change fin from 0 to 1)
  mutate(old_time = hour,
         surv_prob = 1 - psurvreg(old_time,
                                  mean = old_lp,
                                  scale = fit$scale,
                                  distribution = fit$dist),
         old_backup = backup,
         backup = old_backup + 1)

# now with that dataset, i need to find their new time
results_backup <- katrina_nobackup %>%
  # estimate their new linear predictor value if they had financial aid
  mutate(new_lp = predict(fit, newdata = katrina_nobackup, type = "lp"),
         # now, keeping the same survival probability as before, get the time
         # corresponding to the new_lp value
         new_time = qsurvreg(1 - surv_prob,
                             mean = new_lp,
                             scale = fit$scale,
                             distribution = fit$dist),
         # and compute the difference
         pred_time_diff = new_time - old_time) %>%
  select(ID, surv_prob, old_time, new_time, pred_time_diff)
head(results_slope)

#sort the results_backup by variable 'pred_time_diff' descending. this way we can see the pumps that could save 
#most time with servo upgrade

results_<- results_backup[order(-results_backup$pred_time_diff),] 
View(results_backup)





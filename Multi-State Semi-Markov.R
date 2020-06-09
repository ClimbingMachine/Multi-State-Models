############## Multi-State Models #################

# set up the datasets
# setwd("C:/Users/Genghis_Zhang/Desktop/AMAR_Revision")

setwd("C:/Users/test_/OneDrive - purdue.edu/AMAR_Revision")
vehicle.one <- read.csv('2017.csv', header = TRUE, sep = ',') # one-way dataset
vehicle.two <- read.csv('2018.csv', header = TRUE, sep = ',') # two-way dataset

vehicle.one$bus <- ifelse(vehicle.one$VehType == 3, 1, 0)
vehicle.one$Young <- ifelse(vehicle.one$AgeRange <= 2, 1, 0)

vehicle.two$bus <- ifelse(vehicle.two$VehType == 3, 1, 0)
vehicle.two$Young <- ifelse(vehicle.two$AgeRange <= 2, 1, 0)

############# identify the distribution fits for transitions #############
# transition models with full structure complexity
trans1to2 <- vehicle.one[vehicle.one$state.h == 1 & vehicle.one$state.j == 2, ]      # transition 1-2
trans1to4 <- vehicle.one[vehicle.one$state.h == 1 & vehicle.one$state.j == 4, ]      # transition 1-4
trans2to3 <- vehicle.one[vehicle.one$state.h == 2 & vehicle.one$state.j == 3, ]      # transition 2-3
trans2to4 <- vehicle.one[vehicle.one$state.h == 2 & vehicle.one$state.j == 4, ]      # transition 2-4
trans3to4 <- vehicle.one[vehicle.one$state.h == 3 & vehicle.one$state.j == 4, ]      # transition 3-4


# you are likely to be asked to compare your model with a parsimonious model, so here we go - t1 is the parsimonious model

t1 <- vehicle.one[(vehicle.one$state.h == 1 & vehicle.one$state.j == 2)|
                  (vehicle.one$state.h == 1 & vehicle.one$state.j == 4)|
                  (vehicle.one$state.h == 2 & vehicle.one$state.j == 4)|
                  (vehicle.one$state.h == 2 & vehicle.one$state.j == 3)| 
                  (vehicle.one$state.h == 3 & vehicle.one$state.j == 4),
]


# required libraries 
library("MASS")
library(flexsurv)
library(survival)
library(lubridate)

########## whole model - parsimonious model #########
fitAFT.one <- 
  survreg(Surv(RealGap) ~ GroupSize + Queueing + bus + NearFar +  
          Gender + Young + Adj + Distance  + Clos + Distraction, 
          dist="weibull", data=t2)                                  # you can change any distribution by specifying in the "dist"

summary(fitAFT.one)
BIC(fitAFT.one)

######### transition 1-2 ################

fitAFT.one.weib1to2 <- 
  survreg(Surv(RealGap) ~ GroupSize + Queueing + bus + NearFar + 
          Gender + Young + Adj + Distance + Clos + Distraction, 
          dist="weibull", data=trans1to2)


AIC(fitAFT.one.weib1to2)          # AIC
BIC(fitAFT.one.weib1to2)          # BIC


######### transition 1-4 ################

fitAFT.one.weib1to4 <- 
  survreg(Surv(RealGap) ~ GroupSize + Queueing + bus + NearFar  + 
  Gender + Young + Adj + Distance + Speed + Clos + Distraction,
  dist="weibull", data=trans1to4)

summary(fitAFT.one.weib1to4)
extractAIC(fitAFT.one.weib1to4)

### Transition 2-3 ###

fitAFT.one.weib2to3 <- 
  survreg(Surv(RealGap) ~ GroupSize + Queueing + bus + NearFar + 
  Gender + Young + Adj + Distance + Speed + Clos + Distraction,
  dist="weibull", data=trans2to3)

summary(fitAFT.one.weib2to3)
extractAIC(fitAFT.one.weib2to3)

### Transition 2-4 ###

fitAFT.one.weib2to4 <- 
  survreg(Surv(RealGap) ~ GroupSize + Queueing + bus + NearFar  + 
  Gender + Young + Adj + Distance + Speed + Clos + Distraction,
  dist="weibull", data=trans2to4)

summary(fitAFT.one.weib2to4)
extractAIC(fitAFT.one.weib2to4)


### Transition 3-4 ###

fitAFT.one.weib3to4 <- 
  survreg(Surv(RealGap) ~ GroupSize + Queueing + bus + NearFar + 
  Gender + Young + Adj + Distance + Speed + Clos + Distraction,
  dist="weibull", data=trans3to4)

summary(fitAFT.one.weib3to4)
extractAIC(fitAFT.one.weib3to4)

#### Plot things - extract the variables you want #####

list_1to4 <- list(GroupSize = 1, Queueing = 0, bus = 0, NearFar = 1, 
                  Gender = 1, Young = 1, Adj = 0, Distance = 70, 
                  Speed = 10, Clos = 1, Distraction = 0)

list_1to4_q5 <- list(GroupSize = 1, Queueing = 5, bus = 0, NearFar = 1, 
                  Gender = 1, Young = 1, Adj = 0, Distance = 70, 
                  Speed = 10, Clos = 1, Distraction = 0)

list_1to4_q10 <- list(GroupSize = 1, Queueing = 10, bus = 0, NearFar = 1, 
                     Gender = 1, Young = 1, Adj = 0, Distance = 70, 
                     Speed = 10, Clos = 1, Distraction = 0)

### plot Transition 1-4 - no need for convolution products ###

plot(predict(fitAFT.one.weib1to4, newdata = list_1to4,
     type="quantile",p=seq(0,1,by=.01)), 0.3826879 * seq(0,1,by=.01),
     col="red", lwd = 2, "l", xlim = c(0,10), 
     xlab = "Waiting Time since Last State", ylab = "Probablity", 
     main = "Transition 1-4", ylim = c(0,0.4))

lines(predict(fitAFT.one.weib1to4, newdata = list_1to4_q5,
      type="quantile",p=seq(0,1,by=.01)), 0.3826879 * seq(0,1,by=.01),
      col="blue", lty = 2, lwd = 2)

lines(predict(fitAFT.one.weib1to4, newdata = list_1to4_q10,
      type="quantile",p=seq(0,1,by=.01)),0.3826879 * seq(0,1,by=.01),
      col="dark red", lty = 3, lwd = 3)

legend("bottomright", legend = 
         c("Queueing Delay = 0s", "Queueing Delay = 5s", "Queueing Delay = 10s"),
       lwd = c(2,2,3), col = c("red", "blue", "dark red"), lty = c(1,2,3))

### plot transition 1-2-4 with variable -- Queueing ###

x <- seq(0, 20, 0.01)

# transform the cumulative distribution into probability distribution

F1to2.q0 <- 1 - exp(-(x/exp(predict(fitAFT.one.weib1to2, newdata = list_1to4,
                                 type="linear")))^(1/0.6616251))

F2to4.q0 <- 1 - exp(-(x/exp(predict(fitAFT.one.weib2to4, newdata = list_1to4,
                                 type="linear")))^(1/0.2421565))

f2to4.q0 <- (F2to4.q0[-1] - F2to4.q0)/0.01

f2to4.q0[2001] <- 0
F1to2to4.q0 <- matrix(0, 1, length(x))

# use the convolution production to calculate the distritbution

i<- 1000

for(i in 3:length(x)){
  for (j in 1:(i-1)) {
    F1to2to4.q0[i] <- F1to2to4.q0[i] + F1to2.q0[i-j]*f2to4.q0[j] * 0.01
  }
}

#queueing  = 10s

F1to2.q10 <- 1 - exp(-(x/exp(predict(fitAFT.one.weib1to2, newdata = list_1to4_q10,
                                    type="linear")))^(1/0.6616251))

F2to4.q10 <- 1 - exp(-(x/exp(predict(fitAFT.one.weib2to4, newdata = list_1to4_q10,
                                    type="linear")))^(1/0.2421565))

f2to4.q10 <- (F2to4.q10[-1] - F2to4.q10)/0.01

f2to4.q10[2001] <- 0
F1to2to4.q10 <- matrix(0, 1, length(x))

i<- 1000
for(i in 3:length(x)){
  for (j in 1:(i-1)) {
    F1to2to4.q10[i] <- F1to2to4.q10[i] + F1to2.q10[i-j]*f2to4.q10[j] * 0.01
  }
}

#queueing  = 5s

F1to2.q5 <- 1 - exp(-(x/exp(predict(fitAFT.one.weib1to2, newdata = list_1to4_q5,
                                    type="linear")))^(1/0.6616251))

F2to4.q5 <- 1 - exp(-(x/exp(predict(fitAFT.one.weib2to4, newdata = list_1to4_q5,
                                    type="linear")))^(1/0.2421565))

f2to4.q5 <- (F2to4.q5[-1] - F2to4.q5)/0.01

f2to4.q5[2001] <- 0
F1to2to4.q5 <- matrix(0, 1, length(x))

i<- 1000
for(i in 3:length(x)){
  for (j in 1:(i-1)) {
    F1to2to4.q5[i] <- F1to2to4.q5[i] + F1to2.q5[i-j]*f2to4.q5[j] * 0.01
  }
}

F1to4.q0 <- 1 - exp(-(x/exp(predict(fitAFT.one.weib1to4, newdata = list_1to4,
                                     type="linear")))^(1/0.2424537))
F1to4.q5 <- 1 - exp(-(x/exp(predict(fitAFT.one.weib1to4, newdata = list_1to4_q5,
                                    type="linear")))^(1/0.2424537))
F1to4.q10 <- 1 - exp(-(x/exp(predict(fitAFT.one.weib1to4, newdata = list_1to4_q10,
                                    type="linear")))^(1/0.2424537))

# additional plots for the time versus probability

par(mfrow=c(1,2))

plot(x, 0.3826879 * F1to4.q0 , col="red",
     lwd = 2, "l", xlim = c(0,10), xlab = "Total Waiting Time", 
     ylab = "Probablity", main = "Transition 1-4", ylim = c(0,0.4))

lines(x, 0.3826879 * F1to4.q5,col="blue", lty = 2, lwd = 2)
lines(x, 0.3826879 * F1to4.q10, col="dark red", lty = 3, lwd = 3)

legend("bottomright",legend = c("Queueing Delay = 0s", "Queueing Delay = 5s", "Queueing Delay = 10s"),
       lwd = c(2,2,3), col = c("red", "blue", "dark red"), lty = c(1,2,3))

plot(x, 0.1605011 * F1to2to4.q0 , col="red",
     lwd = 2, "l", xlim = c(0,20), xlab = "Total Waiting Time", 
     ylab = "Probablity", main = "Transition 1-2-4", ylim = c(0,0.2))

lines(x, 0.1605011 * F1to2to4.q5,col="blue", lty = 2, lwd = 2)
lines(x, 0.1605011 * F1to2to4.q10, col="dark red", lty = 3, lwd = 3)


legend("bottomright",legend = c("Queueing Delay = 0s", "Queueing Delay = 5s", "Queueing Delay = 10s"),
       lwd = c(2,2,3), col = c("red", "blue", "dark red"), lty = c(1,2,3))


### addtition plots for the variable - bus #############################

# prepare the data

list_1to4_bus <- list(GroupSize = 1, Queueing = 5, bus = 1, NearFar = 1, 
                      Gender = 1, Young = 1, Adj = 0, Distance = 70, 
                      Speed = 10, Clos = 1, Distraction = 1)

list_1to4_nobus <- list(GroupSize = 1, Queueing = 5, bus = 0, NearFar = 1, 
                        Gender = 1, Young = 1, Adj = 0, Distance = 70, 
                        Speed = 10, Clos = 1, Distraction = 1)

# transition 1-4 with/without bus

F1to4.bus <- 1 - exp(-(x/exp(predict(fitAFT.one.weib1to4, newdata = list_1to4_bus,
                                    type="linear")))^(1/0.2424537))

F1to4.nobus <- 1 - exp(-(x/exp(predict(fitAFT.one.weib1to4, newdata = list_1to4_nobus,
                                    type="linear")))^(1/0.2424537))

#Transition 1-2-4 with bus using convolution products

F1to2.bus <- 1 - exp(-(x/exp(predict(fitAFT.one.weib1to2, newdata = list_1to4_bus,       # transition 1-2
                                     type="linear")))^(1/0.6616251))

F2to4.bus <- 1 - exp(-(x/exp(predict(fitAFT.one.weib2to4, newdata = list_1to4_bus,       # transition 2-4
                                     type="linear")))^(1/0.2421565))

f2to4.bus <- (F2to4.bus[-1] - F2to4.bus)/0.01

# convolution products for transition 1-2 and transition 2-4

f2to4.bus[2001] <- 0
F1to2to4.bus <- matrix(0, 1, length(x))

i<- 1000
for(i in 3:length(x)){
  for (j in 1:(i-1)) {
    F1to2to4.bus[i] <- F1to2to4.bus[i] + F1to2.bus[i-j]*f2to4.bus[j] * 0.01
  }
}


# no bus

F1to2.nobus <- 1 - exp(-(x/exp(predict(fitAFT.one.weib1to2, newdata = list_1to4_nobus,    # transition 1-2
                                     type="linear")))^(1/0.6616251))

F2to4.nobus <- 1 - exp(-(x/exp(predict(fitAFT.one.weib2to4, newdata = list_1to4_nobus,    # transition 2-4
                                     type="linear")))^(1/0.2421565))

f2to4.nobus <- (F2to4.nobus[-1] - F2to4.nobus)/0.01

f2to4.nobus[2001] <- 0
F1to2to4.nobus <- matrix(0, 1, length(x))

# convolution products

i<- 1000
for(i in 3:length(x)){
  for (j in 1:(i-1)) {
    F1to2to4.nobus[i] <- F1to2to4.nobus[i] + F1to2.nobus[i-j]*f2to4.nobus[j] * 0.01
  }
}


# additional plots 

par(mfrow=c(1,2))
plot(x, 0.3826879 * F1to4.bus , col="red",
     lwd = 2, "l", xlim = c(0,10), xlab = "Total Waiting Time", 
     ylab = "Probablity", main = "Transition 1-4", ylim = c(0,0.4))

lines(x, 0.3826879 * F1to4.nobus,col="blue", lty = 2, lwd = 2)

legend("bottomright",legend = c("Large Vehicles", "Other Vehicles"),
       lwd = c(2,2), col = c("red", "blue"), lty = c(1,2))

plot(x, 0.1605011 * F1to2to4.bus , col="red",
     lwd = 2, "l", xlim = c(0,20), xlab = "Total Waiting Time", 
     ylab = "Probablity", main = "Transition 1-2-4", ylim = c(0,0.2))

lines(x, 0.1605011 * F1to2to4.nobus,col="blue", lty = 2, lwd = 2)

legend("bottomright",legend = c("Large Vehicles", "Other Vehicles"),
       lwd = c(2,2), col = c("red", "blue"), lty = c(1,2))

## Young

list_1to4_young <- list(GroupSize = 1, Queueing = 5, bus = 0, NearFar = 1, 
                        Gender = 1, Young = 1, Adj = 0, Distance = 70, 
                        Speed = 10, Clos = 1, Distraction = 1)

list_1to4_nonyoung <- list(GroupSize = 1, Queueing = 5, bus = 0, NearFar = 1, 
                           Gender = 1, Young = 0, Adj = 0, Distance = 70, 
                           Speed = 10, Clos = 1, Distraction = 1)

# transition 1-2

F1to2.young <- 1 - exp(-(x/exp(predict(fitAFT.one.weib1to2, newdata = list_1to4_young,
                                       type="linear")))^(1/0.6616251))


F1to2.nonyoung <- 1 - exp(-(x/exp(predict(fitAFT.one.weib1to2, newdata = list_1to4_nonyoung,
                                       type="linear")))^(1/0.6616251))

F2to3.young <- 1 - exp(-(x/exp(predict(fitAFT.one.weib2to3, newdata = list_1to4_young,
                                       type="linear")))^(1/0.5632409))


F2to3.nonyoung <- 1 - exp(-(x/exp(predict(fitAFT.one.weib2to3, newdata = list_1to4_nonyoung,
                                          type="linear")))^(1/0.5632409))

# plots

par(mfrow=c(1,2))

plot(x, 0.6173121 * F1to2.young , col="red",
     lwd = 2, "l", xlim = c(0,20), xlab = "Waiting Time since Last State", 
     ylab = "Probablity", main = "Transition 1-2", ylim = c(0,0.7))

lines(x, 0.6173121 * F1to2.nonyoung,col="blue", lty = 2, lwd = 2)

legend("bottomright",legend = c("Young Group (<30)", "Other Groups (>30)"),
       lwd = c(2,2), col = c("red", "blue"), lty = c(1,2))

plot(x, 0.74 * F2to3.young , col="red",
     lwd = 2, "l", xlim = c(0,20), xlab = "Waiting Time since Last State", 
     ylab = "Probablity", main = "Transition 2-3", ylim = c(0,0.8))

lines(x, 0.74 * F2to3.nonyoung,col="blue", lty = 2, lwd = 2)

legend("bottomright",legend = c("Young Group (<30)", "Other Groups (>30)"),
       lwd = c(2,2), col = c("red", "blue"), lty = c(1,2))


############################ Two-way Estimation #############################
## distribution fits for transitions

trans1to2 <- vehicle.two[vehicle.two$State.h == 1 & vehicle.two$State.j == 2, ]
trans1to4 <- vehicle.two[vehicle.two$State.h == 1 & vehicle.two$State.j == 4, ]
trans2to3 <- vehicle.two[vehicle.two$State.h == 2 & vehicle.two$State.j == 3, ]
trans2to4 <- vehicle.two[vehicle.two$State.h == 2 & vehicle.two$State.j == 4, ]
trans3to4 <- vehicle.two[vehicle.two$State.h == 3 & vehicle.two$State.j == 4, ]

t2 <- vehicle.two[(vehicle.two$State.h == 1 & vehicle.two$State.j == 2)|
                    (vehicle.two$State.h == 1 & vehicle.two$State.j == 4)|
                    (vehicle.two$State.h == 2 & vehicle.two$State.j == 4)|
                    (vehicle.two$State.h == 2 & vehicle.two$State.j == 3)| 
                    (vehicle.two$State.h == 3 & vehicle.two$State.j == 4),
                  ]

library("MASS")
library(flexsurv)
library(survival)
library(lubridate)

# Best Parametric models comparison - Transition 1-2
fitAFT.two <- survreg(Surv(time) ~ GroupSize + Queueing + bus + NearFar +
                      Gender + Young + Adj + Distance  + CloseFoll + 
                      Distraction, dist="loglogistic", data=t2)
summary(fitAFT.two)
BIC(fitAFT.two)

# Best Parametric models comparison - Transition 1-2

fitAFT.two.loglogi <- survreg(Surv(time) ~ GroupSize + Queueing + bus + NearFar +
                              Gender + Young + Adj + Distance + Speed + CloseFoll + 
                              Distraction, dist="loglogistic", data=trans1to2)

summary(fitAFT.two.loglogi)
extractAIC(fitAFT.two.loglogi)

fitAFT.two.weib1to2 <- survreg(Surv(time) ~ GroupSize + Queueing + bus + NearFar  +
                               Gender + Young + Adj + Distance + Speed + CloseFoll + 
                               Distraction, dist="weibull", data=trans1to2)
summary(fitAFT.two.weib1to2)
extractAIC(fitAFT.two.weib1to2)

# Best Parametric models comparison - Transition 1-4

fitAFT.two.loglogi <- survreg(Surv(time) ~ GroupSize + Queueing + bus + NearFar +
                              Gender + Young + Adj + Distance + Speed + CloseFoll + 
                              Distraction, dist="loglogistic", data=trans1to4)

summary(fitAFT.two.loglogi)
extractAIC(fitAFT.two.loglogi)

fitAFT.two.weib1to4 <- survreg(Surv(time) ~ GroupSize + Queueing + bus + NearFar  +
                               Gender + Young + Adj + Distance + Speed + CloseFoll + 
                               Distraction, dist="weibull", data=trans1to4)
summary(fitAFT.two.weib1to4)
extractAIC(fitAFT.two.weib1to4)


# Best Parametric models comparison - Transition 2-3

fitAFT.two.loglogi <- survreg(Surv(time) ~ GroupSize + Queueing + bus + NearFar +
                              Gender + Young + Adj + Distance + Speed + CloseFoll + 
                              Distraction, dist="loglogistic", data=trans2to3)
summary(fitAFT.two.loglogi)
extractAIC(fitAFT.two.loglogi)
# 
fitAFT.two.weib2to3 <- survreg(Surv(time) ~ GroupSize + Queueing + bus + NearFar  +
                               Gender + Young + Adj + Distance + Speed + CloseFoll + 
                               Distraction, dist="weibull", data=trans2to3)
summary(fitAFT.two.weib2to3)
extractAIC(fitAFT.two.weib2to3)


# Best Parametric models comparison - Transition 2-4

fitAFT.two.loglogi <- survreg(Surv(time) ~ GroupSize + Queueing + bus + NearFar +
                              Gender + Young + Adj + Distance + Speed + CloseFoll + 
                              Distraction, dist="loglogistic", data=trans2to4)

summary(fitAFT.two.loglogi)
extractAIC(fitAFT.two.loglogi)
# 
fitAFT.two.weib2to4 <- survreg(Surv(time) ~ GroupSize + Queueing + bus + NearFar  +
                               Gender + Young + Adj + Distance + Speed + CloseFoll + 
                               Distraction, dist="weibull", data=trans2to4)
summary(fitAFT.two.weib2to4)
extractAIC(fitAFT.two.weib2to4)


#Best Parametric models comparison - Transition 3-4


fitAFT.two.loglogi <- survreg(Surv(time) ~ GroupSize + Queueing + bus + NearFar +
                              Gender + Young + Adj + Distance + Speed + CloseFoll + 
                              Distraction, dist="loglogistic", data=trans3to4)
summary(fitAFT.two.loglogi)
extractAIC(fitAFT.two.loglogi)

### Plot variables for two-way data -- Near Far ###

list_1to4_near <- list(GroupSize = 1, Queueing = 5, bus = 0, NearFar = 1, 
                       Gender = 1, Young = 1, Adj = 0, Distance = 70, 
                       Speed = 10, CloseFoll = 1, Distraction = 1)

list_1to4_far <- list(GroupSize = 1, Queueing = 5, bus = 0, NearFar = 2, 
                      Gender = 1, Young = 1, Adj = 0, Distance = 70, 
                      Speed = 10, CloseFoll = 1, Distraction = 1)


par(mfrow=c(1,1))

plot(predict(fitAFT.two.loglogi, newdata = list_1to4_near,
             type="quantile",p=seq(0,1,by=.01)), 0.5023148 * seq(0,1,by=.01),col="red",
             lwd = 2, "l", xlim = c(0,12), xlab = "Total Waiting Time", 
             ylab = "Probablity", main = "Transition 1-4", ylim = c(0,0.5))

lines(predict(fitAFT.two.loglogi, newdata = list_1to4_far,
              type="quantile",p=seq(0,1,by=.01)), 0.5023148 * seq(0,1,by=.01),
              col="blue", lty = 2, lwd = 2)

legend("bottomright",legend = c("Near Side Interaction", "Far Side Interaction"),
       lwd = c(2,2), col = c("red", "blue"), lty = c(1,2))


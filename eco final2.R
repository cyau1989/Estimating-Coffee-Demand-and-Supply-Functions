library(AER)
library(MASS)
#data preparation
dataall<-read.csv("C:/Users/ACER/Desktop/MSBA/Economics/Projects/Project1/Project1Data.csv")
dataall$bprice<-dataall$bprice/dataall$oprice
dataall$wprice<-dataall$wprice/dataall$oprice
dataall$tprice<-dataall$tprice/dataall$oprice
dataall$incom<-dataall$incom/dataall$oprice
dataall$cprice<-dataall$cprice/dataall$oprice
##########################################
#TSLS#linear part
##########################################
#determine the iv
demand.linear<-lm(cprice~bprice+wprice+q1+q2+q3,data=dataall)
summary(demand.linear)
#demand function for linear 2sls
demand.linear.stage1 <- lm(cprice~bprice+incom+q1+q2+q3, data=dataall)
demand.linear.iv<-ivreg(qu~q1+q2+q3+incom+cprice|incom+bprice+q1+q2+q3,data=dataall)
summary(demand.linear.iv)
#hausman test
HT1 <- lm(formula = residuals(demand.linear.iv)~ incom, data = dataall)
summary(HT1)
Rsquare1 <- summary(HT1)$r.squared
HT1_result <- Rsquare1*(84-1-1)
HT1_result
#determine the iv
supply.linear<-lm(formula=cprice~incom+tprice+q1+q2+q3,data=dataall)
summary(supply.linear)
##supply functioin for linear 2sls
supply.linear.stage1 <- lm(cprice~bprice+incom+q1+q2+q3, data=dataall)
supply.linear.iv<-ivreg(qu~q1+q2+q3+bprice+cprice|incom+q1+q2+q3+bprice,data=dataall)
summary(supply.linear.iv)
###hausman test
HT3<- lm(formula = residuals(supply.linear.iv)~incom, data = dataall)
summary(HT3)
Rsquare3<- summary(HT3)$r.squared
HT3_result <- Rsquare3*(84-2)
HT3_result  
##########################################
#TSLS#log part
##########################################
#determine the iv
demand.log<-lm(formula=log(cprice)~log(bprice)+log(wprice)+q1+q2+q3,data=dataall)
summary(demand.log)
#demand function for log 2sls
demand.linear.stage1 <- lm(log(cprice)~log(bprice)+log(incom)+q1+q2+q3, data=dataall)
demand.log.iv<-ivreg(log(qu)~q1+q2+q3+log(incom)+log(cprice)|log(bprice)+q1+q2+q3+log(incom),data=dataall)
summary(demand.log.iv)
#hausman test
HT2<- lm(formula = residuals(demand.log.iv) ~ log(bprice), data = dataall)
summary(HT2)
Rsquare2 <- summary(HT2)$r.squared
HT2_result <- Rsquare2*(84-1-1)
HT2_result
#determine the iv
supply.log<-lm(formula=log(cprice)~log(tprice)+log(incom)+q1+q2+q3,data=dataall)
summary(supply.log)
#supply function for log 2sls
supply.linear.stage1 <- lm(log(cprice)~log(bprice)+log(incom)+q1+q2+q3, data=dataall)
supply.log.iv<-ivreg(log(qu)~q1+q2+q3+log(bprice)+log(cprice)|log(bprice)+q1+q2+q3+log(incom),data=dataall)
summary(supply.log.iv)
#hausman test
HT4<- lm(formula = residuals(supply.log.iv)~log(incom), data = dataall)
summary(HT4)
Rsquare4 <- summary(HT4)$r.squared
df4 <- HT4$df.residual
HT4_result <- Rsquare4*(84-1-1)
HT4_result
##########################################
#OLS part
##########################################
#determine the  demand function for linear
demand.linear.ols<-lm(qu~cprice+incom+q1+q2+q3,data=dataall)
summary(demand.linear.ols)
###supply function for linear ols
supply.linear.ols<-lm(formula = qu~cprice+q1+q2+q3+bprice,data=dataall)
summary(supply.linear.ols)
#demand function for log ols
demand.log.ols<-lm(formula=log(qu)~log(cprice)+q1+q2+q3+log(incom),data = dataall)
summary(demand.log.ols)
#supply function for log ols
supply.log.ols<-lm(formula = log(qu)~log(cprice)+q1+q2+q3+log(bprice),data=dataall)
summary(supply.log.ols)



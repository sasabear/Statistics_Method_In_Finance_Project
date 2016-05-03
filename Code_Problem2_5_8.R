##########################################
#Problem 2 
##########################################
setwd("/Users/jiahonghu/Desktop/project4290/code")

########
#Import Data 
#######

data_raw=read.csv(file="data.csv",header=TRUE)
data_raw=as.data.frame(data_raw)
names(data_raw)
nrow(data_raw) # number of total oberservations for each time series of stock price 

# train data set 
data=data_raw[data_raw$TYPE==0,] 
data=as.data.frame(data)
n=nrow(data) # number of training data
n

# test data set 
data_test=data_raw[data_raw$TYPE==1,] 
data_test=as.data.frame(data_test)
m=nrow(data_test) # number of test data
m

# dataset for 9 stocks and S&P 500
data_new=data[,4:13] 
nme=names(data)[4:13]
names(data_new)=nme
data_new=as.data.frame(data_new)

# time series data 
ts1<-ts(data_new[,1],frequency = 12,start=c(2010,6))
ts2<-ts(data_new[,2],frequency = 12,start=c(2010,6))
ts3<-ts(data_new[,3],frequency = 12,start=c(2010,6))
ts4<-ts(data_new[,4],frequency = 12,start=c(2010,6))
ts5<-ts(data_new[,5],frequency = 12,start=c(2010,6))
ts6<-ts(data_new[,6],frequency = 12,start=c(2010,6))
ts7<-ts(data_new[,7],frequency = 12,start=c(2010,6))
ts8<-ts(data_new[,8],frequency = 12,start=c(2010,6))
ts9<-ts(data_new[,9],frequency = 12,start=c(2010,6))
ts10<-ts(data_new[,10],frequency = 12,start=c(2010,6))

par(mfrow=c(3,3))
plot(ts1,col="blue",main=paste("Stock Price of ",nme[1]))
plot(ts2,col="blue",main=paste("Stock Price of ",nme[2]))
plot(ts3,col="blue",main=paste("Stock Price of ",nme[3]))
plot(ts4,col="blue",main=paste("Stock Price of ",nme[4]))
plot(ts5,col="blue",main=paste("Stock Price of ",nme[5]))
plot(ts6,col="blue",main=paste("Stock Price of ",nme[6]))
plot(ts7,col="blue",main=paste("Stock Price of ",nme[7]))
plot(ts8,col="blue",main=paste("Stock Price of ",nme[8]))
plot(ts9,col="blue",main=paste("Stock Price of ",nme[9]))


#create the monthly return matrix for 9 stocks and S&P 500 


data_ret=matrix(data=NA,ncol=10,nrow=60) ## monthly return of 9 stock and S&P 500 

for(i in 1:10){
        return=data_new[2:n,i]/data_new[1:n-1,i]-1
        data_ret[,i]=return
        
}

data_ret=as.data.frame(data_ret)
names(data_ret)=nme

#calcuated monthly risk-free rate from annualzed risk free rate 
data_rf=data[,14]/12 
mean(data_rf) #average risk-free rate 





#############
#Part 2.1 Sample statistics 
#############
apply(data_new,2,mean) # mean,median, min, max of stock adjusted closing price 
apply(data_new,2,sd) # sd of stock closing price 

apply(data_ret,2,mean) # mean of monthly return
apply(data_ret,2,sd) # volatility of monthly return

12*apply(data_ret,2,mean)*100 # annulized mean return
sqrt(12)*apply(data_ret,2,sd)*100 # annulized volatility of monthly return

library(moments)
skewness(data_ret)  #  degree of symmetry --0 
kurtosis(data_ret)  #  p87 - probability concentrated in center and tails - a high kurtosis is having heavy tailed dist

#check for equity curve 

par(mfrow=c(2,5))

for( i in 1:10){
        ec=cumprod(1+data_ret[,i])
        ec_sp500=cumprod(1+data_ret[,10])
        ec_matrix=cbind(ec,ec_sp500)
        matplot(1:60,ec_matrix,ylab="Cummulative Return",co=c("blue","red"),lwd=2,type="l",main=nme[i],xlab="Time")
        legend("topright",lty=1,legend = (c("stock density","normal density")),col=c("blue","red"),cex=0.10)
        
}




##########
#part 2.2 distritbuion
##########

# check normality 
par(mfrow=c(2,5))
for(i in 1:10){
     hist(data_ret[,i],prob=TRUE,xlab="monthly return", main=paste(nme[i], " Monthly Return"),col="grey", breaks=30)
     lines(density(data_ret[,i]),col="blue")  # density curve of monthly return
     m=mean(data_ret[,i])
     std=sd(data_ret[,i])
     curve(dnorm(x,mean=m,sd=std),col="red",add=TRUE) # normal curve 
     legend("topright",lty=1,legend = (c("stock density","normal density")),col=c("blue","red"),cex=0.20)

}

library(tseries)
index.names=dimnames(data_ret)[[2]]
test_s<-NULL
test_j<-NULL
par(mfrow=c(2,5))
for( i in 1:10){
        qqnorm(data_ret[,i],datax=T,main=index.names[i],col="blue")
        qqline(data_ret[,i],datax=T,col="red")
        print(shapiro.test(data_ret[,i]))
        print(jarque.bera.test(data_ret[,i]))
}


# boxplot - check for outlier 

index.names=dimnames(data_ret)[[2]]
out_vect=NULL
par(mfrow=c(2,5))

for( i in 1:10){
        boxplot(data_ret[,i],datax=T,main=index.names[i])
}

for( i in 1:10){
        a=boxplot(data_ret[,i],datax=T,main=index.names[i])
        print(a$out)
}  # outlier 



# fit distributions 

library(fGarch)
library(fitdistrplus)
AIC<-matrix(0,ncol=6,nrow=10)
index<-vector()

for (i in 1:10){
        
        fit1<-sstdFit(data_ret[,i],hessian=T) ###skewed t-dist
        fit2<-stdFit(data_ret[,i])  ####standard t-dist
        fit3<-gedFit(data_ret[,i])  ####generalized error dist
        fit4<-sgedFit(data_ret[,i])  #####skewed ged
        fit5<-snormFit(data_ret[,i]) #skewed normal
        fit6<-fitdist(data_ret[,i],"norm","mle")
        
        
        AIC[i,1] = 2 * fit1$minimum + 2*length(fit1$estimate)
        AIC[i,2] = 2 * fit2$objective + 2*length(fit2$par)
        AIC[i,3] = 2 * fit3$objective + 2*length(fit3$par)
        AIC[i,4] = 2 * fit4$objective + 2*length(fit4$par)
        AIC[i,5] = 2 * fit5$objective + 2*length(fit5$par) 
        AIC[i,6]= fit6$aic
        index[i] = which.min(AIC[i,])
}

### the result fitted distribution 

par(mfrow=c(1,2))
ret_ind<-seq(-0.5,0.5,length=1000)

# CIE
fit_sget<-sgedFit(data_ret[,7])
y_sget<-dsged(ret_ind, mean = fit_sget$par[1], sd = fit_sget$par[2], nu = fit_sget$par[3],xi=fit_sget$par[4])
plot(density(data_ret[,7]),main="CIE with SGET Dist",xlab="returns",ylim=c(0,5))
points(ret_ind,y_sget,type="l",col="red")
legend("topright",c("Sample Density","Fitted distribution"),fill=c("black","red"),cex=0.20)

#BMP

fit_std<-stdFit(data_ret[,9])
y_std<-dstd(ret_ind, mean = fit_std$par[1], sd = fit_std$par[2], nu = fit_std$par[[3]])
plot(density(data_ret[,9]),main="SMG with STD Dist",xlab="returns",ylim=c(0,8))
points(ret_ind,y_std,type="l",col="red")
legend("topright",c("Sample Density","Fitted distribution"),fill=c("black","red"),cex=0.20)

#################
# 5 PCA 
#################
# plot of return of each stock  and S&P 500 


par(mfrow=c(1,1))
plot(data_ret[,10],type="l",col="blue",xlab="Month",ylab="Return",main="Monthly Return of S&P 500") # plot of S&P 500 
abline(h=0,col="red")

par(mfrow=c(3,3))

for(i in 1:9){
        matplot(1:60,data_ret[,c(i,10)],type="l",col=c("black","red"),xlab="Month",ylab="Monthly Return",main=paste(nme[i],"Monthly Return"))
        abline(h=0,col="blue")
        #legend("topright",lty=1,legend = (c("GOOGL","S&P 500")),col=c("black","red"),cex=0.05)
}




## check for stationary 

library("tseries")
pvalue_vect=NULL


for( i in 1:10){
        a=adf.test(data_ret[,i])
        pvalue_vect[i]=a$p.value
}

pvalue_vect


# correlation between pairwise stock price and s&p 500 
## mostly have positive correlation -- may not good for hedging 
cor_p=as.data.frame(cor(data_ret))  # by pearson's product moments correlation efficient 
cor_k=as.data.frame(cor(data_ret,method="kendall")) # non-parametric method, less sensetive to outlier 
cor_s=as.data.frame(cor(data_ret,method="spearman" ))
pairs(data_ret,col="blue")

###############
## 7 Copula 
##############

### copulas 



AIC_IND=NULL
library(copula)
edata<-cbind(rank(data_ret[,1])/61,rank(data_ret[,2])/61,rank(data_ret[,3])/61,rank(data_ret[,4])/61,
             rank(data_ret[,5])/61,rank(data_ret[,6])/61,rank(data_ret[,7])/61,rank(data_ret[,8])/61,rank(data_ret[,9])/61)

#fit normal copula
ncop<-normalCopula(param=rep(0,36), dim=9, dispstr="un")
fn<-fitCopula(data=edata, copula=ncop, method="ml")
fn_AIC<--2*fn@loglik+2*length(fn@estimate)
AIC_IND[1]=fn_AIC

#fit tcopula
tcop<-tCopula(param=rep(0,36), dim=9, dispstr="un", df=5)
ft<-fitCopula(data=edata, copula=tcop, method="ml")
ft_AIC<--2*ft@loglik+2*length(ft@estimate)
AIC_IND[2]=ft_AIC
#fit clayton
clcop<-archmCopula(family="clayton", dim=9, param=2)
fc<-fitCopula(data=edata, copula=clcop, method="ml")
fc_AIC<--2*fc@loglik+2*length(fc@estimate)
AIC_IND[3]=fc_AIC
#fit gumbel
gcop<-archmCopula(family="gumbel", dim=9, param=2)
fg<-fitCopula(data=edata,method="ml", copula=gcop)
fg_AIC<--2*fg@loglik+2*length(fg@estimate)
AIC_IND[4]=fg_AIC

which.min(AIC_IND)


cop.best <- archmCopula(family="gumbel",param=fg@estimate, dim=9)

dist1<-fitdist(data_ret[,1],"norm","mle")
dist2<-fitdist(data_ret[,2],"norm","mle")
dist3<-fitdist(data_ret[,3],"norm","mle")
dist4<-fitdist(data_ret[,4],"norm","mle")
dist5<-fitdist(data_ret[,5],"norm","mle")
dist6<-fitdist(data_ret[,6],"norm","mle")
dist7<-sgedFit(data_ret[,7])
dist8<-fitdist(data_ret[,8],"norm","mle")
dist9<-stdFit(data_ret[,9])

myMvd = mvdc(copula = cop.best, margins = c("norm", "norm","norm","norm","norm","norm","sged", "norm","std")
             , paramMargins = list(list(mean = dist1$estimate[[1]],sd = dist1$estimate[[2]]), 
                                   list(mean = dist2$estimate[[1]],sd = dist1$estimate[[2]]),
                                   list(mean = dist3$estimate[[1]],sd = dist1$estimate[[2]]),
                                   list(mean = dist4$estimate[[1]],sd = dist1$estimate[[2]]),
                                   list(mean = dist5$estimate[[1]],sd = dist1$estimate[[2]]),
                                   list(mean = dist6$estimate[[1]],sd = dist1$estimate[[2]]),
                                   list(mean = dist7$par[1],sd = dist7$par[2],nu=dist7$par[3],xi=dist7$par[4]),
                                   list(mean = dist8$estimate[[1]],sd = dist1$estimate[[2]]),
                                   list(mean = dist9$par[1],sd = dist9$par[2],nu=dist9$par[3])))


##Use the fitted joint distribution of the returns to compute VaR(0.05) and ES
par(mfrow=c(1,1))

samples=rmvdc(myMvd, 10000)

t_weight=c(0.1113,-0.181,0.142,0.121,0.563,0.186,-0.0618,0.159,-0.04)
ret_est<-samples%*%(t_weight)
sample_ret<-as.matrix(data_ret[,1:9])%*%t_weight
plot(density(sample_ret),main="Portfolio Returm by Copulas",xlab="returns")
points(density(ret_est),type="l",col="red")
legend("topright",c("Data","Fitted Dist"),fill=c("black","red"),cex=0.3)

s = 100000
VaR_est<-NULL
ES_est<-NULL

for(i in 1:1000){
        samples=rmvdc(myMvd, 10000)
        ret_est<-samples%*%(t_weight)
        VaR_est[i]<--s*quantile(ret_est,prob=0.05)
        ES_est[i] = - s * sum(ret_est * (ret_est <=quantile(ret_est, probs = 0.05) )) 
        sum(ret_est <=quantile(ret_est, probs = 0.05))
}

q_l_VaR = quantile(VaR_est, 0.025)
q_u_VaR = quantile(VaR_est, 0.975)
CI_u_VaR = 2 * mean(VaR_est) - q_u_VaR
CI_l_VaR = 2 * mean(VaR_est) - q_l_VaR



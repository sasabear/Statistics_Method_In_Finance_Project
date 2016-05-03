dat = read.csv("Data.csv",header=T)

R[,1]=R=matrix(,nrow=61,ncol=9)
R[,1]=dat$GOOGL[7:67]
R[,2]=dat$IBM[7:67]
R[,3]=dat$AMZN[7:67]
R[,4]=dat$JNJ[7:67]
R[,5]=dat$PFE[7:67]
R[,6]=dat$MRK[7:67]
R[,7]=dat$CIE[7:67]
R[,8]=dat$CVI[7:67]
R[,9]=dat$BWP[7:67]

returns=(R[1:60,]/R[2:61,]-1);
mean_vect = apply(returns,2,mean);
sdP=apply(returns,2,sd)
VaR=rep(0,9)
ES=rep(0,9)

S=100000;

for (i in 1:9)
{
  VaR[i] = -S*(mean_vect[i]+qnorm(.05,0,1)*sdP[i])
  ES[i] = S*(-mean_vect[i]+sdP[i]*(dnorm(qnorm(.05,0,1),0,1))/.05)
  
}  

returns2=matrix(,nrow=60,ncol=9)

for (i in 1:9)
{
  returns2[,i]=sort(returns[,i]);
  
}

npVaR=-S*returns2[3,];
npES=-S*apply(returns2[1:3,],2,mean)

n=1000;

bsVaR=matrix(,nrow=n,ncol=9)
bsES=matrix(,nrow=n,ncol=9)
bsnpVaR=matrix(,nrow=n,ncol=9)
bsnpES=matrix(,nrow=n,ncol=9)

for (i in 1:9)
{
  for (j in 1:n)
  {
    temp_returns=sample(returns[,i],60,replace = TRUE);
    mu=mean(temp_returns)
    sigma=sd(temp_returns)
    bsVaR[j,i]= -S*(mu+qnorm(.05,0,1)*sigma)
    bsES[j,i]=S*(-mu+sigma*(dnorm(qnorm(.05,0,1),0,1))/.05)
    temp_returns2=sort(temp_returns)
    bsnpVaR[j,i]=-S*temp_returns2[3];
    bsnpES[j,i]=-S*mean(temp_returns2[1:3]);
  }
}

SEbsVaR=apply(bsVaR,2,sd)
SEbsES=apply(bsES,2,sd)
SEbsnpVaR=apply(bsnpVaR,2,sd)
SEbsnpES=apply(bsnpES,2,sd)

ciVaRu=VaR+1.96*SEbsVaR;
ciESu=ES+1.96*SEbsES;
cinpVaRu=npVaR+1.96*SEbsnpVaR;
cinpESu=npES+1.96*SEbsnpES;

ciVaRl=VaR-1.96*SEbsVaR;
ciESl=ES-1.96*SEbsES;
cinpVaRl=npVaR-1.96*SEbsnpVaR;
cinpESl=npES-1.96*SEbsnpES;

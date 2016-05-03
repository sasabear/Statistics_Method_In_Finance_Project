dat = read.csv("Data.csv",header=T)

library(Ecdat)
library(quadprog)

R=matrix(,nrow=61,ncol=9)
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

mean_vect = apply(returns,2,mean)
cov_mat = cov(returns)

required_return=0.005;
Amat = cbind(rep(1,3),mean_vect,diag(1,nrow=9))

bvec = c(1,required_return,rep(0,9))  
result =
  solve.QP(Dmat=2*cov_mat,dvec=rep(0,9),Amat=Amat,bvec=bvec,meq=2)

sdP = sqrt(result$value)
weights = result$solution

S=100000;

return_portf=matrix(,nrow=60,ncol=9)

for (i in 1:9)
{
  return_portf[,i]=weights[i]*returns[,i];
}

return_portf=apply(return_portf,1,sum)

return_portf2=sort(return_portf)
npVaR=-S*return_portf2[3]
npES=-S*mean(return_portf2[1:3])

###With risk-free T-Bill

sd_vect = sqrt(diag(cov_mat))
Amat = cbind(rep(1,3),mean_vect,diag(1,nrow=9))  
muP = seq(min(mean_vect)+.0001,max(mean_vect)-.0001,length=300)  


sdP = muP 
weights = matrix(0,nrow=300,ncol=9) 
for (i in 1:length(muP)) 
  
{
  bvec = c(1,muP[i],rep(0,9))  
  result =
    solve.QP(Dmat=2*cov_mat,dvec=rep(0,9),Amat=Amat,bvec=bvec,meq=2)
  sdP[i] = sqrt(result$value)
  weights[i,] = result$solution
}

mufree = mean(dat$RFF[7:61]/100)/12
sharpe =( muP-mufree)/sdP 
ind = (sharpe == max(sharpe)) 
options(digits=3)
weights[ind,] 

w=(.005-mufree)/(muP[ind]-mufree)
sigma_optimal=w*sdP[ind]

return_portf3=matrix(,nrow=60,ncol=9)

for (i in 1:9)
{
  return_portf3[,i]=returns[,i]*weights[ind,i] 
}

return_portf3=apply(return_portf3,1,sum)

return_portf4=sort(return_portf3)
npVaR=-w*S*return_portf4[3]-(1-w)*S*mufree
npES=-w*S*mean(return_portf4[1:3])-(1-w)*S*mufree

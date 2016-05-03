
######## Question 3 ########
rm(list=ls())
data= read.csv("data.csv")
data=data[7:67,]
library(Ecdat)
library(quadprog)

## Now Create the return table for portfolio

return.google=vector()
for(i in 1:(length(data$GOOGL)-1)){
  return.google[i]=(data$GOOGL[[i]]-data$GOOGL[[i+1]])/data$GOOGL[[i+1]]
}
return.google=c(0,return.google)


return.ibm=vector()
for(i in 1:(length(data$IBM)-1)){
  return.ibm[i]=(data$IBM[[i]]-data$IBM[[i+1]])/data$IBM[[i+1]]
}
return.ibm=c(0,return.ibm)


return.amazon=vector()
for(i in 1:(length(data$AMZN)-1)){
  return.amazon[i]=(data$AMZN[[i]]-data$AMZN[[i+1]])/data$AMZN[[i+1]]
}
return.amazon=c(0,return.amazon)


return.jnj=vector()
for(i in 1:(length(data$JNJ)-1)){
  return.jnj[i]=(data$JNJ[[i]]-data$JNJ[[i+1]])/data$JNJ[[i+1]]
}
return.jnj=c(0,return.jnj)


return.pfe=vector()
for(i in 1:(length(data$PFE)-1)){
  return.pfe[i]=(data$PFE[[i]]-data$PFE[[i+1]])/data$PFE[[i+1]]
}
return.pfe=c(0,return.pfe)


return.mrk=vector()
for(i in 1:(length(data$MRK)-1)){
  return.mrk[i]=(data$MRK[[i]]-data$MRK[[i+1]])/data$MRK[[i+1]]
}
return.mrk=c(0,return.mrk)


return.cie=vector()
for(i in 1:(length(data$CIE)-1)){
  return.cie[i]=(data$CIE[[i]]-data$CIE[[i+1]])/data$CIE[[i+1]]
}
return.cie=c(0,return.cie)



return.cvi=vector()
for(i in 1:(length(data$CVI)-1)){
  return.cvi[i]=(data$CVI[[i]]-data$CVI[[i+1]])/data$CVI[[i+1]]
}
return.cvi=c(0,return.cvi)



return.bwp=vector()
for(i in 1:(length(data$BWP)-1)){
  return.bwp[i]=(data$BWP[[i]]-data$BWP[[i+1]])/data$BWP[[i+1]]
}
return.bwp=c(0,return.bwp)


return.sp500=vector()
for(i in 1:(length(data$"SP500")-1)){
  return.sp500[i]=(data$SP500[[i]]-data$SP500[[i+1]])/data$SP500[[i+1]]
}
return.sp500=c(0,return.sp500)

return.table = cbind(return.google,return.ibm,return.amazon,return.jnj,return.pfe,return.mrk,return.cie,return.cvi,return.bwp,return.sp500)
colnames(return.table)=c("GOOGL Return","IBM Return","AMZN Return", "JNJ Return", "PFE Return", "MRK Return", "CIE Return", "CVI Return", "BWF Return","SP500 Return") 
data=cbind(data[,1:11],return.table,data[,12])
colnames(data)[22]="RFR"

R=100*data[-1,12:20]
mean.vect=apply(R,2,mean)
cov.mat=cov(R)
sd.vect=sqrt(diag(cov.mat))
Amat=cbind(rep(1,9),mean.vect)
muP=seq(min(mean.vect)+.0001,max(mean.vect)-.0001,length=500)
sdP=muP
weights=matrix(0,nrow=5000,ncol=9)


#### Get the weight for each target portfolio returns ###

for(i in 1:length(muP)){
  bvec=c(1,muP[i])
  result=solve.QP(Dmat=2*cov.mat,dvec = rep(0,9),Amat=Amat,bvec=bvec,meq=2)
  sdP[i]=sqrt(result$value)
  weights[i,]=result$solution
}

plot(sdP,muP,type="l",lty=3,xlim=c(0,14),ylim=c(-3,6),main="Efficient Frontier with Short Sale") 
mu.free=mean(data[,22])/12
points(0,mu.free,cex=2,pch="*")
sharpe=(muP-mu.free)/sdP
ind=(sharpe==max(sharpe))
options(digits=3)
weights[ind,]
lines(c(0,11),mu.free+c(0,11)*(muP[ind]-mu.free)/sdP[ind],lwd=2,lty=2)
points(sdP[ind],muP[ind],cex=2,pch="*")
 
##### Find The Mininum Variance Portfolio ####
ind2=(sdP==min(sdP))
points(sdP[ind2],muP[ind2],pch="+")  # giving the minium variance portfolio
ind3 = (muP > muP[ind2])
ind3 = (muP > muP[ind2])
lines(sdP[ind3],muP[ind3],type="l",xlim=c(0,10),ylim=c(-3,6),lwd=2)  #  plot the efficient frontier
text(sd.vect[1],mean.vect[1],"GOOGL",cex=.5)
text(sd.vect[2],mean.vect[2],"IBM",cex=.5)
text(sd.vect[3],mean.vect[3],"AMZN",cex=.5)
text(sd.vect[4],mean.vect[4],"JNJ",cex=.5)
text(sd.vect[5],mean.vect[5],"PFE",cex=.5)
text(sd.vect[6],mean.vect[6],"MRK",cex=.5)
text(sd.vect[7],mean.vect[7],"CIE",cex=.5)
text(sd.vect[8],mean.vect[8],"CVI",cex=.5)
text(sd.vect[9],mean.vect[9],"BWF",cex=.5)

##### MVP with Normal Assumption #####
weights[ind2,] # giving the weights of the MVP 
muP[ind2] # giving the expected return of MVP
annul.mvp.return=muP[ind2]*12 # giving the annulized return for MVP
annul.mvp.risk=sdP[ind2]*sqrt(12) #giving the annulized risk for MVP
mu.VaR=-annul.mvp.return*100000
sd.VaR=annul.mvp.risk*100000
VaR.mvp.normal=(mu.VaR+sd.VaR*qnorm(0.95))/12

# mvp expected return and sd with annulized expected return and sd
return.sd.matrix= cbind(muP[ind2],sdP[ind2],annul.mvp.return,annul.mvp.risk)
colnames(return.sd.matrix)=c("mvp mean return","mvp standard deviation","annual mean return","annual standard deviation")
return.sd.matrix

# Using Non-parametric method #
mvp.return=as.matrix(as.matrix(data[-1,12:20])%*%as.matrix(weights[ind2,])[1,])
VaR.mvp.nonparametric=-100000*quantile(mvp.return[,1],prob=c(0.05))


### Now Calculate VAR for Each Stock under Normal Assumption ###
annul.return=mean.vect*12
annul.risk=sd.vect*sqrt(12)
VaR.vect.normal=vector()
for(i in 1: length(annul.return)){
  VaR.vect.normal[i]=(-annul.return[i]*100000+annul.risk[i]*100000*qnorm(0.95))/12
}

### Now Calculate VAR for Each Stock Using Non-parametric Method ###
Var.vect.nonparametric=vector()
for(i in 1:length(mean.vect)){
  Var.vect.nonparametric[i]=-100000*quantile(R[,i],prob=c(0.05))
}

compare.var=as.matrix(cbind(VaR.vect.normal,VaR.mvp.normal,Var.vect.nonparametric,VaR.mvp.nonparametric))
rownames(compare.var)=c("GOOGL","IBM","AMZN","JNJ","PFE","MRK","CIE","CVI","BWF")
colnames(compare.var)=c("VaR Normal","VaR MVP Normal","VaR Non-parametric","VaR MVP Non-parametric")
compare.var

####### Compare VaR for MVP and Individual ######

compare.var

# Based on this output, we have seen that the MVP giving the smallest VaR
# Then followed by medicine firms and tech firms. Energy firms generally
# have really high risk which result in higher VaR. The retults under
# normall assumption and non-parametric method are generally same.

##### For no short sell #####
muP.noshort=seq(min(mean.vect)+.0001,max(mean.vect)-.0001,length=5000)
sdP.noshort=muP.noshort
weights.noshort=matrix(0,nrow=5000,ncol=9)
sd.noshort=rep(0,length(sd.vect))
D.mat=2*cov.mat
d.vec=rep(0,9)
A.mat=cbind(rep(1,9),mean.vect,diag(1,nrow=9))


#### Get the weight for each target portfolio returns ###

for(i in 1:length(muP.noshort)){
  bvec=c(1,muP.noshort[i],rep(0,9))
  result.noshort=solve.QP(Dmat=2*cov.mat,dvec = d.vec,Amat=A.mat,bvec=bvec,meq=2)
  sdP.noshort[i]=sqrt(result.noshort$value)
  weights.noshort[i,]=result.noshort$solution
}

plot(sdP.noshort,muP.noshort,type="l",lty=3,xlim=c(-5,20),ylim=c(-2,6),main="Efficient Frontier without Short Sale")
mu.free=mean(data[,22])/12
points(0,mu.free,cex=2,pch="*")
sharpe.noshort=(muP.noshort-mu.free)/sdP.noshort
ind.noshort=(sharpe.noshort==max(sharpe.noshort))
options(digits=3)
weights.noshort[ind.noshort,]
lines(c(0,11),mu.free+c(0,11)*(muP.noshort[ind.noshort]-mu.free)/sdP.noshort[ind.noshort],lwd=2,lty=2)
points(sdP.noshort[ind.noshort],muP.noshort[ind.noshort],cex=2,pch="*")

# Find MVP for no short sale

ind2.noshort=(sdP.noshort==min(sdP.noshort))
points(sdP.noshort[ind2.noshort],muP.noshort[ind2.noshort],pch="+")  # giving the minium variance portfolio
ind3.noshort = (muP.noshort > muP.noshort[ind2.noshort])
lines(sdP.noshort[ind3.noshort],muP.noshort[ind3.noshort],type="l",xlim=c(0,10),ylim=c(-3,6),lwd=2)
text(sd.vect[1],mean.vect[1],"GOOGL",cex=.5)
text(sd.vect[2],mean.vect[2],"IBM",cex=.5)
text(sd.vect[3],mean.vect[3],"AMZN",cex=.5)
text(sd.vect[4],mean.vect[4],"JNJ",cex=.5)
text(sd.vect[5],mean.vect[5],"PFE",cex=.5)
text(sd.vect[6],mean.vect[6],"MRK",cex=.5)
text(sd.vect[7],mean.vect[7],"CIE",cex=.5)
text(sd.vect[8],mean.vect[8],"CVI",cex=.5)
text(sd.vect[9],mean.vect[9],"BWF",cex=.5)
#Giving the MVP Weights
weights.noshort[ind2.noshort,]
sum(weights.noshort[ind2.noshort,]) # test the contraint so that the weigts sum to 1

##### Compare the MVP with Individual Assets ####

# under normal assumption
weights.noshort[ind2.noshort,] # giving the weights of the MVP 
muP.noshort[ind2.noshort] # giving the expected return of MVP
annul.mvp.return.noshort=muP.noshort[ind2.noshort]*12 # giving the annulized return for MVP
annul.mvp.risk.noshort=sdP.noshort[ind2.noshort]*sqrt(12) #giving the annulized risk for MVP
mu.VaR.noshort=-annul.mvp.return.noshort*100000
sd.VaR.noshort=annul.mvp.risk.noshort*100000
VaR.mvp.normal.noshort=(mu.VaR.noshort+sd.VaR.noshort*qnorm(0.95))/12

## mvp expected return and sd with annulized expected return and sd no short sale
return.sd.matrix.noshort= cbind(muP.noshort[ind2.noshort],sdP.noshort[ind2.noshort],annul.mvp.return.noshort,annul.mvp.risk.noshort)
colnames(return.sd.matrix.noshort)=c("mvp mean return","mvp standard deviation","annual mean return","annual standard deviation")
return.sd.matrix.noshort
final.return.sd.matrix=rbind(return.sd.matrix,return.sd.matrix.noshort)
rownames(final.return.sd.matrix)=c("Allowing Short Sale","No Short Sale")
final.return.sd.matrix

# Using Non-parametric method #
mvp.return.noshort=as.matrix(as.matrix(data[-1,12:20])%*%as.matrix(t(weights.noshort[ind2.noshort,])[1,]))
VaR.mvp.nonparametric.noshort=-100000*quantile(mvp.return.noshort[,1],prob=c(0.05))


### Now Calculate VAR for Each Stock under Normal Assumption ###
annul.return.noshort=mean.vect*12
annul.risk.noshort=sd.vect*sqrt(12)
VaR.vect.normal.noshort=vector()
for(i in 1: length(annul.return.noshort)){
  VaR.vect.normal.noshort[i]=(-annul.return.noshort[i]*100000+annul.risk.noshort[i]*100000*qnorm(0.95))/12
}

### Now Calculate VAR for Each Stock Using Non-parametric Method ###
Var.vect.nonparametric.noshort=vector()
for(i in 1:length(mean.vect)){
  Var.vect.nonparametric.noshort[i]=-100000*quantile(R[-1,][,i],prob=c(0.05))
}

compare.var.noshort=as.matrix(cbind(VaR.vect.normal.noshort,VaR.mvp.normal.noshort,Var.vect.nonparametric.noshort,VaR.mvp.nonparametric.noshort))
rownames(compare.var.noshort)=c("GOOGL","IBM","AMZN","JNJ","PFE","MRK","CIE","CVI","BWF")
colnames(compare.var.noshort)=c("Normal No-short","MVP Normal No-short","Non-parametric No-short","MVP Non-parametric No-short")
compare.var.noshort

#### Comapre the Results ####
compare.var.noshort




##### Summary #####

# the weights of minimum variance portfolio (This is the first question)

weights.mvp.matrix=rbind(weights[ind2,][1,],weights.noshort[ind2.noshort,])
colnames(weights.mvp.matrix)=c("GOOGL","IBM","AMZN","JNJ","PFE","MRK","CIE","CVI","BWF")
rownames(weights.mvp.matrix)=c("Allowing Short Sale","No Short Sale")
weights.mvp.matrix[2,7]=0
weights.mvp.matrix[2,8]=0
weights.mvp.matrix=as.data.frame(weights.mvp.matrix)
round(weights.mvp.matrix,digits=3)


# Comments #

# We have seen that in both condition (allow short sale and no allow short sale), the MVP has put 
# more weights on the pharmaceutical industry stock. In both condition, the total weights of the
# pharmaceutical stock for allowing short sale and not allowing short sale is about 67.6% and 65.8%  
# respectivly. Also, the pharmaceutical stock generally has the smaller volatility comapre to the 
# technology and energy industry stock we have chosen. On the other hand, mvp in both condition has
# put more weights on technology stocks such as googlem, amazon, and IBM, and there is almost no 
# weights on energy stocks. According to the volatility of each stock we have computed, technology
# industry stock as a group generally has 2nd lowest volatility compare to pharmaceutical and energy
# industry stock which explained why MVP has put more weights on technology industry stock than energy
# stock. 

# the mean return and standard deviation of MVP (monthly and annually)
return.sd.mvp.matrix=rbind(return.sd.matrix,return.sd.matrix.noshort)
colnames(return.sd.mvp.matrix)=c("Mean_Return", "Standard_Deviation","Annual_Mean_Return","Annual_Standard_Deviation")
rownames(return.sd.mvp.matrix)=c("Allowing Short Sale","No Short Sale")
return.sd.mvp.matrix

# compare the mvp (with short sale) mean return , sd with individual stock return and mean
compare.mean.sd=cbind(mean.vect*12,return.sd.mvp.matrix[1,3],sd.vect*sqrt(12),return.sd.mvp.matrix[1,4])
colnames(compare.mean.sd)=c("Individual_Mean_Return","MVP_Mean_Return","Individual_Standard_Deviation","MVP_Standard_Devivation")
compare.mean.sd

# As the table showing the individual stock's mean return and standard deviation on the 1st 
# and 3rd colum compared with mean return and standard deviation of mvp on 2nd and 4th colum
# we have seen that JNJ and MRK have almost same mean return with mvp. In generaly, all stocks
# in pharmaceutical group have mean return close to mvp. Also, their standard deviation is closer
# to mvp standard deviation as well. Technology industry stocks have higher mean returns compare
# to the mvp mean return except for IBM. Meanwhile, technology stocks generally have higher 
# standard deviation than mvp. Last, the energy sector have giving inconsistant performance.
# CIE and CVI showed higher return than mvp. The CVI has remarkbly high return 51.27 which is
# almost 4 times of mvp return. Howver, CVI also has the 2nd largest standard deviation which is
# 5 times of mvp standard deviation. BWF has the worst performance over all the other stocks. It
# result in negative return but still has really high volatility. In summary,the mean return and
# standard deviation comparsion showed that energy sector has highest return, and techonology 
# insdusty has 2nd highest return. The pharmaceutical sector has lowest return really close to
# mvp. However, the risk is on the fliped side. We ranked the volatility from higher to low:
# energy sector, technology sector, pharmaceutical sector. 




# Value at Risk
# 0.05 VaR for both allowing short sale and no short sale
# Here we are using non-paramatrix mehtod

VaR.matrix=cbind(Var.vect.nonparametric,VaR.mvp.nonparametric,Var.vect.nonparametric.noshort,VaR.mvp.nonparametric.noshort)
rownames(VaR.matrix)=c("GOOGL","IBM","AMZN","JNJ","PFE","MRK","CIE","CVI","BWF")
colnames(VaR.matrix)=c("Individual_VaR_with_Short","MVP_VaR_with_Short","Individual_VaR_without_Short","MVP_VaR_Without_Short")
VaR.matrix
Var.vect.nonparametric
Var.vect.nonparametric.noshort

#### Comment ####

# All individual VaR are much larger than MVP VaR regardless under short sale or not.
# The VaR for MVP under shor sale is greater than without short sale.



#### Tangency portfolio ####
weights.tangency=rbind(weights[ind,][1,],weights.noshort[ind.noshort,])
rownames(weights.tangency)=c("Allowing Short Sale Weights","No Short Sale Weights")
colnames(weights.tangency)=c("GOOGL","IBM","AMZN","JNJ","PFE","MRK","CIE","CVI","BWF")
weights.tangency[2,7]=0
weights.tangency[2,2]=0
weights.tangency[2,9]=0
weights.tangency

# showed the tangency porfolio with short sale and wihout short sale. With the short sale,
# we have discovered that pharmaceutical stocks still dominant the weights of portfolio. 
# It put 87% of the weight on pharmaceutical stocks. Then 7.2% on technological firms, and
# 5.8% on energy sector. For the tangency porfolio without short sale, the pharmaceutical
# sector stil hold 69% of weight. 20% of the weights put on technology firms and 11% on 
# energy sector.


### Get return variacen,and sd for tangent portfolio ###

tangent.return.short=muP[ind]
tangent.sd.short=sdP[ind]
tangent.variance.short=sdP[ind]^2

tangent.return.noshort=muP.noshort[ind.noshort]
tangent.sd.noshort=sdP.noshort[ind.noshort]
tangent.variance.noshort=sdP.noshort[ind.noshort]^2

tangent.port.short=t(as.matrix(c(tangent.return.short,tangent.sd.short,tangent.variance.short)))
tangent.port.noshort=t(as.matrix(c(tangent.return.noshort,tangent.sd.noshort,tangent.variance.noshort)))
tangent.port.mean.sd=rbind(tangent.port.short,tangent.port.noshort)
tangent.port.mean.sd
colnames(tangent.port.mean.sd)=c("Mean Return","Standard Deviation","Variance")
rownames(tangent.port.mean.sd)=c("Tangent Portfolio Allow Short Sale","Tangent Portfolio Not Allow Short Sale" )
tangent.port.mean.sd

### Obtain the shape ratio for each stock ###

sharpe.each.stock=vector()
for(i in 1:length(mean.vect)){
  sharpe.each.stock[i]= (mean.vect[i]-mu.free)/sd.vect[i]
}
sharpe.each.stock=t(as.matrix(sharpe.each.stock))
sharpe.tangent.short=sharpe[ind]
sharpe.tangent.noshort=sharpe.noshort[ind.noshort]
compare.sharpe.matrix=cbind(sharpe.each.stock,sharpe.tangent.short,sharpe.tangent.noshort)
colnames(compare.sharpe.matrix)=c("GOOGL","IBM","AMZN","JNJ","PFE","MRK","CIE","CVI","BWF","Tangent_Short","Tangent_No_Short")
rownames(compare.sharpe.matrix)=c("Sharpe Ratio")
compare.sharpe.matrix

### Comments ###

# We have seen that the highest sharpe ratio is PFE. However, tangent portfolio sharpe
# ratios are out perform any individual stock sharpe ratios. Comparing tangent portfolio
# under short sale and without short sale, the sharpe ratio is higher under short sale
# but not by much.

### PCA ###
pca = princomp(R)

names(pca)
summary(pca)
plot(pca)
porp.pca=vector()
for(i in 1:9){
  porp.pca[i]=eigen(cov.mat)$values[i]/sum(eigen(cov.mat)$values)
}
porp.pca

par(mfrow=c(1,1))
sum(eigen(cov.mat)$values[1:5])/sum(eigen(cov.mat)$values)

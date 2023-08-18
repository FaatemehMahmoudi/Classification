mat=cbind(z,0)
#uniform distribution without outliers Bayesian
for(j in 1:10){
  mat[,3]=sample(rep(1:10,(n1+m1)/10),(n1+m1))
  k=matrix(c(NA),10,((n1+m1)/10))
  for(i in 1:10){
    k[i,]=mat[mat[,3]==i,1]
  }
  G=matrix(c(NA),10,((n1+m1)/10))
  for(i in 1:10){
    G[i,]=mat[mat[,3]==i,2]
  }
  fmat=matrix(c(k[1,],k[2,],k[3,],k[4,],k[5,],k[6,],k[7,],k[8,],k[9,],k[10,],rep(1:10,rep(((n1+m1)/10),10)),G[1,],G[2,],G[3,],G[4,],G[5,],G[6,],G[7,],G[8,],G[9,],G[10,]),nrow=(n1+m1),ncol=3)
  eachscore=c()
  for(i in 1:10){
    test=fmat[((((((i-1)*2)*(n1/10)))+1):((2*i)*(n1/10))),]
    train=fmat[-((((((i-1)*2)*(n1/10)))+1):((2*i)*(n1/10))),]
    train=as.data.frame(train)
    test=as.data.frame(test)
    mean1=mean((train$V1)[train$V3==1])
    mean2=mean((train$V1)[train$V3==2])
    aa=abs((test$V1)-mean1)
    bb=abs((test$V1)-mean2)
    result=ifelse(aa<bb,1,2)
    eachscore[i]=sum(test[,3]==result)
    meanscore[j]=mean(eachscore)
    eachscore
    mean(eachscore)
  }
}
eachscore
meanscore
mean(meanscore)->finalscore
finalscore
(100*finalscore)/((n1+m1)/10)
#10 times 10-fold cross validation
n1=800
m1=800
a=matrix(c(runif(n1,0,1),rep(1,n1)),nrow=n1,ncol=2)
b=matrix(c(runif(m1,0.8,1.8),rep(2,m1)),nrow=m1,ncol=2)
z=rbind(a,b)
mat=cbind(z,0)
#uniform distribution without outliers Bayesian
for(j in 1:10){
  mat[,3]=sample(rep(1:10,(n1+m1)/10),(n1+m1))
  k=matrix(c(NA),10,((n1+m1)/10))
  for(i in 1:10){
    k[i,]=mat[mat[,3]==i,1]
  }
  G=matrix(c(NA),10,((n1+m1)/10))
  for(i in 1:10){
    G[i,]=mat[mat[,3]==i,2]
  }
  fmat=matrix(c(k[1,],k[2,],k[3,],k[4,],k[5,],k[6,],k[7,],k[8,],k[9,],k[10,],rep(1:10,rep(((n1+m1)/10),10)),G[1,],G[2,],G[3,],G[4,],G[5,],G[6,],G[7,],G[8,],G[9,],G[10,]),nrow=(n1+m1),ncol=3)
  eachscore=c()
  for(i in 1:10){
    test=fmat[((((((i-1)*2)*(n1/10)))+1):((2*i)*(n1/10))),]
    train=fmat[-((((((i-1)*2)*(n1/10)))+1):((2*i)*(n1/10))),]
    train=as.data.frame(train)
    test=as.data.frame(test)
    mean1=mean((train$V1)[train$V3==1])
    mean2=mean((train$V1)[train$V3==2])
    aa=abs((test$V1)-mean1)
    bb=abs((test$V1)-mean2)
    result=ifelse(aa<bb,1,2)
    eachscore[i]=sum(test[,3]==result)
    meanscore[j]=mean(eachscore)
    eachscore
    mean(eachscore)
  }
}
eachscore
meanscore
mean(meanscore)->finalscore
finalscore
(100*finalscore)/((n1+m1)/10)
#normal dist without outliers CMEQS:
for(j in 1:10){
  mat[,3]=sample(rep(1:10,(n1+m1)/10),(n1+m1))
  k=matrix(c(NA),10,((n1+m1)/10))
  for(i in 1:10){
    k[i,]=mat[mat[,3]==i,1]
  }
  G=matrix(c(NA),10,((n1+m1)/10))
  for(i in 1:10){
    G[i,]=mat[mat[,3]==i,2]
  }
  fmat=matrix(c(k[1,],k[2,],k[3,],k[4,],k[5,],k[6,],k[7,],k[8,],k[9,],k[10,],rep(1:10,rep(((n1+m1)/10),10)),G[1,],G[2,],G[3,],G[4,],G[5,],G[6,],G[7,],G[8,],G[9,],G[10,]),nrow=(n1+m1),ncol=3)
  eachscore=c()
  for(i in 1:10){
    test=fmat[((((((i-1)*2)*(n1/10)))+1):((2*i)*(n1/10))),]
    train=fmat[-((((((i-1)*2)*(n1/10)))+1):((2*i)*(n1/10))),]
    train=as.data.frame(train)
    test=as.data.frame(test)
    train1=(train$V1)[(train$V3)==1]
    train2=(train$V1)[(train$V3)==2]
    asort=sort(train1)
    asort[(2*length(asort))/3]->Q3
    bsort=sort(train2)
    bsort[(length(bsort))/3]->Q1
    aa=abs((test$V1)-Q3)
    bb=abs((test$V1)-Q1)
    result=ifelse(aa<bb,1,2)
    eachscore[i]=sum(test[,3]==result)
    meanscore[j]=mean(eachscore)
    eachscore
    mean(eachscore)
  }
}
eachscore
meanscore
mean(meanscore)->finalscore
finalscore
(100*finalscore)/((n1+m1)/10)
#10 times 10-fold cross validation
n1=800
m1=800
a=matrix(c(runif(n1,0,1),rep(1,n1)),nrow=n1,ncol=2)
b=matrix(c(runif(m1,0.6,1.6),rep(2,m1)),nrow=m1,ncol=2)
z=rbind(a,b)
mat=cbind(z,0)
#uniform distribution without outliers Bayesian
for(j in 1:10){
  mat[,3]=sample(rep(1:10,(n1+m1)/10),(n1+m1))
  k=matrix(c(NA),10,((n1+m1)/10))
  for(i in 1:10){
    k[i,]=mat[mat[,3]==i,1]
  }
  G=matrix(c(NA),10,((n1+m1)/10))
  for(i in 1:10){
    G[i,]=mat[mat[,3]==i,2]
  }
  fmat=matrix(c(k[1,],k[2,],k[3,],k[4,],k[5,],k[6,],k[7,],k[8,],k[9,],k[10,],rep(1:10,rep(((n1+m1)/10),10)),G[1,],G[2,],G[3,],G[4,],G[5,],G[6,],G[7,],G[8,],G[9,],G[10,]),nrow=(n1+m1),ncol=3)
  eachscore=c()
  for(i in 1:10){
    test=fmat[((((((i-1)*2)*(n1/10)))+1):((2*i)*(n1/10))),]
    train=fmat[-((((((i-1)*2)*(n1/10)))+1):((2*i)*(n1/10))),]
    train=as.data.frame(train)
    test=as.data.frame(test)
    mean1=mean((train$V1)[train$V3==1])
    mean2=mean((train$V1)[train$V3==2])
    aa=abs((test$V1)-mean1)
    bb=abs((test$V1)-mean2)
    result=ifelse(aa<bb,1,2)
    eachscore[i]=sum(test[,3]==result)
    meanscore[j]=mean(eachscore)
    eachscore
    mean(eachscore)
  }
}
eachscore
meanscore
mean(meanscore)->finalscore
finalscore
(100*finalscore)/((n1+m1)/10)
#nunif distribution without outlierss CMQS:
for(j in 1:10){
  mat[,3]=sample(rep(1:10,(n1+m1)/10),(n1+m1))
  k=matrix(c(NA),10,((n1+m1)/10))
  for(i in 1:10){
    k[i,]=mat[mat[,3]==i,1]
  }
  G=matrix(c(NA),10,((n1+m1)/10))
  for(i in 1:10){
    G[i,]=mat[mat[,3]==i,2]
  }
  fmat=matrix(c(k[1,],k[2,],k[3,],k[4,],k[5,],k[6,],k[7,],k[8,],k[9,],k[10,],rep(1:10,rep(((n1+m1)/10),10)),G[1,],G[2,],G[3,],G[4,],G[5,],G[6,],G[7,],G[8,],G[9,],G[10,]),nrow=(n1+m1),ncol=3)
  eachscore=c()
  for(i in 1:10){
    test=fmat[((((((i-1)*2)*(n1/10)))+1):((2*i)*(n1/10))),]
    train=fmat[-((((((i-1)*2)*(n1/10)))+1):((2*i)*(n1/10))),]
    train=as.data.frame(train)
    test=as.data.frame(test)
    Q3=qunif((2/3),min=0,max=1)
    Q1=qunif((1/3),min=0.6,max=1.6)
    aa=abs((test$V1)-Q3)
    bb=abs((test$V1)-Q1)
    result=ifelse(aa<bb,1,2)
    eachscore[i]=sum(test[,3]==result)
    meanscore[j]=mean(eachscore)
    eachscore
    mean(eachscore)
  }
}
eachscore
meanscore
mean(meanscore)->finalscore
finalscore
(100*finalscore)/((n1+m1)/10)
#normal dist without outliers CMEQS:
for(j in 1:10){
  mat[,3]=sample(rep(1:10,(n1+m1)/10),(n1+m1))
  k=matrix(c(NA),10,((n1+m1)/10))
  for(i in 1:10){
    k[i,]=mat[mat[,3]==i,1]
  }
  G=matrix(c(NA),10,((n1+m1)/10))
  for(i in 1:10){
    G[i,]=mat[mat[,3]==i,2]
  }
  fmat=matrix(c(k[1,],k[2,],k[3,],k[4,],k[5,],k[6,],k[7,],k[8,],k[9,],k[10,],rep(1:10,rep(((n1+m1)/10),10)),G[1,],G[2,],G[3,],G[4,],G[5,],G[6,],G[7,],G[8,],G[9,],G[10,]),nrow=(n1+m1),ncol=3)
  eachscore=c()
  for(i in 1:10){
    test=fmat[((((((i-1)*2)*(n1/10)))+1):((2*i)*(n1/10))),]
    train=fmat[-((((((i-1)*2)*(n1/10)))+1):((2*i)*(n1/10))),]
    train=as.data.frame(train)
    test=as.data.frame(test)
    train1=(train$V1)[(train$V3)==1]
    train2=(train$V1)[(train$V3)==2]
    asort=sort(train1)
    asort[(2*length(asort))/3]->Q3
    bsort=sort(train2)
    bsort[(length(bsort))/3]->Q1
    aa=abs((test$V1)-Q3)
    bb=abs((test$V1)-Q1)
    result=ifelse(aa<bb,1,2)
    eachscore[i]=sum(test[,3]==result)
    meanscore[j]=mean(eachscore)
    eachscore
    mean(eachscore)
  }
}
eachscore
meanscore
mean(meanscore)->finalscore
finalscore
(100*finalscore)/((n1+m1)/10)
#normal distribution without outliers kernel dens est
mina=min(a[,1])
maxa=max(a[,1])
minb=min(b[,1])
maxb=max(b[,1])
eachscore
for(j in 1:10){
  mat[,3]=sample(rep(1:10,(n1+m1)/10),(n1+m1))
  k=matrix(c(NA),10,((n1+m1)/10))
  for(i in 1:10){
    k[i,]=mat[mat[,3]==i,1]
  }
  G=matrix(c(NA),10,((n1+m1)/10))
  for(i in 1:10){
    G[i,]=mat[mat[,3]==i,2]
  }
  fmat=matrix(c(k[1,],k[2,],k[3,],k[4,],k[5,],k[6,],k[7,],k[8,],k[9,],k[10,],rep(1:10,rep(((n1+m1)/10),10)),G[1,],G[2,],G[3,],G[4,],G[5,],G[6,],G[7,],G[8,],G[9,],G[10,]),nrow=(n1+m1),ncol=3)
  eachscore=c()
  for(i in 1:10){
    test=fmat[((((((i-1)*2)*(n1/10)))+1):((2*i)*(n1/10))),]
    train=fmat[-((((((i-1)*2)*(n1/10)))+1):((2*i)*(n1/10))),]
    train=as.data.frame(train)
    test=as.data.frame(test)
    train$V1[train$V3==1]->train1
    train$V1[train$V3==2]->train2
    pdf1=density(train1)
    f1=approxfun(pdf1$x,pdf1$y,yleft=0,yright=0)
    FF1=function(x) integrate(f1,-Inf,x)$v-0.75
    uniroot(FF1,c(mina,maxa))$root->Q3
    pdf2=density(train2)
    f2=approxfun(pdf2$x,pdf2$y,yleft=0,yright=0)
    FF2=function(x) integrate(f2,-Inf,x)$v-0.25
    uniroot(FF2,c(minb,maxb))$root->Q1
    aa=abs((test$V1)-Q3)
    bb=abs((test$V1)-Q1)
    result=ifelse(aa<bb,1,2)
    eachscore[i]=sum(test[,3]==result)
    meanscore[j]=mean(eachscore)
    eachscore
    mean(eachscore)
  }
}
meanscore
mean(meanscore)->finalscore
finalscore
(100*finalscore)/((n1+m1)/10)
#NORMAL DISTRIBUTIPN WITH OUTLIERS
n1=800
m1=800
n=600
nn=200
m=600
mm=200
x1=rnorm(600,0,3)
x12=rnorm(200,25,3)
x2=rnorm(600,6,3)
x22=rnorm(200,-16,3)
z2=rbind(matrix(c(x2,rep(2,600)),nrow=600,ncol=2),matrix(c(x22,rep(2,200)),nrow=200,ncol=2))
z1=rbind(matrix(c(x1,rep(1,600)),nrow=600,ncol=2),matrix(c(x12,rep(1,200)),nrow=200,ncol=2))
mat=rbind(z1,z2)
mat=cbind(mat,0)
#normal distribution with outliers Bayesian
for(j in 1:10){
  mat[,3]=sample(rep(1:10,(n1+m1)/10),(n1+m1))
  k=matrix(c(NA),10,((n1+m1)/10))
  for(i in 1:10){
    k[i,]=mat[mat[,3]==i,1]
  }
  G=matrix(c(NA),10,((n1+m1)/10))
  for(i in 1:10){
    G[i,]=mat[mat[,3]==i,2]
  }
  fmat=matrix(c(k[1,],k[2,],k[3,],k[4,],k[5,],k[6,],k[7,],k[8,],k[9,],k[10,],rep(1:10,rep(((n1+m1)/10),10)),G[1,],G[2,],G[3,],G[4,],G[5,],G[6,],G[7,],G[8,],G[9,],G[10,]),nrow=(n1+m1),ncol=3)
  eachscore=c()
  for(i in 1:10){
    test=fmat[((((((i-1)*2)*(n1/10)))+1):((2*i)*(n1/10))),]
    train=fmat[-((((((i-1)*2)*(n1/10)))+1):((2*i)*(n1/10))),]
    train=as.data.frame(train)
    test=as.data.frame(test)
    mean1=mean((train$V1)[train$V3==1])
    mean2=mean((train$V1)[train$V3==2])
    aa=abs((test$V1)-mean1)
    bb=abs((test$V1)-mean2)
    result=ifelse(aa<bb,1,2)
    eachscore[i]=sum(test[,3]==result)
    meanscore[j]=mean(eachscore)
    eachscore
    mean(eachscore)
  }
}
eachscore
meanscore
mean(meanscore)->finalscore
finalscore
(100*finalscore)/((n1+m1)/10)
#Normal distribution with outliers CMQS:
for(j in 1:10){
  mat[,3]=sample(rep(1:10,(n1+m1)/10),(n1+m1))
  k=matrix(c(NA),10,((n1+m1)/10))
  for(i in 1:10){
    k[i,]=mat[mat[,3]==i,1]
  }
  G=matrix(c(NA),10,((n1+m1)/10))
  for(i in 1:10){
    G[i,]=mat[mat[,3]==i,2]
  }
  fmat=matrix(c(k[1,],k[2,],k[3,],k[4,],k[5,],k[6,],k[7,],k[8,],k[9,],k[10,],rep(1:10,rep(((n1+m1)/10),10)),G[1,],G[2,],G[3,],G[4,],G[5,],G[6,],G[7,],G[8,],G[9,],G[10,]),nrow=(n1+m1),ncol=3)
  eachscore=c()
  for(i in 1:10){
    test=fmat[((((((i-1)*2)*(n1/10)))+1):((2*i)*(n1/10))),]
    train=fmat[-((((((i-1)*2)*(n1/10)))+1):((2*i)*(n1/10))),]
    train=as.data.frame(train)
    test=as.data.frame(test)
    Q3=qnorm(0.75,mean=mean(z1[,1]),sd=sqrt(var(z1[,1])))
    Q1=qnorm(0.25,mean=mean(z2[,1]),sd=sqrt(var(z2[,1])))
    F = function(x,w,u,s) sum( w*pnorm(x,mean=u,sd=s) )
    F_inv = function(p,w,u,s,br=c(-1000,1000))
    {
      G = function(x) F(x,w,u,s) - p
      return( uniroot(G,br)$root )
    }
    F_inv((.75),c(0.75,0.25),c(0,25),c(sqrt(3),sqrt(3)))->Q3
    F_inv(.25,c(0.75,0.25),c(6,-16),c(sqrt(3),sqrt(3)))->Q1
    aa=abs((test$V1)-Q3)
    bb=abs((test$V1)-Q1)
    result=ifelse(aa<bb,1,2)
    eachscore[i]=sum(test[,3]==result)
    meanscore[j]=mean(eachscore)
    eachscore
    mean(eachscore)
  }
}
#normal distribution with outliers Bayesian
for(j in 1:10){
  mat[,3]=sample(rep(1:10,(n1+m1)/10),(n1+m1))
  k=matrix(c(NA),10,((n1+m1)/10))
  for(i in 1:10){
    k[i,]=mat[mat[,3]==i,1]
  }
  G=matrix(c(NA),10,((n1+m1)/10))
  for(i in 1:10){
    G[i,]=mat[mat[,3]==i,2]
  }
  fmat=matrix(c(k[1,],k[2,],k[3,],k[4,],k[5,],k[6,],k[7,],k[8,],k[9,],k[10,],rep(1:10,rep(((n1+m1)/10),10)),G[1,],G[2,],G[3,],G[4,],G[5,],G[6,],G[7,],G[8,],G[9,],G[10,]),nrow=(n1+m1),ncol=3)
  eachscore=c()
  for(i in 1:10){
    test=fmat[((((((i-1)*2)*(n1/10)))+1):((2*i)*(n1/10))),]
    train=fmat[-((((((i-1)*2)*(n1/10)))+1):((2*i)*(n1/10))),]
    train=as.data.frame(train)
    test=as.data.frame(test)
    mean1=mean((train$V1)[train$V3==1])
    mean2=mean((train$V1)[train$V3==2])
    aa=abs((test$V1)-mean1)
    bb=abs((test$V1)-mean2)
    result=ifelse(aa<bb,1,2)
    eachscore[i]=sum(test[,3]==result)
    meanscore[j]=mean(eachscore)
    eachscore
    mean(eachscore)
  }
}
eachscore
mean1
mean2
#Normal distribution with outliers CMQS:
for(j in 1:10){
  mat[,3]=sample(rep(1:10,(n1+m1)/10),(n1+m1))
  k=matrix(c(NA),10,((n1+m1)/10))
  for(i in 1:10){
    k[i,]=mat[mat[,3]==i,1]
  }
  G=matrix(c(NA),10,((n1+m1)/10))
  for(i in 1:10){
    G[i,]=mat[mat[,3]==i,2]
  }
  fmat=matrix(c(k[1,],k[2,],k[3,],k[4,],k[5,],k[6,],k[7,],k[8,],k[9,],k[10,],rep(1:10,rep(((n1+m1)/10),10)),G[1,],G[2,],G[3,],G[4,],G[5,],G[6,],G[7,],G[8,],G[9,],G[10,]),nrow=(n1+m1),ncol=3)
  eachscore=c()
  for(i in 1:10){
    test=fmat[((((((i-1)*2)*(n1/10)))+1):((2*i)*(n1/10))),]
    train=fmat[-((((((i-1)*2)*(n1/10)))+1):((2*i)*(n1/10))),]
    train=as.data.frame(train)
    test=as.data.frame(test)
    Q3=qnorm(0.75,mean=mean(z1[,1]),sd=sqrt(var(z1[,1])))
    Q1=qnorm(0.25,mean=mean(z2[,1]),sd=sqrt(var(z2[,1])))
    F = function(x,w,u,s) sum( w*pnorm(x,mean=u,sd=s) )
    F_inv = function(p,w,u,s,br=c(-1000,1000))
    {
      G = function(x) F(x,w,u,s) - p
      return( uniroot(G,br)$root )
    }
    F_inv((.75),c(0.75,0.25),c(0,25),c(sqrt(3),sqrt(3)))->Q3
    F_inv(.25,c(0.75,0.25),c(6,-16),c(sqrt(3),sqrt(3)))->Q1
    aa=abs((test$V1)-Q3)
    bb=abs((test$V1)-Q1)
    result=ifelse(aa<bb,1,2)
    eachscore[i]=sum(test[,3]==result)
    meanscore[j]=mean(eachscore)
    eachscore
    mean(eachscore)
  }
}
eachscore
Q3
Q1
#Normal distribution with outliers CMQS:
for(j in 1:10){
  mat[,3]=sample(rep(1:10,(n1+m1)/10),(n1+m1))
  k=matrix(c(NA),10,((n1+m1)/10))
  for(i in 1:10){
    k[i,]=mat[mat[,3]==i,1]
  }
  G=matrix(c(NA),10,((n1+m1)/10))
  for(i in 1:10){
    G[i,]=mat[mat[,3]==i,2]
  }
  fmat=matrix(c(k[1,],k[2,],k[3,],k[4,],k[5,],k[6,],k[7,],k[8,],k[9,],k[10,],rep(1:10,rep(((n1+m1)/10),10)),G[1,],G[2,],G[3,],G[4,],G[5,],G[6,],G[7,],G[8,],G[9,],G[10,]),nrow=(n1+m1),ncol=3)
  eachscore=c()
  for(i in 1:10){
    test=fmat[((((((i-1)*2)*(n1/10)))+1):((2*i)*(n1/10))),]
    train=fmat[-((((((i-1)*2)*(n1/10)))+1):((2*i)*(n1/10))),]
    train=as.data.frame(train)
    test=as.data.frame(test)
    Q3=qnorm(0.75,mean=mean(z1[,1]),sd=sqrt(var(z1[,1])))
    Q1=qnorm(0.25,mean=mean(z2[,1]),sd=sqrt(var(z2[,1])))
    F = function(x,w,u,s) sum( w*pnorm(x,mean=u,sd=s) )
    F_inv = function(p,w,u,s,br=c(-1000,1000))
    {
      G = function(x) F(x,w,u,s) - p
      return( uniroot(G,br)$root )
    }
    F_inv((.75),c(0.75,0.25),c(0,25),c(sqrt(3),sqrt(3)))->Q3
    F_inv(.25,c(0.75,0.25),c(6,-16),c(sqrt(3),sqrt(3)))->Q1
    aa=abs((test$V1)-Q1)
    bb=abs((test$V1)-Q3)
    result=ifelse(aa<bb,1,2)
    eachscore[i]=sum(test[,3]==result)
    meanscore[j]=mean(eachscore)
    eachscore
    mean(eachscore)
  }
}
eachscore
meanscore
mean(meanscore)->finalscore
finalscore
(100*finalscore)/((n1+m1)/10)
for(j in 1:10){
  mat[,3]=sample(rep(1:10,(n1+m1)/10),(n1+m1))
  k=matrix(c(NA),10,((n1+m1)/10))
  for(i in 1:10){
    k[i,]=mat[mat[,3]==i,1]
  }
  G=matrix(c(NA),10,((n1+m1)/10))
  for(i in 1:10){
    G[i,]=mat[mat[,3]==i,2]
  }
  fmat=matrix(c(k[1,],k[2,],k[3,],k[4,],k[5,],k[6,],k[7,],k[8,],k[9,],k[10,],rep(1:10,rep(((n1+m1)/10),10)),G[1,],G[2,],G[3,],G[4,],G[5,],G[6,],G[7,],G[8,],G[9,],G[10,]),nrow=(n1+m1),ncol=3)
  eachscore=c()
  for(i in 1:10){
    test=fmat[((((i-1)*160)+1):(i*160)),]
    train=fmat[-((((i-1)*160)+1):(i*160)),]
    train=as.data.frame(train)
    test=as.data.frame(test)
    train1=(train$V1)[(train$V3)==1]
    train2=(train$V1)[(train$V3)==2]
    asort=sort(z1[,1])
    asort[(2*80)/3]->Q3
    bsort=sort(z2[,1])
    bsort[(80)/3]->Q1
    aa=abs((test$V1)-Q3)
    bb=abs((test$V1)-Q1)
    result=ifelse(aa<bb,1,2)
    eachscore[i]=sum(test[,3]==result)
    meanscore[j]=mean(eachscore)
    eachscore
    mean(eachscore)
  }
}
eachscore
meanscore
mean(meanscore)->finalscore
finalscore
(100*finalscore)/((n1+m1)/10)
#Normal distribution with outliers CMKQS:
Q1ker=Q1

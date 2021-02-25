rm(list=ls())


#objective Description-------------------------------------------------------------------------------------------------
#image now we are are given fiduciary responsibilities over 
#$10 million that is to be fully invested in an equity portfolio that 
#is benchmarked by the Russell 3000 index. Your portfolio construction 
#is subject to the following constraints:
#a)	no less than 50 and no more than 1000 of the Russell 3000 components
#b)	No leverage or short sales are allowed and each selected component must 
#have a positive weight wi that cannot exceed 2.5 times an equal weighting at the time of portfolio construction
#c)	For simplicity, only invest in stocks that are in the Russell 3000 as of October 27th, 2017
#d)	No portfolio adjustments are permitted.  


setwd("C:/Users/23721/Downloads/data")
library("readxl")
data1 <- read_excel("Assignment 3 - Data.xlsx",
                    sheet = "Firm Characteristics",skip = 7)
data1<- as.data.frame(data1)
row.names(data1)<-data1[,2]
data1[,c((4:14),(16:19))]<-as.numeric(unlist(data1[,c((4:14),(16:19))]))
data1 <- data1[,-c(1:3)]
data2 <- read_excel("Assignment 3 - Data.xlsx",
                    sheet = "Aggregates",skip = 8)
data2<- as.data.frame(data2)
data2[,2:16]<-as.numeric(unlist(data2[,2:16]))
row.names(data2) <- data2[,1]
data2 <- data2[,-1]
data3 <- read_excel("Assignment 3 - Data.xlsx",
                    sheet = "Stock Prices",skip = 1)
data3<- as.data.frame(data3)
data3[,2:2944]<-as.numeric(unlist(data3[,2:2944]))
data3 <- data3[,-1]




#task1: Using only stock price data on or prior to December 31st, 2016, 
#construct a mean-variance efficient portfolio that maximizes Sharpe ratio 
#over the estimation period.  Report performance statistics of the portfolio 
#including but not limited to Treynor Ratio, CAPM beta (using IWV as the market portfolio), 
#and CAPM alpha.  Provide component weights (only those securities with non-zero weights), 
#documented code, and any relevant analysis for portfolio construction.
 


rt<-as.data.frame(matrix(NA,nrow = 1465,ncol = 2943))
for (i in 1:1465){
  rt[i,]<-log((data3[i+1,])/(data3[i,]))
}
rtr <-rt[1:1257,]
omit <-0
for (j in 1:2943){ #omit NA colunm
  if (is.na(rt[1,j])==T) {
    omit <- c(omit,j)
  }
}

rt <- rt[,-omit]
testr <- data3[1258:1465,-omit]
testrr <- matrix(NA,207,2200)
for (i in 1:207){
  testrr[i,] <- log(testr[i+1,]/testr[i,])
}
rt <- rt[1:1257,]
colnames(rt)<-colnames(data3)
rt <- cbind(date,rt)
write.table(rt,"log_return.txt",row.names = F) #save
rt<- rt[,-1]
meanr <- apply(rt,2,mean)*252
Sigma <- var(rt, use="pairwise") * 252

#optimization
tics <- colnames(rt)
library("alabama")
rf <- 0.0175 #5-y forward treasury rate in 2013
sol <-0
cons <- matrix(NA,200,501)
test <- c(50,100,150,250,500)
for (j in 1:5){
  i <- test[j]
  val <- 0
  for (h in 1:20){
    number <- sample(x = 3:2201,size = i)
    sn <- number-1
    Sig <- Sigma[sn,sn]
    heq <- function(w){
      k <- sum(w)-1
      return(k)
    }
    hin <- function(w){
      n <- length(w)
      k <- 0
      k[1:n]<-w
      k[(n+1):(2*n)]<-2.5/n-w
      return(k)
    }
    Sharpe <- function(w){
      v <- t(w)%*%Sig%*%w
      s <- -(t(w)%*%meanr[sn]-rf)/sqrt(v)
      return(s)
    }
    x0 <- rep(1/i,i)
    tryCatch({conclu <- constrOptim.nl(par=x0, fn=Sharpe,  
                                       heq=heq, hin=hin,
                                       "control.outer"=list("trace"=FALSE),
                                       "control.optim"=list("reltol"=1e-12,
                                                            "abstol"=1e-12,
                                                            "trace"=0))},
             error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
    val[h] <- -conclu$value
    cons[(j-1)*40+(h-1)*2+1,1:i]<-number
    cons[(j-1)*40+h*2,1:i]<-conclu$par
    cons[(j-1)*40+(h-1)*2+1,501]<--conclu$value
  }
  sol[j]<- mean(val)
}
win <- which.max(sol)
winner <- which.max(cons[,501])
if (((win-1)*40)<winner<=(win*40)){
  print("the same")
  output <- cons[(winner):(winner+1),1:501]
  output[1,1001]<-test[win]
  write.table(output,"output",row.names = F)
}else{
  print("not the same")
  print(test[win])
}
l <- seq(1,199,length=100)
sampleN <- c(rep(50,20),rep(100,20),rep(150,20),rep(250,20),rep(500,20))
SharpeRatio<-cons[l,501]
p <- plot(sampleN,SharpeRatio)

#now find the max Sharpe Ratio by sampling
ss <- matrix(NA,400,51)
for (h in 1:200){
  sn <- sample(x = 2:2200,size = 50)
  Sig <- Sigma[sn,sn]
  heq <- function(w){
    k <- sum(w)-1
    return(k)
  }
  hin <- function(w){
    n <- length(w)
    k <- 0
    k[1:n]<-w
    k[(n+1):(2*n)]<-2.5/n-w
    return(k)
  }
  Sharpe <- function(w){
    v <- t(w)%*%Sig%*%w
    s <- -(t(w)%*%meanr[sn]-rf)/sqrt(v)
    return(s)
  }
  x0 <- rep(1/50,50)
  tryCatch({conclu <- constrOptim.nl(par=x0, fn=Sharpe,  
                                     heq=heq, hin=hin,
                                     "control.outer"=list("trace"=FALSE),
                                     "control.optim"=list("reltol"=1e-12,
                                                          "abstol"=1e-12,
                                                          "trace"=0))},
           error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
  ss[(h*2-1),1:50]<-sn
  ss[(h*2),1:50]<-conclu$par
  ss[(h*2-1),51]<--conclu$value
}
winner <- which.max(ss[,51])
# in this plot we can see that the more stocks included, 
# the lower average sharpe ratio
w <- ss[winner+1,1:50]
number <- ss[winner,1:50]
Sharperatio <- ss[winner,51]
ptic <- tics[number]
output1 <- matrix(NA,2,50)
output1[1,1:50]<-ptic
output1[2,1:50]<-w
write.table(output1,"output1.txt",row.names = F,col.names = F)
#CAPM beta
n <- length(w)
beta <- matrix(0,n,1)
for (i in 1:n){
  beta[i,] <- cov(rt[,1],rt[,number[i]])/var(rt[,1])
}
pbeta <- w%*%beta
print(pbeta)
#CAPM alpha
me <- matrix(NA,n,1)
rm <- meanr[1]
me<-meanr[unlist(number)-1]#here calculate the portfolio expected return
pome <- w%*%me
alpha <- pome-pbeta*(rm-rf)-rf
#Treynor ratio
Tre <- (pome-rf)/pbeta
#Sortino ratio
downs <- rt[,c(2,number+1)]
downs[downs>0]<-0
downsig <- var(downs[,2:51], use="pairwise") * 252
downv <- t(w)%*%downsig%*%w
sortino <- (pome-rf)/sqrt(downv)


#Task2 Next, construct a portfolio that uses only firm characteristics 
#(e.g. Price/Book, P/E, ROE, etc.) to determine portfolio weights.  
#consider a multi-dimensional quantitative screening approach or a smart 
#beta (non-rebalanced) approach: the choice is yours, but you may not use 
#prior return time series as in part 1.  Again, report portfolio performance 
#statistics, provide component weights, documented code, and any relevant 
#analysis for portfolio construction.

credit <- unlist(data1[,12])
#here we grade all A as 1 point
#B and no imformation with 0, and C with -1
A <- grep("A",credit)
C <- grep("C",credit)
credit[1:2942] <- 0
credit[A] <- 1
credit[C] <- -1
cm <- mean(credit)
sdc <- sd(credit)
credit <- (credit-cm)/sdc
data1 <- data1[,-12]
mea <- unlist(data2[2,])
vol <- unlist(data2[5,])
score <- matrix(0,2942,15)
rownames(score)<-tic[2:2943]
names(score) <- rownames(data1)
for (i in 1:15){
  score[,i] <- (data1[,i]-mea[i])/vol[i]
}
score <- cbind(score,credit)
score <- score[-(omit),]

#to eliminate extreme value
score[score>2] <-2 
score[score<(-2)] <--2
score[is.na(score)] <-0
zweight <- rep(1/16,16)
score <- score%*%zweight
meanscore <- mean(score)
count <- which(score>0.366)
n <- length(count)
super <- score[count]
sumscore <- sum(super)
W <- super/sumscore
output2 <- as.data.frame(matrix(NA,2,n))
output2[2,] <- W
tic <- colnames(data3)
count <- count+1
output2[1,] <- tics[count]
write.table(output2,"output2.txt",row.names = F,col.names = F)

#check the performance
n <- length(W)
beta <- matrix(0,n,1)
for (i in 1:n){
  beta[i,] <- cov(rt[,1],rt[,count[i]])/var(rt[,1])
}
pbeta <- W%*%beta
print(pbeta)
#CAPM alpha
me <- matrix(NA,n,1)
meanr <- apply(rrrttt,2,mean)*252
rm <- meanr[1]
me<-meanr[unlist(count)-1]#here calculate the portfolio expected return
q2pome <- W%*%me
alpha <- q2pome-pbeta*(rm-rf)-rf
#Treynor ratio
Tre <- (q2pome-rf)/pbeta
#Sortino ratio
downs <- rt[,c(2,count+1)]
downs[downs>0]<-0
downsig <- var(downs[,1:50], use="pairwise") * 252
downv <- t(W)%*%downsig%*%W
sotino <- (q2pome-rf)/sqrt(downv)
v <- t(W)%*%Sigma[count,count]%*%W
q2sharpe <- (W%*%meanr[count]-rf)/sqrt(v)


#Task3 Run both of these portfolios out of sample (from January 1st, 2017 through 
#October 27th, 2017) and recalculate portfolio performance statistics.  Plot 
#the growth and contrast the performance of a $10 million initial investment 
#in each portfolio and the Russell 3000 index for the within sample and out of
#sample time periods in a single chart.  Which portfolio performed better?  
#How did they compare to the Russell 3000 Index?  What might explain the differences 
#in performance?  How could you have improved the construction of your portfolios in part 1 and 2? 


p1 <- testrr[,number]
p2 <- testrr[,count]
n <- length(count)
beta <- matrix(0,n,1)
for (i in 1:n){
  beta[i,] <- cov(testrr[,1],testrr[,count[i]])/var(testrr[,1])
}
pbeta <- W%*%beta
print(pbeta)
#CAPM alpha
me <- matrix(NA,n,1)
meanr <- apply(testrr,2,sum)
rm <- meanr[1]
me<-meanr[unlist(count)]#here calculate the portfolio expected return
q2pome <- W%*%me
alpha <- q2pome-pbeta*(rm-rf)-rf
#Treynor ratio
Tre <- (q2pome-rf)/pbeta
#Sortino ratio
downs <- testrr[,c(count)]
downs[downs>0]<-0
downsig <- var(downs, use="pairwise") * 209
downv <- t(W)%*%downsig%*%W
sotino <- (q2pome-rf)/sqrt(downv)
sig <- var(testrr, use="pairwise")*209
v <- t(W)%*%sig[count,count]%*%W
p2sharpe <- (W%*%meanr[count]-rf)/sqrt(v)
return2017 <- apply(testr,1,sum)

#performance for p1
n <- length(number)
beta <- matrix(0,n,1)
for (i in 1:n){
  beta[i,] <- cov(testrr[,1],testrr[,number[i]])/var(testrr[,1])
}
pbeta <- w%*%beta
print(pbeta)
#CAPM alpha

q2pome <- np[1,207]-1
alpha <- q2pome-pbeta*(rm-rf)-rf
#Treynor ratio
Tre <- (q2pome-rf)/pbeta
#Sortino ratio
downs <- testrr[,c(number)]
downs[downs>0]<-0
downsig <- var(downs, use="pairwise") * 207
downv <- t(w)%*%downsig%*%w
sotino <- (q2pome-rf)/sqrt(downv)
v <- t(w)%*%Sigma[number,number]%*%w
p1sharpe <- (w%*%meanr[number]-rf)/sqrt(v)

return2017 <- apply(testr,1,sum)
po1<-w%*%t(p1)
po2<-W%*%t(p2)
np <- matrix(NA,3,209)
for (i in 209:1){
  np[1,i]<- po1[i+1]/po1[1]
  np[2,i]<- po2[i+1]/po2[1]
  np[3,i]<- testr[i+1,1]/testr[1,1]
}
index <- testrr[,1]
day <- c(1:209)
plot(day,po2,type = "l",col="red",ylim = c(0.95,1.4),ylab = "net asset(m$)")+
lines(day,po1,type = "l",col="green")+lines(day,index,type = "l",col="blue")
q1s <- return2017[number]%*%w
message(sprintf("return in 1st portfolio is %f",q1s))
q2s <- return2017[count]%*%W
message(sprintf("return in 2nd portfolio is %f",q2s))

#
rt<-as.data.frame(matrix(NA,nrow = 1465,ncol = 2943))
for (i in 1:1465){
  rt[i,]<-(data3[i+1,])/(data3[1,])
}

pp1 <- w%*%t(rt[,number])
pp2 <- W%*%t(rt[,count])
pp1 <- pp1[1:1256]
pp2 <- pp2[1:1256]
index <- rt[1:1256,1]
days <- c(1:1256)
plot(days,pp2,type = "l",col="red",ylim = c(1,4),ylab = "net asset(m$)")+
  lines(days,pp1,type = "l",col="green")+lines(days,index,type = "l",col="blue")
rrrttt[1:1256,] <- rrrttt[2:1257,]-rrrttt[1:1256,]
rrrttt <- rrrttt[1:1256,]
testrr <- matrix(NA,209,2200)
for (i in 1:209){
  testrr[i,] <-unlist(testr[i,]/testr[1,]) 
}







n <- length(w)
beta <- matrix(0,n,1)
for (i in 1:n){
  beta[i,] <- cov(rrrttt[,1],rrrttt[,number[i]])/var(rrrttt[,1])
}
pbeta <- w%*%beta
print(pbeta)
#CAPM alpha
me <- matrix(NA,n,1)
rm <- meanr[1]
me<-meanr[number]#here calculate the portfolio expected return
pome <- w%*%me
alpha <- pome-pbeta*(rm-rf)-rf
#Treynor ratio
Tre <- (pome-rf)/pbeta
#Sortino ratio
downs <- rrrttt[,number]
downs[downs>0]<-0
downsig <- var(downs, use="pairwise") * 252
downv <- t(w)%*%downsig%*%w
sortino <- (pome-rf)/sqrt(downv)
V <- t(w)%*%Sigma[number,number]%*%w
q1sh <- (pome-rf)/sqrt(V)

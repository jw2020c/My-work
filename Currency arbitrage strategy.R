#Objective Decription: -----------------------------------------------------------------
#Implement cross-currency trading strategies.  You are able to trade at 5 minute 
#intervals using only market orders (sell at the bid, buy at the ask) for any 
#portion of the bid and ask sizes at the specified prices.  However, you are only 
#allowed to trade (buy or sell, but not both) in any of the six markets once 
#(and only once) in each five minute interval.  You may abstain from trading in 
#any market during any of the 5 minute intervals.  Using the one week of semi-colon 
#separated market data attached (Assignment 1 ¨C Data.txt), develop a trading strategy 
#for each 5-minute interval with zero net currency position that maximizes your profit.  
#If no profitable strategy exists, you may abstain from trading within the interval.

setwd("C:/Users/23721/OneDrive/Desktop/HW/")
data1 <- read.csv("data.csv",header = TRUE)
arb <- function(ub,ubs,bu,bus,ue,ues,eu,eus,uj,ujs,ju,jus,ej,ejs,je,jes,eb,ebs,be,bes,jb,jbs,bj,bjs){
  #data reframe
  ubs <- ubs*ub
  ub <- 1/ub
  ues <- ues*ue 
  ue <- 1/ue
  jes <- jes*je
  je <- 1/je
  jus <- jus*ju
  ju <- 1/ju
  ebs <- ebs*eb
  eb <- 1/eb
  jbs <- jbs*jb
  jb <- 1/jb
  #3currency loops
  uebu <- ue*eb*bu
  ubeu <- ub*be*eu
  ubju <- ub*bj*ju
  ujbu <- uj*jb*bu
  ejbe <- ej*jb*be
  ebje <- eb*bj*je
  ujeu <- uj*je*eu
  ueju <- ue*ej*ju
  #4currency loops
  ubeju <- ub*be*ej*ju
  ubjeu <- ub*bj*je*eu
  uejbu <- ue*ej*jb*bu
  uebju <- ue*eb*bj*ju
  ujebu <- uj*je*eb*bu
  ujbeu <- uj*jb*be*eu
  arbi <- c(uebu,ubeu,ubju,ujbu,ejbe,ebje,ujeu,ueju,
            ubeju,ubjeu,uejbu,uebju,ujebu,ujbeu)
  #define max order size(here we can ensure 0 ending position)
  uebus <- min(ues,ebs/ue,bus/(ue*eb))
  ubeus <- min(ubs,bes/ub,eus/(ub*be))
  ubjus <- min(ubs,bjs/ub,jus/(ub*bj))
  ujbus <- min(ujs,jbs/uj,bus/(uj*jb))
  ejbes <- min(ejs,jbs/ej,bes/(ej*jb))
  ebjes <- min(ebs,bjs/eb,jes/(eb*bj))
  ujeus <- min(ujs,jes/uj,eus/(uj*je))
  uejus <- min(ues,ejs/ue,jus/(ue*ej))
  ubejus <- min(ubs,bes/ub,ejs/ub/be,jus/ub/be/ej)
  ubjeus <- min(ubs,bjs/ub,jes/ub/bj,eus/ub/bj/je)
  uejbus <- min(ues,ejs/ue,jbs/ue/ej,bus/ue/ej/jb)
  uebjus <- min(ues,ebs/ue,bjs/ue/eb,jus/ue/eb/bj)
  ujebus <- min(ujs,jes/uj,ebs/uj/je,bus/uj/je/eb)
  ujbeus <- min(ujs,jbs/uj,bes/uj/jb,eus/uj/jb/be)
  arbis <- c(uebus,ubeus,ubjus,ujbus,ejbes,ebjes,ujeus,uejus,
              ubejus,ubjeus,uejbus,uebjus,ujebus,ujbeus)
  maxp <- c(rep(-1,14))
  for (i in 1:14){
    maxp[i] <- (arbi[i]-1)*arbis[i]
  }
  maxp[5] <- maxp[5]*eu
  maxp[6] <- maxp[6]*eu
  win<-which.max(maxp)
  #text output
  text <- matrix(NA,14,5)
  text[1,] <- c(paste("BUY,EUR/USD-",round(uebus*ue,2)),paste("BUY,BTC/EUR-",round(uebus*ue*eb,2)),paste("SELL,BTC/USD-",round(uebus*ue*eb,2)),"",round(maxp[1],2))
  text[2,] <- c(paste("BUY,BTC/USD-",round(ubeus*ub,2)),paste("SELL,BTC/EUR-",round(ubeus*ub,2)),paste("SELL,EUR/USD-",round(uebus*ub*be)),"",round(maxp[2],2))
  text[3,] <- c(paste("BUY,BTC/USD-",round(ubjus*ub,2)),paste("SELL,BTC/JPY-",round(ubjus*ub,2)),paste("BUY,USD/JPY-",round(ubjus*ub*bj*ju,2)),"",round(maxp[3],2))
  text[4,] <- c(paste("SELL,USD/JPY-",round(ujbus,2)),paste("BUY,BIT/JPY-",round(ujbus*uj*jb,2)),paste("SELL,BTC/USD-",round(ujbus*uj*jb,2)),"",round(maxp[4],2))
  text[5,] <- c(paste("SELL,EUR/JPY-",round(ejbes,2)),paste("BUY,BIT/JPY-",round(ejbes*ej*jb,2)),paste("SELL,BTC/EUR-",round(ejbes*ej*jb,2)),"",round(maxp[5],2))
  text[6,] <- c(paste("BUY,BTC/EUR-",round(ebjes*eb,2)),paste("SELL,BIT/JPY-",round(ebjes*eb,2)),paste("BUY,EUR/JPY-",round(ejbes*ej*jb*be,2)),"",round(maxp[6],2))
  text[7,] <- c(paste("SELL,USD/JPR-",round(ujeus,2)),paste("BUY,EUR/JPY-",round(ujeus*uj*je,2)),paste("SELL,EUR/USD-",round(ujeus*uj*je,2)),"",round(maxp[7],2))
  text[8,] <- c(paste("BUY,EUR/USD-",round(uejus*ue,2)),paste("SELL,EUR/JPY-",round(uejus*ue,2)),paste("BUY,USD/JPY-",round(uejus*ue*ej*ju,2)),"",round(maxp[8],2))
  #ubejus,ubjeus,uejbus,uebjus,ujebus,ujbeus
  text[9,] <- c(paste("BUY,BTC/USD-",round(ubejus*ub,2)),paste("SELL,BTC/EUR-",round(ubejus*ub,2)),paste("SELL,EUR/JPY-",round(ubejus*ub*be,2)),paste("BUY,USD/JPY-",round(ubejus*ub*be*ej*ju,2)),round(maxp[9],2))
  text[10,] <- c(paste("BUY,BTC/USD-",round(ubjeus*ub,2)),paste("SELL,BTC/JPY-",round(ubjeus*ub,2)),paste("BUY,EUR/JPY-",round(ubjeus*ub*bj*je,2)),paste("SELL,EUR/USD-",round(ubjeus*ub*bj*je,2)),round(maxp[10],2))
  text[11,] <- c(paste("BUY,EUR/USD-",round(uejbus*ue,2)),paste("SELL,EUR/JPY-",round(uejbus*ue,2)),paste("BUY,BTC/JPY-",round(uejbus*ue*ej*jb,2)),paste("SELL,BTC/USD-",round(uejbus*ue*ej*jb,2)),round(maxp[11],2))
  text[12,] <- c(paste("BUY,EUR/USD-",round(uebjus*ue,2)),paste("BUY,BTC/EUR-",round(uebjus*ue*eb,2)),paste("SELL,BTC/JPY-",round(uebjus*ue*eb,2)),paste("BUY,USD/JPY-",round(uebjus*ue*eb*bj*ju,2)),round(maxp[12],2))
  text[13,] <- c(paste("SELL,USD/JPY-",round(ujebus,2)),paste("BUY,EUR/JPY-",round(ujebus*uj*je,2)),paste("BUY,BTC/EUR-",round(ujebus*uj*je*eb,2)),paste("SELL,BTC/USD-",round(ujebus*uj*je*eb,2)),round(maxp[13],2))
  text[14,] <- c(paste("SELL,USD/JPY-",round(ujbeus,2)),paste("BUY,BTC/JPY-",round(ujbeus*uj*jb,2)),paste("SELL,BTC/EUR-",round(ujbeus*uj*jb,2)),paste("SELL,EUR/USD-",round(ujbeus*uj*jb*be,2)),round(maxp[14],2))
    if (maxp[win]>0){
    outstate <- list("profit"=maxp[win],"output"=text[win,])
    return(outstate)
  } else{
      return(NA)
      break
  }
}
n <- nrow(data1)
arbitrage <- as.data.frame(matrix(NA,n,8))
names(arbitrage) <- c("date","time","step1","step2","step3","step4","profit","position")
arbitrage[,8] <- 0
arbitrage[,1:2] <- data1[,1:2]
for (i in 1:n){
  result <- arb(bu=data1[i,3],bus=data1[i,4],
                ub=data1[i,5],ubs=data1[i,6],
                be=data1[i,7],bes=data1[i,8],
                eb=data1[i,9],ebs=data1[i,10],
                bj=data1[i,11],bjs=data1[i,12],
                jb=data1[i,13],jbs=data1[i,14],
                eu=data1[i,15],eus=data1[i,16],
                ue=data1[i,17],ues=data1[i,18],
                uj=data1[i,19],ujs=data1[i,20],
                ju=data1[i,21],jus=data1[i,22],
                ej=data1[i,23],ejs=data1[i,24],
                je=data1[i,25],jes=data1[i,26])
  arbitrage[i,3:7] <- result$output
}
p <- rep(0,n)
p[1] <- as.numeric(arbitrage[1,7])
for (i in 2:n){
  p[i] <- as.numeric(c(arbitrage[i,7]))+p[i-1]
}
x <- c(paste(arbitrage[,1],arbitrage[,2]))
benefit <- as.data.frame(matrix(c(x,p), ncol = 2))
names(benefit) <- c("time","cumulative profit")
write.csv(benefit,file="C:/Users/23721/OneDrive/Desktop/HW/data03",quote=F,row.names = F)
write.table (arbitrage, file ="C:/Users/23721/OneDrive/Desktop/HW/data02", sep =";", row.names =FALSE, col.names =TRUE, quote =FALSE)
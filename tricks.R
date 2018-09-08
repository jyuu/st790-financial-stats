library(quantmod)
symbols <- c("GOOG", "AMZN", "FB", "AAPL")
getSymbols(symbols, src = 'yahoo')
closing.prices <- merge.xts(GOOG[,4], AMZN[,4], FB[,4], AAPL[,4])["2016-12-30/"]
# convert to a series of returns
library(TTR)
price.returns = ROC(closing.prices)
# portfolio returns will be means
rowMeans(price.returns)
# generally rely on closing price to rebalance at the start of the year (12/31)

returns<-rbind(c(-0.05,0.04,0.37),c(0.15,0.02,-0.07))
weights<-rbind(c(0.5,0.1,0.4),c(0.4,0.2,0.4))
# > returns
# [,1] [,2]  [,3]
# [1,] -0.05 0.04  0.37
# [2,]  0.15 0.02 -0.07
# > weights
# [,1] [,2] [,3]
# [1,]  0.5  0.1  0.4
# [2,]  0.4  0.2  0.4

ones <- matrix(1,ncol=ncol(returns),nrow=nrow(returns))
add <- returns
percentChange <- ones+add
# > percentChange
# [,1] [,2] [,3]
# [1,] 0.95 1.04 1.37
# [2,] 1.15 1.02 0.93

# calculate the total change with weights 
percentChangeSums <- rowSums(percentChange*weights) 
# calculate the weights AFTER accounting for the returns 
WeightsBefore <- weights * percentChange
# calculate how much to invest in shares to have original weights 
ShareAfter <- percentChangeSums * weights 
# check that you have original weights 
WeightsAfter <- ShareAfter/percentChangeSums
# thus new 
rebalanced.weights <- ShareAfter
# > rebalanced.weights
# [,1]   [,2]   [,3]
# [1,] 0.5635 0.1127 0.4508
# [2,] 0.4144 0.2072 0.4144


# using r package perf ----------------------------------------------------
returns<-rbind(c(-0.05,0.04,0.37),c(0.15,0.02,-0.07))
weights<-rbind(c(0.5,0.1,0.4),c(0.4,0.2,0.4))
library(PerformanceAnalytics)
ret <- xts(returns, order.by= as.Date(c("2015-06-30", "2015-07-31")))
wts <- xts(weights, order.by= as.Date(c("2015-05-31","2015-06-30")))
#  Period returns
returns_PA <- Return.portfolio(ret, wts)
returns_direct <- reclass(sapply(1:nrow(wts), function(n) ret[n,]%*%t(wts[n,])), ret)

# Cummulative returns
returns_PA_cum <- Return.cumulative(returns_PA)
returns_direct_cum <- prod(returns_direct+1) -1

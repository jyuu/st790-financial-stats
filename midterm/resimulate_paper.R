rm(list=ls())

# load --------------------------------------------------------------------
library(quantmod)

# see how she chose papers ------------------------------------------------
# how many citations? 
# ask RM... how he goes about looking.. what is his workflow? ? 

# read  -------------------------------------------------------------------
# 7 “Momentum in Futures Market”, Norges Bank Investment Management, Discussion Note #1-2014.

# get sp500 ---------------------------------------------------------------

# get the financial data  -------------------------------------------------
# Portfolio rebalancing can exacerbate drawdowns during major trends, as in 2008-2009 and 2000-2002. 

# start <- as.Date("2008-01-01")
# end <- as.Date("2009-12-31")
# getSymbols("SNP", src = "yahoo", from = start, to = end)

# getSymbols("^TNX", src = "yahoo", from = start, to = end)
# saveRDS(SNP, "data/SNP_0809.rds")
# saveRDS(TNX, "data/TNX_0809.rds")
# # try different period as well 
# start <- as.Date("2000-01-01")
# end <- as.Date("2002-12-31")
# getSymbols("SNP", src = "yahoo", from = start, to = end)
# getSymbols("^TNX", src = "yahoo", from = start, to = end)
# saveRDS(SNP, "data/SNP_0002.rds")
# saveRDS(TNX, "data/TNX_0002.rds")

# viz raw data ------------------------------------------------------------
TNX1 <- readRDS("data/TNX_0002.rds")
TNX2 <- readRDS("data/TNX_0809.rds")
tnx1 <- TNX1$TNX.Open # just the value at the open 
tnx2 <- TNX2$TNX.Open 
plot(as.numeric(tnx1), type = "l", ylim = c(0, 7))
lines(as.numeric(tnx2)) # obviously very different 
#candleChart(TNX1, up.col = "black", dn.col = "red", theme = "white")
SNP1 <- readRDS("data/SNP_0002.rds")
snp1 <- SNP1$SNP.Open


# demo drawdowns ----------------------------------------------------------
library(PerformanceAnalytics)
snp.roc <- ROC(snp1,n=1,type="discrete")
snp.draw <- Drawdowns(snp.roc)
plot.zoo(log(snp1),plot.type="single",
         col=c(2,3,4),ylab="log Price",xlab=NA,main="Dow Jones Indexes")
drawdowns <- table.Drawdowns(snp.roc[,1])
drawdowns.dates <- cbind(format(drawdowns$From),format(drawdowns$To))
drawdowns.dates[is.na(drawdowns.dates)] <- format(index(DJ.roc)[NROW(DJ.roc)])
chart.Drawdown(SNP1)


# demo cumulative returns -------------------------------------------------
data(managers)
test <- Return.cumulative(managers[,1,drop=FALSE])
test <- Return.cumulative(managers[,1:8])
test <- Return.cumulative(managers[,1:8],geometric=FALSE) 
test <- Return.cumulative(snp1)


# plot of cumulative return with drift ------------------------------------
SNP <- readRDS("data/SNP_0809.rds")
TNX <- readRDS("data/TNX_0809.rds")
snp <- SNP$SNP.Close
tnx <- TNX$TNX.Close
s <- monthlyReturn(snp)
t <- monthlyReturn(tnx)
cs <- cumprod(1+s)-1
ts <- cumprod(1+t)-1

# for a portfolio that maintained the 60/40 split each month
port <- .6*s + .4*t
port.cumreturn <- cumprod(port+1)-1
# for a portfolio that allowed drift from initial investment 
drift <- .6*cs + .4*ts
# viz 
plot(port.cumreturn)
lines(drift)

# another way to calculate the returns  -----------------------------------
library(TTR)
# example of equal weighted portfolio everyday 
start <- as.Date("2008-01-01")
end <- as.Date("2010-12-31")
symbols <- c("SNP", "^TNX")
getSymbols(symbols, src = 'yahoo', from = start, to = end)
closing.prices <- merge.xts(SNP[,4], TNX[,4])["2008-01-02/"]
price.returns <- ROC(closing.prices)
drift <- round(Return.portfolio(price.returns), 4)
d <- cumsum(drift)
fixed <- round(Return.rebalancing(price.returns, rebalance_on = "months"), 4)
f <- cumsum(fixed)
plot(f)

# another way to do rebalancing  ------------------------------------------
data(edhec)
data(weights)
round(Return.portfolio(edhec), 4)
round(Return.portfolio(edhec, contribution = TRUE), 4)
round(Return.rebalancing(edhec, weights, rebalance_on = "years"), 4)

symbols <- c("GOOG", "AMZN", "FB", "AAPL")
getSymbols(symbols, src = 'yahoo')
closing.prices <- merge.xts(GOOG[,4], AMZN[,4], FB[,4], AAPL[,4])["2016-12-30/"]
price.returns <- ROC(snp)

# show allocation in port weight ------------------------------------------
w1 <- .6 # for stocks 
w2 <- .4 # for bonds
# total shares 
t <- 100 
# on the day .. 
numS <- t*w1 
numT <- t*w2
snp <- as.numeric(snp1)
tnx <- as.numeric(tnx1)
ira <- numS*snp[1] + numT*tnx[1] # initial value 
# calculate the value over time with this allocation 
n <- min(length(snp), length(tnx)) # not sure if times line up!
ira <- numS*snp[1:n] + numT*tnx[1:n] 
plot(ira, type = "l") 
# what if we rebalance every month?  
idx <- seq(1, n, by = 29)
# example at first month after 
new_ira <- numS*snp[30] + numT*tnx[30]
cat("SNP should be: ", w1*new_ira, " but it is at ", numS*snp[30], "\n") 
cat("TNX should be: ", w2*new_ira, " but it is at ", numT*tnx[30], "\n")
# let's sell the winners and buy the losers 
(numS*snp[30] - w1*new_ira) / tnx[30 ]  # what happen when add 24 shares of TNX? 
(numT*tnx[30] + tnx[30]*24)  # awesome close to w2*new_ira 
# let's make a function - make sure it applies to EACH time period 
rebalance <-  function(new, index){
  if(w1*new - numS*snp[index] < 0){
    # we want to sell the winner 
    new_tnx <- (numS*snp[index] - w1*new) / tnx[index]
  } else {
    
  }
}
lines()
# assume monthly is every 30 days 
idx <- seq(1, length(tnx1), by = 30) 
plot(idx, tnx1[idx]) 

# fig 7 
# 600 bps at the bottom (Figures 1 and 2)
# Rebalancing, however, is here to stay. Passive portfolio construction in which weights are allowed to drift is not a realistic alternative, because allocations can get extreme after only a few year


# momentum overlay  -------------------------------------------------------
# operating on daily time scale 
# But, as divergences in asset performance stabilise, the momentum overlay will cut out and allow the full rebalance to occur.
# improve the timing of the rebalance.. 
# FINAL -- use SURVIVAL MODEL to improve the TIMING?? 


# autocorrelation ---------------------------------------------------------


# fixed weight rebalancing  -----------------------------------------------


# drawdowns ---------------------------------------------------------------

# trending environment ----------------------------------------------------
# equities keep losing relative to bonds 

# trendless environment ---------------------------------------------------
# characterized by mean reversion 
# show how rebalancing is good 


# momentum overlay  -------------------------------------------------------
# show how it can hedge the drawdown risks  
# try 10%, 20% other rates of overlay 
# the MO is applied as individual trend following strategies  
# on individual assets with risk allocation set to 10% of the constant mix port 


# Mo ----------------------------------------------------------------------
# moving average cross over model employed at AHL 
# target volatility of 10%, turns over positions 4 times a yr 
# show how it increases the sharpe ratio 
# improves skewness 


# take talking pts from movie  --------------------------------------------
# about how people will make a run on the banks without confidence 
# the whole system operates on trust 
# why happens when that is true 
# 

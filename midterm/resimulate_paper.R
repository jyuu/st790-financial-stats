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
start <- as.Date("2008-01-01")
end <- as.Date("2009-12-31")
getSymbols("SNP", src = "yahoo", from = start, to = end)
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

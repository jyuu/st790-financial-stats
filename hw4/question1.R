# load the necessary financial data from quantmod 
library(quantmod)
start <- as.Date("2005-01-01")
end <- as.Date("2015-01-01")
assets <- c(# "DVMT" # DELL is DROPPED since not provided by Quantmod
  "F", 
  "GE", 
  "IBM", 
  "JNJ",
  "MRK",
  "SPY")
getSymbols(assets, from = start, to = end) 
# get the 3 mo tresury data 
getSymbols("DGS3MO", src = "FRED")
# merge into one df 
closing.prices <- merge.xts(DGS3MO, 
                            F[, 4], 
                            GE[, 4],
                            IBM[, 4], 
                            JNJ[, 4], 
                            MRK[, 4], 
                            SPY[, 4])

# filter out the information for the timeframe of interest 
dat <- closing.prices
data <- dat["2005-01-01/2015-01-01"]
data <- na.omit(data)
# get daily log returns 
by_day <- data.frame() 
for(i in 1:ncol(data)){
  temp <- data[, i]
  daily <- log(dailyReturn(temp)+1)
  by_day <- cbind(by_day, daily)
  colnames(by_day)[i] <- strsplit(names(temp), "[.]")[[1]][1]
}
# get the excess return 
excess_return <- data.frame()
for(j in 2:ncol(by_day)){
  temp <- by_day[, j] - by_day[, 1]
  excess_return <- cbind(excess_return, temp)
  colnames(excess_return)[j-1] <- names(temp)
}
# get the covariance matrix 
r <- cbind(by_day[,1], excess_return)
r[is.infinite(r)] <- NA
r <- na.omit(r)
Sigma <- cov(r)
# weight vector given 
w <- c( 0.3, 0.4, -0.2, 0.1, 0.2, 0, 0.4)
w <- w/sum(w) # need to normalize since we removed DELL 
# risk of portfolio 
risk <- sqrt(w %*% Sigma %*% w)
# print 
cat("The risk is: ", risk, "\n")
# equal weight 
w2 <- rep(1, length(w))/length(w)
# new risk 
risk2 <- sqrt(w2 %*% Sigma %*% w2)
# print 
cat("The new risk is: ", risk2, "\n")

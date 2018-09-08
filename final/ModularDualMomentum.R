### MODULAR DUAL MOMENTUM ###

## setup and data wrangling

# libraries
library(PerformanceAnalytics); library(quantmod)
#devtools::install_github("joshuaulrich/quantmod", ref="157_yahoo_502")

# download data from yahoo/google
US_Equities     <- 'SPY'
exUS_Equities   <- 'CWI'
Yield_Bonds     <- 'CSJ'
Credit_Bonds    <- 'HYG' 
T_Bills         <- 'TLT' 
symbols <- c(US_Equities, exUS_Equities, Yield_Bonds, Credit_Bonds, T_Bills)

getSymbols(symbols, from="1998-01-01") #, src="google")

T_Bills_Yield   <- '^TYX'
getSymbols(T_Bills_Yield, from="1998-01-01")
T_Bills_Yield   <- 'TYX' # need to remove the '^'

symbols <- c(symbols, T_Bills_Yield)

# create daily and monthly price series
dly.prices <- list()
mthly.prices <- list()

for(i in 1:length(symbols)) { # get adjusted closing prices
  dly.prices[[i]] <- Ad(get(symbols[i]))
  mthly.prices[[i]] <- Cl(to.monthly(dly.prices[[i]],indexAt='lastof',drop.time=TRUE))
}

dly.prices <- do.call(cbind, dly.prices)
colnames(dly.prices) <- gsub("\\.[A-z]*", "", colnames(dly.prices))

mthly.prices <- do.call(cbind, mthly.prices)
colnames(mthly.prices) <- colnames(dly.prices)

## function for running the Dual Momentum backtest
MomTest <- function(sector1, sector2, dailyPrices, mthlyPrices, formationPeriod, riskFreeTicker)
{
  ### perform dual momentum simulation on 2 sectors of a module 
  ### from a matrix of daily and monthly prices
  
  # args:
  # sector1/sector2 : names of sectors to apply dual momentum
  # dailyPrices : matrix or data frame of daily prices including treasuries
  # mthlyPrices : matrix or data frame of monthly prices including treasuries
  # formationPeriod : integer of number of months over which to calculate momentum
  # riskFreeTicker: ticker of the "risk free" ETF (usually BIL or TYT)
  
  module.assets <- c(sector1, sector2)
  
  # identify newest instrument and align others - etf with most NAs has the least data
  na_count <-sapply(dailyPrices[, module.assets], function(y) sum(is.na(y)))
  maxNAetf <- which.max(na_count)
  dailyPrices <- dailyPrices[!is.na(dailyPrices[, names(maxNAetf)])] 
  mthlyPrices <- mthlyPrices[!is.na(mthlyPrices[, names(maxNAetf)])] 
  
  module.dly.prices <- dailyPrices[, c(module.assets, riskFreeTicker)]
  module.mthly.prices <- mthlyPrices[, c(module.assets, riskFreeTicker)]
  module.mthly.returns <- ROC(x = module.mthly.prices, n = 1, type = "discrete", na.pad = TRUE)
  
  # get treasury yield for relevant period
  t.yield.dly <- dailyPrices[, riskFreeTicker]
  t.yield.mthly <- mthlyPrices[, riskFreeTicker]
  
  N <- formationPeriod # formation period
  
  # calculate treasury return
  module.roc <- ROC(module.mthly.prices[, c(module.assets, riskFreeTicker)], n = N, type = "discrete")
  module.roc[, riskFreeTicker] <- N*t.yield.mthly/1200 #formation period yield on t-BILls
  module.mthly.returns[, riskFreeTicker] <- t.yield.mthly/1200
  
  # calculate strategy returns
  module.rank <- t(apply(-module.roc, 1, rank, na.last = "keep"))
  module.rank <- as.xts(module.rank)
  module.rank.lag <- lag(module.rank, k = 1, na.pad = TRUE)
  module.rank.lag[module.rank.lag > 1] <- NA # this works because T-Bill yield is always positive (for now!)
  returns <- as.matrix(module.rank.lag) * as.matrix(module.mthly.returns)
  
  port <- rowSums(returns, na.rm = TRUE)
  
  return(port)
}

## run modular dual momentum simulations and plot results

# formation period
N <- 12 

# simulations
equities <- MomTest(US_Equities, exUS_Equities, dly.prices, mthly.prices, N, T_Bills_Yield)
bonds <- MomTest(Credit_Bonds, Yield_Bonds, dly.prices, mthly.prices, N, T_Bills_Yield)

# equities benchmarks
spy.returns <- ROC(x = mthly.prices$SPY, n = 1, type = "discrete", na.pad = TRUE)
cwi.returns <- ROC(x = mthly.prices$CWI, n = 1, type = "discrete", na.pad = TRUE)
start.eq <- (names(head(equities, 1)))
end.eq <- (names(tail(equities, 1)))

# bonds benchmarks
csj.returns <- ROC(x = mthly.prices$CSJ, n = 1, type = "discrete", na.pad = TRUE)
hyg.returns <- ROC(x = mthly.prices$HYG, n = 1, type = "discrete", na.pad = TRUE)
start.bnd <- (names(head(bonds, 1)))
end.bnd <- (names(tail(bonds, 1)))

# plot results of individual simulations vs benchmarks
charts.PerformanceSummary(R = data.frame(equities, spy.returns[paste0(start.eq,'::',end.eq)], cwi.returns[paste0(start.eq,'::',end.eq)]), 
Rf = 0, geometric = TRUE, main = paste0("Dual Momentum ETF Rotation - Equities\nLong Only"))

charts.PerformanceSummary(R = data.frame(bonds, csj.returns[paste0(start.bnd,'::',end.bnd)], hyg.returns[paste0(start.bnd,'::',end.bnd)]), 
Rf = 0, geometric = TRUE, main = paste0("Dual Momentum ETF Rotation - Bonds\nLong Only"))

## plot results of equal weighted portfolio of modules

# form portfolio according to date of most recently formed ETF
first.dates <- as.Date(c(head(names(equities), 1), head(names(bonds), 1)))
port.start <- first.dates[order(first.dates)][length(first.dates)]
port.start <- as.character(port.start)

# 60/40 portfolio of equities-bonds modules
portfolio <- (0.6*equities[match(port.start, names(equities)):length(equities)] + 
                0.4*bonds[match(port.start, names(bonds)):length(bonds)])

charts.PerformanceSummary(R = portfolio, Rf = 0, geometric = TRUE, 
                          main = paste0("Dual Momentum ETF Rotation - Portfolio of Modules\nLong Only"))


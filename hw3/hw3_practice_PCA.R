rm(list=ls())
RiskFactors = read.csv("~/Downloads/RiskFactors.csv")
RiskFactors[,2] = RiskFactors[,2]*100       #convert to percentage
RiskFactors[,5:7] = RiskFactors[,5:7]/100       #convert to percentage
RiskFactors[,8] = RiskFactors[,8]*100        #basis point  

Dates = as.vector(RiskFactors[,1])
Dates = strptime(Dates, "%m/%d/%Y") #convert to  POSIXlt (a date class)
Names = c("Date", "Market risk", "Volatility risk", "Exchange rate risk",
          "Maturity risk", "Default risk", "Liquidity risk", 
          "Size effect", "Short term rate")

postscript("Fig61.eps", width=4.4, height=5.5, horizontal=F, pointsize=8)
par(mfrow = c(4, 2),mar=c(2,3,.5,.5)+0.1, cex=0.8)
for(i in 2:9)
  plot(Dates, RiskFactors[,i], type="l", ylab=c("",Names[i]), col=4)
dev.off()


# PCA Analysis ------------------------------------------------------------

X = RiskFactors[801:2630,2]        #2-23-04 to 2-28-2011
Y = RiskFactors[801:2630,3:9]  
tmp = cor(RiskFactors[801:2630,2:9])
tmp
resid = resid(lsfit(X, Y))         # take the return of SP500 out
cor(resid)
cor(cbind(X,resid))
eigen(cor(resid))
a = eigen(cor(resid))$vectors[,1]
a = a / sqrt(apply(resid, 2, var))  	#standardize the variables
Factor = resid %*% a
cor(Factor, X)

postscript("Fig64a.eps", width=4.4, height=3, horizontal=F, pointsize=8)
par(mfrow = c(1, 1), mar=c(2,2,2,.5)+0.1, cex=0.8)
plot(Dates[801:2630],   X, 	  type="l", col=2) 
lines(Dates[801:2630],  Factor, lwd=2,    col=4)
title("Realization of the 1st PC")

postscript("Fig64b.eps", width=4.4, height=2.5, horizontal=F, pointsize=8)
par(mfrow = c(1, 2),mar=c(4,3,3,.2)+0.1,  cex=0.8)
plot(X, Factor, pch=16, main = "Dependence of factors", col=4, 
     xlab="SP 500", ylab= c("", "Other Factors"))
plot(X, Factor, pch=16, xlim=c(-2,2), main="Dependence of factors",
     col=4, xlab="SP 500", ylab= c("", "Other Factors"))


#################### Localized PCA with width = 3 mo or 1 y ##########################

n = length(X)
width = 63
Values = NULL        
for(t in 1:(n-width))
{  resid  = resid(lsfit(X[t:(t+width)], Y[t:(t+width),]))
Eigens = eigen(cor(resid))		#princomp, loadings, summary
Values = rbind(Values, (Eigens$values)[1:2])
}
Values03 = round(Values /ncol(Y), 3)            #percentage of contributions

postscript("Fig65.eps", width=4.4, height=3, horizontal=F, pointsize=8)
par(mfrow = c(1, 2), mar=c(2,3,3,.5)+0.1, cex=0.8)
plot(Dates[800+(width+1):n], Values03[,1], type="l", ylim=c(0,1),
     col=4, ylab="percent explained")
lines(Dates[800+(width+1):n], Values03[,2], type="l", col=2)
title("Variance explained (w = 3 months)")


width = 252
Values = NULL        
for(t in 1:(n-width))
{  Eigens = eigen(cor(Y[t:(t+width),]))		#princomp, loadings, summary
Values = rbind(Values, (Eigens$values)[1:2])
}
Values12 = round(Values /ncol(Y), 3)


plot(Dates[800+(width+1):n], Values12[,1], type="l", ylim=c(0,1),
     col=4, ylab="percent explained")
lines(Dates[800+(width+1):n], Values12[,2], type="l", col=2)
title("Variance explained (w = 12 months)")

# prediction power on equities --------------------------------------------

FF100 = matrix(scan("~/Downloads/100_Portfolios_10x10_Daily.txt", skip=20), ncol=101, byrow=T)
dates = FF100[,1]
FF100 = 100*diff(log(FF100[,2:101]))
FF100 = FF100[474:nrow(FF100),]               #2/24/04 -- 12/31/10
dates = dates[475:length(dates)]

# aside what does diff log do? 
joyce = matrix(1:12, ncol = 3)
diff(joyce)

# matching the dates 
D = Dates[802:2589]			      ##Factor Dates  ##+86400 
D = paste(substr(D, 1, 4), substr(D, 6, 7), substr(D, 9, 10), sep="")
D = as.numeric(D)                             #convert dates into numeric

ind = rep(0, length(Dates))                   #matching two diff dates
for(i in 1:length(dates)){
  ind[i] = (1:1788)[D == dates[i]]
}

X2      = X[ind]
Factor2 = Factor[ind]
D1      = Dates[802:2630]
D1      = D1[ind]			      ###the calendar are now dates
Factors.all = RiskFactors[801:2630,2:9]
Factors.all = Factors.all[ind,]

resid = resid(lsfit(X2, FF100))
resid1 = resid                               #dirty outlier 
for(i in 1:100)                              #cleaning outliers
{ tmp = (resid[,i] > 4)
resid[tmp,i] = 4
tmp = (resid[,i] < -4)
resid[tmp, i] = -4
}
FF100   = FF100 - resid1 + resid  

######## Computing multiple R^2 by one and two, 7 factors ##########
resid = resid(lsfit(X2, FF100))				
Rsq1 = 1-apply(resid, 2, var)/apply(FF100,2,var)
resid = resid(lsfit(cbind(X2, Factor2), FF100))
Rsq2 = 1-apply(resid, 2, var)/apply(FF100,2,var)
resid = resid(lsfit(Factors.all, FF100))
Rsq3 = 1-apply(resid, 2, var)/apply(FF100,2,var)

postscript("Fig66.eps", width=4.4, height=3, horizontal=F, pointsize=8)
par(mfrow = c(1, 1), mar=c(2,2,2,.5)+0.1, cex=0.8)
matplot(cbind(Rsq1, Rsq2, Rsq3), type="l", lwd=2, ylim=c(0,1), col=2:4)
title("Percent of variability explained by Risk Factors")

# apply to last five years ------------------------------------------------
rm(list=ls())
FF100 <- matrix(scan("~/workspace/st790-financial-stats/hw3/100_cleaned.txt"), ncol=101, byrow=T)
dates <- FF100[,1] # Time period used: 1/02/12 -- 08/31/18
# get SPX information from quantmod  
library(quantmod)
# start <- as.Date("2013-01-02")
# end <- as.Date("2018-08-31")
# getSymbols("SPY", from = start, to = end) # SPY
# SPX <- SPY[,4]
# names(SPX) <- "spx"
# daily <- log(dailyReturn(SPX)+1)*100
# saveRDS(daily, "~/workspace/st790-financial-stats/hw3/spx.rds")
X <- readRDS("~/workspace/st790-financial-stats/hw3/spx.rds")
# match dates 
D <- time(X) 
D <- paste0(substr(D, 1, 4), substr(D, 6, 7), substr(D, 9, 10))
D <- as.numeric(D)
ind <- rep(0, length(D)) 
for(i in 1:length(ind)){
  ind[i] = (1:1427)[dates[i] == D]
}
dates <- dates[ind]
FF100 <- FF100[ind, ]

# PCA Analysis 
Y <- FF100[,2:101]
resid <- resid(lsfit(X, Y))         # take the return of SP500 out
cor(resid)
cor(cbind(X,resid))
# variance explained by each principle components
p <- 100
eigen(cor(resid))$values[1:3]/p
# regress each port on these 3 principal components 
a <- eigen(cor(resid))$vectors[,1]
a <- a / sqrt(apply(resid, 2, var))  	# standardize the variables
Factor1 <- resid %*% a
resid1 <- resid(lsfit(cbind(X, Factor1), Y))
Rsq1 <- 1-apply(resid1, 2, var)/apply(Y,2,var)

matplot(cbind(Rsq1, Rsq2, Rsq3), type="l", lwd=2, ylim=c(0,1), col=2:4)
title("Percent of variability explained by  Factors")


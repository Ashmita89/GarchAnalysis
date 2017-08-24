getwd()
setwd("C:/Users/Ashmita/Documents/R/R coding/PersonalProject/WorkingDirectory")
#Start of stockdata analysis
options(myseed = 42)
#install.packages('huge')
library(huge)
#install.packages('vars')
#install.packages('quantmod')
library(vars)
#install.packages('forecast')
library(forecast)

#library(ggplot2)
#Getting the stockdata
data(stockdata)
#dev.off()
#image(stockdata$data)
stockdata$info
summary(stockdata$data)

D = length(stockdata$data[1,])
len = length(stockdata$data[,1])

prices = stockdata$data[,1:D]
#another copy of prices for adjusting closing prices
prices_p = stockdata$data[,1:D]
#check for missing values
missingvalues <- any(is.na(prices))
#check for infinity values
inf_value_check <- any(is.infinite(prices))
#get all the symbols of 452 stocks
lab = stockdata$info[1:D,1]
# find number of distinct category
distinct_cat = stockdata$info[1:D,2]
distinct_cat <- unique(distinct_cat)
distinct_cat
prices_length = length(prices[1,])

#First day stock data summary
summary(prices[1,])
company_info = stockdata$info[,1]
prices_day1 = prices[1,] 
df_day1 = data.frame(company_info,prices_day1)
plot(df_day1)

#Further,the stock analysis will be done for random 100 stocks from the sample
#Assign colnames as the symbols for the stocks for easy recognition of stocks

colnames(prices) <- company_info
colnames(prices_p) <- company_info


#random sample of 100
sampleRandom <- function(x) {
  if (!is.null(seed <- getOption("myseed")))
    set.seed(seed)
  sample(ncol(prices), x)
}

selected_stocks_col<- sampleRandom(100)


#Retrieve data of MMM 

MMM_info = stockdata$info[1,]
MMM_data = stockdata$data[,1]
days_data = seq(1:1258)

MMM_df = data.frame(days_data,MMM_data)
#check if the dataframe has any missing or infinite values
abnormal_indx <- apply(MMM_df, 2, function(x) any(is.na(x) | is.infinite(x)))
View(abnormal_indx)
library(ggplot2)
#plot the stock prices for MMM for 1258 days
ggplot(MMM_df,aes(x=days_data,y=MMM_data)) + 
  geom_line(aes(color="MMM")) +
  labs(color="Legend") +
  scale_colour_manual("", breaks = c("MMM"),
                      values = c("blue")) +
  ggtitle("Closing Stock Prices: MMM") + 
  theme(plot.title = element_text(lineheight=.7, face="bold"))



#from the above plot we see there is a steep drop of closing prices
#at 187th day from 140.54 to 69.07 this indicates almost a 2:1 stock split 
#lets check it 
dim = length(selected_stocks_col)
selected_stocks <- prices[,selected_stocks_col]
#Find corresponding categories
cat_selected_stocks <- stockdata$info[selected_stocks_col,]
#Plot all stocks selected 
selected_stocks_df<- data.frame(days_data,selected_stocks)

require(reshape2)
dev.off()
df <- melt(selected_stocks_df ,  id.vars = 'days_data', variable.name = 'closing_prices')

# plot on same grid, each series colored differently -- 
# good if the series have same scale
ggplot(df, aes(x=days_data,y=value))+geom_line(aes(colour = closing_prices))
displayCharts <- function(prices,lab,nrow=3,ncol=4,sleepSecs=4) {
  Dims=length(prices[1,])
  for(chartGrp in (1:ceiling(Dims/(nrow*ncol)))) {
    print(chartGrp)
    par(mar=c(3.82,1.82,1.82,0.82))
    par(mfrow=c(nrow,ncol))
    for(i in 1:(nrow*ncol)) {
      j = ((chartGrp-1)*nrow*ncol+i)
      if(j <= Dims) {
        print(paste(j,lab[j]))
        plot(prices[,j],type="l",xlab=paste(j,lab[j]))
      }
    }
    Sys.sleep(sleepSecs)
  }
}
##displayCharts(selected_stocks[,1:dim],lab[1:dim],sleepSec=10)

#Adjust prices ACE,AKAM,MO because of sharp prices change may be due to stock splits
#and reverse splits

splitAdjust <- function(prices,symbol) 
  {
  len = length(prices)
  origFinalPrice = prices[len]
  for(j in 2:len) 
    {
    split = 0
    #print(paste(prices[j-1],prices[j]))
    if(prices[j-1] >= 1.4*prices[j]) 
      {
      split = +1.5 # a 3 for 2
      if(prices[j-1] >= 1.8*prices[j])
        split = +2 #At least a 2 for 1
      if(prices[j-1] >= 2.9*prices[j])
        split = +3 #Ah a 3 for 1
      if(prices[j-1] >= 3.9*prices[j])
        split = +4 #Ah a 3 for 1
      if(prices[j-1] >= 4.9*prices[j])
        stop(paste(symbol,'detected more than 4:1 split'))
      print(paste("split adjusting",symbol,split,
                  j,prices[j-1],prices[j]))
    } #reverse splits: price increases so divide
    if(prices[j-1] <= prices[j]/1.4) {
      split = -1.5
      if(prices[j-1] <= prices[j]/1.9 &&
         prices[j-1] >= prices[j]/2.1)
        split = -2
      if(prices[j-1] <= prices[j]/2.9 &&
         prices[j-1] >= prices[j]/3.1)
        split = -3
      if(prices[j-1] <= prices[j]/5.8 &&
         prices[j-1] >= prices[j]/6.2)
        split = -6
      if((prices[j-1] <= prices[j]/7.7) &&
         (prices[j-1] >= prices[j]/8.3))
        split = -8
      if((prices[j-1] <= prices[j]/9.7) &&
         (prices[j-1] >= prices[j]/10.3))
        split = -10
      if((split == 0) && (prices[j-1] <= prices[j]/2.9))
        stop(paste(symbol,
                   'detected more than double reverse split'))
      print(paste("reverse split adjusting",j,symbol,j,
                  split,prices[j-1],prices[j]))
    }
    if(split != 0) {
      for(k in j:len) { #adjust all prices to right from j:len
        if(symbol=="C")
          prices[k] = prices[k]/10 #hard coded for Citi
        else if(split == +1.5)
          prices[k] = 1.5*prices[k] # 3 for 2
        else if(split == +2)
          prices[k] = 2*prices[k] # 2 to 1
        else if(split == +3)
          prices[k] = 3*prices[k] # 3 to 1
        else if(split == +4)
          prices[k] = 4*prices[k] # 4 to 1
        else if(split == -1.5)
          prices[k] = prices[k]/1.5 # 2 to 3 rev
        else if(split == -2)
          prices[k] = prices[k]/2 # 1 to 2 rev
        else if(split == -3)
          prices[k] = prices[k]/3 # 1 to 2 rev
        else if(split == -6)
          prices[k] = prices[k]/6 # 1 to 8 rev
        else if(split == -8)
            prices[k] = prices[k]/8 # 1 to 8 rev
        else if(split == -10)
            prices[k] = prices[k]/10 # 1 to 10 rev
        else stop('splitAdjust internal error')
      }
    }
  }
  finalPrice = prices[len]
  return(prices*origFinalPrice/finalPrice)
}


#MMM adjusted price
MMMidx <- match('MMM',lab)
plot(prices[,MMMidx],type='l',xlab='MMM')
MMM_adjp<-splitAdjust(prices[,MMMidx],c('MMM'))
#"split adjusting MMM 2 188 140.54 69.07"
#plot(adjp,type='l',xlab='MMMadj')
MMM_df_adj= data.frame(days_data,MMM_adjp)
ggplot(MMM_df_adj,aes(x=days_data,y=MMM_adjp)) + 
  geom_line(aes(color="MMM")) +
  labs(color="Legend") +
  scale_colour_manual("", breaks = c("MMM"),
                      values = c("blue")) +
  ggtitle("Adjusted Closing Stock Prices: MMM") + 
  theme(plot.title = element_text(lineheight=.7, face="bold"))

price_adjust <- prices_p

findsplitAdjustedP <- function(prices,price_adjust,isSplitAdjusted=TRUE) {#Find R: logrets:
  len = dim(prices)[1]
  D <<- dim(prices)[2]
  for(i in 1:D) {
    #print(i)
    if(!isSplitAdjusted) price_adjust[,i] <<- splitAdjust(prices[,i],lab[i])
  }
  price_adjust
}

AdjustedPrices <- findsplitAdjustedP(prices_p,price_adjust,isSplitAdjusted=FALSE)

findR <- function(prices,isSplitAdjusted=TRUE) {#Find R: logrets:
  len = dim(prices)[1]
  D <<- dim(prices)[2]
  R = matrix(nrow=(len-1),ncol=D)
  for(i in 1:D) {
    #print(i)
    if(!isSplitAdjusted) prices[,i] <<- splitAdjust(prices[,i],lab[i])
    R[,i] = 100*diff(log(prices[,i])) ###log rets
  }
  R
}

R <- findR(prices_p,isSplitAdjusted=FALSE)

#Check for missing values in R and prices_p

missingvalues_R <- any(is.na(prices_p))
selected_adjust_cprice <- AdjustedPrices[,selected_stocks_col]
D_sacp <<- dim(selected_adjust_cprice)[1]
selected_adjust_cprice <- selected_adjust_cprice[2:D_sacp,]
selected_stocks_ret <- R[,selected_stocks_col]
displayCharts(selected_stocks_ret[,1:dim],lab[1:dim],sleepSec=10)
dev.off()
#Find best and worst stock of the 20 stocks
best_stock = which.max(apply(selected_stocks_ret, 2, sum))
plot(selected_stocks_ret[, best_stock])

worst_stock = which.min(apply(selected_stocks_ret, 2, sum))
plot(selected_stocks_ret[, worst_stock])
#Principal components analysis of return using principal()
final_df <- as.data.frame(t(selected_stocks_ret))
rownames(final_df) <- company_info[selected_stocks_col]
colnames(selected_stocks_ret)<- company_info[selected_stocks_col]

library(psych)
fa.parallel(selected_stocks_ret, fa="pc", n.iter=15,
            show.legend=FALSE, main="Scree plot with parallel analysis")
fit_pca <- principal(selected_stocks_ret, nfactors=6, rotate="varimax")
fit_pca
graphics.off()
#par("mar")
#par(mar=c(1,1,1,1))
#dev.off()
plot(fit_pca)
#biplot(fit_pca,expand=10)


#Obtaining principal component scoring coefficients
round(unclass(fit_pca$weights), 6)
#Principal component analysis of return using princomp
dev.off()
# Pricipal Components Analysis
# entering raw data and extracting PCs 
# from the correlation matrix 
fit_prin <- princomp(selected_stocks_ret)
barplot(height=fit_prin$sdev[1:10]/fit_prin$sdev[1])
load <- loadings(fit_prin)[,c(1:6)]
fit_prin$loadings[,c(1:6)]
pr.cp <- selected_stocks_ret %*% load
pr1 <- as.numeric(pr.cp[,1])
pr2 <- as.numeric(pr.cp[,2])
pr3 <- as.numeric(pr.cp[,3])
pr4 <- as.numeric(pr.cp[,4])
pr5 <- as.numeric(pr.cp[,5])
pr6 <- as.numeric(pr.cp[,6])
pr.num <- cbind(pr1,pr2,pr3,pr4,pr5,pr6)
summary(pr.num) 
plot(pr.num) # scree plot 
biplot(fit_prin)

#Starting clustering 
df_ret_melt <- melt(selected_stocks_ret ,  id.vars = 'days_data', variable.name = 'closing_prices')
adjus_close_price_melt <- melt(selected_adjust_cprice ,  id.vars = 'days_data', variable.name = 'closing_prices')
df_ret <- as.data.frame(t(df_ret_melt[,3]))
df_ret <- t(df_ret)
df_cp <- as.data.frame(t(adjus_close_price_melt$value))
df_cp <- t(df_cp)
df_ret_cp <- data.frame(df_ret[,1],df_cp[,1])
colnames(df_ret_cp) <- c('Return','Adjusted_CP')
wss <- (nrow(df_ret_cp)-1)*sum(apply(df_ret_cp,2,var))
for (i in 2:20) wss[i] <- sum(kmeans(df_ret_cp,centers=i)$withinss)
plot(1:20,wss,type="b",main="Number ofclusters",xlab="no. of cluster",ylab="with cluster sum of squares")
fit <- kmeans(df_ret_cp,5)
fit
fit$withinss
fit$betweenss
fit$size

library(cluster)
#clusplot(df_ret_cp, fit$cluster, color=TRUE, shade=TRUE, labels=2, lines=0)
library(fpc)
plotcluster(df_ret_cp,fit$cluster)
points(fit$centers,col=1:5,pch=5)

mydata <- data.frame(df_ret_cp,fit$cluster)
cluster_mean <- aggregate(df_ret_cp,by = list(fit$cluster),FUN = mean)
cluster_mean

# append cluster assignment
df_ret_cp <- data.frame(df_ret_cp, fit$cluster)
df_ret_cp
df_ret_cp$fit.cluster <- as.factor(df_ret_cp$fit.cluster)
library(ggplot2)
ggplot(df_ret_cp, aes(x=Adjusted_CP, y=Return, color = df_ret_cp$fit.cluster)) + geom_point()

selected_stocks_20<- sampleRandom(20)
selected_ret_stocks_20 <- R[,selected_stocks_20]
colnames(selected_ret_stocks_20)<- company_info[selected_stocks_20]
selected_adjust_cprice_20 <- AdjustedPrices[,selected_stocks_20]
colnames(selected_adjust_cprice_20)<- company_info[selected_stocks_20]

require(timeDate)

# A timeDate Sequence
date.sequence <- timeSequence(as.Date("2003-01-01"), as.Date("2008-01-01"));  
date.sequence;

# holidays in the period
years.included <- unique( as.integer( format( x=date.sequence, format="%Y" ) ) );
holidays <- holidayNYSE(years.included) 

# Subset business days
business.days <- date.sequence[isBizday(date.sequence, holidays)]; 
business.days

business.days_1258 <- business.days[1:1258]
business.days_1258

td = as.Date(business.days_1258@Data, format="%m/%d/%Y") 

head(td)

# require('xts')
# my.date = as.Date("2003/1/1") 
# adcprice_20 <- xts(selected_adjust_cprice_20,my.date+1:1258)
# View(adcprice_20)
# 
# ret_20 <- xts(selected_ret_stocks_20,my.date+1:1257)
# 

stockdata$info[selected_stocks_20,3]
# Analyzing US.Bancorp ,Blackrock
# Financial
USB.stock <- data.frame(date = td, stock.AdjCP = selected_adjust_cprice_20[,1])
# Financial
BLK.stock <- data.frame(date = td, stock.AdjCP = selected_adjust_cprice_20[,8]) 
# Telecommunication Services
VZ.stock <- data.frame(date = td, stock.AdjCP = selected_adjust_cprice_20[,17])
# Selected 20
selected_20.stock <-data.frame(date = td, stock.AdjCP = selected_adjust_cprice_20[,c(1:20)])
require('xts')
USB.xts <- xts(USB.stock[, -1], order.by=as.Date(USB.stock$date))
BLK.xts <- xts(BLK.stock[, -1], order.by=as.Date(BLK.stock$date))
VZ.xts <- xts(VZ.stock[, -1], order.by=as.Date(VZ.stock$date))
stock20.xts <- xts(selected_20.stock[, -1], order.by=as.Date(selected_20.stock$date))
colnames(USB.xts)<- c('USB.AdjClose')
colnames(BLK.xts)<- c('BLK.AdjClose')
colnames(VZ.xts)<- c('VZ.AdjClose')
colnames(stock20.xts)<- company_info[selected_stocks_20]
View(stock20.xts)
require('PerformanceAnalytics')

dev.off()
# 
# table.Stats(stock20.xts, ci = 0.95, digits = 4)
# table.Drawdowns(stock20.xts, top = 5, digits = 4)


mean_prices_usb <- round(mean(USB.xts), 2)
mean_prices_usb
sd_prices_usb <- round(sd(USB.xts), 2)
sd_prices_usb


# Plot the histogram along with a legend
hist(USB.xts, breaks = 100, prob=T, cex.main = 0.9)
abline(v = mean_prices_usb, lwd = 2)
legend("topright", cex = 0.8, bty = "n", paste("mean=", mean_prices_usb, "; sd=", sd_prices_usb))

################
# Stationarity #
################
# Compute log returns
returns.xts <- diff(log(USB.xts))

begin_dates <- c("2003-01-01", "2004-04-01",
                 "2005-04-01", "2006-04-01")
end_dates <- c("2004-03-31", "2005-03-31",
               "2006-03-31", "2007-12-31")

plot_ranges <- function(data, start_date, end_date, title){
  # Set the plot window to be 2 rows and 2 columns
  par(mfrow = c(2, 2))
  for(i in 1:4) {
    # Create a string with the appropriate date range
    range <- paste(start_date[i], "::", end_date[i], sep = "")
    # Create the price vector and necessary statistics
    time_series <- data[range]
    mean_data <- round(mean(time_series, na.rm = TRUE), 3)
    sd_data <- round(sd(time_series, na.rm = TRUE), 3)
    # Plot the histogram along with a legend
    hist_title <- paste(title, range)
    hist(time_series, breaks = 100, prob=TRUE, xlab = "", main = hist_title, cex.main = 0.8)
    abline(v = mean_data, lwd = 2)
    legend("topright", cex = 0.2, bty = "n", paste("mean=", mean_data, "; sd=", sd_data))
    }
    # Reset the plot window
  par(mfrow = c(1, 1))
}
# Use the same function as before to plot returns rather than prices
plot_ranges(returns.xts, begin_dates, end_dates, "USB log prices for:")

# Use the default settings
require(tseries)
test_usb_acp <- adf.test(as.numeric(USB.xts))
testbis_usb_acp<-kpss.test(USB.xts)
test_usb_acp
testbis_usb_acp

test_blk_acp <- adf.test(as.numeric(BLK.xts))
testbis_blk_acp<-kpss.test(BLK.xts)
test_blk_acp
testbis_blk_acp

test_vz_acp <- adf.test(as.numeric(VZ.xts))
testbis_vz_acp<-kpss.test(VZ.xts)
test_vz_acp
testbis_vz_acp


usb_returns <- diff(log(USB.xts))
usb_returns= usb_returns[-1,]
plot(usb_returns)

blk_returns <- diff(log(BLK.xts))
blk_returns= blk_returns[-1,]

vz_returns <- diff(log(VZ.xts))
vz_returns= vz_returns[-1,]


table.Stats(usb_returns, ci = 0.95, digits = 4)
table.Drawdowns(usb_returns, top = 5, digits = 4)
# The maximum draw down
head(drawdownsStats(as.timeSeries(usb_returns)),10)

 
table.Stats(blk_returns, ci = 0.95, digits = 4)
table.Drawdowns(blk_returns, top = 5, digits = 4)
 
plot(vz_returns)
table.Stats(vz_returns, ci = 0.95, digits = 4)
table.Drawdowns(vz_returns, top = 5, digits = 4)


# Test on the returns
test_returns_usb <- adf.test(as.numeric(usb_returns))
testbiss_usb_ret<-kpss.test(as.numeric(usb_returns))
test_returns_usb
testbiss_usb_ret

# Split the dataset in two parts - training and testing
breakpoint_usb = floor(nrow(usb_returns)*(2.9/3))

# Apply the ACF and PACF functions
par(mfrow = c(1,1))
acf.usb_returns = acf(usb_returns[c(1:breakpoint_usb),], main='ACF Plot', lag.max=100)
pacf.usb_returns = pacf(usb_returns[c(1:breakpoint_usb),], main='PACF Plot', lag.max=100)

# Initialzing an xts object for Actual log returns
Actual_series = xts(0,as.Date("2003-01-02","%Y-%m-%d"))

# Initialzing a dataframe for the forecasted return series
forecasted_series = data.frame(Forecasted = numeric())
library(forecast)
for (b in breakpoint_usb:(nrow(usb_returns)-1)) {
  
  usb_stock_train = usb_returns[1:b, ]
  usb_stock_test = usb_returns[(b+1):nrow(usb_returns), ]
  
  # Summary of the ARIMA model using the determined (p,d,q) parameters
  fit_usb = arima(usb_stock_train, order = c(1, 1, 1),include.mean=FALSE)
  summary(fit)
  
  # plotting a acf plot of the residuals
  acf(fit_usb$residuals,main="Residuals plot")
  
  # Forecasting the log returns
  usb_arima.forecast = forecast.Arima(fit_usb, h = 1,level=99)
  summary(usb_arima.forecast)
  
  # plotting the forecast
  par(mfrow=c(1,1))
  plot(usb_arima.forecast, main = "ARIMA Forecast")
  
  # Creating a series of forecasted returns for the forecasted period
  forecasted_series = rbind(forecasted_series,usb_arima.forecast$mean[1])
  colnames(forecasted_series) = c("Forecasted")
  
  # Creating a series of actual returns for the forecasted period
  Actual_return = usb_returns[(b+1),]
  Actual_series = c(Actual_series,xts(Actual_return))
  rm(Actual_return)
  
  print(USB.xts[(b+1),])
  print(USB.xts[(b+2),])
  
}

# Adjust the length of the Actual return series
Actual_series = Actual_series[-1]

# Create a time series object of the forecasted series
forecasted_series = xts(forecasted_series,index(Actual_series))

# Create a plot of the two return series - Actual versus Forecasted
plot(Actual_series,type='l',main='Actual Returns Vs Forecasted Returns')
lines(forecasted_series,lwd=1.5,col='red')
legend('bottomright',c("Actual","Forecasted"),lty=c(1,1),lwd=c(1.5,1.5),col=c('black','red'))

# Create a table for the accuracy of the forecast
comparsion = merge(Actual_series,forecasted_series)
comparsion$Accuracy = sign(comparsion$Actual_series)==sign(comparsion$Forecasted)
print(comparsion)

# Compute the accuracy percentage metric
Accuracy_percentage = sum(comparsion$Accuracy == 1)*100/length(comparsion$Accuracy)
print(Accuracy_percentage)


#obtain monthly data
USB.M  <- to.monthly(usb_returns)$usb_returns.Close
BLK.M  <- to.monthly(blk_returns)$blk_returns.Close
VZ.M  <- to.monthly(vz_returns)$vz_returns.Close

#Chk for na
any(is.na(USB.M))
any(is.na(BLK.M))
any(is.na(VZ.M))

#merge the three databases to get the same length
#inner : simplest way 
dataMonthly <- na.omit(merge(USB.M,BLK.M,VZ.M), join='inner')

dev.off()
chart.CumReturns(dataMonthly, legend.loc="topleft", wealth.index = TRUE, main= "Value of $1 invested from Jan '03 till Dec '07")
chart.Boxplot(dataMonthly) 
# charts.PerformanceSummary(dataMonthly,ylog=TRUE,
#                           main="Smart Money World's Greatest Investors???
#                           CHTTX with 200 Day Mean System",
#                           colorset=c("cadetblue","purple"))
#Fit a simple VAR model to the data#lag max =4 and AIC
require('vars')
var1 <- VAR(dataMonthly, lag.max=4, ic="AIC")
#more established model selection 
#varest class
VARselect(dataMonthly, lag.max=4)
summary(var1)
var1
#obtain the results
var1$varresult
var1$type
var1$p
var1$K
var1$obs
var1$totobs
var1$restrictions
var1$call
dev.off()
par(mar=c(3.82,1.82,1.82,0.82))
plot(var1) 		#Diagram of fit and residuals for each variables
coef(var1)		#concise summary of the estimated variables
residuals(var1)	#list of residuals (of the corresponding ~lm)
fitted(var1)	#list of fitted values
Phi(var1)		#coefficient matrices of VMA representation

#*****************************
#forecast based on a VAR model


#*****************************
#prediction can be done using the estimated VAR model
#with desired confidence interval
dev.off()
par(mar=c(3.82,1.82,1.82,0.82))
var.pred <- predict(var1, n.ahead=10, ci=0.95)
plot(var.pred)

qqnorm(as.numeric(usb_returns),
       main = "USB empirical returns qqplot()",
       cex.main = 0.8)
qqline(as.numeric(usb_returns),  lwd = 2)
grid()
answer_usb <- shapiro.test(as.numeric(USB.M))
answer_usb

dev.off()
usb <- USB.xts$USB.AdjClose
blk <- BLK.xts$BLK.AdjClose
price_matrix<-cbind(usb, blk)
returns_matrix <- apply(price_matrix, 2, function(x) diff(log(x)))
plot(returns_matrix)

sv <- as.xts(returns_matrix)

head(sv)
cor(sv)
outliers <- which(sv[, 2] > 1.0)

# If any outliers exist, remove them
if(length(outliers) > 0) {
  sv <- sv[-outliers, ]
}

cor(sv)
par(mfrow = c(1, 1))
acf(sv[, 1] ^ 2, main = "Actual returns squared",
    cex.main = 0.8, cex.lab = 0.8, cex.axis = 0.8)
grid()

par(mfrow = c(1, 2))
acf(sv[, 1]^3)
acf(abs(sv[, 1]))


archTest=function(rtn,m=10){
  # Perform Lagrange Multiplier Test for ARCH effect of a time series
  # rtn: time series
  # m: selected AR order
  #
  y=(rtn-mean(rtn))^2
  T=length(rtn)
  atsq=y[(m+1):T]
  x=matrix(0,(T-m),m)
  for (i in 1:m){
    x[,i]=y[(m+1-i):(T-i)]
  }
  md=lm(atsq~x)
  summary(md)
}

archTest(usb_returns)
archTest(blk_returns)
par(mfrow = c(1, 1))
acf(usb_returns)
par(mfrow = c(1, 2))
acf(usb_returns^2)
pacf(usb_returns^2)


install.packages('fGarch')
library(fGarch) # Load package`
usbGarch = garchFit(~arma(0, 2) + garch(1, 1), data=as.ts(tail(usb_returns, 500)))
predict(usbGarch, n.ahead=1, doplot=F)
plot(usbGarch)

#obtain monthly data
USB_cp.M  <- to.monthly(USB.xts)$USB.xts.Close
BLK_cp.M  <- to.monthly(BLK.xts)$BLK.xts.Close
VZ_cp.M  <- to.monthly(VZ.xts)$VZ.xts.Close


# 
# t.test(USB_cp.M)
# Box.test(USB_cp.M,lag=12,type='Ljung')
# par(mfcol=c(2,1)) 
# par(mar=c(3.82,1.82,1.82,0.82))
# acf(USB_cp.M,lag=24) 
# acf(abs(USB_cp.M),lag=24) 
# Box.test(abs(USB_cp.M),lag=12,type='Ljung')

dev.off()
m1=garchFit(~1+garch(3,0),data=BLK.M,trace=F) # Fit an ARCH(3) model 
summary(m1)
plot(m1)
m2=garchFit(~1+garch(1,0),data=BLK.M,trace=F) 
summary(m2)
resi=residuals(m2,standardize=T) 
par(mfcol=c(1,1))
par(mar=c(3.82,1.82,1.82,0.82))
acf(resi,lag=20) 
pacf(resi^2,lag=20) # Use fGarch built-in plots 
plot(m2)
m3=garchFit(~1+garch(1,0),data=BLK.M,trace=F,cond.dist="std") 
summary(m3)
plot(m3)
m4=garchFit(~1+garch(1,1),data=BLK.M,trace=F) 
summary(m4)
plot(m4)
v1=volatility(m4) #Obtainvolatility 
resi=residuals(m4,standardize=T) #Standardized residuals 
vol=ts(v1,frequency=12,start=c(2003,1)) 
res=ts(resi,frequency=12,start=c(2003,1)) 
par(mfcol=c(2,1)) #Showvolatilityandresiduals 
plot(vol,xlab='year',ylab='volatility',type='l') 
plot(res,xlab='year',ylab='st. resi',type='l') 
par(mfcol=c(2,2)) # Obtain ACF & PACF 
acf(resi,lag=24) 
pacf(resi,lag=24) 
acf(resi^2,lag=24) 
pacf(resi^2,lag=24)
m5=garchFit(~1+garch(1,1),data=BLK.M,trace=F,cond.dist="std") 
summary(m5)
plot(m5)
v2=volatility(m5) 
m6=garchFit(~1+garch(1,1),data=BLK.M,trace=F,cond.dist='sstd') 
summary(m6)
plot(m6)
v3=volatility(m6) 
cor(cbind(v1,v2,v3))

yt=BLK.M-mean(BLK.M) 
m1=arima(yt^2,order=c(1,0,1)) 
m1
fit_ytblk=yt^2-m1$residuals
cor(v3,sqrt(fit_ytblk))

##References for displaychart ,archtest,split adjustment are taken from books
##FinancialAnalytics in R and Class6.R in the classes



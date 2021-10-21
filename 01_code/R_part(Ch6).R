# Part I: simulate MA time series
y1=arima.sim(n=200,list(ma=0.5),innov = 0.5*rnorm(200))+2
plot.ts(y1)

y2=arima.sim(n=200,list(ma=c(-1,0.25)),innov = 0.5*rnorm(200))+2
plot.ts(y2)

par(mar=c(1,1,1,1))

layout(matrix(c(1,1,2,2), 2,2,byrow=TRUE))
# the first figure occupies the first row. 
# the first figure occupies the second row. 
# try the follow command to see the difference
# layout(matrix(c(1,1,1,2), 2,2,byrow=TRUE))


plot.ts(y1)
plot.ts(y2)

dev.off()
  

# Part II: estimation 

# load data: 5 year-treasure yield
library(readxl)
data <- read_excel("Figure6_5_Table6_1_treasury.xls")
dy<-ts(data$DY, frequency = 12, start = c(1953, 4))
plot(dy)


#install.packages('forecast')
library(forecast)
dysub<-data$DY[1:656]

dysub<-ts(dysub,frequency = 12, start = c(1953,4))
summary(dysub)
model<-arima(dysub,order=c(0,0,1))
summary(model)

# Part III:forecasting
fcasts<-forecast(model,h=5)
summary(fcasts)
plot(fcasts,include = 10)
lines(dy,col="red")


# 给定理论模型arima(3,1,2)，生成符合模型的样本数据
library(tseries)
set.seed(32)
n = 1000
phi = c(0.1,0.3,0.5)
theta = c(0.2,0.4)
e = ts(rnorm(1000))
x = numeric(n)
x[1] = 0
x[2] = 0
x[3] = 0
x[4] = 0
for(t in 5:1000){
    x[t]=(1+phi[1])*x[t-1]+(phi[2]-phi[1])*x[t-2]+(phi[3]-phi[2])*x[t-3]-phi[3]*x[t-4]+e[t]-theta[1]*e[t-1]-theta[2]*e[t-2]
}
ts.plot(x)

acf(x)
pacf(x)
library(zoo)
library(forecast)
auto.arima(x)
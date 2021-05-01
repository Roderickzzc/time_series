library(readxl)
Hualan_info <- read_excel("C:/Users/roderickzzc/Desktop/Hualan_info.xls")
data=na.omit(Hualan_info)
data <- as.data.frame(data)
price=data[,7]
change=diff(price)
ts.plot(price)acf(price)
pacf(price);
m=length(change)
T1=rep(NA,m)
filter=c(-3/320,-6/320,-5/320,3/320,21/320,46/320,67/320,74/320,67/320,46/320,21/320,3/320,-5/320,-
           6/320,-3/320)
radius=7; start=8; end=m-7
for (k in start:end){
  T1[k]=filter%*%price[(k-radius):(k+radius)]
}
ts.plot(T1,ylab="Trend")
IC=function(x,order.input=c(1,1,1)){
  fit=arima(x,order=order.input);
  n=length(x);p=order.input[1];q=order.input[3];sig=fit$ sigma2;
  FPE=sig*(n+p)/(n-p);AIC=fit$ aic
  BIC=(n-p-q)*log(n*sig/(n-p-1))+n*(1+log(sqrt(2*pi)))+(p+q)*log((sum(x^2)-n*sig)/(p+q)); 
  return(c(FPE,AIC,BIC)) }
IC(price,order=c(1,1,1))
n=length(change)
fit=arima(change,order=c(0,0,9))
arima(change,order=c(0,0,9),include.mean = F)
par(mfrow=c(2,1))
ts.plot(fit$ res)
r.z=as.numeric(acf(fit$ res,12)$ acf) ## h=12
portmanteau.stat=n*(n+2)*sum((r.z[-1]^2)/(n-(1:12)))
portmanteau.stat>qchisq(0.95,12-9)
tfore=rep(NA,134)
for (i in (1:104)){
  fit_i=arima(change[i:(i+28)],order=c(0,0,9))
  dfore=predict(fit_i)$pred[1]
  tfore[i+30]=price[i+29]+dfore
}
ts.plot(as.ts(price[31:134]),as.ts(tfore[31:134]),lty=c(1:2)) # 1-solid, 2-dashed, 3-dotted
leg.names=c("Actual","Forecast") # Draw legend
legend("topleft",leg.names,lty=c(1:2))
fore1=rep(NA,34)
dfore1=predict(fit_i,n.ahead=33)$pred[1:33]
fore1[1]=price[100]+dfore1[1]
for(i in (1:33)){
  fore1[(i+1)]=dfore1[(i+1)]+fore1[(i)]
}
ts.plot(as.ts(price[101:134]),as.ts(fore1),lty=c(1:2)) # 1-solid, 2-dashed, 3-dotted
leg.names=c("Actual","Forecast") # Draw legend
legend("topleft",leg.names,lty=c(1:2))library(forecast)
par(mfrow=c(2,1))
plot(forecast(fit))
fore=as.vector(forecast(fit))$mean;
lower=forecast(fit)$lower[,2];
upper=forecast(fit)$upper[,2];
a=as.vector(fore)+price[134]
b=as.vector(lower)+price[134]
c=as.vector(upper)+price[134]
ts.plot(c(price,a),ylim=c(28,55));
lines(c(price,b),lty=2);
lines(c(price,c),lty=2);
money=rep(NA,134)
money[1:30]=1
for (i in (1:100)){
  if (tfore[i+30]>price[i+29]){
    money[i+30]=money[i+29]/(price[i+29])*(price[i+30])
  }else{money[i+30]=money[i+29]}
}
money_hold=rep(NA,134)
money_hold[1:30]=1
for (i in (1:100)){
  money_hold[i+30]=money_hold[i+29]/(price[i+29])*(price[i+30])
}
par(mfrow=c(1,1))
ts.plot(as.ts(money[1:130]),as.ts(money_hold[1:130]),lty=c(1:2)) # 1-solid, 2-dashed, 3-dotted
leg.names=c("Using Strategy","Just holding") # Draw legend
legend("topleft",leg.names,lty=c(1:2))
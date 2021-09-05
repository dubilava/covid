# load the libraries (install them if needed)
library(data.table)
library(ggplot2)
library(stringr)
library(minpack.lm)

# cleaning up the environment (probably not needed, but better safe than sorry)
rm(list=ls())
gc()

# read in the COVID Australia data, downloaded from https://www.covidaustralia.com/cases
dt <- fread("covid_australia.csv")

# pre-define the string of months to fix the date format in the original data
mmm <- c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")

# mess around with the date column to get it sorted in the right format
dt[,`:=`(mo=str_pad(match(substr(mody,1,3),month.abb),2,pad="0"),dy=str_pad(str_trim(as.character(substr(dt$mody,5,6))),2,pad="0"))]
dt[,`:=`(date=as.Date(paste(yr,mo,dy,sep="-")))]

# store the numbers as numbers
dt[,`:=`(vic=as.numeric(vic),nsw=as.numeric(nsw),qld=as.numeric(qld),wa=as.numeric(wa),sa=as.numeric(sa),tas=as.numeric(tas),act=as.numeric(act),nt=as.numeric(nt),total=as.numeric(total))]

# toss some unused columns -- keep the ones we (may) need
covid_dt <- dt[date>=as.Date("2021-06-06"),.(date,total,nsw,vic,qld,wa,sa,tas,act,nt)]
covid_dt <- covid_dt[order(date)]

# add trend and weekdays
covid_dt[,`:=`(trend=as.numeric(as.factor(date)),wday=weekdays(date,abbreviate=T))]

# order the weekday factors
covid_dt$wday <- factor(covid_dt$wday,levels=c("Mon","Tue","Wed","Thu","Fri","Sat","Sun"))

# main model: logistic smooth transition regression of the form: y_t = a + b G(t;g,c)
# where G(t;g,c)=(1+exp[-g(t-c)/sd(t)])^{-1} is the logistic transition function, 
# and where g and c are parameters of this logistic transition function.

# fit the restricted logistic smooth transition model (we restrict a=0)

# obtain approximate starting values (usually quite important)
covid_dt$G <- as.numeric((1+exp(-(2/sd(covid_dt$trend))*(covid_dt$trend-max(covid_dt$trend))))^(-1))

theta <- as.numeric(c(lm(nsw~G-1,data=covid_dt)$coef,2,max(covid_dt$trend)))
theta <- 0.9*theta+rnorm(length(theta),0,0.01)
names(theta) <- c("b","g","c")

# fit the nonlinear least squares
reg_str <- nlsLM(nsw~b*((1+exp(-g/sd(trend)*(trend-c)))^(-1)),data=covid_dt,start=theta,lower=c(-Inf,.1,-Inf),upper=c(Inf,100,Inf),control=nls.control(maxiter=1000,warnOnly=T))

# the estimated logistic function
Ghat <- ((1+exp(-coefficients(reg_str)["g"]/(sd(covid_dt$trend))*(covid_dt$trend-coefficients(reg_str)["c"])))^(-1))

# store the fitted values into the dataset
covid_dt$yhat_str <- coefficients(reg_str)["b"]*Ghat

# generate bootstrap parameters 
B <- 1000
boot_mat <- matrix(NA,B,length(coefficients(reg_str))) 
for(i in 1:B){
  set.seed(i)
  eboot_str <- sample(residuals(reg_str),length(residuals(reg_str)),replace=T)
  covid_dt$yboot_str <- round(covid_dt$yhat_str+residuals(reg_str)*sample(c(-1,1),length(residuals(reg_str)),replace=T))
  boot_str <- nlsLM(yboot_str~b*((1+exp(-g/sd(trend)*(trend-c)))^(-1)),data=covid_dt,start=theta,lower=c(-Inf,.1,-Inf),upper=c(Inf,100,Inf),control=nls.control(maxiter=1000,warnOnly=T))
  boot_mat[i,] <- coefficients(boot_str)
}

colnames(boot_mat) <- names(coefficients(boot_str))

# multi-day forecast horizon
h <- 7

trend_f <- c(as.numeric(max(covid_dt$trend)+1):as.numeric(max(covid_dt$trend)+h))

# point forecast
Ghat_f <- ((1+exp(-coefficients(reg_str)["g"]/(sd(covid_dt$trend))*(trend_f-coefficients(reg_str)["c"])))^(-1))

ystr_f <- coefficients(reg_str)["b"]*Ghat_f

# bootstrap forecasts
ystr_b <- matrix(NA,length(ystr_f),B)
for(i in 1:B){
  Ghat_b <- ((1+exp(-boot_mat[i,"g"]/(sd(covid_dt$trend))*(trend_f-boot_mat[i,"c"])))^(-1))
  
  ystr_b[,i] <- boot_mat[i,"b"]*Ghat_b
}

ystr_ci <- as.data.table(t(apply(ystr_b,1,quantile,c(.025,.975))))
colnames(ystr_ci) <- c("lower","upper")

# arrange the dataset to bind together the fitted and forecast values along with the observed data
nsw_dt <- covid_dt[,.(date,observed=nsw,forecast=yhat_str)]
nsw_dt[,`:=`(lower=NA,upper=NA)]
nsw_fc <- data.table(date=seq(covid_dt[date==max(date)]$date+1,by="day",length.out=h),observed=NA,forecast=ystr_f,lower=ystr_ci$lower,upper=ystr_ci$upper)

# bind the in-sample and out-of-sample segments
nsw_cb <- rbind(nsw_dt,nsw_fc) 

# melt to 'long table' for plotting purposes
nsw_lg <- melt(nsw_cb,id.vars=c("date","lower","upper"))

nsw_lg$variable <- factor(nsw_lg$variable,levels=c("observed","forecast"))

# plot the graph
gg_plot <- ggplot(nsw_lg,aes(x=date,y=value))+
  geom_ribbon(aes(ymin=lower,ymax=upper),fill="steelblue",alpha=.25)+
  geom_line(aes(color=variable,linetype=variable),size=.5,na.rm=T)+
  scale_color_manual(values=c("darkgray","steelblue"))+
  scale_linetype_manual(values=c(1,5,2,2))+
  labs(title="COVID NSW",subtitle=paste("Forecast made on",format(as.Date(max(covid_dt$date)),"%d %b %Y"),"for the subsequent seven days"),x="Date",y="Daily Cases",caption="Source of the data: https://www.covidaustralia.com/cases")+
  theme_classic()+
  theme(axis.title=element_text(size=12),axis.text=element_text(size=10),legend.title=element_blank(),legend.text=element_text(size=8),legend.position=c(.025,1),legend.justification=c(0,1),plot.caption = element_text(color="darkgray",size=9),plot.subtitle=element_text(size=9),legend.key.size=unit(0.5,"cm"))

# save the figure
ggsave("covid_star.png",dpi="retina",width=6.5,height=3.5)


# print the rounded case numbers for the forecast period
forecast_dt <- nsw_fc[,.(date,forecast)]
forecast_dt$weekday <- weekdays(forecast_dt$date,abbreviate=T)

forecast_dt <- forecast_dt[,.(weekday,date=format(date,"%d %b"),forecast=round(forecast))]


# an alternative model: exponential trend: y_t = exp(a+b*t)
reg_exp <- nlsLM(nsw~exp(a+trend*b),data=covid_dt,start=list(a=1,b=.1),control=nls.control(maxiter=1000,warnOnly=T))
covid_dt$yhat_exp <- c(fitted(reg_exp))

# seven-day forecast
trend_f <- c(as.numeric(max(covid_dt$trend)+1):as.numeric(max(covid_dt$trend)+h))
yexp_f <- exp(coef(reg_exp)["a"]+trend_f*coef(reg_exp)["b"])

# arrange the dataset to bind together the fitted and forecast values along with the observed data
nsw_dt <- covid_dt[,.(date,observed=nsw,logistic=yhat_str,exponential=yhat_exp)]
nsw_fc <- data.table(date=seq(covid_dt[date==max(date)]$date+1,by="day",length.out=7),observed=NA,logistic=ystr_f,exponential=yexp_f)

nsw_cb <- rbind(nsw_dt,nsw_fc) 

# melt to 'long table' for plotting purposes
nsw_lg <- melt(nsw_cb,id.vars="date")

# plot the graph
gg_plot <- ggplot(nsw_lg,aes(x=date,y=value,color=variable,linetype=variable))+
  geom_line(size=.5,na.rm=T)+
  scale_color_manual(values=c("darkgray","steelblue","indianred"))+
  scale_linetype_manual(values=c(1,5,2))+
  labs(title="COVID NSW",subtitle=paste("Forecast made on",format(as.Date(max(covid_dt$date)),"%d %b %Y"),"for the subsequent seven days"),x="Date",y="Daily Cases",caption="Source of the data: https://www.covidaustralia.com/cases")+
  theme_classic()+
  theme(axis.title=element_text(size=12),axis.text=element_text(size=10),legend.title=element_blank(),legend.text=element_text(size=8),legend.position=c(.025,1),legend.justification=c(0,1),plot.caption = element_text(color="darkgray",size=9),plot.subtitle=element_text(size=9),legend.key.size=unit(0.5,"cm"))

# save the figure
ggsave("covid_models.png",dpi="retina",width=6.5,height=3.5)


# # a pseudo-forecasting exercise to check how the model has been performing in previous several days
# P <- 7
# 
# end_dates <- covid_dt$date[(length(covid_dt$date)-P-6):(length(covid_dt$date)-P)]
# 
# errors_mat <- matrix(NA,nrow=length(end_dates),ncol=h)
# 
# for(i in 1:P){
#   
#   # logistic smooth transition trend
#   window_dt <- covid_dt[date<=as.Date(end_dates[i])]
#   
#   window_dt$G <- as.numeric((1+exp(-(2/sd(window_dt$trend))*(window_dt$trend-max(window_dt$trend))))^(-1))
#   
#   theta <- as.numeric(c(lm(nsw~G-1,data=window_dt)$coef,2,max(window_dt$trend)))
#   theta <- 0.9*theta+rnorm(length(theta),0,0.01)
#   names(theta) <- c("b","g","c")
#   
#   # fit the nonlinear least squares
#   reg_str <- nlsLM(nsw~b*((1+exp(-g/sd(trend)*(trend-c)))^(-1)),data=window_dt,start=theta,lower=c(-Inf,.1,-Inf),upper=c(Inf,100,Inf),control=nls.control(maxiter=1000,warnOnly=T))
#   
#   
#   # multi-day forecast
#   trend_f <- c(as.numeric(max(window_dt$trend)+1):as.numeric(max(window_dt$trend)+h))
#   
#   Ghat_f <- ((1+exp(-coefficients(reg_str)["g"]/(sd(window_dt$trend))*(trend_f-coefficients(reg_str)["c"])))^(-1))
#   
#   ystr_f <- round(coefficients(reg_str)["b"]*Ghat_f)
#   ystr_a <- covid_dt[date>as.Date(end_dates[i]) & date<=as.Date(end_dates[i])+h]$nsw
#   
#   errors_mat[i,] <- ystr_a-ystr_f
#   
# }
# 
# 
# errors_dt <- as.data.table(abs(errors_mat))
# colnames(errors_dt) <- c("one","two","three","four","five","six","seven")
# errors_dt$date <- end_dates
# 
# errors_lg <- melt(errors_dt[,.(date,one_day_ahead=one,seven_days_ahead=seven)],id.vars="date")
# 
# gg_errors <- ggplot(errors_lg,aes(x=date,y=value,color=variable,linetype=variable))+
#   geom_line(size=.6,na.rm=T)+
#   scale_color_manual(values=c("darkgray","steelblue"))+
#   scale_linetype_manual(values=c(1,5))+
#   coord_cartesian(ylim=c(0,max(errors_lg$value)*1.5))+
#   labs(title="COVID NSW",subtitle=paste("Forecasts made between",format(as.Date(end_dates[1]),"%d %b %Y"),"and",format(as.Date(end_dates[7]),"%d %b %Y")),x="Date",y="Absolute Forecast Error (Days)",caption="Source of the data: https://www.covidaustralia.com/cases")+
#   theme_classic()+
#   theme(axis.title=element_text(size=12),axis.text=element_text(size=10),legend.title=element_blank(),legend.text=element_text(size=9),legend.position=c(.025,1),legend.justification=c(0,1),plot.caption = element_text(color="darkgray",size=9),plot.subtitle=element_text(size=9))
# 
# # save the figure
# ggsave("covid_errors.png",dpi="retina",width=6.5,height=3.5)

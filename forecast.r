# load the libraries (install them if needed)
library(data.table)
library(ggplot2)
library(stringr)
library(fastDummies)
library(minpack.lm)

# cleaning up the environment (probably not needed, but better safe than sorry)
rm(list=ls())
gc()

# load the LSTAR function (to fit the smooth transition type of nonlinear trend in the series)
source("lstar.r")
source("estar.r")

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

# # create the weekday dummies
# covid_dt <- dummy_cols(covid_dt,select_columns = "wday")

# main model: logistic smooth transition regression of the form: y_t = a + b G(t;g,c)
# where G(t;g,c)=(1+exp[-g(t-c)/sd(t)])^{-1} is the logistic ytransition function, 
# and where g and c are parameters of this logistic transition function.

# dependent and independent variables
y <- covid_dt$nsw
X <- as.matrix(rep(1,nrow(covid_dt)))

# fit the lstar model (using the 'lstar' function available separately)
reg_str <- lstar(y=y,x.0=X,x.1=X,tv=covid_dt$trend)

# the estimated logistic function
Ghat <- ((1+exp(-reg_str$coef["g1"]/(sd(covid_dt$trend))*(covid_dt$trend-reg_str$coef["c1"])))^(-1))

# store the fitted values into the dataset
covid_dt$yhat_str <- reg_str$coef["a1"]+reg_str$coef["b1"]*Ghat

# multi-day forecast horizon
h <- 7

trend_f <- c(as.numeric(max(covid_dt$trend)+1):as.numeric(max(covid_dt$trend)+h))

Ghat_f <- ((1+exp(-reg_str$coef["g1"]/(sd(covid_dt$trend))*(trend_f-reg_str$coef["c1"])))^(-1))

ystr_f <- reg_str$coef["a1"]+reg_str$coef["b1"]*Ghat_f

# arrange the dataset to bind together the fitted and forecast values along with the observed data
nsw_dt <- covid_dt[,.(date,observed=nsw,forecast=yhat_str)]
nsw_fc <- data.table(date=seq(covid_dt[date==max(date)]$date+1,by="day",length.out=h),observed=NA,forecast=ystr_f)

# bidn the in-sample and out-of-sample segments
nsw_cb <- rbind(nsw_dt,nsw_fc) 

# melt to 'long table' for plotting purposes
nsw_lg <- melt(nsw_cb,id.vars="date")

# plot the graph
gg_plot <- ggplot(nsw_lg,aes(x=date,y=value,color=variable,linetype=variable))+
  geom_line(size=.6,na.rm=T)+
  scale_color_manual(values=c("darkgray","goldenrod"))+
  scale_linetype_manual(values=c(1,5))+
  labs(title="COVID NSW",subtitle=paste("Forecast made on",format(as.Date(max(covid_dt$date)),"%d %b %Y"),"for the subsequent seven days"),x="Date",y="Daily Cases",caption="Source of the data: https://www.covidaustralia.com/cases")+
  theme_classic()+
  theme(axis.title=element_text(size=12),axis.text=element_text(size=10),legend.title=element_blank(),legend.text=element_text(size=9),legend.position=c(.025,1),legend.justification=c(0,1),plot.caption = element_text(color="darkgray",size=9),plot.subtitle=element_text(size=9))

# save the figure
ggsave("covid_star.png",dpi="retina",width=6.5,height=3.5)

# print the rounded case numbers for the forecast period
forecast_dt <- nsw_fc[,.(date,forecast)]
forecast_dt$weekday <- weekdays(forecast_dt$date,abbreviate=T)

forecast_dt <- forecast_dt[,.(weekday,date=format(date,"%d %b"),forecast=round(forecast))]


# alternative models: power trend: y_t = a*t^b, and exponential trend: y_t = exp(a+b*t)

# power trend
reg_pow <- nlsLM(nsw~a*trend^b,data=covid_dt,start=list(a=1,b=.1),control=nls.control(maxiter=1000,warnOnly=T))

# store the fitted values into the dataset
covid_dt$yhat_pow <- c(fitted(reg_pow))

# seven-day forecast
trend_f <- c(as.numeric(max(covid_dt$trend)+1):as.numeric(max(covid_dt$trend)+h))

ypow_f <- coef(reg_pow)["a"]*trend_f^coef(reg_pow)["b"]


# exponential trend
reg_exp <- nlsLM(nsw~exp(a+trend*b),data=covid_dt,start=list(a=1,b=.1),control=nls.control(maxiter=1000,warnOnly=T))

# store the fitted values into the dataset
covid_dt$yhat_exp <- c(fitted(reg_exp))

# seven-day forecast
yexp_f <- exp(coef(reg_exp)["a"]+trend_f*coef(reg_exp)["b"])


# arrange the dataset to bind together the fitted and forecast values along with the observed data
nsw_dt <- covid_dt[,.(date,observed=nsw,power=yhat_pow,exponential=yhat_exp,logistic=yhat_str)]
nsw_fc <- data.table(date=seq(covid_dt[date==max(date)]$date+1,by="day",length.out=7),observed=NA,power=ypow_f,exponential=yexp_f,logistic=ystr_f)

nsw_cb <- rbind(nsw_dt,nsw_fc) 

# melt to 'long table' for plotting purposes
nsw_lg <- melt(nsw_cb,id.vars="date")

# plot the graph
gg_plot <- ggplot(nsw_lg,aes(x=date,y=value,color=variable,linetype=variable))+
  geom_line(size=.6,na.rm=T)+
  scale_color_manual(values=c("darkgray","goldenrod","indianred","steelblue"))+
  scale_linetype_manual(values=c(1,5,3,2))+
  labs(title="COVID NSW",subtitle=paste("Forecast made on",format(as.Date(max(covid_dt$date)),"%d %b %Y"),"for the subsequent seven days"),x="Date",y="Daily Cases",caption="Source of the data: https://www.covidaustralia.com/cases")+
  theme_classic()+
  theme(axis.title=element_text(size=12),axis.text=element_text(size=10),legend.title=element_blank(),legend.text=element_text(size=9),legend.position=c(.025,1),legend.justification=c(0,1),plot.caption = element_text(color="darkgray",size=9),plot.subtitle=element_text(size=9))

# save the figure
ggsave("covid_models.png",dpi="retina",width=6.5,height=3.5)


# a pseudo-forecasting exercise to check how the model has been performing in previous several days
P <- 7

end_dates <- covid_dt$date[(length(covid_dt$date)-P-6):(length(covid_dt$date)-P)]

errors_mat <- matrix(NA,nrow=length(end_dates),ncol=h)

for(i in 1:P){
  
  # logistic smooth transition trend
  window_dt <- covid_dt[date<=as.Date(end_dates[i])]
  y <- window_dt$nsw
  X <- as.matrix(rep(1,nrow(window_dt)))
  
  reg_str <- lstar(y=y,x.0=X,x.1=X,tv=window_dt$trend)
  
  # multi-day forecast
  trend_f <- c(as.numeric(max(window_dt$trend)+1):as.numeric(max(window_dt$trend)+h))
  
  Ghat_f <- ((1+exp(-reg_str$coef["g1"]/(sd(window_dt$trend))*(trend_f-reg_str$coef["c1"])))^(-1))
  
  ystr_f <- round(reg_str$coef["a1"]+reg_str$coef["b1"]*Ghat_f)
  ystr_a <- covid_dt[date>as.Date(end_dates[i]) & date<=as.Date(end_dates[i])+h]$nsw
  
  errors_mat[i,] <- ystr_a-ystr_f
  
}


errors_dt <- as.data.table(abs(errors_mat))
colnames(errors_dt) <- c("one","two","three","four","five","six","seven")
errors_dt$date <- end_dates

errors_lg <- melt(errors_dt[,.(date,one_day_ahead=one,seven_days_ahead=seven)],id.vars="date")

gg_errors <- ggplot(errors_lg,aes(x=date,y=value,color=variable,linetype=variable))+
  geom_line(size=.6,na.rm=T)+
  scale_color_manual(values=c("darkgray","goldenrod"))+
  scale_linetype_manual(values=c(1,5))+
  coord_cartesian(ylim=c(0,max(errors_lg$value)*1.5))+
  labs(title="COVID NSW",subtitle=paste("Forecasts made between",format(as.Date(end_dates[1]),"%d %b %Y"),"and",format(as.Date(end_dates[7]),"%d %b %Y")),x="Date",y="Absolute Forecast Error (Days)",caption="Source of the data: https://www.covidaustralia.com/cases")+
  theme_classic()+
  theme(axis.title=element_text(size=12),axis.text=element_text(size=10),legend.title=element_blank(),legend.text=element_text(size=9),legend.position=c(.025,1),legend.justification=c(0,1),plot.caption = element_text(color="darkgray",size=9),plot.subtitle=element_text(size=9))

# save the figure
ggsave("covid_errors.png",dpi="retina",width=6.5,height=3.5)






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

# read in the COVID Australia data, downloaded from https://www.covidaustralia.com/cases
dt <- fread("covid_australia.csv")

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

# create the weekday dummies
covid_dt <- dummy_cols(covid_dt,select_columns = "wday")

# exponential trend
reg_exp <- nlsLM(nsw~a*trend^b,data=covid_dt,start=list(a=1,b=.1),control=nls.control(maxiter=1000,warnOnly=T))

# store the fitted values into the dataset
covid_dt$yhat_exp <- c(fitted(reg_exp))

# seven-day forecast
trend_f <- c(as.numeric(max(covid_dt$trend)+1):as.numeric(max(covid_dt$trend)+7))

yexp_f <- coef(reg_exp)["a"]*trend_f^coef(reg_exp)["b"]


# logistic smooth transition trend
y <- covid_dt$nsw
X <- as.matrix(rep(1,nrow(covid_dt)))

reg_str <- lstar(y=y,x.0=X,x.1=X,tv=covid_dt$trend,crit=.1)

# the estimated logistic function
Ghat <- ((1+exp(-reg_str$coef["g1"]/(sd(covid_dt$trend))*(covid_dt$trend-reg_str$coef["c1"])))^(-1))

# store the fitted values into the dataset
covid_dt$yhat_str <- reg_str$coef["a1"]+reg_str$coef["b1"]*Ghat

# seven-day forecast
trend_f <- c(as.numeric(max(covid_dt$trend)+1):as.numeric(max(covid_dt$trend)+7))
Ghat_f <- ((1+exp(-reg_str$coef["g1"]/(sd(covid_dt$trend))*(trend_f-reg_str$coef["c1"])))^(-1))

ystr_f <- reg_str$coef["a1"]+reg_str$coef["b1"]*Ghat_f

# arrange the dataset to bind together the fitted and forecast values along with the observed data
nsw_dt <- covid_dt[,.(date,observed=nsw,exponential=yhat_exp,logistic=yhat_str)]
nsw_fc <- data.table(date=seq(covid_dt[date==max(date)]$date+1,by="day",length.out=7),observed=NA,exponential=yexp_f,logistic=ystr_f)

nsw_cb <- rbind(nsw_dt,nsw_fc) 

# melt to 'long table' for plotting purposes
nsw_lg <- melt(nsw_cb,id.vars="date")

# plot the graph
gg_plot <- ggplot(nsw_lg,aes(x=date,y=value,color=variable,linetype=variable))+
  geom_line(size=.8,na.rm=T)+
  scale_color_manual(values=c("darkgray","indianred","steelblue"))+
  scale_linetype_manual(values=c(1,2,5))+
  labs(title="COVID NSW",subtitle=paste("Forecast made on",format(as.Date(max(covid_dt$date)),"%d %b %Y"),"for the subsequent seven days"),x="Date",y="Daily Cases",caption="Source of the data: https://www.covidaustralia.com/cases")+
  theme_classic()+
  theme(axis.title = element_text(size=13),axis.text = element_text(size=11),legend.title = element_blank(), legend.position = c(.15,.85), legend.text = element_text(size=12),plot.caption = element_text(color="darkgray"))

# save the figure
ggsave("covid_nsw.png",dpi="retina",width=6.5,height=4.5)

# seven-day forecast values
forecast_dt <- nsw_fc[,.(date,exponential,logistic)]
forecast_dt$weekday <- weekdays(forecast_dt$date,abbreviate=T)

forecast_dt <- forecast_dt[,.(weekday,date,exponential=round(exponential),logistic=round(logistic))]

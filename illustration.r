library(data.table)
library(ggplot2)

a=0
b1=3200
b2=-2500
g1=5
c1=350
g2=7
c2=670

trend <- 1:1000
G1 <- (1+exp(-g1*(trend-c1)/sd(trend)))^{-1}
G2 <- (1+exp(-g2*(trend-c2)/sd(trend)))^{-1}

y <- a+b1*G1+b2*G2

dt <- data.table(t=trend,y=y)

gg_plot <- ggplot(dt,aes(x=t,y=y))+
  geom_line(size=1,color="steelblue")+
  coord_cartesian(ylim=c(0,3200))+
  theme_classic()+
  theme(axis.title = element_text(size=12),axis.text = element_text(size=10))

ggsave("illustration.png",gg_plot,device="png",dpi="retina",width=6.5,height=3.5)

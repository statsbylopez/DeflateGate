library(RCurl)
x<-getURL('https://raw.githubusercontent.com/octonion/football/master/deflategate/psi.csv')
y <- read.csv(text = x)

y$ID<-c(1:11,1:11,12:15,12:15)


#Scatter plot of PSI readings
par(mar=c(4,5,2,2))
plot(y$ID,y$halftime_psi,
     col="white",xaxt='n',
     xlab="Ball ID",ylab="PSI",ylim=c(10.5,13.3),cex.lab=2,
     cex.axis=1.4,cex.main=1.6,
     main="Halftime PSI values by gauge",xlim=c(0.5,15.5))
text(y$ID,y$halftime_psi,y$gauge,col=c(rep("red",22),rep("blue",8)),cex=1.3)
axis(1,at=1:15,y[12:26,]$ball,cex.axis=1.3)
lines(c(1,11),c(12.5,12.5),col="red",lty=2)
lines(c(12,15),c(13,13),col="blue",lty=2)
text(6,12.6,"Pats pre-game",col="red",cex=1.5)
text(13.5,13.1,"Colts pre-game",col="blue",cex=1.5)
theta <- seq(0, 2 * pi, length=(10000))
x <- 14 + 0.5 * cos(theta)
y1 <- 12.71 + 0.32 * sin(theta)
points(x, y1, type = "l")




model0 <- delta ~ gauge + (1|ball)
model1 <- delta ~ team + gauge + (1|ball)
model2 <- delta ~ team + gauge + team:gauge + (1|ball)



#Residuals plot
fit1 <- lmer(model1, data=y)
fit1
pvalue<-2*pt(summary(fit1)$coeff[2,3],13)
pvalue

plot(fitted(fit1),resid(fit1),pch=16,xlab="Fitted values",cex.lab=1.5,
     cex.main=1.5,cex.axis=1.5,cex.main=1.5,
     ylab="Residuals",main="Residuals vs. Fitted values, Wells Report")
fit1$fitted[,1]
summary(fit1)



#Plot of pvalue sensitivity
diff<-seq(-0.05,0.25,by=0.01)
pvalues1<-NULL
pvalues2<-NULL
pvalues3<-NULL
for (i in 1:length(diff)){
  y.2<-y
  y.2[y$team=="Colts",]$delta<-y.2[y$team=="Colts",]$delta-diff[i]
  fit1 <- lmer(model1, data=y.2)
  fit2 <- lmer(model1, data=filter(y.2,ball!="C3"))
  pvalues1[i]<-2*pt(summary(fit1)$coeff[2,3],13)
  pvalues2[i]<-2*pt(summary(fit2)$coeff[2,3],13)
  pvalues3[i]<-wilcox.test(delta ~ team, data=y.2)$p.value
}
pvalues1
pvalues2
pvalues3
library(ggplot2)
p<-ggplot() +   geom_line(aes(y=1/pvalues1,x=diff),col="red")+
  geom_line(aes(y=1/pvalues2,x=diff),col="black")+
  theme_bw(base_size = 20)+ theme(legend.position = "none") +
  scale_x_continuous("Colts pre-game value",breaks=c(0,0.1,0.2,0.3), 
                     labels=c("13.0", "13.1", "13.2","13.3"))+
  scale_y_continuous("",breaks=c(20,100,200,300), 
                     labels=c("1 in 20", "1 in 100", "1 in 200","1 in 300"))+
  geom_abline(intercept = 20,slope=0,lty=2)+ggtitle("Probability of deflation differences")+
  annotate("text", label = "With C3", x = .135, y = 100, size = 6, colour = "red")+
  annotate("text", label = "Without C3", x = -.00, y = 100, size = 6, colour = "black")+
  annotate("text", label = "Wells Report value", x = .0501, y = 243, size = 6, colour = "dark red")+
  geom_point(aes(x=0,y=240),size=5,col="dark red") 
p




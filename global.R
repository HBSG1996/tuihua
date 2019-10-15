library(lme4)

data<-read.csv("C:\\Users\\HECTOR\\Documents\\Learning_data\\Software_learning\\R_work\\tuihua\\C80chelun1.csv", header=T)

Wheel<-as.numeric(as.character(data$Series))*as.numeric(as.character(data$Car))*as.numeric(as.character(data$Position))
data$Position<-as.factor(data$Position)
data$Axle<-as.factor(data$Axle)
data$Car<-as.factor(data$Car)
data$Series<-as.factor(data$Series)
data$Wheel<-as.factor(Wheel)

data$Mileage<-data$Mileage*1e-5
data$tamian[data$tamian<=0]<-NA
data$tamian[data$tamian>=10]<-NA

m6.glmer<-glmer(tamian~Mileage+(1+Mileage|Series:Car:Position),data,family=Gamma(link = "identity"))

library(nlme)

m6<-gls(tamian~Mileage,data = data,na.action=na.omit,correlation = corAR1(form = ~1|Car),weights = varExp())

data$y0<-predict(m6,data)

for(i in 1:length(data$tamian)){
  if (is.na (data$tamian[i]))
    data$tamian[i]<-data$y0[i]
}

data$Mileage<-data$Mileage+1e-10

glm.gamma<-glm(tamian~Mileage,data = data,family = Gamma(link = "identity"))

m<-glm.gamma

newdat<-data.frame(Mileage=seq(0,15,length=30))
mm<-model.matrix(~Mileage,newdat)
newdat$y<-mm%*%m$coefficients
pvar1 <- diag(mm %*% tcrossprod(vcov(m),mm))
tvar1 <- pvar1+summary(m)$dispersion*(newdat$y)*(newdat$y)
newdat <- data.frame(
  newdat
  , plo = newdat$y-qnorm(.975)*sqrt(pvar1)
  , phi = newdat$y+qnorm(.975)*sqrt(pvar1)
  , lo80 = newdat$y-qnorm(.9)*sqrt(tvar1)
  , hi80 = newdat$y+qnorm(.9)*sqrt(tvar1)
  , lo60 = newdat$y-qnorm(.8)*sqrt(tvar1)
  , hi60 = newdat$y+qnorm(.8)*sqrt(tvar1)
  , lo40 = newdat$y-qnorm(.7)*sqrt(tvar1)
  , hi40 = newdat$y+qnorm(.7)*sqrt(tvar1)
)


library(shiny)

server<-function(input,output){
  output$tu<-renderPlot({
    library(showtext)
    showtext.begin()# 开始使用showtext
    tit<-"C80试验列踏面磨耗量"        
    yl<-"踏面磨耗量/毫米"               
    xl<-"行走里程/万公里"    
    data1<-subset(data,data$Time==0)
    data1$Mileage<-as.numeric(input$ys)/10
    data1$tamian<-NA
    data1$y0<-NA
    data1$zhijing<-NA
    
    data <- rbind(data1, data[1:nrow(data),])
    
    data$dummy<-is.na (data$tamian)
    data$dummy1[data$dummy=="FALSE"]<-"建模数据"
    data$dummy1[data$dummy=="TRUE"]<-"外推数据"
    
    data$y1<-predict(m6.glmer,data)
    
    
    for(i in 1:length(data$tamian)){
      if (is.na (data$tamian[i]))
        data$tamian[i]<-data$y1[i]
      if (i<=length(data1$tamian))
        data1$tamian[i]<-data$y1[i]
    }
    
    newdat$Mileage<-newdat$Mileage*10
    data$Mileage<-10*data$Mileage
    data1$Mileage<-10*data1$Mileage
    plot(tamian~Mileage,data,xlim=c(0,input$xlim),ylim=c(0,input$ylim),xlab="",ylab="")
    points(tamian~Mileage,data1,col="red")
    legend("topleft",legend=c("建模数据","外推数据"),col=c("black","red"),pch=c(1, 1),bty="n",cex=2)
    axis(1,at=seq(0,151,10))
    axis(2,at=seq(0,20,1))
    title(main=tit, ylab=yl, xlab=xl,cex.axis=1,cex.lab=2,cex.main=3)
    lines(newdat$Mileage,newdat$y,col="black",lty=1,lwd=2)
    lines(newdat$Mileage,newdat$plo,col="red",lty=1,lwd=2)
    lines(newdat$Mileage,newdat$phi,col="red",lty=1,lwd=2)
    lines(newdat$Mileage,newdat$hi80,col="orange",lty=2,lwd=2)
    lines(newdat$Mileage,newdat$hi60,col="darkgreen",lty=2,lwd=2)
    lines(newdat$Mileage,newdat$hi40,col="green",lty=2,lwd=2)
    abline(v=input$vl,lwd=1,lty=3)
    nd<-data.frame(Mileage=seq(input$vl/10,length=1))
    nm<-model.matrix(~Mileage,nd)
    nd$y<-nm%*%m$coefficients
    p1 <- diag(nm %*% tcrossprod(vcov(m),nm))
    t1 <- p1+summary(m)$dispersion*(nd$y)*(nd$y)
    h80 = nd$y+qnorm(as.numeric(input$per))*sqrt(t1)
    abline(h=h80,lwd=1,lty=3)
    legend(x=input$vl,y=h80,legend = c(input$vl,round(x=h80,digits = 1)))
    legend("bottomright",legend=c("总体趋势","总体趋势范围","80%个体范围","60%个体范围","40%个体范围"),col=c("black","red","orange","darkgreen","green"),lty=c(1,1,2,2,2),lwd=2,bty="n",cex=2)
  })
  output$su<-renderPrint(
    summary(m)
  )
}
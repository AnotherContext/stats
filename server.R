# I thank R bloggers: R.Hyndman, T.Hirsch and klr from TimelyPortfolio for inspiration
# H. Wickham from R-Studio, and R Cookbook for ggplot2 package, "multiplot" function and many other fantastic features 
# and last but not least- R-Studio Team for their work on Shiny. 


# Installing R-INLA on Shiny server requires dowloading the program from website
# source("http://www.math.ntnu.no/inla/givemeINLA.R")
# upgrading
# inla.upgrade(testing=TRUE)

# other packages can be installed directly from CRAN

library(shiny)
library(INLA)
library(ggplot2)
library(reshape2)

df<-read.csv("~/ShinyApps/project2/mortalitydata.csv", header=TRUE, sep=",")

shinyServer(function(input, output) {
  
  output$downloadData <- downloadHandler(
    filename = function() {"mortalitydata.csv"},
    content = function(file) {
      write.csv(df, file)
    }
  ) 
  

  n <- 100
  k <- 21.92
  N <- k*n
  x <- (1:N)
  #Gaussian noise
  #y1 <- rnorm(N)
  #random walk
  #y2 <- cumsum(rnorm(N)) + cumsum(rnorm(N)) + rnorm(N)
  sim1<- 4 + .01* (1:N)+ rnorm(N)
  sim2<- 5*sin(2*pi*(1:N))+rnorm(N)
  sim3<- cos(2*pi*(1:N)/10) + rnorm(N) + cumsum(rnorm(N))
  sim4<- sin(2*pi*(1:N)) + rnorm(N) + cumsum(rnorm(N))
  sim5<- 4 + sqrt(.001*log10(1:N)) + rnorm(N) + cumsum(rnorm(N))
  # With a seasonal component/ non linear
  sim6 <- -.05*cos(2*pi*x) -.05*sin(2*pi*x) + cos(4*pi*x) + sin(4*pi*x) + cumsum(cumsum(rnorm(N)))  
  #plot(x, data.sim)
  #sim_text <- input$sim_func


  data.orig <- data.frame(df[,2], sim1, sim2, sim3, sim4, sim5, sim6)
  #head(data.set, data.sim)

   user_func <- reactive(function(){
     
      userchoice <- eval(parse(text=input$userchoice))
      
   })
  
  
   func <- reactive(function(){
     
    func_type <- switch(input$func_type,
                        sim="sim",
                        d_set="d_set")   
     
   })
  
  
  simulation_func <- reactive(function(){
    
    sim_func <- switch(input$sim_func,
                       sim1="linear function",
                       sim2="sine function",
                       sim3="cosine function",
                       sim4="polynomial",
                       sim5="non-linear type1",
                       sim6="non-linear type2")
    
  })
    

   data <- reactive(function(){  
    
    f_type <- switch(input$f_type,
                   rw1 = "rw1",
                   rw2 = "rw2",
                   para = "para")
    
   # f_type(input$h)
  })
  
  
  harm <- reactive(function(){
    
    harm_func <- switch(input$harm_fit, 
                        auto = "auto",
                        harm_user_def = "harm_user_def")
  })
  
  
  
   output$prediction <- reactivePlot(function(){
    func_type <- input$func_type
    f_type <- input$f_type
    sim_func <- input$sim_func
    harm_func <- input$harm_fit
    h <- input$h
    b <- input$b
    #b <- 1000
    #h<-100
    userchoice<-eval(parse(text=input$userchoice))
    data.orig<-cbind(data.orig, userchoice)
    #Y <- data.orig[b:2192, 1] #ifelse(func_type=="d_set", 1 , ifelse(sim_func=="linear function", 2, ifelse(sim_func=="sine function", 3, ifelse(sim_func=="cosine function", 4, ifelse(sim_func=="polynomial", 5, ifelse(sim_func=="non-linear type1", 6, ifelse(sim_func=="non-linear type2", 7, 8) ))))))]
    
    Y <- data.orig[b:2192, ifelse(func_type=="d_set", 1 , ifelse(sim_func=="linear function", 2, ifelse(sim_func=="sine function", 3, ifelse(sim_func=="cosine function", 4, ifelse(sim_func=="polynomial", 5, ifelse(sim_func=="non-linear type1", 6, ifelse(sim_func=="non-linear type2", 7, 8) ))))))]
    n <- length(Y)
    Y.predict<-c(Y[1:(length(Y)-h)],rep(NA,h)) 
    #Y.predict <- c(Y, rep(NA, h))
    n.predict <- length(Y.predict)
    time.predict <- 1:n.predict
    time.hor<- 1: (n.predict-h)
    time.predict_p <- (n.predict-h+1):n.predict
    
    #harmonics fitting
    # sin.year.predict <- 1000*sin(2*pi * time.predict/365)
    # cos.year.predict <- 0.01*cos(2*pi * time.predict/365)
    if(harm_func == "auto"){
      
      sin.year.predict  <- cos.year.predict  <- matrix(nr=length(time.predict), nc=4)
      for (i in 1:4){ 
        cos.year.predict[,i] <- cos(2*pi*i*time.predict/365)
        sin.year.predict[,i] <- sin(2*pi*i*time.predict/365) 
      }
      
      } else {
        
      sin.year.predict <- eval(parse(text=input$para_sin_func))
      cos.year.predict <- eval(parse(text=input$para_cos_func))
     
      }
    
    
    data.predict <- data.frame(Y.predict=Y.predict, time.predict=time.predict, sin.year.predict=sin.year.predict, cos.year.predict=cos.year.predict)
    formula <- Y.predict ~ time.predict + sin.year.predict + cos.year.predict
    model.para<- inla(formula=formula, family="gaussian", data=data.predict, control.compute=list(dic=TRUE, mlik=TRUE), control.fixed=list(mean.intercept=0, prec.intercept=0.0001), quantiles=c(.025, .1, .9, .975))
    lower95.para<- model.para$summary.fitted.values[time.predict_p, 3]
    lower80.para<- model.para$summary.fitted.values[time.predict_p, 4]
    upper80.para<- model.para$summary.fitted.values[time.predict_p, 5]
    upper95.para<- model.para$summary.fitted.values[time.predict_p ,6]
    plot(time.predict, Y.predict, col="darkseagreen", pch=20, ylab="Dataset values", xlab="Time")
    lines(time.hor, model.para$summary.fitted.values[time.hor ,1], col="grey60")
    legend("topleft",legend=c("Posterior mean for chosen fitting type", "Posterior mean for chosen horizon", "95% Credible intervals", "80% Credible intervals"), col=c("palevioletred2","red", "lightskyblue1", "lightskyblue"),lty=1, lwd=2)               
    
    data.predict <- data.frame(Y.predict=Y.predict, time.predict=time.predict)
    formula <- Y.predict ~ f(time.predict, model="rw1", cyclic=FALSE)
    model.rw1<- inla(formula=formula, family="gaussian", data=data.predict, control.compute=list(dic=TRUE, mlik=TRUE), control.fixed=list(mean.intercept=0, prec.intercept=0.0001), quantiles=c(.025, .1, .9, .975))
    lower95.rw1<- model.rw1$summary.fitted.values[time.predict_p, 3]
    lower80.rw1<- model.rw1$summary.fitted.values[time.predict_p, 4]
    upper80.rw1<- model.rw1$summary.fitted.values[time.predict_p, 5]
    upper95.rw1<- model.rw1$summary.fitted.values[time.predict_p ,6]
    lines(time.hor, model.rw1$summary.fitted.values[time.hor ,1], col="grey60")
    
    data.predict <- data.frame(Y.predict=Y.predict, time.predict=time.predict)
    formula <- Y.predict ~ f(time.predict, model="rw2", cyclic=FALSE)
    model.rw2<- inla(formula=formula, family="gaussian", data=data.predict, control.compute=list(dic=TRUE, mlik=TRUE), control.fixed=list(mean.intercept=0, prec.intercept=0.0001), quantiles=c(.025, .1, .9, .975))
    lower95.rw2<- model.rw2$summary.fitted.values[time.predict_p, 3]
    lower80.rw2<- model.rw2$summary.fitted.values[time.predict_p, 4]
    upper80.rw2<- model.rw2$summary.fitted.values[time.predict_p, 5]
    upper95.rw2<- model.rw2$summary.fitted.values[time.predict_p ,6]
    lines(time.hor, model.rw2$summary.fitted.values[time.hor,1], col="grey60")
    
    
    if(f_type=="para"){
    
    polygon(c(time.predict_p, rev(time.predict_p)), c(upper95.para, rev(lower95.para)), col="lightskyblue1", border="lightskyblue1")
    polygon(c(time.predict_p, rev(time.predict_p)), c(upper80.para, rev(lower80.para)), col="lightskyblue", border="lightskyblue")
    points(time.predict_p, Y[time.predict_p], col="deepskyblue4", pch=20)
    lines(time.hor, model.para$summary.fitted.values[time.hor,1], col="palevioletred2", lwd=3)
    lines(time.predict_p, model.para$summary.fitted.values[time.predict_p ,1], col="red", lwd=3)
    
   
    } else if(f_type=="rw1") {
    
     polygon(c(time.predict_p, rev(time.predict_p)), c(upper95.rw1, rev(lower95.rw1)), col="lightskyblue1", border="lightskyblue1")
     polygon(c(time.predict_p, rev(time.predict_p)), c(upper80.rw1, rev(lower80.rw1)), col="lightskyblue", border="lightskyblue")
     points(time.predict_p, Y[time.predict_p], col="deepskyblue4", pch=20)
     lines(time.hor, model.rw1$summary.fitted.values[time.hor,1], col="palevioletred2", lwd=3)
     lines(time.predict_p, model.rw1$summary.fitted.values[time.predict_p ,1], col="red", lwd=3)
     
    } else {
      
      polygon(c(time.predict_p, rev(time.predict_p)), c(upper95.rw2, rev(lower95.rw2)), col="lightskyblue1", border="lightskyblue1")
      polygon(c(time.predict_p, rev(time.predict_p)), c(upper80.rw2, rev(lower80.rw2)), col="lightskyblue", border="lightskyblue")
      points(time.predict_p, Y[time.predict_p], col="lightsteelblue4", pch=20)
      lines(time.hor, model.rw2$summary.fitted.values[time.hor ,1], col="palevioletred2", lwd=3)
      lines(time.predict_p, model.rw2$summary.fitted.values[time.predict_p ,1], col="red", lwd=3)
     
      }
    
   })
    
    output$prediction_summary <- reactiveTable(function() {
      func_type <- input$func_type
      f_type <- input$f_type
      sim_func <- input$sim_func
      harm_func <- input$harm_fit
      h <- input$h
      b <- input$b
      #b <- 1000
      #h<-100
      userchoice<-eval(parse(text=input$userchoice))
      data.orig<-cbind(data.orig, userchoice)
      #Y <- data.orig[b:2192, 1] #ifelse(func_type=="d_set", 1 , ifelse(sim_func=="linear function", 2, ifelse(sim_func=="sine function", 3, ifelse(sim_func=="cosine function", 4, ifelse(sim_func=="polynomial", 5, ifelse(sim_func=="non-linear type1", 6, ifelse(sim_func=="non-linear type2", 7, 8) ))))))]
      
      Y <- data.orig[b:2192, ifelse(func_type=="d_set", 1 , ifelse(sim_func=="linear function", 2, ifelse(sim_func=="sine function", 3, ifelse(sim_func=="cosine function", 4, ifelse(sim_func=="polynomial", 5, ifelse(sim_func=="non-linear type1", 6, ifelse(sim_func=="non-linear type2", 7, 8) ))))))]
      n <- length(Y)
      Y.predict<-c(Y[1:(length(Y)-h)],rep(NA,h)) 
      #Y.predict <- c(Y, rep(NA, h))
      n.predict <- length(Y.predict)
      time.predict <- 1:n.predict
      time.hor<- 1: (n.predict-h)
      time.predict_p <- (n.predict-h+1):n.predict
      
      #harmonics fitting
      # sin.year.predict <- 1000*sin(2*pi * time.predict/365)
      # cos.year.predict <- 0.01*cos(2*pi * time.predict/365)
      if(harm_func == "auto"){
        
        sin.year.predict  <- cos.year.predict  <- matrix(nr=length(time.predict), nc=4)
        for (i in 1:4){ 
          cos.year.predict[,i] <- cos(2*pi*i*time.predict/365)
          sin.year.predict[,i] <- sin(2*pi*i*time.predict/365) 
        }
        
      } else {
        
        sin.year.predict <- eval(parse(text=input$para_sin_func))
        cos.year.predict <- eval(parse(text=input$para_cos_func))
        
      }
      
      
      data.predict <- data.frame(Y.predict=Y.predict, time.predict=time.predict, sin.year.predict=sin.year.predict, cos.year.predict=cos.year.predict)
      formula <- Y.predict ~ time.predict + sin.year.predict + cos.year.predict
      model.para<- inla(formula=formula, family="gaussian", data=data.predict, control.compute=list(dic=TRUE, mlik=TRUE), control.fixed=list(mean.intercept=0, prec.intercept=0.0001), quantiles=c(.025, .1, .9, .975))
        
        data.predict <- data.frame(Y.predict=Y.predict, time.predict=time.predict)
        formula <- Y.predict ~ f(time.predict, model="rw1", cyclic=FALSE)
        model.rw1<- inla(formula=formula, family="gaussian", data=data.predict, control.compute=list(dic=TRUE, mlik=TRUE), control.fixed=list(mean.intercept=0, prec.intercept=0.0001), quantiles=c(.025, .1, .9, .975))
       
        data.predict <- data.frame(Y.predict=Y.predict, time.predict=time.predict)
        formula <- Y.predict ~ f(time.predict, model="rw2", cyclic=FALSE)
        model.rw2<- inla(formula=formula, family="gaussian", data=data.predict, control.compute=list(dic=TRUE, mlik=TRUE), control.fixed=list(mean.intercept=0, prec.intercept=0.0001), quantiles=c(.025, .1, .9, .975))
 
        data.frame("True Value"=Y[time.predict_p], "RW1 Forecast"=model.rw1$summary.fitted[time.predict_p,1], "RW2 Forecast"=model.rw2$summary.fitted[time.predict_p,1], "PRM Forecast"=model.para$summary.fitted[time.predict_p,1], 
                   "RW1 Lower95"=model.rw1$summary.fitted[time.predict_p,3], "RW2 Lower95"=model.rw2$summary.fitted[time.predict_p,3], "PRM Lower95"=model.para$summary.fitted[time.predict_p,3],
                   #"RW1 Lower80"=model.rw1$summary.fitted[time.predict_p,4], "RW2 Lower80"=model.rw2$summary.fitted[time.predict_p,4], "PRM Lower80"=model.para$summary.fitted[time.predict_p,4],
                   #"RW1 Upper80"=model.rw1$summary.fitted[time.predict_p,5], "RW2 Upper80"=model.rw2$summary.fitted[time.predict_p,5], "PRM Upper80"=model.para$summary.fitted[time.predict_p,5],
                   "RW1 Upper95"=model.rw1$summary.fitted[time.predict_p,6], "RW2 Upper95"=model.rw2$summary.fitted[time.predict_p,6], "PRM Upper95"=model.para$summary.fitted[time.predict_p,6])
        
        
    
  })
   

   output$hyperpar_summary <- reactivePrint(function() {
     func_type <- input$func_type
     f_type <- input$f_type
     sim_func <- input$sim_func
     harm_func <- input$harm_fit
     h <- input$h
     b <- input$b
     #b <- 1597
     
     userchoice<-eval(parse(text=input$userchoice))
     data.orig<-cbind(data.orig, userchoice)
     
     Y <- data.orig[b:2192, ifelse(func_type=="d_set", 1 , ifelse(sim_func=="linear function", 2, ifelse(sim_func=="sine function", 3, ifelse(sim_func=="cosine function", 4, ifelse(sim_func=="polynomial", 5, ifelse(sim_func=="non-linear type1", 6, ifelse(sim_func=="non-linear type2", 7, 8) ))))))]
     n <- length(Y)
     Y.predict<-c(Y[1:(length(Y)-h)],rep(NA,h)) 
     #Y.predict <- c(Y, rep(NA, h))
     n.predict <- length(Y.predict)
     time.predict <- 1:n.predict
     time.hor<- 1: (n.predict-h)
     time.predict_p <- (n.predict-h+1):n.predict
     
     
     if(f_type=="para"){
       
       if(harm_func == "auto"){
         
         sin.year.predict  <- cos.year.predict  <- matrix(nr=length(time.predict), nc=4)
         for (i in 1:4){ 
           cos.year.predict[,i] <- cos(2*pi*i*time.predict/365)
           sin.year.predict[,i] <- sin(2*pi*i*time.predict/365) 
         }
         
       } else {
         
         sin.year.predict <- eval(parse(text=input$para_sin_func))
         cos.year.predict <- eval(parse(text=input$para_cos_func))
         
       }
       data.predict <- data.frame(Y.predict=Y.predict, time.predict=time.predict, sin.year.predict=sin.year.predict, cos.year.predict=cos.year.predict)
       formula <- Y.predict ~ time.predict + sin.year.predict + cos.year.predict
       model.para <- inla(formula=formula, family="gaussian", data=data.predict, control.compute=list(dic=TRUE, mlik=TRUE), control.fixed=list(mean.intercept=0, prec.intercept=0.0001), quantiles=c(.025,0.95))
      
       model.para$summary.hyperpar
             
     } else {
       
       data.predict <- data.frame(Y.predict=Y.predict, time.predict=time.predict)
       formula <- Y.predict ~ f(time.predict, model=f_type, cyclic=FALSE)
       model <- inla(formula=formula, family="gaussian", data=data.predict, control.compute=list(dic=TRUE, mlik=TRUE), control.fixed=list(mean.intercept=0, prec.intercept=0.0001), quantiles=c(.025,0.95))
       
       model$summary.hyperpar
       }
     
   })
   
   output$fixed_summary <- reactivePrint(function() {
     func_type <- input$func_type
     f_type <- input$f_type
     sim_func <- input$sim_func
     harm_func <- input$harm_fit
     h <- input$h
     b <- input$b
     #b <- 1597
     
     userchoice<-eval(parse(text=input$userchoice))
     data.orig<-cbind(data.orig, userchoice)
     
     Y <- data.orig[b:2192, ifelse(func_type=="d_set", 1 , ifelse(sim_func=="linear function", 2, ifelse(sim_func=="sine function", 3, ifelse(sim_func=="cosine function", 4, ifelse(sim_func=="polynomial", 5, ifelse(sim_func=="non-linear type1", 6, ifelse(sim_func=="non-linear type2", 7, 8) ))))))]
     n <- length(Y)
     Y.predict<-c(Y[1:(length(Y)-h)],rep(NA,h)) 
     #Y.predict <- c(Y, rep(NA, h))
     n.predict <- length(Y.predict)
     time.predict <- 1:n.predict
     time.hor<- 1: (n.predict-h)
     time.predict_p <- (n.predict-h+1):n.predict
     
     if(f_type=="para"){
       
       if(harm_func == "auto"){
         
         sin.year.predict  <- cos.year.predict  <- matrix(nr=length(time.predict), nc=4)
         for (i in 1:4){ 
           cos.year.predict[,i] <- cos(2*pi*i*time.predict/365)
           sin.year.predict[,i] <- sin(2*pi*i*time.predict/365) 
         }
         
       } else {
         
         sin.year.predict <- eval(parse(text=input$para_sin_func))
         cos.year.predict <- eval(parse(text=input$para_cos_func))
         
       }
       data.predict <- data.frame(Y.predict=Y.predict, time.predict=time.predict, sin.year.predict=sin.year.predict, cos.year.predict=cos.year.predict)
       formula <- Y.predict ~ time.predict + sin.year.predict + cos.year.predict
       model.para <- inla(formula=formula, family="gaussian", data=data.predict, control.compute=list(dic=TRUE, mlik=TRUE), control.fixed=list(mean.intercept=0, prec.intercept=0.0001), quantiles=c(.025,0.95))
       
       #model.para$summary.fixed
       summary(model.para)
     } else {
       
       data.predict <- data.frame(Y.predict=Y.predict, time.predict=time.predict)
       formula <- Y.predict ~ f(time.predict, model=f_type, cyclic=FALSE)
       model <- inla(formula=formula, family="gaussian", data=data.predict, control.compute=list(dic=TRUE, mlik=TRUE), control.fixed=list(mean.intercept=0, prec.intercept=0.0001), quantiles=c(.025,0.95))
       
       #model$summary.fixed
       summary(model)
     }
     
   })
  
   
    
    
  
  output$prediction_errors <- reactiveTable(function() {
    func_type <- input$func_type
    f_type <- input$f_type
    sim_func <- input$sim_func
    harm_func <- input$harm_fit
    h <- input$h
    b <- input$b
    #b <- 1597
    
    userchoice<-eval(parse(text=input$userchoice))
    data.orig<-cbind(data.orig, userchoice)
    
    Y <- data.orig[b:2192, ifelse(func_type=="d_set", 1 , ifelse(sim_func=="linear function", 2, ifelse(sim_func=="sine function", 3, ifelse(sim_func=="cosine function", 4, ifelse(sim_func=="polynomial", 5, ifelse(sim_func=="non-linear type1", 6, ifelse(sim_func=="non-linear type2", 7, 8) ))))))]
    n <- length(Y)
    Y.predict<-c(Y[1:(length(Y)-h)],rep(NA,h)) 
    #Y.predict <- c(Y, rep(NA, h))
    n.predict <- length(Y.predict)
    time.predict <- 1:n.predict
    time.predict_p <- (n.predict-h+1):n.predict

      
    if(harm_func == "auto"){
      
      sin.year.predict  <- cos.year.predict  <- matrix(nr=length(time.predict), nc=4)
      for (i in 1:4){ 
        cos.year.predict[,i] <- cos(2*pi*i*time.predict/365)
        sin.year.predict[,i] <- sin(2*pi*i*time.predict/365) 
      }
      
    } else {
      
      sin.year.predict <- eval(parse(text=input$para_sin_func))
      cos.year.predict <- eval(parse(text=input$para_cos_func))
      
    }
    
      data.predict <- data.frame(Y.predict=Y.predict, time.predict=time.predict, sin.year.predict=sin.year.predict, cos.year.predict=cos.year.predict)
      formula <- Y.predict ~ time.predict + sin.year.predict + cos.year.predict
      model.para <- inla(formula=formula, family="gaussian", data=data.predict, control.compute=list(dic=TRUE, mlik=TRUE), control.fixed=list(mean.intercept=0, prec.intercept=0.0001), quantiles=c(.025,0.95))
      
      mape.para <- rmse.para  <- mse.para <- bias.para <- matrix(NA,h)
      diff.para <- model.para$summary.fitted[time.predict_p,1]-Y[time.predict_p]
      #diff.para_m <- (Y[time.predict_p]-model.para$summary.fitted[time.predict_p,1])/Y[time.predict_p]
    
      data.predict <- data.frame(Y.predict=Y.predict, time.predict=time.predict)
      formula <- Y.predict ~ f(time.predict, model="rw1", cyclic=FALSE)
      model.rw1 <- inla(formula=formula, family="gaussian", data=data.predict, control.compute=list(dic=TRUE, mlik=TRUE), control.fixed=list(mean.intercept=0, prec.intercept=0.0001), quantiles=c(.025,0.95))
      
      mape.rw1<- rmse.rw1 <- bias.rw1 <- matrix(NA,h)
      diff.rw1<- model.rw1$summary.fitted[time.predict_p,1]-Y[time.predict_p]
      #diff.rw1_m <- (Y[time.predict_p]-model.rw1$summary.fitted[time.predict_p,1])/Y[time.predict_p]
      
      
      data.predict <- data.frame(Y.predict=Y.predict, time.predict=time.predict)
      formula <- Y.predict ~ f(time.predict, model="rw2", cyclic=FALSE)
      model.rw2 <- inla(formula=formula, family="gaussian", data=data.predict, control.compute=list(dic=TRUE, mlik=TRUE), control.fixed=list(mean.intercept=0, prec.intercept=0.0001), quantiles=c(.025,0.95))
      
      mape.rw2 <- rmse.rw2 <- bias.rw2 <- matrix(NA,h)
      diff.rw2<- model.rw2$summary.fitted[time.predict_p,1] -Y[time.predict_p]
      #diff.rw2_m <- (Y[time.predict_p]-model.rw2$summary.fitted[time.predict_p,1])/Y[time.predict_p]
         
      for(i in 2:h){
        
        bias.para[i]<-sum((diff.para[2:i])/i)
        mape.para[i]<-sum(abs(diff.para[2:i])/i)
        rmse.para[i]<-sqrt(sum((diff.para[2:i])^2)/ i)
        
        #mape.rw1[i]<-sum(abs(diff.rw1_m[1:i]))*100/n
        bias.rw1[i]<-sum((diff.rw1[2:i])/i)
        mape.rw1[i]<-sum(abs(diff.rw1[2:i])/i)
        rmse.rw1[i]<-sqrt(sum((diff.rw1[2:i])^2)/ i)
        
        #mape.rw2[i]<-sum(abs(diff.rw2_m[1:i]))*100/n
        bias.rw2[i]<-sum((diff.rw2[2:i])/i)
        mape.rw2[i]<-sum(abs(diff.rw2[2:i])/i)
        rmse.rw2[i]<-sqrt(sum((diff.rw2[2:i])^2) / i)
  
      }
                   
      data.frame("True Value"=Y[time.predict_p], "RW1 Forecast"=model.rw1$summary.fitted[time.predict_p,1], "RW2 Forecast"=model.rw2$summary.fitted[time.predict_p,1], "PRM Forecast"=model.para$summary.fitted[time.predict_p,1], "RW1 Bias"=bias.rw1, "RW2 Bias"=bias.rw2, "PRM Bias"=bias.para, "RW1 Rmse"=rmse.rw1, "RW2 Rmse"=rmse.rw2, "PRM Rmse"=rmse.para, "RW1 Mape"=mape.rw1, "RW2 Mape"=mape.rw2, "PRM Mape"=mape.para )
     
  })
  
  
  
  output$error.facets <- reactivePlot(function() {
    func_type <- input$func_type
    f_type <- input$f_type
    sim_func <- input$sim_func
    harm_func <- input$harm_fit
    h <- input$h
    b <- input$b
    #b <- 1597
    userchoice<-eval(parse(text=input$userchoice))
    data.orig<-cbind(data.orig, userchoice)
    
    Y <- data.orig[b:2192, ifelse(func_type=="d_set", 1 , ifelse(sim_func=="linear function", 2, ifelse(sim_func=="sine function", 3, ifelse(sim_func=="cosine function", 4, ifelse(sim_func=="polynomial", 5, ifelse(sim_func=="non-linear type1", 6, ifelse(sim_func=="non-linear type2", 7, 8) ))))))]
    n <- length(Y)
    Y.predict<-c(Y[1:(length(Y)-h)],rep(NA,h)) 
    #Y.predict <- c(Y, rep(NA, h))
    n.predict <- length(Y.predict)
    time.predict <- 1:n.predict
    time.predict_p <- (n.predict-h+1):n.predict
    
    
    if(harm_func == "auto"){
      
      sin.year.predict  <- cos.year.predict  <- matrix(nr=length(time.predict), nc=4)
      for (i in 1:4){ 
        cos.year.predict[,i] <- cos(2*pi*i*time.predict/365)
        sin.year.predict[,i] <- sin(2*pi*i*time.predict/365) 
      }
      
    } else {
      
      sin.year.predict <- eval(parse(text=input$para_sin_func))
      cos.year.predict <- eval(parse(text=input$para_cos_func))
      
    }
    data.predict <- data.frame(Y.predict=Y.predict, time.predict=time.predict, sin.year.predict=sin.year.predict, cos.year.predict=cos.year.predict)
    formula <- Y.predict ~ time.predict + sin.year.predict + cos.year.predict
    model.para <- inla(formula=formula, family="gaussian", data=data.predict, control.compute=list(dic=TRUE, mlik=TRUE), control.fixed=list(mean.intercept=0, prec.intercept=0.0001), quantiles=c(.025,0.95))
    
    data.predict <- data.frame(Y.predict=Y.predict, time.predict=time.predict)
    formula <- Y.predict ~ f(time.predict, model="rw1", cyclic=FALSE)
    model.rw1 <- inla(formula=formula, family="gaussian", data=data.predict, control.compute=list(dic=TRUE, mlik=TRUE), control.fixed=list(mean.intercept=0, prec.intercept=0.0001), quantiles=c(.025,0.95))
    
    data.predict <- data.frame(Y.predict=Y.predict, time.predict=time.predict)
    formula <- Y.predict ~ f(time.predict, model="rw2", cyclic=FALSE)
    model.rw2 <- inla(formula=formula, family="gaussian", data=data.predict, control.compute=list(dic=TRUE, mlik=TRUE), control.fixed=list(mean.intercept=0, prec.intercept=0.0001), quantiles=c(.025,0.95))
    
    mape.para  <-mape.rw1 <- mape.rw2 <- matrix(NA,n)
    rmse.para  <-rmse.rw1 <- rmse.rw2 <- matrix(NA,n)
    #bias.para  <-bias.rw1 <- bias.rw2 <- matrix(NA,n)

  #  diff.para_m <- (Y[time.predict]-model.para$summary.fitted[time.predict,1])/Y[time.predict]
  #  diff.rw1_m <- (Y[time.predict]-model.rw1$summary.fitted[time.predict,1])/Y[time.predict]
  #  diff.rw2_m <- (Y[time.predict]-model.rw2$summary.fitted[time.predict,1])/Y[time.predict]
    diff.para<- model.para$summary.fitted[time.predict,1] - Y[time.predict]
    diff.rw1 <- model.rw1$summary.fitted[time.predict,1] - Y[time.predict]
    diff.rw2 <- model.rw2$summary.fitted[time.predict,1] - Y[time.predict]
    
    
    for(i in 2:n) {
       
      #mape.para[i]<-sum(abs(diff.para_m[1:i]))*100/n
      #bias.para[i]<-((sum(diff.para[1:i])^2)/i)
      mape.para[i]<-sum(abs(diff.para[2:i])/i)
      rmse.para[i]<-sqrt(sum((diff.para[2:i])^2) / i)

      #mape.rw1[i]<-sum(abs(diff.rw1_m[1:i]))*100/n
      #bias.rw1[i]<-((sum(diff.rw1[1:i])^2)/i)
      mape.rw1[i]<-sum(abs(diff.rw1[2:i])/i)
      rmse.rw1[i]<-sqrt(sum((diff.rw1[2:i])^2) / i)
      
      #mape.rw2[i]<-sum(abs(diff.rw2_m[1:i]))*100/n
      #bias.rw2[i]<-((sum(diff.rw2[1:i])^2)/i)
      mape.rw2[i]<-sum(abs(diff.rw2[2:i])/i)
      rmse.rw2[i]<-sqrt(sum((diff.rw2[2:i])^2) / i)
      
    }
    
    #bias<-data.frame(RW1= bias.rw1, RW2= bias.rw2, PRM=bias.para)# error=rep("bias", h))
    #bias_m<-melt(bias)
    #bias_g<-cbind(bias_m, error=rep("BIAS", 3*n))
    rmse<-data.frame(RW1= rmse.rw1, RW2= rmse.rw2, PRM=rmse.para)#, error=rep("rmse", h))
    rmse_m<-melt(rmse)
    rmse_g<-cbind(rmse_m, error=rep("RMSE", 3*n))
    mape<-data.frame(RW1= mape.rw1, RW2= mape.rw2, PRM=mape.para)#, error=rep("mape", h))
    mape_m<-melt(mape)
    mape_g<-cbind(mape_m, error=rep("MAPE", 3*n))
    horz <- data.frame(horz=rep(1:n, 6))
    
    err1<- rbind(rmse_g, mape_g)
    err<- data.frame(err1)
    err<-  cbind(err, horz)
     #err_bias<- cbind(bias_g, horz=rep(1:n, 3))

    par(mar=c(0,0,0,0))
    g1 <- ggplot(err, aes(horz, value, col=variable))+ geom_line() + facet_grid(. ~ error) +
      ylab("Error values") + xlab("Time") + geom_text(data=err[err$horz==n,], aes(label= variable), hjust=0.5, vjust=0, size=3) + 
      geom_vline(xintercept = n-h, color = "black", linetype="dashed", size = 0.3) + 
      theme(legend.position="none")
    
    err_lim <- boxplot.stats(err$value)$stats[c(1, 5)]
    g2 <- ggplot(err, aes(factor(variable), value)) + geom_boxplot(aes(fill=variable), colour="grey30" ) + 
      geom_violin(fill="grey70", alpha=.3, colour="white") + 
      #geom_jitter(height = 0,  alpha=.1, col="grey80")  + 
      theme(legend.position="none") +
      #stat_summary(fun.y=median, geom="point", fill="white", shape=21, size=4) +  
      facet_grid(.~error)
    
    #R Cookbook mulitplot function
    multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
      require(grid)
      
      # Make a list from the ... arguments and plotlist
      plots <- c(list(...), plotlist)
      
      numPlots = length(plots)
      
      if (is.null(layout)) {
        layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                         ncol = cols, nrow = ceiling(numPlots/cols))
      }
      
      if (numPlots==1) {
        print(plots[[1]])
        
      } else {
        # Set up the page
        grid.newpage()
        pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
        
        # Make each plot, in the correct location
        for (i in 1:numPlots) {
          # Get the i,j matrix positions of the regions that contain this subplot
          matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
          
          print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                          layout.pos.col = matchidx$col))
        }
      }
    }
    
    multiplot(g1, g2)
    
  
  })
  
  
  
  output$sd.facets <- reactivePlot(function() {
    func_type <- input$func_type
    f_type <- input$f_type
    sim_func <- input$sim_func
    harm_func <- input$harm_fit
    h <- input$h
    b <- input$b
    #b <- 1597
    #Y <- data.orig[b:2192, ifelse(func_type=="d_set", 1 , ifelse(sim_func=="sim1", 2, ifelse(sim_func=="sim2", 3, ifelse(sim_func=="sim3", 4, ifelse(sim_func=="sim5", 5, 6 )))))]
    
    userchoice<-eval(parse(text=input$userchoice))
    data.orig<-cbind(data.orig, userchoice)
    
    Y <- data.orig[b:2192, ifelse(func_type=="d_set", 1 , ifelse(sim_func=="linear function", 2, ifelse(sim_func=="sine function", 3, ifelse(sim_func=="cosine function", 4, ifelse(sim_func=="polynomial", 5, ifelse(sim_func=="non-linear type1", 6, ifelse(sim_func=="non-linear type2", 7, 8) ))))))]
    n <- length(Y)
    Y.predict<-c(Y[1:(length(Y)-h)],rep(NA,h)) 
    #Y.predict <- c(Y, rep(NA, h))
    n.predict <- length(Y.predict)
    time.predict <- 1:n.predict
    time.predict_p <- (n.predict-h+1):n.predict
    time.predict_nh<-1:(n.predict-h)
    
    
    if(harm_func == "auto"){
      
      sin.year.predict  <- cos.year.predict  <- matrix(nr=length(time.predict), nc=4)
      for (i in 1:4){ 
        cos.year.predict[,i] <- cos(2*pi*i*time.predict/365)
        sin.year.predict[,i] <- sin(2*pi*i*time.predict/365) 
      }
      
    } else {
      
      sin.year.predict <- eval(parse(text=input$para_sin_func))
      cos.year.predict <- eval(parse(text=input$para_cos_func))
      
    }
    data.predict <- data.frame(Y.predict=Y.predict, time.predict=time.predict, sin.year.predict=sin.year.predict, cos.year.predict=cos.year.predict)
    formula <- Y.predict ~ time.predict + sin.year.predict + cos.year.predict
    model.para <- inla(formula=formula, family="gaussian", data=data.predict, control.compute=list(dic=TRUE, mlik=TRUE), control.fixed=list(mean.intercept=0, prec.intercept=0.0001), quantiles=c(.025,0.95))
    
    data.predict <- data.frame(Y.predict=Y.predict, time.predict=time.predict)
    formula <- Y.predict ~ f(time.predict, model="rw1", cyclic=FALSE)
    model.rw1 <- inla(formula=formula, family="gaussian", data=data.predict, control.compute=list(dic=TRUE, mlik=TRUE), control.fixed=list(mean.intercept=0, prec.intercept=0.0001), quantiles=c(.025,0.95))
  
    data.predict <- data.frame(Y.predict=Y.predict, time.predict=time.predict)
    formula <- Y.predict ~ f(time.predict, model="rw2", cyclic=FALSE)
    model.rw2 <- inla(formula=formula, family="gaussian", data=data.predict, control.compute=list(dic=TRUE, mlik=TRUE), control.fixed=list(mean.intercept=0, prec.intercept=0.0001), quantiles=c(.025,0.95))
    
    bias.para  <-bias.rw1 <- bias.rw2 <- matrix(NA,n)
    
    diff.para<- model.para$summary.fitted[time.predict,1]-Y[time.predict]
    diff.rw1 <- model.rw1$summary.fitted[time.predict,1]-Y[time.predict]
    diff.rw2 <- model.rw2$summary.fitted[time.predict,1]-Y[time.predict]
    
    
    for(i in 2:n){
      bias.para[i]<-sum(diff.para[2:i])/i
      bias.rw1[i]<-sum(diff.rw1[2:i])/i
      bias.rw2[i]<-sum(diff.rw2[2:i])/i
    }
    
    bias<-data.frame(RW1= bias.rw1, RW2= bias.rw2, PRM=bias.para)# error=rep("bias", h))
    bias_m<-melt(bias)
    bias_g<-cbind(bias_m, error=rep("BIAS", 3*n))
    err_bias<- cbind(bias_g, horz=rep(1:n, 3))
    
    sd<- data.frame(RW1=model.rw1$summary.fitted[ time.predict,2], RW2= model.rw2$summary.fitted[time.predict,2], PRM=model.para$summary.fitted[time.predict,2])
    sd_m<-melt(sd)
    sd_g<-cbind(sd_m, error=rep("SD", 3*n))
    err_sd<-cbind(sd_g, horz=rep(1:n, 3))
    
    par(mar=c(0,0,0,0))
    # compute lower and upper whiskers
    sd_lim <- boxplot.stats(err_sd$value)$stats[c(1, 5)]    
    bias_lim <- boxplot.stats(err_bias$value)$stats[c(1, 5)]
   
    g1 <- ggplot(err_sd, aes(horz, value, col=variable))+ geom_line() + facet_grid(. ~ error) +
      ylab("SD values") + xlab("Time") + geom_text(data=err_sd[err_sd$horz==n,], aes(label= variable), hjust=1, vjust=0, size=3) + 
      theme(legend.position="none") +
      geom_vline(xintercept = n-h, color = "black", linetype="dashed", size = 0.3)

    g2 <- ggplot(err_bias, aes(horz, value, col=variable))+ geom_line() + facet_grid(. ~ error) +
      ylab("Bias values") + xlab("Time") + geom_text(data=err_bias[err_bias$horz==n,], aes(label= variable), hjust=1, vjust=0, size=3) + 
      theme(legend.position="none") +
      geom_vline(xintercept = n-h, color = "black", linetype="dashed", size = 0.3)
    
    # scale g3&g4 limits based on sd_lim and bias_lim, source: http://stackoverflow.com/questions/5677885/ignore-outliers-in-ggplot2-boxplot
    g3 <- ggplot(err_sd, aes(factor(variable), value)) + geom_boxplot(aes(fill=variable), colour="grey30" )+ 
      geom_violin(fill="grey70", alpha=.3, colour="white") + 
      #geom_jitter(height = 0,  alpha=.2, col="grey80")  +  
      facet_grid(.~error) + theme(legend.position="none") + coord_cartesian(ylim = sd_lim*1.05)
      #stat_summary(fun.y=median, geom="point", fill="white", shape=21, size=4) +  facet_grid(.~error)
    g4 <- ggplot(err_bias, aes(factor(variable), value)) + geom_boxplot(aes(fill=variable), colour="grey30" )+ 
      geom_violin(fill="grey70", alpha=.3, colour="white") + 
      #geom_jitter(height = 0,  alpha=.2, col="grey80")  +
      #stat_summary(fun.y=median, geom="point", fill="white", shape=21, size=4) + 
      facet_grid(.~error) + theme(legend.position="none") + coord_cartesian(ylim = bias_lim*1.05)
    
    #R Cookbook mulitplot function
    multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
      require(grid)
      
      # Make a list from the ... arguments and plotlist
      plots <- c(list(...), plotlist)
      
      numPlots = length(plots)
      
      if (is.null(layout)) {
        layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                         ncol = cols, nrow = ceiling(numPlots/cols))
      }
      
      if (numPlots==1) {
        print(plots[[1]])
        
      } else {
        # Set up the page
        grid.newpage()
        pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
        
        # Make each plot, in the correct location
        for (i in 1:numPlots) {
          # Get the i,j matrix positions of the regions that contain this subplot
          matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
          
          print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                          layout.pos.col = matchidx$col))
        }
      }
    }
    
    multiplot(g1, g3, g2, g4, cols=2)
  })
  
 
    
  output$res.facets <- reactivePlot(function() {
    func_type <- input$func_type
    f_type <- input$f_type
    sim_func <- input$sim_func
    harm_func <- input$harm_fit
    h <- input$h
    b <- input$b
    #b <- 1597
 
    userchoice<-eval(parse(text=input$userchoice))
    data.orig<-cbind(data.orig, userchoice)
    
    Y <- data.orig[b:2192, ifelse(func_type=="d_set", 1 , ifelse(sim_func=="linear function", 2, ifelse(sim_func=="sine function", 3, ifelse(sim_func=="cosine function", 4, ifelse(sim_func=="polynomial", 5, ifelse(sim_func=="non-linear type1", 6, ifelse(sim_func=="non-linear type2", 7, 8) ))))))]
    n <- length(Y)
    Y.predict<-c(Y[1:(length(Y)-h)],rep(NA,h)) 
    #Y.predict <- c(Y, rep(NA, h))
    n.predict <- length(Y.predict)
    time.predict <- 1:n.predict
    time.predict_p <- (n.predict-h+1):n.predict
    
    
    if(harm_func == "auto"){
      
      sin.year.predict  <- cos.year.predict  <- matrix(nr=length(time.predict), nc=4)
      for (i in 1:4){ 
        cos.year.predict[,i] <- cos(2*pi*i*time.predict/365)
        sin.year.predict[,i] <- sin(2*pi*i*time.predict/365) 
      }
      
    } else {
      
      sin.year.predict <- eval(parse(text=input$para_sin_func))
      cos.year.predict <- eval(parse(text=input$para_cos_func))
      
    }
    
    data.predict <- data.frame(Y.predict=Y.predict, time.predict=time.predict, sin.year.predict=sin.year.predict, cos.year.predict=cos.year.predict)
    formula <- Y.predict ~ time.predict + sin.year.predict + cos.year.predict
    model.para <- inla(formula=formula, family="gaussian", data=data.predict, control.compute=list(dic=TRUE, mlik=TRUE), control.fixed=list(mean.intercept=0, prec.intercept=0.0001), quantiles=c(.025,0.95))
    
    data.predict <- data.frame(Y.predict=Y.predict, time.predict=time.predict)
    formula <- Y.predict ~ f(time.predict, model="rw1", cyclic=FALSE)
    model.rw1 <- inla(formula=formula, family="gaussian", data=data.predict, control.compute=list(dic=TRUE, mlik=TRUE), control.fixed=list(mean.intercept=0, prec.intercept=0.0001), quantiles=c(.025,0.95))
    
    data.predict <- data.frame(Y.predict=Y.predict, time.predict=time.predict)
    formula <- Y.predict ~ f(time.predict, model="rw2", cyclic=FALSE)
    model.rw2 <- inla(formula=formula, family="gaussian", data=data.predict, control.compute=list(dic=TRUE, mlik=TRUE), control.fixed=list(mean.intercept=0, prec.intercept=0.0001), quantiles=c(.025,0.95))
    
    res.para<- Y[time.predict]-model.para$summary.fitted[time.predict,1]
    res.rw1 <- Y[time.predict]-model.rw1$summary.fitted[time.predict,1]
    res.rw2 <- Y[time.predict]-model.rw2$summary.fitted[time.predict,1]
    
    diff.para_m <- (Y[time.predict_p]-model.para$summary.fitted[time.predict_p,1])/Y[time.predict_p]
    diff.para<- Y[time.predict_p]-model.para$summary.fitted[time.predict_p,1]
    diff.rw1_m <- (Y[time.predict_p]-model.rw1$summary.fitted[time.predict_p,1])/Y[time.predict_p]
    diff.rw1 <- Y[time.predict_p]-model.rw1$summary.fitted[time.predict_p,1]
    diff.rw2_m <- (Y[time.predict_p]-model.rw2$summary.fitted[time.predict_p,1])/Y[time.predict_p]
    diff.rw2 <- Y[time.predict_p]-model.rw2$summary.fitted[time.predict_p,1]
    
       
    ## melting   
    res.all<-data.frame(RW1=res.rw1, RW2=res.rw2, PRM=res.para)
    res.all_m<-melt(res.all)
    res.all_g<-cbind(res.all_m, id=rep(1:n, 3))
    res.h<-data.frame(RW1=diff.rw1, RW2=diff.rw2, PRM=diff.para)
    res.h_m<-melt(res.h)
    res.h_g<-cbind(res.h_m, id=rep(1:h, 3))
    
    
     
    #R Cookbook mulitplot function
    multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
      require(grid)
      
      # Make a list from the ... arguments and plotlist
      plots <- c(list(...), plotlist)
      
      numPlots = length(plots)
      
      if (is.null(layout)) {
        layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                         ncol = cols, nrow = ceiling(numPlots/cols))
      }
      
      if (numPlots==1) {
        print(plots[[1]])
        
      } else {
        # Set up the page
        grid.newpage()
        pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
        
        # Make each plot, in the correct location
        for (i in 1:numPlots) {
          # Get the i,j matrix positions of the regions that contain this subplot
          matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
          
          print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                          layout.pos.col = matchidx$col))
        }
      }
    }
    
    
    par(mar=c(0,0,0,0))
    g1 <- ggplot(res.all_g, aes(id, value, col=variable)) + geom_line() + facet_grid(.~ variable) +
      ylab("Res for All Time Values") + xlab("Time") + theme(legend.position="none") +
      geom_hline(yintercept = 0, color = "black", linetype="dashed", size = 0.3) +
      geom_vline(xintercept = n-h, color = "black", linetype="dashed", size = 0.3)
    g2 <- ggplot(res.h_g, aes(id, value, col=variable)) + geom_line() + facet_grid(.~ variable) +
      ylab("Res for Horizon Values") + xlab("Horizon") + theme(legend.position="none") +
      geom_hline(yintercept = 0, color = "black", linetype="dashed", size = 0.3)
    
    
    
    multiplot(g1,g2)
  })
  
  
  
  
  output$acf.facets <- reactivePlot(function() {
    func_type <- input$func_type
    f_type <- input$f_type
    sim_func <- input$sim_func
    harm_func <- input$harm_fit
    h <- input$h
    b <- input$b
    #b <- 1597
    
    
    userchoice<-eval(parse(text=input$userchoice))
    data.orig<-cbind(data.orig, userchoice)
    
    Y <- data.orig[b:2192, ifelse(func_type=="d_set", 1 , ifelse(sim_func=="linear function", 2, ifelse(sim_func=="sine function", 3, ifelse(sim_func=="cosine function", 4, ifelse(sim_func=="polynomial", 5, ifelse(sim_func=="non-linear type1", 6, ifelse(sim_func=="non-linear type2", 7, 8) ))))))]
    n <- length(Y)
    Y.predict<-c(Y[1:(length(Y)-h)],rep(NA,h)) 
    #Y.predict <- c(Y, rep(NA, h))
    n.predict <- length(Y.predict)
    time.predict <- 1:n.predict
    time.predict_p <- (n.predict-h+1):n.predict
    
    if(harm_func == "auto"){
      
      sin.year.predict  <- cos.year.predict  <- matrix(nr=length(time.predict), nc=4)
      for (i in 1:4){ 
        cos.year.predict[,i] <- cos(2*pi*i*time.predict/365)
        sin.year.predict[,i] <- sin(2*pi*i*time.predict/365) 
      }
      
    } else {
      
      sin.year.predict <- eval(parse(text=input$para_sin_func))
      cos.year.predict <- eval(parse(text=input$para_cos_func))
      
    }
    
    data.predict <- data.frame(Y.predict=Y.predict, time.predict=time.predict, sin.year.predict=sin.year.predict, cos.year.predict=cos.year.predict)
    formula <- Y.predict ~ time.predict + sin.year.predict + cos.year.predict
    model.para <- inla(formula=formula, family="gaussian", data=data.predict, control.compute=list(dic=TRUE, mlik=TRUE), control.fixed=list(mean.intercept=0, prec.intercept=0.0001), quantiles=c(.025,0.95))
    
    data.predict <- data.frame(Y.predict=Y.predict, time.predict=time.predict)
    formula <- Y.predict ~ f(time.predict, model="rw1", cyclic=FALSE)
    model.rw1 <- inla(formula=formula, family="gaussian", data=data.predict, control.compute=list(dic=TRUE, mlik=TRUE), control.fixed=list(mean.intercept=0, prec.intercept=0.0001), quantiles=c(.025,0.95))
    
    data.predict <- data.frame(Y.predict=Y.predict, time.predict=time.predict)
    formula <- Y.predict ~ f(time.predict, model="rw2", cyclic=FALSE)
    model.rw2 <- inla(formula=formula, family="gaussian", data=data.predict, control.compute=list(dic=TRUE, mlik=TRUE), control.fixed=list(mean.intercept=0, prec.intercept=0.0001), quantiles=c(.025,0.95))
    
    res.para<- Y[time.predict]-model.para$summary.fitted[time.predict,1]
    res.rw1 <- Y[time.predict]-model.rw1$summary.fitted[time.predict,1]
    res.rw2 <- Y[time.predict]-model.rw2$summary.fitted[time.predict,1]
    
    diff.para<- Y[time.predict_p]-model.para$summary.fitted[time.predict_p,1]
    diff.rw1 <- Y[time.predict_p]-model.rw1$summary.fitted[time.predict_p,1]
    diff.rw2 <- Y[time.predict_p]-model.rw2$summary.fitted[time.predict_p,1]
    
    
    ## melting   
    res.all<-data.frame(RW1=res.rw1, RW2=res.rw2, PRM=res.para)
    res.all_m<-melt(res.all)
    res.all_g<-cbind(res.all_m, id=rep(1:n, 3))
    res.h<-data.frame(RW1=diff.rw1, RW2=diff.rw2, PRM=diff.para)
    res.h_m<-melt(res.h)
    res.h_g<-cbind(res.h_m, id=rep(1:h, 3))
    
    
    
    #R Cookbook mulitplot function
    multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
      require(grid)
      
      # Make a list from the ... arguments and plotlist
      plots <- c(list(...), plotlist)
      
      numPlots = length(plots)
      
      if (is.null(layout)) {
        layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                         ncol = cols, nrow = ceiling(numPlots/cols))
      }
      
      if (numPlots==1) {
        print(plots[[1]])
        
      } else {
        # Set up the page
        grid.newpage()
        pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
        
        # Make each plot, in the correct location
        for (i in 1:numPlots) {
          # Get the i,j matrix positions of the regions that contain this subplot
          matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
          
          print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                          layout.pos.col = matchidx$col))
        }
      }
    }
    
    
    #http://files.meetup.com/1696476/ACFinGGPLOT2Presentation.pdf
    qacf <- function(x, conf.level = 0.95, max.lag = NULL, min.lag = 0, title = "") {
      ciline <- qnorm((1 - conf.level)/2)/sqrt(length(x))
      bacf <- acf(x, plot = FALSE, lag.max = max.lag)
      bacfdf <- with(bacf, data.frame(lag, acf))
      if (min.lag > 0) {
        bacfdf <- bacfdf[-seq(1, min.lag), ]
      }
      significant <- (abs(bacfdf[, 2]) > abs(ciline))^2
      bacfdf <- cbind(bacfdf, significant)
      q <- qplot(lag, acf, data = bacfdf, geom = "bar", stat = "identity",
                 position = "identity", main = title,
                 fill = factor(significant))
      q <- q + scale_fill_manual(name = paste("Significant at", 0.95, "level"), breaks = 0:1, labels = c("False", "True"), values=c( "gold", "grey60"))
      q <- q + geom_hline(yintercept = -ciline, color = "blue", size = 0.2)
      q <- q + geom_hline(yintercept = ciline, color = "blue", size = 0.2)
      q <- q + geom_hline(yintercept = 0, color = "deeppink1", size = 0.3)
      
       
      return(q)
    }
    
    g1<-qacf(x=res.all_g$value[res.all_g$variable=="RW1"], title="RW1") + theme(legend.position="none", axis.title.x = element_blank()) + ylab("Autocorrelation")
    g2<-qacf(x=res.all_g$value[res.all_g$variable=="RW2"], title="RW2") + theme(legend.position="none", axis.title.y=element_blank()) + xlab("Lag")
    g3<-qacf(x=res.all_g$value[res.all_g$variable=="PRM"], title="PRM") + theme(legend.justification=c(1,1), legend.position=c(1,1), axis.title.x = element_blank(), axis.title.y=element_blank()) #R-Cookbook legend pos
    
    #density plot on istogram requncy scalehttps://stat.ethz.ch/pipermail/r-help/2011-June/280588.html
    g4<- ggplot(res.all_g, aes(x=value[variable=="RW1"])) + geom_histogram(binwidth=.7, alpha=.4, fill="orangered") + geom_density(aes(y=0.7*..count..), colour="grey40") +  theme(legend.position="none", axis.title.x = element_blank()) + ylab("Frequency") + ggtitle("RW1")
    g5<- ggplot(res.all_g, aes(x=value[variable=="RW2"])) + geom_histogram(binwidth=.7, alpha=.4, fill="green3") + geom_density(aes(y=0.7*..count..), colour="grey40") + theme(legend.position="none", axis.title.y = element_blank()) + xlab("Residuals") + ggtitle("RW2")
    g6<- ggplot(res.all_g, aes(x=value[variable=="PRM"])) + geom_histogram(binwidth=.7, alpha=.6, fill="cornflowerblue") + geom_density(aes(y=0.7*..count..), colour="grey40") + theme(legend.position="none", axis.title.x = element_blank(), axis.title.y = element_blank()) +  ggtitle("PRM")
    #g4<-qacf(x=res.h_g$value[res.h_g$variable=="RW1"], title="RW1 h") + theme(legend.position="none", axis.title.x = element_blank()) + ylab("Autocorrelation")
    #g5<-qacf(x=res.h_g$value[res.h_g$variable=="RW2"], title="RW2 h") + theme(legend.position="none", axis.title.y=element_blank()) + xlab("Lab")
    #g6<-qacf(x=res.h_g$value[res.h_g$variable=="PRM"], title="PRM h") + theme(legend.justification=c(1,1), legend.position=c(1,1), axis.title.x = element_blank(), axis.title.y=element_blank()) #R-Cookbook legend pos
    
    
    multiplot(g4 ,g1, g5, g2, g6, g3, cols=3)
  })
  
  
  


    output$dic_crit <- reactiveTable(function() {
      func_type <- input$func_type
      f_type <- input$f_type
      sim_func <- input$sim_func
      harm_func <- input$harm_fit
      h <- input$h
      b <- input$b
      #b <- 1597
      
      
      userchoice<-eval(parse(text=input$userchoice))
      data.orig<-cbind(data.orig, userchoice)
      
      Y <- data.orig[b:2192, ifelse(func_type=="d_set", 1 , ifelse(sim_func=="linear function", 2, ifelse(sim_func=="sine function", 3, ifelse(sim_func=="cosine function", 4, ifelse(sim_func=="polynomial", 5, ifelse(sim_func=="non-linear type1", 6, ifelse(sim_func=="non-linear type2", 7, 8) ))))))]
      n <- length(Y)
      Y.predict<-c(Y[1:(length(Y)-h)],rep(NA,h)) 
      #Y.predict <- c(Y, rep(NA, h))
      n.predict <- length(Y.predict)
      time.predict <- 1:n.predict
      time.hor<- 1: (n.predict-h)
      time.predict_p <- (n.predict-h+1):n.predict
      
        
      if(harm_func == "auto"){
        
        sin.year.predict  <- cos.year.predict  <- matrix(nr=length(time.predict), nc=4)
        for (i in 1:4){ 
          cos.year.predict[,i] <- cos(2*pi*i*time.predict/365)
          sin.year.predict[,i] <- sin(2*pi*i*time.predict/365) 
        }
        
      } else {
        
        sin.year.predict <- eval(parse(text=input$para_sin_func))
        cos.year.predict <- eval(parse(text=input$para_cos_func))
        
      }
        data.predict <- data.frame(Y.predict=Y.predict, time.predict=time.predict, sin.year.predict=sin.year.predict, cos.year.predict=cos.year.predict)
        formula <- Y.predict ~ time.predict + sin.year.predict + cos.year.predict
        model.para <- inla(formula=formula, family="gaussian", data=data.predict, control.compute=list(dic=TRUE, mlik=TRUE), control.fixed=list(mean.intercept=0, prec.intercept=0.0001), quantiles=c(.025,0.95))
        
        data.predict <- data.frame(Y.predict=Y.predict, time.predict=time.predict)
        formula <- Y.predict ~ f(time.predict, model="rw1", cyclic=FALSE)
        model.rw1 <- inla(formula=formula, family="gaussian", data=data.predict, control.compute=list(dic=TRUE, mlik=TRUE), control.fixed=list(mean.intercept=0, prec.intercept=0.0001), quantiles=c(.025,0.95))
        
        data.predict <- data.frame(Y.predict=Y.predict, time.predict=time.predict)
        formula <- Y.predict ~ f(time.predict, model="rw2", cyclic=FALSE)
        model.rw2 <- inla(formula=formula, family="gaussian", data=data.predict, control.compute=list(dic=TRUE, mlik=TRUE), control.fixed=list(mean.intercept=0, prec.intercept=0.0001), quantiles=c(.025,0.95))
        
        data.frame(RW1=model.rw1$dic$dic, RW2=model.rw2$dic$dic , PRM=model.para$dic$dic)
      
    })
  
  
  
  output$prediction_acf.facets <- reactivePlot(function() {
    func_type <- input$func_type
    f_type <- input$f_type
    sim_func <- input$sim_func
    harm_func <- input$harm_fit
    h <- input$h
    b <- input$b
    #b <- 1597
    
    
    userchoice<-eval(parse(text=input$userchoice))
    data.orig<-cbind(data.orig, userchoice)
    
    Y <- data.orig[b:2192, ifelse(func_type=="d_set", 1 , ifelse(sim_func=="linear function", 2, ifelse(sim_func=="sine function", 3, ifelse(sim_func=="cosine function", 4, ifelse(sim_func=="polynomial", 5, ifelse(sim_func=="non-linear type1", 6, ifelse(sim_func=="non-linear type2", 7, 8) ))))))]
    n <- length(Y)
    Y.predict<-c(Y[1:(length(Y)-h)],rep(NA,h)) 
    #Y.predict <- c(Y, rep(NA, h))
    n.predict <- length(Y.predict)
    time.predict <- 1:n.predict
    time.predict_p <- (n.predict-h+1):n.predict
    
    
    if(harm_func == "auto"){
      
      sin.year.predict  <- cos.year.predict  <- matrix(nr=length(time.predict), nc=4)
      for (i in 1:4){ 
        cos.year.predict[,i] <- cos(2*pi*i*time.predict/365)
        sin.year.predict[,i] <- sin(2*pi*i*time.predict/365) 
      }
      
    } else {
      
      sin.year.predict <- eval(parse(text=input$para_sin_func))
      cos.year.predict <- eval(parse(text=input$para_cos_func))
      
    }
    
    data.predict <- data.frame(Y.predict=Y.predict, time.predict=time.predict, sin.year.predict=sin.year.predict, cos.year.predict=cos.year.predict)
    formula <- Y.predict ~ time.predict + sin.year.predict + cos.year.predict
    model.para <- inla(formula=formula, family="gaussian", data=data.predict, control.compute=list(dic=TRUE, mlik=TRUE), control.fixed=list(mean.intercept=0, prec.intercept=0.0001), quantiles=c(.025,0.95))
    
    data.predict <- data.frame(Y.predict=Y.predict, time.predict=time.predict)
    formula <- Y.predict ~ f(time.predict, model="rw1", cyclic=FALSE)
    model.rw1 <- inla(formula=formula, family="gaussian", data=data.predict, control.compute=list(dic=TRUE, mlik=TRUE), control.fixed=list(mean.intercept=0, prec.intercept=0.0001), quantiles=c(.025,0.95))
    
    data.predict <- data.frame(Y.predict=Y.predict, time.predict=time.predict)
    formula <- Y.predict ~ f(time.predict, model="rw2", cyclic=FALSE)
    model.rw2 <- inla(formula=formula, family="gaussian", data=data.predict, control.compute=list(dic=TRUE, mlik=TRUE), control.fixed=list(mean.intercept=0, prec.intercept=0.0001), quantiles=c(.025,0.95))

    ## melting   
    model.all<-data.frame(RW1=model.rw1$summary.fitted.values[,1], RW2=model.rw2$summary.fitted.values[,1], PRM=model.para$summary.fitted.values[,1])
    model.all_m<-melt(model.all)
    model.all_g<-cbind(model.all_m, id=rep(1:n, 3))
    
    
    #R Cookbook mulitplot function
    multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
      require(grid)
      
      # Make a list from the ... arguments and plotlist
      plots <- c(list(...), plotlist)
      
      numPlots = length(plots)
      
      if (is.null(layout)) {
        layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                         ncol = cols, nrow = ceiling(numPlots/cols))
      }
      
      if (numPlots==1) {
        print(plots[[1]])
        
      } else {
        # Set up the page
        grid.newpage()
        pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
        
        # Make each plot, in the correct location
        for (i in 1:numPlots) {
          # Get the i,j matrix positions of the regions that contain this subplot
          matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
          
          print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                          layout.pos.col = matchidx$col))
        }
      }
    }
    
    
     
    #density plot on istogram frequncy scale https://stat.ethz.ch/pipermail/r-help/2011-June/280588.html
    g4<- ggplot(model.all_g, aes(value[x=variable=="RW1"])) + geom_histogram(binwidth=.7, alpha=.4, fill="orangered") + geom_density(aes(y=0.7*..count..), colour="grey40") +  theme(legend.position="none") + xlab("RW1") + ylab("Frequency")
    g5<- ggplot(model.all_g, aes(value[x=variable=="RW2"])) + geom_histogram(binwidth=.7, alpha=.4, fill="green3") + geom_density(aes(y=0.7*..count..), colour="grey40") + theme(legend.position="none") + xlab("RW2") + ylab("Frequency")
    g6<- ggplot(model.all_g, aes(value[x=variable=="PRM"])) + geom_histogram(binwidth=.7, alpha=.6, fill="cornflowerblue") + geom_density(aes(y=0.7*..count..), colour="grey40") + theme(legend.position="none") + xlab("PRM") + ylab("Frequency")
    
    multiplot( g4, g5, g6)
  })
  
  
  
  output$prediction.all_facets <- reactivePlot(function(){
    func_type <- input$func_type
    f_type <- input$f_type
    sim_func <- input$sim_func
    harm_func <- input$harm_fit
    h <- input$h
    b <- input$b
    #b <- 1597
     
    userchoice<-eval(parse(text=input$userchoice))
    data.orig<-cbind(data.orig, userchoice)
    
    Y <- data.orig[b:2192, ifelse(func_type=="d_set", 1 , ifelse(sim_func=="linear function", 2, ifelse(sim_func=="sine function", 3, ifelse(sim_func=="cosine function", 4, ifelse(sim_func=="polynomial", 5, ifelse(sim_func=="non-linear type1", 6, ifelse(sim_func=="non-linear type2", 7, 8) ))))))]
    n <- length(Y)
    Y.predict<-c(Y[1:(length(Y)-h)],rep(NA,h)) 
    #Y.predict <- c(Y, rep(NA, h))
    n.predict <- length(Y.predict)
    time.predict <- 1:n.predict
    time.hor<- 1: (n.predict-h)
    time.predict_p <- (n.predict-h+1):n.predict
    
    #harmonics fitting
    if(harm_func == "auto"){
      
      sin.year.predict  <- cos.year.predict  <- matrix(nr=length(time.predict), nc=4)
      for (i in 1:4){ 
        cos.year.predict[,i] <- cos(2*pi*i*time.predict/365)
        sin.year.predict[,i] <- sin(2*pi*i*time.predict/365) 
      }
      
    } else {
      
      sin.year.predict <- eval(parse(text=input$para_sin_func))
      cos.year.predict <- eval(parse(text=input$para_cos_func))
      
    }
    data.predict <- data.frame(Y.predict=Y.predict, time.predict=time.predict, sin.year.predict=sin.year.predict, cos.year.predict=cos.year.predict)
    formula <- Y.predict ~ time.predict + sin.year.predict + cos.year.predict
    model.para<- inla(formula=formula, family="gaussian", data=data.predict, control.compute=list(dic=TRUE, mlik=TRUE), control.fixed=list(mean.intercept=0, prec.intercept=0.0001), quantiles=c(.025, .1, .9, .975))
    
    data.predict <- data.frame(Y.predict=Y.predict, time.predict=time.predict)
    formula <- Y.predict ~ f(time.predict, model="rw1", cyclic=FALSE)
    model.rw1<- inla(formula=formula, family="gaussian", data=data.predict, control.compute=list(dic=TRUE, mlik=TRUE), control.fixed=list(mean.intercept=0, prec.intercept=0.0001), quantiles=c(.025, .1, .9, .975))
    
    data.predict <- data.frame(Y.predict=Y.predict, time.predict=time.predict)
    formula <- Y.predict ~ f(time.predict, model="rw2", cyclic=FALSE)
    model.rw2<- inla(formula=formula, family="gaussian", data=data.predict, control.compute=list(dic=TRUE, mlik=TRUE), control.fixed=list(mean.intercept=0, prec.intercept=0.0001), quantiles=c(.025, .1, .9, .975))
    
    data.all<-data.frame(RW1=Y, RW2=Y, PRM=Y)
    data.all_m<-melt(data.all)
    
    model.all<-data.frame(RW1=model.rw1$summary.fitted.values[,1], RW2=model.rw2$summary.fitted.values[,1], PRM=model.para$summary.fitted.values[,1])
    model.all_m<-melt(model.all)
   
    ci95.lower<-data.frame(RW1=model.rw1$summary.fitted[,3], RW2=model.rw2$summary.fitted[,3], PRM=model.para$summary.fitted[,3])
    ci80.lower<-data.frame(RW1=model.rw1$summary.fitted[,4], RW2=model.rw2$summary.fitted[,4], PRM=model.para$summary.fitted[,4])
    ci80.upper<-data.frame(RW1=model.rw1$summary.fitted[,5], RW2=model.rw2$summary.fitted[,5], PRM=model.para$summary.fitted[,5])
    ci95.upper<-data.frame(RW1=model.rw1$summary.fitted[,6], RW2=model.rw2$summary.fitted[,6], PRM=model.para$summary.fitted[,6])
    
    ci95.lower_m<-melt(ci95.lower)
    ci80.lower_m<-melt(ci80.lower)
    ci80.upper_m<-melt(ci80.upper)
    ci95.upper_m<-melt(ci95.upper)
    
    model.all_g<-cbind(data=data.all_m, model=model.all_m[,2], lower95=ci95.lower_m[,2], lower80=ci80.lower_m[,2], upper80=ci80.upper_m[,2], upper95=ci95.upper_m[,2], id=rep(1:n, 3))
    
      g <- ggplot(model.all_g, aes(id, model, colour=data.variable)) + geom_line(size=1.2) + geom_point(aes(id, data.value), colour="grey70", alpha=.2) + 
            geom_line(aes(id, lower95), colour="white", linetype="longdash", size=1) +
            geom_line(aes(id, upper95), colour="white", linetype="longdash", size=1) +
            #geom_line(aes(id, lower80), colour="lightskyblue", linetype="twodash", size=1) +
            #geom_line(aes(id, upper80), colour="lightskyblue", linetype="twodash", size=1) +
            facet_grid(. ~ data.variable) + theme(legend.position="none") + xlab("Time") + ylab("Data Values") 
     print(g)
    
    })
  
  
  output$dic.all_crit <- reactivePlot(function() {
    func_type <- input$func_type
    f_type <- input$f_type
    sim_func <- input$sim_func
    harm_func <- inout$harm_fit
    #h <- input$h
    #b <- input$b
    #b <- 1597
    
    #dic<-matrix(NA, nrow=100, ncol=3)
    data.all<-data.frame()
    data.all<-data.frame(matrix(NA, nrow = 6*7, ncol = 5))
    dic.rw1 <- dic.rw2 <- dic.para <- matrix(NA, nrow=6, ncol=7)
   
    #for(h in 1:nrow(dic)){
    for(j in 1: 6){
      for(i in 1:7){
        
        b<-c(700, 1000, 1300, 1500, 1800, 2100)
        h<-c(1, 5, 10, 20, 50, 80, 100)
        
    Y <- data.orig[b[j]:2192, 2] #ifelse(func_type=="d_set", 1 , ifelse(sim_func=="sim1", 2, ifelse(sim_func=="sim2", 3, ifelse(sim_func=="sim3", 4, ifelse(sim_func=="sim5", 5, 6 )))))]
    n <- length(Y)
    Y.predict<-c(Y[1:(length(Y)-h[i])],rep(NA,h[i])) 
    #Y.predict <- c(Y, rep(NA, h))
    n.predict <- length(Y.predict)
    time.predict <- 1:n.predict
    time.predict_p <- (n.predict-h[i]+1):n.predict
   
        if(harm_func == "auto"){
          
          sin.year.predict  <- cos.year.predict  <- matrix(nr=length(time.predict), nc=4)
          for (i in 1:4){ 
            cos.year.predict[,i] <- cos(2*pi*i*time.predict/365)
            sin.year.predict[,i] <- sin(2*pi*i*time.predict/365) 
          }
          
        } else {
          
          sin.year.predict <- eval(parse(text=input$para_sin_func))
          cos.year.predict <- eval(parse(text=input$para_cos_func))
          
        }  
    data.predict <- data.frame(Y.predict=Y.predict, time.predict=time.predict, sin.year.predict=sin.year.predict, cos.year.predict=cos.year.predict)
    formula <- Y.predict ~ time.predict + sin.year.predict + cos.year.predict
    model.para <- inla(formula=formula, family="gaussian", data=data.predict, control.compute=list(dic=TRUE, mlik=TRUE), control.fixed=list(mean.intercept=0, prec.intercept=0.0001), quantiles=c(.025,0.95))
    
    data.predict <- data.frame(Y.predict=Y.predict, time.predict=time.predict)
    formula <- Y.predict ~ f(time.predict, model="rw1", cyclic=FALSE)
    model.rw1 <- inla(formula=formula, family="gaussian", data=data.predict, control.compute=list(dic=TRUE, mlik=TRUE), control.fixed=list(mean.intercept=0, prec.intercept=0.0001), quantiles=c(.025,0.95))
    
    data.predict <- data.frame(Y.predict=Y.predict, time.predict=time.predict)
    formula <- Y.predict ~ f(time.predict, model="rw2", cyclic=FALSE)
    model.rw2 <- inla(formula=formula, family="gaussian", data=data.predict, control.compute=list(dic=TRUE, mlik=TRUE), control.fixed=list(mean.intercept=0, prec.intercept=0.0001), quantiles=c(.025,0.95))
    
    #dic[i,1] <- model.rw1$dic$dic
    #dic[i,2] <- model.rw2$dic$dic 
    #dic[i,3] <- model.para$dic$dic
        
        dic.rw1[j,i] <- model.rw1$dic$dic
        dic.rw2[j,i] <- model.rw2$dic$dic 
        dic.para[j,i] <- model.para$dic$dic
        
        data.all[i*j, ]<-data.frame(b=b[j], h=h[i], RW1=dic.rw1[j,i], RW2=dic.rw2[j,i], RW3=dic.para[j,i])
    }
  }
    
    
    data<-data.all[complete.cases(data.all),]
    sort.data<-data[order(data$X1),]
    
    data.frame(StartingValue=sort.data[,1], Horizon=sort.data[,2], DIC.RW1=sort.data[,3], DIC.RW2=sort.data[,4], DIC.PRM=sort.data[,5])
    
    
    multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
      require(grid)
      
      # Make a list from the ... arguments and plotlist
      plots <- c(list(...), plotlist)
      
      numPlots = length(plots)
      
      if (is.null(layout)) {
        layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                         ncol = cols, nrow = ceiling(numPlots/cols))
      }
      
      if (numPlots==1) {
        print(plots[[1]])
        
      } else {
        # Set up the page
        grid.newpage()
        pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
        
        # Make each plot, in the correct location
        for (i in 1:numPlots) {
          # Get the i,j matrix positions of the regions that contain this subplot
          matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
          
          print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                          layout.pos.col = matchidx$col))
        }
      }
    }
    
    
   # dic.all<-data.frame(RW1=dic[,1], RW2=dic[,2] , PRM=dic[,3])
   # dic.all_m<-melt(dic.all)
   # dic.all_g<-cbind(dic.all_m, id=rep(1: nrow(dic), 3))
    
  
    #g<- ggplot(dic.all_g, aes(id, value, colour=variable)) + geom_line() + 
    # ylab("DIC") + xlab("Horizon values") + geom_text(data=dic.all_g[dic.all_g$id==100,], aes(label= variable), hjust=0, vjust=0, size=3) #+ theme(legend.position="none")
    #print(g)
    
    #g1<-ggplot(data, aes(x=X1, y=X2, size=X3),legend=FALSE)+
    #  geom_point(colour="white", fill="deeppink2", alpha=.7, shape=21)+ scale_size(range=c(1,15))+
    #  scale_x_continuous(name="Dataset starting point", limits=c(600,2100))+ 
    #  scale_y_continuous(name="Horizon", limits=c(-20,120))
    
    #g2<-ggplot(data, aes(x=X1, y=X2, size=X4),legend=FALSE)+
    #  geom_point(colour="white", fill="green3", alpha=.7, shape=21)+ scale_size(range=c(1,15))+
    #  scale_x_continuous(name="Dataset starting point", limits=c(600,2100))+
    #  scale_y_continuous(name="Horizon", limits=c(-20,120))
    
    #g3<-ggplot(data, aes(x=X1, y=X2, size=X5),legend=FALSE)+
    #  geom_point(colour="white", fill="cornflowerblue", alpha=.7, shape=21)+ scale_size(range=c(1,15))+
    #  scale_x_continuous(name="Dataset starting point", limits=c(600,2100))+
    #  scale_y_continuous(name="Horizon", limits=c(-20,120))
    
   # multiplot(g, g1, g2, g3)
                
  })
  

})

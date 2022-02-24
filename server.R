##################################################
#####
##### Age and Growth app - Server
##### Jonas Vasconcelos-Filho
##### 09/06/2019
##### v 1.0
#####
##################################################

check.packages <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
}

# Usage example
packages<-c("shiny", "FSA", "fishmethods", "AICcmodavg", 
            "nlstools", "shinydashboard", "shinyWidgets")


if(!any(check.packages(packages)==F)){
  shinyServer(function(session,input, output) {
     
    data <- reactive({ 
      file1 <- input$file
      if(is.null(file1)) {return()}
      read.table(file = file1$datapath,
                 sep = input$sep,
                 header = input$header,
                 stringsAsFactors = input$stringAsFactors)
      
    })
    
    observeEvent(input$modelsvbgf, {
      if(input$modelsvbgf == F){
        shinyjs::disable("vbgfL")
        shinyjs::disable("vbgfK")
        shinyjs::disable("vbgft0")
      } else {
        shinyjs::enable("vbgfL")
        shinyjs::enable("vbgfK")
        shinyjs::enable("vbgft0")
      }
    })
    
    observeEvent(input$modelsgomp, {
      if(input$modelsgomp == F){
        shinyjs::disable("gompL")
        shinyjs::disable("gompgi")
        shinyjs::disable("gompti")
      } else {
        shinyjs::enable("gompL")
        shinyjs::enable("gompgi")
        shinyjs::enable("gompti")
      }
    })
    
    observeEvent(input$modelslog, {
      if(input$modelslog == F){
        shinyjs::disable("logL")
        shinyjs::disable("logginf")
        shinyjs::disable("logti")
      } else {
        shinyjs::enable("logL")
        shinyjs::enable("logginf")
        shinyjs::enable("logti")
      }
    })
    
    observeEvent(input$boot, {
      if(input$boot == F){
        shinyjs::disable("nboot")
      } else {
        shinyjs::enable("nboot")
      }
    })
    
    Linfvbgf<-reactive({
      as.numeric(input$vbgfL)
    })
    K<-reactive({
      as.numeric(input$vbgfK)
    })
    t0<-reactive({
      as.numeric(input$vbgft0)
    })
    
    Linfgom<-reactive({
      as.numeric(input$gompL)
    })
    gi<-reactive({
      as.numeric(input$gompgi)
    })
    ti<-reactive({
      as.numeric(input$gompti)
    })
    
    Linflog<-reactive({
      as.numeric(input$logL)
    })
    ginf<-reactive({
      as.numeric(input$logginf)
    })
    tilog<-reactive({
      as.numeric(input$logti)
    })
    
    
    x <- reactive({
      as.numeric(input$x)
    })
    
    y <- reactive({
      as.numeric(input$y)
    })
    
    observe({
      if(input$modelsvbgf == F){return()}
      else{
        dados<-na.omit(data())
        age <- dados[,x()]
        tl <- dados[,y()]
        
        Seeds<-vbStarts(tl~age,data=dados,type="typical")
        
        # Control the value, min, max, and step.
        # Step size is 2 when input value is even; 1 when value is odd.
        updateSliderInput(session, "vbgfL", value = Seeds$Linf,
                          min = round(Seeds$Linf/5), max = round(Seeds$Linf*5), 
                          step = 1)
      }
    })
    
    observe({
      if(input$modelsvbgf == F){return()}
      else{
        dados<-na.omit(data())
        age <- dados[,x()]
        tl <- dados[,y()]
        
        Seeds<-vbStarts(tl~age,data=dados,type="typical")
        
        # Control the value, min, max, and step.
        # Step size is 2 when input value is even; 1 when value is odd.
        updateSliderInput(session, "vbgfK", value = Seeds$K,
                          min = 0.001, max = round(Seeds$K*5,2), 
                          step = 0.001)
      }
    })
    
    observe({
      if(input$modelsvbgf == F){return()}
      else{
        dados<-na.omit(data())
        age <- dados[,x()]
        tl <- dados[,y()]
        
        Seeds<-vbStarts(tl~age,data=dados,type="typical")
        
        # Control the value, min, max, and step.
        # Step size is 2 when input value is even; 1 when value is odd.
        updateSliderInput(session, "vbgft0", value = Seeds$t0,
                          min = -abs(round(Seeds$t0*5,2)), max = abs(round(Seeds$t0*5,2)), 
                          step = 0.001)
      }
    })
    ###
    observe({
      if(input$modelsgomp == F){return()}
      else{
        dados<-na.omit(data())
        age <- dados[,x()]
        tl <- dados[,y()]
        
        Seeds<-vbStarts(tl~age,data=dados,type="typical")
        
        # Control the value, min, max, and step.
        # Step size is 2 when input value is even; 1 when value is odd.
        updateSliderInput(session, "gompL", value = Seeds$Linf,
                          min = round(Seeds$Linf/5), max = round(Seeds$Linf*5), 
                          step = 1)
      }
    })
    
    observe({
      if(input$modelsgomp == F){return()}
      else{
        dados<-na.omit(data())
        age <- dados[,x()]
        tl <- dados[,y()]
        
        Seeds<-vbStarts(tl~age,data=dados,type="typical")
        
        # Control the value, min, max, and step.
        # Step size is 2 when input value is even; 1 when value is odd.
        updateSliderInput(session, "gompgi", value = Seeds$K,
                          min = 0.001, max = round(Seeds$K*5,2), 
                          step = 0.001)
      }
    })
    
    observe({
      if(input$modelsgomp == F){return()}
      else{
        dados<-na.omit(data())
        age <- dados[,x()]
        tl <- dados[,y()]
        
        Seeds<-vbStarts(tl~age,data=dados,type="typical")
        
        # Control the value, min, max, and step.
        # Step size is 2 when input value is even; 1 when value is odd.
        updateSliderInput(session, "gompti", value = Seeds$t0,
                          min = -abs(round(Seeds$t0*5,2)), max = abs(round(Seeds$t0*5,2)), 
                          step = 0.001)
      }
    })
    
    ###
    output$filedf <- renderTable({
      if(is.null(data())) {return()}
      data()
    })
    
    output$filedf2 <- renderTable({
      if(is.null(input$file)) {return()}
      input$file$datapath
    })
    
    output$fileob <- renderPrint({
      if(is.null(input$file)) {return()}
      str(data())
    })
    
    output$selectfile <- renderUI({
      if(is.null(input$file)) {return()}
      list(
           hr(),
           helpText("Select the files for which you need to see data and summary stats"),
           selectInput("Select", "Select", choices = input$file$name)
      )
    }) 
    
    output$summ <- renderPrint({
      if(is.null(input$file)) {return()}
      summary(data())
    })
    
    output$tableui <- renderUI({
      dataout <- data()
      output$dataout<-renderDataTable(dataout)
      dataTableOutput("dataout")
    })
    
    output$tb <- renderUI({
      if(is.null(input$file)) {return()}
      tabsetPanel(
                  tabPanel("Data",
                           uiOutput("tableui")),
                  
                  tabPanel("Structure",
                           verbatimTextOutput("fileob")),
                  
                  tabPanel("Summary",
                           verbatimTextOutput("summ"))
                    
                  )
      })
    
    output$table <-renderTable({
      if(is.null(input$file)) {return()}
      data()
    })
    
    pch<-reactive({
      as.numeric(input$pch)
    })
    
    output$plot <- renderPlot({
      dados<-na.omit(data())
      x<-dados[,x()]
      y<-dados[,y()]
      
      plot(x, y, 
           xlab = input$xlab,
           ylab = input$ylab,
           pch=pch(), col=rgb(0,0,0,1/4))
    })
  
    width <- reactive({
      as.numeric(input$width)
    })
    height <- reactive({
      as.numeric(input$height)
    })
    res <- reactive({
      as.numeric(input$res)
    })
  
    # checkboxGroupInput("columns", "Choose columns", 
    #                    choices  = colnames,
    #                    selected = colnames)
    
    # output$down <- downloadHandler(
    #   filename = function(){
    #     paste("", ".", input$download, sep="")
    #   },
    #   content = function(file){
    #     if(input$download=="tiff")
    #       tiff(file, width = width(), height = height(), res= res())
    #     else
    #       jpeg(file, width = width(), height = height(), res= res())
    #     
    # 
    #     dados<-na.omit(data())
    #     x<-dados[,x()]
    #     y<-dados[,y()]
    #     
    #     plot(x, y, 
    #          xlab = input$xlab,
    #          ylab = input$ylab,
    #          pch=pch(), col=rgb(0,0,0,1/4))
    #     dev.off()
    #   }
    # )
    
    #Colocar como excluir pontos  https://www.google.com/search?q=how+to+add+or+remove+point+in+shiny&rlz=1C1EJFC_enBR835BR835&oq=how+to+add+or+remove+point+in+shiny&aqs=chrome..69i57j33l5.202362j1j7&sourceid=chrome&ie=UTF-8#kpvalbx=1
    
    # data3<-input
    # data2<-data3()
    
    # observeEvent(y(), {
    #   updateSliderInput(session, "vbgfL", max=max(y2), min = min(y2), value = mean(y2))
    # })
    
    bootN<-reactive({
      as.numeric(input$nboot)
    })
    
    output$plot2 <- renderPlot({
      dados<-na.omit(data())
      x<-dados[,x()]
      y<-dados[,y()]
      
      plot(x, y, 
           xlab = input$xlab,
           ylab = input$ylab,
           pch=pch(), col=rgb(0,0,0,1/4))
      
      #vbgf
      x<-seq(min(x, na.rm = T),max(x, na.rm = T), 0.1)
      if(input$modelsvbgf == T){
        curve(Linfvbgf() * (1 - exp(-K() * (x - t0()))), 
            from = min(x, na.rm = T), to = max(x, na.rm = T),
            lwd = 2, col="black", add=T)
      }
      
      #gomp
      if(input$modelsgomp == T){
      curve(Linfgom() * exp(-exp(-gi() * (x - ti()))), 
            from = min(x, na.rm = T), to = max(x, na.rm = T),
            lwd = 2, col="red", add=T)
      }
      
      #log
      if(input$modelslog == T){
      curve( Linflog()/(1 + exp(-ginf() * (x - tilog()))), 
            from = min(x, na.rm = T), to = max(x, na.rm = T),
            lwd = 2, col="blue", add=T)
      }
    })
  
    output$plot3 <- renderPlot({
      dados<-na.omit(data())
      x<-dados[,x()]
      y<-dados[,y()]
      
      plot(x, y, 
           xlab = input$xlab,
           ylab = input$ylab,
           pch=pch(), col=rgb(0,0,0,1/4))
      
      #vbgf
      x<-seq(min(x, na.rm = T),max(x, na.rm = T), 0.1)
      if(input$modelsvbgf == T){
        curve(Linfvbgf() * (1 - exp(-K() * (x - t0()))), 
              from = min(x, na.rm = T), to = max(x, na.rm = T),
              lwd = 2, col="black", add=T)
      }
      
      #gomp
      if(input$modelsgomp == T){
        curve(Linfgom() * exp(-exp(-gi() * (x - ti()))), 
              from = min(x, na.rm = T), to = max(x, na.rm = T),
              lwd = 2, col="red", add=T)
      }
      
      #log
      if(input$modelslog == T){
        curve( Linflog()/(1 + exp(-ginf() * (x - tilog()))), 
               from = min(x, na.rm = T), to = max(x, na.rm = T),
               lwd = 2, col="blue", add=T)
      }
    })
    
    output$plot4 <- renderPlot({
      dados<-na.omit(data())
      x<-dados[,x()]
      y<-dados[,y()]
      
      plot(x, y, 
           xlab = input$xlab,
           ylab = input$ylab,
           pch=pch(), col=rgb(0,0,0,1/4))
      
      #vbgf
      x<-seq(min(x, na.rm = T),max(x, na.rm = T), 0.1)
      if(input$modelsvbgf == T){
        curve(Linfvbgf() * (1 - exp(-K() * (x - t0()))), 
              from = min(x, na.rm = T), to = max(x, na.rm = T),
              lwd = 2, col="black", add=T)
      }
      
      #gomp
      if(input$modelsgomp == T){
        curve(Linfgom() * exp(-exp(-gi() * (x - ti()))), 
              from = min(x, na.rm = T), to = max(x, na.rm = T),
              lwd = 2, col="red", add=T)
      }
      
      #log
      if(input$modelslog == T){
        curve( Linflog()/(1 + exp(-ginf() * (x - tilog()))), 
               from = min(x, na.rm = T), to = max(x, na.rm = T),
               lwd = 2, col="blue", add=T)
      }
    })
    
    output$plot5 <- renderPlot({
      dados<-na.omit(data())
      x<-dados[,x()]
      y<-dados[,y()]
      
      plot(x, y, 
           xlab = input$xlab,
           ylab = input$ylab,
           pch=pch(), col=rgb(0,0,0,1/4))
      
      #vbgf
      x<-seq(min(x, na.rm = T),max(x, na.rm = T), 0.1)
      if(input$modelsvbgf == T){
        curve(Linfvbgf() * (1 - exp(-K() * (x - t0()))), 
              from = min(x, na.rm = T), to = max(x, na.rm = T),
              lwd = 2, col="black", add=T)
      }
      
      #gomp
      if(input$modelsgomp == T){
        curve(Linfgom() * exp(-exp(-gi() * (x - ti()))), 
              from = min(x, na.rm = T), to = max(x, na.rm = T),
              lwd = 2, col="red", add=T)
      }
      
      #log
      if(input$modelslog == T){
        curve( Linflog()/(1 + exp(-ginf() * (x - tilog()))), 
               from = min(x, na.rm = T), to = max(x, na.rm = T),
               lwd = 2, col="blue", add=T)
      }
    })
    
    modVBGF<-reactive({
      if(is.null(input$file)) {return()}
      
      v1 <- vbFuns(param="typical")
      
        dados<-data()
        Linf<-Linfvbgf()
        K<-K()
        t0<-t0()
        age<- dados[, x()]
        svv1f <- list(Linf=Linf, K=K, t0=t0)
  
        if(y()==2){
          tl<-dados[,y()]  
          fitv1f <- nls(tl~v1(age,Linf,K,t0),data=dados,start=svv1f)
        }
        if(y()==3){
          tw<-dados[,y()]  
          fitv1f <- nls(tw~v1(age,Linf,K,t0),data=dados,start=svv1f)
        }
        
        return(fitv1f)
      
    })
  
    output$modVBGF2 <- renderPrint({
      print(overview(modVBGF()))
    })
    
    
    # vbgf graf
    output$vbgfBoot2<-renderPlot({
      
      
      v1 <- vbFuns(param="typical")
      dados<-data()
      dados<-dados[which(!is.na(dados[,y()])),]
      dados<-dados[which(!is.na(dados[,x()])),]
      
      Linf<-Linfvbgf()
      K<-K()
      t0<-t0()
    
      age<- dados[, x()]
      
      if(y()==2){
        tl<-dados[,y()]
        fitv1f <- nls(tl ~ v1(age, Linf, K, t0), data = dados,
                      start = list(Linf=Linf, K=K, t0=t0))
        
        plot(tl~age, 
             pch=pch(), col=rgb(0,0,0,1/4),
             ylim=c(0,(max(tl)*1.2)),
             xlab=input$xlab,
             ylab=input$ylab)
      }
      
      
      if(y()==3){
        tw<-dados[,y()]
        fitv1f <- nls(tw ~ v1(age, Linf, K, t0), data = dados,
                      start = list(Linf=Linf, K=K, t0=t0))
        
        plot(tw~age, 
             pch=pch(), col=rgb(0,0,0,1/4),
             ylim=c(0,(max(tw)*1.2)),
             xlab=input$xlab,
             ylab=input$ylab)
      }
      
  
      
      x<-seq(min(age),max(age), 0.1)
      
      bootP<-matrix(NA, ncol = 3, nrow = bootN())
      nls.control(maxiter = 100, minFactor = 1/2048, tol = 1e-07)
      
      if(input$boot == T){
        if(y()==2){
          tl<-dados[,y()]
          
          for(i in 1:bootN()){
            NewSamplesLT<-dados[sample(1:nrow(dados),replace = T),] 
            try(fitv1fNew <- nls(tl ~ v1(age, Linf, K, t0), data = NewSamplesLT,
                             start = list(Linf=Linf, K=K, t0=t0)),silent = T)
            bootP[i,1]<-coef(fitv1fNew)[1]
            bootP[i,2]<-coef(fitv1fNew)[2]
            bootP[i,3]<-coef(fitv1fNew)[3]
            
            curve(bootP[i , 1] * (1 - exp(-bootP[i , 2] * (x - bootP[ i, 3]))), 
                  from = min(age), to = max(age),
                  lwd = 1, col=rgb(0,0,0,1/25), add=T)
          }
        }
          
          if(y()==3){
            tw<-dados[,y()]
            
            for(i in 1:bootN()){
              NewSamplesLT<-dados[sample(1:nrow(dados),replace = T),] 
              try(fitv1fNew <- nls(tw ~ v1(age, Linf, K, t0), data = NewSamplesLT,
                                   start = list(Linf=Linf, K=K, t0=t0)),silent = T)
              bootP[i,1]<-coef(fitv1fNew)[1]
              bootP[i,2]<-coef(fitv1fNew)[2]
              bootP[i,3]<-coef(fitv1fNew)[3]
              
              curve(bootP[i , 1] * (1 - exp(-bootP[i , 2] * (x - bootP[ i, 3]))), 
                    from = min(age), to = max(age),
                    lwd = 1, col=rgb(0,0,0,1/25), add=T)
            }
          }
        
        ###
        LCI <- UCI <- seq()
        idades <- seq(0,max(age), 0.05)
        for(i in 1:length(idades)){
          TL <- bootP[ , 1] * (1 - exp( - bootP[ , 2] *
                                          (idades[ i ] - bootP[ , 3])))
          LCI[ i ] <- quantile( TL, 0.025 , na.rm = T)
          UCI[ i ] <- quantile( TL, 0.975 , na.rm = T) 
        }
      }
  
      curve(coef(fitv1f)[1] * (1 - exp(-coef(fitv1f)[2] * (x - coef(fitv1f)[3]))), 
            from = min(age), to = max(age),
            lwd = 2, col="red", add=T)
      
      if(input$boot == T){
        lines(idades, LCI, lty=3, col="red", lwd=2)
        lines(idades, UCI, lty=3, col="red", lwd=2)
      }
    })
   
    ## gomp
    modGom<-reactive({
      if(is.null(input$file)) {return()}
      
      g1 <- GompertzFuns(param="Ricker1")
      
      dados<-data()
      dados<-dados[which(!is.na(dados[,y()])),]
      dados<-dados[which(!is.na(dados[,x()])),]
      
      Linf<-Linfgom()
      gi<-gi()
      ti<-ti()
      age<- dados[, x()]
      svG1f <- list(Linf=Linf, gi=gi, ti=ti)
      
      if(y()==2){
        tl<-dados[,y()]
        fitG1f <- nls(tl~g1(age,Linf,gi,ti),data=dados,start=svG1f)
      }
      if(y()==3){
        tw<-dados[,y()]
        fitG1f <- nls(tw~g1(age,Linf,gi,ti),data=dados,start=svG1f)
      }
      
      return(fitG1f)
  
    })
    
    output$modGom2 <- renderPrint({
      print(overview(modGom()))
    })
    
    # gom boot
    output$gomBoot<-renderPlot({  ### svG1f nao foi encontrado. resolver isso
      
      g1 <- GompertzFuns(param="Ricker1")
      
      dados<-data()
      dados<-dados[which(!is.na(dados[,y()])),]
      dados<-dados[which(!is.na(dados[,x()])),]
      
      Linf<-Linfgom()
      gi<-gi()
      ti<-ti()
      age<- dados[, x()]
      
      svG1f <- list(Linf=Linf, gi=gi, ti=ti)
      
      if(y()==2){
        tl<-dados[,y()]
        fitG1f <- nls(tl~g1(age,Linf,gi,ti),
                      data=dados, start=svG1f)
        plot(tl~age, 
             pch=pch(), col=rgb(0,0,0,1/4),
             ylim=c(0,(max(tl)*1.2)),
             xlab=input$xlab,
             ylab=input$ylab)
      }
      
      if(y()==3){
        tw<-dados[,y()]
        fitG1f <- nls(tw~g1(age,Linf,gi,ti),
                      data=dados, start=svG1f)
        plot(tw~age, 
             pch=pch(), col=rgb(0,0,0,1/4),
             ylim=c(0,(max(tw)*1.2)),
             xlab=input$xlab,
             ylab=input$ylab)
      }
      
      x<-seq(min(age),max(age), 0.1)
      
      if(input$boot == T){
        if(y()==3){
          bootP<-matrix(NA, ncol = 3, nrow = bootN())
          nls.control(maxiter = 100, minFactor = 1/2048, tol = 1e-07)
          
          for(i in 1:bootN()){
            NewSamplesLT<-dados[sample(1:nrow(dados),replace = T),] 
            try(fitv1fNew2 <- nls(tw ~ g1(age, Linf, gi, ti), data = NewSamplesLT,
                             start = list(Linf=Linf, gi=gi, ti=ti)), silent = T)
            bootP[i,1]<-coef(fitv1fNew2)[1]
            bootP[i,2]<-coef(fitv1fNew2)[2]
            bootP[i,3]<-coef(fitv1fNew2)[3]
            
            curve(bootP[i , 1] * exp( - exp(-bootP[i , 2] * (x - bootP[ i, 3]))), 
                  from = min(age), to = max(age),
                  lwd = 1, col=rgb(0,0,0,1/25), add=T)
          }
        
          ###
          LCI <- UCI <- seq()
          idades <- seq(0,max(age), 0.05)
          for(i in 1:length(idades)){
            TL <- bootP[ , 1] * exp( - exp( - bootP[ , 2] *
                                            (idades[ i ] - bootP[ , 3])))
            LCI[ i ] <- quantile( TL, 0.025 , na.rm = T)
            UCI[ i ] <- quantile( TL, 0.975 , na.rm = T) 
          }
        }
        
        if(y()==2){
          bootP<-matrix(NA, ncol = 3, nrow = bootN())
          nls.control(maxiter = 100, minFactor = 1/2048, tol = 1e-07)
          
          for(i in 1:bootN()){
            NewSamplesLT<-dados[sample(1:nrow(dados),replace = T),] 
            try(fitv1fNew2 <- nls(tl ~ g1(age, Linf, gi, ti), data = NewSamplesLT,
                                  start = list(Linf=Linf, gi=gi, ti=ti)), silent = T)
            bootP[i,1]<-coef(fitv1fNew2)[1]
            bootP[i,2]<-coef(fitv1fNew2)[2]
            bootP[i,3]<-coef(fitv1fNew2)[3]
            
            curve(bootP[i , 1] * exp( - exp(-bootP[i , 2] * (x - bootP[ i, 3]))), 
                  from = min(age), to = max(age),
                  lwd = 1, col=rgb(0,0,0,1/25), add=T)
          }
          
          ###
          LCI <- UCI <- seq()
          idades <- seq(0,max(age), 0.05)
          for(i in 1:length(idades)){
            TL <- bootP[ , 1] * exp( - exp( - bootP[ , 2] *
                                              (idades[ i ] - bootP[ , 3])))
            LCI[ i ] <- quantile( TL, 0.025 , na.rm = T)
            UCI[ i ] <- quantile( TL, 0.975 , na.rm = T) 
          }
        }
      }
      
      curve(coef(fitG1f)[1] * exp( - exp(-coef(fitG1f)[2] * (x - coef(fitG1f)[3]))), 
            from = min(age), to = max(age),
            lwd = 2, col="red", add=T)
      if(input$boot == T){
        lines(idades, LCI, lty=3, col="red", lwd=2)
        lines(idades, UCI, lty=3, col="red", lwd=2)
      }
    })
    
    ## log
    modLog<-reactive({
      if(is.null(input$file)) {return()}
      
      l1 <- logisticFuns(param = "CJ1")
      
      dados<-data()
      Linflog<-Linflog()
      gninf<-ginf()
      tilog<-tilog()
      age<- dados[, x()]
      svl1f <- list(Linf=Linflog, gninf=gninf, ti=tilog)
      
      if(y()==2){
        tl<-dados[,y()]
        fitl1f <- nls(tl~l1(age, Linf, gninf, ti),data=dados,start=svl1f)
      }
      if(y()==3){
        tw<-dados[,y()]
        fitl1f <- nls(tw~l1(age, Linf, gninf, ti),data=dados,start=svl1f)
      }
      
      return(fitl1f)
      
    })
    
    output$modLog2 <- renderPrint({
      print(overview(modLog()))
    })
  
    output$logBoot2<-renderPlot({
      if(is.null(input$file)) {return()}
      
      l1 <- logisticFuns(param = "CJ1")
      
      dados<-na.omit(data())
      Linflog<-Linflog()
      gninf<-ginf()
      tilog<-tilog()
      age<- dados[, x()]
      svl1f <- list(Linf=Linflog, gninf=gninf, ti=tilog)
      
      if(y()==2){
        tl<-dados[,y()]
        fitl1f <- nls(tl~l1(age, Linf, gninf, ti),data=dados,start=svl1f)
        
        plot(tl~age, 
             pch=pch(), col=rgb(0,0,0,1/4),
             ylim=c(0,(max(tl)*1.2)),
             xlab=input$xlab,
             ylab=input$ylab)
      }
      
      if(y()==3){
        tw<-dados[,y()]
        fitl1f <- nls(tw~l1(age, Linf, gninf, ti),data=dados,start=svl1f)
        
        plot(tw~age, 
             pch=pch(), col=rgb(0,0,0,1/4),
             ylim=c(0,(max(tw)*1.2)),
             xlab=input$xlab,
             ylab=input$ylab)
      }
      
      x<-seq(min(age),max(age), 0.1)
      
      if(input$boot == T){
        if(y()==2){
          bootP<-matrix(NA, ncol = 3, nrow = bootN())
          nls.control(maxiter = 100, minFactor = 1/2048, tol = 1e-07)
          for(i in 1:bootN()){
            NewSamplesLT<-dados[sample(1:nrow(dados),replace = T),] 
            try(fitv1fNew3 <- nls(tl~l1(age, Linf, gninf, ti),data=NewSamplesLT,
                                  start=svl1f),silent = T)
            bootP[i,1]<-coef(fitv1fNew3)[1]
            bootP[i,2]<-coef(fitv1fNew3)[2]
            bootP[i,3]<-coef(fitv1fNew3)[3]
            
            curve(bootP[i , 1] / (1 + exp(-bootP[i , 2] * (x - bootP[ i, 3]))), 
                  from = min(age), to = max(age),
                  lwd = 1, col=rgb(0,0,0,1/25), add=T)
          }
          ###
          LCI <- UCI <- seq()
          idades <- seq(0,max(age), 0.05)
          for(i in 1:length(idades)){
            TL <- bootP[ , 1] / (1 + exp( - bootP[ , 2] *
                                            (idades[ i ] - bootP[ , 3])))
            LCI[ i ] <- quantile( TL, 0.025 , na.rm = T)
            UCI[ i ] <- quantile( TL, 0.975 , na.rm = T) 
          }
        }
        
        if(y()==3){
          bootP<-matrix(NA, ncol = 3, nrow = bootN())
          nls.control(maxiter = 100, minFactor = 1/2048, tol = 1e-07)
          for(i in 1:bootN()){
            NewSamplesLT<-dados[sample(1:nrow(dados),replace = T),] 
            try(fitv1fNew3 <- nls(tw~l1(age, Linf, gninf, ti),data=NewSamplesLT,
                                  start=svl1f),silent = T)
            bootP[i,1]<-coef(fitv1fNew3)[1]
            bootP[i,2]<-coef(fitv1fNew3)[2]
            bootP[i,3]<-coef(fitv1fNew3)[3]
            
            curve(bootP[i , 1] / (1 + exp(-bootP[i , 2] * (x - bootP[ i, 3]))), 
                  from = min(age), to = max(age),
                  lwd = 1, col=rgb(0,0,0,1/25), add=T)
          }
          ###
          LCI <- UCI <- seq()
          idades <- seq(0,max(age), 0.05)
          for(i in 1:length(idades)){
            TL <- bootP[ , 1] / (1 + exp( - bootP[ , 2] *
                                            (idades[ i ] - bootP[ , 3])))
            LCI[ i ] <- quantile( TL, 0.025 , na.rm = T)
            UCI[ i ] <- quantile( TL, 0.975 , na.rm = T) 
          }
        }
      }
      
      curve(coef(fitl1f)[1] / (1 + exp(-coef(fitl1f)[2] * (x - coef(fitl1f)[3]))), 
            from = min(age), to = max(age),
            lwd = 2, col="red", add=T)
      if(input$boot == T){
        lines(idades, LCI, lty=3, col="red", lwd=2)
        lines(idades, UCI, lty=3, col="red", lwd=2)
      }
    })
    
    ##
    
    output$todos<-renderPlot({
      if(is.null(input$file)) {return()}
      
      v1 <- vbFuns(param="typical")
      l1 <- logisticFuns(param = "CJ1")
      g1 <- GompertzFuns(param="Ricker1")
      
      dados<-na.omit(data())
      tl<-dados[,y()]
      age<- dados[, x()]
    
      plot(tl~age, 
           pch=pch(), col=rgb(0,0,0,1/4),
           ylim=c(0,(max(tl)*1.2)),
           xlab=input$xlab,
           ylab=input$ylab)
      
      x<-seq(min(age),max(age), 0.1)
      
      if(input$modelsvbgf==T){
        svv1f <- list(Linf=Linfvbgf(), K=K(), t0=t0())
        fitv1f <- nls(tl ~ v1(age, Linf, K, t0), 
                      data = dados, start = svv1f)
        curve(coef(fitv1f)[1] * (1 - exp(-coef(fitv1f)[2] * (x - coef(fitv1f)[3]))), 
              from = min(age), to = max(age),
              lwd = 2, col="black", add=T)
      }
      if(input$modelslog==T){
        svl1f <- list(Linf=Linflog(), gninf=ginf(), ti=tilog())
        fitl1f <- nls(tl~l1(age, Linf, gninf, ti),
                      data=dados, start=svl1f)
        curve(coef(fitl1f)[1] * exp( - exp(-coef(fitl1f)[2] * (x - coef(fitl1f)[3]))), 
              from = min(age), to = max(age),
              lwd = 2, col="blue", add=T)
      }
      if(input$modelsgomp==T){
        svG1f <- list(Linf=Linfgom(), gi=gi(), ti=ti())
        fitG1f <- nls(tl~g1(age,Linf,gi,ti), 
                      data=dados, start=svG1f)
        curve(coef(fitG1f)[1] / (1 + exp(-coef(fitG1f)[2] * (x - coef(fitG1f)[3]))), 
              from = min(age), to = max(age),
              lwd = 2, col="red", add=T)
       }
      
      {
        if(input$modelsvbgf==T & input$modelslog==F & input$modelsgomp==F ){
        legend("bottomright", "VBGF", col="black",lwd=2,
               bty="n", lty = 1)
        }
        
        else if(input$modelsvbgf==F & input$modelslog==F & input$modelsgomp==T ){
          legend("bottomright", "Gompertz", col="red",
                 bty="n", lty = 1, lwd=2)
        }
        
        else if(input$modelsvbgf==F & input$modelslog==T & input$modelsgomp==F ){
          legend("bottomright", "Logistic", col="blue",lwd=2,
                 bty="n", lty = 1)
        }
        
        else if(input$modelsvbgf==T & input$modelslog==F & input$modelsgomp==T ){
          legend("bottomright", c("VBGF","Gompertz"),lwd=2,
               col=c("black", "red"), bty="n", lty = 1)
        }
        else if(input$modelsvbgf==T & input$modelslog==T & input$modelsgomp==F ){
          legend("bottomright", c("VBGF", "Logistic"),lwd=2,
                 col=c("black", "blue"), bty="n", lty = 1)
        }
        else if(input$modelsvbgf==F & input$modelslog==T & input$modelsgomp==T ){
          legend("bottomright", c("Gompertz", "Logistic"),lwd=2,
                 col=c("red", "blue"), bty="n", lty = 1)
        }
        else if(input$modelsvbgf==T & input$modelslog==T & input$modelsgomp==T ){
        legend("bottomright", c("VBGF","Gompertz", "Logistic"), lwd=2,
               col=c("black", "red", "blue"), bty="n", lty = 1)
        }
      }
    })
    
    local<-reactive({
      as.numeric(input$factor)
    })
    
    
    output$AIC<-renderTable({
      if(input$modelsvbgf==T & input$modelslog==F & input$modelsgomp==T ){
        A<-aictab(list(modVBGF(), modGom()),
               c("VBGF","Gompertz"))
      }
      else if(input$modelsvbgf==T & input$modelslog==T & input$modelsgomp==F ){
        A<-aictab(list(modVBGF(), modLog()),
               c("VBGF", "Logistic"))
      }
      else if(input$modelsvbgf==F & input$modelslog==T & input$modelsgomp==T ){
        A<- aictab(list(modGom(), modLog()),
               c("Gompertz", "Logistic"))
      }
      else if(input$modelsvbgf==T & input$modelslog==T & input$modelsgomp==T ){
        A<-aictab(list(modVBGF(), modGom(), modLog()),
               c("VBGF","Gompertz", "Logistic"))
      }
      return(A)
    })
    ### Exportando graficos
    
    res<-reactive({
      as.numeric(input$res)
    })
    height<-reactive({
      as.numeric(input$height)
    })
    width<-reactive({
      as.numeric(input$width)
    })
  
    
  
    # output$downvbgfBoot1 <- downloadHandler(
    #   filename = function(){
    #     paste("VBGF.", downfile())
    #   },
    #   content = function(file){
    #     if(downfile()=="tiff"){
    #       tiff(file, width = 2400, height = 1800, res = 300)
    #       plot(1:10,1:10)
    #       dev.off()
    #     }
    #     if(downfile()=="jpeg"){
    #       jpeg(file, width = 2400, height = 1800, res = 300)
    #       plot(1:10,1:10)
    #       dev.off()
    #     }
    #   }
    # )
  
    output$downVBGF <- downloadHandler(
      filename = function(){
        paste("VBGF", input$downloadfile, sep = ".")
      },
      content = function(file){
        if(input$downloadfile=="tiff")
          tiff(file, width = width(), height = height(), res= res())
        else
          jpeg(file, width = width(), height = height(), res= res())
        
        plot(1:10,1:10)
        dev.off()
      }
    )  
    
    
  
    
    
    ###
    output$AICprint <- renderPrint({
      AIC()
    })
    
    kimura <- reactive({
      if(is.null(input$file)) {return()}
      
      v1 <- vbFuns(param="typical")
      dados<-na.omit(data())
      
      age<-dados[,x()]
      tl<-dados[,y()]
      local<-dados[,local()]
  
      return(growthlrt(len = tl, age = age, group = local,
                model = 1, error = 2, select = 2, 
                Linf = rep(Linfvbgf(),length(levels(local))), 
                K = rep(K(),length(levels(local))), 
                t0 = rep(t0(),length(levels(local)))))
      })
    
    output$kimuraTab<-renderPrint({
      print(kimura())
    })
  })
}

##################################################
#####
##### Age and Growth app - Server
##### Jonas Vasconcelos-Filho
##### 09/06/2019
##### v 1.0
#####
##################################################

# check.packages <- function(pkg){
#   new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
#   if (length(new.pkg)) 
#     install.packages(new.pkg, dependencies = TRUE)
#   sapply(pkg, require, character.only = TRUE)
# }

# # Usage example
# packages<-c("shiny", "FSA", "fishmethods", "AICcmodavg", 
#             "nlstools", "shinydashboard", "shinyWidgets", 
#             "ggplot2", "waiter","shinyalert")
#check.packages(packages)

library("shiny")
library("FSA")
library("fishmethods")
library("AICcmodavg")
library("nlstools")
library("shinydashboard")
library("shinyWidgets")
library("ggplot2")
library("waiter")
library("shinyalert")
library("rethinking")
library("tidyverse")
library("investr")

# if(!any(check.packages(packages)==F)){
shinyServer(function(session,input, output) {
  
  ###################################
  #### Length-Weight Relationship (LWR)
  # Data
  lwrdata <- reactive({
    file1 <- input$lwrfile
    if(is.null(file1)) {return()}
    read.table(file = file1$datapath,
               sep = input$lwrsep,
               header = input$lwrheader,
               stringsAsFactors = input$lwrstringAsFactors)
  })
  
  # LWR - Variables
  observe({
      Vxlwr<-1:length(colnames(lwrdata()))
      atr_listlwr<-list(names = colnames(lwrdata()))
      attributes(Vxlwr)<-atr_listlwr
      Faclwr<-!sapply(lwrdata(), is.factor)
      Vxlwr<-Vxlwr[Faclwr]
      updateSelectInput(session, "lwrx", "1. Select x-variable",
                        choices = Vxlwr)
  })
  
  observe({
      Vylwr<-1:length(colnames(lwrdata()))
      atr_listlwr<-list(names = colnames(lwrdata()))
      attributes(Vylwr)<-atr_listlwr
      Faclwr<-!sapply(lwrdata(), is.factor)
      Vylwr<-Vylwr[Faclwr]
      updateSelectInput(session, "lwry", "2. Select y-variable",
                        choices = Vylwr)
  })
  
  observe({
    Vx<-1:length(colnames(lwrdata()))
    atr_list<-list(names = colnames(lwrdata()))
    attributes(Vx)<-atr_list
    Fac<-!sapply(lwrdata(), is.factor)
    Vx<-c("None", Vx[!Fac])
    updateSelectInput(session, "lwrfactor", "3. Select factor-variable:",
                      choices = Vx, selected = "None")
  })
  
  ## Table, summary, and structure
  output$fileoblwr <- renderPrint({
    if(is.null(input$lwrfile)) {return()}
    str(lwrdata())
  })
  
  output$summlwr <- renderPrint({
    if(is.null(input$lwrfile)) {return()}
    summary(lwrdata())
  })
  
  output$tableuilwr <- renderUI({
    dataoutlwr <- lwrdata()
    output$dataoutlwr<-renderDataTable(dataoutlwr)
    dataTableOutput("dataoutlwr")
  })
  
  output$lwrtb <- renderUI({
    if(is.null(input$lwrfile)) {return()}
    tabsetPanel(
      tabPanel("Data",
               uiOutput("tableuilwr")),
      
      tabPanel("Structure",
               verbatimTextOutput("fileoblwr")),
      
      tabPanel("Summary",
               verbatimTextOutput("summlwr"))
    )
  })

  # Graphical parameters
  lwrx <- reactive({
    as.numeric(input$lwrx)
  })
  
  lwry <- reactive({
    as.numeric(input$lwry)
  })
  
  lwrpch<-reactive({
    as.numeric(input$lwrpch)
  })
  
  lwrcolpt<-reactive({
    as.numeric(input$lwrcolpt)
  })
  
  lwrA <- reactive({
    as.numeric(input$lwrA)
  })
  
  lwrB <- reactive({
    as.numeric(input$lwrB)
  })
  
  lwrptsize <- reactive({
    as.numeric(input$lwrptsize)
  })
  
  lwrptalpha <- reactive({
    as.numeric(input$lwrptalpha)
  })
  
  lwrICalpha <- reactive({
    as.numeric(input$lwrICalpha)
  })
  
  lwrfactor<-reactive({
    as.numeric(input$lwrfactor)
  })
  
  widthlwr <- reactive({
    as.numeric(input$widthlwr)
  })
  
  heightlwr <- reactive({
    as.numeric(input$heightlwr)
  })
  
  reslwr <- reactive({
    as.numeric(input$reslwr)
  })
  
  ##############################
  ## Models
  modLWRnls2<-reactive({
    mods$lwr_Lm_Ancova_H1 <- FALSE
    H0 <- 3
    if(!is.na(lwrfactor())){
      dados<-data.frame(x = lwrdata()[, lwrx()],
                        y = lwrdata()[, lwry()],
                        factor = lwrdata()[, lwrfactor()])
      
      a <- lwrA()
      b <- lwrB()
      
      ancova <- glm(y ~ x + factor, data = dados)
      AOV <- summary(aov(ancova))
      AOV <- AOV[[1]]
      pvalue <- AOV$`Pr(>F)`[2]

      
      if(pvalue < (1-lwrICalpha())){
        mods$lwr_Lm_Ancova_H1 <- TRUE
        N <- levels(dados$factor)
        DF <- as.data.frame(matrix(NA, nrow = (2*length(N)), ncol = 8))
        Linha <- seq(1, (2*length(N))-1, 2)
        
        for(i in 1:length(N)){
          subdata <- subset(dados, dados$factor == N[i])
          mod <- nls(y ~ a * x^b,
                     data= subdata,
                     start = list(a = a, 
                                  b = b))
          S<-summary(mod)
          DF[Linha[i]:(Linha[i]+1), 1] <- N[i]
          DF[Linha[i]:(Linha[i]+1), 2] <- c('intercept', "slope")
          DF[Linha[i]:(Linha[i]+1), 3:4] <- round(S$coefficients[ ,1:2], 4)
          
          
          DF[Linha[i]:(Linha[i]+1), 5] <- round(abs(H0 - DF[Linha[i]:(Linha[i]+1), 3]) / 
                                                DF[Linha[i]:(Linha[i]+1), 4], 4)
          DF[Linha[i]:(Linha[i]+1), 6] <- S$df[2]
          DF[Linha[i]:(Linha[i]+1), 7] <- round(pt(DF[Linha[i]:(Linha[i]+1), 5],
                                             DF[Linha[i]:(Linha[i]+1), 6], lower.tail = F), 4)
        }
        for(j in 1:nrow(DF)){
          if(DF[j, 7] < (1 - lwrICalpha())){
            if(DF[j, 3] > H0){
              DF[j, 8] <-  "Positive allometric"
            } else
              DF[j, 8] <-  "Negative allometric"
          } else{
            DF[j, 8] <- "Isometric"
          }
        }
        
        colnames(DF)<-c("Group",
                        "Parameter",
                        "Estimate",
                        "Std. Error",
                        "t value",
                        'df',
                        "Pr(>|t|)",
                        'Status')
        
        mods$lwr_Lm_fits<-DF #[seq(2, length(N)*2,2), ]
      } else {
        DF <- as.data.frame(matrix(NA, nrow = 2, ncol = 7))
        mod <- nls(y ~ a * x^b,
                   data= dados,
                   start = list(a = a, 
                                b = b))
        S<-summary(mod)
        DF[, 1] <- "Polled"
        DF[, 2] <- c('intercept', "slope")
        DF[, 3:4] <- round(S$coefficients[ ,1:2], 4)
        
        DF[2, 5] <- round(abs(H0 - DF[2, 3]) / 
                          DF[2, 4], 4)
        DF[2, 6] <- S$df[2]
        DF[2, 7] <- round(pt(DF[2, 5], DF[2, 6], lower.tail = F), 4)
        
        if(DF[2, 7] < (1 - lwrICalpha())){
          if(DF[2, 3] > H0){
            DF[2, 8] <-  "Positive allometric"
          } else {
            DF[2, 8] <-  "Negative allometric"
          }
        } else{
          DF[2, 8] <- "Isometric"
        }
        
        colnames(DF)<-c("Group",
                        "Parameter",
                        "Estimate",
                        "Std. Error",
                        "t value",
                        'df',
                        "Pr(>|t|)",
                        "Status")
        mods$lwr_Lm_fits<-DF #[2, ]
      }
      
      mods$lwr_Lm_AOV<-AOV
      
    } else {
      dados<-data.frame(x = lwrdata()[, lwrx()],
                        y = lwrdata()[, lwry()]) 
      
      a <- lwrA()
      b <- lwrB()
      
      mod <- nls(y ~ a * x^b,
                 data= dados,
                 start = list(a = a, 
                              b = b))
      AOV <- summary(mod)
      mods$lwr_Lm_AOV<-AOV
      
      DF <- as.data.frame(matrix(NA, nrow = 2, ncol = 8))
      S<-summary(mod)
      DF[1:2, 1] <- "Polled"
      DF[1:2, 2] <- c('intercept', "slope")
      DF[1:2, 3:4] <- round(S$coefficients[, 1:2], 4)
      
      DF[2, 5] <- round(abs(H0 - DF[2, 3])/
                                DF[2, 4],4)
      DF[2, 6] <- S$df[2]
      DF[2, 7] <- pt(DF[2, 5],
                     DF[2, 6], lower.tail = F)
      
      if(DF[2, 7] < (1- lwrICalpha())){
        if(DF[2, 3] > H0){
          DF[2, 8] <-  "Positive allometric"
        } else
          DF[2, 8] <-  "Negative allometric"
      } else{
        DF[2, 8] <- "Isometric"
      }
      
      colnames(DF)<-c("Group",
                      "Parameter",
                      "Estimate",
                      "Std. Error",
                      "t value",
                      'df',
                      "Pr(>|t|)", 
                      'Status')
      
      mods$lwr_Lm_fits<-DF
    }
    
    return(mods$lwr_Lm_fits)
  })
  
  modLWRlm<-reactive({
    mods$lwr_Lm_Ancova_H1 <- FALSE
    if(!is.na(lwrfactor())){
      dados<-data.frame(x = lwrdata()[, lwrx()],
                        y = lwrdata()[, lwry()],
                        factor = lwrdata()[, lwrfactor()])
      dados<-subset(dados, dados$x > 0 & dados$y > 0)
      dados$x<-log(dados$x)
      dados$y<-log(dados$y)
      a <- lwrA()
      b <- lwrB()
      
      fitlm <- lm(y ~ factor + x + factor:x, data = dados)
      AOV <- anova(fitlm)
      pvalue <- AOV$`Pr(>F)`[1]

      if(pvalue < (1-lwrICalpha())){
        mods$lwr_Lm_Ancova_H1 <- TRUE
        N <- levels(dados$factor)
        DF <- as.data.frame(matrix(NA, nrow = (2*length(N)), ncol = 8))
        Linha <- seq(1, (2*length(N))-1, 2)
        
        for(i in 1:length(N)){
          subdata <- subset(dados, dados$factor == N[i])
          mod <- lm(y ~ x, data= subdata)
          S<-summary(mod)
          DF[Linha[i]:(Linha[i]+1), 1] <- N[i] # adaptar a nova tabela de allometria para sair somente o slope pra factor
          DF[Linha[i]:(Linha[i]+1), 2] <- c('intercept', "slope")
          DF[Linha[i]:(Linha[i]+1), 3:4] <- round(S$coefficients[,-4],4)
          
          B0 <- 1
          DF[(Linha[i]+1), 5] <- round(abs(B0 - DF[(Linha[i]+1), 3]) / DF[(Linha[i]+1), 4],4)
          DF[Linha[i]:(Linha[i]+1), 6] <- S$df[2]
          DF[(Linha[i]+1), 7]  <- round(pt(DF[(Linha[i]+1), 5],
                                           DF[(Linha[i]+1), 6], lower.tail = F), 4)
          
          if(DF[(Linha[i]+1), 7] < (1-lwrICalpha())){
            if(DF[(Linha[i]+1), 3] > B0){
              DF[(Linha[i]+1), 8] <-  "Positive allometric"
            } else
              DF[(Linha[i]+1), 8] <-  "Negative allometric"
          } else{
            DF[(Linha[i]+1), 8] <- "Isometric"
          }
          
        }
        
        colnames(DF)<-c("Group",
                        "Parameter",
                        "Estimate",
                        "Std. Error",
                        "t value",
                        'df',
                        "Pr(>|t|)",
                        'Body Growth Behavior')
        DF <- DF[seq(2, length(N)*2, 2),]
        mods$lwr_Lm_fits<-DF
      } else {
        DF <- as.data.frame(matrix(NA, nrow = 2, ncol = 8))
        mod <- lm(y ~ x, data= dados)
        S<-summary(mod)
        DF[1, 1] <- "Polled"
        DF[1, 2] <- "slope"
        DF[1, 3:4] <- round(S$coefficients[2, 1:2],4)
        
        B0 <- 1
        DF[1, 5] <- round(abs(B0 - DF[1, 3]) / DF[1, 4],4)
        DF[1, 6] <- S$df[2]
        DF[1, 7] <- round(pt(DF[1, 6], DF[1, 5],lower.tail = F),4)
        
        if(DF[1, 7] <  (1-lwrICalpha())){
          if(DF[1, 3] > B0){
            DF[1, 8] <-  "Positive allometric"
          } else
            DF[1, 8] <-  "Negative allometric"
        } else{
          DF[1, 8] <- "Isometric"
        }
        colnames(DF)<-c("Group",
                        "Parameter",
                        "Estimate",
                        "Std. Error",
                        "t value",
                        'df',
                        "Pr(>|t|)",
                        'Body Growth Behavior')
        mods$lwr_Lm_fits<-DF
      }
      
      rownames(AOV) <- c(colnames(lwrdata())[lwrfactor()],
                         colnames(lwrdata())[lwrx()],
                         paste("",colnames(lwrdata())[lwrfactor()],
                               ":",colnames(lwrdata())[lwrx()], sep = ""),
                         "Residuals")
      attributes(AOV)$heading[2]<-paste('Response:', colnames(lwrdata())[lwry()])
      mods$lwr_Lm_AOV<-AOV

    } else {
      dados<-data.frame(x = lwrdata()[, lwrx()],
                        y = lwrdata()[, lwry()]) 
      
      dados<-subset(dados, dados$x > 0 & dados$y > 0)
      dados$x<-log(dados$x)
      dados$y<-log(dados$y)
      a <- lwrA()
      b <- lwrB()
      
      fitlm <- lm(y ~ x, data = dados)
      AOV <- anova(fitlm)
      rownames(AOV) <- c(colnames(lwrdata())[lwrx()],
                         "Residuals")
      attributes(AOV)$heading[2]<-paste('Response:', colnames(lwrdata())[lwry()])
      mods$lwr_Lm_AOV<-AOV
      
      DF <- as.data.frame(matrix(NA, nrow = 1, ncol = 8))
      S<-summary(fitlm)
      DF[1, 1] <- "Polled"
      DF[1, 2] <- "slope"
      DF[1, 3:4] <- round(S$coefficients[2, 1:2],4)
      
      B0 <- 1
      DF[1, 5] <- round(abs(B0 - DF[1, 3]) / DF[1, 4],4)
      DF[1, 6] <- S$df[2]
      DF[1, 7] <- round(pt(DF[1, 6], DF[1, 5],lower.tail = F),4)
      
      if(DF[1, 7] <  (1-lwrICalpha())){
        if(DF[1, 3] > B0){
          DF[1, 8] <-  "Positive allometric"
        } else
          DF[1, 8] <-  "Negative allometric"
      } else{
        DF[1, 8] <- "Isometric"
      }
      colnames(DF)<-c("Group",
                      "Parameter",
                      "Estimate",
                      "Std. Error",
                      "t value",
                      'df',
                      "Pr(>|t|)",
                      'Body Growth Behavior')
      mods$lwr_Lm_fits<-DF
    }
    
    mods$fitLWRlm<-fitlm
    return(fitlm)
  }) 
  
  lwrANCOVA <- reactive({
    if(input$lwrmodel==1){
      bH0 <- "LWR are equals"
      bH1 <- "LWR are not equals"
      S <- summary(mods$fitnlsANCOVA)
      nlsH1<-mods$nlsH1
      if(nlsH1){ 
        whoIsDifferent <- rownames(S$coefficients)[2+which(S$coefficients[3:nrow(S$coefficients),4]<lwrICalpha())]
        DF <- as.data.frame(t(as.matrix(S$coefficients[2+which(S$coefficients[3:nrow(S$coefficients),4]<lwrICalpha()),])))
        Status <- paste("Null hypothesis (H0) rejected. LWR of", whoIsDifferent, " is different", sep = "")
      } else{
        whoIsDifferent <- rownames(S$coefficients)[2+which(S$coefficients[3:nrow(S$coefficients),4]<lwrICalpha())]
        DF <- as.data.frame(t(as.matrix(S$coefficients[2+which(S$coefficients[3:nrow(S$coefficients),4]<lwrICalpha()),])))
        Status <- paste("Null hypothesis (H0) accepted. LWR curves are equals", sep = "")
      }
      
      DF$bH0<-bH0
      DF$bH1<-bH1
      DF$Status <- Status
      
      DF<-DF[,c(5,6,1:4,7)]
      return(DF)
    }
  })
  
  {
  lwrallometric <- reactive({ # Porque a saida so funciona se essa porra tiver ativa?
    if(input$lwrmodel==1){
      mod <- modLWRnls2()
      S <- summary(mod)

      B0<- 3
      Bh1<-coef(mod)[2]
      epBh1<-S$coefficients[2,2]

      t <- abs(B0 - Bh1)/epBh1
      p <- pt(t, S$df[2], lower.tail = F)
    } else{
      mod <- modLWRlm()
      S <- summary(mod)

      B0<- 1
      Bh1<-coef(mod)[2]
      epBh1<-S$coefficients[2,2]

      t <- abs(B0 - Bh1)/epBh1
      p <- pt(t, S$df[2], lower.tail = F)
    }

    if(p < 0.05){
      if(Bh1 > B0){
        Status <- "Positive allometric"
      } else
        Status <- "Negative allometric"
    } else{
      Status <- "Isometric"
    }

    # return(data.frame(bH0 = B0, bH1 = Bh1,
    #                   t = t, df = S$df[2],
    #                   'p-value' = ifelse(p<0.000001, "<2e-16",p),
    #                   status = Status))
    return(mods$lwr_Lm_fits)
  })
  }
  
  output$plotlwr <- renderPlot({
    if(!is.na(lwrfactor())){
      dados<-data.frame(x = lwrdata()[, lwrx()],
                        y = lwrdata()[, lwry()],
                        factor = lwrdata()[, lwrfactor()])
    } else {
      dados<-data.frame(x = lwrdata()[, lwrx()],
                        y = lwrdata()[, lwry()]) 
    }
    
    dados<-na.omit(dados)
    a <- lwrA()
    b <- lwrB()
  
    if(input$lwrmodel==1){
      
      parameters <- mods$lwr_Lm_fits
      finalData <- dados[1,]
      finalData$fit<-NA
      finalData$lwr<-NA
      finalData$upr<-NA
        
      if(mods$lwr_Lm_Ancova_H1){
        listFactors <- levels(dados$factor)
        
        for(j in 1:length(listFactors)){
          subData <- subset(dados, dados$factor == listFactors[j])
          mod <- nls(y ~ a * x ^ b,
                     data= subData,
                     start = list(a = a, 
                                  b = b))
          subData <- cbind(subData, 
                         as_tibble(predFit(mod, 
                                           newdata = subData, 
                                           interval = "confidence",
                                           level= lwrICalpha())))
          finalData<-rbind(finalData, subData)
        }
        dados<-finalData[-1,]
        
        gg <- ggplot(data = dados,
                     aes(x = x, y = y, color=factor)) +
          theme_bw(base_size = 14) +
          xlab(input$xlablwr) + ylab(input$ylablwr) +
          theme(axis.text = element_text(size=15,
                                         face = "bold", color = "black"),
                axis.title = element_text(size=15,
                                          face = "bold", color = "black"),
                strip.text = element_text(size=15,
                                          face = "bold", color = "black")) +
          geom_point(shape = lwrpch(),
                     alpha = lwrptalpha(),
                     size = lwrptsize())+
          geom_line(aes(x= x, y = fit, color=factor), size=1.25)
        
        if(input$lwrIC) {
          gg <- gg + geom_ribbon(aes(x = x, 
                                     ymin = lwr, 
                                     ymax = upr,
                                     fill = factor),
                                 alpha=0.2,
                                 linetype = 0)
        }
        
      } else {
        mod <- nls(y ~ a * x ^ b,
                   data= dados,
                   start = list(a = a, 
                                b = b))
        dados <- cbind(dados, 
                       as_tibble(predFit(mod, 
                                         newdata = dados, 
                                         interval = "confidence",
                                         level= lwrICalpha())))

        gg <- ggplot(data = dados,
                     aes(x = x, y = y)) +
          theme_bw(base_size = 14) +
          xlab(input$xlablwr) + ylab(input$ylablwr) +
          theme(axis.text = element_text(size=15,
                                         face = "bold", color = "black"),
                axis.title = element_text(size=15,
                                          face = "bold", color = "black"),
                strip.text = element_text(size=15,
                                          face = "bold", color = "black")) +
          geom_point(shape = lwrpch(),
                     alpha = lwrptalpha(),
                     size = lwrptsize())+
          geom_line(aes(x= x, y = fit))
        
        if(input$lwrIC) {
          gg <- gg + geom_ribbon(aes(x = x, ymin = lwr, ymax = upr),
                                 alpha=0.2)
        }
      }
    }  
    else{
      dados<-subset(dados, dados$x > 0 & dados$y > 0)
      dados$x<-log(dados$x)
      dados$y<-log(dados$y)
      
      gg <- ggplot(data = dados,
                   aes(x = x, y = y)) +
        theme_bw(base_size = 14) +
        xlab(input$xlablwr) + ylab(input$ylablwr) +
        theme(axis.text = element_text(size=15,
                                       face = "bold", color = "black"),
              axis.title = element_text(size=15,
                                        face = "bold", color = "black"),
              strip.text = element_text(size=15,
                                        face = "bold", color = "black")) +
        geom_point(shape = lwrpch(), 
                   alpha = lwrptalpha(), 
                   size = lwrptsize())
        
      if(mods$lwr_Lm_Ancova_H1){
        gg <- gg +
              geom_point(data = dados, 
                     aes(x = x, y = y, color = factor),
                     shape = lwrpch(), 
                     alpha = lwrptalpha(), 
                     size = lwrptsize())+
              geom_smooth(data = dados, 
                          aes(x = x, y = y, color=factor), 
                          method = "lm", se = input$lwrIC,
                          level = lwrICalpha())+
              labs(color = colnames(lwrdata())[lwrfactor()])
      } else {
        gg <- gg +
          geom_point(data = dados, 
                     aes(x = x, y = y),
                     shape = lwrpch(), 
                     alpha = lwrptalpha(), 
                     size = lwrptsize()) +
          geom_smooth(data = dados, 
                      aes(x = x, y = y), color = "black",
                      method = "lm", se = input$lwrIC,
                      level = lwrICalpha())
      }
    }
    
    graphs$basiclwr <- gg
    print(gg)
  })
    
  output$downLWRstat <- downloadHandler(
    filename = "LWR stats.txt", 
    content = function(file) {
      if(input$lwrmodel==1){
        sink(file)
          print(modLWRnls2())
          print(lwrallometric())
        sink()  
      } else {
        sink(file)
          print(summary(modLWRlm()))
          print(lwrallometric())
        sink()
      }
    })

  output$plotlwrstat <- renderPrint({
    print(mods$lwr_Lm_AOV)
    cat("\n\n")
    print(summary(mods$fitLWRlm))
    cat("\n\n")
    cat("Body Growth Behavior \n")
    print(lwrallometric(), row.names = FALSE)
  })
    
  
    
  output$downLWRplot <- downloadHandler(
    filename = function(){
      paste("LWR", input$downloadfilelwr, sep = ".")
    },
    content = function(file){
      if(input$downloadfilelwr=="tiff")
        tiff(file, width = widthlwr(), height = heightlwr(), res= reslwr())
      else
        jpeg(file, width = widthlwr(), height = heightlwr(), res= reslwr())
      
      print(graphs$basiclwr)
      dev.off()
    }
  )  
    
    ######################################## 
    ######################################## 
    ######################################## 
    # Age Input
    agedata <- reactive({
      file1 <- input$agefile
      if(is.null(file1)) {return()}
      read.table(file = file1$datapath,
                 sep = input$agesep,
                 header = input$ageheader,
                 stringsAsFactors = input$agestringAsFactors)
    })
    
    # Age - Variables and Graphical parameters
    observe(
      {
        Vx<-1:length(colnames(agedata()))
        atr_list<-list(names = colnames(agedata()))
        attributes(Vx)<-atr_list
        Fac<-!sapply(agedata(), is.factor)
        Vx<-Vx[Fac]
        updateSelectInput(session, "agex", "1. Select x-variable",
                              choices = Vx)
      })

    observe(
      {
        Vy<-1:length(colnames(agedata()))
        atr_list<-list(names = colnames(agedata()))
        attributes(Vy)<-atr_list
        Fac<-!sapply(agedata(), is.factor)
        Vy<-Vy[Fac]
        updateSelectInput(session, "agey", "2. Select y-variable",
                          choices = Vy)
    })
    
    x <- reactive({
      as.numeric(input$agex)
    })
    
    y <- reactive({
      as.numeric(input$agey)
    })
    
    output$plot <- renderPlot({
      dados<-data.frame(x = agedata()[, x()],
                        y = agedata()[, y()]) 
      dados<-na.omit(dados)
      
      gg <- ggplot() +
        geom_point(data = dados, aes(x = x, y = y),
                   colour = agecolpt(), shape = agepch())+
        theme_bw(base_size = 14) + 
        xlab(input$xlab) + ylab(input$ylab) +
        theme(axis.text=element_text(size=15, face = "bold", color = "black"),
              axis.title=element_text(size=15, face = "bold", color = "black"),
              strip.text = element_text(size=15, face = "bold", color = "black"))
        
      graphs$basic<-gg
      print(gg)
    })
    
    agepch<-reactive({
      as.numeric(input$agepch)
    })
    
    agecolpt<-reactive({
      as.numeric(input$agecolpt)
    })
    
    # Growth models
    plotInput <- reactive({
      dados<-na.omit(agedata())
      
      x<-dados[, x()]
      y<-dados[, y()]

      x<-seq(min(x, na.rm = T), max(x, na.rm = T), 0.1)
      dataY<-data.frame(x = x,
                        VBGF = NA,
                        Gomp = NA,
                        Log = NA)
      
      base<-graphs$basic
      
      if(input$modelsvbgf == T){
        dataY$VBGF <- Linfvbgf() * (1 - exp(-K() * (x - t0())))
        base <- base+ 
                geom_line(data = dataY, aes(x = x, y = VBGF),
                          color="black", size=1.25)
      }
      
      #gomp
      if(input$modelsgomp == T){
        dataY$Gomp <- Linfgom() * exp( - exp(-gi() * (x - ti())))
        base <- base+ 
                geom_line(data = dataY, aes(x = x, y = Gomp),
                          color="red", size=1.25)
      }
      
      #log
      if(input$modelslog == T){
        dataY$Log <- Linflog()/(1 + exp(-ginf() * (x - tilog()))) 
        base <- base+ 
          geom_line(data = dataY, aes(x = x, y = Log),
                    color="blue", size=1.25)
      }
      graphs$inputs<-base
      
    })
    
    output$plot2 <- renderPlot({
      plotInput()
      base<-graphs$inputs
      print(base)
    })
    
    output$plot3 <- renderPlot({
      plotInput()
      base<-graphs$inputs
      print(base)
    })
    
    output$plot4 <- renderPlot({
      plotInput()
      base<-graphs$inputs
      print(base)
    })
    
    output$plot5 <- renderPlot({
      plotInput()
      base<-graphs$inputs
      print(base)
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
    
    # VBGF seed values
    Linfvbgf<-reactive({
      as.numeric(input$vbgfL)
    })
    K<-reactive({
      as.numeric(input$vbgfK)
    })
    t0<-reactive({
      as.numeric(input$vbgft0)
    })
    
    # Gompertz seed values
    Linfgom<-reactive({
      as.numeric(input$gompL)
    })
    gi<-reactive({
      as.numeric(input$gompgi)
    })
    ti<-reactive({
      as.numeric(input$gompti)
    })
    
    # Log seed values
    Linflog<-reactive({
      as.numeric(input$logL)
    })
    ginf<-reactive({
      as.numeric(input$logginf)
    })
    tilog<-reactive({
      as.numeric(input$logti)
    })
    
    
    observe({
      if(input$modelsvbgf == F){return()}
      else{
        dados<-na.omit(agedata())
        dados<-data.frame(age = dados[, x()],
                          Y = dados[, y()])
        
        Seeds<-vbStarts(Y~age, data = dados, type = "typical")
        
        updateSliderInput(session, "vbgfL", 
                          value = Seeds$Linf,
                          min = round(Seeds$Linf/5), 
                          max = round(Seeds$Linf*5), 
                          step = 1)
      }
    })
    
    observe({
      if(input$modelsvbgf == F){return()}
      else{
        dados<-na.omit(agedata())
        dados<-data.frame(age = dados[, x()],
                          Y = dados[, y()])
        
        Seeds<-vbStarts(Y ~ age, data = dados,
                        type = "typical")
        
        updateSliderInput(session, "vbgfK", 
                          value = Seeds$K,
                          min = 0.001, 
                          max = round(Seeds$K*5,2), 
                          step = 0.001)
      }
    })
    
    observe({
      if(input$modelsvbgf == F){return()}
      else{
        dados<-na.omit(agedata())
        dados<-data.frame(age = dados[, x()],
                          Y = dados[, y()])
        
        Seeds<-vbStarts(Y ~ age, data = dados, type = "typical")
        
        updateSliderInput(session, "vbgft0", 
                          value = Seeds$t0,
                          min = -abs(round(Seeds$t0*5,2)),
                          max = abs(round(Seeds$t0*5,2)), 
                          step = 0.001)
      }
    })

    observe({
      if(input$modelsgomp == F){return()}
      else{
        dados <- na.omit(agedata())
        dados<-data.frame(age = dados[, x()],
                          Y = dados[, y()])
        
        Seeds<-vbStarts(Y ~ age, data = dados, type = "typical")
        
        updateSliderInput(session, "gompL",
                          value = Seeds$Linf,
                          min = round(Seeds$Linf/5),
                          max = round(Seeds$Linf*5), 
                          step = 1)
      }
    })
    
    observe({
      if(input$modelsgomp == F){return()}
      else{
        dados <- na.omit(agedata())
        dados<-data.frame(age = dados[, x()],
                          Y = dados[, y()]) 
        
        Seeds <-vbStarts(Y ~ age, data = dados, type = "typical")
        
        updateSliderInput(session, "gompgi", 
                          value = Seeds$K,
                          min = 0.001, 
                          max = round(Seeds$K*5,2), 
                          step = 0.001)
      }
    })
    
    observe({
      if(input$modelsgomp == F){return()}
      else{
        dados <- na.omit(agedata())
        dados<-data.frame(age = dados[, x()],
                          Y = dados[, y()]) 
        
        Seeds<-vbStarts(Y ~ age, data = dados, type = "typical")
        
        updateSliderInput(session, "gompti",
                          value = Seeds$t0,
                          min = -abs(round(Seeds$t0*5,2)),
                          max = abs(round(Seeds$t0*5,2)), 
                          step = 0.001)
      }
    })
    
    output$fileob <- renderPrint({
      if(is.null(input$agefile)) {return()}
      str(agedata())
    })
    
    output$summ <- renderPrint({
      if(is.null(input$agefile)) {return()}
      summary(agedata())
    })
    
    output$tableuiage <- renderUI({
      dataoutage <- agedata()
      output$dataoutage<-renderDataTable(dataoutage)
      dataTableOutput("dataoutage")
    })
    
    output$agetb <- renderUI({
      if(is.null(input$agefile)) {return()}
      tabsetPanel(
        tabPanel("Data",
                  uiOutput("tableuiage")),
                  
        tabPanel("Structure",
                  verbatimTextOutput("fileob")),
                  
        tabPanel("Summary",
                  verbatimTextOutput("summ"))
        )
    })
    
    # Graphical resolution
    width <- reactive({
      as.numeric(input$width)
    })
    height <- reactive({
      as.numeric(input$height)
    })
    res <- reactive({
      as.numeric(input$res)
    })
    
    
    #Colocar como excluir pontos  https://www.google.com/search?q=how+to+add+or+remove+point+in+shiny&rlz=1C1EJFC_enBR835BR835&oq=how+to+add+or+remove+point+in+shiny&aqs=chrome..69i57j33l5.202362j1j7&sourceid=chrome&ie=UTF-8#kpvalbx=1
    
    # data3<-input
    # data2<-data3()
    
    # observeEvent(y(), {
    #   updateSliderInput(session, "vbgfL", max=max(y2), min = min(y2), value = mean(y2))
    # })
    
    bootN<-reactive({
      as.numeric(input$nboot)
    })
    
    boot<-reactive({
      as.numeric(input$boot)
    })
    
    mods<-reactiveValues()
    
    modVBGF<-reactive({
      v1 <- vbFuns(param="typical")
      dados<-na.omit(agedata())
      dados<-data.frame(age = dados[, x()],
                        tl = dados[, y()]) 
      
      Linf<-Linfvbgf()
      K<-K()
      t0<-t0()
      
      svv1f <- vbStarts(tl ~ age,
                        data = dados)
      fitv1f <- nls(tl ~ v1(age, Linf, K, t0),
                    data = dados, start = svv1f)
        
      mods$vbgf<-fitv1f
      return(fitv1f)
    })
    
    output$modVBGF2 <- renderPrint({
      if(input$modelsvbgf){
        print(overview(modVBGF()))
      }
    })
    
    
    # VBGF Graphic with bootstrap
    graphs <- reactiveValues()
    
    output$vbgfBoot2<-renderPlot({
      v1 <- vbFuns(param="typical")
      fitv1f <- mods$vbgf
      
      dados <- na.omit(agedata())
      dados <- data.frame(age = dados[, x()],
                          tl = dados[, y()])
      
      Linf <- coef(fitv1f)[1]
      K    <- coef(fitv1f)[2]
      t0   <- coef(fitv1f)[3]

      svv1f <- list(Linf=Linf, K=K, t0=t0)
    
      x <- seq(min(dados$age), max(dados$age), 0.1)
      pred <- data.frame(x= x,
                         pred=predict(fitv1f, 
                                      newdata = data.frame(age = x)))
      
      gg <- graphs$basic+
        geom_line(data = pred, aes(x = x, y = pred),
                  size = 1.25, colour = "black")
   
      bootP<-matrix(NA, ncol = 3, nrow = bootN())
      nls.control(maxiter = 100, minFactor = 1/2048, tol = 1e-07)
      
      if(input$boot == T){
          for(i in 1:bootN()){
            NewSamplesLT <- dados[sample(1:nrow(dados), replace = T),] 
            try(fitv1fNew = nls(tl ~ v1(age, Linf, K, t0),
                                 data = NewSamplesLT,
                                 start = svv1f),
                silent = T)
            bootP[i,1]<-coef(fitv1fNew)[1]
            bootP[i,2]<-coef(fitv1fNew)[2]
            bootP[i,3]<-coef(fitv1fNew)[3]
          }

        LCI <- UCI <- seq()
        for(i in 1:length(x)){
          TL <- bootP[ , 1] * (1 - exp( - bootP[ , 2] *
                                          (x[ i ] - bootP[ , 3])))
          LCI[ i ] <- quantile( TL, 0.025 , na.rm = T)
          UCI[ i ] <- quantile( TL, 0.975 , na.rm = T) 
        }
        pred$LCI<-LCI
        pred$UCI<-UCI
        
        gg<-gg+
          geom_ribbon(data = pred, 
                      aes(x = x, ymin = LCI, ymax = UCI),
                      alpha=0.2)
      }
      
      graphs$vbgf<-gg
      if(input$modelsvbgf){
        print(gg)
      }
    })
    
   
    ## gomp
    modGom<-reactive({
      g1 <- GompertzFuns(param="Ricker1")
      dados<-na.omit(agedata())
      dados <- data.frame(age = dados[, x()],
                          tl = dados[, y()])
      
      Linf<-Linfgom()
      gi<-gi()
      ti<-ti()
      
      svG1f <- list(Linf = Linf, gi = gi, ti = ti)
      fitG1f <- nls(tl ~ g1(age, Linf, gi, ti),
                    data = dados, start = svG1f)
      
      mods$gomp<-fitG1f
      return(fitG1f)
    })
    
    output$modGom2 <- renderPrint({
      if(input$modelsgomp){
        print(overview(modGom()))
      }
    })
    
    # gom boot
    output$gomBoot<-renderPlot({  
      g1 <- GompertzFuns(param="Ricker1")
      fitG1f <- mods$gomp
      
      Linf <- coef(fitG1f)[1]
      gi <- coef(fitG1f)[2]
      ti <- coef(fitG1f)[3]
      
      dados <- na.omit(agedata())
      dados <- data.frame(age = dados[, x()],
                          tl = dados[, y()])
      
      x<-seq(min(dados$age), max(dados$age), 0.1)
      pred <- data.frame(x= x,
                         pred=predict(fitG1f, 
                                      newdata = data.frame(age = x)))
      
      gg <- graphs$basic+
            geom_line(data = pred, aes(x = x, y = pred),
                      size = 1.25, colour = "red")
        
      bootP<-matrix(NA, ncol = 3, nrow = bootN())
      nls.control(maxiter = 100, minFactor = 1/2048, tol = 1e-07)
      
      if(input$boot == T){
        for(i in 1:bootN()){
          NewSamplesLT <- dados[sample(1:nrow(dados), replace = T),] 
          try(fitv1fNew <- nls(tl ~ g1(age, Linf, gi, ti),
                               data = NewSamplesLT,
                               start = list(Linf = Linf,
                                            gi = gi, 
                                            ti = ti)),
              silent = T)
          bootP[i,1]<-coef(fitv1fNew)[1]
          bootP[i,2]<-coef(fitv1fNew)[2]
          bootP[i,3]<-coef(fitv1fNew)[3]
        }
        
        LCI <- UCI <- seq()
        for(i in 1:length(x)){
          TL <- bootP[ , 1] * exp(- exp( - bootP[ , 2] *
                                          (x[ i ] - bootP[ , 3])))
          LCI[ i ] <- quantile( TL, 0.025 , na.rm = T)
          UCI[ i ] <- quantile( TL, 0.975 , na.rm = T) 
        }
        pred$LCI<-LCI
        pred$UCI<-UCI
        
        gg<-gg+
          geom_ribbon(data = pred, 
                      aes(x = x, ymin = LCI, ymax = UCI),
                      alpha=0.2)
      }
      
      graphs$gomp<-gg
      if(input$modelsgomp){
        print(gg)
      }
    })
    
    ## log
    modLog<-reactive({
      l1 <- logisticFuns(param = "CJ1")
      
      dados<-na.omit(agedata())
      Linflog<-Linflog()
      gninf<-ginf()
      tilog<-tilog()
      dados<-data.frame(age = dados[, x()],
                        tl = dados[, y()])
      
      svl1f <- list(Linf = Linflog, gninf = gninf, ti = tilog)
      
      tryCatch(
        {fitl1f <- nls(tl~l1(age, Linf, gninf, ti),
                       data = dados, start = svl1f)
        },
        
        error = {showNotification("Error on fitting model: 
                                  the algorithm didn't converge yet.",
                                  duration = 2)}
      )
      mods$log<-fitl1f
      return(fitl1f)
    })
    
    output$modLog2 <- renderPrint({
      if(input$modelslog){
        print(overview(modLog()))
      }
    })
  
    output$logBoot2<-renderPlot({
      l1 <- logisticFuns(param = "CJ1")
      Linflog <- Linflog()
      gninf <- ginf()
      tilog <- tilog()
      fitl1f <- mods$log
      
      dados <- na.omit(agedata())
      dados <- data.frame(age = dados[, x()],
                          tl = dados[, y()])
      
      svl1f <- list(Linf = Linflog, gninf = gninf, ti = tilog)
      
      x<-seq(min(dados$age), max(dados$age), 0.1)
      pred <- data.frame(x= x,
                         pred=predict(fitl1f, 
                                      newdata = data.frame(age = x)))
      
      gg <- graphs$basic+
        geom_line(data = pred, aes(x = x, y = pred),
                  size = 1.25, colour = "blue")
      
      bootP<-matrix(NA, ncol = 3, nrow = bootN())
      nls.control(maxiter = 100, minFactor = 1/2048, tol = 1e-07)
      
      if(input$boot == T){
        for(i in 1:bootN()){
          NewSamplesLT <- dados[sample(1:nrow(dados), replace = T),] 
          try(fitv1fNew <- nls(tl ~ l1(age, Linf, gninf, ti),
                               data = NewSamplesLT,
                               start = svl1f),
              silent = T)
          bootP[i,1]<-coef(fitv1fNew)[1]
          bootP[i,2]<-coef(fitv1fNew)[2]
          bootP[i,3]<-coef(fitv1fNew)[3]
        }
        
        LCI <- UCI <- seq()
        for(i in 1:length(x)){
          TL <- bootP[ , 1] / (1 + exp( - bootP[ , 2] *
                                           (x[ i ] - bootP[ , 3])))
          LCI[ i ] <- quantile( TL, 0.025 , na.rm = T)
          UCI[ i ] <- quantile( TL, 0.975 , na.rm = T) 
        }
        pred$LCI<-LCI
        pred$UCI<-UCI
        
        gg<-gg+
            geom_ribbon(data = pred, 
                           aes(x = x, ymin = LCI, ymax = UCI),
                           alpha=0.2)
      }
      
      graphs$log<-gg
      if(input$modelslog){
        print(gg)
      }
    })
    
    output$todos<-renderPlot({
      dados   <- na.omit(agedata())
      dados   <- data.frame(age = dados[, x()],
                            tl = dados[, y()])
      
      x<-seq(min(dados$age), max(dados$age), 0.1)
      
      pred <- data.frame(x = x,
                         VBGF = NA,
                         Gomp = NA,
                         Log = NA)
      
      gg <- graphs$basic
      colors <- c("von Bertalanffy" = "black",
                  "Gompertz" = "red",
                  "Logistic" = "blue")
      
      if(input$modelsvbgf==T){
        fitvbgf <- mods$vbgf
        pred$VBGF <- predict(fitvbgf, 
                             newdata = data.frame(age = x))
        gg<-gg+
          geom_line(data = pred, aes(x = x, y = VBGF, color = 'von Bertalanffy'),
                    size = 1.25)
      }
      
      if(input$modelsgomp==T){
        fitgomp <- mods$gomp
        pred$Gomp <- predict(fitgomp, 
                             newdata = data.frame(age = x))
        gg<-gg+
          geom_line(data = pred, aes(x = x, y = Gomp, color = 'Gompertz'),
                    size = 1.25)
      }
      
      if(input$modelslog==T){
        fitlog  <- mods$log
        pred$Log <- predict(fitlog, 
                             newdata = data.frame(age = x))
        gg<-gg+
          geom_line(data = pred, aes(x = x, y = Log, color = 'Logistic'),
                    size = 1.25)
      }
      
      gg<-gg+
        labs(color = "Models")+
        scale_color_manual(values = colors[c(input$modelsvbgf, 
                                             input$modelsgomp,
                                             input$modelslog)])+
        theme(legend.position="bottom")
      
      graphs$all<-gg
      if(input$modelslog | input$modelsvbgf | input$modelsgomp){
        print(gg)
      }
    })
    
    AIC<-reactive({
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
  
    output$downVBGF <- downloadHandler(
      filename = function(){
        paste("VBGF", input$downloadfile, sep = ".")
      },
      content = function(file){
        if(input$downloadfile=="tiff")
          tiff(file, width = width(), height = height(), res= res())
        else
          jpeg(file, width = width(), height = height(), res= res())
        
        print(graphs$vbgf)
        dev.off()
      }
    )  
    
    output$downGomp <- downloadHandler(
      filename = function(){
        paste("Gomp", input$downloadfile, sep = ".")
      },
      content = function(file){
        if(input$downloadfile=="tiff")
          tiff(file, width = width(), height = height(), res= res())
        else
          jpeg(file, width = width(), height = height(), res= res())
        
        print(graphs$gomp)
        dev.off()
      }
    )  
    
    output$downLog <- downloadHandler(
      filename = function(){
        paste("Log", input$downloadfile, sep = ".")
      },
      content = function(file){
        if(input$downloadfile=="tiff")
          tiff(file, width = width(), height = height(), res= res())
        else
          jpeg(file, width = width(), height = height(), res= res())
        
        print(graphs$log)
        dev.off()
      }
    )  
    
    output$downAll <- downloadHandler(
      filename = function(){
        paste("All growth models", input$downloadfile, sep = ".")
      },
      content = function(file){
        if(input$downloadfile=="tiff")
          tiff(file, width = width(), height = height(), res= res())
        else
          jpeg(file, width = width(), height = height(), res= res())
        
        print(graphs$all)
        dev.off()
      }
    )  
    
    ### Exportando estatisticas
    output$downloadDataVBGF <- downloadHandler(
      filename = "VBGF stats.txt", 
      content = function(file) {
        sink(file)
          overview(modVBGF())
        sink()
      })
    
    output$downloadDataGomp <- downloadHandler(
      filename = "Gomp stats.txt", 
      content = function(file) {
        sink(file)
        overview(modGom())
        sink()
      })
    
    output$downloadDataLog <- downloadHandler(
      filename = "Log stats.txt", 
      content = function(file) {
        sink(file)
        overview(modLog())
        sink()
      })
  
    ###
    output$AICprint <- renderUI({
      dataoutAIC <- AIC()
      renderDataTable(dataoutAIC)
      
    })
    
    output$downloadAIC <- downloadHandler(
      filename = "AIC.txt", 
      content = function(file) {
        sink(file)
          print(AIC())
        sink()
    })
    
    kimuramodel<-reactive({
      as.numeric(input$kimuramodel)
    })
    
    kimurafactor<-reactive({
      as.numeric(input$kimurafactor)
    })
    
    observe(
      {
        Vx<-1:length(colnames(agedata()))
        atr_list<-list(names = colnames(agedata()))
        attributes(Vx)<-atr_list
        Fac<-!sapply(agedata(), is.factor)
        Vx<-c("None", Vx[!Fac])
        updateSelectInput(session, "kimurafactor", "Which group?",
                          choices = Vx, selected = "None")
      })
    
    kimura <- reactive({
      dados<-na.omit(agedata())
        
      age   <- dados[, x()]
      tl    <- dados[, y()]
      fator <- dados[, kimurafactor()]
    
      return(growthlrt(len = tl, age = age, group = fator,
                       model = kimuramodel(), 
                       error = 2, select = 1))
    })
    
    output$kimuraTab<-renderPrint({
      print(kimura())
    })
    
    output$downloadKimura <- downloadHandler(
      filename = "Kimura.txt", 
      content = function(file) {
        sink(file)
        print(kimura())
        sink()
      })
    
  })


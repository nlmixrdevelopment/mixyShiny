# nlmixr NGS 2018 Fellowship Program
# Clin Pharm nlmixr shinyDashboard Application
# Author: Bernard Ngara, Mirjam Trame

library(RxODE)
library(nlmixr)
library(ggplot2)
library(dplyr)
library(shiny)
library(shinydashboard)

ui <- dashboardPage(
  dashboardHeader(title="nlmixrSim_shiny"),
  dashboardSidebar(menuItem("Simulation")),
  
  dashboardBody(
    fluidRow(
      
      column(3,
      box(h3("Population Parameter Estimates"),
                 uiOutput("THETA"),
                 width=12,
                 inline = TRUE)
     ),
      column(9,
      box((radioButtons("GT", "GT", 
                              label = "Type of graph to plot ", 
                              choices=c("Spaghetti"="g1", 
                                        "Spaghetti(incl mean)"="g1b", 
                                        "Median and percentiles(no uncertainty)"="g2",
                                        "Median and percentiles(with uncertainty)"="g3",
                                        "VPC"="g4"
                                        ),
                              inline = TRUE)),
          plotOutput("PLOT",height=350),
          width=12,
          height=450),
      box(title="Population Data",
          verbatimTextOutput("fitdat"),
          width=4,
          #height=250,
          #style = "overflow-y: scroll; overflow-x: scroll"
          scrollX=T,
          collapsible = T,
          solidHeader = T),
      box(numericInput("Dose", "Dose amount (any units):",min=0, max=max(fit$simInfo$events$AMT*10), value= max(fit$simInfo$events$AMT), step=0.05, width=100),
          numericInput("reg", "Number of Doses:",min=1, max=30, value=1, step=1,width=100),
          width=2,
          height=250),
      box(numericInput("interval", "Dosing Interval (hrs):",min=0, max=720, value=24, step=1,width=100),
          numericInput("dura", "Simulated Duration (hrs):",min=0, max=720, value=24, step=24, width=100),
          width=2,
          height=250),
      box(numericInput("n", "Number of Individuals:",min=0, max=1000, value=10, step=50,width= 100),
          numericInput("ns", "Number of Studies:",min=0, max=1000, value=10, step=50,width= 100),
          width=2,
          height=250),
      box(h6("**Click to update plot**"),
          actionButton("SIMU", "Update Plot"),
          radioButtons("report","Save your file as:",choices = list("pdf","png")),
          downloadButton("export"),
          width=2,
          height=250,
          background = "navy")
          )
      ),
    fluidRow(
      box(h3("Population Model ODEs"),
          textOutput("ODE"),
          width=12,
          background = "light-blue")
      )
  )
)


server <- function(input, output) {
  load('fit2.rdata')
  
 #Displaying the model ODEs
  output$ODE <- renderText({
    print(fit$simInfo$rx)
    }) 
  
 #Displaying FIXED EFFECT parameter values in boxes/slider
  output$THETA <- renderUI({
    param_names = names(fit$simInfo$params)
    param_vals = round(exp(fit$simInfo$params), digits = 2)
    min= round(exp(fit$simInfo$params)/10, digits = 2)
    max= round(exp(fit$simInfo$params)*10, digits = 2)
      lapply(1:length(param_names), function(i) {
      div( 
          sliderInput(label=param_names[i], inputId= param_names[i], value= param_vals[i], min=min[i], max=max[i], width= 350)
      )
  })
  })
 
  output$fitdat <- renderPrint(fit$simInfo$events[1:5,])
  
  #Graph1: Spaghetti plots 
  graph1 <- eventReactive(input$SIMU,{
    ds <- input$Dose
    reg <- input$reg
    n <- input$n
    ns <- input$ns
    interval<-input$interval
    ev <- eventTable()
    ev$add.dosing(dose=ds, nbr.doses= reg, dosing.interval = interval, start.time=0)
    ev$get.EventTable()
    ev$add.sampling(0:input$dura)
    param_names = names(fit$simInfo$params)
    param_vals = NULL
    if(length(param_names)>0){
      tmp = paste0(param_names,'=as.numeric(input$',param_names,')')
      tmp = paste('c(',toString(tmp),')')
      param_vals <- eval(parse(text=tmp))
      para=log(param_vals)
    }  
    simdat<-simulate(fit,params=para,events =ev, nSub=n, nStud=ns)
    plot<- ggplot(as.data.frame(simdat), aes(x=time, y=sim)) + xlab("Time(hrs)") + ylab("Simulated Concentration")
    plot+ geom_line(aes(group=sim.id, col="Individual Profile")) + 
    geom_point(aes(col="Simulated Concentrations"))+ 
    scale_colour_manual(values=c("blue", "black")) 
  })
  
  graph1b <- eventReactive(input$SIMU,{
    ds <- input$Dose
    reg <- input$reg
    n <- input$n
    ns <- input$ns
    interval<-input$interval
    ev <- eventTable()
    ev$add.dosing(dose=ds, nbr.doses= reg, dosing.interval = interval, start.time=0)
    ev$get.EventTable()
    ev$add.sampling(0:input$dura)
    param_names = names(fit$simInfo$params)
    param_vals = NULL
    if(length(param_names)>0){
      tmp = paste0(param_names,'=as.numeric(input$',param_names,')')
      tmp = paste('c(',toString(tmp),')')
      param_vals <- eval(parse(text=tmp))
      para=log(param_vals)
    }  
    simdat<-simulate(fit,params=para,events =ev, nSub=n, nStud=ns)
    plot<- ggplot(as.data.frame(simdat), aes(x=time, y=sim)) + xlab("Time(hrs)") + ylab("Simulated Concentration")
    plot+ geom_line(aes(group=sim.id, col="Individual Profile")) + 
      geom_point(aes(col="Simulated Concentrations"))+ 
      stat_summary(aes(colour="Typical Value Profile"), geom="smooth", fun.y=mean)+
      scale_colour_manual(values=c("blue", "black", "red")) 
  })
  
  #Graph2:Simulations without parameter uncertainty
  
  omega_pars<-fit$simInfo$omega-fit$simInfo$omega
    graph2 <- eventReactive(input$SIMU,{
    ds <- input$Dose
    reg <- input$reg
    n <- input$n
    ns <- input$ns
    interval<-input$interval
    ev <- eventTable()
    ev$add.dosing(dose=ds, nbr.doses= reg, dosing.interval = interval, start.time=0)
    ev$get.EventTable()
    ev$add.sampling(0:input$dura)
    param_names = names(fit$simInfo$params)
    param_vals = NULL
    if(length(param_names)>0){
      tmp = paste0(param_names,'=as.numeric(input$',param_names,')')
      tmp = paste('c(',toString(tmp),')')
      param_vals = eval(parse(text=tmp))
      para=log(param_vals)
    }  
    omega_pars<-fit$simInfo$omega-fit$simInfo$omega
    simdat<-simulate(fit, params=para, events=ev, nSub=n, nStud=ns, omega=omega_pars, omegaIsChol = TRUE)
    p <-c(0.05, 0.5, 0.95);
    s <-simdat%>%mutate(id=sim.id%%n)%>%group_by(id,time)%>%
      do(data.frame(p1=p, eff=quantile(.$sim, probs=p)))%>%group_by(p1,time)%>%
      do(data.frame(p2=p, eff=quantile(.$eff, probs=p)))%>%ungroup()%>%
      mutate(p2=sprintf("p%02d",(p2*100)))%>%spread(p2,eff)%>%
      mutate(Percentile=factor(sprintf("%02d%%",round(p1*100))))
    
    ggplot(s,aes(time,p50,col=Percentile,fill=Percentile))+
      geom_ribbon(aes(ymin=p05,ymax=p95),alpha=0.5)+
      geom_line(size=1.2)+theme_light(base_size=18)+
      xlab("Time(hrs)") + ylab("Simulated Concentration")
  })
  
  #Graph3: Simulations with parameter uncertainty
  graph3 <- eventReactive(input$SIMU,{
    ds <- input$Dose
    reg <- input$reg
    n <- input$n
    ns <- input$ns
    interval<-input$interval
    ev <- eventTable()
    ev$add.dosing(dose=ds, nbr.doses= reg, dosing.interval = interval, start.time=0)
    ev$get.EventTable()
    ev$add.sampling(0:input$dura)
    param_names = names(fit$simInfo$params)
    param_vals = NULL
    if(length(param_names)>0){
      tmp = paste0(param_names,'=as.numeric(input$',param_names,')')
      tmp = paste('c(',toString(tmp),')')
      param_vals <- eval(parse(text=tmp))
      para=log(param_vals)
    }  
    simdat<-simulate(fit, params=para, events =ev, nSub=n, nStud=ns) 
    
    p <-c(0.05, 0.5, 0.95);
    s <-simdat%>%mutate(id=sim.id%%n)%>%group_by(id,time)%>%
      do(data.frame(p1=p, eff=quantile(.$sim, probs=p,na.rm=TRUE)))%>%group_by(p1,time)%>%
      do(data.frame(p2=p, eff=quantile(.$eff, probs=p,na.rm=TRUE)))%>%ungroup()%>%
      mutate(p2=sprintf("p%02d",(p2*100)))%>%spread(p2,eff)%>%
      mutate(Percentile=factor(sprintf("%02d%%",round(p1*100),na.rm=TRUE)))
  
    ggplot(s,aes(time,p50,col=Percentile,fill=Percentile))+
        geom_ribbon(aes(ymin=p05,ymax=p95),alpha=0.5)+
        geom_line(size=1.2)+theme_light(base_size=18)+
        xlab("Time(hrs)") + ylab("Simulated Concentration")
  })
  
  #Graph4: VPC
  graph4 <- eventReactive(input$SIMU,{
    vpc(fit, n=600,show=list(obs_dv=T), xlab = "Time(hrs)", ylab = "Concentration")
  })

 #Switch from one graph to another
  graphical_output<-eventReactive(input$SIMU,{
  if (input$GT=="g1"){
      graph1() 
  }
  else if (input$GT=="g1b"){
      graph1b() 
   }
  else if(input$GT=="g2"){ 
      graph2()
   }
   else if (input$GT=="g3"){
      graph3() 
   }
   else {
      graph4() 
   }
  })
  
 
 #Graphical Panel 
  output$PLOT <- renderPlot({
     graphical_output()
   })
  
 #Download Handler
  output$export <- downloadHandler(
    
    filename = function() {
      paste("simplot", input$report, sep = ".")
    },
    content = function(file){
      
      if(input$report == "pdf")
        pdf(file)
      else
        png(file)
      
      print(graphical_output())
      
      
      dev.off()
      
    }
  )
  
}


shinyApp(ui, server)
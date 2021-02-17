#' Create the shiny server for an nlmixr object
#'
#' For an example, see \code{run_app()}.
#' @inheritParams run_app
#' @param ... Not used
#' @return A shiny server function
#' @seealso \code{\link{run_app}}
#' @export
#' @importFrom dplyr "%>%" do group_by mutate ungroup
#' @importFrom ggplot2 aes geom_line geom_point geom_ribbon ggplot scale_colour_manual stat_summary theme_light xlab ylab
#' @importFrom grDevices dev.off pdf png
#' @importFrom nlmixr eventTable vpc
#' @importFrom shiny div downloadHandler eventReactive renderPlot renderPrint renderText renderUI sliderInput
#' @importFrom stats setNames simulate
#' @importFrom tidyr spread
create_server <- function(fit, ...) {
  if (!inherits(fit, "nlmixrFitCore")) {
    stop("The model fit must be a 'nlmixrFitCore' object")
  }
  
  function(input, output) {
    # Displaying the model ODEs
    output$ODE <- shiny::renderText({
      fit$simInfo$rx
    })
    
    # Displaying FIXED EFFECT parameter values in boxes/slider
    output$THETA <- shiny::renderUI({
      param_names = names(fit$simInfo$params)
      # FIXME: Detect the transform
      param_vals = signif(fit$simInfo$params, digits = 3)
      lower <-
        set_param_limits(
          estimate=param_vals,
          limit=stats::setNames(fit$ini$lower, fit$ini$name)[param_names],
          trans=NA_character_,
          direction="lower",
          multiplier=10
        )
      upper <-
        set_param_limits(
          estimate=param_vals,
          limit=stats::setNames(fit$ini$upper, fit$ini$name)[param_names],
          trans=NA_character_,
          direction="upper",
          multiplier=10
        )
      lapply(
        seq_along(param_names),
        function(i) {
          shiny::div(
            shiny::sliderInput(
              label=param_names[i],
              inputId=param_names[i],
              value=param_vals[i],
              min=lower[i],
              max=upper[i],
              width=350
            )
          )
        }
      )
    })
    
    output$fitdat <- shiny::renderPrint(fit$simInfo$events[1:5,])
    
    #Graph1: Spaghetti plots
    graph1 <- shiny::eventReactive(input$SIMU,{
      ds <- input$Dose
      reg <- input$reg
      n <- input$n
      ns <- input$ns
      interval<-input$interval
      ev <- nlmixr::eventTable()
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
      simdat <- stats::simulate(fit, params=para, events=ev, nSub=n, nStud=ns)
      ggplot2::ggplot(as.data.frame(simdat), ggplot2::aes(x=time, y=sim)) +
        ggplot2::xlab("Time (hr)") +
        ggplot2::ylab("Simulated Concentration") +
        ggplot2::geom_line(ggplot2::aes(group=sim.id, col="Individual Profile")) +
        ggplot2::geom_point(ggplot2::aes(col="Simulated Concentrations")) +
        ggplot2::scale_colour_manual(values=c("blue", "black"))
    })
    
    graph1b <- shiny::eventReactive(input$SIMU,{
      ds <- input$Dose
      reg <- input$reg
      n <- input$n
      ns <- input$ns
      interval<-input$interval
      ev <- nlmixr::eventTable()
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
      simdat<-stats::simulate(fit,params=para,events =ev, nSub=n, nStud=ns)
      ggplot2::ggplot(as.data.frame(simdat), ggplot2::aes(x=time, y=sim)) +
        ggplot2::xlab("Time (hr)") +
        ggplot2::ylab("Simulated Concentration") +
        ggplot2::geom_line(ggplot2::aes(group=sim.id, col="Individual Profile")) +
        ggplot2::geom_point(ggplot2::aes(col="Simulated Concentrations"))+
        ggplot2::stat_summary(ggplot2::aes(colour="Typical Value Profile"), geom="smooth", fun.y=mean)+
        ggplot2::scale_colour_manual(values=c("blue", "black", "red"))
    })
    
    #Graph2:Simulations without parameter uncertainty
    
    omega_pars<-fit$simInfo$omega-fit$simInfo$omega
    graph2 <- shiny::eventReactive(input$SIMU,{
      ds <- input$Dose
      reg <- input$reg
      n <- input$n
      ns <- input$ns
      interval<-input$interval
      ev <- nlmixr::eventTable()
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
      simdat<-stats::simulate(fit, params=para, events=ev, nSub=n, nStud=ns, omega=omega_pars, omegaIsChol = TRUE)
      p <-c(0.05, 0.5, 0.95);
      s <-
        simdat %>%
        dplyr::mutate(id=sim.id %% n) %>%
        dplyr::group_by(id, time) %>%
        dplyr::do(data.frame(p1=p, eff=stats::quantile(.$sim, probs=p))) %>%
        dplyr::group_by(p1,time)%>%
        dplyr::do(data.frame(p2=p, eff=stats::quantile(.$eff, probs=p))) %>%
        dplyr::ungroup() %>%
        dplyr::mutate(p2=sprintf("p%02d", p2*100)) %>%
        tidyr::spread(p2, eff) %>%
        mutate(Percentile=factor(sprintf("%02d%%", round(p1*100))))
      
      ggplot2::ggplot(s, ggplot2::aes(time, p50, col=Percentile, fill=Percentile))+
        ggplot2::geom_ribbon(ggplot2::aes(ymin=p05, ymax=p95), alpha=0.5)+
        ggplot2::geom_line(size=1.2) +
        ggplot2::theme_light(base_size=18) +
        ggplot2::xlab("Time (hr)") +
        ggplot2::ylab("Simulated Concentration")
    })
    
    #Graph3: Simulations with parameter uncertainty
    graph3 <- shiny::eventReactive(input$SIMU,{
      ds <- input$Dose
      reg <- input$reg
      n <- input$n
      ns <- input$ns
      interval<-input$interval
      ev <- nlmixr::eventTable()
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
      simdat<-stats::simulate(fit, params=para, events =ev, nSub=n, nStud=ns)
      
      p <-c(0.05, 0.5, 0.95);
      s <-
        simdat %>%
        dplyr::mutate(id=sim.id %% n) %>%
        dplyr::group_by(id, time) %>%
        dplyr::do(data.frame(p1=p, eff=stats::quantile(.$sim, probs=p,na.rm=TRUE))) %>%
        dplyr::group_by(p1, time) %>%
        dplyr::do(data.frame(p2=p, eff=stats::quantile(.$eff, probs=p,na.rm=TRUE))) %>%
        dplyr::ungroup() %>%
        dplyr::mutate(p2=sprintf("p%02d",(p2*100))) %>%
        tidyr::spread(p2, eff) %>%
        dplyr::mutate(Percentile=factor(sprintf("%02d%%", round(p1*100), na.rm=TRUE)))
      
      ggplot2::ggplot(s, ggplot2::aes(time, p50, col=Percentile, fill=Percentile)) +
        ggplot2::geom_ribbon(ggplot2::aes(ymin=p05, ymax=p95), alpha=0.5) +
        ggplot2::geom_line(size=1.2) +
        ggplot2::theme_light(base_size=18) +
        ggplot2::xlab("Time (hr)") +
        ggplot2::ylab("Simulated Concentration")
    })
    
    #Graph4: VPC
    graph4 <- shiny::eventReactive(input$SIMU,{
      nlmixr::vpc(
        fit,
        n=600,
        show=list(obs_dv=TRUE),
        xlab="Time (hr)",
        ylab="Concentration"
      )
    })
    
    #Switch from one graph to another
    graphical_output <-
      shiny::eventReactive(
        input$SIMU,
        {
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
        }
      )
    
    #Graphical Panel
    output$PLOT <- shiny::renderPlot({
      graphical_output()
    })
    
    #Download Handler
    output$export <- shiny::downloadHandler(
      filename = function() {
        paste("simplot", input$report, sep = ".")
      },
      content = function(file){
        if(input$report == "pdf") {
          grDevices::pdf(file)
        } else {
          grDevices::png(file)
        }
        print(graphical_output())
        grDevices::dev.off()
      }
    )
    
  }
}
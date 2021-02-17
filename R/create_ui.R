#' Create the shiny user interface for an nlmixr object
#' 
#' @inheritParams run_app
#' @param ... Not used
#' @return A shiny dashboard page
#' @family nlmixr shiny app
#' @export
#' @importFrom shiny actionButton column downloadButton fluidRow h3 h6 numericInput plotOutput radioButtons uiOutput verbatimTextOutput
#' @importFrom shinydashboard box dashboardBody dashboardPage dashboardSidebar menuItem
create_ui <- function(fit, ...) {
  if (!inherits(fit, "nlmixrFitCore")) {
    stop("The model fit must be a 'nlmixrFitCore' object")
  }
  
  shinydashboard::dashboardPage(
    shinydashboard::dashboardHeader(title="nlmixrSim_shiny"),
    shinydashboard::dashboardSidebar(shinydashboard::menuItem("Simulation")),
    
    shinydashboard::dashboardBody(
      shiny::fluidRow(
        shiny::column(
          3,
          shinydashboard::box(
            shiny::h3("Population Parameter Estimates"),
            shiny::uiOutput("THETA"),
            width=12,
            inline=TRUE
          )
        ),
        shiny::column(
          9,
          shinydashboard::box(
            shiny::radioButtons(
              "GT",
              "GT",
              label="Type of graph to plot ",
              choices=c(
                "Spaghetti"="g1",
                "Spaghetti(incl mean)"="g1b",
                "Median and percentiles(no uncertainty)"="g2",
                "Median and percentiles(with uncertainty)"="g3",
                "VPC"="g4"
              ),
              inline=TRUE
            ),
            shiny::plotOutput("PLOT", height=350),
            width=12,
            height=450
          ),
          shinydashboard::box(
            title="Population Data",
            shiny::verbatimTextOutput("fitdat"),
            width=4,
            #height=250,
            #style="overflow-y: scroll; overflow-x: scroll"
            scrollX=TRUE,
            collapsible=TRUE,
            solidHeader=TRUE
          ),
          shinydashboard::box(
            shiny::numericInput(
              "Dose",
              "Dose amount (any units):",
              min=0,
              max=max(fit$simInfo$events$AMT*10),
              value=max(fit$simInfo$events$AMT),
              step=0.05,
              width=100
            ),
            shiny::numericInput(
              "reg",
              "Number of Doses:",
              min=1,
              max=30,
              value=1,
              step=1,
              width=100
            ),
            width=2,
            height=250
          ),
          shinydashboard::box(
            shiny::numericInput("interval", "Dosing Interval (hrs):",min=0, max=720, value=24, step=1, width=100),
            shiny::numericInput("dura", "Simulated Duration (hrs):",min=0, max=720, value=24, step=24, width=100),
            width=2,
            height=250
          ),
          shinydashboard::box(
            shiny::numericInput("n", "Number of Individuals:",min=0, max=1000, value=10, step=50, width=100),
            shiny::numericInput("ns", "Number of Studies:",min=0, max=1000, value=10, step=50, width=100),
            width=2,
            height=250
          ),
          shinydashboard::box(
            shiny::h6("**Click to update plot**"),
            shiny::actionButton("SIMU", "Update Plot"),
            shiny::radioButtons("report","Save your file as:",choices=list("pdf","png")),
            shiny::downloadButton("export"),
            width=2,
            height=250,
            background="navy"
          )
        ),
        fluidRow(
          shinydashboard::box(
            shiny::h3("Population Model ODEs"),
            shiny::verbatimTextOutput("ODE"),
            width=12,
            background="light-blue"
          )
        )
      )
    )
  )
}

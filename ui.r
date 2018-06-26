
library(stats)
library(tidyr)
library(dplyr)
library(knitr)
library(shiny)
library(DT)
library(DiagrammeR)
library(magrittr)

renderInputs <- function(prefix) {
  wellPanel(
    fluidRow(
    
      column(12,
           tags$h3("Inputs"),
             p(actionButton(paste0(prefix, "_", "recalc"),
                            "Re-run simulation", icon("random")
             )),
             numericInput(paste0(prefix, "_", "sampSize"), "Sample size", min = 1, max = 100000, value = 1000),
             sliderInput(paste0(prefix, "_", "RRay"), "True Total Effect (Odds ratio): A->Y", min = 0.0, max = 2.0, value = 1.0, step = 0.01),
             sliderInput(paste0(prefix, "_", "RRuy"), "Odds ratio: U->Y", min = 0.0, max = 2.0, value = 1.0, step = 0.01),
             sliderInput(paste0(prefix, "_", "RRum"), "Odds ratio: U->M", min = 0.0, max = 2.0, value = 1.0, step = 0.01),
             sliderInput(paste0(prefix, "_", "RRam"), "Odds ratio: A->M", min = 0.0, max = 2.0, value = 1.0, step = 0.01),
             sliderInput(paste0(prefix, "_", "RRmy"), "Odds ratio: M->Y", min = 0.0, max = 2.0, value = 1.0, step = 0.01),
             sliderInput(paste0(prefix, "_", "pY1U0"), "Baseline risk", min = 0, max = 1, value =0.1, step = 0.01),
             sliderInput(paste0(prefix, "_", "pM1U0A0"), "Probability of M, when A=0, U=0",min = 0, max = 1, value =0.4, step = 0.01),
             sliderInput(paste0(prefix, "_", "pA"), "Prevalence of A", min = 0, max = 1, value = 0.3, step = 0.1),
             sliderInput(paste0(prefix, "_", "pU"), "Prevalence of U", min = 0.0, max = 1.0, value = 0.2, step = 0.1),
             checkboxInput(paste0(prefix, "_", "isAMint"), "A-M interaction", value = TRUE, width = '100%'),
             sliderInput(paste0(prefix, "_", "AMint"), "Interaction between A & M", min = 0.0, max = 2.0, value = 0.5, step = 0.01)
      )
    )
  )
}

# Define UI for application that plots random distributions

fluidPage(
  titlePanel("Mediation Example", 
             tags$h2("Simulation of the potential for bias when estimating the Controlled Direct Effect")),
  
  sidebarLayout(position = "left", 
    sidebarPanel(
      renderInputs("a")
    ), 
    mainPanel (
      tabsetPanel(
        tabPanel("DAGS",
                 tags$h3("True DAG"),
                 grVizOutput("trueDAG"),
                 tags$h3("Estimated DAG"),
                 grVizOutput("simDAG") ),
        tabPanel("Effect estimates", DT::dataTableOutput("table2")),
        tabPanel("Simulated data", DT::dataTableOutput("table1"))
        
      )
    )
  )
  
)

# fluidPage(# Application title
#           tags$h2("Mediation Example"),
#           p("Simulation of the potential for bias when estimating the Controlled Direct Effect"),
#           hr(),
#           
#           fluidRow(
#             column(6, tags$h3("Inputs")), 
#             column(6, tags$h3("Outputs"))
#           ),
#           fluidRow(
#             column(6, renderInputs("a"),
#                   dataTableOutput("table1")
#                   ),
#             column(6, 
#                    DT::dataTableOutput("table2"),
#                    grVizOutput("trueDAG"),
#                    grVizOutput("simDAG")
#                   )
#           )
# )
# 

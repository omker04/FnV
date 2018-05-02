#get(load('50run_noFineline_noSubsWithinFunction.RData', sys.frame()))
options(warn = -1)
source('setup_packages.R')
source('demandSharePlot.R')
source('upcWiseFlow.R')
source('deleteRecommendation.R')

# packages_required <- c("dplyr","data.table", "ggplot2", "ggrepel", "shiny", "shinydashboard", "grid", "gtable")
# packages_installed <- row.names(installed.packages()) # already installed packages
# packages_to_install <- setdiff(packages_required, packages_installed) # installation required
# 
# lapply(packages_to_install, function(pkg){install.packages(pkg, dependencies = TRUE)}) # installing
# lapply(packages_required, function(pkg){library(pkg, character.only = TRUE)}) # loading in directory
# 
# rm(packages_required, packages_installed, packages_to_install)
# 
# globalEnv <<- globalenv()
# globalEnv <<- load('EnvForApp.RData')
# print(number_of_simulations)
# 
# get(load('EnvForApp.RData'))


header <- dashboardHeader(title = h2(strong("F&V Demand Transference output for Lightbulbs")), titleWidth = "400px")
                          # dropdownMenu(type = "tasks", badgeStatus = "success", icon = icon("home"),
                          #              taskItem(value = 100, color = "green", text = "HOME Page", href = 'hazr10070725k4.cloud.wal-mart.com:3851/index1.html'),
                          #              taskItem(value = 100, color = "aqua", text = "GM-MNL App Page", href = 'hazr10070725k4.cloud.wal-mart.com:3851/GM-MNL'),
                          #              taskItem(value = 100, color = "yellow", text = "GM-CBT App Page", href = 'hazr10070725k4.cloud.wal-mart.com:3851/CBT_v3')
                          # ), 
                          # disable = TRUE)
#header <- dashboardHeader(title = span(tagList(icon("calendar"), "Example")))
sidebar <- dashboardSidebar(disable = TRUE)
body <- dashboardBody(
  #titlePanel(uiOutput("title")),
  useShinyjs(),
  tags$style(type="text/css", ".shiny-output-error { visibility: hidden; }", ".shiny-output-error:before { visibility: hidden; }"),
  tags$style(type='text/css', ".selectize-dropdown-content {max-height: 100px; }"), 
  # fluidPage(fileInput("env1", label = "Select Environment"),
  #           uiOutput("restOfPage")
  #           )
  fluidPage(
    hidden(div(fluidPage(
      #hidden(div(id = "R2",
      fluidRow(
        column(width = 4, selectInput("simulationNbr", label = "Please select the Simulation Nbr", choices = 1:number_of_simulations, width = "100%")),
        column(width = 4, valueBoxOutput("RsqrTrain", width = "100%")),
        column(width = 4, valueBoxOutput("RsqrTest", width = "100%"))
      ),
      #)),
      box(title = "Validation Plots", status = "warning", solidHeader = TRUE, width = "100%", collapsible = TRUE, collapsed = TRUE,
          fluidRow(
            column(width = 6, plotOutput("trainDemShare", height = "300px")),
            column(width = 6, plotOutput("trainSubsParam", height = "300px"))
          )
      ),
      box(title = "Sales-Share Train Plots", status = "primary", solidHeader = TRUE, width = "100%", collapsible = TRUE, collapsed = TRUE,
          fluidRow(
            column(width = 5, plotOutput("trainScatterPlot")),
            column(width = 7, plotOutput("trainLinePlot"))
          )
      ),
      box(title = "Sales-Share Test Plots", status = "info", solidHeader = TRUE, width = "100%", collapsible = TRUE, collapsed = TRUE,
          fluidRow(
            column(width = 5, plotOutput("testScatterPlot")),
            column(width = 7, plotOutput("testLinePlot"))
          )
      )
    ))),
    box(title = "Demand Transference Visualization", status = "success", solidHeader = TRUE, width = "100%", collapsible = TRUE,
        fluidPage(
          # column(width = 12, fixedPanel(sliderInput("cutoff1", label = "Enter the Substitution Cutoff", min = 0, max = 1, step = 0.01, value = 0.8))),
          fluidRow(
            column(width = 9, plotOutput("salesTransfer1", height = "1020px", dblclick = "salesTransfer1DblClick"),
                   fluidRow(column(width = 3, selectInput("n", label = "Select number of items to delete", choices = 1:10, selected = 5)),
                            column(width = 9, tableOutput("deleteReco"))
                   )
            ),
            column(width = 3, uiOutput("absPanel"))
          ))
    )
  )
)

ui <- dashboardPage(header, sidebar, body)

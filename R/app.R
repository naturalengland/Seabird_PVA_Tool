## ########################################################################################
## Project: NEC06986
## Script purpose: PVA Tool interface
## Date v2.0 release: January 2020
## Author: UK Centre for Ecology & Hydrology
## Author: BioSS
## ########################################################################################

# These R packages are required for the shiny interface
library(shiny)
library(shinyjs)
library(shinydashboard)
library(shinyWidgets)
library(DT)
library(plotly)
library(rmarkdown)
library(dplyr)
library(tidyr)

# These R packages are required for the underlying PVA code
library(popbio)

# The functions
source(file.path("Rpackage","functions-highlevel.R",fsep=.Platform$file.sep))
source(file.path("Rpackage","functions-maincalcs.R",fsep=.Platform$file.sep))
source(file.path("Rpackage","functions-sensitivity.R",fsep=.Platform$file.sep))
source(file.path("Rpackage","functions-preprocessing.R",fsep=.Platform$file.sep))
source(file.path("Rpackage","functions-runpva.R",fsep=.Platform$file.sep))
source(file.path("Rpackage","functions-models.R",fsep=.Platform$file.sep))
source("busy-indicator.R")
source("shiny-helptext.R")
source("shiny-helpers.R")

# Read and reformat the input data
sourcepop.ref <<- "National"
svdat <<- read.csv(file.path("Rpackage","lookup-surv.csv",fsep=.Platform$file.sep))
svdat <<- svdat[svdat$Source != "",]
lookup <<- list(Spmeta = read.csv(file.path("Rpackage","lookup-spmeta.csv",fsep=.Platform$file.sep)),
   BS = read.csv(file.path("Rpackage","lookup-BS.csv",fsep=.Platform$file.sep)), Surv = svdat)
modeoptions <<- read.csv(file.path("Rpackage","ModeOptions.csv",fsep=.Platform$file.sep))

# Version numbers
pva_ver <- 4.14
ui_ver <- 1.7
release_ver <- 2.0

# Set some global values
maxnpop <- 10
maxnscen <- 10
minyr <- 1950
maxyr <- 2100
maxyr_ini <- 2050
startyr <- 2000
maxbs <- 10
maxage <- 10
helpsize <- "xs"

sensnames <- c(
   "1 = initial population size (% change in value of parameter)",
   "2 = baseline productivity (% change in value of parameter)",
   "3 = baseline adult survival (% change in value of parameter)",
   "4 = impact of scenario on productivity rate (% change in value of parameter)",
   "5 = impact of scenario on adult survival rate (% change in value of parameter)"
)

# #################################################################################################
# ### Define UI for the tool
# #################################################################################################

ui <- dashboardPage(skin = "green",

   # ##############################################################################
   # ## THE HEADER
   dashboardHeader(title = "PVA Tool",
      titleWidth = 280
   ),

   # ##############################################################################
   # ## THE SIDEBAR

   dashboardSidebar(width = 280,
                    sidebarMenu(id = "sidebarinfo",
                                menuItem("Tool Information",tabName = "documentation", icon = icon("info-sign",lib="glyphicon"),selected = TRUE)
                    ),
                    sidebarMenu(id = "sidebar",
                                menuItem("1. Basic PVA Information", tabName = "par1", icon = icon("wrench", lib = "glyphicon"),selected = FALSE),
                                menuItem("2. Baseline demographic rates", tabName = "par2", icon = icon("wrench", lib = "glyphicon"),selected = FALSE),
                                menuItem("3. Impacts", tabName = "par3", icon = icon("wrench", lib = "glyphicon"), selected = FALSE),
                                menuItem("4. Run", tabName = "runtab", icon = icon("play", lib = "glyphicon"),selected = FALSE),
                                menuItem("5. Tables", tabName = "table", icon = icon("list-alt", lib = "glyphicon"), selected = FALSE),
                                menuItem("6. Charts", tabName = "charts", icon = icon("stats", lib = "glyphicon"), selected = FALSE)
                    )

   ), # end dashboardSidebar


   # ##############################################################################
   # ## THE BODY
   dashboardBody(
      # Use the Shinyjs package
      useShinyjs(),
      tags$head(
         tags$link(rel = "stylesheet", type = "text/css", href = "custom.css"),
         tags$style(type ="text/css", "#downloadlog {color: white; background-color: #337ab7")
      ),

      # container to hold the tabItems
      tabItems(

         ### ##############################################################################
         ### TAB PARAMETERS - BASIC
         ###
         tabItem(tabName = "par1",
            div(id = "BasicTAB",
               h3(textOutput("runrefname_txt1")),

               fluidRow(
                  shinydashboard::box(title = "PVA run details", status = "primary", width = 4, solidHeader = TRUE, collapsible = FALSE,
                     footer = actionBttn("resetALL", label = "Reset all parameters", style = "gradient", color = "warning", icon = icon("refresh")),

                     # Run reference name
                     textInput("runrefname", label = "Reference name", value = "",
                        placeholder="Enter a name for this run...", width = "100%"),

                     # Run_type
                     prettyRadioButtons("run_type",
                        label = "Choose type:",
                        choices = c("Simulation" = 'simplescenarios', "Validation" = 'validation', "Sensitivity" = 'sensitivity.local'),
                        icon = icon("asterisk"), shape = "round",
                        bigger = TRUE, status = "success", animation = "jelly"
                     ),
                     conditionalPanel(condition = "input.run_type=='simplescenarios'",
                        strong("Simulation"),
                        "Use to generate PVAs, based upon specified demographic rate, initial population size(s), and scenarios of impact"
                     ),
                     conditionalPanel(condition = "input.run_type=='validation'",
                        strong("Validation"),
                        "Use to produce ‘historical’ population forecasts using a specified PVA model, for validation purposes against observed data (baseline projections only)"
                     ),
                     conditionalPanel(condition = "input.run_type=='sensitivity.local'",
                        strong("Sensitivity"),
                        "Run a local sensitivity analysis associated with a specified PVA (baseline projections only)"
                     )
                  ),

                  # Model options box - always visible
                  shinydashboard::box(title = "Basic information about the form of PVA", status = "primary",
                     width = 4, solidHeader = TRUE, collapsible = TRUE, collapsed = FALSE,
                     footer = dropdownButton(circle = TRUE, status = "info",
                        size = helpsize, icon = icon("info"), right = FALSE, up = FALSE,
                        helptext$basic
                     ),

                     # model_envstoch envmodels <- c("deterministic", "betagamma", "logitnlogn")
                     prettyRadioButtons("model_envstoch",
                        label = "Model to use for environmental stochasticity:",
                        choices = c("Deterministic" = 'deterministic', "Beta/Gamma" = 'betagamma'),
                        icon = icon("check"),
                        selected = 'betagamma',
                        bigger = TRUE, status = "success", animation = "jelly"
                     ),

                     # model_envstoch
                     prettyRadioButtons("model_dd",
                        label = "Choose model for density dependence:",
                        choices = c("No density dependence" = 'nodd', "log-linear model for density dependence" = 'dduloglin'),
                        icon = icon("check"),
                        selected = 'nodd',
                        bigger = TRUE, status = "success", animation = "jelly"
                     ),

                     # model_demostoch
                     div(class = "swtextinline", "Include demographic stochasticity in model?"),
                     switchInput("model_demostoch",
                        onLabel = "Yes", onStatus = "success",
                        offLabel = "No", offStatus = "warning",
                        value = TRUE, disabled = FALSE, inline = TRUE, size = "mini"
                     )
                  ),

                  # Simulation options box - always visible
                  shinydashboard::box(title = "Simulation", status = "primary",
                     width = 4, solidHeader = TRUE, collapsible = TRUE, collapsed = FALSE,
                     footer = dropdownButton(circle = TRUE, status = "info",
                        size = helpsize, icon = icon("info"), right = FALSE, up = FALSE,
                        helptext$sim
                     ),
                     htmlOutput("note_simn"),
                     numericInput("sim_n", label = "Number of simulations",
                                  value = NA, min = 1, max = NA, step = 1, width = NULL),
                     numericInput("sim_seed", label = "Random seed",
                                  value = NA, min = 0, max = NA, step = 1, width = NULL),
                     numericInput("nburn", label = "Years for burn-in",
                                  value = 0, min = 0, max = 30, step = 1, width = NULL)
                  )
               ),

               fluidRow(
                  # Case Study box
                  shinydashboard::box(title = "Case Studies", status = "info", width = 4,
                     solidHeader = TRUE, collapsible = TRUE,
                     selectizeInput("case_studies", label = 'Select a case study',
                        choices = c("None","Case study 1a","Case study 1b","Case study 1c","Case study 1d")
                     )
                  )
                  # ,shinydashboard::box(title = "FOR TESTING ONLY", status = "danger", width = 4,
                  #    solidHeader = TRUE, collapsible = TRUE,
                  #    HTML("Select a test to set some default values"),
                  #    selectizeInput("testrun", label = '',
                  #       choices = c("None", "Example Simulation", "Example Validation", "Example Sensitivity", "Example 3"
                  #       )
                  #    )
                  # )
               ) # end fluidrow
            )
         ), # tabitem

         ### ######################################################################################
         ### TAB PARAMETERS - BASELINE
         ###
         tabItem(tabName = "par2",
            h3(textOutput("runrefname_txt2")),
            div(id = "BaselineTAB",
               fluidRow(
                  shinydashboard::box(title = "Species-specific values", status = "primary", width = 4,
                     solidHeader = TRUE, collapsible = TRUE,
                     footer = actionBttn("resetBaselineTAB", label = "Reset baseline parameters", style = "gradient", color = "warning", icon = icon("refresh")),
                     fluidRow(
                        column(width = 12,
                           selectizeInput("species", label = "Species",
                              choices = NULL, selected = NULL,
                              options = list(
                                 placeholder = "Select a species",
                                 onInitialize = I('function() { this.setValue(""); }'))
                           ),
                           helptext$species
                        )
                     ),
                     fluidRow(
                        column(width = 6,
                           # (lookup$BS$Regclass == poolregtype.BS)
                           selectizeInput("poolregtypeBS", label = "Region type to use for breeding success data",
                              choices = NULL, selected = NULL,
                              options = list(
                                 placeholder = "Select...",
                                 onInitialize = I('function() { this.setValue(""); }'))
                           )
                        ),
                        column(width = 6,
                           # sourcepop.surv
                           selectizeInput("sourcepopsurv", label = "Available colony-specific survival rate",
                              choices = NULL, selected = NULL,
                              options = list(
                                 placeholder = "Select...",
                                 onInitialize = I('function() { this.setValue(""); }'))
                           ),
                           textOutput("sourcepop_text")
                        )
                     ),
                     fluidRow(
                        column(width = 12,
                           # (lookup$BS$Region == poolregion.BS)
                           selectizeInput("poolregionBS", label = "Sector to use within breeding success region",
                              choices = NULL, selected = NULL,
                              options = list(
                                 placeholder = "Select...",
                                 onInitialize = I('function() { this.setValue(""); }'))
                           ),
                           textOutput("region_text")
                        )
                     )
                  ),

                  # Age at first breeding box - always visible
                  shinydashboard::box(title = "Age at first breeding", status = "primary",
                     width = 4, solidHeader = TRUE, collapsible = TRUE, collapsed = FALSE,
                     footer = dropdownButton(circle = TRUE, status = "info",
                        size = helpsize, icon = icon("info"), right = FALSE, up = FALSE,
                        helptext$afb
                     ),
                     #: input$afb - all model runs
                     sliderInput("afb", label = 'Age at first breeding (years)', 1, maxage,
                        value = 2, step = 1, round = TRUE, ticks = TRUE, width = "100%")
                  ),

                  # Productivity box - always visible
                  shinydashboard::box(title = "Productivity parameters", status = "primary",
                     width = 4, solidHeader = TRUE, collapsible = TRUE, collapsed = FALSE,
                     footer = dropdownButton(circle = TRUE, status = "info",
                        size = helpsize, icon = icon("info"), right = FALSE, up = FALSE,
                        helptext$productivity
                     ),
                     div(class = "swtextinline", "Is there an upper constraint on productivity in the model?"),
                     switchInput("model_prodmax",
                        onLabel = "Yes", onStatus = "success",
                        offLabel = "No", offStatus = "warning",
                        value = TRUE, disabled = FALSE, inline = TRUE, size = "mini"
                     ), br(),br(),
                     #: input$mbs - all model runs
                     div(id = "model_prodmax_txt", "Maximum brood size per pair (chicks) will be constrained to be no greater than:"),
                     sliderInput("mbs", label = NULL, 1, maxbs,
                        value = 4, step = 1, round = TRUE, ticks = TRUE, width = "100%")
                  )
               ),

               fluidRow(
                  shinydashboard::box(title = "Options for subpopulations", status = "primary", width = 4,
                     solidHeader = TRUE, collapsible = TRUE, collapsed = FALSE,
                     footer = dropdownButton(circle = TRUE, status = "info",
                        size = helpsize, icon = icon("info"), right = FALSE, up = FALSE,
                        helptext$subpop
                     ),
                     div(class = "inlinen",
                        #: input$npop - only used for 'simulation'
                        numericInput("npop", label = "Number of subpopulations",
                           value = 1, min = 1, max = maxnpop, step = 1, width = NULL)
                     ),
                     htmlOutput("note_npop"),
                     # demobase_splitpops
                     div(class = "swtextinline", "Are demographic rates applied separately to each subpopulation?"),
                     switchInput("demobase_splitpops",
                        onLabel = "Yes", onStatus = "success",
                        offLabel = "No", offStatus = "warning",
                        value = FALSE, disabled = FALSE, inline = TRUE, size = "mini"
                     )
                  ),
                  shinydashboard::box(title = "Units for initial population size", status = "primary", width = 4,
                     solidHeader = TRUE, collapsible = TRUE, collapsed = FALSE,
                     footer = dropdownButton(circle = TRUE, status = "info",
                        size = helpsize, icon = icon("info"), right = FALSE, up = FALSE,
                        helptext$inputformat
                     ),
                     #iniagetypes <- c("breeding.adults", "breeding.pairs", "all.individuals")
                     prettyRadioButtons("inipop_inputformat",
                        label = "Choose:",
                        choices = c("Breeding adults"="breeding.adults",
                           "Breeding pairs"="breeding.pairs",
                           "All individuals (adults and immatures)"="all.individuals"),
                        selected = "breeding.adults",
                        icon = icon("check"),
                        bigger = TRUE, status = "success", animation = "jelly"
                     )
                  ),

                  shinydashboard::box(title = "Options for immatures", status = "primary", width = 4,
                     solidHeader = TRUE, collapsible = TRUE, collapsed = FALSE,
                     footer = dropdownButton(circle = TRUE, status = "info",
                        size = helpsize, icon = icon("info"), right = FALSE, up = FALSE,
                        helptext$immat
                     ),
                     # demobase_splitimmat
                     div(class = "swtextinline", "Are baseline demographic rates specified separately for immatures?"),
                     switchInput("demobase_splitimmat",
                        onLabel = "Yes", onStatus = "success",
                        offLabel = "No", offStatus = "warning",
                        value = FALSE, disabled = FALSE, inline = TRUE, size = "mini"
                     )
                  )
               ),

               fluidRow(
                  # Subpopulation records
                  lapply(1:maxnpop, function(ia) {
                     conditionalPanel(
                        condition = paste0("input.npop >= ", ia),
                        shinydashboard::box(title=paste0("Baseline demographic rates: ",ia), status = "primary", width = 12, height = "auto", solidHeader = TRUE,
                           collapsible = TRUE, collapsed = FALSE,
                           h4(textOutput(paste0("demobase_tab_txt",ia))),
                           fluidRow(
                              shinydashboard::box(title="Initial population size", status = "info", width = 4, solidHeader = FALSE, collapsible = FALSE, collapsed = FALSE,
                                 numericInput(paste0("inipopyrs_",ia), "Year", min = minyr, max = maxyr_ini, step = 1, value = NULL),
                                 numericInput(paste0("inipopval_",ia), "Initial population size", min = 1, max = 1000000, step = 1, value = NULL)
                              ),
                              shinydashboard::box(title="Productivity rate per pair", status = "info", width = 4, solidHeader = FALSE, collapsible = FALSE, collapsed = FALSE,
                                 numericInput(paste0("pr_mn_",ia), "Mean", min = 0, max = NA, step = 0.001, width = NULL, value = NULL),
                                 numericInput(paste0("pr_sd_",ia), "Standard deviation", min = 0, max = NA, step = 0.001, width = NULL, value = NULL),
                                 numericInput(paste0("pr_idd_",ia), "Effect of density dependence", min = 0, max = NA, step = 0.001, width = NULL, value = NULL)
                              ),
                              shinydashboard::box(title="Adult survival rate", status = "info", width = 4, solidHeader = FALSE, collapsible = FALSE, collapsed = FALSE,
                                 numericInput(paste0("ad_mn_",ia), "Mean", min = 0, max = NA, step = 0.001, value = NULL),
                                 numericInput(paste0("ad_sd_",ia), "Standard deviation", min = 0, max = NA, step = 0.001, value = NULL),
                                 numericInput(paste0("ad_idd_",ia), "Effect of density dependence", min = 0, max = NA, step = 0.001, value = NULL)
                              )
                           ),
                           fluidRow(
                              shinydashboard::box(title="Immature survival rates", status = "info", width = 12, solidHeader = FALSE, collapsible = FALSE, collapsed = FALSE, id = paste0("immat_surv_box_",ia),
                                 column(width = 4,
                                    lapply(1:maxage, function(ik) {
                                       conditionalPanel(condition = paste0("input.afb >= ", ik),
                                          numericInput(paste0("im_mn_",ia,"_",ik), paste0("Mean for age class ",ik-1,"-",ik),
                                             min = 0, max = NA, step = 0.001, value = NULL)
                                       )
                                    })
                                 ),
                                 column(width = 4,
                                    lapply(1:maxage, function(ik) {
                                       conditionalPanel(condition = paste0("input.afb >= ", ik),
                                          numericInput(paste0("im_sd_",ia,"_",ik), paste0("Standard dev. for age class ",ik-1,"-",ik),
                                             min = 0, max = NA, step = 0.001, value = NULL)
                                       )
                                    })
                                 ),
                                 column(width = 4,
                                    lapply(1:maxage, function(ik) {
                                       conditionalPanel(condition = paste0("input.afb >= ", ik),
                                          numericInput(paste0("im_idd_",ia,"_",ik), paste0("Effect of DD for age class ",ik-1,"-",ik),
                                             min = 0, max = NA, step = 0.001, value = NULL)
                                       )
                                    })
                                 )
                              )
                           )
                        )
                     )
                  })
               ),

               fluidRow(
                  conditionalPanel(condition = "input.run_type == 'sensitivity.local'",
                     shinydashboard::box(id = "output_sensitivity_box", title = "Options: Sensitivity", status = "primary", width = 12,
                        solidHeader = TRUE, collapsible = TRUE, collapsed = FALSE,
                        footer = dropdownButton(circle = TRUE, status = "info",
                           size = helpsize, icon = icon("info"), right = FALSE, up = FALSE,
                           helptext$sensitivity
                        ),
                        p("Number of different values to use per parameter for the sensitivity analysis. The default value is 1,
                           which means that for each of the five parameters a value of x% above and
                           x% below the value of the parameter will be simulated, where x is set in the boxes below. A value of 3
                           means that three equally spaced values above and below the value of each parameter, up to the percentage
                           specified by x, will be simulated (e.g., -10%, -6.6%, -3.3%, +3.3%, +6.6%, +10%).
                           Please see user guidance for more information. Values of between 1 and 5 may be used."),
                        numericInput("sens_npvlocal", label = NULL, value = 1, min = 1, max = 5, step = 1, width = "50%"),

                        p("Set ranges for each of the five PVA parameters: specify the range over which each parameter is to be changed
                        as a percentage of the “standard” value of each parameter (e.g., specify ‘10’ to change the parameter by 10%
                        of its value)"),
                        lapply(1:5, function(i) {
                           column(width = 4,
                              numericInput(paste0("sens_pcr_",i), label = sensnames[i], value = NA, min = NA, max = NA, step = NA)
                           )
                        })
                     )
                  )
               )
            )

         ), # end tabitem

         ### ##############################################################################
         ### TAB PARAMETERS - IMPACTS
         ###
         tabItem(tabName = "par3",
            h3(textOutput("runrefname_txt3")),
            div(id = "ImpactTAB",
               fluidRow(
                  shinydashboard::box(title="Scenarios",
                     status = "primary", width = 4, solidHeader = TRUE, collapsible = TRUE, collapsed = FALSE,
                     footer = actionBttn("resetImpactTAB",label = "Reset impact parameters", style = "gradient", color = "warning", icon = icon("refresh")),
                     numericInput("nscen", label = "Number of scenarios of impact",
                        min = 0, max = maxnscen, step = 1, width = NULL, value = 0),
                     htmlOutput("note_nscen"),
                     helpText("The baseline is also included as an additional scenario.")
                  ),

                  shinydashboard::box(title="Options", status = "primary", width = 4, solidHeader = TRUE,
                     collapsible = TRUE, collapsed = FALSE,
                     footer = dropdownButton(circle = TRUE, status = "info",
                        size = helpsize, icon = icon("info"), right = FALSE, up = FALSE,
                        helptext$imp1
                     ),
                     # impacts_splitpops
                     div(class = "swtextinline", "Are impacts applied separately to each subpopulation?"),
                     switchInput("impacts_splitpops",
                        onLabel = "Yes", onStatus = "success",
                        offLabel = "No", offStatus = "warning",
                        value = TRUE, disabled = FALSE, inline = TRUE, size = "mini"
                     ), br(),

                     # impacts_splitimmat
                     div(class = "swtextinline", "Are impacts specified separately for immatures?"),
                     switchInput("impacts_splitimmat",
                        onLabel = "Yes", onStatus = "success",
                        offLabel = "No", offStatus = "warning",
                        value = FALSE, disabled = FALSE, inline = TRUE, size = "mini"
                     ), br(),

                     # impacts_provideses
                     div(class = "swtextinline", "Are standard errors of impacts available?"),
                     switchInput("impacts_provideses",
                        onLabel = "Yes", onStatus = "success",
                        offLabel = "No", offStatus = "warning",
                        value = FALSE, disabled = FALSE, inline = TRUE, size = "mini"
                     ), br(),

                     # "impacts_matchscens"
                     div(class = "swtextinline", "Should random seeds be matched for impact scenarios?"),
                     switchInput("impacts_matchscens",
                        onLabel = "Yes", onStatus = "success",
                        offLabel = "No", offStatus = "warning",
                        value = FALSE, disabled = FALSE, inline = TRUE, size = "mini"
                     )
                  ),

                  shinydashboard::box(title="Form of impact", status = "primary", width = 4, solidHeader = TRUE,
                     collapsible = TRUE, collapsed = FALSE,
                     footer = dropdownButton(circle = TRUE, status = "info",
                        size = helpsize, icon = icon("info"), right = FALSE, up = FALSE,
                        helptext$imp2
                     ),

                     # "impacts_relative"
                     prettyRadioButtons("impacts_relative",
                        label = "Impacts are specified as ",
                        choices = c("Relative" = "relative", "Absolute harvest" = "absolute"),
                        selected = "relative",
                        icon = icon("check"),
                        bigger = TRUE, status = "success", animation = "jelly"
                     )
                  )
               ),

               fluidRow(
                  # Years - impacts_year_start, impacts_year_end
                  shinydashboard::box(title="Years in which impacts are assumed to begin and end",
                     status = "primary", width = 12, solidHeader = TRUE, collapsible = TRUE, collapsed = FALSE,
                     footer = dropdownButton(circle = TRUE, status = "info",
                        size = helpsize, icon = icon("info"), right = FALSE, up = FALSE,
                        helptext$impactyears
                     ),
                     sliderInput("impacts_year", label=NULL, minyr, maxyr,
                        value=c(startyr+20,startyr+50), step = 1,
                        round = TRUE, ticks = TRUE, width = "100%", sep = "", dragRange = TRUE),
                     textOutput("output_year_display")
                  )
               ),
               fluidRow(
                  lapply(1:maxnscen, function(ic) {
                     conditionalPanel(
                        condition = paste0("input.nscen >= ", ic), value = paste0("impsc",ic),
                        shinydashboard::box(title=paste0("Scenario ",LETTERS[ic]), status = "primary", width = 6, height = "auto", solidHeader = TRUE,
                           collapsible = TRUE, collapsed = FALSE,
                           textInput(paste0("scn_nm_str_",ic), label = "Scenario name", value = NULL,
                              placeholder="Enter a reference name (no commas!)...", width = "100%"),

                           lapply(1:maxnpop, function(ia) {
                              conditionalPanel(
                                 condition = paste0("input.npop >= ", ia),
                                 shinydashboard::box(id = paste0("box_",ic,"_",ia), title=paste0("Subpopulation: ",ia), status = "info", width = 12, height = "auto", solidHeader = TRUE,
                                    collapsible = TRUE, collapsed = FALSE,

                                    h4(textOutput(paste0("impacts_tab_txt_", ic, "_", ia))),

                                    shinydashboard::box(title = NULL, status = NULL, width = 4, solidHeader = FALSE, collapsible = FALSE, collapsed = FALSE,
                                       textOutput(paste0("imp_pr_txt_", ic,"_", ia)),
                                       #"impacts_prod_mean"
                                       numericInput(paste0("imp_pr_mn_", ic,"_", ia), label = "Mean", width="auto", step = 0.001, value = NULL),
                                       #"impacts_prod_se"
                                       numericInput(paste0("imp_pr_se_", ic,"_", ia), label = "Standard error", width="auto", step = 0.001, value = NULL)
                                    ),
                                    shinydashboard::box(title = NULL, status = NULL, width = 4, solidHeader = FALSE, collapsible = FALSE, collapsed = FALSE,
                                       textOutput(paste0("imp_ad_txt_", ic,"_", ia)),
                                       #"impacts_survadult_mean"
                                       numericInput(paste0("imp_ad_mn_", ic,"_", ia) , label = "Mean", width="auto", step = 0.001, value = NULL),
                                       #"impacts_survadult_se"
                                       numericInput(paste0("imp_ad_se_", ic,"_", ia) ,label = "Standard error", width="auto", step = 0.001, value = NULL)
                                    ),
                                    shinydashboard::box(title = NULL, status = NULL, width = 4, solidHeader = FALSE, collapsible = FALSE, collapsed = FALSE,
                                       textOutput(paste0("imp_im_txt_", ic,"_", ia)),
                                       #"impacts_survimmat_mean"
                                       numericInput(paste0("imp_im_mn_", ic,"_", ia) ,label = "Mean", width="auto", step = 0.001, value = NULL),
                                       #"impacts_survimmat_se"
                                       numericInput(paste0("imp_im_se_", ic,"_", ia) ,label = "Standard error", width="auto", step = 0.001, value = NULL)
                                    )
                                 )
                              )
                           })
                        )
                     )
                  })
               )
            )
         ), # end tabitem

         ### ######################################################################################
         ### TAB PARAMETERS - RUN SUMMARY
         ###
         tabItem(tabName = "runtab",
            div(id = "RunTAB",
               h3(textOutput("runrefname_txt4")),
               fluidRow(
                  # Output years box - always visible
                  shinydashboard::box(title = "Output: Years", status = "primary", width = 4,
                     solidHeader = TRUE, collapsible = TRUE, collapsed = FALSE,

                     # output_year_start
                     numericInput("output_year_start", label = "First year to include in outputs",
                        value = NA, min = minyr, max = maxyr, step = 1),

                     # output_year_end
                     numericInput("output_year_end", label = "Final year to include in outputs",
                        value = NA, min = minyr, max = maxyr, step = 1),
                     # A note about impact years if applicable
                     textOutput("impacts_year_display")
                  ),

                  # Target population size - optional, can be NULL
                  shinydashboard::box(title = "Output: Target population size (optional)", status = "primary", width = 4,
                     solidHeader = TRUE, collapsible = TRUE, collapsed = FALSE,
                     footer = dropdownButton(circle = TRUE, status = "info",
                        size = helpsize, icon = icon("info"), right = FALSE, up = FALSE,
                        helptext$targetpop
                     ),
                     numericInput("output_popsize_target", label = "Target population size to use in calculating impact metrics",
                        value = NA, min = 0, max = NA, step = NA, width = NULL),
                     numericInput("output_popsize_qe", label = "Quasi-extinction threshold to use in calculating impact metrics",
                        value = NA, min = NA, max = NA, step = NA, width = NULL),
                     helpText("These values must be entered using the same units as specified for outputs. Can be left blank, but are required to calculate metrics 'Quasi_Extinction', 'pc_ImpSims_above_TPS' and TPS_YR.")
                  ),

                  # Output.agetype
                  shinydashboard::box(id = "output_agetype_box",title = "Units for output", status = "primary", width = 4,
                     solidHeader = TRUE, collapsible = TRUE, collapsed = FALSE,
                     footer = dropdownButton(circle = TRUE, status = "info",
                        size = helpsize, icon = icon("info"), right = FALSE, up = FALSE,
                        helptext$outputages
                     ),

                     prettyRadioButtons("output_agetype",
                        label = "Choose",
                        choices = c("Breeding adults" = 'breeding.adults', "Breeding pairs" = 'breeding.pairs',
                                    "All ages separately" = 'age.separated', "Whole population" = 'whole.population'),
                        selected = 'breeding.pairs',
                        icon = icon("check"),
                        bigger = TRUE, status = "success", animation = "jelly"
                     )
                  ),
                  conditionalPanel(condition = "input.run_type=='validation'",
                     shinydashboard::box(id = "output_validation_box", title = "Outputs: Validation data", status = "primary", width = 12,
                        solidHeader = TRUE, collapsible = TRUE, collapsed = FALSE,
                        helpText("Validation data for comparison with PVA. Values can only be added for the baseline and if
                        there is only one subpopulation. Population size must be specified in same units as set for output."),
                        shinydashboard::box(width = 3,
                           lapply(1:5, function(i){
                              fluidRow(
                                 column(width = 6,
                                    numericInput(paste0("output_validation_years_",i), if(i==1) {label = "Year"} else {label = NULL} ,
                                       value = NULL, min = minyr, max = maxyr, step = 1)
                                 ),
                                 column(width = 6,
                                    numericInput(paste0("output_validation_counts_",i), if(i==1) {label = "Population size"} else {label = NULL} ,
                                       value = NULL, min = NA, max = NA, step = NA)
                                 )
                              )
                           })),
                        shinydashboard::box(width = 3,
                           lapply(6:10, function(i){
                              fluidRow(
                                 column(width = 6,
                                    numericInput(paste0("output_validation_years_",i), if(i==6) {label = "Year"} else {label = NULL} ,
                                       value = NULL, min = minyr, max = maxyr, step = 1)
                                 ),
                                 column(width = 6,
                                    numericInput(paste0("output_validation_counts_",i), if(i==6) {label = "Population size"} else {label = NULL} ,
                                       value = NULL, min = NA, max = NA, step = NA)
                                 )
                              )
                           })),
                        shinydashboard::box(width = 3,
                           lapply(11:15, function(i){
                              fluidRow(
                                 column(width = 6,
                                    numericInput(paste0("output_validation_years_",i), if(i==11) {label = "Year"} else {label = NULL} ,
                                       value = NULL, min = minyr, max = maxyr, step = 1)
                                 ),
                                 column(width = 6,
                                    numericInput(paste0("output_validation_counts_",i), if(i==11) {label = "Population size"} else {label = NULL} ,
                                       value = NULL, min = NA, max = NA, step = NA)
                                 )
                              )
                           })),
                        shinydashboard::box(width = 3,
                           lapply(16:20, function(i){
                              fluidRow(
                                 column(width = 6,
                                    numericInput(paste0("output_validation_years_",i), if(i==16) {label = "Year"} else {label = NULL} ,
                                       value = NULL, min = minyr, max = maxyr, step = 1)
                                 ),
                                 column(width = 6,
                                    numericInput(paste0("output_validation_counts_",i), if(i==16) {label = "Population size"} else {label = NULL} ,
                                       value = NULL, min = NA, max = NA, step = NA)
                                 )
                              )
                           })
                        )
                     )
                  )

               ), # end fluidrow

               h3("Population Viability Analysis (PVA)"),

               fluidRow(
                  shinydashboard::box(title = "Summary of parameters", status = "primary", width = 12,
                     solidHeader = TRUE, collapsible = TRUE, collapsed = FALSE,
                     footer = dropdownButton(circle = TRUE, status = "info",
                        size = helpsize, icon = icon("info"), right = FALSE, up = FALSE,
                        helptext$summary),
                     fluidRow(
                        column(width = 6, offset = 3,
                           shinyWidgets::progressBar(id = "progress_bar", value = 0, total = 100, display_pct = TRUE, status = "danger", striped = TRUE, title = "Completed parameters:")
                        )),
                     fluidRow(
                        column(width = 8, offset = 2,
                           htmlOutput("nepva_text"),
                           br(),
                           textOutput("error_text"),
                           br()
                        )),
                     fluidRow(
                        column(width = 2, offset = 5,
                           downloadButton("downloadlog", label = "Run Model (& Generate log)")
                        )
                      )
                  ) # box
               ) # fluidRow
            )
         ), # end tabitem

         ### ##############################################################################
         ### TAB OUTPUT CHARTS
         ###
         tabItem(tabName = "charts",
            h3(textOutput("runrefname_txt5")),
            conditionalPanel(condition = "input.run_type != 'sensitivity.local'",
               shinydashboard::box(title = "Metrics", status = "success", width = 12, height = "690px",
                  solidHeader = FALSE, collapsible = FALSE, collapsed = FALSE,
                  column(width = 12,
                     plotlyOutput("pva_plot_01")
                  )
               )
            ),
            conditionalPanel(condition = "input.run_type == 'sensitivity.local'",
               shinydashboard::box(title = "Sensitivity: Metrics", status = "success", width = 12, height = "1200px",
                  solidHeader = FALSE, collapsible = FALSE, collapsed = TRUE,
                  column(width=12,
                     plotlyOutput("pva_plot_sl_all")
                  )
               )),
            conditionalPanel(condition = "input.run_type=='simplescenarios'",
               shinydashboard::box(title = "Counterfactual of Population Growth Rate (CGR)", status = "success", width = 12, height = "690px",
                  solidHeader = FALSE, collapsible = FALSE, collapsed = FALSE,
                  column(width = 12,
                     htmlOutput("M1_text"),
                     plotlyOutput("pva_plot_M1")
                  )
               )
            ),
            conditionalPanel(condition = "input.run_type=='simplescenarios'",
               shinydashboard::box(title = "Counterfactual of Population size (CPS)", status = "success", width = 12, height = "690px",
                  solidHeader = FALSE, collapsible = FALSE, collapsed = FALSE,
                  column(width = 12,
                     htmlOutput("M2_text"),
                     plotlyOutput("pva_plot_M2")
                  )
               )
            )
         ), # end tabitem

         ### ##############################################################################
         ### TAB OUTPUT TABLE
         ###
         tabItem(tabName = "table",
                 h3(textOutput("runrefname_txt6")),
                 conditionalPanel(condition = "input.run_type=='simplescenarios'",
                                  shinydashboard::box(title = "Table of main PVA metrics", status = "success", width = 12,
                                                      solidHeader = FALSE, collapsible = FALSE, collapsed = TRUE,
                                                      column(width = 12,
                                                             DT::dataTableOutput("resultstableA")
                                                      )
                                  )
                 ),
                 conditionalPanel(condition = "input.run_type=='simplescenarios'",
                                  shinydashboard::box(title = "Table for conservation target-related PVA metrics", status = "success", width = 12,
                                                      solidHeader = FALSE, collapsible = FALSE, collapsed = TRUE,
                                                      column(width = 12,
                                                             DT::dataTableOutput("resultstableM")
                                                      )
                                  )
                 ),
                 conditionalPanel(condition = "input.run_type=='simplescenarios'",
                                  shinydashboard::box(title = "Metric TPS_YR", status = "success", width = 12,
                                                      solidHeader = FALSE, collapsible = FALSE, collapsed = TRUE,
                                                      column(width = 12,
                                                             DT::dataTableOutput("resultstableD")
                                                      )
                                  )
                 ),
                 conditionalPanel(condition = "input.run_type=='simplescenarios'",
                                  shinydashboard::box(title = "Population growth rate & population % change", status = "success", width = 12,
                                                      solidHeader = FALSE, collapsible = FALSE, collapsed = TRUE,
                                                      column(width = 12,
                                                             DT::dataTableOutput("resultstableB")
                                                      )
                                  )
                 ),
                 shinydashboard::box(title = "Full table of outputs", status = "success", width = 12,
                                     solidHeader = FALSE, collapsible = FALSE, collapsed = TRUE,
                                     column(width = 12,
                                            DT::dataTableOutput("resultstableC")
                                     )
                 )
         ), # end tabItem

         ### ##############################################################################
         ### TAB OUTPUT Front Page Information
         ###
         tabItem(tabName = "documentation",
                 h3("A Population Viability Analysis Modelling Tool for Seabird Species"),
                 div(id = "InfoTAB",
                     fluidRow(
                        shinydashboard::box(title=" ", status = "info", width = 12, solidHeader = FALSE, collapsible = FALSE, collapsed = FALSE,
                                            tags$h4("This online version of the PVA Tool allows users to run Population Viability Analyses (PVAs) applicable
                                                    to seabirds at a variety of scales, and can be used to set-up and run bespoke PVA models via the user-friendly
                                                    interface accessed via a standard web browser."),
                                            tags$h4("The tool allows users the flexibility to explore population management-oriented objectives (e.g.
                                                    assessment of impacts, evaluation of management options etc.), as well as being able to explicitly
                                                    highlight the effects on model predictions of different assumptions about the model, data, species
                                                    and populations concerned. It can be used to assess any type of impact that changes survival or
                                                    productivity rates, including as a cull or harvest of a fixed size per year. Impacts may also be
                                                    positive, meaning that mitigation or conservation measures aimed at increasing demographic rates
                                                    may also be modelled. The tool also allows users to conduct PVAs at a range of scales (e.g. breeding
                                                    colony to SPA or wider region), and for non-seabird species."),
                                            tags$h4("The tool produces a range of tabular and graphical outputs for interpreting outputs from PVAs, and a
                                                    facility for using pre-set demographic rates for a number of seabird species, based on currently available demographic data."))
                     ),
                     fluidRow(
                        shinydashboard::box(title="Documentation ", status = "info", width = 4, solidHeader = FALSE, collapsible = FALSE, collapsed = FALSE,
                                            tags$p("For Population Viability Analysis Modelling Tool documentation see", tags$a("Github", href = "https://github.com/naturalengland/Seabird_PVA_Tool", target="_blank",
                                            style="font-weight: bold;color:#000;"), "(opens in a new browser tab)")
                        ),
                        shinydashboard::box(title="Version ", status = "warning", width = 4, solidHeader = FALSE, collapsible = FALSE, collapsed = FALSE,
                                            tags$p("Tool v",release_ver," (Code: v",pva_ver," Interface: v",ui_ver,"). For documentation of changes, issues and comments, see",
                                                   tags$a("Github", href = "https://github.com/naturalengland/Seabird_PVA_Tool", target="_blank", style="font-weight: bold;color:#000;"), "(opens in a new browser tab)")
                        ),
                        shinydashboard::box(title="About the Tool", status = "info", width = 4, solidHeader = FALSE, collapsible = FALSE, collapsed = FALSE,
                                            tags$p("This tool was developed by UKCEH and BioSS, under contract to Natural England and JNCC. See ",
                                            tags$a("Github", href = "https://github.com/naturalengland/Seabird_PVA_Tool/blob/master/LICENCE.md", target="_blank", style="font-weight: bold;color:#000;"),
                                            "(opens in a new browser tab) for licence details."),
                                            tags$p(tags$img(src="ukceh.png", height="40px"), tags$img(src="bioss.jpg", height="40px"), align = "right")
                        )
                     )

                 )
         ) # end tabItem

      ), # end tabItems
      busyIndicator(text = "...Please wait...", img = "busyIndicator/hover.gif", wait = 600)
   ) # dashboardbody
)


# ##############################################################################
# ## Server
# ##############################################################################

server <- function(input, output, session) {

   ## SCRATCH AREA --------------------------------------------------------------------------------



   ## SET DYNAMIC DROPDOWNS -----------------------------------------------------------------------
   rv <- reactiveValues(results = NULL, shortrunrefname = "")
   updateSelectizeInput(session, "species", choices = c("None", unique(as.character(lookup$Spmeta$Species))))

   ## SENSE CHECKS --------------------------------------------------------------------------------
   observe({
      req(!is.na(input$nscen))
      x <- errfn3b(input$nscen)
      if(all(!x, (input$nscen > maxnscen))) {
         textnote <- paste0("<font color=\"#e05959\"><b>", "Note: using maximum (", maxnscen,  ")</b></font>")
         output$note_nscen <- renderText({HTML(textnote) })
      } else {
         output$note_nscen <- renderText({HTML("") })
      }
   })
   observe({
      req(!is.na(input$npop))
      x <- errfn3a(input$npop)
      if(all(!x, (input$npop > maxnpop))) {
         textnote <- paste0("<font color=\"#e05959\"><b>", "Note: using maximum (", maxnpop,  ")</b></font>")
         output$note_npop <- renderText({HTML(textnote) })
      } else {
         output$note_npop <- renderText({HTML("") })
      }
   })
   observe({
      req(!is.na(input$sim_n))
      if(input$sim_n < 1000) {
         textnote <- paste0("Values lower than 1000 are unlikely to lead to stable results and should be used for testing/exploratory purposes only! ")
         output$note_simn <- renderText({HTML(textnote) })
      } else if (input$sim_n > 1000000){
         textnote <- paste0("Values higher than 1000000 are likely to be highly computer intensive to run...")
         output$note_simn <- renderText({HTML(textnote) })
      } else {
         output$note_simn <- renderText({HTML("") })
      }
   })

   ## OBSERVE--------------------------------------------------------------------------------------

   # Specific to sensitivity run...
   observe({
      req(input$run_type == 'sensitivity.local')
      updatePrettyRadioButtons(session, "model_dd", selected = "nodd"); shinyjs::disable("model_dd"); input$model_dd
      updatePrettyRadioButtons(session, "inipop_inputformat", selected = "breeding.adults"); shinyjs::disable("inipop_inputformat")
      updatePrettyRadioButtons(session, "output_agetype", selected = "breeding.adults"); shinyjs::disable("output_agetype")
      updateNumericInput(session, "nscen", value = 1); shinyjs::disable("nscen"); input$nscen
      updateSwitchInput(session, "impacts_matchsens", value = TRUE, disabled = TRUE)
      updateSwitchInput(session, "demobase_splitimmat", value = FALSE, disabled = TRUE); input$demobase_splitimmat
      updateTextInput(session, "scn_nm_str_1", value = "impacted"); shinyjs::disable("scn_nm_str_1")
   })
   # ... and the reverse
   observe({
      req(input$run_type == 'validation' || input$run_type == 'simplescenarios')
      shinyjs::enable("model_dd"); input$model_dd
      shinyjs::enable("inipop_inputformat")
      shinyjs::enable("output_agetype")
      updateSwitchInput(session, "demobase_splitimmat", disabled = FALSE); input$demobase_splitimmat
      shinyjs::enable("scn_nm_str_1")
   })

   # Specific to simplescenarios...
   observe({req(input$run_type == 'simplescenarios')
      shinyjs::enable("output_year_start")
      shinyjs::enable("npop")
      shinyjs::enable("nscen")
      updateSwitchInput(session, "demobase_splitpops", disabled = FALSE)
      updateSwitchInput(session, "impacts_splitpops", disabled = FALSE)
      updateSwitchInput(session, "impacts_splitimmat", disabled = FALSE)
      updateSwitchInput(session, "impacts_provideses", disabled = FALSE)
      updateSwitchInput(session, "impacts_matchsens", disabled = FALSE)
   })
   # ... and the reverse
   observe({
      req(input$run_type == 'validation' || input$run_type == 'sensitivity.local')
      updateNumericInput(session, "npop", value = 1); shinyjs::disable("npop"); input$npop
      updateSwitchInput(session, "demobase_splitpops", disabled = TRUE); input$demobase_splitpops
      updateSwitchInput(session, "impacts_splitpops", value = FALSE, disabled = TRUE); input$impacts_splitpops
      updateSwitchInput(session, "impacts_splitimmat", value = FALSE, disabled = TRUE); input$impacts_splitimmat
      updateSwitchInput(session, "impacts_provideses", value = FALSE, disabled = TRUE); input$impacts_provideses
      updateNumericInput(session, "output_year_start", value = NA); shinyjs::disable("output_year_start")
   })

   # Specific to validation and the reverse
   observe({
      if (input$run_type == 'validation') {
         updateNumericInput(session, "nscen", value = 0); shinyjs::disable("nscen"); input$nscen
         shinyjs::enable("output_validation_box"); shinyjs::show("output_validation_box")
         updatePrettyRadioButtons(session, "impacts_relative", selected = "relative"); shinyjs::disable("impacts_relative")
         updateSliderInput(session, "impacts_year", value = c(2200, 2202)); shinyjs::disable("impacts_year")
         updateSwitchInput(session, "impacts_matchsens", value = FALSE, disabled = TRUE)
      } else {
         shinyjs::enable("impacts_relative")
         shinyjs::disable("output_validation_box")
         shinyjs::hide("output_validation_box")
         shinyjs::enable("impacts_year")
      }
   })

   ##----------------------------------------------------------------------------------------------
   # When the number of populations is one
   observe({
      req(!errfn3a(input$npop))
      if (input$npop == 1) {
         updateSwitchInput(session, "demobase_splitpops", value = FALSE, disabled = TRUE, offStatus = NULL)
         updateSwitchInput(session, "impacts_splitpops", value = FALSE, disabled = TRUE, offStatus = NULL)
      } else {
         if (input$run_type == 'simplescenarios'){
            updateSwitchInput(session, "demobase_splitpops", disabled = FALSE, offStatus = "warning")
            updateSwitchInput(session, "impacts_splitpops", disabled = FALSE, offStatus = "warning")
         }
      }
   })

   # When the number of scenarios is zero
   observe({
      req(!errfn3b(input$nscen))
      v <- c("impacts_matchscens", "impacts_splitimmat", "impacts_provideses", "impacts_splitpops")
      if (input$nscen == 0) {
         shinyjs::disable("impacts_year")
         shinyjs::disable("impacts_relative")
         lapply(v, function(iv) {updateSwitchInput(session, iv, disabled = TRUE)})
      } else {
         shinyjs::enable("impacts_year")
         shinyjs::enable("impacts_relative")
         lapply(v, function(iv) {updateSwitchInput(session, iv, disabled = FALSE)})
         if (input$run_type != 'simplescenarios') {
            updateSwitchInput(session, "impacts_splitpops", disabled = TRUE)
            updateSwitchInput(session, "impacts_splitimmat", disabled = TRUE)
            updateSwitchInput(session, "impacts_provideses", disabled = TRUE)
         }
      }
   })

   ##----------------------------------------------------------------------------------------------

   # Disable baseline immature survival rates if demobase_splitimmat = FALSE
   observe({
      req(input$demobase_splitimmat == FALSE)
      req(!errfn3a(input$npop))
      lapply(1:input$npop, function(i) {
         lapply(1:maxage, function(j) {
            lapply(c("im_mn_","im_sd_","im_idd_"), function(iv) {
               updateNumericInput(session,paste0(iv,i,"_",j), value = NA)
               shinyjs::disable(paste0(iv,i,"_",j))
            })
         })
      })
   })

   # Enable baseline immature survival rates if appropriate
   observe({
      req(input$demobase_splitimmat == TRUE)
      req(input$run_type != "sensitivity.local")
      lapply(c("im_mn_","im_sd_"), function(iv) {lapply(1:maxage, function(j) {shinyjs::enable(paste0(iv,1,"_",j))})})
      isolate({
         if (input$model_dd != 'nodd') {lapply(1:maxage, function(j) {shinyjs::enable(paste0("im_idd_",1,"_",j))})}
         if (input$demobase_splitpops == TRUE) {
            lapply(2:maxnpop, function(i) {
               lapply(c("im_mn_","im_sd_"), function(iv) {lapply(1:maxage, function(j) {shinyjs::enable(paste0(iv,i,"_",j))})})
               if (input$model_dd != 'nodd') {lapply(1:maxage, function(j) {shinyjs::enable(paste0("im_idd_",i,"_",j))})}
            })
         }
      })

   })

   # Disable impacts immature survival rates if impacts_splitimmat = FALSE
   observe({
      req(input$impacts_splitimmat == FALSE)
      req(!errfn3a(input$npop))
      req(!errfn3b(input$nscen))
      lapply(1:input$nscen, function(s) {
         lapply(1:input$npop, function(i) {
            lapply(c("imp_im_mn_","imp_im_se_"), function (v) {
               updateNumericInput(session,paste0(v, s,"_",i), value = NA)
               shinyjs::disable(paste0(v, s,"_", i))
            })
         })
      })
   })

   # Enable impacts immature survival rates if appropriate
   observe({
      req(input$impacts_splitimmat == TRUE)
      req(!errfn3b(input$nscen))
      lapply(1:input$nscen, function(s) {
         shinyjs::enable(paste0("imp_im_mn_", s,"_",1))
         isolate({
            if (input$impacts_provideses) {shinyjs::enable(paste0("imp_im_se_", s,"_",1))}
            if (input$impacts_splitpops == TRUE) {
               lapply(2:maxnpop, function(i) {
                  shinyjs::enable(paste0("imp_im_mn_", s,"_", i))
                  if (input$impacts_provideses) {shinyjs::enable(paste0("imp_im_se_", s,"_", i))}
               })
            }
         })
      })
   })

   ##----------------------------------------------------------------------------------------------

   # Disable baseline impact of density dependence if model_dd = 'nodd'
   observe({
      req(input$model_dd == 'nodd')
      req(!errfn3a(input$npop))
      lapply(1:input$npop, function(i) {
         lapply(c("pr_idd_", "ad_idd_"), function(v) {
            updateNumericInput(session,paste0(v,i), value = NA)
            shinyjs::disable(paste0(v,i))
         })
         lapply(1:maxage, function(j) {
            updateNumericInput(session,paste0("im_idd_",i,"_",j), value = NA)
            shinyjs::disable(paste0("im_idd_",i,"_",j))
         })
      })
   })

   # Enable baseline impact of density dependence if appropriate
   observe({
      req(input$model_dd != 'nodd')
      lapply(c("pr_idd_","ad_idd_"), function(iv) {shinyjs::enable(paste0(iv,1))})
      lapply(1:maxage, function(j) {
         isolate({
            if (input$demobase_splitimmat == TRUE) {shinyjs::enable(paste0("im_idd_",1,"_",j))}
            if (input$demobase_splitpops == TRUE) {
               lapply(2:maxnpop, function(i) {
                  lapply(c("pr_idd_", "ad_idd_"), function(v) {shinyjs::enable(paste0(v,i)) })
                  if (input$demobase_splitimmat == TRUE) {shinyjs::enable(paste0("im_idd_",i,"_",j))}
               })
            }
         })
      })
   })

   ##----------------------------------------------------------------------------------------------

   # Disable variables for pop > 1 if demobase_splitpops = FALSE
   observe({
      req(input$demobase_splitpops == FALSE)
      output[[paste0("demobase_tab_txt",1)]] <- renderPrint({ HTML("Specify productivity and survival rates for all subpopulations") })
      lapply(2:maxnpop, function(i) {
         output[[paste0("demobase_tab_txt",i)]] <- renderPrint({HTML("(See subpopulation 1 for productivity and survival rates)")})
         lapply(c("pr_mn_","pr_sd_","pr_idd_","ad_mn_","ad_sd_","ad_idd_"),
            function(v) {
               updateNumericInput(session,paste0(v,i), value = NA)
               shinyjs::disable(paste0(v,i))
            })
         lapply(c("im_mn_","im_sd_","im_idd_"),
            function(v) {
               lapply(1:maxage, function(j){
                  updateNumericInput(session,paste0(v,i,"_",j), value = NA)
                  shinyjs::disable(paste0(v,i,"_",j))
               })
            })
      })
   })

   # Enable variables for pop > 1 if appropriate
   observe({
      req(input$demobase_splitpops == TRUE)
      output[[paste0("demobase_tab_txt",1)]] <- renderPrint({HTML("Specify productivity and survival rates for subpopulation ",1)})
      lapply(2:maxnpop, function(i) {
         output[[paste0("demobase_tab_txt",i)]] <- renderPrint({HTML("Specify productivity and survival rates for subpopulation ",i)})
         lapply(c("pr_mn_","pr_sd_","ad_mn_","ad_sd_"), function(v) {shinyjs::enable(paste0(v,i))})
         isolate({
            if (input$model_dd != 'nodd') {lapply(c("pr_idd_","ad_idd_"), function(v) { shinyjs::enable(paste0(v,i)) })}
            if (input$demobase_splitimmat == TRUE) {
               lapply(c("im_mn_","im_sd_"), function(v) {lapply(1:maxage, function(j){shinyjs::enable(paste0(v,i,"_",j)) })})
            }
            if (input$model_dd != 'nodd' && input$demobase_splitimmat == TRUE) {
               lapply(1:maxage, function(j){shinyjs::enable(paste0("im_idd_",i,"_",j))})
            }
         })
      })
   })

   ##----------------------------------------------------------------------------------------------

   # Disable variables for pop > 1 if impacts_splitpops = FALSE
   observe({
      req(input$impacts_splitpops == FALSE)
      req(!errfn3b(input$nscen))
      lapply(1:input$nscen, function(s) {
         output[[paste0("impacts_tab_txt_", s, "_", 1)]] <- renderPrint({ HTML("Specify impacts for all subpopulations") })
         lapply(2:maxnpop, function(i) {
            output[[paste0("impacts_tab_txt_", s, "_", i)]] <- renderPrint({ HTML("(See subpopulation 1 for impacts)") })
            lapply(c("imp_pr_mn_","imp_pr_se_","imp_ad_mn_","imp_ad_se_","imp_im_mn_","imp_im_se_"),
               function(v) {
                  updateNumericInput(session,paste0(v,s,"_",i), value = NA)
                  shinyjs::disable(paste0(v,s,"_",i))
               })
         })
      })
   })

   # Enable variables for pop > 1 if appropriate
   observe({
      req(input$impacts_splitpops == TRUE)
      req(!errfn3b(input$nscen))
      lapply(1:input$nscen, function(s) {
         output[[paste0("impacts_tab_txt_", s, "_", 1)]] <- renderPrint({HTML("Specify impacts for subpopulation ",1)})
         if (input$impacts_splitimmat == TRUE) {shinyjs::enable(paste0("imp_im_mn_",s,"_",1)) }
         lapply(2:maxnpop, function(i) {
            output[[paste0("impacts_tab_txt_", s, "_", i)]] <- renderPrint({HTML("Specify impacts for subpopulation ",i)})
            lapply(c("imp_pr_mn_","imp_ad_mn_"), function(v) { shinyjs::enable(paste0(v,s,"_",i)) })
            isolate({
               if (input$impacts_provideses == TRUE) {
                  lapply(c("imp_pr_se_","imp_ad_se_"), function(v) { shinyjs::enable(paste0(v,s,"_",i)) })
               }
               if (input$impacts_splitimmat == TRUE) {shinyjs::enable(paste0("imp_im_mn_",s,"_",i))}
               if (input$impacts_splitimmat == TRUE && input$impacts_provideses == TRUE) {shinyjs::enable(paste0("imp_im_se_",s,"_",i))}
            })
         })
      })
   })

   ##----------------------------------------------------------------------------------------------

   # Disable impacts se When the impacts_provideses == FALSE
   observe({
      req(input$impacts_provideses == FALSE)
      req(!errfn3a(input$npop))
      req(!errfn3b(input$nscen))
      lapply(1:input$nscen, function(s) {
         lapply(1:input$npop, function(i) {
            lapply(c("imp_pr_se_","imp_ad_se_","imp_im_se_"), function(v){
               updateNumericInput(session, paste0(v, s,"_", i), value = NA)
               shinyjs::disable(paste0(v, s,"_", i))
            })
         })
      })
   })

   # Enable impacts se When the impacts_provideses == TRUE
   observe({
      req(input$impacts_provideses == TRUE)
      req(!errfn3b(input$nscen))
      lapply(1:input$nscen, function(s) {
         shinyjs::enable(paste0("imp_pr_se_", s,"_", 1))
         shinyjs::enable(paste0("imp_ad_se_", s,"_", 1))
         isolate({
            if (input$impacts_splitimmat) {shinyjs::enable(paste0("imp_im_se_", s,"_", 1))}
            if (input$impacts_splitpops == TRUE) {
               lapply(2:maxnpop, function(i) {
                  shinyjs::enable(paste0("imp_pr_se_", s,"_", i))
                  shinyjs::enable(paste0("imp_ad_se_", s,"_", i))
                  if (input$impacts_splitimmat) {shinyjs::enable(paste0("imp_im_se_", s,"_", i))}
               })
            }
         })
      })
   })

   # Change impact value type depending if impacts_relative is TRUE or FALSE
   observe({
      req(!errfn3a(input$npop))
      req(!errfn3b(input$nscen))
      lapply(1:input$nscen, function(ic){
         lapply(1:input$npop, function(ip){
            output[[paste0("imp_pr_txt_",ic,"_",ip)]] <- renderPrint({
               if (input$impacts_relative == 'relative') {
                  HTML("Impact on productivity rate per pair")
               } else {
                  HTML("Number of chicks harvested")
               }
            })
            output[[paste0("imp_ad_txt_",ic,"_",ip)]] <- renderPrint({
               if (input$impacts_relative == 'relative') {
                  HTML("Impact on adult survival rate")
               } else {
                  HTML("Number of adults harvested")
               }
            })
            output[[paste0("imp_im_txt_",ic,"_",ip)]] <- renderPrint({
               if (input$impacts_relative == 'relative') {
                  HTML("Impact on immature survival rate")
               } else {
                  HTML("Number of immatures harvested")
               }
            })
         })
      })
   })



   ## OBSERVEEVENTS -------------------------------------------------------------------------------

   # Run a check on completeness (numerical parameters only)
   observeEvent({
      input$sidebar
      input$output_year_end
      input$output_year_start}, {
         req(input$sidebar == "runtab")
         progress <- prog_sum(input)
         tot <- prog_total(input)
         status = "danger"
         tryCatch(
            if (progress/tot >= 1) {
               status = "success"
            } else {
               status = "danger"
            }
         )
         updateProgressBar(session = session, id = "progress_bar", value = progress, total = tot, status = status)
      })

   # Show run reference name at the top of each page
   observeEvent(input$runrefname,{
      rv$shortrunrefname <- make.names(input$runrefname)
      if (input$runrefname=="" || is.na(input$runrefname)) {
         txt1 <- "Parameters - Basic information: (Reference name not set)"
         txt2 <- "Parameters - Baseline demographic rates: (Reference name not set)"
         txt3 <- "Parameters - Impacts: (Reference name not set)"
         txt4 <- "Summary: (Reference name not set)"
         txt5 <- "Output charts: (Reference name not set)"
         txt6 <- "Output table: (Reference name not set)"
      } else {
         txt1 <- paste0("Parameters - Basic information: ",input$runrefname)
         txt2 <- paste0("Parameters - Baseline demographic rate: ",input$runrefname)
         txt3 <- paste0("Parameters - Impacts: ",input$runrefname)
         txt4 <- paste0("Summary: ",input$runrefname)
         txt5 <- paste0("Output charts: ",input$runrefname)
         txt6 <- paste0("Output table: ",input$runrefname)
      }
      shinyjs::html(id = "runrefname_txt1", html = txt1)
      shinyjs::html(id = "runrefname_txt2", html = txt2)
      shinyjs::html(id = "runrefname_txt3", html = txt3)
      shinyjs::html(id = "runrefname_txt4", html = txt4)
      shinyjs::html(id = "runrefname_txt5", html = txt5)
      shinyjs::html(id = "runrefname_txt6", html = txt6)
   })

   # Setting a species - if not one of the input species, reset or ignore
   observeEvent(input$species,{
      req(!(input$species %in% lookup$Spmeta$Species))
      shinyjs::hide("poolregtypeBS"); shinyjs::reset("poolregtypeBS")
      shinyjs::hide("poolregionBS"); shinyjs::reset("poolregionBS"); shinyjs::html(id = "region_text", html = "")
      shinyjs::hide("sourcepopsurv"); shinyjs::reset("sourcepopsurv"); shinyjs::html(id = "sourcepop_text", html = "")
   })

   # Setting a species - dynamic
   observeEvent(input$species,{
      req(input$species %in% lookup$Spmeta$Species)

      ii <- (lookup$Spmeta$Species == input$species)

      ## Species-level metadata
      updateSwitchInput(session, "model_prodmax", value = TRUE)
      updateSliderInput(session, "mbs", value = lookup$Spmeta$MBS[ii])
      updateSliderInput(session, "afb", value = lookup$Spmeta$AFB[ii])

      isolate({
         this_Regclass <- subset(lookup$BS,Species == input$species,select = c("Regclass"))
         RegclassList <- unique(this_Regclass$Regclass)
         updateSelectizeInput(session, "poolregtypeBS", choices = RegclassList)
         shinyjs::show("poolregtypeBS")
         shinyjs::hide("poolregionBS"); shinyjs::reset("poolregionBS"); shinyjs::html(id = "region_text", html = "")

         this_sourcepopList <- subset(lookup$Surv, Species == input$species, select = c("Source"))
         sourcepopList <- unique(this_sourcepopList$Source)
         updateSelectizeInput(session, "sourcepopsurv", choices = sourcepopList)
         shinyjs::show("sourcepopsurv"); shinyjs::html(id = "sourcepop_text", html = "")
      })

   })

   # Set the poolregtypeBS after species selected
   observeEvent(input$poolregtypeBS,{
      req(input$species %in% lookup$Spmeta$Species)
      req(input$poolregtypeBS)

      this_RegionList <- subset(lookup$BS,(Species == input$species & Regclass == input$poolregtypeBS),select = c("Region"))
      RegionList <- unique(this_RegionList$Region)
      updateSelectizeInput(session, "poolregionBS", choices = RegionList)
      shinyjs::show("poolregionBS"); shinyjs::html(id = "region_text", html = "")
   })

   # When all three species options are set...
   observeEvent(input$poolregionBS,{
      req(input$species %in% lookup$Spmeta$Species)
      req(input$poolregtypeBS)
      req(input$poolregionBS)

      # Wipe any previous numbers
      updateNumericInput(session,"npop", value = 1)
      updateNumericInput(session,"pr_mn_1",value = NA)
      updateNumericInput(session,"pr_sd_1",value = NA)

      ## Breeding success
      isolate({
         jj <- (lookup$BS$Species == input$species) & (lookup$BS$Regclass == input$poolregtypeBS) & (lookup$BS$Region == input$poolregionBS)
         if(any(jj)){
            updateNumericInput(session,"pr_mn_1",value = lookup$BS$BS.mean[jj])
            updateNumericInput(session,"pr_sd_1",value = lookup$BS$BS.sd[jj])
            shinyjs::html(id = "region_text", html = "<font color=\"#3CB371\"><b>Species breeding success values set</b></font>")
         } else {
            updateNumericInput(session,"pr_mn_1",value = NA)
            updateNumericInput(session,"pr_sd_1",value = NA)
            shinyjs::html(id = "region_text", html = "<font color=\"#000000\">No default values available for breeding success</font>")
         }
      })
   })

   # Update some settings when species survival is selected
   observeEvent(input$sourcepopsurv,{
      req(input$sourcepopsurv)

      # Wipe any previous numbers
      updateNumericInput(session,"npop", value = 1)
      updateSwitchInput(session,"demobase_splitimmat", value = TRUE)
      updateNumericInput(session,"ad_mn_1",value = NA)
      updateNumericInput(session,"ad_sd_1",value = NA)
      for (i in 1:maxage){
         updateNumericInput(session,paste0("im_mn_1_",i),value = NA)
         updateNumericInput(session,paste0("im_sd_1_",i),value = NA)
      }

      # Survival
      lookup$Surv$Age.hi[is.na(lookup$Surv$Age.hi)] <- Inf
      dat <- lookup$Surv[(lookup$Surv$Species == input$species) & (as.character(lookup$Surv$Source) == input$sourcepopsurv),]
      ref <- lookup$Surv[(lookup$Surv$Species == input$species) & (as.character(lookup$Surv$Source) == sourcepop.ref),]

      demobase.survadult <- getsurvdefaults(dat = dat, ref = ref, age.lo = input$afb, age.hi = NULL)
      updateNumericInput(session,"ad_mn_1",value = as.numeric(demobase.survadult["SV.mean"]))
      updateNumericInput(session,"ad_sd_1",value = as.numeric(demobase.survadult["SV.SD"]))

      demobase.survimmat <- data.frame(mean = rep(NA, input$afb), sd = rep(NA, input$afb))
      for(k in 1:input$afb){
         demobase.survimmat[k,] <- getsurvdefaults(dat = dat, ref = ref, age.lo = (k - 1), age.hi = k)
         updateNumericInput(session,paste0("im_mn_1_",k),value = demobase.survimmat[k,"mean"])
         updateNumericInput(session,paste0("im_sd_1_",k),value = demobase.survimmat[k,"sd"])
      }

      shinyjs::html(id = "sourcepop_text", html = "<font color=\"#3CB371\"><b>Species survival values set</b></font>")
   })

   # Disable maximum brood size if model_prodmax is false
   observeEvent(input$model_prodmax,{
      if (input$model_prodmax) {
         shinyjs::show("mbs")
         shinyjs::show("model_prodmax_txt")
      } else {
         shinyjs::hide("mbs")
         shinyjs::hide("model_prodmax_txt")
      }
   })

   # When impacts year range changes -
   observeEvent({
      input$inipopyrs_1
      input$inipopyrs_2
      input$inipopyrs_3
      input$inipopyrs_4
      input$inipopyrs_5
      input$inipopyrs_6
      input$inipopyrs_7
      input$inipopyrs_8
      input$inipopyrs_9
      input$inipopyrs_10
      input$impacts_year
      input$output_year_end},{
         if (input$nscen > 0) {
            popyearlimit <- max(inipop_vec(input,"inipopyrs_")) + 1
            if (!is.na(popyearlimit)) {
               if (popyearlimit > input$impacts_year[1]){
                  mintext <- paste0("<font color=\"#FF0000\"><b>Error: Impacts must start in year ", as.character(popyearlimit)," or later. ","</b></font>")
                  shinyjs::html("error_text", mintext)
               } else {
                  mintext <- paste0("<font color=\"#3CB371\"><b>Impacts must start in year ", as.character(popyearlimit)," or later. ","</b></font>")
                  shinyjs::html("error_text", " ")
               }
            } else {
               mintext <- paste0("")
            }

            if (any(is.na(input$output_year_end),is.null(input$output_year_end))) {
               end_year <- "not set"
               shinyjs::html("output_year_display", paste0(mintext,"Final year for output is ", end_year, "(see tab 4. Run)"))
               shinyjs::html("impacts_year_display", paste0("Impacts begin in ", input$impacts_year[1]," and end in ",input$impacts_year[2]))
            } else {
               end_year <- as.character(input$output_year_end)
               if (input$output_year_end < input$impacts_year[1] + 1) {
                  shinyjs::html("output_year_display", paste0(mintext,"<font color=\"#FF8C00\"><b>Warning: Final year for output is ", end_year,"</b></font>"))
                  shinyjs::html("impacts_year_display", paste0("<font color=\"#FF8C00\"><b>Warning: Impacts begin in ", input$impacts_year[1],
                     " and end in ",input$impacts_year[2],"</b></font>"))
               } else {
                  shinyjs::html("output_year_display",  paste0(mintext,"<font color=\"#3CB371\"><b>Final year for output is ", end_year,"</b></font>"))
                  shinyjs::html("impacts_year_display", paste0("<font color=\"#3CB371\"><b>Impacts begin in ", input$impacts_year[1]," and end in ",input$impacts_year[2],"</b></font>"))
               }
            }

         } else {
            shinyjs::html("output_year_display", " ")
            shinyjs::html("impacts_year_display", " ")
         }
      })

   # Reset buttons
   observeEvent(input$resetImpactTAB,{
      shinyjs::reset("ImpactTAB")
   })
   observeEvent(input$resetBaselineTAB,{
      shinyjs::reset("BaselineTAB")
   })
   observeEvent(input$resetALL,{
      shinyjs::reset("BasicTAB")
      shinyjs::reset("BaselineTAB")
      shinyjs::reset("ImpactTAB")
      shinyjs::reset("RunTAB")
   })

   ## LOG, CHART AND TABLES -----------------------------------------------------------------------

   output$nepva_text <- renderText({
      HTML(paste0("The progress bar indicates how many of the mandatory numeric parameters have been
         completed based on the run type, number of scenarios and number of subpopulations specified.
         Note that the RUN button can be clicked at any time but will give an error message if
         any mandatory parameters are unset!","<br><br>A document listing all parameters is generated
         with each run."))
   })

   # The table(s) of the output

   # METRICS
   output$resultstableA <-  DT::renderDataTable({
      req(class(rv$results) == 'data.frame')
      req(input$run_type == "simplescenarios")
      tabledata <- rv$results[(rv$results$Year == max(rv$results$Year)) & (rv$results$Scenario != "baseline"),
                              c("Age_Class", "Scenario", "CGR_Median", "CPS_Median","QuantileUNIMP50pcIMP","QuantileIMP50pcUNIMP","Quasi_Extinction","pc_ImpSims_above_TPS")]

      DT::datatable(tabledata,
                    extensions = c('Buttons'),
                    selection = 'multiple', rownames = FALSE, filter = 'none', class = 'compact row-border hover',
                    fillContainer = FALSE,
                    options = list(dom = 'Bit', scrollY = TRUE,  scrollX = TRUE, pageLength = 20,
                                   searching = FALSE, buttons = list('copy',
                                                                     list(extend = 'excel', filename = paste0("NEPVA_", rv$shortrunrefname,"_metrics_",Sys.Date())),
                                                                     list(extend = 'csv', filename = paste0("NEPVA_", rv$shortrunrefname,"_metrics_",Sys.Date()))),
                                   initComplete = JS(
                                      "function(settings, json) {",
                                      "$(this.api().table().header()).css({'background-color': '#000', 'color': '#fff'});",
                                      "}"))
      ) %>%
         formatRound(c("QuantileUNIMP50pcIMP", "QuantileIMP50pcUNIMP", "Quasi_Extinction", "pc_ImpSims_above_TPS"),1) %>%
         formatSignif(c("CGR_Median", "CPS_Median"),3)
   })

   # METRIC TPS_YR (M7)
   output$resultstableD <-  DT::renderDataTable({
      req(class(rv$results) == 'data.frame')
      req(input$run_type == "simplescenarios")
      tabledata <- calc.m7(rv$results)

      DT::datatable(tabledata,
         extensions = c('Buttons'),
         selection = 'multiple', rownames = FALSE, filter = 'none', class = 'compact row-border hover',
         fillContainer = FALSE,
         options = list(dom = 'Bit', scrollY = TRUE,  scrollX = TRUE, pageLength = 10,
            searching = FALSE, buttons = list('copy',
               list(extend = 'excel', filename = paste0("NEPVA_", rv$shortrunrefname,"_metrics_",Sys.Date())),
               list(extend = 'csv', filename = paste0("NEPVA_", rv$shortrunrefname,"_metrics_",Sys.Date()))),
            initComplete = JS(
               "function(settings, json) {",
               "$(this.api().table().header()).css({'background-color': '#000', 'color': '#fff'});",
               "}"))
      )
   })

   # METRICS Quasi_Extinction, pc_ImpSims_above_TPS, M7
   output$resultstableM <-  DT::renderDataTable({
      req(class(rv$results) == 'data.frame')
      req(input$run_type == "simplescenarios")
      tabledata <- rv$results[, c("Year","Age_Class", "Scenario", "Quasi_Extinction", "pc_ImpSims_above_TPS")]

      DT::datatable(tabledata,
                    extensions = c('Buttons'),
                    selection = 'multiple', rownames = FALSE, filter = 'none', class = 'compact row-border hover',
                    fillContainer = FALSE,
                    options = list(dom = 'Bit', scrollY = TRUE,  scrollX = TRUE, pageLength = 10,
                                   searching = FALSE, buttons = list('copy',
                                                                     list(extend = 'excel', filename = paste0("NEPVA_", rv$shortrunrefname,"_metrics_",Sys.Date())),
                                                                     list(extend = 'csv', filename = paste0("NEPVA_", rv$shortrunrefname,"_metrics_",Sys.Date()))),
                                   initComplete = JS(
                                      "function(settings, json) {",
                                      "$(this.api().table().header()).css({'background-color': '#000', 'color': '#fff'});",
                                      "}"))
      )
   })



   # Population growth rate & population % change
   output$resultstableB <-  DT::renderDataTable({
      req(class(rv$results) == 'data.frame')
      tabledata <- rv$results[(rv$results$Year == max(rv$results$Year)),
         c("Age_Class", "Scenario", "Popsize_Median", "Popsize_2.5%_quantile", "Popsize_97.5%_quantile",
           "Annual_GR_Median", "Annual_GR_LCI", "Annual_GR_UCI",
           "pc_Pop_Change_Median", "pc_Pop_Change_LCI", "pc_Pop_Change_UCI")]

      DT::datatable(tabledata,
         extensions = c('Buttons'),
         selection = 'multiple', rownames = FALSE, filter = 'none', class = 'compact row-border hover',
         fillContainer = FALSE,
         options = list(dom = 'Bit', scrollY = TRUE,  scrollX = TRUE, buttons = list(I('colvis'), 'copy',
            list(extend = 'excel', filename = paste0("NEPVA_", rv$shortrunrefname,"_pop_",Sys.Date())),
            list(extend = 'csv', filename = paste0("NEPVA_", rv$shortrunrefname,"_pop_",Sys.Date()))),
            initComplete = JS(
               "function(settings, json) {",
               "$(this.api().table().header()).css({'background-color': '#000', 'color': '#fff'});",
               "}")
            ),
         # callback = JS("var tips = ['tooltip1', 'tooltip2', 'tooltip3', 'tooltip4', 'tooltip5'],
         #                    firstRow = $('#resultstableB thead tr th');
         #                    for (var i = 0; i < tips.length; i++) {
         #                      $(firstRow[i]).attr('title', tips[i]);
         #                    }"),
      ) %>%
         formatRound(c("Popsize_Median","Popsize_2.5%_quantile","Popsize_97.5%_quantile"), 0) %>%
         formatRound(c("Annual_GR_Median", "Annual_GR_LCI", "Annual_GR_UCI"),3) %>%
         formatRound(c("pc_Pop_Change_Median", "pc_Pop_Change_LCI", "pc_Pop_Change_UCI"),1)
   })

   output$resultstableC <-  DT::renderDataTable({
      req(class(rv$results) == 'data.frame')
      DT::datatable(rv$results,
         extensions = c('Buttons'),
         selection = 'multiple', rownames = FALSE, filter = 'none', class = 'compact row-border hover',
         fillContainer = FALSE,
         options = list(dom = 'lBfiptp', scrollY = TRUE,  scrollX = TRUE, language = list(search = 'Find:'),
            pageLength = 20, lengthMenu = list(c(10, 20, 50, 100, -1), list('10', '20', '50', '100', 'All')),
            searching = TRUE, buttons = list(I('colvis'), 'copy',
               list(extend = 'excel', filename = paste0("NEPVA_", rv$shortrunrefname,"_full_",Sys.Date())),
               list(extend = 'csv', filename = paste0("NEPVA_", rv$shortrunrefname,"_full_",Sys.Date()))),
            initComplete = JS(
               "function(settings, json) {",
               "$(this.api().table().header()).css({'background-color': '#000', 'color': '#fff'});",
               "}"))
      ) %>%
         formatRound(paste("Popsize", c("Mean", "Median", paste0(c(1, 2.5, 5, 10, 20, 25, 33, 66, 75, 80, 90, 95, 97.5, 99), "%_quantile")), sep="_"), 0) %>%
         formatRound(c("Annual_GR_Median","Annual_GR_Mean", "Annual_GR_LCI", "Annual_GR_UCI"),3) %>%
         formatRound(c("pc_Pop_Change_Median", "pc_Pop_Change_Mean", "pc_Pop_Change_LCI", "pc_Pop_Change_UCI", "QuantileUNIMP50pcIMP", "QuantileIMP50pcUNIMP", "Quasi_Extinction", "pc_ImpSims_above_TPS"),1) %>%
         formatSignif(c("Annual_GR_SD","CGR_SD","CPS_SD","pc_Pop_Change_SD", "Popsize_SD"),2) %>%
         formatSignif(c("CGR_Median","CGR_Mean","CGR_LCI","CGR_UCI","CPS_Median","CPS_Mean","CPS_LCI","CPS_UCI"),3)
   },server = FALSE)

   # The Parameter settings report
   output$downloadlog <- downloadHandler(
      filename = function() {paste0('NEPVA_log_',rv$shortrunrefname,"_",format(Sys.time(), '%Y%m%d_%H%M'), '.docx')},
      content = function(file) {
         src <- normalizePath('NEPVAlog.Rmd')
         owd <- setwd(tempdir())
         on.exit(setwd(owd))
         file.copy(src, 'NEPVAlog.Rmd', overwrite = TRUE)
         out <- render('NEPVAlog.Rmd', output_format = word_document(),
            params=list(d = Sys.time(),ui_ver = ui_ver, pva_ver = pva_ver),
            quiet = TRUE)
         file.rename(out, file)
      }
   )

   # Charts with plotly ---------------------------------------------------------------------------

   # Main metric plot
   output$pva_plot_01 <- renderPlotly({
      req(class(rv$results) == 'data.frame')
      req(input$run_type == "simplescenarios" | input$run_type == "validation")

      if (input$output_agetype == 'breeding.adults'){
         PVA <- rv$results[rv$results$Age_Class == "breeding.adults",]
         outtype <-  "breeding adults"
      } else if (input$output_agetype == 'breeding.pairs') {
         PVA <- rv$results[rv$results$Age_Class == "breeding.pairs",]
         outtype <-  "breeding pairs"
      } else {
         PVA <- rv$results[rv$results$Age_Class == "whole.population",]
         outtype <-  "whole population"
      }

      gg_pva <- ggplot(data = PVA) + theme_bw() +
         geom_line(aes(x = Year, y = Popsize_Median, colour = Scenario)) +
         geom_line(aes(x = Year, y = PVA[,"Popsize_2.5%_quantile"], colour = Scenario), linetype="dotted") +
         geom_line(aes(x = Year, y = PVA[,"Popsize_97.5%_quantile"], colour = Scenario), linetype="dotted") +
         ylab(paste("Projected population size (",outtype,")")) + ggtitle("Population Size")


      if (input$run_type == "validation") {
         validation_years <- numeric()
         validation_counts <- numeric()
         for(x in 1:20) {
            if (is.na(input[[paste0("output_validation_years_",x)]])
               || is.null(input[[paste0("output_validation_years_",x)]])) {
            } else {
               validation_years <- c(validation_years, input[[paste0("output_validation_years_",x)]])
               validation_counts <- c(validation_counts, input[[paste0("output_validation_counts_",x)]])
            }
         }
         df <- data.frame(year = validation_years, counts = validation_counts)
         if (length(df) > 1){
            gg_pva <- gg_pva + geom_point(data = df, aes(year, counts))
         }
      }

      if (!is.na(input$output_popsize_target) & input$output_popsize_target > 0) {
         gg_pva <- gg_pva + geom_hline (yintercept = input$output_popsize_target, colour="grey", linetype="dotdash") +
            annotate ("text", label = "Target population size", x = mean(PVA$Year), y = input$output_popsize_target, colour = "grey")
      }

      plotly_pva <- ggplotly(gg_pva, autosize = T, height = 600) %>%
         layout(yaxis = list(tickmode="auto", autorange=TRUE),
            margin = list(t=100,r=100,l=100,unit="pt"))
   })

   ## Sensitivity chart
   ## #**DL**

   output$pva_plot_sl_all <- renderPlotly({
      req(class(rv$results) == 'data.frame')
      req(input$run_type == "sensitivity.local")

      # Only plot adult data even if juveniles are output separately
      if (input$output_agetype == 'breeding.adults'){
         tabledata <- rv$results[rv$results$Age_Class == "breeding.adults",]

      } else if (input$output_agetype == 'breeding.pairs') {
         tabledata <- rv$results[rv$results$Age_Class == "breeding.pairs",]

      } else {
         tabledata <- rv$results[rv$results$Age_Class == "whole.population",]

      }

      baselinerun <- tabledata[tabledata$parname == "standard",] %>%
         slice(rep(1:n(), each = nlevels(tabledata$parname)))
      baselinerun$parname <- levels(tabledata$parname)

      baselinerun <- baselinerun %>% mutate(
         pcchange = case_when(
            pcchange.inipop.vals == 0 ~ pcchange.inipop.vals,
            pcchange.demobase.prod.mean == 0 ~ pcchange.demobase.prod.mean,
            pcchange.demobase.survadult.mean == 0 ~ pcchange.demobase.survadult.mean,
            pcchange.impact.prod.mean == 0 ~ pcchange.impact.prod.mean,
            pcchange.impact.survadult.mean == 0 ~ pcchange.impact.survadult.mean
         )) %>% filter(parname != "standard") %>%
         select(parname, pcchange, CGR_Median, CPS_Median, QuantileUNIMP50pcIMP, QuantileIMP50pcUNIMP, Quasi_Extinction, pc_ImpSims_above_TPS) %>%
         gather(key, value = "Metric", CGR_Median:pc_ImpSims_above_TPS)

      tabledata <- tabledata %>% mutate(
         pcchange = case_when(
            pcchange.inipop.vals != 0 ~ pcchange.inipop.vals,
            pcchange.demobase.prod.mean != 0 ~ pcchange.demobase.prod.mean,
            pcchange.demobase.survadult.mean != 0 ~ pcchange.demobase.survadult.mean,
            pcchange.impact.prod.mean != 0 ~ pcchange.impact.prod.mean,
            pcchange.impact.survadult.mean != 0 ~ pcchange.impact.survadult.mean
         )) %>% filter(parname != "standard") %>%
         select(parname, pcchange, CGR_Median, CPS_Median, QuantileUNIMP50pcIMP, QuantileIMP50pcUNIMP, Quasi_Extinction, pc_ImpSims_above_TPS) %>%
         gather(key, value = "Metric", CGR_Median:pc_ImpSims_above_TPS)

      tabledata <- rbind(tabledata, baselinerun)
      levels(tabledata$parname) <- c(levels(tabledata$parname),"Initial population size",
         "Baseline productivity (mean)","Baseline adult survival (mean)",
         "Impact on productivity (mean)","Impact on survival (mean)")
      tabledata$parname[(tabledata$parname)=="inipop.vals"] <- "Initial population size"
      tabledata$parname[(tabledata$parname)=="demobase.prod.mean"] <- "Baseline productivity (mean)"
      tabledata$parname[(tabledata$parname)=="demobase.survadult.mean"] <- "Baseline adult survival (mean)"
      tabledata$parname[(tabledata$parname)=="impact.prod.mean"] <- "Impact on productivity (mean)"
      tabledata$parname[(tabledata$parname)=="impact.survadult.mean"] <- "Impact on survival (mean)"

      gg_pva <- ggplot() + theme_light(base_size = 12) +
         geom_line(data = tabledata, aes(x = pcchange, y = Metric , colour = parname), show.legend = FALSE) +
         geom_point(data = tabledata, aes(x = pcchange, y = Metric, colour = parname), show.legend = FALSE) +
         facet_wrap(key ~ ., ncol = 2, scales = "free", strip.position = "left") +
         theme(strip.placement = "outside", panel.spacing = unit(1, "cm")) +
         xlab("\n\n % change in input parameter") + ylab(NULL)  +
         ggtitle("\n\n")

      plotly_pva <- ggplotly(gg_pva, autosize = T, height = 1100) %>%
         layout(legend = list(orientation = "v", x = 0.35, y = -0.3),
            margin = list(t = 100, b = 100, r = 110, l = 90, unit="pt"))

   })


   ## Counterfactual of Population Growth Rate plot
   output$pva_plot_M1 <- renderPlotly({
      req(class(rv$results) == 'data.frame')
      req(input$run_type == 'simplescenarios')
      req(input$nscen > 0)

      # Only plot adult data even if juveniles are output separately
      if (input$output_agetype == 'breeding.adults'){
         PVA <- rv$results[rv$results$Age_Class == "breeding.adults",]

      } else if (input$output_agetype == 'breeding.pairs') {
         PVA <- rv$results[rv$results$Age_Class == "breeding.pairs",]

      } else {
         PVA <- rv$results[rv$results$Age_Class == "whole.population",]

      }

      # If >1 scenario specified, and only one subpopulation, or multiple populations all with the same impact
      if (input$npop == 1 | input$impacts_splitpops == FALSE) {
         output$M1_text <- renderText({HTML("") })
         tableindata <- data.frame(Scenario = rv$impacts_scennames, Impact = rv$impacts_survadult_mean[1,])
         tableoutdata <- PVA[PVA$Scenario != "baseline", c("Year", "Scenario", "CGR_Median", "CGR_LCI","CGR_UCI")]
         tableoutdata$Year <- tableoutdata$Year - input$impacts_year[1]
         tableoutdata$ImpactYear <- paste0("Yr ",as.character(tableoutdata$Year))

         a <- tableoutdata$Year == max(tableoutdata$Year)
         b <- tableoutdata$Year%%5 == 0
         c <- tableoutdata$Year > 0
         yrs <- unique(tableoutdata[ c & (a|b),"Year"])
         tabledata <- merge(tableindata, tableoutdata[c & (a|b),], by = "Scenario")

         #    X-axis is the impact on adult survival for each of the scenarios
         #    Y-axis is CGR_Median (solid), CGR_LCI (dotted line) and CGR_UCI (dotted line)
         gg_pva_p1 <- ggplot() + theme_bw() +
            geom_line(data=tabledata,aes(x=Impact, y=CGR_Median, colour = ImpactYear)) +
            geom_point(data=tabledata,aes(x=Impact, y=CGR_Median, colour = ImpactYear)) +
            geom_line(data=tabledata,aes(x=Impact, y=CGR_LCI, colour = ImpactYear),linetype="dotted") +
            geom_line(data=tabledata,aes(x=Impact, y=CGR_UCI, colour = ImpactYear), linetype = "dotted")  +
            ylab("CGR (median and confidence interval) \n\n") + ggtitle("Counterfactual of Population Growth Rate")

         plotly_pva <- ggplotly(gg_pva_p1, autosize = T, height = 600) %>%
            layout(yaxis = list(tickmode="auto", autorange=TRUE),
               margin = list(t=100,r=110,l=90,unit="pt"))
      } else {
         # Plot would be difficult to interpret!
         output$M1_text <- renderText({HTML("When subpopulations have different impacts, charts cannot be drawn (see Report for details)") })
         plotly_pva <- ggplotly(ggplot() + theme_bw())
      }

   })

   ## Counterfactual of population size (Metric M2)
   output$pva_plot_M2 <- renderPlotly({
      req(class(rv$results) == 'data.frame')
      req(input$run_type == 'simplescenarios')
      req(input$nscen > 0)

      # Only plot adult data even if juveniles are output separately
      if (input$output_agetype == 'breeding.adults'){
         PVA <- rv$results[rv$results$Age_Class == "breeding.adults",]

      } else if (input$output_agetype == 'breeding.pairs') {
         PVA <- rv$results[rv$results$Age_Class == "breeding.pairs",]

      } else {
         PVA <- rv$results[rv$results$Age_Class == "whole.population",]

      }

      # If >1 scenario specified, and only one subpopulation, or multiple populations all with the same impact
      if (input$npop == 1 | input$impacts_splitpops == FALSE) {
         output$M2_text <- renderText({HTML("") })
         tableindata <- data.frame(Scenario = rv$impacts_scennames, Impact = rv$impacts_survadult_mean[1,])
         tableoutdata <- PVA[PVA$Scenario != "baseline", c("Year", "Scenario", "CPS_Median", "CPS_LCI","CPS_UCI")]
         tableoutdata$Year <- tableoutdata$Year - input$impacts_year[1]
         tableoutdata$ImpactYear <- paste0("Yr ",as.character(tableoutdata$Year))

         a <- tableoutdata$Year == max(tableoutdata$Year)
         b <- tableoutdata$Year%%5 == 0
         c <- tableoutdata$Year > 0
         yrs <- unique(tableoutdata[ c & (a|b),"Year"])
         tabledata <- merge(tableindata, tableoutdata[c & (a|b),], by = "Scenario")

         #    X-axis is the impact on adult survival for each of the scenarios
         #    Y-axis is CPS_Median (solid), CPS_LCI (dotted line) and CPS_UCI (dotted line)

         gg_pva_p2 <- ggplot() + theme_bw() +
            geom_line(data=tabledata,aes(x=Impact, y=CPS_Median, colour = ImpactYear)) +
            geom_point(data=tabledata,aes(x=Impact, y=CPS_Median, colour = ImpactYear)) +
            geom_line(data=tabledata,aes(x=Impact, y=CPS_LCI, colour = ImpactYear),linetype="dotted") +
            geom_line(data=tabledata,aes(x=Impact, y=CPS_UCI, colour = ImpactYear), linetype = "dotted")  +
            ylab("CPS (median and confidence interval) \n\n") + ggtitle("Counterfactual of Population Size")

         plotly_pva <- ggplotly(gg_pva_p2, autosize = T, height = 600) %>%
            layout(yaxis = list(tickmode="auto", autorange=TRUE),
               margin = list(t=100,r=110,l=90,unit="pt"))

      } else {
         # Plot would be difficult to interpret!
         output$M2_text <- renderText({HTML("When subpopulations have different impacts, charts cannot be drawn (see Report for details)") })
         plotly_pva <- ggplotly(ggplot() + theme_bw())
      }

   })


   ## #############################################################################################
   ## RUNTHE MODEL --------------------------------------------------------------------------------
   ##

   #observeEvent(input$goButton, {
   onclick("downloadlog", {
      errflag <- 0

      # NB: For the Shiny version ...
      specify.as.params <- FALSE
      include.baseline <- TRUE
      demobase.cormat <- NULL
      demobase.bskippc <- NULL
      inipop.splitimmat <- FALSE
      inipop.immatvals <- FALSE

      # Seed - if missing, add one.
      if (is.na(input$sim_seed) || is.null(input$sim_seed)) {
         sim_seed <- round(runif(1,0.5,10000.5))
         updateNumericInput(session, "sim_seed", value = sim_seed)
      } else {
         sim_seed <- input$sim_seed
      }

      # Popsize_target - can be NULL but not NA
      if (is.na(input$output_popsize_target)) {
         popsize_target <- NULL
      } else {
         popsize_target <- input$output_popsize_target
      }

      # Final year for output
      output_yr_end <- input$output_year_end
      if (input$nscen > 0 & (input$output_year_end < input$impacts_year[1] + 1)) {
         updateNumericInput(session, "output_year_end", value = input$impacts_year[1] + 1)
         shinyjs::html("impacts_year_display", paste("Changed to ", input$impacts_year[1] + 1, " as this must be after the impacts begin"))
         output_yr_end <- input$impacts_year[1] + 1
      }

      # First year for output
      output_yr_start <- input$output_year_start
      if (input$run_type == 'simplescenarios' & !is.na(output_yr_start)) {
         #if (any(is.na(output_yr_start), is.null(output_yr_start), (output_yr_start >= input$output_year_end))) {
         if (output_yr_start >= output_yr_end) {
            output_yr_start <- max(minyr, output_yr_end - 1)
            updateNumericInput(session, "output_year_start", value = max(minyr, output_yr_end - 1))
         }
      }

      # Reformat and test the inipop values
      inipop_years <- inipop_vec(input,"inipopyrs_")
      # if(errfn5s(inipop_years, input$npop)) {
      #    errmess <- "One or more 'Output: Years' has an incorrect format/dimension"
      #    errflag <- errflag + 1
      # }

      # Convert the adult baseline values to the correct format for the function
      NP <- ifelse (input$demobase_splitpops == TRUE, input$npop, 1)
      demobase_prod <- data.frame(Mean = numeric(NP), SD = numeric(NP))
      demobase_survadult <- data.frame(Mean = numeric(NP), SD = numeric(NP))
      for (x in 1:NP) {
         demobase_prod[x, "Mean"] <- input[[paste0("pr_mn_",x)]]
         demobase_prod[x, "SD"] <- input[[paste0("pr_sd_",x)]]
         demobase_survadult[x, "Mean"] <- input[[paste0("ad_mn_",x)]]
         demobase_survadult[x, "SD"] <- input[[paste0("ad_sd_",x)]]
      }
      if (input$model_dd != 'nodd') {
         demobase_prod$DD <- numeric(NP)
         demobase_survadult$DD <- numeric(NP)
         for (x in 1:NP) {
            demobase_prod$DD[x] <- input[[paste0("pr_idd_",x)]]
            demobase_survadult$DD[x] <- input[[paste0("ad_idd_",x)]]
         }
      }

      # Convert the immature values to the right format for the function
      # Dimensions are: subpopulation, age, parameter (mean/SD)
      if (input$demobase_splitimmat) {
         NF <- input$afb
         if (input$model_dd == 'nodd') {
            demobase_survimmat <- array(dim=c(NP, NF, 2))
            for (x in 1:NP) {
                for (y in 1:NF) {
                  demobase_survimmat[x,y,1] <- input[[paste0("im_mn_",x,"_",y)]]
                  demobase_survimmat[x,y,2] <- input[[paste0("im_sd_",x,"_",y)]]
               }
            }
         } else {
            demobase_survimmat <- array(dim=c(NP, NF, 3))
            for (x in 1:NP) {
               for (y in 1:NF) {
                  demobase_survimmat[x,y,1] <- input[[paste0("im_mn_",x,"_",y)]]
                  demobase_survimmat[x,y,2] <- input[[paste0("im_sd_",x,"_",y)]]
                  demobase_survimmat[x,y,3] <- input[[paste0("im_idd_",x,"_",y)]]
               }
            }
         }
      } else {
         demobase_survimmat <- NULL
      }

      # Scenario names
      if (input$nscen > 0) {
         impacts_scennames <- as.character()
         for (x in 1:input$nscen) {
            this_name <- input[[paste0("scn_nm_str_",x)]]
            err <- ! (is.character(this_name) & this_name != "")
            this_name <- ifelse(err, LETTERS[x], this_name)
            impacts_scennames <- c(impacts_scennames, this_name)
            # Save for charts
            rv$impacts_scennames <- impacts_scennames
         }
      } else {
         impacts_scennames <- NULL
      }

      # Convert the impacts values to the right format for the function
      impacts_prod_mean <- NULL
      impacts_prod_se <- NULL
      impacts_survadult_mean <- NULL
      impacts_survadult_se <- NULL
      impacts_survimmat_mean <- NULL
      impacts_survimmat_se <- NULL

      NiS <- input$nscen
      if (NiS > 0) {
         NiP <- ifelse (input$impacts_splitpops == TRUE, input$npop, 1)

         # Productivity and adult survival
         impacts_prod_mean <- matrix(NA, nrow = NiP, ncol = NiS)
         impacts_survadult_mean <- matrix(NA, nrow = NiP, ncol = NiS)
         for (y in 1:NiS) {
            for (x in 1:NiP) {
               impacts_prod_mean[x, y] <- input[[paste0("imp_pr_mn_", y,"_", x)]]
               impacts_survadult_mean[x, y] <- input[[paste0("imp_ad_mn_", y,"_", x)]]
            }
         }
         # Save for charts
         rv$impacts_survadult_mean <- impacts_survadult_mean

         # Standard errors
         if (input$impacts_provideses) {
            impacts_prod_se <- matrix(NA, nrow = NiP, ncol = NiS)
            impacts_survadult_se <- matrix(NA, nrow = NiP, ncol = NiS)
            for (y in 1:NiS) {
               for (x in 1:NiP) {
                  impacts_prod_se[x, y] <- input[[paste0("imp_pr_se_", y,"_", x)]]
                  impacts_survadult_se[x, y] <- input[[paste0("imp_ad_se_", y,"_", x)]]
               }
            }
         }

         # Immatures
         if (input$impacts_splitimmat) {
            impacts_survimmat_mean <- matrix(NA, nrow = NiP, ncol = NiS)
            for (y in 1:NiS) {
               for (x in 1:NiP) {
                  impacts_survimmat_mean[x, y] <- input[[paste0("imp_im_mn_", y,"_", x)]]
               }
            }
         }

         # Immatures and standard errors
         if (input$impacts_splitimmat && input$impacts_provideses) {
            impacts_survimmat_se <- matrix(NA, nrow = NiP, ncol = NiS)
            for (y in 1:NiS) {
               for (x in 1:NiP) {
                  impacts_survimmat_se[x, y] <- input[[paste0("imp_im_se_", y,"_", x)]]
               }
            }
         }
      }

      # RUN the appropriate function now ==========================================================

      if (errflag == 0) {
         if (input$run_type == "simplescenarios") {

            rv$results <- tryCatch({

               nepva.simplescenarios(
                  model.envstoch = input$model_envstoch,
                  model.demostoch = input$model_demostoch,
                  model.dd = input$model_dd,
                  model.prodmax = input$model_prodmax,
                  mbs = input$mbs,
                  afb = input$afb,
                  npop = input$npop,
                  nscen = input$nscen,
                  sim.n = input$sim_n,
                  nburn = input$nburn,
                  sim.seed = sim_seed,
                  demobase.specify.as.params = specify.as.params,
                  demobase.splitpops = input$demobase_splitpops,
                  demobase.splitimmat = input$demobase_splitimmat,
                  demobase.prod = demobase_prod,
                  demobase.survimmat = drop(demobase_survimmat),
                  demobase.survadult = demobase_survadult,
                  inipop.years = inipop_years,
                  inipop.inputformat = input$inipop_inputformat,
                  inipop.vals = inipop_vec(input,"inipopval_"),
                  impacts.relative = (input$impacts_relative == 'relative'),
                  impacts.splitpops = input$impacts_splitpops,
                  impacts.splitimmat = input$impacts_splitimmat,
                  impacts.provideses = input$impacts_provideses,
                  impacts.year.start = input$impacts_year[1],
                  impacts.year.end = input$impacts_year[2],
                  impacts.scennames = impacts_scennames,
                  impacts.matchscens = input$impacts_matchscens,
                  impacts.prod.mean = impacts_prod_mean,
                  impacts.prod.se = impacts_prod_se,
                  impacts.survimmat.mean = impacts_survimmat_mean,
                  impacts.survimmat.se = impacts_survimmat_se,
                  impacts.survadult.mean = impacts_survadult_mean,
                  impacts.survadult.se = impacts_survadult_se,
                  output.agetype = input$output_agetype,
                  output.year.start = output_yr_start,
                  output.year.end = output_yr_end,
                  output.popsize.target = popsize_target,
                  output.popsize.qe = input$output_popsize_qe,
                  silent = TRUE, output.raw = FALSE, changetablenames = FALSE
               )
            }, warning = function(war) {
               sendSweetAlert(session, title = "Something went wrong", text = war,
                  type = "warning", btn_labels = "Ok", html = FALSE, closeOnClickOutside = TRUE)
               #return("No data available")

            }, error = function(err) {
               sendSweetAlert(session, title = "Something went wrong", text = err,
                  type = "error", btn_labels = "Ok", html = FALSE, closeOnClickOutside = TRUE)
               return("No data available")

            }, finally = {

            }) # END tryCatch

         } else if (input$run_type == "validation") {

            # Check for validation data and reformat into required shape
            validation_years <- numeric()
            validation_counts <- numeric()
            for(x in 1:20) {
               if (is.na(input[[paste0("output_validation_years_",x)]])
                  || is.null(input[[paste0("output_validation_years_",x)]])) {
               } else {
                  validation_years <- c(validation_years, input[[paste0("output_validation_years_",x)]])
                  validation_counts <- c(validation_counts, input[[paste0("output_validation_counts_",x)]])
               }
            }

            rv$results <- tryCatch({
               nepva.validation(
                  model.envstoch = input$model_envstoch,
                  model.demostoch = input$model_demostoch,
                  model.dd = input$model_dd,
                  model.prodmax = input$model_prodmax,
                  mbs = input$mbs,
                  afb = input$afb,
                  sim.n = input$sim_n,
                  sim.seed = sim_seed,
                  nburn = input$nburn,
                  demobase.specify.as.params = specify.as.params,
                  demobase.splitimmat = input$demobase_splitimmat,
                  demobase.prod = demobase_prod, #data.frame(Mean = 0.5, SD = 0.11),
                  demobase.survimmat = drop(demobase_survimmat),
                  demobase.survadult = demobase_survadult, # data.frame(Mean = 0.82, SD = 0.02),
                  inipop.years = inipop_years,
                  inipop.inputformat = input$inipop_inputformat,
                  inipop.vals = inipop_vec(input,"inipopval_"),
                  output.agetype = input$output_agetype,
                  output.year.end = output_yr_end,
                  output.popsize.target = popsize_target,
                  output.popsize.qe = input$output_popsize_qe,
                  output.validation.counts = validation_counts,
                  output.validation.years = validation_years,
                  silent = TRUE, changetablenames = FALSE
               )

            }, warning = function(war) {
               sendSweetAlert(session, title = "Validation: Something went wrong", text = war,
                  type = "warning", btn_labels = "Ok", html = FALSE, closeOnClickOutside = TRUE)
               #return("No data available")

            }, error = function(err) {
               sendSweetAlert(session, title = "Validation: Something went wrong", text = err,
                  type = "error", btn_labels = "Ok", html = FALSE, closeOnClickOutside = TRUE)
               return("No data available")

            }, finally = {

            }) # END tryCatch

         } else if (input$run_type == "sensitivity.local") {

            names(demobase_survadult)  <-  c("mean", "sd")
            names(demobase_prod)  <-  c("mean", "sd")
            sens_pcr <- numeric(5)
            for(x in 1:5) {sens_pcr[x] <- input[[paste0("sens_pcr_",x)]]}

            rv$results <- tryCatch({

               nepva.sensitivity.local(
                  model.envstoch = input$model_envstoch,
                  model.demostoch = input$model_demostoch,
                  model.prodmax = input$model_prodmax,
                  mbs = input$mbs,
                  afb = input$afb,
                  sim.n = input$sim_n,
                  sim.seed = sim_seed,
                  nburn = input$nburn,
                  demobase.specify.as.params = specify.as.params,
                  demobase.prod = demobase_prod,
                  demobase.survadult = demobase_survadult,
                  inipop.years = inipop_years,
                  inipop.vals = inipop_vec(input,"inipopval_"),
                  impacts.relative = (input$impacts_relative == 'relative'),
                  impacts.year.start = input$impacts_year[1],
                  impacts.year.end = input$impacts_year[2],
                  impacts.prod.mean = impacts_prod_mean,
                  impacts.survadult.mean = impacts_survadult_mean,
                  output.popsize.qe = input$output_popsize_qe,
                  output.popsize.target = popsize_target,
                  output.year.end = output_yr_end,
                  sens.npvlocal = input$sens_npvlocal,
                  sens.pcr = sens_pcr,
                  silent = TRUE, changetablenames = FALSE
               )
            }, warning = function(war) {
               sendSweetAlert(session, title = "Sensitivity: Something went wrong", text = war,
                  type = "warning", btn_labels = "Ok", html = FALSE, closeOnClickOutside = TRUE)
               #return("No data available")

            }, error = function(err) {
               sendSweetAlert(session, title = "Sensitivity: Something went wrong", text = err,
                  type = "error", btn_labels = "Ok", html = FALSE, closeOnClickOutside = TRUE)
               return("No data available")

            }, finally = {

            }) # END tryCatch
         }
      }

      if (!is.null(rv$results) && (class(rv$results)  == "data.frame")) {

         if (input$run_type == "sensitivity.local"){
            rv$results <- change.table.names.sen(rv$results)
         } else {
            rv$results <- change.table.names(rv$results)
         }

         sendSweetAlert(session, title = "Finished", text = "See Table and Charts tab for results (log file is being written now)",
            type = "success", btn_labels = "Ok", html = FALSE, closeOnClickOutside = TRUE)
      }

   })

   ## SET TEST AND/OR CASE STUDY VALUES -----------------------------------------------------------
   ## any new examples added here must also be added to the Case Study box in the UI

   # CASE STUDY 1A
   observeEvent(input$case_studies,{
      req(input$case_studies == "Case study 1a")
      updateTextInput(session, "runrefname", value = "Case study 1a")
      updatePrettyRadioButtons(session, "run_type", selected = "simplescenarios")
      updateSelectizeInput(session, "species", selected = "None")
      updateSelectizeInput(session, "testrun", selected = "None")
      updatePrettyRadioButtons(session, "model_envstoch",selected = "betagamma")
      updateSwitchInput(session, "model_demostoch", value = TRUE)
      updatePrettyRadioButtons(session, "model_dd", selected = "nodd")
      updateSwitchInput(session, "model_prodmax", value = TRUE)
      updateSliderInput(session, "mbs", value = 2)
      updateSliderInput(session, "afb", value = 4)
      updateNumericInput(session, "npop", value = 2)
      updateNumericInput(session, "nscen", value = 5)
      updateNumericInput(session, "sim_n", value = 5000)
      updateNumericInput(session, "sim_seed", value = NA)
      updateNumericInput(session, "nburn", value = 0)
      updateSwitchInput(session, "demobase_splitpops", value = TRUE)
      updateSwitchInput(session, "demobase_splitimmat", value = FALSE); input$demobase_splitimmat
      updateNumericInput(session, "pr_mn_1", value = 0.58)
      updateNumericInput(session, "pr_mn_2", value = 0.39)
      updateNumericInput(session, "pr_sd_1", value = 0.074)
      updateNumericInput(session, "pr_sd_2", value = 0.035)
      updateNumericInput(session, "pr_idd_1", value = NA)
      updateNumericInput(session, "pr_idd_2", value = NA)
      updateNumericInput(session, "ad_mn_1", value = 0.854)
      updateNumericInput(session, "ad_mn_2", value = 0.854)
      updateNumericInput(session, "ad_sd_1", value = 0.077)
      updateNumericInput(session, "ad_sd_2", value = 0.077)
      updateNumericInput(session, "ad_idd_1", value = NA)
      updateNumericInput(session, "ad_idd_2", value = NA)
      updateNumericInput(session, "im_mn_1_1", value = NA)
      updateNumericInput(session, "im_mn_2_1", value = NA)
      updateNumericInput(session, "im_sd_1_1", value = NA)
      updateNumericInput(session, "im_sd_2_1", value = NA)
      updateNumericInput(session, "im_idd_1_1", value = NA)
      updateNumericInput(session, "im_idd_2_1", value = NA)
      updateNumericInput(session, "im_mn_1_2", value = NA)
      updateNumericInput(session, "im_mn_2_2", value = NA)
      updateNumericInput(session, "im_sd_1_2", value = NA)
      updateNumericInput(session, "im_sd_2_2", value = NA)
      updateNumericInput(session, "im_idd_1_2", value = NA)
      updateNumericInput(session, "im_idd_2_2", value = NA)
      updateNumericInput(session, "inipopyrs_1", value = 2017)
      updateNumericInput(session, "inipopyrs_2", value = 2017)
      updateNumericInput(session, "inipopval_1", value = 45504)
      updateNumericInput(session, "inipopval_2", value = 6031)
      updatePrettyRadioButtons(session, "inipop_inputformat", selected = "breeding.pairs")
      updateSwitchInput(session, "impacts_matchscens", value = TRUE)
      updatePrettyRadioButtons(session, "impacts_relative", selected = "relative")
      updateSwitchInput(session, "impacts_splitpops", value = FALSE)
      updateSwitchInput(session, "impacts_splitimmat", value = FALSE)
      updateSwitchInput(session, "impacts_provideses", value = FALSE)
      updateSliderInput(session, "impacts_year", value = c(2020, 2045))
      updateTextInput(session, "scn_nm_str_1", value = "Impact 50")
      updateTextInput(session, "scn_nm_str_2", value = "Impact 100")
      updateTextInput(session, "scn_nm_str_3", value = "Impact 200")
      updateTextInput(session, "scn_nm_str_4", value = "Impact 300")
      updateTextInput(session, "scn_nm_str_5", value = "Impact 500")
      updateNumericInput(session, "imp_pr_mn_1_1", value = 0.0)
      updateNumericInput(session, "imp_pr_mn_2_1", value = 0.0)
      updateNumericInput(session, "imp_pr_mn_3_1", value = 0.0)
      updateNumericInput(session, "imp_pr_mn_4_1", value = 0.0)
      updateNumericInput(session, "imp_pr_mn_5_1", value = 0.0)
      updateNumericInput(session, "imp_ad_mn_1_1", value = 0.0005)
      updateNumericInput(session, "imp_ad_mn_2_1", value = 0.00097)
      updateNumericInput(session, "imp_ad_mn_3_1", value = 0.0019)
      updateNumericInput(session, "imp_ad_mn_4_1", value = 0.0029)
      updateNumericInput(session, "imp_ad_mn_5_1", value = 0.0049)
      updateNumericInput(session, "imp_pr_se_1_1", value = NA)
      updateNumericInput(session, "imp_pr_se_2_1", value = NA)
      updateNumericInput(session, "imp_pr_se_3_1", value = NA)
      updateNumericInput(session, "imp_pr_se_4_1", value = NA)
      updateNumericInput(session, "imp_pr_se_5_1", value = NA)
      updateNumericInput(session, "imp_ad_se_1_1", value = NA)
      updateNumericInput(session, "imp_ad_se_2_1", value = NA)
      updateNumericInput(session, "imp_ad_se_3_1", value = NA)
      updateNumericInput(session, "imp_ad_se_4_1", value = NA)
      updateNumericInput(session, "imp_ad_se_5_1", value = NA)
      updatePrettyRadioButtons(session, "output_agetype", selected = "breeding.adults")
      updateNumericInput(session, "output_year_start", value = 2017)
      updateNumericInput(session, "output_year_end", value = 2045)
      updateNumericInput(session, "output_popsize_target", value = 83700*2)
      updateNumericInput(session, "output_popsize_qe", value = 20)
   })

   # CASE STUDY 1B
   observeEvent(input$case_studies,{
      req(input$case_studies == "Case study 1b")
      updateTextInput(session, "runrefname", value = "Case study 1b")
      updatePrettyRadioButtons(session, "run_type", selected = "simplescenarios")
      updateSelectizeInput(session, "species", selected = "None")
      updateSelectizeInput(session, "testrun", selected = "None")
      updatePrettyRadioButtons(session, "model_envstoch",selected = "betagamma")
      updateSwitchInput(session, "model_demostoch", value = TRUE)
      updatePrettyRadioButtons(session, "model_dd", selected = "nodd")
      updateSwitchInput(session, "model_prodmax", value = TRUE)
      updateSliderInput(session, "mbs", value = 2)
      updateSliderInput(session, "afb", value = 4)
      updateNumericInput(session, "npop", value = 2)
      updateNumericInput(session, "nscen", value = 5)
      updateNumericInput(session, "sim_n", value = 5000)
      updateNumericInput(session, "sim_seed", value = NULL)
      updateNumericInput(session, "nburn", value = 0)
      updateSwitchInput(session, "demobase_splitpops", value = TRUE)
      updateSwitchInput(session, "demobase_splitimmat", value = TRUE); input$demobase_splitimmat
      updateNumericInput(session, "pr_mn_1", value = 0.58)
      updateNumericInput(session, "pr_mn_2", value = 0.39)
      updateNumericInput(session, "pr_sd_1", value = 0.074)
      updateNumericInput(session, "pr_sd_2", value = 0.035)
      updateNumericInput(session, "pr_idd_1", value = NA)
      updateNumericInput(session, "pr_idd_2", value = NA)
      updateNumericInput(session, "ad_mn_1", value = 0.854)
      updateNumericInput(session, "ad_mn_2", value = 0.854)
      updateNumericInput(session, "ad_sd_1", value = 0.077)
      updateNumericInput(session, "ad_sd_2", value = 0.077)
      updateNumericInput(session, "ad_idd_1", value = NA)
      updateNumericInput(session, "ad_idd_2", value = NA)
      updateNumericInput(session, "im_mn_1_1", value = 0.79)
      updateNumericInput(session, "im_mn_2_1", value = 0.79)
      updateNumericInput(session, "im_sd_1_1", value = 0.077)
      updateNumericInput(session, "im_sd_2_1", value = 0.077)
      updateNumericInput(session, "im_mn_1_2", value = 0.79)
      updateNumericInput(session, "im_mn_2_2", value = 0.79)
      updateNumericInput(session, "im_sd_1_2", value = 0.077)
      updateNumericInput(session, "im_sd_2_2", value = 0.077)
      updateNumericInput(session, "im_mn_1_3", value = 0.79)
      updateNumericInput(session, "im_mn_2_3", value = 0.79)
      updateNumericInput(session, "im_sd_1_3", value = 0.077)
      updateNumericInput(session, "im_sd_2_3", value = 0.077)
      updateNumericInput(session, "im_mn_1_4", value = 0.79)
      updateNumericInput(session, "im_mn_2_4", value = 0.79)
      updateNumericInput(session, "im_sd_1_4", value = 0.077)
      updateNumericInput(session, "im_sd_2_4", value = 0.077)
      updateNumericInput(session, "im_idd_1_1", value = NA)
      updateNumericInput(session, "im_idd_2_1", value = NA)
      updateNumericInput(session, "im_idd_1_2", value = NA)
      updateNumericInput(session, "im_idd_2_2", value = NA)
      updateNumericInput(session, "im_idd_1_3", value = NA)
      updateNumericInput(session, "im_idd_2_3", value = NA)
      updateNumericInput(session, "im_idd_1_4", value = NA)
      updateNumericInput(session, "im_idd_2_4", value = NA)
      updateNumericInput(session, "inipopyrs_1", value = 2017)
      updateNumericInput(session, "inipopyrs_2", value = 2017)
      updateNumericInput(session, "inipopval_1", value = 45504)
      updateNumericInput(session, "inipopval_2", value = 6031)
      updatePrettyRadioButtons(session, "inipop_inputformat", selected = "breeding.pairs")
      updateSwitchInput(session, "impacts_matchscens", value = TRUE)
      updatePrettyRadioButtons(session, "impacts_relative", selected = "relative")
      updateSwitchInput(session, "impacts_splitpops", value = FALSE)
      updateSwitchInput(session, "impacts_splitimmat", value = FALSE)
      updateSwitchInput(session, "impacts_provideses", value = FALSE)
      updateSliderInput(session, "impacts_year", value = c(2020, 2045))
      updateTextInput(session, "scn_nm_str_1", value = "Impact 50")
      updateTextInput(session, "scn_nm_str_2", value = "Impact 100")
      updateTextInput(session, "scn_nm_str_3", value = "Impact 200")
      updateTextInput(session, "scn_nm_str_4", value = "Impact 300")
      updateTextInput(session, "scn_nm_str_5", value = "Impact 500")
      updateNumericInput(session, "imp_pr_mn_1_1", value = 0.0)
      updateNumericInput(session, "imp_pr_mn_2_1", value = 0.0)
      updateNumericInput(session, "imp_pr_mn_3_1", value = 0.0)
      updateNumericInput(session, "imp_pr_mn_4_1", value = 0.0)
      updateNumericInput(session, "imp_pr_mn_5_1", value = 0.0)
      updateNumericInput(session, "imp_ad_mn_1_1", value = 0.0005)
      updateNumericInput(session, "imp_ad_mn_2_1", value = 0.00097)
      updateNumericInput(session, "imp_ad_mn_3_1", value = 0.0019)
      updateNumericInput(session, "imp_ad_mn_4_1", value = 0.0029)
      updateNumericInput(session, "imp_ad_mn_5_1", value = 0.0049)
      updateNumericInput(session, "imp_pr_se_1_1", value = NA)
      updateNumericInput(session, "imp_pr_se_2_1", value = NA)
      updateNumericInput(session, "imp_pr_se_3_1", value = NA)
      updateNumericInput(session, "imp_pr_se_4_1", value = NA)
      updateNumericInput(session, "imp_pr_se_5_1", value = NA)
      updateNumericInput(session, "imp_ad_se_1_1", value = NA)
      updateNumericInput(session, "imp_ad_se_2_1", value = NA)
      updateNumericInput(session, "imp_ad_se_3_1", value = NA)
      updateNumericInput(session, "imp_ad_se_4_1", value = NA)
      updateNumericInput(session, "imp_ad_se_5_1", value = NA)
      updatePrettyRadioButtons(session, "output_agetype", selected = "breeding.adults")
      updateNumericInput(session, "output_year_start", value = 2017)
      updateNumericInput(session, "output_year_end", value = 2045)
      updateNumericInput(session, "output_popsize_target", value = 83700*2)
      updateNumericInput(session, "output_popsize_qe", value = 20)
   })

   # CASE STUDY 1c
   observeEvent(input$case_studies,{
      req(input$case_studies == "Case study 1c")
      updateTextInput(session, "runrefname", value = "Case study 1c")
      updatePrettyRadioButtons(session, "run_type", selected = "simplescenarios")
      updateSelectizeInput(session, "species", selected = "None")
      updateSelectizeInput(session, "testrun", selected = "None")
      updatePrettyRadioButtons(session, "model_envstoch",selected = "betagamma")
      updateSwitchInput(session, "model_demostoch", value = TRUE)
      updatePrettyRadioButtons(session, "model_dd", selected = "nodd")
      updateSwitchInput(session, "model_prodmax", value = TRUE)
      updateSliderInput(session, "mbs", value = 2)
      updateSliderInput(session, "afb", value = 4)
      updateNumericInput(session, "npop", value = 2)
      updateNumericInput(session, "nscen", value = 5)
      updateNumericInput(session, "sim_n", value = 5000)
      updateNumericInput(session, "sim_seed", value = NULL)
      updateNumericInput(session, "nburn", value = 0)
      updateSwitchInput(session, "demobase_splitpops", value = TRUE)
      updateSwitchInput(session, "demobase_splitimmat", value = FALSE); input$demobase_splitimmat
      updateNumericInput(session, "pr_mn_1", value = 0.58)
      updateNumericInput(session, "pr_mn_2", value = 0.39)
      updateNumericInput(session, "pr_sd_1", value = 0.074)
      updateNumericInput(session, "pr_sd_2", value = 0.035)
      updateNumericInput(session, "pr_idd_1", value = NA)
      updateNumericInput(session, "pr_idd_2", value = NA)
      updateNumericInput(session, "ad_mn_1", value = 0.854)
      updateNumericInput(session, "ad_mn_2", value = 0.854)
      updateNumericInput(session, "ad_sd_1", value = 0.077)
      updateNumericInput(session, "ad_sd_2", value = 0.077)
      updateNumericInput(session, "ad_idd_1", value = NA)
      updateNumericInput(session, "ad_idd_2", value = NA)
      updateNumericInput(session, "im_mn_1_1", value = NA)
      updateNumericInput(session, "im_mn_2_1", value = NA)
      updateNumericInput(session, "im_sd_1_1", value = NA)
      updateNumericInput(session, "im_sd_2_1", value = NA)
      updateNumericInput(session, "im_mn_1_2", value = NA)
      updateNumericInput(session, "im_mn_2_2", value = NA)
      updateNumericInput(session, "im_sd_1_2", value = NA)
      updateNumericInput(session, "im_sd_2_2", value = NA)
      updateNumericInput(session, "im_idd_1_1", value = NA)
      updateNumericInput(session, "im_idd_2_1", value = NA)
      updateNumericInput(session, "im_idd_1_2", value = NA)
      updateNumericInput(session, "im_idd_2_2", value = NA)
      updateNumericInput(session, "inipopyrs_1", value = 2017)
      updateNumericInput(session, "inipopyrs_2", value = 2017)
      updateNumericInput(session, "inipopval_1", value = 45504)
      updateNumericInput(session, "inipopval_2", value = 6031)
      updatePrettyRadioButtons(session, "inipop_inputformat", selected = "breeding.pairs")
      updateSwitchInput(session, "impacts_matchscens", value = TRUE)
      updatePrettyRadioButtons(session, "impacts_relative", selected = "absolute")
      updateSwitchInput(session, "impacts_splitpops", value = FALSE)
      updateSwitchInput(session, "impacts_splitimmat", value = FALSE)
      updateSwitchInput(session, "impacts_provideses", value = FALSE)
      updateSliderInput(session, "impacts_year", value = c(2020, 2045))
      updateTextInput(session, "scn_nm_str_1", value = "Impact 50")
      updateTextInput(session, "scn_nm_str_2", value = "Impact 100")
      updateTextInput(session, "scn_nm_str_3", value = "Impact 200")
      updateTextInput(session, "scn_nm_str_4", value = "Impact 300")
      updateTextInput(session, "scn_nm_str_5", value = "Impact 500")
      updateNumericInput(session, "imp_pr_mn_1_1", value = 0.0)
      updateNumericInput(session, "imp_pr_mn_2_1", value = 0.0)
      updateNumericInput(session, "imp_pr_mn_3_1", value = 0.0)
      updateNumericInput(session, "imp_pr_mn_4_1", value = 0.0)
      updateNumericInput(session, "imp_pr_mn_5_1", value = 0.0)
      updateNumericInput(session, "imp_ad_mn_1_1", value = 50)
      updateNumericInput(session, "imp_ad_mn_2_1", value = 100)
      updateNumericInput(session, "imp_ad_mn_3_1", value = 200)
      updateNumericInput(session, "imp_ad_mn_4_1", value = 300)
      updateNumericInput(session, "imp_ad_mn_5_1", value = 500)
      updateNumericInput(session, "imp_pr_se_1_1", value = NA)
      updateNumericInput(session, "imp_pr_se_2_1", value = NA)
      updateNumericInput(session, "imp_pr_se_3_1", value = NA)
      updateNumericInput(session, "imp_pr_se_4_1", value = NA)
      updateNumericInput(session, "imp_pr_se_5_1", value = NA)
      updateNumericInput(session, "imp_ad_se_1_1", value = NA)
      updateNumericInput(session, "imp_ad_se_2_1", value = NA)
      updateNumericInput(session, "imp_ad_se_3_1", value = NA)
      updateNumericInput(session, "imp_ad_se_4_1", value = NA)
      updateNumericInput(session, "imp_ad_se_5_1", value = NA)
      updatePrettyRadioButtons(session, "output_agetype", selected = "breeding.adults")
      updateNumericInput(session, "output_year_start", value = 2017)
      updateNumericInput(session, "output_year_end", value = 2045)
      updateNumericInput(session, "output_popsize_target", value = 83700*2)
      updateNumericInput(session, "output_popsize_qe", value = 20)
   })

   # CASE STUDY 1d
   observeEvent(input$case_studies,{
      req(input$case_studies == "Case study 1d")
      updateTextInput(session, "runrefname", value = "Case study 1d")
      updatePrettyRadioButtons(session, "run_type", selected = "simplescenarios")
      updateSelectizeInput(session, "species", selected = "None")
      updateSelectizeInput(session, "testrun", selected = "None")
      updatePrettyRadioButtons(session, "model_envstoch",selected = "betagamma")
      updateSwitchInput(session, "model_demostoch", value = TRUE)
      updatePrettyRadioButtons(session, "model_dd", selected = "dduloglin")
      updateSwitchInput(session, "model_prodmax", value = TRUE)
      updateSliderInput(session, "mbs", value = 2)
      updateSliderInput(session, "afb", value = 4)
      updateNumericInput(session, "npop", value = 2)
      updateNumericInput(session, "nscen", value = 5)
      updateNumericInput(session, "sim_n", value = 5000)
      updateNumericInput(session, "sim_seed", value = NA)
      updateNumericInput(session, "nburn", value = 0)
      updateSwitchInput(session, "demobase_splitpops", value = TRUE)
      updateSwitchInput(session, "demobase_splitimmat", value = FALSE); input$demobase_splitimmat
      updateNumericInput(session, "pr_mn_1", value = 0.58)
      updateNumericInput(session, "pr_mn_2", value = 0.39)
      updateNumericInput(session, "pr_sd_1", value = 0.074)
      updateNumericInput(session, "pr_sd_2", value = 0.035)
      updateNumericInput(session, "pr_idd_1", value = -0.003)
      updateNumericInput(session, "pr_idd_2", value = -0.003)
      updateNumericInput(session, "ad_mn_1", value = 0.854)
      updateNumericInput(session, "ad_mn_2", value = 0.854)
      updateNumericInput(session, "ad_sd_1", value = 0.077)
      updateNumericInput(session, "ad_sd_2", value = 0.077)
      updateNumericInput(session, "ad_idd_1", value = -0.002)
      updateNumericInput(session, "ad_idd_2", value = -0.002)
      updateNumericInput(session, "im_mn_1_1", value = NA)
      updateNumericInput(session, "im_mn_2_1", value = NA)
      updateNumericInput(session, "im_sd_1_1", value = NA)
      updateNumericInput(session, "im_sd_2_1", value = NA)
      updateNumericInput(session, "im_idd_1_1", value = NA)
      updateNumericInput(session, "im_idd_2_1", value = NA)
      updateNumericInput(session, "im_mn_1_2", value = NA)
      updateNumericInput(session, "im_mn_2_2", value = NA)
      updateNumericInput(session, "im_sd_1_2", value = NA)
      updateNumericInput(session, "im_sd_2_2", value = NA)
      updateNumericInput(session, "im_idd_1_2", value = NA)
      updateNumericInput(session, "im_idd_2_2", value = NA)
      updateNumericInput(session, "inipopyrs_1", value = 2017)
      updateNumericInput(session, "inipopyrs_2", value = 2017)
      updateNumericInput(session, "inipopval_1", value = 45504)
      updateNumericInput(session, "inipopval_2", value = 6031)
      updatePrettyRadioButtons(session, "inipop_inputformat", selected = "breeding.pairs")
      updateSwitchInput(session, "impacts_matchscens", value = TRUE)
      updatePrettyRadioButtons(session, "impacts_relative", selected = "relative")
      updateSwitchInput(session, "impacts_splitpops", value = FALSE)
      updateSwitchInput(session, "impacts_splitimmat", value = FALSE)
      updateSwitchInput(session, "impacts_provideses", value = FALSE)
      updateSliderInput(session, "impacts_year", value = c(2020, 2045))
      updateTextInput(session, "scn_nm_str_1", value = "Impact 50")
      updateTextInput(session, "scn_nm_str_2", value = "Impact 100")
      updateTextInput(session, "scn_nm_str_3", value = "Impact 200")
      updateTextInput(session, "scn_nm_str_4", value = "Impact 300")
      updateTextInput(session, "scn_nm_str_5", value = "Impact 500")
      updateNumericInput(session, "imp_pr_mn_1_1", value = 0.0)
      updateNumericInput(session, "imp_pr_mn_2_1", value = 0.0)
      updateNumericInput(session, "imp_pr_mn_3_1", value = 0.0)
      updateNumericInput(session, "imp_pr_mn_4_1", value = 0.0)
      updateNumericInput(session, "imp_pr_mn_5_1", value = 0.0)
      updateNumericInput(session, "imp_ad_mn_1_1", value = 0.0005)
      updateNumericInput(session, "imp_ad_mn_2_1", value = 0.00097)
      updateNumericInput(session, "imp_ad_mn_3_1", value = 0.0019)
      updateNumericInput(session, "imp_ad_mn_4_1", value = 0.0029)
      updateNumericInput(session, "imp_ad_mn_5_1", value = 0.0049)
      updateNumericInput(session, "imp_pr_se_1_1", value = NA)
      updateNumericInput(session, "imp_pr_se_2_1", value = NA)
      updateNumericInput(session, "imp_pr_se_3_1", value = NA)
      updateNumericInput(session, "imp_pr_se_4_1", value = NA)
      updateNumericInput(session, "imp_pr_se_5_1", value = NA)
      updateNumericInput(session, "imp_ad_se_1_1", value = NA)
      updateNumericInput(session, "imp_ad_se_2_1", value = NA)
      updateNumericInput(session, "imp_ad_se_3_1", value = NA)
      updateNumericInput(session, "imp_ad_se_4_1", value = NA)
      updateNumericInput(session, "imp_ad_se_5_1", value = NA)
      updatePrettyRadioButtons(session, "output_agetype", selected = "breeding.adults")
      updateNumericInput(session, "output_year_start", value = 2017)
      updateNumericInput(session, "output_year_end", value = 2045)
      updateNumericInput(session, "output_popsize_target", value = 83700*2)
      updateNumericInput(session, "output_popsize_qe", value = 20)
   })

   # TEST EXAMPLE 1
   observeEvent(input$testrun,{
      req(input$testrun == "Example Simulation")
      updateTextInput(session, "runrefname", value = "Example")
      updateSelectizeInput(session, "case_studies", selected = "None")
      updateSelectizeInput(session, "species", selected = "None")
      updatePrettyRadioButtons(session, "run_type", selected = "simplescenarios")
      updatePrettyRadioButtons(session, "model_envstoch",selected = "betagamma")
      updateSwitchInput(session, "model_demostoch", value = TRUE)
      updatePrettyRadioButtons(session, "model_dd", selected = "nodd")
      updateSwitchInput(session, "model_prodmax", value = TRUE)
      updateSliderInput(session, "mbs", value = 4)
      updateSliderInput(session, "afb", value = 5)
      updateNumericInput(session, "npop", value = 2)
      updateNumericInput(session, "nscen", value = 3)
      updateNumericInput(session, "sim_n", value = 10)
      updateNumericInput(session, "sim_seed", value = 43576)
      updateNumericInput(session, "nburn", value = 0)
      updateSwitchInput(session, "demobase_splitpops", value = TRUE)
      updateSwitchInput(session, "demobase_splitimmat", value = FALSE); input$demobase_splitimmat
      updateNumericInput(session, "pr_mn_1", value = 0.3)
      updateNumericInput(session, "pr_mn_2", value = 0.4)
      updateNumericInput(session, "pr_sd_1", value = 0.11)
      updateNumericInput(session, "pr_sd_2", value = 0.13)
      updateNumericInput(session, "pr_idd_1", value = -0.01)
      updateNumericInput(session, "pr_idd_2", value = -0.02)
      updateNumericInput(session, "ad_mn_1", value = 0.89)
      updateNumericInput(session, "ad_mn_2", value = 0.92)
      updateNumericInput(session, "ad_sd_1", value = 0.02)
      updateNumericInput(session, "ad_sd_2", value = 0.03)
      updateNumericInput(session, "ad_idd_1", value = -0.03)
      updateNumericInput(session, "ad_idd_2", value = -0.04)
      updateNumericInput(session, "inipopyrs_1", value = 2012)
      updateNumericInput(session, "inipopyrs_2", value = 2015)
      updateNumericInput(session, "inipopval_1", value = 791)
      updateNumericInput(session, "inipopval_2", value = 113)
      updatePrettyRadioButtons(session, "inipop_inputformat", selected = "breeding.pairs")
      updateSwitchInput(session, "impacts_matchscens", value = TRUE)
      updatePrettyRadioButtons(session, "impacts_relative", selected = "relative")
      updateSwitchInput(session, "impacts_splitpops", value = FALSE)
      updateSwitchInput(session, "impacts_splitimmat", value = FALSE)
      updateSwitchInput(session, "impacts_provideses", value = FALSE)
      updateSliderInput(session, "impacts_year", value = c(2035, 2055))
      updateTextInput(session, "scn_nm_str_1", value = "ice")
      updateTextInput(session, "scn_nm_str_2", value = "fish")
      updateTextInput(session, "scn_nm_str_3", value = "bob")
      updateNumericInput(session, "imp_pr_mn_1_1", value = 0.03)
      updateNumericInput(session, "imp_pr_mn_2_1", value = 0)
      updateNumericInput(session, "imp_pr_mn_3_1", value = 0)
      updateNumericInput(session, "imp_ad_mn_1_1", value = 0.05)
      updateNumericInput(session, "imp_ad_mn_2_1", value = 0.12)
      updateNumericInput(session, "imp_ad_mn_3_1", value = 0.17)
      updatePrettyRadioButtons(session, "output_agetype", selected = "age.separated")
      updateNumericInput(session, "output_year_start", value = 2015)
      updateNumericInput(session, "output_year_end", value = 2070)
      updateNumericInput(session, "output_popsize_target", value = 150)
      updateNumericInput(session, "output_popsize_qe", value = 10)
   })

   # TEST EXAMPLE 2
   observeEvent(input$testrun,{
      req(input$testrun == "Example Validation")
      updateTextInput(session, "runrefname", value = "Example")
      updateSelectizeInput(session, "case_studies", selected = "None")
      updateSelectizeInput(session, "species", selected = "None")
      updatePrettyRadioButtons(session, "run_type", selected = "validation")
      updatePrettyRadioButtons(session, "model_envstoch",selected = "betagamma")
      updateSwitchInput(session, "model_demostoch", value = TRUE)
      updatePrettyRadioButtons(session, "model_dd", selected = "nodd")
      updateSwitchInput(session, "model_prodmax", value = TRUE)
      updateSliderInput(session, "mbs", value = 4)
      updateSliderInput(session, "afb", value = 5)
      updateNumericInput(session, "sim_n", value = 1000)
      updateNumericInput(session, "sim_seed", value = 43576)
      updateNumericInput(session, "nburn", value = 0)
      updateNumericInput(session, "pr_mn_1", value = 0.5)
      updateNumericInput(session, "pr_sd_1", value = 0.11)
      updateNumericInput(session, "ad_mn_1", value = 0.82)
      updateNumericInput(session, "ad_sd_1", value = 0.02)
      updateNumericInput(session, "inipopyrs_1", value = 2001)
      updateNumericInput(session, "inipopval_1", value = 791)
      updatePrettyRadioButtons(session, "output_agetype", selected = "breeding.pairs")
      updateNumericInput(session, "output_year_end", value = 2018)
      updateNumericInput(session, "output_popsize_target", value = 150)
      updateNumericInput(session, "output_popsize_qe", value = 10)
      updateNumericInput(session, "output_validation_years_1", value = 2004)
      updateNumericInput(session, "output_validation_years_2", value = 2008)
      updateNumericInput(session, "output_validation_years_3", value = 2009)
      updateNumericInput(session, "output_validation_years_4", value = 2016)
      updateNumericInput(session, "output_validation_counts_1", value = 501)
      updateNumericInput(session, "output_validation_counts_2", value = 589)
      updateNumericInput(session, "output_validation_counts_3", value = 612)
      updateNumericInput(session, "output_validation_counts_4", value = 698)
   })

   # TEST EXAMPLE 3
   observeEvent(input$testrun,{
      req(input$testrun == "Example Sensitivity")
      updateTextInput(session, "runrefname", value = "Example")
      updateSelectizeInput(session, "case_studies", selected = "None")
      updateSelectizeInput(session, "species", selected = "None")
      updatePrettyRadioButtons(session, "run_type", selected = "sensitivity.local")
      updatePrettyRadioButtons(session, "model_envstoch",selected = "betagamma")
      updateSwitchInput(session, "model_demostoch", value = TRUE)
      updateSwitchInput(session, "model_prodmax", value = TRUE)
      updateSliderInput(session, "mbs", value = 4)
      updateSliderInput(session, "afb", value = 5)
      updateNumericInput(session, "sim_n", value = 10)
      updateNumericInput(session, "sim_seed", value = 43576)
      updateNumericInput(session, "nburn", value = 0)
      updateNumericInput(session, "pr_mn_1", value = 0.7)
      updateNumericInput(session, "pr_sd_1", value = 0.11)
      updateNumericInput(session, "ad_mn_1", value = 0.92)
      updateNumericInput(session, "ad_sd_1", value = 0.02)
      updateNumericInput(session, "inipopyrs_1", value = 2012)
      updateNumericInput(session, "inipopval_1", value = 791)
      updateSliderInput(session, "impacts_year", value = c(2030, 2050))
      updatePrettyRadioButtons(session, "impacts_relative", selected = "relative")
      updateTextInput(session, "scn_nm_str_1", value = "impacted")
      updateNumericInput(session, "imp_pr_mn_1_1", value = -0.02)
      updateNumericInput(session, "imp_ad_mn_1_1", value = -0.03)
      updateNumericInput(session, "output_year_end", value = 2070)
      updateNumericInput(session, "output_popsize_target", value = 150)
      updateNumericInput(session, "output_popsize_qe", value = 10)
      updateNumericInput(session, "sens_npvlocal", value = 3)
      updateNumericInput(session, "sens_pcr_1", value = 10)
      updateNumericInput(session, "sens_pcr_2", value =  5)
      updateNumericInput(session, "sens_pcr_3", value =  5)
      updateNumericInput(session, "sens_pcr_4", value = 50)
      updateNumericInput(session, "sens_pcr_5", value = 50)
   })

   # TEST EXAMPLE 3x
   observeEvent(input$testrun,{
      req(input$testrun == "Example 3")
      updateTextInput(session, "runrefname", value = "run3")
      updateSelectizeInput(session, "case_studies", selected = "None")
      updateSelectizeInput(session, "species", selected = "None")
      updatePrettyRadioButtons(session, "run_type", selected = "simplescenarios")
      updatePrettyRadioButtons(session, "model_envstoch",selected = "deterministic")
      updateSwitchInput(session, "model_demostoch", value = FALSE)
      updatePrettyRadioButtons(session, "model_dd", selected = "nodd")
      updateSwitchInput(session, "model_prodmax", value = TRUE)
      updateSliderInput(session, "mbs", value = 2)
      updateSliderInput(session, "afb", value = 4)
      updateNumericInput(session, "npop", value = 3)
      updateNumericInput(session, "nscen", value = 2)
      updateNumericInput(session, "sim_n", value = 8)
      updateNumericInput(session, "nburn", value = 9)
      updateNumericInput(session, "sim_seed", value = 7064)
      updateSwitchInput(session, "demobase_splitpops", value = FALSE)
      updateSwitchInput(session, "demobase_splitimmat", value = FALSE); input$demobase_splitimmat
      updateNumericInput(session, "pr_mn_1", value = 0.7590806)
      updateNumericInput(session, "pr_sd_1", value = 0.1179217)
      updateNumericInput(session, "ad_mn_1", value = 0.9803358)
      updateNumericInput(session, "ad_sd_1", value = 0.0658929)
      updatePrettyRadioButtons(session, "inipop_inputformat", selected = "breeding.pairs")
      updateNumericInput(session, "inipopyrs_1", value = 2010)
      updateNumericInput(session, "inipopyrs_2", value = 2006)
      updateNumericInput(session, "inipopyrs_3", value = 2014)
      updateNumericInput(session, "inipopval_1", value = 8375)
      updateNumericInput(session, "inipopval_2", value = 502)
      updateNumericInput(session, "inipopval_3", value = 773)
      updateSwitchInput(session, "impacts_splitpops", value = FALSE)
      updateSwitchInput(session, "impacts_splitimmat", value = TRUE)
      updateSwitchInput(session, "impacts_provideses", value = TRUE)
      updateSwitchInput(session, "impacts_matchscens", value = TRUE)
      updatePrettyRadioButtons(session, "impacts_relative", selected = "relative")
      updateSliderInput(session, "impacts_year", value = c(2033, 2053))
      updateTextInput(session, "scn_nm_str_1", value = "scen1")
      updateTextInput(session, "scn_nm_str_2", value = "scen2")
      updateNumericInput(session, "imp_pr_mn_1_1", value = -0.09077062)
      updateNumericInput(session, "imp_pr_mn_2_1", value = 0.04593874)
      updateNumericInput(session, "imp_pr_se_1_1", value = -0.004374508)
      updateNumericInput(session, "imp_pr_se_2_1", value = 0.00144323)
      updateNumericInput(session, "imp_ad_mn_1_1", value = -0.04271304)
      updateNumericInput(session, "imp_ad_mn_2_1", value = -0.04346782)
      updateNumericInput(session, "imp_ad_se_1_1", value = -0.0007258702)
      updateNumericInput(session, "imp_ad_se_2_1", value = -0.0005980797)
      updateNumericInput(session, "imp_im_mn_1_1", value = -0.0006136637)
      updateNumericInput(session, "imp_im_mn_2_1", value = 0.013220849)
      updateNumericInput(session, "imp_im_se_1_1", value = -7.759761e-06)
      updateNumericInput(session, "imp_im_se_2_1", value = 0.0006537367)
      updatePrettyRadioButtons(session, "output_agetype", selected = "whole.population")
      updateNumericInput(session, "output_year_start", value = 2031)
      updateNumericInput(session, "output_year_end", value = 2053)
      updateNumericInput(session, "output_popsize_target", value = 256)
      updateNumericInput(session, "output_popsize_qe", value = 4)
   })

}

# ##############################################################################
# Run the application
# ##############################################################################

shinyApp(ui = ui, server = server)


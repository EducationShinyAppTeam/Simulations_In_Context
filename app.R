# Load Packages ----
library(shiny)
library(shinydashboard)
library(shinyBS)
library(shinyWidgets)
library(boastUtils)
library(ggplot2) 
library(boot)

# Load additional dependencies and setup functions

# Datasets ----
flightData <- read.csv(file = "flightData.csv", stringsAsFactors = FALSE)

# Define UI for App ----
ui <- list(
  ## App page ----
  dashboardPage(
    skin = "blue",
    dashboardHeader(
      title = "Simulations in Context",
      titleWidth = 250,
      tags$li(class = "dropdown", actionLink("info", icon("info"))),
      tags$li(
        class = "dropdown",
        boastUtils::surveyLink(name = "Simulations_In_Context")
      ),
      tags$li(
        class = "dropdown",
        tags$a(href = 'https://shinyapps.science.psu.edu/',
               icon("house")
        )
      )
    ),
    ## Navigation menu ----
    dashboardSidebar(
      width = 250,
      sidebarMenu(
        id = "pages",
        menuItem("Overview", tabName = "overview", icon = icon("gauge-high")),
        menuItem("Prerequisites", tabName = "prerequisites", icon = icon("book")),
        menuItem("CI for Mean", tabName = "ciMean", icon = icon("wpexplorer")),
        menuItem("CI for Proportion", tabName = "ciProp", icon = icon("wpexplorer")),
        #menuItem("Hypothesis Test", tabName = "hypTest", icon = icon("wpexplorer")),
        menuItem("Probability", tabName = "prob", icon = icon("wpexplorer")),
        menuItem("References", tabName = "references", icon = icon("leanpub"))
      ),
      tags$div(
        class = "sidebar-logo",
        boastUtils::sidebarFooter()
      )
    ),
    ## Create the content ----
    dashboardBody(
      tabItems(
        ### Overview Page ----
        tabItem(
          tabName = "overview",
          withMathJax(),
          h1("Simulations in Context"), 
          p("Learn how simulations come together in specific scenarios. Read through 
            a context and link key terms to their meanings. Then create a simulation 
            that accurately reflects the situation."),
          h2("Instructions"),
          tags$ol(
            tags$li("Review the prereqisities as needed."),
            tags$li("Next, go to the explore page and go through each type of simulation."),
            tags$li("For each simulation, read the context, identify terms within the 
                    context, and create possible simulations.")
          ),
          #### Overview to Explore Button
          div(
            style = "text-align: center;",
            bsButton(
              inputId = "overviewToPrereq",
              label = "Prerequisites",
              size = "large",
              icon = icon("book")
            )
          ),
          br(),
          br(),
          h2("Acknowledgements"),
          p(
            "This version of the app was developed and coded by Taryn McHugh.",
            br(),
            br(),
            "Cite this app as:",
            br(),
            citeApp(),
            br(),
            br(),
            div(class = "updated", "Last Update: 11/27/2023 by T.M.")
          )
        ),
        ### Prerequisites ----
        tabItem(
          tabName = "prerequisites",
          withMathJax(),
          h2("Prerequisites"),
          box(
            title = strong("Vocabulary"),
            status = "primary",
            collapsible = TRUE,
            collapsed = FALSE,
            width = "100%",
            tags$ul(
              tags$li("Population: The entire group that we want information about. "),
              tags$li("Population Parameter(s): A number that describes 
                      something about the population. "),
              tags$li("Sample: The part of the population that we are actually examining."),
              tags$li("Statistic(s): A number that describes something about 
                      the sample."),
              tags$li("Bootstrap: Type of resampling with replacement. You take samples 
                      from the original sample using the same sample size."),
              tags$li("Confidence Interval: The range of population parameters that 
                      are compatible with the test statistic with a certain level of
                      confidence.")
            )
          ),
          fluidRow(
            column(
              width = 6,
              box(
                title = strong("Simulations for Calculating Probabilities"),
                status = "primary",
                collapsible = TRUE,
                collapsed = TRUE,
                width = "100%",          
                tags$ul(
                  tags$li("Simulate a chance process that mirrors a real-life experiment."),
                  tags$li("For each simulation check whether a particular event occurs."),
                  tags$li("Estimate the probability of the event by the proportion 
                          of times it comes up in a large number of simulations.")
                )
              )
            ),
            column(
              width = 6,
              box(
                title = strong("Bootstrapping"),
                status = "primary",
                collapsible = TRUE,
                collapsed = TRUE,
                width = "100%",          
                tags$ul(
                  tags$li("Bootstrapping can be useful to estimate standard errors 
                          and confidence intervals for sample statistics in situations 
                          where common formulas don't apply."),
                  tags$li("Calculate the statistic for each bootstrap sample and 
                          look at the variability across bootstrap samples to estimate the variability around the original sample statistic."),
                  tags$li("Use as many replications of bootstrap samples as practical.")
                )
              )
            )
          )
        ),
        ### Confidence Interval for Mean ----
        tabItem(
          tabName = "ciMean",
          h2("Confidence Interval for Mean"),
          h4("Step 1: Context"),
          fluidRow(
            column(
              width = 12,
              p("Every year numerous airplanes takeoff and land at international
                airports globally. HOW MANY INTERNATIONAL NORTHEATERN AIRPORTS ARE HERE. 
                HOW MANY ARE WE USING DATA FROM. In a study, a sample was drawn from six international 
                airports in the northeast region of the USA. Over the period from 
                January 2020 to August 2023, a total of 253 data points were collected 
                regarding the number of planes taking off or landing. It  
                revealed that the mean number of planes engaged in these activities 
                was 22,253.162, with a median of 20,600.  Create a 90% confidence 
                interval for the numbers of flights departing/arriving at northeast 
                international airports based on this data."),
            )
          ),
          br(),
          tabsetPanel(
            id = "simulationType",
            ##### Identifying Components ----
            tabPanel(
              title = "Step 2: Identifying Components",
              value = "b",
              wellPanel(
                fluidRow(
                  column(
                    width = 5, 
                    selectInput(
                      inputId = "ciMeanPop",
                      label = "Population",
                      choices = c( " ", "International Airports", "Airports in the Northeast region", 
                                   "Number of flights arriving/departing", 
                                   "Number of layovers", "Number of international airports",  
                                   "International airports in the Northeast region"),
                    )
                  ),
                  column(width = 1, uiOutput(outputId = "ciMeanPopIcon")), 
                  column(
                    width = 5, 
                    selectInput(
                      inputId = "ciMeanPara",
                      label = "Population Parameter",
                      choices = c(" ", "Number of international airports", 
                                  "Airports in the Northeast region",
                                  "Number of flights arriving/departing", 
                                  "International Airports", "Number of layovers",
                                  "International airports in the Northeast region")
                    )
                  ),
                  column(width = 1, uiOutput(outputId = "ciMeanParaIcon")),
                ),
                fluidRow(
                  column(
                    width = 5, 
                    selectInput(
                      inputId = "ciMeanSamp",
                      label = "Sample",
                      choices = c(" ", "22253", "All Airports", "253", "20600", 
                                  "6 International Airports", "13 States in Northeast Region")
                    )
                  ),
                  column(width = 1, uiOutput(outputId = "ciMeanSampIcon")),
                  column(
                    width = 5, 
                    selectInput(
                      inputId = "ciMeanStat",
                      label = "Sample Statistic",
                      choices = c( " ", "253", "All Airports", "13 States in Northeast Region", 
                                   "20600", "6 International Airports", "22253")
                    )
                  ),
                  column(width = 1, uiOutput(outputId = "ciMeanStatIcon"))
                ),
                fluidRow(
                  column(
                    width = 9,
                    textOutput(outputId = "ciMeanCompFeed")
                  ),
                  column(
                    width = 1,
                    bsButton(
                      inputId = "ciMeanReset",
                      label = "Reset"
                    )
                  ),
                  column(
                    width = 1,
                    bsButton(
                      inputId = "ciMeanSubmit",
                      label = "Submit"
                    )
                  )
                )
              )
            ),
            ##### Simulation  ----
            tabPanel(
              title = "Step 3: Simulation",
              value = "b",
              fluidRow(
                column(
                  width = 4,
                  wellPanel(
                    sliderInput(
                      inputId = "ciMeanNS",
                      label = "Number of Samples",
                      min = 200, 
                      max = 300,
                      value = 250
                    ),
                    sliderInput(
                      inputId = "ciMeanNumRep",
                      label = "Number of Replications",
                      min = 1, 
                      max = 5000,
                      value = 2500
                    ),
                    numericInput(
                      inputId = "ciMeanCL",
                      label = 'Confidence Level',
                      value = 0.95
                    ),
                    checkboxInput(
                      inputId = "ciMeanReplications",
                      label = "Use Replication",
                      value = FALSE
                    ),
                    bsButton(
                      inputId = "simCIMean",
                      label = "Simulate"
                    )
                  )
                ),
                column(
                  width = 8,
                  plotOutput("ciMeanSim"),
                  textOutput('ciMeanResults')
                )
              )
            )
          ),
          ### Answer ----
          h4("Step 4: Answer"),
          fluidRow(
            column( 
              width = 2,
              numericInput(
                inputId = 'ciMeanLower',
                label = 'Lower Bound',
                value = 0.0,
                step = 0.01
              )
            ),
            column(width = 1, uiOutput(outputId = "ciMeanLowerIcon")),
            column(
              width = 2, 
              numericInput(
                inputId = 'ciMeanUpper',
                label = 'Upper Bound',
                value = 0.0,
                step = 0.01
              )
            ),
            column(width = 1, uiOutput(outputId = "ciMeanUpperIcon")),
          ),
          fluidRow(
            column(
              width = 2,
              bsButton(
                inputId = "ciMeanGuessSubmit",
                label = 'Submit'
              )
            ),
            column(width = 10, textOutput(outputId = "ciMeanGuessFeedback"))
          )
        ),
        ### CI for Prop ----
        tabItem(
          tabName = "ciProp",
          h2("Confidence Interval for Proportion"),
          h4("Step 1: Context"),
          fluidRow(
            column(
              width = 12, 
              p("CONTEXT HERE")
            )
          ),
          br(),
          tabsetPanel(
            id = "simulationType",
            ##### Identifying Components ----
            tabPanel(
              title = "Step 2: Identifying Components",
              value = "b",
              wellPanel(
                fluidRow(
                  column(
                    width = 5, 
                    selectInput(
                      inputId = "ciPropPop",
                      label = "Population",
                      choices = c("HERE")
                    )
                  ),
                  column(width = 1, uiOutput(outputId = "ciPropPopIcon")),
                  column(
                    width = 5, 
                    selectInput(
                      inputId = "ciPropPara",
                      label = "Population Parameter",
                      choices = c("HERE")
                    )
                  ),
                  column(width = 1, uiOutput(outputId = "ciPropParaIcon"))
                ),
                fluidRow(
                  column(
                    width = 5, 
                    selectInput(
                      inputId = "ciPropSamp",
                      label = "Sample",
                      choices = c("HERE")
                    )
                  ),
                  column(width = 1, uiOutput(outputId = "ciPropSampIcon")),
                  column(
                    width = 5, 
                    selectInput(
                      inputId = "ciPropStat",
                      label = "Sample Statistic",
                      choices = c("HERE")
                    )
                  ),
                  column(width = 1, uiOutput(outputId = "ciPropStatIcon")),
                ),
                fluidRow(
                  column(
                    width = 9,
                    textOutput(outputId = "ciPropCompFeed")
                  ),
                  column(
                    width = 1,
                    bsButton(
                      inputId = "ciPropReset",
                      label = "Reset"
                    )
                  ),
                  column(
                    width = 1,
                    bsButton(
                      inputId = "ciPropSubmit",
                      label = "Submit"
                    )
                  )
                )
              )
            ),
            ##### Simulation  ----
            tabPanel(
              title = "Step 3: Simulation",
              value = "b",
              fluidRow(
                column(
                  width = 4,
                  wellPanel(
                    sliderInput(
                      inputId = "ciPropNS",
                      label = "Number of Samples",
                      min = 109000, 
                      max = 110000,
                      value = 109490,
                      step = 35
                    ),
                    sliderInput(
                      inputId = "ciPropNumRep",
                      label = "Number of Replicaions",
                      min = 1, 
                      max = 5000,
                      value = 2500
                    ),
                    numericInput(
                      inputId = "ciPropCL",
                      label = 'Confidence Level',
                      value = 0.95
                    ),
                    checkboxInput(
                      inputId = "ciPropReplications",
                      label = "Use Replication",
                      value = FALSE
                    ),
                    bsButton(
                      inputId = "simCIProp",
                      label = "Simulate"
                    )
                  )
                ),
                column(
                  width = 8,
                  plotOutput("ciPropSim"),
                  textOutput('ciPropResults')
                )
              )
            ),
          ),
          #### Answer ----
          h4("Step 4: Answer"),
          fluidRow(
            column( 
              width = 2,
              numericInput(
                inputId = 'ciPropLower',
                label = 'Lower Bound',
                value = 0.0,
                step = 0.01
              )
            ),
            column(width = 1, uiOutput(outputId = "ciPropLowerIcon")),
            column(
              width = 2, 
              numericInput(
                inputId = 'ciPropUpper',
                label = 'Upper Bound',
                value = 0.0,
                step = 0.01
              )
            ),
            column(width = 1, uiOutput(outputId = "ciPropUpperIcon")),
          ),
          fluidRow(
            column(
              width = 2,
              bsButton(
                inputId = "ciPropGuessSubmit",
                label = 'Submit'
              )
            ),
            column(width = 10, textOutput(outputId = "ciPropGuessFeedback"))
          )
        ),      
        ### Probability ----
        tabItem(
          tabName = "prob",
          h2("Estimating Probability"),
          h4("Step 1: Context"),
          fluidRow(
            column(
              width = 12,
              p("Nick and Jennifer were rolling dice to see who could get
                  a higher total. Nick claims that he can get a higher total with
                  less die, but Jennifer does not believe him. So Nick rolls 
                  5 die  while Jennifer rolls 6 die simutaneously. What is the probability 
                  that Nick gets a higher total than Jennifer?"),
            )
          ),
          
          tabsetPanel(
            id = "simulationType",
            ##### Identifying Components ----
            tabPanel(
              title = "Step 2: Identifying Components",
              value = "b",
              wellPanel(
                fluidRow(
                  column(
                    width = 5, 
                    selectInput(
                      inputId = "probPop",
                      label = "Population",
                      choices = c("HERE")
                    )
                  ),
                  column(width = 1, uiOutput(outputId = "probPopIcon")),
                  column(
                    width = 5, 
                    selectInput(
                      inputId = "probSamp",
                      label = "Sample",
                      choices = c("HERE")
                    )
                  ),
                  column(width = 1, uiOutput(outputId = "probSampIcon")),
                ),
                fluidRow(
                  column(
                    width = 5, 
                    selectInput(
                      inputId = "probEvent",
                      label = "Event",
                      choices = c("HERE")
                    )
                  ),
                  column(width = 1, uiOutput(outputId = "probEventIcon")),
                  column(
                    width = 5, 
                    selectInput(
                      inputId = "probReplace",
                      label = "Replacement",
                      choices = c("With replacement", "Without replacement")
                    )
                  ),
                  column(width = 1, uiOutput(outputId = "probReplaceIcon")),
                ),
                fluidRow(
                  column(
                    width = 9,
                    textOutput(outputId = "probCompFeed")
                  ),
                  column(
                    width = 1,
                    bsButton(
                      inputId = "probReset",
                      label = "Reset"
                    )
                  ),
                  column(
                    width = 1,
                    bsButton(
                      inputId = "probSubmit",
                      label = "Submit"
                    )
                  )
                )
              )
            ),
            ##### Simulation  ----
            tabPanel(
              title = "Step 3: Simulation",
              value = "b",
              fluidRow(
                column(
                  width = 4,
                  wellPanel(
                    sliderInput(
                      inputId = 'trialsProb', 
                      label = "Number of Trials:",
                      min = 2, 
                      max = 1000,
                      value = 2
                    ),
                    sliderInput(
                      inputId = 'nickRollsProb', 
                      label = "Nick Number of Rolls:",
                      min = 1, 
                      max = 10,
                      value = 1
                    ),
                    sliderInput(
                      inputId = 'jennRollsProb', 
                      label = "Jennifer Number of Rolls:",
                      min = 1, 
                      max = 10,
                      value = 1
                    ),
                    bsButton(
                      inputId = 'simProb',
                      label = 'Simulate'
                    )
                  )
                ),
                column(
                  width = 8,
                  plotOutput('probSim'),
                  textOutput('resultProb')
                )
              )
            )
          ),
          #### Answer ----
          h4("Step 4: Answer"),
          fluidRow(
            column(
              width = 5, 
              numericInput(
                inputId = 'guessProb',
                label = 'Estimated probability that Nick wins',
                value = 0.0,
                step = 0.01
              )
            ),
            column(width = 1, uiOutput(outputId = "guessIconProb"))
          ),
          fluidRow(
            column(
              width = 2,
              bsButton(
                inputId = "guessSubmitProb",
                label = 'Submit'
              )
            ),
            column(width = 10, textOutput(outputId = "guessProbFeedback"))
          ),
        ),
        ### References Page ----
        tabItem(
          tabName = "references",
          withMathJax(),
          h2("References"),
          p(
            class = "hangingindent",
            "“American Airports - Guide to Airports in United States.” American Airports - Guide to Airports in United States, www.americanairportguide.com/."
          ),
          p(
            class = "hangingindent",
            "Bailey, E. (2015). shinyBS: Twitter bootstrap components for shiny.
            (v0.61). [R package]. Available from
            https://CRAN.R-project.org/package=shinyBS"
          ),
          p(
            class = "hangingindent",
            "Carey, R. and Hatfield, N.J. (2023). boastUtils: BOAST utilities. 
            (v0.1.11.2). [R Package]. Avaliable from 
            https://github.com/EducationShinyappTeam/boastUtils"
          ),
          p(
            class = "hangingindent",
            "Chang, W. and Borges Ribeio (2021). shinydashboard: Create dashboards
            with 'Shiny.' (v0.7.2). Avaliable from
            https://CRAN.R-project.org/package=shinydashboard"
          ),
          p(
            class = "hangingindent",
            "Chang W, Cheng J, Allaire J, Sievert C, Schloerke B, Xie Y, Allen J, 
            McPherson J, Dipert A, Borges B (2023). shiny: Web Application Framework 
            for R. R package version 1.7.4.9002. Avaliable from 
            https://CRAN.R-project.org/package=shiny"
          ),
          p(
            class = "hangingindent",
            "“District Employees and Finance.” The School District of Philadelphia, www.philasd.org/performance/programsservices/open-data/district-information/#employee_data."
          ),
          p(
            class = "hangingindent",
            "Perrier, V., Meyer, F., Granjon, D. (2023) shinyWidgets: Custom Input
            Widgets for Shiny. (v0.7.6). Avaliable from 
            https://cran.r-project.org/web/packages/shinyWidgets/index.html"
          ),
          p(
            class = "hangingindent",
            "Wickham H (2016). ggplot2: Elegant Graphics for Data Analysis. 
            Springer-Verlag New York. ISBN 978-3-319-24277-4. Avaliable from 
            https://ggplot2.tidyverse.org."
          ),
          br(),
          br(),
          br(),
          boastUtils::copyrightInfo()
        )
      )
    )
  )
)

# Define server logic ----
server <- function(input, output, session) {
  
  ## Info button ----
  observeEvent(
    eventExpr = input$info,
    handlerExpr = {
      tab <- input$pages
      if (tab == "prerequisites") {
        sendSweetAlert(
          session = session, 
          type = "info",
          title = "Information",
          text = "View the definitions and terms as needed. Once ready 
          go trhough each simulation left on the tab options, identify components
          and practice different types of simulations."
        )
      } else if (tab %in% c("ciMean", "ciProb", "hypTest", "prob")) {
        sendSweetAlert(
          session = session,
          type = "info",
          title = "Information",
          text = "Click through each tab to view different type of simulations. 
          Read the context, identify components, and practice  each simulation
          type."
        )
      } else {
        sendSweetAlert(
          session = session,
          type = "info",
          title = "Information",
          text = "View the prerequisties as needed. Once ready 
          go trhough each simulation left on the tab options, identify components
          and practice different types of simulations."
        )
      }
    }
  )
  
  ## Overview to Prereq Button----
  observeEvent(
    eventExpr = input$overviewToPrereq,
    handlerExpr = {
      updateTabItems(
        session = session,
        inputId = "pages",
        selected = "prerequisites"
      )
    }
  )
  
  ## Confidence Interval for Mean ----
  observeEvent(
    input$ciMeanSubmit,
    handlerExpr = {
      selectedOption <- input$ciMeanPop
      if (selectedOption == "International airports in the Northeast region") {
        output$ciMeanPopIcon <- renderIcon(icon = "correct", width = 30)
      } else {
        output$ciMeanPopIcon <- renderIcon(icon = "incorrect", width = 30)
      }
    }
  )
  observeEvent(
    input$ciMeanSubmit,
    handlerExpr = {
      selectedOption <- input$ciMeanPara
      if (selectedOption == "Number of flights arriving/departing") {
        output$ciMeanParaIcon <- renderIcon(icon = "correct", width = 30)
      } else {
        output$ciMeanParaIcon <- renderIcon(icon = "incorrect", width = 30)
      }
    }
  )
  observeEvent(
    input$ciMeanSubmit,
    handlerExpr = {
      selectedOption <- input$ciMeanSamp
      if (selectedOption == "6 International Airports") {
        output$ciMeanSampIcon <- renderIcon(icon = "correct", width = 30)
      } else {
        output$ciMeanSampIcon <- renderIcon(icon = "incorrect", width = 30)
      }
    }
  )
  observeEvent(
    input$ciMeanSubmit,
    handlerExpr = {
      selectedOption <- input$ciMeanStat
      if (selectedOption == "20600" || selectedOption == "22253") {
        output$ciMeanStatIcon <- renderIcon(icon = "correct", width = 30)
      } else {
        output$ciMeanStatIcon <- renderIcon(icon = "incorrect", width = 30)
      }
    }
  )
  #### Feedback
  observeEvent(
    input$ciMeanSubmit,
    handlerExpr = {
      pop <- input$ciMeanPop
      para <- input$ciMeanPara
      samp <- input$ciMeanSamp
      stat <- input$ciMeanStat
  
      if (pop != "International airports in the Northeast region" || para != "Number of flights arriving/departing"||
          samp != "6 International Airports" || (stat != "20600" && stat != "22253")) {
        output$ciMeanCompFeed <- renderText("Remember that population relates to a whole and sample relates to a small section of the population.")
      } else {
        output$ciMeanCompFeed <- renderText("Correct!")
      }  
    }
  )
  #### Reset
  observeEvent(
    eventExpr = input$ciMeanReset,
    handlerExpr = {
      output$ciMeanPopIcon <- renderIcon()
      output$ciMeanParaIcon <- renderIcon()
      output$ciMeanSampIcon <- renderIcon()
      output$ciMeanStatIcon <- renderIcon()
    }
  )
  
  ### Simulation ----
    upperCI <- reactiveVal(0)
    lowerCI <- reactiveVal(0)
  
    observeEvent(
    input$simCIMean, 
    handlerExpr = {
      numSamp <- input$ciMeanNS
      numRep <- input$ciMeanNumRep
      cl <- input$ciMeanCL
      rep <- input$ciMeanReplications
      
      stat <- function(data, index) {
        subset_data <- data$Flights[index] 
        statistic_value <- mean(subset_data)
        return(statistic_value)
      }
      
      set.seed(461)
      
      flightData
      bootOut <- boot::boot(
        data = flightData,
        statistic = stat,
        R = numRep
      )
      bootCI <- boot::boot.ci(
        boot.out = bootOut,
        conf = cl,
        type = "perc"
      )
      
      lower <- (1 - cl)/2
      upper <- cl + lower 
      cii <- quantile(bootOut$t, c(lower, upper)) 
      
      upperCI(round(cii[2],2))
      lowerCI(round(cii[1],2))
      
      if(rep == FALSE || numRep < 100 || numSamp != 253) {
        output$ciMeanResults <- renderText(
          expr = {
            "Review the conditions. What is a requirement of bootstrapping?"
          }
        )
        output$ciMeanSim <- renderPlot(
          expr = {
            ggplot() + 
              geom_blank()
          }
        )
      } else {
        output$ciMeanSim <- renderPlot(
          expr = {
            ggplot() +
              geom_histogram(
                data = data.frame(x = bootOut$t), 
                aes(x = x), 
                bins = 15, fill = boastPalette[1], color = "black"
              ) +
              geom_vline(
                xintercept = quantile(bootOut$t, c(lower, upper)), 
                color = "red", 
                linetype = "dashed"
              ) +
              labs(x = "Bootstrap Mean", y = "Frequency")
          }
        )
        output$ciMeanResults <- renderText({
          paste("Lower Confidence Interval:", round(cii[1], 2), "\n",
                "Upper Confidence Interval:", round(cii[2], 2))
        })
      }
    })
  
  #### Guessing Feedback
  observeEvent(
    input$ciMeanGuessSubmit,
    handlerExpr = {
      numSamp <- input$ciMeanNS
      numRep <- input$ciMeanNumRep
      cl <- input$ciMeanCL
      rep <- input$ciMeanReplications
      
      upper <- input$ciMeanUpper
      lower <- input$ciMeanLower
      
      if (numRep > 500 && rep && lowerCI() == lower && upperCI() == upper && cl == .90 && numSamp == 253) {
        output$ciMeanLowerIcon <- renderIcon(icon = "correct", width = 30)
        output$ciMeanUpperIcon <- renderIcon(icon = "correct", width = 30)
        output$ciMeanGuessFeedback <- renderText("Correct!")
      } else if (rep == FALSE || numSamp != 253) {
        output$ciMeanLowerIcon <- renderIcon(icon = "incorrect", width = 30)
        output$ciMeanUpperIcon <- renderIcon(icon = "incorrect", width = 30)
        output$ciMeanGuessFeedback <- renderText("Think of conditions needed to be met in order to boostrap.")
      } else if (numRep < 500) {
        output$ciMeanLowerIcon <- renderIcon(icon = "incorrect", width = 30)
        output$ciMeanUpperIcon <- renderIcon(icon = "incorrect", width = 30)
        output$ciMeanGuessFeedback <- renderText("Think of how many replications are needed to be representative.")
      } else if (cl != .9) {
        output$ciMeanLowerIcon <- renderIcon(icon = "incorrect", width = 30)
        output$ciMeanUpperIcon <- renderIcon(icon = "incorrect", width = 30)
        output$ciMeanGuessFeedback <- renderText("Reread the context and verify inputs match")
      } else {
        output$ciMeanLowerIcon <- renderIcon(icon = "incorrect", width = 30)
        output$ciMeanUpperIcon <- renderIcon(icon = "incorrect", width = 30)
        output$ciMeanGuessFeedback <- renderText("Incorrect, reread the context and make adjustments to inputs. References the prerequsities as needed.")
      }
    }
  )
  
  ## Confidence Interval for Proportion----
  observeEvent(
    input$ciPropSubmit,
    handlerExpr = {
      selectedOption <- input$ciPropPop
      if (selectedOption == "HERE") {
        output$ciPropPopIcon <- renderIcon(icon = "correct", width = 30)
      } else {
        output$ciPropPopIcon <- renderIcon(icon = "incorrect", width = 30)
      }
    }
  )
  observeEvent(
    input$ciPropSubmit,
    handlerExpr = {
      selectedOption <- input$ciPropPara
      if (selectedOption == "HERE") {
        output$ciPropParaIcon <- renderIcon(icon = "correct", width = 30)
      } else {
        output$ciPropParaIcon <- renderIcon(icon = "incorrect", width = 30)
      }
    }
  )
  observeEvent(
    input$ciPropSubmit,
    handlerExpr = {
      selectedOption <- input$ciPropSamp
      if (selectedOption == "HERE") {
        output$ciPropSampIcon <- renderIcon(icon = "correct", width = 30)
      } else {
        output$ciPropSampIcon <- renderIcon(icon = "incorrect", width = 30)
      }
    }
  )
  observeEvent(
    input$ciPropSubmit,
    handlerExpr = {
      selectedOption <- input$ciPropStat
      if (selectedOption == "HERE") {
        output$ciPropStatIcon <- renderIcon(icon = "correct", width = 30)
      } else {
        output$ciPropStatIcon <- renderIcon(icon = "incorrect", width = 30)
      }
    }
  )
  #### Feedback
  observeEvent(
    input$ciPropSubmit,
    handlerExpr = {
      pop <- input$ciPropPop
      para <- input$ciPropPara
      samp <- input$ciPropSamp
      stat <- input$ciPropStat

      if (pop != "HERE" || para != "HERE" ||
          samp != "HERE" || (stat != "HERE")) {
        output$ciPropCompFeed <- renderText("Remember that population relates to a whole and sample relates to a small section of the population.")
      } else {
        output$ciPropCompFeed <- renderText("Correct!")
      }  
    }
  )
  #### Reset
  observeEvent(
    eventExpr = input$ciPropReset,
    handlerExpr = {
      output$ciPropPopIcon <- renderIcon()
      output$ciPropParaIcon <- renderIcon()
      output$ciPropSampIcon <- renderIcon()
      output$ciPropStatIcon <- renderIcon()
    }
  )

  ### Simulation ----
  upperCI <- reactiveVal(0)
  lowerCI <- reactiveVal(0)
  
  observeEvent(
    input$simCIProp, 
    handlerExpr = {
      sampleSamp <- input$ciPropNS
      numRep <- input$ciPropNumRep
      cl <- input$ciPropCL
      rep <- input$ciPropReplications
      
      proportionData <- data.frame(Success = c(
        rep(1, round(sqrt(41222))), 
        rep(0, round(sqrt(67848)))),
        Total = rep(1, round(sqrt(41222)) + round(sqrt(67848)))
      )
      stat <- function(data, index) {
        subset_data <- data[index, ]
        successes <- sum(subset_data$Success)
        total <- sum(subset_data$Total)
        proportion_value <- successes / total
        return(proportion_value)
      }
      
      set.seed(461)
      
      bootOut <- boot::boot(
        data = proportionData,
        statistic = stat,
        R = numRep
      )
      bootCI <- boot::boot.ci(
        boot.out = bootOut,
        conf = cl,
        type = "perc"
      )
      
      lower <- (1 - cl)/2
      upper <- cl + lower
      cii <- quantile(bootOut$t, c(lower, upper))
      
      upperCI(round(cii[2], 2))
      lowerCI(round(cii[1], 2))
      
      if (rep == FALSE || numRep < 100) {
        output$ciPropResults <- renderText({
          "Review the conditions. What is a requirement of bootstrapping?"
        })
        output$ciPropSim <- renderPlot({
          ggplot() +
            geom_blank()
        })
      } else {
        output$ciPropSim <- renderPlot({
          ggplot() +
            geom_histogram(
              data = data.frame(x = bootOut$t), 
              aes(x = x), 
              bins = 15, 
              fill = boastPalette[1],
              color = "black"
            ) +
            geom_vline(
              xintercept = quantile(bootOut$t, c(lower, upper)), 
              color = "red", 
              linetype = "dashed"
            ) +
            labs(x = "Bootstrap Proportion", y = "Frequency")
        })
        output$ciPropResults <- renderText({
          paste("Lower Confidence Interval:", round(cii[1], 2), "\n",
                "Upper Confidence Interval:", round(cii[2], 2))
        })
      }
    })
    
    #### Guessing Feedback
    observeEvent(
      input$ciPropGuessSubmit,
      handlerExpr = {
        numSamp <- input$ciPropNS
        numRep <- input$ciPropNumRep
        cl <- input$ciPropCL
        rep <- input$ciPropReplications
        
        upper <- input$ciPropUpper
        lower <- input$ciPropLower
        
        if (numRep > 500 && rep && lowerCI() == lower && upperCI() == upper && cl == .98 && numSamp == 109070) {
          output$ciPropLowerIcon <- renderIcon(icon = "correct", width = 30)
          output$ciPropUpperIcon <- renderIcon(icon = "correct", width = 30)
          output$ciPropGuessFeedback <- renderText("Correct!")
        } else if (rep == FALSE || numSamp != 109070) {
          output$ciPropLowerIcon <- renderIcon(icon = "incorrect", width = 30)
          output$ciPropUpperIcon <- renderIcon(icon = "incorrect", width = 30)
          output$ciPropGuessFeedback <- renderText("Think of conditions needed to be met in order to boostrap.")
        } else if (numRep < 500) {
          output$ciPropLowerIcon <- renderIcon(icon = "incorrect", width = 30)
          output$ciPropUpperIcon <- renderIcon(icon = "incorrect", width = 30)
          output$ciPropGuessFeedback <- renderText("Think of how many replications are needed to be representative.")
        } else if (cl != .98) {
          output$ciPropLowerIcon <- renderIcon(icon = "incorrect", width = 30)
          output$ciPropUpperIcon <- renderIcon(icon = "incorrect", width = 30)
          output$ciPropGuessFeedback <- renderText("Reread the context and verify inputs match")
        } else {
          output$ciPropLowerIcon <- renderIcon(icon = "incorrect", width = 30)
          output$ciPropUpperIcon <- renderIcon(icon = "incorrect", width = 30)
          output$ciPropGuessFeedback <- renderText("Incorrect, reread the context and make adjustments to inputs. References the prerequsities as needed.")
        }
      }
    )
    
    ## Probability ----
    probability <- reactiveVal(0)
    
    observeEvent(
      input$probSubmit,
      handlerExpr = {
        selectedOption <- input$probPop
        if (selectedOption == "INSERT ASNWER") {
          output$probPopIcon <- renderIcon(icon = "correct", width = 30)
        } else {
          output$probPopIcon <- renderIcon(icon = "incorrect", width = 30)
        }
      }
    )
    observeEvent(
      input$probSubmit,
      handlerExpr = {
        selectedOption <- input$probSamp
        if (selectedOption == "INSERT ANSWER") {
          output$probSampIcon <- renderIcon(icon = "correct", width = 30)
        } else {
          output$probSampIcon <- renderIcon(icon = "incorrect", width = 30)
        }
      }
    )
    observeEvent(
      input$probSubmit,
      handlerExpr = {
        selectedOption <- input$probEvent
        if (selectedOption == "INSERT ANSWER") {
          output$probEventIcon <- renderIcon(icon = "correct", width = 30)
        } else {
          output$probEventIcon <- renderIcon(icon = "incorrect", width = 30)
        }
      }
    )
    observeEvent(
      input$probSubmit,
      handlerExpr = {
        selectedOption <- input$probReplace
        if (selectedOption == "With replacement") {
          output$probReplaceIcon <- renderIcon(icon = "correct", width = 30)
        } else {
          output$probReplaceIcon <- renderIcon(icon = "incorrect", width = 30)
        }
      }
    )
    
    #### Feedback
    observeEvent(
      input$probSubmit,
      handlerExpr = {
        pop <- input$probPop
        samp <- input$probSamp
        event <- input$probEvent
        replace <- input$probReplace
        
        if (pop != "HERE" || samp != "HERE" ||
            event != "HERE" || replace != "With replacement") {
          output$probCompFeed <- renderText("Remember that population relates to a whole and sample relates to a small section of the population.")
        } else {
          output$probCompFeed <- renderText("Correct!")
        }  
      }
    )
    
    ### Simulations ----
    observeEvent(
      input$simProb, 
      handlerExpr = {
        trials <- input$trialsProb
        nickRolls <- input$nickRollsProb
        jennRolls <- input$jennRollsProb
        
        nickWin <- 0
        tie <- 0 
        jennWin <- 0
        totalScore <- numeric(trials)
        
        for (i in 1:trials) {
          nickResults <- sample(1:6, nickRolls, replace = TRUE)
          jennResults <- sample(1:6, jennRolls, replace = TRUE)
          nickSum <- sum(nickResults)
          jennSum <- sum(jennResults)
          
          if (nickSum > jennSum) {
            nickWin <- nickWin + 1
          } else if (jennSum > nickSum) {
            jennWin <- jennWin + 1
          } else {
            tie <- tie + 1
          }
          totalScore[i] <- nickSum - jennSum
        }
        
        probability(nickWin / trials)
        
        output$resultProb <- renderText({
          paste("Estimated probability that Nick gets a higher total:", round(probability(),4), "\n")
        })
        
        output$probSim <- renderPlot({
          results <- data.frame(
            outcome = c("Nick Wins", "Jennifer Wins", "Tie"),
            frequency = c(nickWin, jennWin, tie))
          barplot(results$frequency, names.arg = results$outcome,
                  main = "Distribution of Outcomes",
                  xlab = "Outcome", 
                  ylab = "Frequency", 
                  col = "blue", 
                  ylim = c(0, max(results$frequency) * 1.1))
        })
      })
    
  observeEvent(
    input$guessSubmitProb,
    handlerExpr = {
      guess <- input$guessProb
      trials <- input$trialsProb
      nickRolls <- input$nickRollsProb
      jennRolls <- input$jennRollsProb
      
      #### Guessing Feedback
      if (trials > 100 && nickRolls == 5 && jennRolls == 6 && abs(probability() - guess) < 0.001) {
        output$guessIconProb <- renderIcon(icon = "correct", width = 30)
        output$guessProbFeedback <- renderText("Correct!")
      } else if (trials > 100 && nickRolls == 5 && jennRolls == 6) { 
        output$guessIconProb <- renderIcon(icon = "incorrect", width = 30)
        output$guessProbFeedback <- renderText("Look at the estimated probability below the chart")
      } else if (trials < 100 && nickRolls == 5 && jennRolls == 6) { 
        output$guessIconProb <- renderIcon(icon = "incorrect", width = 30)
        output$guessProbFeedback <- renderText("Keep in mind how many trials should be used")
      } else if ((nickRolls != 5 | jennRolls != 6) & trials > 100) {
        output$guessIconProb <- renderIcon(icon = "incorrect", width = 30)
        output$guessProbFeedback <- renderText("Double check the number of rolls for Nick and Jennifer")
      } else {
        output$guessIconProb <- renderIcon(icon = "incorrect", width = 30)
        output$guessProbFeedback <- renderText("Set the simulation options according to the context")
      }
    }
  )
}

# Boast App Call ----
boastUtils::boastApp(ui = ui, server = server)
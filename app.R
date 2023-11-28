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
              inputId = "overviewToMean",
              label = "CI for Mean",
              size = "large",
              icon = icon("wpexplorer")
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
                      confidence."),
              tags$li("Event: A specific outcome or a set of outcomes that we are interested
                      in observing or analyzing.")
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
                          look at the variability across bootstrap samples to estimate 
                          the variability around the original sample statistic."),
                  tags$li("Use as many replications of bootstrap samples as practical.")
                )
              )
            )
          )
        ),
        ### CI for Mean ----
        tabItem(
          tabName = "ciMean",
          h2("Confidence Interval for Mean"),
          h4("Step 1: Context"),
          fluidRow(
            column(
              width = 12,
              p("Every year numerous airplanes takeoff and land at international
                airports globally, with approximately 32 situated in the Northeast region. 
                In a study, a sample from 8 international airports in the 9 Northeastern 
                states. Over the period from December 2020 to August 2023, a total of 
                290 data points were collected regarding the number of planes taking off 
                or landing. It revealed that the mean number of planes engaged in these activities 
                was 16517. Create a 98% confidence interval for the number of flights 
                departing/arriving at northeast international airports based on this data.")
            )
          ),
          tabsetPanel(
            id = "simulationType",
            #### Identifying Components ----
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
                      choices = c(
                        " ", "International airports", "Airports in the Northeast region", 
                        "Number of flights arriving/departing", 
                        "Number of layovers", "Number of international airports",  
                        "International airports in the Northeast region"
                      )
                    )
                  ),
                  column(width = 1, uiOutput(outputId = "ciMeanPopIcon")), 
                  column(
                    width = 5, 
                    selectInput(
                      inputId = "ciMeanPara",
                      label = "Population Parameter",
                      choices = c(
                        " ", "Number of international airports", 
                        "Airports in the Northeast region", "Number of flights arriving/departing", 
                        "International airports", "Number of layovers",
                        "International airports in the Northeast region"
                      )
                    )
                  ),
                  column(width = 1, uiOutput(outputId = "ciMeanParaIcon"))
                ),
                fluidRow(
                  column(
                    width = 5, 
                    selectInput(
                      inputId = "ciMeanSamp",
                      label = "Sample",
                      choices = c(
                        " ", "16517", "All airports", "290", "32", 
                        "8 International airports", "9 States in Northeast region"
                      )
                    )
                  ),
                  column(width = 1, uiOutput(outputId = "ciMeanSampIcon")),
                  column(
                    width = 5, 
                    selectInput(
                      inputId = "ciMeanStat",
                      label = "Sample Statistic",
                      choices = c( 
                        " ", "290", "All airports", "9 States in Northeast region", 
                        "32", "8 International airports", "16517"
                      )
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
            #### Simulation  ----
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
          #### Answer ----
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
            column(width = 1, uiOutput(outputId = "ciMeanUpperIcon"))
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
              p("Each year QSR magazine does a survey of drive-thru service at America’s
                quick serve restaurants.  The magazine selects a number of restaurants
                from each of the major chains and sends a customer to order a meal 
                from the drive thru window.  The order will include an entree, a 
                side dish, and a drink, and one deviation from the usual order. 
                QSR asks the customer to rate their experience in different ways,
                including how much time it took for them to be served and whether
                the restaurant got the order correct or not.  In the 2023 QSR survey, 
                165 out of Burger King’s 6500 restaurants with drive-thru windows 
                were selected and it turned out that there was a mistake in the 
                order at 16 of the sampled restaurants. Create a 90% confidence 
                interval for the proportion of all Burger King Drive Thru windows 
                that would make a mistake in an order of this type.")
            )
          ),
          tabsetPanel(
            id = "simulationType",
            #### Identifying Components ----
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
                      choices = c(
                        " ", "All U.S. Burger Kings with a drive through window",
                        "All U.S. Burger Kings", "Time it took to be served",
                        "Number of Burger Kings that made a mistake",
                        "Number of Impossible Burgers ordered", "All U.S. Burger Kings with seating"
                      )
                    )
                  ),
                  column(width = 1, uiOutput(outputId = "ciPropPopIcon")),
                  column(
                    width = 5, 
                    selectInput(
                      inputId = "ciPropPara",
                      label = "Population Parameter",
                      choices = c(
                        " ", "Time it took to be served", "All U.S. Burger Kings with seating",
                        "Number of Burger Kings that made a mistake",
                        "Number of Impossible Burgers ordered",
                        "All U.S. Burger Kings with a drive through window",
                        "All U.S. Burger Kings"
                      )
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
                      choices = c(
                        " ", "165/6500", "6500 QSR readers", "16/165",
                        "165 Burger Kings tested by QSR magazine",
                        "165 fast food restaurants", "16/6500"
                      )
                    )
                  ),
                  column(width = 1, uiOutput(outputId = "ciPropSampIcon")),
                  column(
                    width = 5, 
                    selectInput(
                      inputId = "ciPropStat",
                      label = "Sample Statistic",
                      choices = c(
                        " ", "16/165", "165 Burger Kings tested by QSR magazine",
                        "6500 QSR readers", "165 fast food restaurants", "165/6500",
                        "16/6500"
                      )
                    )
                  ),
                  column(width = 1, uiOutput(outputId = "ciPropStatIcon"))
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
            #### Simulation  ----
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
                      min = 100, 
                      max = 200,
                      value = 150
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
            )
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
            column(width = 1, uiOutput(outputId = "ciPropUpperIcon"))
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
                  fewer dice, but Jennifer does not believe him. So Nick rolls 
                  5 dice  while Jennifer rolls 6 dice simutaneously. What is the probability 
                  that Nick gets a higher total than Jennifer?")
            )
          ),
          tabsetPanel(
            id = "simulationType",
            #### Identifying Components ----
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
                      choices = c(
                        " ", "All possible sum combinations", "Sum of all the dice", 
                        "Possible outcome 1-6 on each die"
                      )
                    )
                  ),
                  column(width = 1, uiOutput(outputId = "probPopIcon")),
                  column(
                    width = 5, 
                    selectInput(
                      inputId = "probSamp",
                      label = "Sample",
                      choices = c(
                        " ", "Sum of Nick's 5 dice rolls and Jennifer'6 dice rolls",
                        "Sum of the 11 dice", "The number of trials"
                      )
                    )
                  ),
                  column(width = 1, uiOutput(outputId = "probSampIcon"))
                ),
                fluidRow(
                  column(
                    width = 5, 
                    selectInput(
                      inputId = "probEvent",
                      label = "Event",
                      choices = c(
                        " ", "Whether Jennifer has a higher total than Nick",
                        "Whether Nick has a higher total than Jennifer",
                        "Whether Nick and Jennifer end up in a tie"
                      )
                    )
                  ),
                  column(width = 1, uiOutput(outputId = "probEventIcon")),
                  column(
                    width = 5, 
                    selectInput(
                      inputId = "probReplace",
                      label = "Replacement",
                      choices = c(" ", "With replacement", "Without replacement")
                    )
                  ),
                  column(width = 1, uiOutput(outputId = "probReplaceIcon"))
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
            #### Simulation  ----
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
          )
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
            "Canty, A. and Ripley, B. boot: Boostrap Functions (Originally by Angelo
            Canty for S). (v1.3-28.1). Avaliable from 
            https://cran.r-project.org/web/packages/boot/index.html"
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
            "Perrier, V., Meyer, F., Granjon, D. (2023) shinyWidgets: Custom Input
            Widgets for Shiny. (v0.7.6). Avaliable from 
            https://cran.r-project.org/web/packages/shinyWidgets/index.html"
          ),
          p(
            class = "hangingindent",
            "Wickham, H. (2016). ggplot2: Elegant Graphics for Data Analysis. 
            Springer-Verlag New York. ISBN 978-3-319-24277-4. Avaliable from 
            https://ggplot2.tidyverse.org."
          ),
          p(
            class = "hangingindent",
            "REFERNCE HERE FOR BK"
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
  
  ## Overview to Mean Button----
  observeEvent(
    eventExpr = input$overviewToMean,
    handlerExpr = {
      updateTabItems(
        session = session,
        inputId = "pages",
        selected = "ciMean"
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
      if (selectedOption == "8 International airports") {
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
      if (selectedOption == "16517") {
        output$ciMeanStatIcon <- renderIcon(icon = "correct", width = 30)
      } else {
        output$ciMeanStatIcon <- renderIcon(icon = "incorrect", width = 30)
      }
    }
  )
  ### Feedback
  observeEvent(
    input$ciMeanSubmit,
    handlerExpr = {
      pop <- input$ciMeanPop
      para <- input$ciMeanPara
      samp <- input$ciMeanSamp
      stat <- input$ciMeanStat
  
      if (pop != "International airports in the Northeast region" || para != "Number of flights arriving/departing"||
          samp != "6 International airports" || stat != "16517") {
        output$ciMeanCompFeed <- renderText("Remember that population relates to a whole and sample relates to a small section of the population.")
      } else {
        output$ciMeanCompFeed <- renderText("Correct!")
      }  
    }
  )
  ### Reset
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
      
      if(rep == FALSE || numRep < 100 || numSamp != 290) {
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
      
      if (numRep > 100 && rep && lowerCI() == lower && upperCI() == upper && cl == .98 && numSamp == 290) {
        output$ciMeanLowerIcon <- renderIcon(icon = "correct", width = 30)
        output$ciMeanUpperIcon <- renderIcon(icon = "correct", width = 30)
        output$ciMeanGuessFeedback <- renderText("Correct!")
      } else if (numRep > 100 && rep && numSamp == 290 && cl == .98 && (upperCI() != upper || lowerCI() != lower)) {
        output$ciMeanLowerIcon <- renderIcon(icon = "incorrect", width = 30)
        output$ciMeanUpperIcon <- renderIcon(icon = "incorrect", width = 30)
        output$ciMeanGuessFeedback <- renderText("Make sure bounds match the ones below the graph")
      } else if (rep == FALSE || numSamp != 290) {
        output$ciMeanLowerIcon <- renderIcon(icon = "incorrect", width = 30)
        output$ciMeanUpperIcon <- renderIcon(icon = "incorrect", width = 30)
        output$ciMeanGuessFeedback <- renderText("Think of conditions needed to be met in order to boostrap.")
      } else if (numRep < 100) {
        output$ciMeanLowerIcon <- renderIcon(icon = "incorrect", width = 30)
        output$ciMeanUpperIcon <- renderIcon(icon = "incorrect", width = 30)
        output$ciMeanGuessFeedback <- renderText("Think of how many replications are needed to be representative.")
      } else if (cl != .98) {
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
      if (selectedOption == "All U.S. Burger Kings with a drive through window") {
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
      if (selectedOption == "Number of Burger Kings that made a mistake") {
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
      if (selectedOption == "165 Burger Kings tested by QSR magazine") {
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
      if (selectedOption == "16/165") {
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

      if (pop != "All U.S. Burger Kings with a drive through window" || para != "Number of Burger Kings that made a mistake" ||
          samp != "165 Burger Kings tested by QSR magazine" || (stat != "16/165")) {
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
        rep(1, 16), 
        rep(0, 149)),
        Total = rep(1, 165)
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
        
        if (numRep > 100 && rep && lowerCI() == lower && upperCI() == upper && cl == .90 && numSamp == 165) {
          output$ciPropLowerIcon <- renderIcon(icon = "correct", width = 30)
          output$ciPropUpperIcon <- renderIcon(icon = "correct", width = 30)
          output$ciPropGuessFeedback <- renderText("Correct!")
        } else if (numRep > 100 && rep && numSamp == 165 && cl == .90 && (upperCI() != upper || lowerCI() != lower)) {
          output$ciPropLowerIcon <- renderIcon(icon = "incorrect", width = 30)
          output$ciPropUpperIcon <- renderIcon(icon = "incorrect", width = 30)
          output$ciPropGuessFeedback <- renderText("Make sure bounds match the ones below the graph")
        } else if (rep == FALSE || numSamp != 165) {
          output$ciPropLowerIcon <- renderIcon(icon = "incorrect", width = 30)
          output$ciPropUpperIcon <- renderIcon(icon = "incorrect", width = 30)
          output$ciPropGuessFeedback <- renderText("Think of conditions needed to be met in order to boostrap.")
        } else if (numRep < 100) {
          output$ciPropLowerIcon <- renderIcon(icon = "incorrect", width = 30)
          output$ciPropUpperIcon <- renderIcon(icon = "incorrect", width = 30)
          output$ciPropGuessFeedback <- renderText("Think of how many replications are needed to be representative.")
        } else if (cl != .90) {
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
        if (selectedOption == "Possible outcome 1-6 on each die") {
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
        if (selectedOption == "Sum of Nick's 5 dice rolls and Jennifer'6 dice rolls") {
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
        if (selectedOption == "Whether Nick has a higher total than Jennifer") {
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
    
    ### Feedback
    observeEvent(
      input$probSubmit,
      handlerExpr = {
        pop <- input$probPop
        samp <- input$probSamp
        event <- input$probEvent
        replace <- input$probReplace
        
        if (pop != "Possible outcome 1-6 on each die" || samp != "Sum of Nick's 5 dice rolls and Jennifer'6 dice rolls" ||
            event != "Whether Nick has a higher total than Jennifer" || replace != "With replacement") {
          output$probCompFeed <- renderText(
            "Remember that population relates to a whole, sample relates to a small 
            section of the population, and an event is is a specific outcome to analyze")
        } else {
          output$probCompFeed <- renderText("Correct!")
        }  
      }
    )
    ### Feedback
    observeEvent(
      eventExpr = input$probReset,
      handlerExpr = {
        output$probPopIcon <- renderIcon()
        output$probStatIcon <- renderIcon()
        output$probEventIcon <- renderIcon()
        output$probReplaceIcon <- renderIcon()
      }
    )
    
    
    ### Simulation ----
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
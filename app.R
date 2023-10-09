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
        menuItem("Hypothesis Test", tabName = "hypTest", icon = icon("wpexplorer")),
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
          p("APP DESC."),
          h2("Instructions"),
          tags$ol(
            tags$li("Review the prereqisities as needed."),
            tags$li("Next, go to the explore page and go through each tab."),
            tags$li("For each tab, read the context, identify components of the 
                    context, and view possible simulations.")
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
            div(class = "updated", "Last Update: 06/26/2023 by T.M.")
          )
        ),
        ### Prerequisites ----
        tabItem(
          tabName = "prerequisites",
          withMathJax(),
          box(
            title = strong("Vocabulary"),
            status = "primary",
            collapsible = TRUE,
            collapsed = TRUE,
            width = "100%",
            tags$ul(
              tags$li("Population: The entire group that we want information about. "),
              tags$li("Population Parameter(s): A measurement that describes 
                      something about the population. "),
              tags$li("Sample: The part of the population that we are actually examining"),
              tags$li("Statistic(s): A measurement that describes something about 
                      the sample"),
              tags$li("Bootrap: Type of resampling with replacement. You take samples 
                      from the original sample using the same sample size."),
              tags$li("Confidence Interval: The range that you expect the true 
                      population parameter to lie between with a certain level of
                      confidence. ")
            )
          ),
          fluidRow(
            column(
              width = 6,
              box(
                title = strong("Simulations for Calculating Probailities"),
                status = "primary",
                collapsible = TRUE,
                collapsed = TRUE,
                width = "100%",          
                tags$ul(
                  tags$li("An easy way to help test probabilities is by using simulations."),
                  tags$li("You can use these simulaitons to create probability tables
                          to see the liklihood of real life outcomes."),
                  tags$li("Some simulation examples are coin flipping, card drawing, 
                          spinners, etc. ")
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
                  tags$li("Bootrapping can be useful to 
                          get estimates of standard error, confidence intervals, 
                          and other statistics. It is very heplful when the 
                          sample is small or not normally distributed"),
                  tags$li("When bootstrappong you want use the mean of the 
                          original sample and standard error from the bootstrapped 
                          sample.")
                )
              )
            )
          )
        ),
        ### Confidence Interval for Mean ----
        tabItem(
          tabName = "ciMean",
          h4("1. Context"),
          fluidRow(
            column(
              width = 9,
              p("Every year thousands of airplanes takeoff and land at international
                airports. 6 international airports from the northeast region of the
                USA were samples. It would found that the mean number of planes 
                that landed and took off at this airport was 22,253.162, the median 
                of 20,600. Create a 90% confidence interval 
                for the numbers of flights departing/arriving at northeast airports
                from Q4 of the fiscal year. "),
            ),
            column(
              width = 3,
              tags$figure(
                tags$img(
                  src = "die.jpg",
                  width = "50%",
                  alt = "HOV lane street sign"
                )
              )
            )
          ),
          fluidRow(
            column( 
              width = 2,
              numericInput(
                inputId = 'ciMeanLower',
                label = 'Lower Bound: ',
                value = 0.0
              )
            ),
            column(width = 1, uiOutput(outputId = "ciMeanLowerIcon")),
            column(
              width = 2, 
              numericInput(
                inputId = 'ciMeanUpper',
                label = 'Upper Bound: ',
                value = 0.0
              )
            ),
            column(width = 1, uiOutput(outputId = "ciMeanUpperIcon")),
          ),
          bsButton(
            inputId = "ciMeanGuessSubmit",
            label = 'Submit'
          ),
          br(),
          tabsetPanel(
            id = "simulationType",
            ##### Identifying Components ----
            tabPanel(
              title = "2. Identifying Components",
              value = "b",
              wellPanel(
                fluidRow(
                  column(
                    width = 2, 
                    selectInput(
                      inputId = "ciMeanPop",
                      label = "Population",
                      choices = c( " ", "Airports in the Northeast region", 
                                   "Number of flights arriving/departing", 
                                   "Number of international airports", 
                                   "International airports in the Northeast region")
                    )
                  ),
                  column(width = 1, uiOutput(outputId = "ciMeanPopIcon")), 
                  column(
                    width = 2, 
                    selectInput(
                      inputId = "ciMeanPara",
                      label = "Population Parameter",
                      choices = c(" ", "Number of international airports", 
                                  "Airports in the Northeast region",
                                  "Number of flights arriving/departing", 
                                  "International airports in the Northeast region")
                    )
                  ),
                  column(width = 1, uiOutput(outputId = "ciMeanParaIcon")),
                  column(
                    width = 2, 
                    selectInput(
                      inputId = "ciMeanSamp",
                      label = "Sample",
                      choices = c(" ", "22253.162", "20600", "6 International Airports",
                                  "13 States in Northeast Region")
                    )
                  ),
                  column(width = 1, uiOutput(outputId = "ciMeanSampIcon")),
                  column(
                    width = 2, 
                    selectInput(
                      inputId = "ciMeanStat",
                      label = "Sample Statistic",
                      choices = c( " ", "13 States in Northeast Region", "20600", 
                                   "6 International Airports", "22253.162")
                    )
                  ),
                  column(width = 1, uiOutput(outputId = "ciMeanStatIcon"))
                ),
                fluidRow(
                  column(
                    offset = 9,
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
              title = "3. Simulation",
              value = "b",
              fluidRow(
                column(
                  width = 4,
                  wellPanel(
                    sliderInput(
                      inputId = "ciMeanSM",
                      label = "Sample Mean",
                      min = 20000, 
                      max = 25000,
                      value = 22500
                    ),
                    sliderInput(
                      inputId = "ciMeanNumSamp",
                      label = "Number of samples",
                      min = 1, 
                      max = 5000,
                      value = 2500
                    ),
                    numericInput(
                      inputId = "ciMeanCL",
                      label = 'Confidence Level',
                      value = 0.95
                    ),
                    bsButton(
                      inputId = "simCIMean",
                      label = "Simulate"
                    )
                  )
                ),
                column(
                  width = 8,
                  plotOutput("ciMeanSim")
                )
              )
            )
          )
        ),
        ### CI for Prop ----
        tabItem(
          tabName = "ciProp",
          h4("1. Context"),
          fluidRow(
            column(
              width = 9, 
              p("Researchers were curious about the retention rate of students with 
                Science, Technology, Engineering, and Mathematic (STEM) majors.
                They gathered data on 6 R1 College Instiutions. Of the students 
                they recorded information on 5.92% transferred into STEM, 15.58% 
                transferred out of STEM, 46.62% stayed in a nonSTEM major,
                and 31.88% stayed within a STEM major by the time of graduation. 
                Create a 95% confidence interval of the proportion of students 
                switching into STEM or keeping a STEM degree. ")
            ),
            column(
              width = 3,
              tags$figure(
                tags$img(
                  src = "die.jpg",
                  width = "50%",
                  alt = "HOV lane street sign"
                )
              )
            )
          ),
          fluidRow(
            column( 
              width = 2,
              numericInput(
                inputId = 'ciPropLower',
                label = 'Lower Bound: ',
                value = 0.0
              )
            ),
            column(width = 1, uiOutput(outputId = "ciPropLowerIcon")),
            column(
              width = 2, 
              numericInput(
                inputId = 'ciPropUpper',
                label = 'Upper Bound: ',
                value = 0.0
              )
            ),
            column(width = 1, uiOutput(outputId = "ciPropUpperIcon")),
          ),
          bsButton(
            inputId = "ciPropGuessSubmit",
            label = 'Submit'
          ),
          br(),
          tabsetPanel(
            id = "simulationType",
            ##### Identifying Components ----
            tabPanel(
              title = "2. Identifying Components",
              value = "b",
              wellPanel(
                fluidRow(
                  column(
                    width = 2, 
                    selectInput(
                      inputId = "ciPropPop",
                      label = "Population",
                      choices = c(" ", "All Research Institutions",
                                  "Students who switch into/stay within STEM", 
                                  "R1 Institutions", "Students who stayed in STEM")
                    )
                  ),
                  column(width = 1, uiOutput(outputId = "ciPropPopIcon")),
                  column(
                    width = 2, 
                    selectInput(
                      inputId = "ciPropPara",
                      label = "Population Parameter",
                      choices = c(" ", "Students who stayed in STEM",
                                  "Students who switch into/stay within STEM",
                                  "R1 Institutions", "All Research Institutions")
                    )
                  ),
                  column(width = 1, uiOutput(outputId = "ciPropParaIcon")),
                  column(
                    width = 2, 
                    selectInput(
                      inputId = "ciPropSamp",
                      label = "Sample",
                      choices = c(" ", "Number of STEM majors", "6 R1 Institutions", 
                                  "37.80%", "46.62%")
                    )
                  ),
                  column(width = 1, uiOutput(outputId = "ciPropSampIcon")),
                  column(
                    width = 2, 
                    selectInput(
                      inputId = "ciPropStat",
                      label = "Sample Statistic",
                      choices = c(" ", "37.80%", "46.62%", "Number of STEM majors",
                                  "6 R1 Institutions")
                    )
                  ),
                  column(width = 1, uiOutput(outputId = "ciPropStatIcon")),
                ),
                fluidRow(
                  column(
                    offset = 9,
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
              title = "3. Simulation",
              value = "b",
              fluidRow(
                column(
                  width = 4,
                  wellPanel(
                    sliderInput(
                      inputId = "ciPropSM",
                      label = "Sample Mean",
                      min = 0, 
                      max = 1,
                      value = .5
                    ),
                    sliderInput(
                      inputId = "ciMeanPropSamp",
                      label = "Number of samples",
                      min = 1, 
                      max = 5000,
                      value = 2500
                    ),
                    numericInput(
                      inputId = "ciPropCL",
                      label = 'Confidence Level',
                      value = 0.95
                    ),
                    bsButton(
                      inputId = "simCIPop",
                      label = "Simulate"
                    )
                  )
                ),
                column(
                  width = 8,
                  plotOutput("ciPopSim")
                )
              )
            )
          )
        ),      
        ### Hypothesis Test ----
        tabItem(
          tabName = "hypTest",
          h4("1. Context"),
          fluidRow(
            column(
              width = 9,
              p("CONTEXT")
            ),
            column(
              width = 3,
              tags$figure(
                tags$img(
                  src = "die.jpg",
                  width = "50%",
                  alt = "HOV lane street sign"
                )
              )
            )
          ),
          tabsetPanel(
            id = "simulationType",
            ##### Identifying Components ----
            tabPanel(
              title = "2. Identifying Components",
              value = "b",
              wellPanel(
                fluidRow(
                  column(
                    width = 3, 
                    selectInput(
                      inputId = "ciMeanPop",
                      label = "Population",
                      choices = c(" ","Option 1","Option 2","Option 3","Option 4")
                    )
                  ),
                  column(
                    width = 3, 
                    selectInput(
                      inputId = "ciMeanPara",
                      label = "Population Parameter",
                      choices = c(" ","Option 1","Option 2","Option 3","Option 4")
                    )
                  ),
                  column(
                    width = 3, 
                    selectInput(
                      inputId = "ciMeanSamp",
                      label = "Sample",
                      choices = c(" ","Option 1","Option 2","Option 3","Option 4")
                    )
                  ),
                  column(
                    width = 3, 
                    selectInput(
                      inputId = "ciMeanStat",
                      label = "Sample Statistic",
                      choices = c(" ","Option 1","Option 2","Option 3","Option 4")
                    )
                  )
                ),
                fluidRow(
                  column(
                    offset = 9,
                    width = 1,
                    bsButton(
                      inputId = "ex2Reset",
                      label = "Reset"
                    )
                  ),
                  column(
                    width = 1,
                    bsButton(
                      inputId = "ex2Submit",
                      label = "Submit"
                    )
                  )
                )
              )
            ),
            ##### Simulation  ----
            tabPanel(
              title = "3. Simulation",
              value = "b",
              fluidRow(
                column(
                  width = 4,
                  wellPanel(
                    p("Simulation Options Here")
                  )
                ),
                column(
                  width = 8,
                  p("Simulation Output Here")
                )
              )
            )
          )
        ),
        
        ### Probability ----
        tabItem(
          tabName = "prob",
          h4("1. Context"),
          fluidRow(
            column(
              width = 9,
              p("Nick and Jennifer were rolling dice to see who could get
                  a higher total. Nick claims that he can get a higher total with
                  less die, but Jennifer does not believe him. So Nick rolls 
                  5 die  while Jennifer rolls 6 die at once. What is the probability 
                  that Nick gets a higher total than Jennifer?"),
            ),
            column(
              width = 3,
              tags$figure(
                tags$img(
                  src = "die.jpg",
                  width = "50%",
                  alt = "2 die rolling. "
                )
              )
            )
          ),
          fluidRow(
            column(
              width = 3, 
              numericInput(
                inputId = 'guessProb',
                label = 'Probability that Nick wins: ',
                value = 0.0
              )
            ),
            column(width = 1, uiOutput(outputId = "guessIconProb")),
            column(width = 6, textOutput(outputId = "guessProbFeedback"))
          ),
          fluidRow(
            column(
              width = 2,
              bsButton(
                inputId = "guessSubmitProb",
                label = 'Submit'
              )
            )
          ),
            #### Simulation ----
            h4("2. Simulation"),
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
        ),
        ### References Page ----
        tabItem(
          tabName = "references",
          withMathJax(),
          h2("References"),
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
      if (selectedOption == "20600") {
        output$ciMeanStatIcon <- renderIcon(icon = "correct", width = 30)
      } else {
        output$ciMeanStatIcon <- renderIcon(icon = "incorrect", width = 30)
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
    observeEvent(
    input$simCIMean, 
    handlerExpr = {
      sampleMean <- input$ciMeanSM
      numSamp <- input$ciMeanNumSamp
      cl <- input$ciMeanCL
      
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
        R = numSamp
      )
      
      bootCI <- boot::boot.ci(
        boot.out = bootOut,
        conf = cl,
        type = "perc"
      )
      
      lower <- (1 - cl)/2
      upper <- cl + lower 

      output$ciMeanSim <- renderPlot(
        expr = {
          ggplot() +
            geom_histogram(data = data.frame(x = bootOut$t), aes(x = x), bins = 15) +
            geom_vline(xintercept = quantile(bootOut$t, c(lower, upper)), color = "red", linetype = "dashed") +
            labs(x = "Bootstrap", y = "Frequency")
          
        }
      )
    })
  

  
  ## Confidence Interval for Proportion----
  observeEvent(
    input$ciPropSubmit,
    handlerExpr = {
      selectedOption <- input$ciPropPop
      if (selectedOption == "R1 Institutions") {
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
      if (selectedOption == "Students who switch into/stay within STEM") {
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
      if (selectedOption == "6 R1 Institutions") {
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
      if (selectedOption == "37.80%") {
        output$ciPropStatIcon <- renderIcon(icon = "correct", width = 30)
      } else {
        output$ciPropStatIcon <- renderIcon(icon = "incorrect", width = 30)
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

  
  ## Hypothesis Test ----
  ### Simulation ----
  
  ## Probability ----

  probability <- reactiveVal(0)
  
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
      
      
      # SET GUESS == PROBABILITY (REACTIVE VAL)
      if (trials > 100 && nickRolls == 5 && jennRolls == 6 && abs(probability() - guess) < 0.001) {
        output$guessIconProb <- renderIcon(icon = "correct", width = 30)
        output$guessProbFeedback <- renderText(" ")
      } else if (trials > 100 && nickRolls == 5 && jennRolls == 6) { 
        output$guessIconProb <- renderIcon(icon = "incorrect", width = 30)
        output$guessProbFeedback <- renderText("Look at the estimated probability below the chart")
      } else if (trials < 100 && nickRolls == 5 && jennRolls == 6) { 
        output$guessIconProb <- renderIcon(icon = "incorrect", width = 30)
        output$guessProbFeedback <- renderText("Keep in mind how many trials should be used")
      } else if ((nickRolls != 5 | jennRolls != 6) & trials > 100) {
        output$guessIconProb <- renderIcon(icon = "incorrect", width = 30)
        output$guessProbFeedback <- renderText("Double check the number of rolls for Nick and Jenn")
      } else {
        output$guessIconProb <- renderIcon(icon = "incorrect", width = 30)
        output$guessProbFeedback <- renderText("set the simulation options according to the context")
      }
    }
  )
}

# Boast App Call ----
boastUtils::boastApp(ui = ui, server = server)
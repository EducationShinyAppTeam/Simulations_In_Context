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
          fluidRow(
            column(
              width = 9,
              h4("1. Context"),
              p("Every year thousands of airplanes takeoff and land at international
                airports. 6 international airports from the northeast region of the
                USA were samples. It would found that the mean number of planes 
                that landed and took off at this airport was 22253.162, the median 
                of 20600 with sd of 11398.343. Create a 90% confidence interval 
                for the numbers of flights departing/arriving at northeast airports
                from Q4 of the fiscal year. ")
            ),
            column(
              width = 3,
              tags$figure(
                tags$img(
                  src = "hov2.jpg",
                  width = "100%",
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
                      min = 10000, 
                      max = 30000,
                      value = 15000
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
                      label = 'Confidence Level (between 1-99%',
                      value = 95
                    ),
                    bsButton(
                      inputId = "simCIMean",
                      label = "Simulate"
                    )
                  )
                ),
                column(
                  width = 8,
                  br(),
                  br(),
                  plotOutput("ciMeanSim")
                )
              )
            )
          )
        ),
        ### CI for Prop ----
        tabItem(
          tabName = "ciProp",
          fluidRow(
            column(
              width = 9,
              h4("1. Context"),
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
                  src = "hov2.jpg",
                  width = "100%",
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
                    p("Simulation Options Here")
                  )
                ),
                column(
                  width = 8,
                  br(),
                  br(),
                  p("Simulation Output Here")
                )
              )
            )
          )
        ),      
        ### Hypothesis Test ----
        tabItem(
          tabName = "hypTest",
          fluidRow(
            column(
              width = 9,
              h4("1. Context"),
              p("CONTEXT")
            ),
            column(
              width = 3,
              tags$figure(
                tags$img(
                  src = "hov2.jpg",
                  width = "100%",
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
                  br(),
                  br(),
                  p("Simulation Output Here")
                )
              )
            )
          )
        ),
        
        ### Probability ----
        tabItem(
          tabName = "prob",
          tabsetPanel(
            id = "simulationType",
            fluidRow(
              column(
                width = 9,
                h4("1. Context"),
                p("Nick and Jennifer were rolling dice to see who could get
                  a higher total. Nick claims that he can get a higher total with
                  less dice rolls, but Jennifer does not believe him. So Nick rolls 
                  5 die  while Jennifer rolls 6 at once. What is the probability 
                  that Nick gets a higher total than Jennifer?"),
                numericInput(
                  inputId = 'guessProb',
                  label = 'Probability that Nick wins: ',
                  value = 0.0
                ),
                fluidRow(
                  column(
                    width = 2,
                    bsButton(
                      inputId = "guessSubmitProb",
                      label = 'Submit'
                    )
                  ),
                  column(width = 1, uiOutput(outputId = "guessIconProb")
                  )
                )
              ),
              column(
                width = 3,
                tags$figure(
                  tags$img(
                    src = "die.jpg",
                    width = "100%",
                    alt = "2 die rolling. "
                  )
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
                br(),
                br(),
                plotOutput('probSim'),
                textOutput('resultProb')
              )
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
  
  ### Simulation ----
  
  
  ## Confidence Interval for Population----
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
  
  

  ### Simulation ----
  
  ## Hypothesis Test ----
  ### Simulation ----
  
  ## Probability ----
  
  observeEvent(
    input$guessSubmitProb,
    handlerExpr = {
      guess <- input$guessProb
      if (0.22 <= guess && guess <= 0.26) {
        output$guessIconProb <- renderIcon(icon = "correct", width = 30)
      } else {
        output$guessIconProb <- renderIcon(icon = "incorrect", width = 30)
      }
    }
  )
  
  observeEvent(
    input$simProb, 
    handlerExpr = {
      trials <- input$trialsProb
      nickRolls <- input$nickRollsProb
      jennRolls <- input$jennRollsProb
      
      nickWin <- 0
      tie <- 0 
      jennWin <- 0
      total_scores <- numeric(trials)
      
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
        total_scores[i] <- nickSum - jennSum
      }
      
      probability <- nickWin / trials
      
      output$resultProb <- renderText({
        paste("Estimated probability that Nick gets a higher total:", probability, "\n")
      })
      
      output$probSim <- renderPlot({
        result_df <- data.frame(
          Outcome = c("Nick Wins", "Jennifer Wins", "Tie"),
          Frequency = c(nickWin, jennWin, tie))
        barplot(result_df$Frequency, names.arg = result_df$Outcome,
                main = "Distribution of Outcomes",
                xlab = "Outcome", 
                ylab = "Frequency", 
                col = "blue", 
                ylim = c(0, max(result_df$Frequency) * 1.1))
      })
    })
}

# Boast App Call ----
boastUtils::boastApp(ui = ui, server = server)
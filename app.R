# Load Packages ----
library(shiny)
library(shinydashboard)
library(shinyBS)
library(shinyWidgets)
library(boastUtils)
library(ggplot2)
library(boot)

# Load additional dependencies and setup functions
source("shuffleChoices.R")

# Datasets/Choices ----
meanCompChoices <-  c(
  "All hotel guests that use the dispenser represented by the amount of ice they
  would take",
  "The amount of ice taken by each of the ten guests",
  "3.1 ounces",
  "Ten values drawn with replacement from the list {5, 3, 0, 6, 0, 0, 4, 7, 0, and 6}",
  "The average amount of ice that would be taken by all hotel guests using the dispenser"
)
propCompChoices <- c(
  "All U.S. Burger Kings with drive thru represented by a “1” if a mistake would
  be made and a “0” if the order would be handled correctly",
  "The proportion of 1's in the population out of 6500 values",
  "16/165", "165 values sampled from a list of sixteen 1's and 149 0's",
  "Sixteen 1's and 149 0's"
)
probCompChoices <- c(
  "Possible outcomes 1, 2, 3, 4, 5, or 6 when a die is rolled",
  "Results for each of eleven draws from the population",
  "Whether the sum of the first five draws is larger than the sum of the next six
  draws from the population",
  "Possible sums for eleven rolls"
)
replaceChoices <- c(
  "Select an answer",
  "With replacement",
  "Without replacement"
)

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
            div(class = "updated", "Last Update: 5/29/2024 by T.M.")
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
              tags$li("Population: The entire group that we want information about."),
              tags$li("Population Parameter: A number that describes
                      something about the population."),
              tags$li("Sample: The part of the population that we are actually examining."),
              tags$li("Statistic: A number that describes something about
                      the sample."),
              #tags$li("Statistic: A function that measures an aspect about a sample."),
              tags$li("Confidence Interval: The interval/set of values for the
                      population parameter that are compatible with the test
                      statistic with a certain level of confidence."),
              tags$li("Event: A specific outcome or a set of outcomes that we are
                      interested in observing or analyzing.")
            )
          ),
          box(
            title = strong("Bootstrapping"),
            status = "primary",
            collapsible = TRUE,
            collapsed = TRUE,
            width = "100%",
            tags$ul(
              tags$li("(Nonparametric) Bootstrapping is a type of resampling with replacement
                      where you take samples from the original sample using the
                      same sample size."),
              tags$li("Bootstrapping can be useful to estimate standard errors
                      and confidence intervals for sample statistics in situations
                      where common formulas don't apply."),
              tags$li("Calculate the statistic for each bootstrap sample and
                      look at the variability across bootstrap samples to estimate
                      the variability around the original sample statistic."),
              tags$li("Use as many replications of bootstrap samples as practical.")
            )
          ),
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
          ),
        ),
        ### CI for Mean ----
        tabItem(
          tabName = "ciMean",
          h2("Confidence Interval for Mean"),
          p("Use the following context to walk through three steps for using
            simulation to find a potential answer. Steps 1 and 2 are tabs with
            Step 3 located at the bottom of the page."),
          h3("Context"),
          p("A hotel has put a self-serve water dispenser in its lobby and wants
            to know the average amount of ice that will be used by guests who get
            water from the machine. They look at the ice used by ten guests and
            find the amount taken for these ten as {5, 3, 0, 6, 0, 0, 4, 7, 0,
            and 6} which has an average of 3.1 ounces. We would like to use the
            bootstrap method to make a 98% confidence interval for the mean
            amount of ice taken by all guests using the dispenser."
          ),
          tabsetPanel(
            id = "simulationType",
            #### Identifying Components ----
            tabPanel(
              title = "Step 1: Identify Components",
              value = "b",
              br(),
              wellPanel(
                fluidRow(
                  column(
                    width = 5,
                    selectInput(
                      inputId = "ciMeanPop",
                      label = "Population",
                      choices = shuffleChoices(meanCompChoices)
                    )
                  ),
                  column(width = 1, uiOutput(outputId = "ciMeanPopIcon")),
                  column(
                    width = 5,
                    selectInput(
                      inputId = "ciMeanPara",
                      label = "Population parameter",
                      choices = shuffleChoices(meanCompChoices)
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
                      choices = shuffleChoices(meanCompChoices)
                    )
                  ),
                  column(width = 1, uiOutput(outputId = "ciMeanSampIcon")),
                  column(
                    width = 5,
                    selectInput(
                      inputId = "ciMeanStat",
                      label = "Sample statistic",
                      choices = shuffleChoices(meanCompChoices)
                    )
                  ),
                  column(width = 1, uiOutput(outputId = "ciMeanStatIcon"))
                ),
                fluidRow(
                  column(
                    width = 5,
                    selectInput(
                      inputId = "ciMeanSampMeth",
                      label = "Sampling method",
                      choices = replaceChoices
                    )
                  ),
                  column(width = 1, uiOutput(outputId = "ciMeanSampMethIcon")),
                  column(
                    width = 5,
                    selectInput(
                      inputId = "ciMeanBs",
                      label = "Boostrap sampling method",
                      choices = replaceChoices
                    )
                  ),
                  column(width = 1, uiOutput(outputId = "ciMeanBsIcon"))
                ),
                fluidRow(
                  column(
                    width = 5,
                    selectInput(
                      inputId = "ciMeanBsRep",
                      label = "Boostrap sample replicates",
                      choices = shuffleChoices(meanCompChoices)
                    )
                  ),
                  column(width = 1, uiOutput(outputId = "ciMeanBsRepIcon")),
                  column(
                    offset = 1,
                    width = 1,
                    bsButton(
                      inputId = "ciMeanReset",
                      label = "Reset",
                      size = "large",
                      style = "warning",
                      icon = icon("eraser")
                    )
                  ),
                  column(
                    width = 1,
                    offset = 1,
                    bsButton(
                      inputId = "ciMeanSubmit",
                      label = "Submit",
                      size = "large"
                    )
                  )
                ),
                fluidRow(
                  column(
                    width = 12,
                    textOutput(outputId = "ciMeanCompFeed")
                  )
                )
              )
            ),
            #### Simulation  ----
            tabPanel(
              title = "Step 2: Simulation",
              value = "b",
              br(),
              p("Set the appropriate values to carry out the bootstrap method."),
              fluidRow(
                column(
                  width = 4,
                  wellPanel(
                    sliderInput(
                      inputId = "ciMeanNS",
                      label = "Size of original sample",
                      min = 0,
                      max = 50,
                      value = 25,
                      step = 5
                    ),
                    sliderTextInput(
                      inputId = "ciMeanNumRep",
                      label = "Number of bootstrap replications",
                      choices = c(10, 100, 1000, 10000),
                      selected = "10",
                      grid = TRUE
                    ),
                    numericInput(
                      inputId = "ciMeanCL",
                      label = 'Confidence level',
                      value = 0.95,
                      step = 0.01,
                      min = 0,
                      max = 1
                    ),
                    bsButton(
                      inputId = "simCIMean",
                      label = "Simulate",
                      size = "large"
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
          h4("Step 3: Potential Answer"),
          fluidRow(
            column(
              width = 2,
              numericInput(
                inputId = 'ciMeanLower',
                label = 'Lower bound',
                value = NULL,
                min = 0,
                max = 10000
              )
            ),
            column(width = 1, uiOutput(outputId = "ciMeanLowerIcon")),
            column(
              width = 2,
              numericInput(
                inputId = 'ciMeanUpper',
                label = 'Upper bound',
                value = NULL,
                min = 0,
                max = 10000
              )
            ),
            column(width = 1, uiOutput(outputId = "ciMeanUpperIcon"))
          ),
          fluidRow(
            column(
              width = 2,
              bsButton(
                inputId = "ciMeanGuessSubmit",
                label = 'Submit',
                size = "large"
              )
            ),
            column(width = 10, textOutput(outputId = "ciMeanGuessFeedback"))
          )
        ),
        ### CI for Prop ----
        tabItem(
          tabName = "ciProp",
          h2("Confidence Interval for Proportion"),
          p("Use the following context to walk through three steps for using
            simulation to find a potential answer. Steps 1 and 2 are tabs with
            Step 3 located at the bottom of the page."),
          h3("Context"),
          p("Each year QSR magazine does a survey of drive-thru service at
            America’s quick serve restaurants. The magazine selects a number of
            restaurants from each of the major chains and sends a customer to
            order a meal from the drive thru window. The order will include an
            entree, a side dish, and a drink, and one deviation from the usual
            order. For example, at Burger King they might order an Impossible
            Burger, a medium Coke, and an onion rings but ask for no sauce on the
            Impossible Burger. QSR asks the customer to rate their experience in
            different ways, including how much time it took for them to be served
            and whether the restaurant got the order correct or not. In the 2023
            QSR survey, 165 out of Burger King’s 6500 restaurants with drive-thru
            windows were selected and it turned out that there was a mistake in
            the order at 16 of the sampled restaurants. We would like to use the
            bootstrap method to make a 90% confidence interval for the proportion
            of all Burger King Drive Thru windows that would make a mistake in an
            order of this type."
          ),
          tabsetPanel(
            id = "simulationType",
            #### Identifying Components ----
            tabPanel(
              title = "Step 1: Identify Components",
              value = "b",
              br(),
              wellPanel(
                fluidRow(
                  column(
                    width = 5,
                    selectInput(
                      inputId = "ciPropPop",
                      label = "Population",
                      choices = shuffleChoices(propCompChoices)
                    )
                  ),
                  column(width = 1, uiOutput(outputId = "ciPropPopIcon")),
                  column(
                    width = 5,
                    selectInput(
                      inputId = "ciPropPara",
                      label = "Population parameter",
                      choices = shuffleChoices(propCompChoices)
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
                      choices = shuffleChoices(propCompChoices)
                    )
                  ),
                  column(width = 1, uiOutput(outputId = "ciPropSampIcon")),
                  column(
                    width = 5,
                    selectInput(
                      inputId = "ciPropStat",
                      label = "Sample statistic",
                      choices = shuffleChoices(propCompChoices)
                    )
                  ),
                  column(width = 1, uiOutput(outputId = "ciPropStatIcon"))
                ),
                fluidRow(
                  column(
                    width = 5,
                    selectInput(
                      inputId = "ciPropSampMeth",
                      label = "Sampling method",
                      choices = replaceChoices
                    )
                  ),
                  column(width = 1, uiOutput(outputId = "ciPropSampMethIcon")),
                  column(
                    width = 5,
                    selectInput(
                      inputId = "ciPropBs",
                      label = "Boostrap sampling method",
                      choices = replaceChoices
                    )
                  ),
                  column(width = 1, uiOutput(outputId = "ciPropBsIcon"))
                ),
                fluidRow(
                  column(
                    width = 5,
                    selectInput(
                      inputId = "ciPropBsRep",
                      label = "Boostrap sample replicates",
                      choices = shuffleChoices(propCompChoices)
                    )
                  ),
                  column(width = 1, uiOutput(outputId = "ciPropBsRepIcon")),
                  column(
                    offset = 1,
                    width = 1,
                    bsButton(
                      inputId = "ciPropReset",
                      label = "Reset",
                      size = "large",
                      style = "warning",
                      icon = icon("eraser")
                    )
                  ),
                  column(
                    width = 1,
                    offset = 1,
                    bsButton(
                      inputId = "ciPropSubmit",
                      label = "Submit",
                      size = "large"
                    )
                  )
                ),
                fluidRow(
                  column(
                    width = 12,
                    textOutput(outputId = "ciPropCompFeed")
                  )
                )
              )
            ),
            #### Simulation  ----
            tabPanel(
              title = "Step 2: Simulation",
              value = "b",
              br(),
              p("Set the appropriate values to carry out the bootstrap method."),
              fluidRow(
                column(
                  width = 4,
                  wellPanel(
                    sliderInput(
                      inputId = "ciPropNS",
                      label = "Size of original sample",
                      min = 100,
                      max = 200,
                      value = 150,
                      step = 5
                    ),
                    sliderTextInput(
                      inputId = "ciPropNumRep",
                      label = "Number of bootstrap replications",
                      choices = c(10, 100, 1000, 10000),
                      selected = "10",
                      grid = TRUE
                    ),
                    numericInput(
                      inputId = "ciPropCL",
                      label = 'Confidence level',
                      value = 0.95,
                      step = 0.01,
                      min = 0,
                      max = 1
                    ),
                    bsButton(
                      inputId = "simCIProp",
                      label = "Simulate",
                      size = "large"
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
          h4("Step 3: Potential Answer"),
          fluidRow(
            column(
              width = 2,
              numericInput(
                inputId = 'ciPropLower',
                label = 'Lower bound',
                value = NULL,
                step = 0.01,
                min = 0,
                max = 1
              )
            ),
            column(width = 1, uiOutput(outputId = "ciPropLowerIcon")),
            column(
              width = 2,
              numericInput(
                inputId = 'ciPropUpper',
                label = 'Upper bound',
                value = NULL,
                step = 0.01,
                min = 0,
                max = 1
              )
            ),
            column(width = 1, uiOutput(outputId = "ciPropUpperIcon"))
          ),
          fluidRow(
            column(
              width = 2,
              bsButton(
                inputId = "ciPropGuessSubmit",
                label = 'Submit',
                size = "large"
              )
            ),
            column(width = 10, textOutput(outputId = "ciPropGuessFeedback"))
          )
        ),
        ### Probability ----
        tabItem(
          tabName = "prob",
          h2("Estimating Probability"),
          p("Use the following context to walk through three steps for using
            simulation to find a potential answer. Steps 1 and 2 are tabs with
            Step 3 located at the bottom of the page."),
          h3("Context"),
          p("Nick and Jennifer were rolling dice to see who could get a higher
            total. Nick claims that he can get a higher total with fewer dice,
            but Jennifer does not believe him. So Nick rolls a die five times and
            then Jennifer rolls a die six times. Use simulation to find the
            probability that Nick gets a higher total than Jennifer."
          ),
          tabsetPanel(
            id = "simulationType",
            #### Identifying Components ----
            tabPanel(
              title = "Step 1: Identify Components",
              value = "b",
              br(),
              wellPanel(
                fluidRow(
                  column(
                    width = 5,
                    selectInput(
                      inputId = "probPop",
                      label = "Population",
                      choices = shuffleChoices(probCompChoices)
                    )
                  ),
                  column(width = 1, uiOutput(outputId = "probPopIcon")),
                  column(
                    width = 5,
                    selectInput(
                      inputId = "probSamp",
                      label = "Sample",
                      choices = shuffleChoices(probCompChoices)
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
                      choices = shuffleChoices(probCompChoices)
                    )
                  ),
                  column(width = 1, uiOutput(outputId = "probEventIcon")),
                  column(
                    width = 5,
                    selectInput(
                      inputId = "probReplace",
                      label = "Sampling method",
                      choices = c(
                        "Select an answer",
                        "Draw with replacement",
                        "Draw without replacement"
                      )
                    )
                  ),
                  column(width = 1, uiOutput(outputId = "probReplaceIcon"))
                ),
                fluidRow(
                  column(
                    width = 8,
                    textOutput(outputId = "probCompFeed")
                  ),
                  column(
                    width = 1,
                    bsButton(
                      inputId = "probReset",
                      label = "Reset",
                      size = "large",
                      style = "warning",
                      icon = icon("eraser")
                    )
                  ),
                  column(
                    width = 1,
                    offset = 1,
                    bsButton(
                      inputId = "probSubmit",
                      label = "Submit",
                      size = "large"
                    )
                  )
                )
              )
            ),
            #### Simulation  ----
            tabPanel(
              title = "Step 2: Simulation",
              value = "b",
              br(),
              p("Set the appropriate values to conduct the simulation."),
              fluidRow(
                column(
                  width = 4,
                  wellPanel(
                    sliderTextInput(
                      inputId = 'simsProb',
                      label = "Number of simulations",
                      choices = c(10, 100, 1000, 10000),
                      selected = "10",
                      grid = TRUE
                    ),
                    sliderInput(
                      inputId = 'nickRollsProb',
                      label = "Nick's number of rolls",
                      min = 1,
                      max = 10,
                      value = 1
                    ),
                    sliderInput(
                      inputId = 'jennRollsProb',
                      label = "Jennifer's number of rolls",
                      min = 1,
                      max = 10,
                      value = 1
                    ),
                    bsButton(
                      inputId = 'simProb',
                      label = 'Simulate',
                      size = "large"
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
          h4("Step 3: Potential Answer"),
          fluidRow(
            column(
              width = 5,
              numericInput(
                inputId = 'guessProb',
                label = 'Estimated probability that Nick wins',
                value = NULL,
                step = 0.01,
                min = 0,
                max = 1
              )
            ),
            column(width = 1, uiOutput(outputId = "guessIconProb"))
          ),
          fluidRow(
            column(
              width = 2,
              bsButton(
                inputId = "guessSubmitProb",
                label = 'Submit',
                size = "large"
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
            "Klein, Danny. “The 2023 QSR® Drive-Thru Report.” QSR Magazine, 27 Oct.
            2023, www.qsrmagazine.com/reports/the-2023-qsr-drive-thru-report/. "
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
      if (input$pages %in% c("ciMean", "ciProb", "hypTest", "prob")) {
        message <- "Click through each tab to view different type of simulations.
          Read the context, identify components, and practice  each simulation
          type."
      } else {
        message <- "View the prerequisties as needed. Once ready, go through each
          simulation left on the tab options, identify components and practice
          different types of simulations."
      }
      sendSweetAlert(
        session = session,
        type = "info",
        title = "Information",
        text = message
      )
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
      if (selectedOption == "All hotel guests that use the dispenser represented by the amount of ice they would take") {
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
      if (selectedOption == "The average amount of ice that would be taken by all hotel guests using the dispenser") {
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
      if (selectedOption == "The amount of ice taken by each of the ten guests") {
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
      if (selectedOption == "3.1 ounces") {
        output$ciMeanStatIcon <- renderIcon(icon = "correct", width = 30)
      } else {
        output$ciMeanStatIcon <- renderIcon(icon = "incorrect", width = 30)
      }
    }
  )
  observeEvent(
    input$ciMeanSubmit,
    handlerExpr = {
      selectedOption <- input$ciMeanBsRep
      if (selectedOption == "Ten values drawn with replacement from the list {5, 3, 0, 6, 0, 0, 4, 7, 0, and 6}") {
        output$ciMeanBsRepIcon <- renderIcon(icon = "correct", width = 30)
      } else {
        output$ciMeanBsRepIcon <- renderIcon(icon = "incorrect", width = 30)
      }
    }
  )
  observeEvent(
    input$ciMeanSubmit,
    handlerExpr = {
      selectedOption <- input$ciMeanBs
      if (selectedOption == "With replacement") {
        output$ciMeanBsIcon <- renderIcon(icon = "correct", width = 30)
      } else {
        output$ciMeanBsIcon <- renderIcon(icon = "incorrect", width = 30)
      }
    }
  )
  observeEvent(
    input$ciMeanSubmit,
    handlerExpr = {
      selectedOption <- input$ciMeanSampMeth
      if (selectedOption == "Without replacement") {
        output$ciMeanSampMethIcon <- renderIcon(icon = "correct", width = 30)
      } else {
        output$ciMeanSampMethIcon <- renderIcon(icon = "incorrect", width = 30)
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

      if (pop != "All hotel guests that use the dispenser represented by the amount of ice they would take" ||
          para != "The average amount of ice that would be taken by all hotel guests using the dispenser" ||
          samp != "The amount of ice taken by each of the ten guests" || stat != "3.1 ounces") {
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
      output$ciMeanBsRepIcon <- renderIcon()
      output$ciMeanBsIcon <- renderIcon()
      output$ciMeanSampMethIcon <- renderIcon()
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

      meanData <- c(5, 3, 0, 6, 0, 0, 4, 7, 0, 6)

      stat <- function(data, index, num_samples) {
        subset_data <- data[index]
        subset_data <- subset_data[sample(1:length(subset_data), num_samples, replace = TRUE)]

        statistic_value <- mean(subset_data)
        return(statistic_value)
      }

      set.seed(461)

      bootOut <- boot::boot(
        data = meanData,
        statistic = function(data, index) stat(data, index, numSamp),
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
            labs(x = "Bootstrap Mean", y = "Frequency") +
            theme_bw() +
            theme(
              text = element_text(size = 20)
            ) +
            scale_y_continuous(
              expand = expansion(mult = 0, add = c(0, 2))
            )
        }
      )
      output$ciMeanResults <- renderText({
        paste("Lower Confidence Interval:", round(cii[1], 2), "\n",
              "Upper Confidence Interval:", round(cii[2], 2))
      })
    })

  #### Guessing Feedback
  observeEvent(
    input$ciMeanGuessSubmit,
    handlerExpr = {
      numSamp <- input$ciMeanNS
      numRep <- input$ciMeanNumRep
      cl <- input$ciMeanCL

      upper <- input$ciMeanUpper
      lower <- input$ciMeanLower

      if (numRep >= 100 && lowerCI() == lower && upperCI() == upper && cl == .98 && numSamp == 10) {
        output$ciMeanLowerIcon <- renderIcon(icon = "correct", width = 30)
        output$ciMeanUpperIcon <- renderIcon(icon = "correct", width = 30)
        output$ciMeanGuessFeedback <- renderText("Correct!")
      } else if (numRep > 100 && numSamp == 10 && cl == .98 && (upperCI() != upper || lowerCI() != lower)) {
        output$ciMeanLowerIcon <- renderIcon(icon = "incorrect", width = 30)
        output$ciMeanUpperIcon <- renderIcon(icon = "incorrect", width = 30)
        output$ciMeanGuessFeedback <- renderText("Make sure bounds match the output below the graph")
      } else if (numSamp != 10) {
        output$ciMeanLowerIcon <- renderIcon(icon = "incorrect", width = 30)
        output$ciMeanUpperIcon <- renderIcon(icon = "incorrect", width = 30)
        output$ciMeanGuessFeedback <- renderText("Think of conditions needed to be met in order to boostrap such as replacement and sample size in relation to the original size.")
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
      if (selectedOption == "All U.S. Burger Kings with drive thru represented by a “1” if a mistake would be made and a “0” if the order would be handled correctly") {
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
      if (selectedOption == "The proportion of 1's in the population out of 6500 values") {
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
      if (selectedOption == "Sixteen 1's and 149 0's") {
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
  observeEvent(
    input$ciPropSubmit,
    handlerExpr = {
      selectedOption <- input$ciPropBsRep
      if (selectedOption == "165 values sampled from a list of sixteen 1's and 149 0's") {
        output$ciPropBsRepIcon <- renderIcon(icon = "correct", width = 30)
      } else {
        output$ciPropBsRepIcon <- renderIcon(icon = "incorrect", width = 30)
      }
    }
  )

  observeEvent(
    input$ciPropSubmit,
    handlerExpr = {
      selectedOption <- input$ciPropBs
      if (selectedOption == "With replacement") {
        output$ciPropBsIcon <- renderIcon(icon = "correct", width = 30)
      } else {
        output$ciPropBsIcon <- renderIcon(icon = "incorrect", width = 30)
      }
    }
  )
  observeEvent(
    input$ciPropSubmit,
    handlerExpr = {
      selectedOption <- input$ciPropSampMeth
      if (selectedOption == "Without replacement") {
        output$ciPropSampMethIcon <- renderIcon(icon = "correct", width = 30)
      } else {
        output$ciPropSampMethIcon <- renderIcon(icon = "incorrect", width = 30)
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
      bsRep <- input$ciPropBsRep
      bs <- input$ciPropBs
      method <- input$ciPropSampMeth

      if (pop != "All U.S. Burger Kings with drive thru represented by a “1” if a mistake would be made and a “0” if the order would be handled correctly" ||
          para != "The proportion of 1's in the population out of 6500 values" ||
          samp != "Sixteen 1's and 149 0's" || stat != "16/165" ||
          bsRep != "165 values sampled from a list of sixteen 1's and 149 0's") {
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
      output$ciPropBsRepIcon <- renderIcon()
      output$ciPropBsIcon <- renderIcon()
      output$ciPropSampMethIcon <- renderIcon()
    }
  )

  ### Simulation ----
  upperCI <- reactiveVal(0)
  lowerCI <- reactiveVal(0)

  observeEvent(
    input$simCIProp,
    handlerExpr = {
      numSamp <- input$ciPropNS
      numRep <- input$ciPropNumRep
      cl <- input$ciPropCL

      propData <- data.frame(Success = c(
        rep(1, 16),
        rep(0, 149)),
        Total = rep(1, 165)
      )

      stat <- function(data, index, num_samples) {
        subset_data <- data[index, , drop = FALSE]
        subset_data <- subset_data[sample(1:nrow(subset_data), num_samples, replace = TRUE), ]

        successes <- sum(subset_data$Success)
        total <- sum(subset_data$Total)
        proportion_value <- successes / total
        return(proportion_value)
      }

      set.seed(461)

      bootOut <- boot::boot(
        data = propData,
        statistic = function(data, index) stat(data, index, numSamp),
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
          labs(x = "Bootstrap Proportion", y = "Frequency")  +
          theme_bw() +
          theme(
            text = element_text(size = 20)
          ) +
          scale_y_continuous(
            expand = expansion(mult = 0, add = c(0, 2))
          )
      })
      output$ciPropResults <- renderText({
        paste("Lower Confidence Interval:", round(cii[1], 2), "\n",
              "Upper Confidence Interval:", round(cii[2], 2))
      })
    })

    #### Guessing Feedback
    observeEvent(
      input$ciPropGuessSubmit,
      handlerExpr = {
        numSamp <- input$ciPropNS
        numRep <- input$ciPropNumRep
        cl <- input$ciPropCL

        upper <- input$ciPropUpper
        lower <- input$ciPropLower

        if (numRep >= 100 && lowerCI() == lower && upperCI() == upper && cl == .90 && numSamp == 165) {
          output$ciPropLowerIcon <- renderIcon(icon = "correct", width = 30)
          output$ciPropUpperIcon <- renderIcon(icon = "correct", width = 30)
          output$ciPropGuessFeedback <- renderText("Correct!")
        } else if (numRep >= 100 && numSamp == 165 && cl == .90 && (upperCI() != upper || lowerCI() != lower)) {
          output$ciPropLowerIcon <- renderIcon(icon = "incorrect", width = 30)
          output$ciPropUpperIcon <- renderIcon(icon = "incorrect", width = 30)
          output$ciPropGuessFeedback <- renderText("Make sure bounds match the output below the graph")
        } else if (numSamp != 165) {
          output$ciPropLowerIcon <- renderIcon(icon = "incorrect", width = 30)
          output$ciPropUpperIcon <- renderIcon(icon = "incorrect", width = 30)
          output$ciPropGuessFeedback <- renderText("Think of conditions needed to be met in order to boostrap such as replacement and sample size in relation to the original size.")
        } else if (numRep == 10) {
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
        if (selectedOption == "Possible outcomes 1, 2, 3, 4, 5, or 6 when a die is rolled") {
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
        if (selectedOption == "Results for each of eleven draws from the population") {
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
        if (selectedOption == "Whether the sum of the first five draws is larger than the sum of the next six draws from the population") {
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
        if (selectedOption == "Draw with replacement") {
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

        if (pop != "Possible outcomes 1, 2, 3, 4, 5, or 6 when a die is rolled" ||
            samp != "Results for each of eleven draws from the population" ||
            event != "Whether the sum of the first five draws is larger than the sum of the next six draws from the population" ||
            replace != "Draw with replacement") {
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
      eventExpr = input$simProb,
      handlerExpr = {
        trials <- input$simsProb
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
          paste("Estimated probability that Nick gets a higher total:", round(probability(),2), "\n")
        })

        results <- data.frame(
          outcome = c("Nick Wins", "Jennifer Wins", "Tie"),
          frequency = c(nickWin, jennWin, tie)
        )

        output$probSim <- renderPlot(
          expr = {
            ggplot(
              data = results,
              mapping = aes(x = outcome, y = frequency)
            ) +
              geom_col(fill = boastPalette[1]) +
              labs(x = "Outcome", y = "Frequency") +
              theme_bw() +
              theme(
                text = element_text(size = 20)
              ) +
              scale_y_continuous(
                expand = expansion(mult = 0, add = c(0, 2))
              )
          },
          alt = "coming soon"
        )
      })

  observeEvent(
    input$guessSubmitProb,
    handlerExpr = {
      guess <- input$guessProb
      trials <- input$simsProb
      nickRolls <- input$nickRollsProb
      jennRolls <- input$jennRollsProb

      #### Guessing Feedback
      if (trials >= 100 && nickRolls == 5 && jennRolls == 6 && abs(probability() - guess) < 0.001) {
        output$guessIconProb <- renderIcon(icon = "correct", width = 30)
        output$guessProbFeedback <- renderText("Correct!")
      } else if (trials >= 100 && nickRolls == 5 && jennRolls == 6) {
        output$guessIconProb <- renderIcon(icon = "incorrect", width = 30)
        output$guessProbFeedback <- renderText("Look at the estimated probability below the chart")
      } else if (trials == 10 && nickRolls == 5 && jennRolls == 6) {
        output$guessIconProb <- renderIcon(icon = "incorrect", width = 30)
        output$guessProbFeedback <- renderText("Keep in mind how many replications there should be")
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
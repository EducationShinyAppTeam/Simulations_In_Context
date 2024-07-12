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

TOL1 <- 0.1
TOL2 <- 0.01

bootMean <- function(x, index) {
  return(mean(x[index], na.rm = TRUE))
}
meanData <- c(5, 3, 0, 6, 0, 0, 4, 7, 0, 6)
propData <- rep(c(1,0), times = c(16, 149))

diceGame <- function(p1Name, p1Times, p2Name, p2Times) {
  p1Results <- sum(sample(x = 1:6, size = p1Times, replace = TRUE))
  p2Results <- sum(sample(x = 1:6, size = p2Times, replace = TRUE))

  if (p1Results == p2Results) {return("Tie")}
  else if (p1Results > p2Results) {return(paste(p1Name, "Wins"))}
  else {return(paste(p2Name, "Wins"))}
}

# Choices ----
## NOTE: line breaks in these choices will cause scoring issues
meanCompChoices <-  c(
  "All hotel guests that use the dispenser represented by the amount of ice they would take",
  "The amount of ice taken by each of the ten guests",
  "3.1 ounces",
  "Ten values drawn with replacement from the list {5, 3, 0, 6, 0, 0, 4, 7, 0, and 6}",
  "The average amount of ice that would be taken by all hotel guests using the dispenser"
)
propCompChoices <- c(
  "All U.S. Burger Kings with drive thru represented by a “1” if a mistake would be made and a “0” if the order would be handled correctly",
  "The proportion of 1's in the population out of 6500 values",
  "16/165", "165 values sampled from a list of sixteen 1's and 149 0's",
  "Sixteen 1's and 149 0's"
)
probCompChoices <- c(
  "Possible outcomes 1, 2, 3, 4, 5, or 6 when a die is rolled",
  "Results for each of eleven draws from the population",
  "Whether the sum of the first five draws is larger than the sum of the next six draws from the population",
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
              icon = icon("bolt")
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
            div(class = "updated", "Last Update: 7/2/2024 by NJH.")
          )
        ),
        ### Prerequisites ----
        tabItem(
          tabName = "prerequisites",
          withMathJax(),
          h2("Prerequisites"),
          p("Take a moment to review core concepts for this app."),
          box(
            title = strong("Vocabulary"),
            status = "primary",
            collapsible = TRUE,
            collapsed = FALSE,
            width = "100%",
            tags$ul(
              tags$li(tags$strong("Population:"), "The entire group that we want
                      information about."),
              tags$li(tags$strong("Population Parameter:"), "A number that describes
                      something about the population."),
              tags$li(tags$strong("Sample:"), "The part of the population that
                      we are actually examining."),
              tags$li(tags$strong("Statistic:"), "A function that measures an
                      aspect about a sample by reporting a single value."),
              tags$li(tags$strong("Confidence Interval:"), "The interval/set of
                      values for the population parameter that are compatible
                      with the test statistic with a certain level of confidence."),
              tags$li(tags$strong("Event:"), "A specific outcome or a set of
                      outcomes that we are interested in observing or analyzing."),
              tags$li(tags$strong("Simulation:"), "The process of creating and
                      employing a model (either physically or via a computer) that
                      mimics a real-world phenonomon you want to learn something
                      more about.")
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
                uiOutput(outputId = "ciMeanCompFeed")
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
                  uiOutput('ciMeanResults')
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
                    uiOutput(outputId = "ciPropCompFeed")
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
                  uiOutput('ciPropResults')
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
                    uiOutput(outputId = "probCompFeed")
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
                      inputId = 'nickRolls',
                      label = "Nick's number of rolls",
                      min = 1,
                      max = 10,
                      value = 1
                    ),
                    sliderInput(
                      inputId = 'jennRolls',
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
                  uiOutput('resultProb')
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
            "Bailey, E. (2022). shinyBS: Twitter bootstrap components for shiny.
            (v 0.61.1). [R package]. Available from
            https://CRAN.R-project.org/package=shinyBS"
          ),
          p(
            class = "hangingindent",
            "Canty, A. and Ripley, B. (2024). boot: Boostrap R (S-Plus) functions.
            (v 1.3-30). [R package]. Avaliable from
            https://CRAN.R-project.org/package=boot"
          ),
          p(
            class = "hangingindent",
            "Carey, R. and Hatfield, N. J. (2024). boastUtils: BOAST utlities.
            (v 0.1.12.2). [R package]. Available from
            https://github.com/EducationShinyAppTeam/boastUtils"
          ),
          p(
            class = "hangingindent",
            "Chang, W. and Borges Ribeio (2021). shinydashboard: Create dashboards
            with 'Shiny.' (v0.7.2). Avaliable from
            https://CRAN.R-project.org/package=shinydashboard"
          ),
          p(
            class = "hangingindent",
            "Chang, W., Cheng J., Allaire, J., Sievert, C., Schloerke, B.,
            Xie, Y., Allen, J., McPherson, J., Dipert, A., and Borges, B.
            (2024). shiny: Web application framework for R. (v 1.8.1.1).
            [R package]. Available from
            https://CRAN.R-project.org/package=shiny"
          ),
          p(
            class = "hangingindent",
            "Klein, D. “The 2023 QSR® Drive-Thru Report.” QSR Magazine, 27 Oct.
            2023, www.qsrmagazine.com/reports/the-2023-qsr-drive-thru-report/. "
          ),
          p(
            class = "hangingindent",
            "Perrier, V., Meyer, F., and Granjon, D. (2024). shinyWidgets: Custom
            inputs widgets for shiny. (v 0.8.6). [R package]. Available from
            https://CRAN.R-project.org/package=shinyWidgets"
          ),
          p(
            class = "hangingindent",
            "Wickham, H. (2016). ggplot2: Elegant Graphics for Data Analysis.
            Springer-Verlag New York. (v 3.5.1). [R package]. Avaliable from
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

  ## CI Mean ----
  ### Step 1: Submit ----
  observeEvent(
    eventExpr = input$ciMeanSubmit,
    handlerExpr = {
      #### Check for user mistakes ----
      pop <- input$ciMeanPop == "All hotel guests that use the dispenser represented by the amount of ice they would take"
      para <- input$ciMeanPara == "The average amount of ice that would be taken by all hotel guests using the dispenser"
      samp <- input$ciMeanSamp == "The amount of ice taken by each of the ten guests"
      stat <- input$ciMeanStat == "3.1 ounces"
      rep <- input$ciMeanBsRep == "Ten values drawn with replacement from the list {5, 3, 0, 6, 0, 0, 4, 7, 0, and 6}"
      bsMeth <- input$ciMeanBs == "With replacement"
      sampMeth <- input$ciMeanSampMeth == "Without replacement"

      #### Component Icons ----
      output$ciMeanPopIcon <- renderIcon(
        icon = ifelse(test = pop, yes = "correct", no = "incorrect"),
        width = 30
      )
      output$ciMeanParaIcon <- renderIcon(
        icon = ifelse(test = para, yes = "correct", no = "incorrect"),
        width = 30
      )
      output$ciMeanSampIcon <- renderIcon(
        icon = ifelse(test = samp, yes = "correct", no = "incorrect"),
        width = 30
      )
      output$ciMeanStatIcon <- renderIcon(
        icon = ifelse(test = stat, yes = "correct", no = "incorrect"),
        width = 30
      )
      output$ciMeanBsRepIcon <- renderIcon(
        icon = ifelse(test = rep, yes = "correct", no = "incorrect"),
        width = 30
      )
      output$ciMeanBsIcon <- renderIcon(
        icon = ifelse(test = bsMeth, yes = "correct", no = "incorrect"),
        width = 30
      )
      output$ciMeanSampMethIcon <- renderIcon(
        icon = ifelse(test = sampMeth, yes = "correct", no = "incorrect"),
        width = 30
      )

      #### Component Feedback ----
      if (!pop || !para || !samp || !stat) {
        feedback <- "Remember that population relates to a whole and sample relates to a small section of the population."
      } else if (!sampMeth || !bsMeth || !rep) {
        feedback <- "Think carefully through the process of selecting the original ten guests and then simulating."
      } else {
        feedback <- "All Correct!"
      }

      output$ciMeanCompFeed <- renderUI(
        expr = {
          p(tags$strong("Feedback on your choices:"), feedback)
        }
      )
    }
  )

  ### Step 1: Reset ----
  observeEvent(
    eventExpr = c(input$ciMeanReset, input$ciMeanPop, input$ciMeanPara,
                  input$ciMeanSamp, input$ciMeanStat, input$ciMeanSampMeth,
                  input$ciMeanBs, input$ciMeanBsRep),
    handlerExpr = {
      output$ciMeanPopIcon <- renderIcon()
      output$ciMeanParaIcon <- renderIcon()
      output$ciMeanSampIcon <- renderIcon()
      output$ciMeanStatIcon <- renderIcon()
      output$ciMeanBsRepIcon <- renderIcon()
      output$ciMeanBsIcon <- renderIcon()
      output$ciMeanSampMethIcon <- renderIcon()
      output$ciMeanCompFeed <- renderUI({NULL})
    }
  )

  ### Step 2: Simulation ----
  meanCI <- reactiveVal(NULL)

    observeEvent(
      eventExpr = input$simCIMean,
      handlerExpr = {
        #### Sample size error check ----
        if (input$ciMeanNS != 10) {
          sendSweetAlert(
            session = session,
            title = "Wrong Sample Size",
            type = "error",
            text = "The sample size you entered doesn't match that of the context.
            Please re-read the context and adjust the sample size."
          )
        } else {
          #### Bootstrap ----
          bootOut <- boot::boot(
            data = meanData,
            statistic = bootMean,
            R = input$ciMeanNumRep
          )
          bootCI <- boot::boot.ci(
            boot.out = bootOut,
            conf = input$ciMeanCL,
            type = "perc"
          )

          meanCI(bootCI$percent[4:5])

          #### Plot ----
          output$ciMeanSim <- renderPlot(
            expr = {
              ggplot(
                data = data.frame(means = bootOut$t),
                mapping = aes(x = means)
              ) +
                geom_histogram(
                  bins = 15,
                  fill = boastPalette[1],
                  color = "black"
                ) +
                geom_vline(
                  xintercept = bootCI$percent[4:5],
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
            },
            alt = "Coming soon"
          )
          #### Result statement ----
          output$ciMeanResults <- renderUI(
            expr = {
              tagList(
                p(tags$strong("Lower Confidence Interval:"),
                  round(bootCI$percent[4], digits = 2)),
                p(tags$strong("Upper Confidence Interval:"),
                  round(bootCI$percent[5], digits = 2)),
              )
            }
          )
        }
      }
    )

    ### Step 3: Feedback ----
    observeEvent(
      eventExpr = input$ciMeanGuessSubmit,
      handlerExpr = {
        lowerError <- abs(input$ciMeanLower - meanCI()[1])
        upperError <- abs(input$ciMeanUpper - meanCI()[2])

        if (lowerError <= TOL1 && upperError <= TOL1 &&
            input$ciMeanCL == 0.98 && input$ciMeanNumRep >= 100) {
          output$ciMeanLowerIcon <- renderIcon(icon = "correct", width = 30)
          output$ciMeanUpperIcon <- renderIcon(icon = "correct", width = 30)
          output$ciMeanGuessFeedback <- renderText({"Correct!"})
        } else if (!(lowerError <= TOL1 && upperError <= TOL1) &&
                   input$ciMeanCL == 0.98 && input$ciMeanNumRep >= 100) {
          output$ciMeanLowerIcon <- renderIcon(icon = "incorrect", width = 30)
          output$ciMeanUpperIcon <- renderIcon(icon = "incorrect", width = 30)
          output$ciMeanGuessFeedback <- renderText({"Make sure bounds match the output below the graph."})
        } else if (input$ciMeanCL != 0.98) {
          output$ciMeanLowerIcon <- renderIcon(icon = "partial", width = 30)
          output$ciMeanUpperIcon <- renderIcon(icon = "partial", width = 30)
          output$ciMeanGuessFeedback <- renderText({"Reread the context and verify inputs match."})
        } else if (input$ciMeanNumRep < 100) {
          output$ciMeanLowerIcon <- renderIcon(icon = "partial", width = 30)
          output$ciMeanUpperIcon <- renderIcon(icon = "partial", width = 30)
          output$ciMeanGuessFeedback <- renderText({"Think of how many replications are needed to be representative."})
        } else {
          output$ciMeanLowerIcon <- renderIcon(icon = "incorrect", width = 30)
          output$ciMeanUpperIcon <- renderIcon(icon = "incorrect", width = 30)
          output$ciMeanGuessFeedback <- renderText({"Incorrect, reread the context and make adjustments to inputs. Reference the prerequsities as needed."})
        }
      }
    )

    ### Reset Step 3 ----
    observeEvent(
      eventExpr = c(input$ciMeanLower, input$ciMeanUpper),
      handlerExpr = {
        output$ciMeanLowerIcon <- renderIcon()
        output$ciMeanUpperIcon <- renderIcon()
        output$ciMeanGuessFeedback <- renderText({NULL})
      }
    )

    ## CI Proportion----
    ### Step 1: Submit ----
    observeEvent(
      eventExpr = input$ciPropSubmit,
      handlerExpr = {
        #### Check for user mistakes ----
        pop <- input$ciPropPop == "All U.S. Burger Kings with drive thru represented by a “1” if a mistake would be made and a “0” if the order would be handled correctly"
        para <- input$ciPropPara == "The proportion of 1's in the population out of 6500 values"
        samp <- input$ciPropSamp == "Sixteen 1's and 149 0's"
        stat <- input$ciPropStat == "16/165"
        sampMeth <- input$ciPropSampMeth == "Without replacement"
        bsMeth <- input$ciPropBs == "With replacement"
        rep <- input$ciPropBsRep == "165 values sampled from a list of sixteen 1's and 149 0's"

        #### Component Icons ----
        output$ciPropPopIcon <- renderIcon(
          icon = ifelse(test = pop, yes = "correct", no = "incorrect"),
          width = 30
        )
        output$ciPropParaIcon <- renderIcon(
          icon = ifelse(test = para, yes = "correct", no = "incorrect"),
          width = 30
        )
        output$ciPropSampIcon <- renderIcon(
          icon = ifelse(test = samp, yes = "correct", no = "incorrect"),
          width = 30
        )
        output$ciPropStatIcon <- renderIcon(
          icon = ifelse(test = stat, yes = "correct", no = "incorrect"),
          width = 30
        )
        output$ciPropBsRepIcon <- renderIcon(
          icon = ifelse(test = rep, yes = "correct", no = "incorrect"),
          width = 30
        )
        output$ciPropBsIcon <- renderIcon(
          icon = ifelse(test = bsMeth, yes = "correct", no = "incorrect"),
          width = 30
        )
        output$ciPropSampMethIcon <- renderIcon(
          icon = ifelse(test = sampMeth, yes = "correct", no = "incorrect"),
          width = 30
        )

        #### Component Feedback ----
        if (!pop || !para || !samp || !stat) {
         feedback <- "Remember that population relates to a whole and sample relates to a small section of the population."
        } else if (!sampMeth || !bsMeth || !rep) {
          feedback <- "Think carefully through the process of selecting the original 165 restaurants and then simulating."
        } else {
          feedback <- "All Correct!"
        }

        output$ciPropCompFeed <- renderUI(
          expr = {
            p(tags$strong("Feedback on your choices:"), feedback)
          }
        )
      }
    )

    ### Step 1: Reset ----
    observeEvent(
      eventExpr = c(input$ciPropReset, input$ciPropPop, input$ciPropPara,
                    input$ciPropSamp, input$ciPropStat, input$ciPropSampMeth,
                    input$ciPropBs, input$ciPropBsRep),
      handlerExpr = {
        output$ciPropPopIcon <- renderIcon()
        output$ciPropParaIcon <- renderIcon()
        output$ciPropSampIcon <- renderIcon()
        output$ciPropStatIcon <- renderIcon()
        output$ciPropBsRepIcon <- renderIcon()
        output$ciPropBsIcon <- renderIcon()
        output$ciPropSampMethIcon <- renderIcon()
        output$ciPropCompFeed <- renderUI({NULL})
      }
    )

    ### Step 2: Simulation ----
    propCI <- reactiveVal(NULL)

    observeEvent(
      eventExpr = input$simCIProp,
      handlerExpr = {
        if (input$ciPropNS != 165) {
          sendSweetAlert(
            session = session,
            title = "Wrong Sample Size",
            type = "error",
            text = "The sample size you entered doesn't match that of the context.
                   Please re-read the context and adjust the sample size."
          )
        } else {
          #### Bootstrap ----
          bootOut <- boot::boot(
            data = propData,
            statistic = bootMean,
            R = input$ciPropNumRep
          )
          bootCI <- boot::boot.ci(
            boot.out = bootOut,
            conf = input$ciPropCL,
            type = "perc"
          )

          propCI(bootCI$percent[4:5])

          #### Plot ----
          output$ciPropSim <- renderPlot(
            expr = {
              ggplot(
                data = data.frame(means = bootOut$t),
                mapping = aes(x = means)
              ) +
                geom_histogram(
                  bins = 15,
                  fill = boastPalette[1],
                  color = "black"
                ) +
                geom_vline(
                  xintercept = bootCI$percent[4:5],
                  color = "red",
                  linetype = "dashed"
                ) +
                labs(x = "Bootstrap Proportion", y = "Frequency") +
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
          #### Result statement ----
          output$ciPropResults <- renderUI(
            expr = {
              tagList(
                p(tags$strong("Lower Confidence Interval:"),
                  round(bootCI$percent[4], digits = 2)),
                p(tags$strong("Upper Confidence Interval:"),
                  round(bootCI$percent[5], digits = 2))
              )
            }
          )
        }
      }
    )

    ### Step 3: Feedback ----
    observeEvent(
      eventExpr = input$ciPropGuessSubmit,
      handlerExpr = {
        numSamp <- input$ciPropNS
        numRep <- input$ciPropNumRep
        cl <- input$ciPropCL

        upper <- input$ciPropUpper
        lower <- input$ciPropLower

        lowerError <- abs(input$ciPropLower - propCI()[1])
        upperError <- abs(input$ciPropUpper - propCI()[2])

        if (lowerError <= TOL2 && upperError <= TOL2 &&
            input$ciPropCL == 0.9 && input$ciPropNumRep >= 100) {
          output$ciPropLowerIcon <- renderIcon(icon = "correct", width = 30)
          output$ciPropUpperIcon <- renderIcon(icon = "correct", width = 30)
          output$ciPropGuessFeedback <- renderText({"Correct!"})
        } else if (!(lowerError <= TOL2 && upperError <= TOL2) &&
                   input$ciPropCL == 0.9 && input$ciPropNumRep >= 100) {
          output$ciPropLowerIcon <- renderIcon(icon = "incorrect", width = 30)
          output$ciPropUpperIcon <- renderIcon(icon = "incorrect", width = 30)
          output$ciPropGuessFeedback <- renderText({"Make sure bounds match the output below the graph."})
        } else if (input$ciPropCL != 0.9) {
          output$ciPropLowerIcon <- renderIcon(icon = "partial", width = 30)
          output$ciPropUpperIcon <- renderIcon(icon = "partial", width = 30)
          output$ciPropGuessFeedback <- renderText({"Reread the context and verify inputs match."})
        } else if (input$ciPropNumRep < 100) {
          output$ciPropLowerIcon <- renderIcon(icon = "partial", width = 30)
          output$ciPropUpperIcon <- renderIcon(icon = "partial", width = 30)
          output$ciPropGuessFeedback <- renderText({"Think of how many replications are needed to be representative."})
        } else {
          output$ciPropLowerIcon <- renderIcon(icon = "incorrect", width = 30)
          output$ciPropUpperIcon <- renderIcon(icon = "incorrect", width = 30)
          output$ciPropGuessFeedback <- renderText({"Incorrect, reread the context and make adjustments to inputs. Reference the prerequsities as needed."})
        }
      }
    )

    ### Reset Step 3 ----
    observeEvent(
      eventExpr = c(input$ciPropLower, input$ciPropUpper),
      handlerExpr = {
        output$ciPropLowerIcon <- renderIcon()
        output$ciPropUpperIcon <- renderIcon()
        output$ciPropGuessFeedback <- renderText({NULL})
      }
    )

    ## Probability ----
    ### Step 1: Submit ----
    observeEvent(
      eventExpr = input$probSubmit,
      handlerExpr = {
        #### Check for user mistakes ----
        pop <- input$probPop == "Possible outcomes 1, 2, 3, 4, 5, or 6 when a die is rolled"
        samp <- input$probSamp == "Results for each of eleven draws from the population"
        event <- input$probEvent == "Whether the sum of the first five draws is larger than the sum of the next six draws from the population"
        sampMeth <- input$probReplace == "Draw with replacement"

        #### Component Icons ----
        output$probPopIcon <- renderIcon(
          icon = ifelse(test = pop, yes = "correct", no = "incorrect"),
          width = 30
        )
        output$probSampIcon <- renderIcon(
          icon = ifelse(test = samp, yes = "correct", no = "incorrect"),
          width = 30
        )
        output$probEventIcon <- renderIcon(
          icon = ifelse(test = event, yes = "correct", no = "incorrect"),
          width = 30
        )
        output$probReplaceIcon <- renderIcon(
          icon = ifelse(test = sampMeth, yes = "correct", no = "incorrect"),
          width = 30
        )

        #### Component Feedback ----
        if (pop && samp && event && sampMeth) {
          feedback <- "All Correct!"
        } else {
          feedback <- "Remember that population relates to a whole, sample relates to a small
            section of the population, and an event is is a specific outcome to analyze"
        }

        output$probCompFeed <- renderUI(
          expr = {
            p(tags$strong("Feedback on your choices:"), feedback)
          }
        )
      }
    )

    ### Step 1: Reset ----
    observeEvent(
      eventExpr = c(input$probReset, input$probPop, input$probSamp,
                    input$probEvent, input$probReplace),
      handlerExpr = {
        output$probPopIcon <- renderIcon()
        output$probSampIcon <- renderIcon()
        output$probEventIcon <- renderIcon()
        output$probReplaceIcon <- renderIcon()
        output$probCompFeed <- renderUI({NULL})
      }
    )

    ### Step 2: Simulation ----
    probability <- reactiveVal(NULL)
    observeEvent(
      eventExpr = input$simProb,
      handlerExpr = {
        simResults <- replicate(
          n = input$simsProb,
          expr = diceGame(
            p1Name = "Nick",
            p1Times = input$nickRolls,
            p2Name = "Jennifer",
            p2Times = input$jennRolls
          )
        )

        probability((table(simResults)/length(simResults))["Nick Wins"])

        #### Simulation plot ----
        output$probSim <- renderPlot(
          expr = {
            ggplot(
              data = data.frame(results = simResults),
              mapping = aes(x = results)
            ) +
              geom_bar(fill = boastPalette[1]) +
              labs(x = "Outcome", y = "Frequency") +
              theme_bw() +
              theme(
                text = element_text(size = 20)
              ) +
              scale_y_continuous(
                expand = expansion(mult = c(0, 0.02), add = 0)
              )
          },
          alt = "coming soon"
        )

        ### Result statement ----
        output$resultProb <- renderUI(
          expr = {
            p(tags$strong("Estimated probability that Nick gets a higher total:"),
              round(probability(), digits = 2))
          }
        )
      }
    )

    ### Step 3: Feedback ----
    observeEvent(
      eventExpr = input$guessSubmitProb,
      handlerExpr = {
        guess <- input$guessProb
        trials <- input$simsProb
        nickRolls <- input$nickRolls
        jennRolls <- input$jennRolls

        guessError <- abs(input$guessProb - probability())

        if (guessError <= TOL2 && input$nickRolls == 5 && input$jennRolls == 6 &&
            input$simsProb >= 100) {
          output$guessIconProb <- renderIcon(icon = "correct", width = 30)
          output$guessProbFeedback <- renderText({"Correct!"})
        } else if (!(input$nickRolls == 5 && input$jennRolls == 6)) {
          output$guessIconProb <- renderIcon(icon = "incorrect", width = 30)
          output$guessProbFeedback <- renderText({"Double check the number of rolls for Nick and Jennifer."})
        } else if (guessError > TOL2) {
          output$guessIconProb <- renderIcon(icon = "incorrect", width = 30)
          output$guessProbFeedback <- renderText({"Look at the estimated probability below the chart."})
        } else if (input$simProb < 100) {
          output$guessIconProb <- renderIcon(icon = "partial", width = 30)
          output$guessProbFeedback <- renderText({"Think about how many times you would need the simulation to run to convince you."})
        } else {
          output$guessIconProb <- renderIcon(icon = "incorrect", width = 30)
          output$guessProbFeedback <- renderText({"Set the simulation options according to the context."})
        }
      }
    )

    ### Reset Step 3 ----
    observeEvent(
      eventExpr = input$guessProb,
      handlerExpr = {
        output$guessIconProb <- renderIcon()
        output$guessProbFeedback <- renderText({NULL})
      }
    )
}

# Boast App Call ----
boastUtils::boastApp(ui = ui, server = server)
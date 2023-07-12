# Load Packages ----
library(shiny)
library(shinydashboard)
library(shinyBS)
library(shinyWidgets)
library(boastUtils)
library(ggplot2) 

# Load additional dependencies and setup functions
# source("global.R")

## Datasets ----

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
    ### Navigation menu ----
    dashboardSidebar(
      width = 250,
      sidebarMenu(
        id = "pages",
        menuItem("Overview", tabName = "overview", icon = icon("gauge-high")),
        menuItem("Prerequisites", tabName = "prerequisites", icon = icon("book")),
        menuItem("Explore", tabName = "explore", icon = icon("wpexplorer")),
        menuItem("References", tabName = "references", icon = icon("leanpub"))
      ),
      tags$div(
        class = "sidebar-logo",
        boastUtils::sidebarFooter()
      )
    ),
    ### Create the content ----
    dashboardBody(
      tabItems(
        #### Overview Page ----
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
          ##### Overview to Explore Button
          div(
            style = "text-align: center;",
            bsButton(
              inputId = "overviewToExplore",
              label = "Explore",
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
        #### Prerequisites ----
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
                      confidence. "),
              
              
            ),
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
              ),
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
                          sample."),
                )
              ),
            ),
          )
        ),
        ####  Explore ----
        tabItem(
          tabName = "explore",
          tabsetPanel(
            id = "simulationType",
            ##### Example One ----
            tabPanel(
              title = "Example 1",
              value = "b",
            ),
            ##### Example Two ----
            tabPanel(
              title = "Example 2",
              value = "b",
              fluidRow(
                column(
                  width = 9,
                wellPanel(
                  p("A large city is deciding on whether to add a HOV lane on weekdays
                    between 8 and 10 a.m. to a section of a four-lane highway with 
                    heavy traffic (HOV = High Occupancy Vehicles where only cars
                    with at least two passengers are allowed). The idea of a HOV 
                    lane is to encourage an increase in the number of passengers
                    per car in order to reduce air pollution and traffic. Before 
                    installing the HOV lane, the city decides to collect baseline 
                    data on the number of passengers per car so they will have 
                    information to compare with after the installation. Cameras
                    are set up along the road and photographs of 1000 cars are taken
                    on weekdays between 8 and 10 a.m.. It turned out that 740 cars
                    had only one person (the driver); 208 had two people; 43 had 
                    three people; 8 had four people and 1 had five people. The city
                    would like to make an 80% confidence interval for the average
                    number of people per car between 8 an 10 on weekdays. 
                    (CI #2?) They are also interested in creating a 
                    confidence interval for the percentage of cars that would be
                    eligible to use the HOV lane, which also 
                    might change after installing it.")
                )
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
              fluidRow(
                h4("Identifying Components"),
                wellPanel(
                  column(
                    width = 3, 
                    selectInput(
                      inputId = "ex2Pop",
                      label = "Population",
                      choices = c(" ","Option 1","Option 2","Option 3","Option 4")
                    )
                  ),
                  column(
                    width = 3, 
                    selectInput(
                      inputId = "ex2Para",
                      label = "Population Parameter",
                      choices = c(" ","Option 1","Option 2","Option 3","Option 4")
                    )
                  ),
                  column(
                    width = 3, 
                    selectInput(
                      inputId = "ex2Samp",
                      label = "Sample",
                      choices = c(" ","Option 1","Option 2","Option 3","Option 4")
                    )
                  ),
                  column(
                    width = 3, 
                    selectInput(
                      inputId = "ex2Stat",
                      label = "Sample Statistic",
                      choices = c(" ","Option 1","Option 2","Option 3","Option 4")
                    )
                  ),
                  fluidRow(
                    column(
                      width = 3, 
                      selectInput(
                        inputId = "ex2Sim",
                        label = "Simulation Type",
                        choices = c(" ","Option 1","Option 2","Option 3","Option 4")
                      )
                    ),
                    column(
                      offset = 7,
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
                ),
                fluidRow(
                  column(
                    width = 4,
                    h4("Simulation"),
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
            ),
            ##### Example Three  ----
            tabPanel(
              title = "Example 3",
              value = "c",
              
            ),
            ##### Example Four ----
            tabPanel(
              title = "Example 4",
              value = "D",
              
            ),
          )
        ),
        #### References Page ----
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
  
  ## Set up Info button ----
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
          go to the explore tab to identify compoents and practice different types of simulations."
        )
      } else if (tab == "explore") {
        sendSweetAlert(
          session = session,
          type = "info",
          title = "Information",
          text = "Click through each tab to view different type of simulations. 
          Read the context, identify components, and practice different simulation
          types."
        )
      } else {
        sendSweetAlert(
          session = session,
          type = "info",
          title = "Information",
          text = "View the prerequisties as needed and then go to the explore page
          to practice identifying context and different simulations."
        )
      }

    }
  )
  
  ## Overview to  Button----
  observeEvent(
    eventExpr = input$overviewPrereq,
    handlerExpr = {
      updateTabItems(
        session = session,
        inputId = "pages",
        selected = "prerequisites"
      )
    }
  )
}

# Boast App Call ----
boastUtils::boastApp(ui = ui, server = server)
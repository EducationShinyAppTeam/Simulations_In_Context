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
      title = "APP NAME",
      titleWidth = 250,
      tags$li(class = "dropdown", actionLink("info", icon("info"))),
      tags$li(
        class = "dropdown",
        boastUtils::surveyLink(name = "APP NAME")
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
          h1("APP NAME"), 
          p("APP DESC."),
          h2("Instructions"),
          tags$ol(
            tags$li("Review the prereqisities as needed."),
            tags$li("INSTRUCTIONS")
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
              tags$li("text"),
            ),
          ),
          box(
            title = strong("Simulations for Calculating Probailities"),
            status = "primary",
            collapsible = TRUE,
            collapsed = TRUE,
            width = "100%",          
            tags$ul(
              tags$li("WORDS"),
            )
          ),
          box(
            title = strong("Bootstrapping Confidence Intervals"),
            status = "primary",
            collapsible = TRUE,
            collapsed = TRUE,
            width = "100%",          
            tags$ul(
              tags$li("WORDS"),
            )
          )
        ),
        ####  Game ----
        tabItem(
          tabName = "game",
          withMathJax(),
          h3("GAME"),
          p("Read the context of each scenario and then correctly identify the components
            and plots that match the scenario."),
          hr(),
          fluidRow(
            wellPanel(
              p("CONTEXT HERE")
            )
          ),
          fluidRow(
            column(
              width = 6,
              h4("Identifying Components"),
              wellPanel(
                p("IDENTIFIER HERE")
              )
            ),
            column(
              width = 6,
              h4("Identifying Plots"),
              wellPanel(
                p("Simulation Output Here")
              )
            )
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
      sendSweetAlert(
        session = session,
        type = "info",
        title = "Information",
        text = "This app will help you learn more about Taryn McHugh"
      )
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
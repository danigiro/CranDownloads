library(shiny)
library(httr)
library(jsonlite)
library(shinythemes)
library(plotly)

# get the list of all packages on CRAN
package_names = names(httr::content(httr::GET("http://crandb.r-pkg.org/-/desc")))

fluidPage(
  theme = shinytheme("readable"),
  # Application title
  titlePanel("Package Downloads from R CRAN"),
  hr(),
  # Sidebar with a slider input for number of bins
  sidebarLayout(
    sidebarPanel(style = "background: white",
      wellPanel(HTML("Enter an (or multiple) R package(s) to see the number of downloads over time from the RStudio CRAN Mirror."),
                selectInput("package", 
                  label = "Packages:",
                  selected = sample(package_names, 2),
                  choices = package_names,
                  multiple = TRUE),   
      radioButtons("transformation", 
                   "Data Transformation:",
                   c("Daily" = "daily","Weekly" = "weekly", "Monthly" = "monthly", "Cumulative" = "cumulative"))),
      wellPanel(HTML("Created using the <a href='https://github.com/metacran/cranlogs'>cranlogs</a> package",
                     "and starting from the shiny app described <a href='http://varianceexplained.org/r/cran-view/'>here</a>.",
                     "This app is not affiliated with RStudio or CRAN.",
                     "You can find the code for the this app <a href='https://github.com/dgrtwo/cranview'>here</a>"))
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      tabsetPanel(
        tabPanel("Static", br(), plotOutput("downloadsPlot", width = "95%", height = 500)), 
        tabPanel("Plotly", br(), plotlyOutput("graph", width = "95%", height = 500))
      )
    )
  ),
)
library(shiny)
library(stringr)
library(dplyr)
library(ggplot2)
library(lubridate)
library(cranlogs)
library(zoo)
library(scales)
library(gridExtra)
library(plotly)
library(httr)

## Function to extract legend
g_legend <- function(a.gplot){ 
  tmp <- ggplot_gtable(ggplot_build(a.gplot)) 
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box") 
  legend <- tmp$grobs[[leg]] 
  legend
} 

shinyServer(function(input, output) {
  # get the list of all packages on CRAN
  package_names <- tools::CRAN_package_db()[, c("Package")]
  
  updateSelectizeInput(session = getDefaultReactiveDomain(), 
                       'package', 
                       selected = sample(package_names, 2),
                       choices = package_names,
                       server = TRUE)
  
  downloads <- reactive({
    packages <- input$package
    date_list = structure(c(sapply(packages, function(x){
      pkg_data = content(GET(paste0("http://crandb.r-pkg.org/", x, "/all")))$timeline[[1]]
      as.Date(pkg_data)}), Sys.Date() -1), class = "Date")
    
    cran_downloads0 <- purrr::possibly(cran_downloads, NULL, quiet = TRUE)
    d <- cran_downloads0(package = packages, 
                         from    = min(date_list), 
                         to      = Sys.Date()-1)
    if (input$transformation=="weekly") {
      d <- d %>%
        group_by(package, week(date), year(date)) %>%
        mutate(count=sum(count), date = max(date)) %>% 
        unique() %>% ungroup()
    } else if (input$transformation=="monthly") {
      d <- d %>%
        group_by(package, month(date), year(date)) %>%
        mutate(count=sum(count), date = max(date)) %>% 
        unique() %>% ungroup()
    } else if (input$transformation=="cumulative") {
      d <- d %>%
        group_by(package) %>%
        transmute(count=cumsum(count), date=date)  %>% 
        ungroup()
    }
    d
  })
  
  output$downloadsPlot <- renderPlot({
    if(is.null(input$package)){
      ggplot() +theme_minimal() 
    }else{
      d <- downloads()
      
      g1 <- ggplot(d[between(d$date, max(d$date) - years(1), max(d$date)),], 
                   aes(date, count, color = package)) + 
        geom_line() +
        xlab("")+
        ylab("")+
        labs(subtitle = "Last year") +
        theme_bw()+
        theme(legend.position = "none")
      g12 <- ggplot(d[between(d$date, max(d$date) - months(1), max(d$date)),], 
                    aes(date, count, color = package)) + 
        geom_line() +
        xlab("")+
        ylab("")+
        theme_bw()+
        labs(subtitle = "Last month") + 
        theme(legend.position = "none")
      
      g2 <- ggplot(d, aes(date, count, color = package)) + 
        geom_line() +
        xlab("")+
        ylab("")+
        labs(subtitle = "All time") +
        theme_bw() + 
        theme(legend.position = "bottom")
      
      grid.arrange(arrangeGrob(arrangeGrob(g1, g12, ncol = 2),
                               g2 + theme(legend.position = "none"), bottom = "Date", left = "Number of downloads"),
                   g_legend(g2), heights = c(2,0.25))
    }
  })
  
  output$graph <- renderPlotly({
    if(is.null(input$package)){
      plot_ly(type="scatter", mode = "markers+lines")
    }else{
      d <- downloads()
      plot_ly(d, x = ~date, y = ~count, color = ~package, 
              colors = hue_pal()(length(unique(d$package))), 
              type="scatter", mode = "markers+lines", size = I(2),
              line = list(shape = 'linear', width = 0.35, dash = 'dot')) %>%
        layout(
          xaxis = list(
            title = "Date",
            range=c(min(d$date),max(d$date)),
            constrain = "range",
            rangeselector = list(
              buttons = list(
                list(
                  count = 3,
                  label = "3 mo",
                  step = "month",
                  stepmode = "backward"),
                list(
                  count = 6,
                  label = "6 mo",
                  step = "month",
                  stepmode = "backward"),
                list(
                  count = 1,
                  label = "1 yr",
                  step = "year",
                  stepmode = "backward"),
                list(
                  count = 1,
                  label = "YTD",
                  step = "year",
                  stepmode = "todate"),
                list(
                  label = 'All',
                  method = "relayout",
                  args = list(list(xaxis = list(range = c(0,10)))))
              )),
            
            rangeslider = list(type = "date",
                               range=c(min(d$date),max(d$date)))),
          yaxis = list(title = "Number of downloads", fixedrange = FALSE))
    }
  })
})

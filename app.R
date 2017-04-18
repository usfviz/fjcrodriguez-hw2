#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(ggplot2)
library(plotly)

shiny.version.str <- packageVersion('shiny')
shiny.version <- unlist(shiny.version.str )
shiny.v <- shiny.version[1] + shiny.version[2] * 0.1 + shiny.version[3] * 0.01 

if (shiny.v < 1.01) {
  stop(paste0(c("Program requires Shiny version 4.5.6 or higher, you have ", paste0(shiny.version.str,sep="."),"\nRun 'install.packages('shiny')' in your R console to update your version\n\n\t- Franco", collapse="")))
}

plotly.version.str <- packageVersion('plotly')
plotly.version <- unlist(plotly.version.str )
plotly.v <- plotly.version[1] + plotly.version[2] * 0.1 + plotly.version[3] * 0.01 


if ("plotly" %in% installed.packages() == FALSE) {
  stop("plotly package must be installed to run app - Franco\nRun 'install.packages('plotly')' in your R console")
}



if (plotly.v < 4.56) {
  stop(paste0(c("Program requires plotly version 4.5.6 or higher, you have ", paste0(plotly.version.str,sep="."),"\nRun 'install.packages('plotly')' in your R console to update your version\n\n\t- Franco", collapse="")))
}

world.data <- read.csv("world_data.csv")
world.data <- world.data[world.data$region!="",]
world.data <- na.omit(world.data)


# Define UI for application that draws a histogram
ui <- fluidPage(
  fluidRow(
    column(6, offset=1, 
           plotlyOutput("distPlot", width = "130%")
    ),
    column(3, offset = 2,
           checkboxGroupInput("region", "Region",
                              choiceNames = list(HTML("<p><img src='http://i.imgur.com/CxkMk5b.png'/> East Asia & Pacific</p>"),
                                                 HTML("<p><img src='http://i.imgur.com/J22Lsv3.png'/> Europe & Central Asia</p>"),
                                                 HTML("<p><img src='http://i.imgur.com/mgnJIJG.png'/> Latin America & Caribbean</p>"),
                                                 HTML("<p><img src='http://i.imgur.com/DZq6hBA.png'/> Middle East & North Africa</p>"),
                                                 HTML("<p><img src='http://i.imgur.com/dNy79b5.png'/> North America</p>"),
                                                 HTML("<p><img src='http://i.imgur.com/GJ4YE6A.png'/> South Asia</p>"),
                                                 HTML("<p><img src='http://i.imgur.com/ChagPfk.png'/> Sub-Saharan Africa</p>")
                              ),
                              choiceValues = list(
                                "East Asia & Pacific",
                                "Europe & Central Asia",
                                "Latin America & Caribbean",
                                "Middle East & North Africa",
                                "North America",
                                "South Asia",
                                "Sub-Saharan Africa"
                              ),
                              selected =NULL )
    )
  ),# fluid row 1
  fluidRow(column(7, offset = 1,
                  sliderInput("year",
                              "Choose a year",
                              min = 1960,
                              max = 2014,
                              value = 1960,
                              animate = animationOptions(interval = 350),
                              sep = "", width = "100%")
  ),
  column(2, offset=0, sliderInput("population_size",
                                  "Population",
                                  min = 3,
                                  max = 40,
                                  value = 16,
                                  sep="",
                                  ticks = FALSE)
  )
  )
  
  
)

theme1 <- theme( panel.grid.major=element_line(color = "grey90", size = 0.5),
                 panel.background = element_rect(fill = "white",color = "grey50", size=0.5),
                 axis.title = element_text(size=16,face = 'bold')
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  output$distPlot <- renderPlotly({
    
    subset_data <- world.data[world.data$year==input$year,]
    subset_data["reduce_trans"] = subset_data$region %in% input$region == FALSE
    
    g <- ggplot(data=subset_data, aes(key=Country))
    g <- g + geom_point(aes(x=life_expectancy, y=fertility_rate, fill=region, size=population_size, alpha=reduce_trans), pch=21,stroke=.2, show.legend = FALSE)
    g <- g + scale_fill_manual(values = c("dodgerblue3", "orangered3", "orange", "forestgreen", "magenta4", 'deepskyblue3', "deeppink2"))
    g <- g + scale_size(range = c(3,input$population_size))
    g <- g + theme1
    g <- g + xlab("Life Expectancy") + ylab("Fertility Rate")
    g <- g + scale_x_continuous(breaks = seq(10,90,5))
    g <- g + scale_y_continuous(breaks = seq(1,9))
    g <- g + scale_alpha_manual(values = c(.8, 0.1))
    g <- g + coord_cartesian(xlim = c(10,90), ylim= c(0.5, 9))
    p <- ggplotly(g, tooltip=c('key')) 
    hide_guides(p)
    
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
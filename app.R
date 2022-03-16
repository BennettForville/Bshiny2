library(shiny)
library(hector)
library(ggplot2)

# Define UI for app that draws a histogram ----
ui <- fluidPage(
  
  # App title ----
  titlePanel("Hello Shiny!"),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(
      
      # Input: Slider for the number of bins ----
      sliderInput(inputId = "Q10",
                  label = "Q10 values:",
                  min = 1,
                  max = 5,
                  value = 2)
      
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(
      
      # Output: Histogram ----
      plotOutput(outputId = "distPlot")
      
    )
  )
)

# Define server logic required to draw a histogram ----
server <- function(input, output) {

  
  
  # Histogram of the Old Faithful Geyser Data ----
  # with requested number of bins
  # This expression that generates a histogram is wrapped in a call
  # to renderPlot to indicate that:
  #
  # 1. It is "reactive" and therefore should be automatically
  #    re-executed when inputs (input$bins) change
  # 2. Its output type is a plot
  output$distPlot <- renderPlot({
    
    RCP45 <- system.file("input/hector_ssp245.ini", package = "hector")
    core45 <- newcore(RCP45)
    run(core45)
    plot_data <- fetchvars(core45,2000:2200)
    
    # ggplot(plot_data, aes(year, value))+
    #   geom_point()+
    #   facet_wrap(~variable, scales = "free")+
    #   ggtitle(input$Q10)
  
    
   run_with_param <- function(core, parameter, value) {
       old_value <- fetchvars(core, NA, parameter)
       unit <- as.character(old_value[["units"]])
       setvar(core, NA, parameter, value, unit)
       reset(core)
       run(core)
       result <- fetchvars(core, 2000:2200)
       result[["parameter_value"]] <- value
       result
    }

   x <- run_with_param(core45, Q10_RH(), input$Q10)
   
   ggplot(x, aes(year, value))+
   geom_point()+
   facet_wrap(~variable, scales = "free")
   shutdown(core45)
  })

}

# Create Shiny app ----
shinyApp(ui = ui, server = server)


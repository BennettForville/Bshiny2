library(shiny)
library(hector)
library(ggplot2)
library(dplyr)



# Define UI for app that draws a histogram ----
ui <- fluidPage(
  
  # App title ----
  titlePanel("graphing Hector!"),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(
      
      # Input: Slider for the value of q10 ----
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

# SSP45 <- system.file("input/hector_ssp245.ini", package = "hector")
# reference_plot <- newcore(SSP45)
# run(reference_plot)
# #setvar(base_core45, NA, Q10_RH(), q10, getunits(Q10_RH()))
# #fortify(reference_plot)
# shutdown(reference_plot)

# Define server logic required to draw a histogram ----
server <- function(input, output) {

    output$distPlot <- renderPlot({
    
    q10 <- input$Q10
    
    RCP45 <- system.file("input/hector_ssp245.ini", package = "hector")
    core45 <- newcore(RCP45)
    reference <- fetchvars(core45, NA)
    reference$source <- "reference"
    setvar(core45, NA, Q10_RH(), q10, getunits(Q10_RH()))
    run(core45)
    result <- fetchvars(core45, 2000:2200)
    result$source <- "user_input"
    shutdown(core45)
    
    output <- bind_rows(reference,result)
    
    ggplot(output, aes(year, value, color = source))+
      geom_point()+
      facet_wrap(~variable, scales = "free") + 
      ggtitle(q10)
    #+
     # geom_line(data = reference)
      
  })
  
}

# Create Shiny app ----
shinyApp(ui = ui, server = server)


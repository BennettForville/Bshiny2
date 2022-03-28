library(shiny)
library(hector)
library(ggplot2)
library(dplyr)

scenario_choices <- c("SSP119 (Green hippie world)",
                      "SSP245(Middle of the road)",
                      "SSP370 (Eh.)",
                      "SSP460 (We might have a chance of survival)",
                      "SSP585 (Armageddon)")
SSP_files <- c("input/hector_ssp119.ini",
               "input/hector_ssp245.ini",
               "input/hector_ssp370.ini",
               "input/hector_ssp460.ini",
               "input/hector_ssp585.ini")
names(SSP_files) <- scenario_choices

               
variables <- c(ATMOSPHERIC_CO2(), GLOBAL_TEMP(), SOIL_C(), PH_HL(), PH_LL())
               


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
                  value = 2),
    
        radioButtons(inputId = "SSP",
                   label = "Scenario choices:",
                   choices = scenario_choices,
                   selected = "SSP245")
      
      
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
server <- function(inputID, output) {

    output$distPlot <- renderPlot({
    
    q10 <- input$Q10
    
    file <- SSP_files[input$SSP]
    Sc <- system.file(file, package = "hector")
    core <- newcore(Sc)
    run(core)
    reference <- fetchvars(core, 2000:2200, variables)
    reference$source <- "reference"
    setvar(core, NA, Q10_RH(), q10, getunits(Q10_RH()))
    reset(core)
    run(core)
    result <- fetchvars(core, 2000:2200, variables)
    result$source <- "user_input"
    shutdown(core)
    
    output <- bind_rows(reference,result)
    
    ggplot(output, aes(year, value, color = source))+
      geom_line()+
      facet_wrap(~variable, scales = "free") + 
      ggtitle(paste0("Q10 = ", q10, " in scenario ", input$SSP))
    
        
      
  })
  
}

# Create Shiny app ----
shinyApp(ui = ui, server = server)


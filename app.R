library(shiny)
library(hector)
library(ggplot2)
library(dplyr)
library(shinythemes)
library(shinyBS)

scenario_choices <- c("Scenario 1 - Green hippie world",
                      "Scenario 2 - Middle of the road",
                      "Scenario 3 - A rocky road",
                      "Scenario 4 - Inequality",
                      "Scenario 5 -  Armageddon")
SSP_files <- c("input/hector_ssp119.ini",
               "input/hector_ssp245.ini",
               "input/hector_ssp370.ini",
               "input/hector_ssp460.ini",
               "input/hector_ssp585.ini")
names(SSP_files) <- scenario_choices

               
variables <- c(ATMOSPHERIC_CO2(), GLOBAL_TEMP(), SOIL_C(), PH_HL(), PH_LL())
               


# Define UI for app that draws a histogram ----
ui <- fluidPage(
  
  # define theme
  theme = shinytheme("superhero"),
  
  # App title ----
  titlePanel("What does our future look like?"),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(
      
      # Input: Slider for the value of q10 ----
      sliderInput(inputId = "Q10",
                  label = "Choose a Q10 value:",
                  min = 1,
                  max = 5,
                  value = 2),
    
        radioButtons(inputId = "SSP",
                   label = "Select a specific scenerio (SSP):",
                   choices = scenario_choices,
                   selected = "Scenario 2 - Middle of the road")
      
      
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(
      
      tabsetPanel(type = "tabs",
                  tabPanel("Home", verbatimTextOutput("Home")),
                  tabPanel("graphs", plotOutput(outputId = "distPlot")),
                  tabPanel("summary", verbatimTextOutput("summary"))),
                  
      # Output: Histogram ----
     # plotOutput(outputId = "distPlot")
      
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
    
    ggplot(output, aes(year, value, color = source, linetype = units)) +
      geom_line() +
      facet_wrap(~variable, scales = "free") + 
      ggtitle(paste0("Q10 = ", q10, " in scenario ", input$SSP)) +
      scale_color_viridis_d(begin = 0.4, end = 0.8) +
      scale_linetype_manual(values = c(3, 5, 4, 1)) +
      theme_light()
        
      
  })
  
}

# Create Shiny app ----
shinyApp(ui = ui, server = server)


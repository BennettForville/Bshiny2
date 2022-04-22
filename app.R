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


variables <- c(ATMOSPHERIC_CO2(), PH_HL(), SOIL_C(), GLOBAL_TEMP())

# https://stackoverflow.com/questions/36132204/reactive-radiobuttons-with-tooltipbs-in-shiny
# This function allows for multiple unique hover bars within a radioButtons call
# while maintaining one consistent inputId
radioTooltip <- function(id, choice, title, placement = "bottom", trigger = "hover", options = NULL){
  
  options = shinyBS:::buildTooltipOrPopoverOptionsList(title, placement, trigger, options)
  options = paste0("{'", paste(names(options), options, sep = "': '", collapse = "', '"), "'}")
  bsTag <- shiny::tags$script(shiny::HTML(paste0("
    $(document).ready(function() {
      setTimeout(function() {
        $('input', $('#", id, "')).each(function(){
          if(this.getAttribute('value') == '", choice, "') {
            opts = $.extend(", options, ", {html: true});
            $(this.parentElement).tooltip('destroy');
            $(this.parentElement).tooltip(opts);
          }
        })
      }, 500)
    });
  ")))
  htmltools::attachDependencies(bsTag, shinyBS:::shinyBSDep)
}

# Define UI for app that draws a histogram ----
ui <- fluidPage(
  
  # Define theme
  theme = shinytheme("superhero"),
  
  # App title ----
  titlePanel("What does our future look like?"),
  
  # Main panel for displaying outputs ----
  mainPanel(
    
    tabsetPanel(type = "tabs",
                tabPanel("Home", verbatimTextOutput("Home"),
                         mainPanel("This will be baisc climate info & explanation of important terms")),
                
                
                tabPanel("graphs", 
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
                             
                             fluidRow(
                               column(12,
                                      radioButtons(inputId = "SSP", 
                                                   label = "Select a specific scenario (SSP):", 
                                                   choices = scenario_choices,
                                                   selected = "Scenario 2 - Middle of the road")
                                      
                               ),
                               # id: same inputId as above (very important for reactivity)
                               # title: text that will appear in the hover bar
                               # choice: from the list of scenario_choices passed into radioButtons
                               # placement: optional, but where the hover bar will appear relative to text
                               radioTooltip(id = "SSP", 
                                            title = "SSP119 - lots of change to daily life, slow improvement",
                                            choice = "Scenario 1 - Green hippie world",
                                            placement = "right"),
                               radioTooltip(id = "SSP",
                                            title = "SSP245 - Not much change to daily life, no improvement",
                                            choice = "Scenario 2 - Middle of the road",
                                            placement = "right"),
                               radioTooltip(id = "SSP",
                                            title = "SSP370 - Focus on food & energy security, environmental degradation",
                                            choice = "Scenario 3 - A rocky road",
                                            placement = "right"),
                               radioTooltip(id = "SSP",
                                            title = "SSP460 - Unequal investments, rich get richer, poor get poorer",
                                            choice = "Scenario 4 - Inequality",
                                            placement = "right"),
                               radioTooltip(id = "SSP",
                                            title = "SSP585 - Human & global capital become main focus, fossil fuels are managed",
                                            choice = "Scenario 5 -  Armageddon",
                                            placement = "right")
                             )),
                           mainPanel(plotOutput(outputId = "distPlot")))),
                tabPanel("impacts", verbatimTextOutput("impacts"),
                         mainPanel("This will be overall impacts on climate and big take aways from app"))
                )))
                             
                           
    
    
    
    
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
        
        output <- bind_rows(reference, result)
        
        var_labels <- c("Atmospheric C", "Ocean pH", "Soil C", "Temperature increase")
        names(var_labels) <- variables
        
        ggplot(output, aes(year, value, color = source, linetype = units)) +
          geom_line() +
          facet_wrap(~variable, 
                     scales = "free",
                     labeller = labeller(variable = var_labels)) + 
          ggtitle(paste0("Q10 = ", q10, " in scenario ", input$SSP)) +
          scale_color_viridis_d(begin = 0.4, end = 0.8) +
          scale_linetype_manual(values = c(3, 5, 4, 1)) +
          theme_light()
      }
      )
    }
    
    # Create Shiny app ----
    
    shinyApp(ui = ui, server = server)
    
    shinyApp(ui = ui, server = server)
    

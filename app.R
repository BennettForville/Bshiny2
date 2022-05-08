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

# Run Hector for the reference scenario and save those data -
# we always want to show this reference line on the graphs
file <- SSP_files["Scenario 2 - Middle of the road"]
core <- newcore(system.file(file, package = "hector"))
run(core)
motr_reference <- fetchvars(core, 2000:2200, variables)
motr_reference$source <- "MOTR reference"
shutdown(core)

first_time <- TRUE

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
    
    # Navigation bar to list app tabs
    navbarPage(title = "",
               id = "tabs",
               
               tabPanel(title = "Welcome", value = "welcome", verbatimTextOutput("Welcome"),
                        mainPanel(p("Welcome!"),
                                  p("This is an interactive app to help spread information about climate change
                                    and what the worlds future looks like."),
                                  p(em("App by Bennett Grace Forville")),
                                  
                                  actionButton(inputId = "to_home",
                                               label = "Next page"),
                                  width = 20),
                        sidebarPanel(
                          img(src = "intro_image.png", height = 140, width = 400)
                        )),
               
 
               
               tabPanel(title ="Intro", value = "intro", verbatimTextOutput("intro"),
                        mainPanel( p("As we know, the planet is quickly warming. 
                                   This is an event that we all know as climate change, but what exactly does it mean?"),
                                   
                                   p("Well, too much carbon dioxide, CO2, is being released into the atmosphere. This prevents 
                                   heat from escaping and creates a global warming effect. There are many different impacts 
                                   of climate change, some which we see in daily life and others that don't become apparent 
                                   until you take a closer look."),
                                   
                                   p("Using Hector, a simple climate model, this app allows you to take a closer look at graphs
                                   that track historical climate data and project possible future outcomes for the climate based
                                   on our actions in the present day."),
                                   
                                   p("You will be able to manipulate two model inputs. “Q10” measures the sensitivity of microbial respiration
                                     to temperature increases - essentially looking at how many carbon dioxide microbes are put into the
                                     atmosphere as the world warms. A higher Q10 means more CO2. An “SSP” is a shared socioeconomic pathway;
                                     the SSPs are a widely used class of climate scenarios. SSP 2, called the middle of the road, is the best 
                                     projection for our future if we don't make any drastic changes. SSP 1, 3, 4, & 5 are different possible 
                                     outcomes for our climate depending on changes to global policy and our daily actions. The SSPs are differentiated
                                     by different levels of challenge to mitigation, which is essentially looking at the reduction of carbon 
                                     emissions, and adaptation, meaning changes that humans will have to make in order to live with the consequences
                                     of climate change. For example, taking mitigation efforts such as putting limits on fossil fuel emissions, 
                                     encouraging the use of renewable energy, or even encouraging the use of public transportation would require 
                                     a change in one's day to day life - some actions being more challenging to implement than others."),
                                   
                                   p("This app will provide you with several outcomes for the future while looking specifically at 
                                   changes in global temperature, atmospheric carbon, carbon levels in soil, and ocean salinity. 
                                   While it is impossible to establish all of the impacts caused by changes in these four climate
                                   variables, this app allows you to take a look at possibilities for the future and to examine different
                                   consequences."),
                                   
                                   p("After viewing the information under the graphs tab, head over to the impacts tab to learn more."),
                                   width = 20),
                        
                        # Button to take user to the graphs tab
                        actionButton(inputId = "to_graphs",
                                     label = "Next page")),
               
               tabPanel(title = "Graphs", value = "graphs",
                        
                        # Sidebar layout with input and output definitions ----
                        sidebarLayout(
                          
                          # Sidebar panel for inputs ----
                          sidebarPanel(
                            
                            # Input: Slider for the value of q10 ----
                            sliderInput(inputId = "Q10",
                                        label = "Multiply CO2 by:",
                                        min = 1,
                                        max = 5,
                                        value = 2),
                            
                            fluidRow(
                              column(12,
                                     radioButtons(inputId = "SSP", 
                                                  label = "Select a specific scenario (SSP):", 
                                                  choices = scenario_choices,
                                                  selected = "Scenario 2 - Middle of the road")),
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
                          mainPanel(plotOutput(outputId = "distPlot"))),
                        
                        # Button to take user to impacts tab
                        actionButton(inputId = "to_impacts",
                                     label = "Next page")),
               
               
               tabPanel(title = "Impacts", value = "impacts", verbatimTextOutput("impacts"),
                        mainPanel(p("The impacts of climate change are vast and sometimes unexpected. As a result of the quickly changing
                                   climate, we can expect a variety of outcomes."), 
                                  
                                  p("Our climate becomes:"),
                                  
                                  p(strong("Hotter.")),
                                  
                                  p("High temperatures are often talked about in connection to climate change. An increase of CO2 in the 
                                   atmosphere means an increase in temperatures as well as a decrease in soil carbon levels. The decrease
                                   of soil carbon could create a disturbance for the plants growing which could eventually result in e
                                   xtinction and a lack of biodiversity. Heat waves, crop failures, and shifts in plant and animal ranges are 
                                   expected to occur if carbon emissions continue to go unchecked."), 
                                  
                                  p(strong("Wetter.")),
                                  
                                  p("In correlation with the warming temperatures, glaciers and ice sheets will melt and the oceans will expand. 
                                   Rising sea levels will ruin coastal communities and infrastructure. This will also lead to an increase in 
                                   rainfall and flooding. It is important to note that a short term increase in rainfall could lead to drought 
                                   and water shortages in the future."),  
                                  
                                  p(strong("More extreme weather.")), 
                                  
                                  p("Warmer air and oceans are leading to an increase in the severity and number of storms and in drier areas, 
                                   warmer weather is linked to more significant droughts and a longer fire season."), 
                                  
                                  p(em("To sum it up…")),  
                                  
                                  HTML("<ul>
                            <li> Droughts </li>
                            <li> Extreme weather conditions </li>
                            <li> Shortages in staple crops (corn & wheat) </li>
                            <li> Food insecurity </li>
                            <li> Price spikes on consumable items </li> 
                            <li> Unlivable coastal regions </li>
                            <li> Death of coral reefs </li>
                            <li> Fish and sea life would be pushed from their homes </li>
                            <li> Mass extinction of climate dependent animals </li>
                            <li> Arctic melting </li>
                            <li> Lack of land and other resources </li>
                            <li> Lack of resources could lead to political and social tensions </li>
                            </ul>"), 
                            width = 20)),
                        
                   tabPanel(title = "Info", value = "info", verbatimTextOutput("Info"),
                                 mainPanel(p("Thank you for going through my app!"),
                                           p(em("For more information, you can look at the following sources...")),
                                           
                                           HTML("<ul>
                            <li> https://www.noaa.gov/education/resource-collections/climate/climate-change-impacts </li>
                            <li> https://www.ucsusa.org/climate/impacts </li>
                            <li> https://climate.nasa.gov/news/2878/a-degree-of-concern-why-global-temperatures-matter/ </li>
                            <li> https://climate.nasa.gov/news/2865/a-degree-of-concern-why-global-temperatures-matter/ </li>
                            <li> https://jgcri.shinyapps.io/HectorUI/ </li>
                            </ul>"),
                            width = 20,
                            
                                    )))))


# Define server logic required to draw a histogram ----
server <- function(input, output, session) {
  
  output$distPlot <- renderPlot({
    
    q10 <- input$Q10
    
    if(first_time) {
      output <- motr_reference
      first_time <<- FALSE
    } else {
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
      
      output <- bind_rows(reference, result, motr_reference)
    }

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
  })
  
  # Server functions connecting to the actionButton calls that update which tab
  # a user is on. The session argument is passed into the server() call above
  # and allows for functional modifications to the user's active Shiny session.
  observeEvent(input$to_home, {updateTabsetPanel(session,
                                                 inputId = "tabs",
                                                 selected = "intro")})
  observeEvent(input$to_graphs, {updateTabsetPanel(session,
                                                   inputId = "tabs", 
                                                   selected = "graphs")})
  
  observeEvent(input$to_impacts, {updateTabsetPanel(session,
                                                    inputId = "tabs", 
                                                    selected = "impacts")})
}

# Create Shiny app ----

shinyApp(ui = ui, server = server)


## code that did not work, from the q10 code, it is simplified in the actual shiny app called app.R

#   RCP45 <- system.file("input/hector_ssp245.ini", package = "hector")
#   core45 <- newcore(RCP45)
#   run(core45)
#   plot_data <- fetchvars(core45,2000:2200)
#   shutdown(core45)
#   
#   # ggplot(plot_data, aes(year, value))+
#   #   geom_point()+
#   #   facet_wrap(~variable, scales = "free")+
#   #   ggtitle(input$Q10)
# 
#   
#  run_with_param <- function(core, parameter, value) {
#      old_value <- fetchvars(core, NA, parameter)
#      unit <- as.character(old_value[["units"]])
#      setvar(core, NA, parameter, value, unit)
#      reset(core)
#      run(core)
#      result <- fetchvars(core, 2000:2200)
#      result[["parameter_value"]] <- value
#      result
#   }
# 
#  x <- run_with_param(core45, Q10_RH(), input$Q10)
#  
#  ggplot(x, aes(year, value))+
#  geom_point()+
#  facet_wrap(~variable, scales = "free")
#  
# })
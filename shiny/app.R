# CHI-power dashboard
# You can run the application by clicking the 'Run App' button above.

#######
# App #
#######

shinyApp(ui     = CHIpower::app_ui(),
         server = CHIpower::app_server())

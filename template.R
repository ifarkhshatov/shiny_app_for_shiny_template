
library(shiny)

ui <-  fluidPage(tabsetPanel(
 
tabPanel(id = 'sadas',
flowLayout(
textInput('dasd', 'dasd') ,
rendertext('adsasd', 'adsasd'))) 
))

server <- function(input, output, session) {

}

shinyApp(ui, server)

library(shiny)

ui <- fluidPage(
  titlePanel("Shiny App Generator"),
  sidebarLayout(
    sidebarPanel(
      
      selectInput("tab_type", "Select Tab type:",
                  c("tabsetPanel", "navbarPage", "navlistPanel")),
      textInput("tab_title", "Tab title:"),
      selectInput("tab_layout", "Select Tab layout:",
                  c("flowLayout", "sidebarLayout", "fluidRow",
                    "splitLayout","verticalLayout")),
      actionButton("add_tab", "Add tab"),
      actionButton("remove_tab", "Remove last tab"),
      
      selectInput("input_type", "Select input type:",
                  c("text", "numeric", "date", "select", "slider", "checkbox", "radio", "file")),
      textInput("input_label", "Input label:"),
      actionButton("add_input", "Add input"),
      actionButton("remove_input", "Remove last input"),
      
      selectInput("output_type", "Select output type:",
                  c("text", "plot", "table", "download")),
      textInput("output_label", "Output label:"),
      actionButton("add_output", "Add Output"),
      actionButton("remove_output", "Remove last output"),
      
      checkboxInput("include_library", "Include additional libraries"),
      textInput("library", "Library Name"),
      
      actionButton("save_code", "Save Code"),
    ),
    mainPanel(
      verbatimTextOutput("code")
    )
  )
)

server <- function(input, output) {
  #############################################################################
  ###                                                                      ####
  ###                               TABS                                   ####
  ###                                                                      ####
  #############################################################################
  
  tabs <- reactiveValues(input = c(), id = c())
  # Add input
  observeEvent(input$add_tab, {
     if(input$tab_title %in% tabs$id) {
       tabs$input <- tabs$input[-length(tabs$input)]
       tabs$id <- tabs$id[-length(tabs$id)]
     }
    #Tab layout
      if (input$tab_layout == "flowLayout" ) {
        tab_layout <- " flowLayout( %%paste_input%%  %%paste_output% )"
      } else if (input$tab_layout == "fluidRow"  ) {
        tab_layout <- "fluidRow(column(width = 12 %%paste_input%% %%paste_output%% ))"
      } else if (input$tab_layout == "sidebarLayout") {
        tab_layout <-  "sidebarLayout( 
          sidebarPanel(%%paste_input%%), 
          mainPanel(%%paste_output%) 
        )"
      } else if (input$tab_layout == "splitLayout") {
        tab_layout <-  "splitLayout(%%paste_input%% %%paste_output%)"
      } else {
        tab_layout <-  "verticalLayout( %%paste_input%% %%paste_output% )"
      }
        
      tabs$input <- append(tabs$input,
                           paste0( "tabPanel(id = '", input$tab_title, "', '",tab_layout, "')"))    
      tabs$id <- append(tabs$id, input$tab_title) 

    print(tabs$input)
  })
  
  #Remove Input
  observeEvent(input$remove_tab, {
    tabs$input <- tabs$input[-length(tabs$input)]
    tabs$id <- tabs$id[-length(tabs$id)]
    print(tabs$input)
  
  })
  
  #############################################################################
  ###                                                                      ####
  ###                               INPUTS                                 ####
  ###                                                                      ####
  #############################################################################
  
  inputs <- reactiveValues(input = c(), id = c())
  # Add input
  observeEvent(input$add_input, {
    
    if(!input$input_label %in% inputs$id) {
      inputs$input <- append(inputs$input,      paste0(input$input_type, "Input('",
                                                       input$input_label, "', '", input$input_label, "')"))    
      
      inputs$id <- append(inputs$id, input$input_label) 
    }
    print(inputs$input)
  })
  
  #Remove Input
  observeEvent(input$remove_input, {
    inputs$input <- inputs$input[-length(inputs$input)]
    inputs$id <- inputs$id[-length(inputs$id)]
    print(inputs$input)
  })
  #############################################################################
  ###                                                                      ####
  ###                               OUTPUT                                 ####
  ###                                                                      ####
  #############################################################################
  
  outputs <- reactiveValues(input = c(), id = c())
  
  observeEvent(input$add_output, {
    
    if(!input$output_label %in% outputs$id) {
      outputs$input <- append(outputs$input,      paste0("render", input$input_type, "('",
                                                       input$input_label, "', '", input$input_label, "')"))    
      
      outputs$id <- append(outputs$id, input$input_label) 
    }
    print(outputs$input)
  })
  
  #Remove Input
  observeEvent(input$remove_output, {
    outputs$input <- outputs$input[-length(outputs$input)]
    outputs$id <- outputs$id[-length(outputs$id)]
    print(outputs$input)
  })
  
  
  
  
  
  
  
  default <- reactiveValues(
    x = 
    paste0("library(shiny)\n\nui <-  fluidPage(%%tab_type%%(\n %%paste_layout%% ",
           "\n))\n\nserver <- function(input, output, session) {\n\n}\n\nshinyApp(ui, server)")
  ) 
  
  
  observeEvent(c(input$tab_type, input$add_input, input$add_tab, input$add_output,
                 input$remove_input, input$remove_tab, input$remove_output), {
    
    render <-  gsub("%%tab_type%%", input$tab_type , default$x )   
    
    if (!is.null(tabs$input)) {
      
      render <- gsub("%%paste_layout%%",paste0("\n",tabs$input,collapse = ",\n") , render)
    }
    if (!is.null(inputs$input)) {
      render <- gsub("%%paste_input%%", paste0("\n",inputs$input, collapse = ",\n"), render)
    }
    
    if (!is.null(outputs$output)) {
      render <- gsub("%%paste_output%", paste0("\n",outputs$input, collapse = ",\n"), render)
    }
    
    output$code <- renderText(render)         
                   
  })
  
  output$code <- renderText({
    default$x
  })
  
  
  
  
  
  
  save_code_button <- reactiveValues(x = FALSE)
  observeEvent(input$save_code, {
    save_code_button$x <- !save_code_button$x
  })
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  # observe({
  #   code <- renderText({
  #     if (input$include_library) {
  #       paste0(library_str,"\n",
  #              "library(shiny)\n\nui <- ",input$tab_type,"(\n  ", 
  #              paste0(inputs, collapse = ",\n  "), 
  #              "\n  ", paste0(tabs, collapse = ",\n  "),"\n",
  #              paste0(outputs, collapse = ",\n  "),"\n)\n\nserver <- function(input, output) {\n\n}\n\nshinyApp(ui, server)")
  #     }
  #     else {
  #       paste0("library(shiny)\n\nui <- ",input$tab_type,"(\n  ", 
  #              paste0(inputs, collapse = ",\n  "), 
  #              "\n  ", paste0(tabs, collapse = ",\n  "),"\n",
  #              paste0(outputs, collapse = ",\n  "),"\n)\n\nserver <- function(input, output) {\n\n}\n\nshinyApp(ui, server)")
  #     }
  #   })
  #   
  #   
  #   
  #   
  #   
  #   
  #   if (save_code_button$x) {
  #     if(input$tab_title != ""){
  #       write(code, file = paste0(input$tab_title,".R"), append = FALSE)
  #       showModal(modalDialog(
  #         title = "Success",
  #         "Code has been saved to the directory",
  #         easyClose = TRUE,
  #         footer = NULL
  #       ))
  #     }
  #     else {
  #       showModal(modalDialog(
  #         title = "Error",
  #         "Please provide a name for your tab",
  #         easyClose = TRUE,
  #         footer = NULL
  #       ))
  #     }
  #   }
  #  
  # })

}

shinyApp(ui, server)
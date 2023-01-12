library(shiny)

ui <- fluidPage(
  titlePanel("Shiny App Generator"),
  sidebarLayout(
    sidebarPanel(
      h4("Filling layout"),
      selectInput("tab_type", "Select Tab type:",
                  c("tabsetPanel", "navbarPage", "navlistPanel")),
      textInput("tab_title", "Tab title:"),
      selectInput("tab_layout", "Select Tab layout:",
                  c("flowLayout", "sidebarLayout", "fluidRow",
                    "splitLayout","verticalLayout")),
      actionButton("add_tab", "Add tab"),
      actionButton("remove_tab", "Remove last tab"),
      br(),
      h4("Filling tab"),
      selectInput("tab_id_select", "Select tab Id", NULL),
      br(),
      selectInput("input_type", "Select input type:",
                  c("text", "numeric", "date", "select", 
                    "slider", "checkbox", "radio", "file")),
      textInput("input_label", "Input label:"),
      actionButton("add_input", "Add input"),
      actionButton("remove_input", "Remove last input"),
      
      selectInput("output_type", "Select output type:",
                  c("Text", "Plot", "Table", "Print", "UI","Image",
                    "DataTable")),
      textInput("output_label", "Output label:"),
      actionButton("add_output", "Add Output"),
      actionButton("remove_output", "Remove last output"),
      
      checkboxInput("include_library", "Include additional libraries"),
      textInput("library", "Library Name (use comma to add different libs)"),
      
      actionButton("save_code", "Save Code"),
    ),
    mainPanel(
      verbatimTextOutput("code")
    )
  )
)

server <- function(input, output, session) {
  #############################################################################
  ###                                                                      ####
  ###                               TABS                                   ####
  ###                                                                      ####
  #############################################################################
  
  tabs <- reactiveValues(input = c(), id = c())
  # Add input
  observeEvent(input$add_tab, {
    if(input$tab_title %in% tabs$id) {
      print("triggered remove")
      tabs$input <- tabs$input[tabs$id != input$tab_title]
      tabs$id <- tabs$id[tabs$id != input$tab_title]
      
    }
    
    if (!input$tab_title %in% tabs$id) {
      tabs$id <- append(tabs$id, input$tab_title) 
    }
      #Tab layout
      # if (!input$tab_title %in% tabs$id) {
      print("triggered add")
        paste_input <- paste0("%%paste_input_",input$tab_title,"%% ")
        paste_output <-paste0("%%paste_output_",input$tab_title,"%%")
        
        if (input$tab_layout == "flowLayout" ) {
          tab_layout <- paste0("flowLayout(",paste_input ,",",paste_output,")")
        } else if (input$tab_layout == "fluidRow"  ) {
          tab_layout <- paste0("fluidRow(column(width = 12, ",paste_input ,",",paste_output,"))")
        } else if (input$tab_layout == "sidebarLayout") {
          tab_layout <-  paste0("sidebarLayout( 
          sidebarPanel(",paste_input,"), 
          mainPanel(",paste_output,") 
        )")
        } else if (input$tab_layout == "splitLayout") {
          tab_layout <-  paste0("splitLayout(",paste_input ,",",paste_output,")")
        } else {
          tab_layout <-  paste0("verticalLayout(",paste_input ,",", paste_output,")")
        }
          
        
      # }
    updateSelectInput(session = session, "tab_id_select", choices = tabs$id)
    
      tabs$input <- append(tabs$input,
                     paste0( "tabPanel(id = '", input$tab_title, "',\n",tab_layout, ")"))  

  })

  #Remove Input
  observeEvent(input$remove_tab, {
    tabs$input <- tabs$input[-length(tabs$input)]
    tabs$id <- tabs$id[-length(tabs$id)]
  
  })
  
  #############################################################################
  ###                                                                      ####
  ###                               INPUTS                                 ####
  ###                                                                      ####
  #############################################################################
  
  inputs <- reactiveValues(input = c(), id = c(), tab_id = c())
  # Add input
  observeEvent(input$add_input, {
    
    if(!input$input_label %in% inputs$id) {
      inputs$input <- append(inputs$input,      
                       paste0(input$input_type, "Input('",
                           input$input_label, "', '", input$input_label, "')"))    
      
      inputs$id <- append(inputs$id, input$input_label) 
      inputs$tab_id <- append(inputs$tab_id,  input$tab_id_select)
    }
  })
  
  #Remove Input
  observeEvent(input$remove_input, {
    inputs$input <- inputs$input[-length(inputs$input)]
    inputs$id <- inputs$id[-length(inputs$id)]
    inputs$tab_id <- inputs$tab_id[-length(inputs$id)]

  })
  #############################################################################
  ###                                                                      ####
  ###                               OUTPUT                                 ####
  ###                                                                      ####
  #############################################################################
  
  outputs <- reactiveValues(input = c(), id = c(), tab_id = c())
  
  observeEvent(input$add_output, {
    
    if(!input$output_label %in% outputs$id) {
      outputs$input <- append(outputs$input,    
                              
                      paste0("render", input$input_type, "('",
                           input$output_label, "', '", input$output_label, "')"))    
      
      outputs$id <- append(outputs$id, input$output_label) 
      outputs$tab_id <- append(outputs$tab_id,  input$tab_id_select)
    }
  })
  
  #Remove Input
  observeEvent(input$remove_output, {
    outputs$input <- outputs$input[-length(outputs$input)]
    outputs$id <- outputs$id[-length(outputs$id)]
    outputs$tab_id <- outputs$tab_id[-length(outputs$id)]

  })
  
  
  default <- reactiveValues(
    x = 
    paste0("\nlibrary(shiny)\n\nui <-  fluidPage(%%tab_type%%(\n %%paste_layout%% ",
           "\n))\n\nserver <- function(input, output, session) {\n\n}\n\nshinyApp(ui, server)")
  ) 
  
  render <- reactiveValues(x = ""
  )
  
  
  save_code_text <- reactiveValues(x = "")
  
  
  observeEvent(c(input$tab_type, input$add_input, input$add_tab, input$add_output,
                 input$remove_input, input$remove_tab, input$remove_output), {
    
    render$x <-  gsub("%%tab_type%%", input$tab_type , default$x )   
    
    if (!is.null(tabs$input)) {
      render$x <- gsub("%%paste_layout%%",paste0("\n",tabs$input,collapse = ",\n") , render$x)
    }
    
    
    if (!is.null(inputs$input)) {
    for (tab_id in inputs$tab_id) {
      
      render$x <- gsub(paste0("%%paste_input_",tab_id,"%%"),
                     paste0("\n",inputs$input[inputs$tab_id == tab_id], 
                            collapse = ",\n"), render$x)
    
      }

    }
    
    if (!is.null(outputs$input)) {
      
      for (tab_id in outputs$tab_id) {
        
        render$x <- gsub(paste0("%%paste_output_",tab_id,"%%"),
                       paste0("\n",outputs$input[outputs$tab_id == tab_id],
                              collapse = ",\n"), render$x)
        save_code_text$x <- render$x
        
        }
    }
    output$code <- renderText(render$x)         
  })
  
  output$code <- renderText({
    default$x
  })
  
  observe(
    if(input$include_library && input$library != "" )  {
      lib_list <- paste0("library(",unlist(strsplit(input$library, ",")),")", collapse = "\n")
      output$code <- renderText( paste0(lib_list,render$x, collapse = "\n"))
      save_code_text$x <- paste0(lib_list,render$x, collapse = "\n")
    }
  )
  
  
  observeEvent(input$save_code, {
     write(save_code_text$x, file = paste0("template.R"))
    
    
          showModal(modalDialog(
            title = "File stored",
            "in the directory of the app",
            easyClose = TRUE,
            footer = NULL
          ))
  })
  
  
  
  
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
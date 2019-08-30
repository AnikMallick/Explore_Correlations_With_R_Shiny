loadUI <- function(id){
  ns <- NS(id)
  
  tags$div(
    column(4,
           wellPanel(
             
             selectInput(ns("dataset"), "Dataset", 
                         c("mtcars", "airquality", "faithful","upload data")),
             
             conditionalPanel("input.dataset === 'upload data'",
                              fileInput(ns("file1"), "Only csv file"),ns = ns),
             
             textInput(ns("colnames"),"Enter new column names seperated by ',' : ","Defult Column Names"),
             
             actionButton(ns("show"), "Load"),
             
             actionButton(ns("update"), "Update"),
             
             actionButton(ns("reset"), "Reset"),
             
             tags$br(),
             tags$hr(),
             
             disabled(selectInput(ns("col_class_2change"),"Select the column to change class",
                         NULL,multiple = F)),
             
             disabled(selectInput(ns("to_class"),"Select the class you want to change",
                         c("as.factor","as.numeric","as.character"),multiple = F)),
             
             disabled(actionButton(ns("change"), "Change"))
           )
    ),
    column(8,
           tabBox(title = "Data Snapshot",
                  id = "data_snapshot",height = "800px",width = "100%",
                  tabPanel(title = "Quick look",
                           textOutput(ns("col_n_change_op")),
                           tags$hr(),
                           DTOutput(ns('data_out'))
                  ),
                  tabPanel(title = "Columns",
                           verbatimTextOutput(ns("all_cols"))
                  ),
                  tabPanel(title = "Missing Values",
                           tags$h4("Total Missing values:"),
                           tags$br(),
                           textOutput(ns("total_missing")),
                           tags$hr(),
                           tags$h4("Missing values percentage per column:"),
                           tags$br(),
                           verbatimTextOutput(ns("missing_values"))
                  ),
                  
                  tabPanel(title = "str",
                                  verbatimTextOutput(ns("str")),
                                  tags$head(tags$style(paste0("#",ns("str"),"{font-size:12px; font-style:italic; ",
                                                              "overflow-y:scroll; max-height: 600px; background: ghostwhite;}"))
                           )
                  )
           )
    )
  )
}

load <- function(input, output, session, data_list){
  observeEvent(input$show,{
    if(input$dataset != 'upload data'){
      tbl <- eval(parse(text = input$dataset))
    }else{
      in_file <- input$file1
      if(is.null(in_file)) return(NULL)
      
      tbl <- read.csv(in_file$datapath, header = T,stringsAsFactors = F,as.is=T )
    }
    
    data_list$raw_data <- tbl
    data_list$raw_data_bk <- data_list$raw_data
    data_list$name <- names(data_list$raw_data)
    
    output$data_out <- renderDT(data_list$raw_data,options = list(scrollX = TRUE))
    
    enable("col_class_2change")
    
    enable("to_class")
    
    enable("change")
    
    updateSelectInput(session,"col_class_2change",choices = data_list$name)
  })
  
  observeEvent(input$colnames,{
    if(input$colnames != "Defult Column Names"){
      col_names <- input$colnames
      col_names <- unlist(strsplit(col_names,","))
      if(length(col_names) != length(names(data_list$raw_data))){
        output$col_n_change_op <- renderText({"Failed: please enter same no of column names as the data"})
      }else{
        output$col_n_change_op <- renderText({"Success"})
      }
    }
  })
  
  observeEvent(input$update,{
    
    if(input$colnames != "Defult Column Names"){
      col_names <- input$colnames
      col_names <- unlist(strsplit(col_names,","))
      if(length(col_names) == length(names(data_list$raw_data))){
        names(data_list$raw_data)  <-  col_names
        data_list$name  <-  names(data_list$raw_data)
      }
    }
    output$data_out <- renderDT(data_list$raw_data,options = list(scrollX = TRUE))
    
  })
  
  observeEvent(input$reset, {
    data_list$raw_data  <- data_list$raw_data_bk
    output$data_out <- renderDT(data_list$raw_data,options = list(scrollX = TRUE))
    reset("colnames")
  })
  
  output$all_cols <- renderPrint({paste(names(data_list$raw_data),sep = " , ")})
  
  output$str <- renderPrint({str(data_list$raw_data)})
  
  output$missing_values <- renderPrint({
    sapply(data_list$raw_data, function(x) sum(is.na(x))/length(x) * 100)
  })
  
  output$total_missing <- renderText({
    sum(is.na(data_list$raw_data))
  })
  
  observeEvent(input$change,{
    
    col <- input$col_class_2change
    
    val <- data_list$raw_data[,col]
    data_list$raw_data[,col] = do.call(input$to_class, list(x=val))
    
  })
  
  
  return(data_list)
}
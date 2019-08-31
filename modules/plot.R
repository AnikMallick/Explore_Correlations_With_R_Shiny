# for the tab = Correlogram
# I have followed the below repo
# https://github.com/saurfang/shinyCorrplot

plot_UI <- function(id,data_list){    
  ns <- NS(id)
  
  tagList(
    tabBox(title = "Explore Correlations",
           id = "eca_tabbox",height = "900px",width = "100%",
           
           tabPanel(title = "CorrelationFunnel",
                    column(3,
                           wellPanel(
                             
                             selectInput(ns("predictors_ecf"),"Select the predictors to drop",
                                         names(data_list$raw_data),multiple = T),
                             sliderInput(ns("bins"),"Select the num of bins for numerical features",
                                         max = 8,min = 2, value = 4,step=1),
                             actionButton(ns("binarize"),"Make Binary"),
                             selectInput(ns("target_ecf"),"Select the target",
                                         choices = NULL,multiple = F),
                             sliderInput(ns("zoom"),"Range for the Correlations",
                                         max = 1,min = -1, value = c(-1,1),step=0.1),
                             disabled(actionButton(ns("plot_ecf"),"Generate plot")))),
                    column(9,
                           wellPanel(
                             plotlyOutput(ns("ecf_op_plot"),width = "100%",height = 700)))
                    
           ),
           
           tabPanel(title = "Correlogram",
                    column(3,
                           wellPanel(
                             
                             helpText("Only numeric colmus are used in this sectior"),
                             
                             selectInput(ns("predictors_corplt"),"Select the predictors to drop",
                                         colnames(data_list$raw_data)[sapply(data_list$raw_data, is.numeric)],multiple = T),
                             
                             selectInput(ns("corr_methods"),"Correlation moethod to use",eval(formals(cor)$method)),
                             
                             selectInput(ns("corr_use"), "Handle NA values",
                                         c("everything", "all.obs", "complete.obs", "na.or.complete", "pairwise.complete.obs")),
                             
                             selectInput(ns("corr_plot_orders"), "Reorder Correlation",
                                         eval(formals(corrplot)$order)),
                             
                             selectInput(ns("corr_plot_method"), "Corrplot Method",
                                         eval(formals(corrplot)$method)),
                             
                             checkboxInput(ns("corr_sig"), "Significance Test"),
                             conditionalPanel('input.corr_sig',ns=ns,
                                              numericInput(ns("corr_siglvl"), "Significane Level",
                                                           0.05, 0, 1, 0.01),
                                              selectInput(ns("corr_sigactn"), "Insignificant Action",
                                                          eval(formals(corrplot)$insig))),
                             checkboxInput(ns("corr_show_conf"), "Show Confidence Interval"),
                             conditionalPanel("input.corr_show_conf",ns=ns,
                                              numericInput(ns("corr_conf_intv"), "Confidence Level",
                                                           0.95, 0, 1, 0.01)),
                             
                             actionButton(ns("plot_corplt"),"Plot")
                           )),
                    column(9,
                           wellPanel(
                                     plotOutput(ns("plot_correlogram"),height = 750)))
           )
           
    )
  )
}

plot_ <- function(input, output, session, data_list){
  
  # reactive(updateSelectInput(session, "predictors_ecf",choices = names(data_list$raw_data)))
  
  data_module <- reactiveValues(
    data = NULL
  )
  
  observeEvent(input$target_ecf,{
    if(input$target_ecf != "Not Selected") enable('plot_ecf')
    
  })
  
  
  observeEvent(input$binarize,{
    
    if(length(input$predictors_ecf)>0) data_module$data <- data_list$raw_data %>% select(-c(input$predictors_ecf))
    else data_module$data <- data_list$raw_data
    data_module$data <- data_module$data %>% 
      binarize(n_bins = input$bins, thresh_infreq = 0.01)
    
    updateSelectInput(session, "target_ecf",choices = c("Not Selected",names(data_module$data)))
    
  })
  
  observeEvent(input$plot_ecf,{
    output$ecf_op_plot <- renderPlotly({
      df <- tbl_df(data_module$data)
      df %>% correlate(target = input$target_ecf) %>% 
        plot_correlation_funnel(interactive = T, limits = input$zoom) 
    })
  })
  
  observeEvent(input$plot_corplt,{
    output$plot_correlogram <- renderPlot({
      
      corr_data <- data_list$raw_data %>% 
        select_if(is.numeric)
      
      if(is.null(input$predictors_corplt)){
        corr_data <- corr_data %>% cor(use = input$corr_use, method = input$corr_methods)
      }else{
        corr_data <- corr_data %>% select(-c(input$predictors_corplt)) %>%
          cor(use = input$corr_use, method = input$corr_methods)
      }
      
      sig_data <- corr_corTest(corr_data, input$corr_conf_intv)
      
      corr_data %>%
        corrplot(type="upper", order=input$corr_plot_orders,
                 method = if(!input$corr_show_conf) input$corr_plot_method else NULL,
                 
                 p.mat = sig_data[[1]],
                 sig.level = if(input$corr_sig) input$corr_siglvl else NULL,
                 insig = if(input$corr_sig) input$corr_sigactn else NULL,
                 
                 lowCI.mat = sig_data[[2]],
                 uppCI.mat = sig_data[[3]],
                 plotCI = if(input$corr_show_conf) input$corr_plot_method else "n",
                 tl.col="black", tl.pos = "td", tl.cex = 0.8)
    })
  })
  
  observeEvent(input$corr_show_conf,{
    if(input$corr_show_conf){
      updateSelectInput(session, "corr_plot_method",choices = eval(formals(corrplot)$plotCI)[-1])
    }else{
      updateSelectInput(session, "corr_plot_method",choices = eval(formals(corrplot)$method))
    }
  })
  
}
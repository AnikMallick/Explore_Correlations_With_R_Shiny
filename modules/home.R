homeUI <- function(id){
  tagList(
    tags$h3("Explore Correlations - app by Anik Mallick"),
    tags$hr(),
    tags$h4("Shiny app (Peer-graded Assignment: Course Project) for Coursera Developing Data Products (JHU - Data Science)"),
    column(2),
    column(8,
           wellPanel(
             "In this app you can explore correlation of a dataset.",
             tags$br(),
             tags$hr(),
             "In the 'Data Load' section you can load your own data or load data from base R which is mentioned in the drop down.",
             tags$br(),
             "Clicking on 'Load' will load the data into the app, you can update the names of the columns but for that ",
             "you have to mention all the column names in proper order, you can use the datatable shown beside to take help",
             ". After giving new colnames clicking on 'Update' will update the data",
             ". At any point clicking on the 'Reset' will reset the data to its primery state. ",
             "You can also convert any column to numeric, factor or charecter. ",
             "If you are loading your own data you need to select the 'upload data' selection from the 'Dataset' dropdown. ",
             "Verious tabs here show verious stats of the data.",
             tags$br(),
             tags$hr(),
             "In the 'Correlation' section you can create a corrplot or CorrelationsFunnel plot",
             tags$br(),
             "For 'CorrelationsFunnel', select colums you want to drop (not mendatory). Then cliking on 'Make Binary' will convert ",
             "all the columns to binary data, then select the terget and click on 'Generate plot' to generate the plot.",
             " With the range selector it is possible to select the range of the plot. ",
             "For more info please visit: https://business-science.github.io/correlationfunnel/index.html ",
             tags$br(),
             "For 'Correlogram', select colums you want to drop (not mendatory). Then select all the paremeters for the corrplot. ",
             "Correlation method, Handle Na values, Order, Method, Sig test and Conf interval. ",
             "For more info on this: https://cran.r-project.org/web/packages/corrplot/vignettes/corrplot-intro.html"
           )
    )
  )
}
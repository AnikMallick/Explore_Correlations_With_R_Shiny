source("global.R")

ui <- dashboardPage(
    dashboardHeader(title = "Explore Correlations",titleWidth = 250),
    dashboardSidebar(width = 250,
        sidebarMenu(id = "tab",
                    menuItem("Home",tabName = "home",icon = icon("home")),
                    menuItem("Data Load",tabName = "load",icon = icon("database")),
                    menuItem("Correlation",tabName = "plot_",icon = icon("project-diagram")),
                    
                    tags$br(),
                    tags$hr(),
                    
                    div(style = "padding: 20px;text-align:center;",
                        tags$hr(),
                        helpText("App by Anik Mallick")
                        )
                    )
    ),
    dashboardBody(
        # attaching shinyjs
        useShinyjs(),
        
        tabItems(
            
            tabItem(tabName = "home",
                    homeUI("home")),
            
            tabItem(tabName = "load",
                    loadUI("load")),
            
            tabItem(tabName = "plot_",
                    uiOutput("plot_UI"))
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output,session) {
    
    # closing app if using as standalone
    if (!interactive()) {
        session$onSessionEnded(function() {
            stopApp()
            q("no")
        })
    }
    
    data_list <- reactiveValues(
        raw_data = NULL,
        raw_data_bk = NULL,
        name = NULL
    )
    
    data_list <- callModule(load,id="load",data_list)
    
    observeEvent(input$tab,{
        if(input$tab == "plot_") {output$plot_UI <- renderUI(plot_UI("plot_",data_list))}
        callModule(plot_,id="plot_",data_list)
    })
    
}

# Run the application 
shinyApp(ui = ui, server = server)

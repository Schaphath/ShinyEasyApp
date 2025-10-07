

library(shiny)
library(bslib)


# EXEMPLE APP : DataViz

ui <- fluidPage(
    
    theme = bslib::bs_theme(bootswatch = "morph"),
    
    tabsetPanel(
        
        
        tabPanel("Data", style="margin-top:70px; margin-left:20px;",
                 
                 sidebarLayout( 
                     
                     sidebarPanel(width = 3,
                                  
                                  fileInput("file", label = "Import data", accept = c(".csv", ".tsv", ".xls", ".xlsx"),
                                            multiple = T, width = "270px"),
                                  
                                  numericInput("n", "Rows", value = 5, min = 1, step = 1, width = "100px")
                     ),
                     
                     mainPanel(
                         dataTableOutput("table"), style="margin-top:70px; margin-left:55px;"
                     )
                 )
                 
                 
                 
        ),
        
        tabPanel("DataViz",
                 
                 sidebarLayout(
                     
                     sidebarPanel( width = 3,
                                   
                                   div( style = "padding:10px;",
                                        
                                        selectInput("target", label = "Fill (categorical variable)", choices = NULL, width = "220px"),
                                        
                                        selectInput("feature", label = "Features", choices = NULL, multiple = TRUE,
                                                    width = "250px"),
                                        
                                        selectInput("plots", label = "Plots", selected = NULL,
                                                    choices = c("densPlots", "scatterPlots", "boxPlots", "histPlots"), width = "220px"),
                                        
                                        textAreaInput("title", label = "", placeholder = "Title", width = "250px",
                                                      rows = 1),
                                        
                                        textAreaInput("caption", label = "", placeholder = "Caption", width = "250px",
                                                      rows = 1),
                                        
                                        sliderInput("alpha", label = "Alpha", value = 0.2, min = 0.2, max = 1, step = 0.2,
                                                    width = "220px"),
                                        
                                        actionButton("action", label = "ShowPlots", style = "margin-top:30px;")  
                                        
                                   )
                     ),
                     
                     mainPanel( plotOutput("plots", height = "550px"), style="margin-top:50px; margin-left:55px;")
                     
                 )
        )
    )
)




# SERVER

server <- function(input, output, session){
    
    # Reactive elements
    features <- reactive({input$feature})
    target <- reactive({input$target})
    title <- reactive({input$title})
    caption <- reactive({input$caption})
    alpha <- reactive({input$alpha})
    
    
    
    
    data <- reactive({
        req(input$file)
        
        ext <- tolower(tools::file_ext(input$file$name))
        
        switch(ext,
               csv  = vroom::vroom(input$file$datapath, delim = ","),
               tsv  = vroom::vroom(input$file$datapath, delim = "\t"),
               xls  = readxl::read_excel(input$file$datapath),
               xlsx = readxl::read_excel(input$file$datapath),
               validate("Fichier non valide. Veuillez importer un fichier .csv, .tsv, .xls ou .xlsx")
        )
    })
    
    
    output$table <- renderDataTable({
        
        head(data(), input$n)
        
    })
    
    
    # Met à jour les menus déroulants 
    observeEvent(data(), {
        vars <- names(data())
        updateSelectInput(session, "target", choices = vars, selected = character(0)) 
        updateSelectInput(session, "feature", choices = vars)
    })
    
    
    # Retire la variable cible de la liste des features
    observeEvent(input$target, {
        vars <- setdiff(names(data()), input$target)
        updateSelectInput(session, "feature", choices = vars, selected = intersect(input$feature, vars))
    })
    
    
    
    event_graphe <- eventReactive(input$action, {
        
        if (input$plots == "densPlots") {
            
            densPlots(data = data(), vars = features(),
                      target = target(), alpha = alpha(),
                      title = title(), caption = caption(), tagLevel = "A")
            
        } else if(input$plots == "scatterPlots") {
            
            scatterPlots(data = data(), vars = features(),
                         target = target(), alpha = alpha(),
                         title = title(), caption = caption(), tagLevel = "A")
            
        } else if (input$plots == "histPlots") {
            
            histPlots(data = data(), vars = features(),
                         target = target(), alpha = alpha(),
                         title = title(), caption = caption(), tagLevel = "A")
            
        }
        
        else {
            
            boxPlots(data = data(), vars = features(),
                     target = target(), alpha = alpha(),
                     title = title(), caption = caption(), tagLevel = "A")
            
        }
    })
    
    
    output$plots <- renderPlot({
        
        event_graphe()
    })
}


# RUN APP
shinyApp(ui = ui, server = server)


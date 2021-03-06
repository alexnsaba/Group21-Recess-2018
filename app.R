library(shiny)
first <- read.csv("Seasons_Stats.csv", header=TRUE,sep=",")

second <- read.csv("Players.csv", header=TRUE,sep=",")

third <- read.csv("player_data.csv", header=TRUE,sep=",")
ui = fluidPage(
  tags$style(type = 'text/css', 
             '.navbar { background-color: black;color: white}',
             '.navbar-default .navbar-brand{color: white;}',
            

             'body{ color: black; background-color:#008080}'

             
  ),
  
  navbarPage(title=p("NATIONAL BASKET BALL ASSOCIATION ANALYSIS"),
                
                  
                tabPanel("SUMMARY",
               sidebarLayout(
                 sidebarPanel(
                   # player
                   selectInput(
                     "Choice4",
                     "SELECT VARIABLE TO SUMMARIZE IN PLAYER DATASET",
                     choices = colnames(second)[2:8],
                     selected = "please select"
                   ),
                   uiOutput("out5"),


               br(),br(),
               hr(),
              # seasons stats
              selectInput(
                 "Choice1",
                 "SELECT VARIABLE TO SUMMARIZE IN THE SEASONS STATS DATASET",
                 choices = colnames(first)[2:53],
                 selected = "please choose"
               ),
               uiOutput("Out1"),br(), br(),
              
              # player_data
              selectInput(
                "Choice5",
                "SELECT VARIABLE TO SUMMARIZE IN THE PLAYERDATA DATASET",
                choices = colnames(third)[],
                selected = "please choose"
              ),
              uiOutput("Out10")
               
               
                 ),
                 mainPanel(
                   
                   conditionalPanel("input.Choice3 === 'Summarize player'", verbatimTextOutput("Out6")),
                   conditionalPanel("input.Choice3 === 'View details'", tableOutput("Out7")),
                   
                   conditionalPanel("input.Choice2 === 'Summary'", verbatimTextOutput("Out2")),
                   #conditionalPanel("input.Choice2 === 'Not interested'", tableOutput("Out3")),
                   conditionalPanel("input.Choice6 === 'Summarize'", verbatimTextOutput("Out12")),
                   conditionalPanel("input.Choice6 === 'Not interested in'", tableOutput("Out13"))
     
                 )
               )),
               tabPanel("PREDICTIONS"
               ),
               tabPanel("RELATIONSHIPS",
                        sidebarLayout(
                          sidebarPanel(
                            checkboxGroupInput("datasets", "CHOOSE THE DATASET TO ANALYZE",
                                               choiceNames = list("Seasons stats","Player","Player data"),
                                               choiceValues= list("first","","")
                                               
                            )
                            
                          ),
                          mainPanel(

                          )
                          
                        )
               ),
               tabPanel("COMPARISON"

               ),
               tabPanel("HELP"
                       
               ),
               tabPanel("LOGOUT"
                        
               ),
               
               
             
          footer = h4("CopyRight @ Group21 (2018)")    
               
               
               ))

server = function(input, output) {
  
  a = reactive({
    first[, colnames(first) == input$Choice1]
  })
  b= reactive({
    second[, colnames(second) == input$Choice4]
  })
  d = reactive({
    first[, colnames(third) == input$Choice5]
  })
  
  
  output$Out1 = renderUI({
    selectInput(
      "Choice2",
      "Are you sure you want to summarize Seasons stats",
      choices = c("Not interested", "Summary")
      
    )
  })
  output$Out2 = renderPrint({

    summary(a())
  })
  output$Out3 = renderTable({
    return(a())
  })
  
 output$out5 = renderUI({
    selectInput(
      "Choice3",
      "Are you sure you want to summarize Player data",
      choices = c("Summarize player", "View details")
      
    )
    
  })
 
 output$Out6 = renderPrint({
   summary(b())
 })
 output$Out7 = renderTable({
   return(b())
 })
 
 
 output$Out10 = renderUI({
   selectInput(
     "Choice6",
     "Are you sure you want to summarize Seasons stats",
     choices = c("Summarize", "Not interested in")
     
   )
 })
 output$Out12 = renderPrint({
   summary(b())
 })
 output$Out13 = renderTable({
   return(b())
 })
 

}
shinyApp(ui = ui, server = server)
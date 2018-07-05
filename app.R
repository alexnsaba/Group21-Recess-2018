library(shiny)
ui <- fluidPage(
  
  tags$style(HTML("
   p{color:green;font-size:20pt;}
   h4{color:blue;}
   
 
                   ")),

  
  navbarPage(title=p("NATIONAL BASKET BALL ASSOCIATION ANALYSIS"),
             
             
               tabPanel("SUMMARY", 
                        
                        fluidRow(
                          selectInput("select1", label = h4("Choose the data to summarize"), 
                                      choices = list("SEASON STATS" = read.csv('Seasons_Stats.csv', stringsAsFactors = FALSE) , "PLAYER DATA" = 2, "PLAYER" = 3), selected = 1),
                          col( 2,
                              tableOutput("sel")
                              )
                 # conditionalPanel(input.select1="SEASON STAT",
                  #select) 
               )),
               tabPanel("PREDICTIONS",
                        fluidRow(plotOutput("statePlot"),
                                 wellPanel(
                                   sliderInput(inputId = "nlabels",
                                               label = "Top n States:",
                                               min = 1,
                                               max = 10,
                                               value = 6,
                                               step = 1)
                                 )
                        )
               ),
               tabPanel("RELATIONSHIPS",
                        fluidRow(plotOutput("iStatePlot"),
                                 wellPanel(
                                   htmlOutput("selectState"))
                        ),
                        
                        mainPanel(
                          plotOutput("histPlot")  
                        )
               ),
               
               tabPanel("COMPARISONS",
                        fluidRow(plotOutput("iStatePlot"),
                                 wellPanel(
                                   htmlOutput("selectState"))
                        )
               ),
               
               tabPanel("HELP",
                        fluidRow(plotOutput("iStatePlot"),
                                 wellPanel(
                                   htmlOutput("selectState"))
                        )
               ),
               
               
               tabPanel("LOGOUT",
                        fluidRow(plotOutput("iStatePlot"),
                                 wellPanel(
                                   htmlOutput("selectState"))
                        )
               )
               
               
             
             
             
             ),
  
  
  
  
  
  
  
  
  
  column(3,
         selectInput("select", label = h3("Select box"), 
                     choices = list("Choice 1" = 1, "Choice 2" = 2, "Choice 3" = 3), selected = 1)),
  br(),
  
  
  sliderInput(inputId="num",label="choose from 1 to 100 ",value=25, min=1,max=100),
  textInput(inputId="title",label="write a title", value="Histogram of random normal values"),
  
  plotOutput("hist"),
  verbatimTextOutput("stat")
 
)

 


server <- function(input, output){
  output$hist <- renderPlot({
   
    hist(rnorm(input$num),main=input$title)
    
    })
  output$stat <- renderTable({summary(rnorm(Seasons_Stats))
})
  
 
}
shinyApp(ui=ui,server=server)
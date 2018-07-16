library(shiny)
playerCSV <- read.csv("Players.csv", sep = ",")
playerdata <- read.csv("player_data.csv", stringsAsFactors = FALSE)
seasonstats <- read.csv("Seasons_Stats.csv", stringsAsFactors = FALSE)
pname <- (playerCSV$Player)

ui <- fluidPage(
  
  tags$style(HTML("
   p{color:green;font-size:20pt;}
   h4{color:blue;}
   
 
                   ")),

  
  navbarPage(title=p("NATIONAL BASKET BALL ASSOCIATION ANALYSIS"),
             
             
               tabPanel("COMPARISONS",
                       
                        # make sidebar with user inputs
                        sidebarPanel(
                          selectInput('player1_select', 
                                      label = 'Player 1 (Bio)', 
                                      choices = sort(unlist(pname)), 
                                      selected = 'Cliff Barker'),
                          
                          selectInput('player2_select', 
                                      label = 'Player 2 (Bio)',
                                      choices = sort(unlist(pname)),
                                      selected = '')
                        
                        ),
                        
                        sidebarPanel(
                          selectInput('p1data', 
                                      label = 'Player 1 (Player Data)', 
                                      choices = sort(unlist(playerdata$name)), 
                                      selected = 'Cliff Barker'),
                          
                          selectInput('p2data', 
                                      label = 'Player 2 (Player Data)',
                                      choices = sort(unlist(playerdata$name)),
                                      selected = '')
                          
                        ),
                        
                        
                        sidebarPanel(
                          selectInput('p1stat', 
                                      label = 'Player 1 (Statistics)', 
                                      choices = sort(unlist(pname)), 
                                      selected = 'Cliff Barker'),
                          
                          selectInput('p2stat', 
                                      label = 'Player 2 (Statistics)',
                                      choices = sort(unlist(pname)),
                                      selected = '')
                          
                        ),
                        
                        # output plots to main panel with tabs to select type
                        mainPanel(
                          tabsetPanel(type = 'tabs',
                                      tabPanel('Player Comparisons',tableOutput('compare'),tableOutput('compare1'),br(),br(),
                                               tableOutput('player1data'),tableOutput('player2data'),br(),br(),
                                               tableOutput('p1st'),tableOutput('p2st')
                                               ),
                                      tabPanel('List of Players',tableOutput('players'), read.csv("Players.csv", header=FALSE,sep=","))
                                      #tabPanel('Reason for Delay', plotOutput('typePlot')),
                                      #tabPanel('Cancellations', plotOutput('cancelPlot'))
                          )
                      
                        
                        
                        )
               ),
               
  br(),
   plotOutput("hist"),
  verbatimTextOutput("stat")
 
))
server <- function(input, output){

    # show flight delays plot in main panel
 output$players = renderTable({playerCSV
    
  })

 
 output$compare = renderTable(
   caption = "BIO Comparisons",
 Playerfilter <- subset(playerCSV,pname == input$player1_select)
 
 
   
   ) 
 output$compare1 = renderTable( 
   
   Player2filter <- subset(playerCSV,pname == input$player2_select)
 )
 
 output$player1data = renderTable(
   caption = "Comparison By Player Data",
   Playerfilter <- subset(playerdata,playerdata$name == input$p1data)
   
 ) 
 output$player2data = renderTable( 
   
   Player2filter <- subset(playerdata,playerdata$name == input$p2data)
 )

 
 output$p1st = renderTable(caption = "Comparison By Season Statistics",
   Playerfilter <- subset(seasonstats,seasonstats$Player == input$p1stat)
   
 ) 
 output$p2st = renderTable( 
   
   
   Player2filter <- subset(seasonstats,seasonstats$Player == input$p2stat)
 )
 
}
shinyApp(ui=ui,server=server)
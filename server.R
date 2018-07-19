library(shiny)
library(shinyjs)
library(shinydashboard)
library(shinyWidgets)
playerCSV <- read.csv("Players.csv", header=TRUE,sep=",")
seasonstats <- read.csv("Seasons_Stats.csv", header=TRUE,sep=",")
playerdata <- read.csv("player_data.csv", header=TRUE,sep=",")
pname <- (playerCSV$Player)

library(sqldf)

shinyServer(function(input, output,session) {
  
 
  db <- dbConnect(SQLite(), dbname="try.sqlite")
  
  
  USER <- reactiveValues(Logged = FALSE)
  
  
  observeEvent(input$login, {
 
    c<-sqldf("SELECT * FROM user ", dbname = "try.sqlite")  
    nrow(c)
    rt<- 0;
    for (row in 1:nrow(c)) {
      username <- c[row, "uname"]
      passwordd  <- c[row, "password"]
      t<- grepl(input$username, username)
      f<- grepl(input$password, passwordd)
      if(t == TRUE & f == TRUE) {
        rt<- rt+1
      }
    }
    if(rt>0){
      USER$Logged <- TRUE
      output$well = renderText("Welcome Mr/Ms.")
      output$valid = renderText(input$username)
    }else{
      output$message = renderText("Sorry username or Password is incorrect")
      
      show("message")
      
      
      delay(10000, hide("message", anim = TRUE, animType = "fade"))
      
    }
    
  
  })
  
  observeEvent(input$register, {
  
    res <- dbSendQuery(my_db$con, 
                       'INSERT INTO my_table VALUES (9.9, 9.9, 9.9, 9.9, "new")')
    
    c<-sqldf("INSERT INTO user Values(nakafu,claire )", dbname = "try.sqlite")  
    nrow(c)
    rt<- 0;
    for (row in 1:nrow(c)) {
      username <- c[row, "uname"]
      passwordd  <- c[row, "password"]
      t<- grepl(input$username, username)
      f<- grepl(input$password, passwordd)
      if(t == TRUE & f == TRUE) {
        rt<- rt+1
      }
    }
    if(rt>0){
      USER$Logged <- TRUE
      output$well = renderText("Welcome Mr/Ms.")
      output$valid = renderText(input$username)
    }else{
      output$message = renderText("Sorry username or Password is incorrect")
      
      show("message")
      
      
      delay(10000, hide("message", anim = TRUE, animType = "fade"))
      
    }
    
    
  })
  
  
  
  
  observeEvent(input$select, {
    if(input$select=="Players"){
      w3=read.csv("Players.csv")
      output$a=renderPlot(hist(w3$height,breaks=10,col = "blue",main = "Histogram showing the distribution of the Height of players",xlab = "Height"))
      output$b=renderPlot(hist(w3$weight,breaks=10,col = "red",main = "Histogram showing the distribution of the weight of players",xlab = "Weight"))
      output$C=renderPlot(boxplot(w3$height,main="A box plot of height of players",ylab="height",col="pink"))
      output$d=renderPlot(boxplot(w3$weight,main="A box plot of weight of players",ylab="weight",col="blue"))
      output$e=renderPlot(plot(w3$height,w3$weight,main = "A scatter plot of weight against height of players",xlab = "Height",ylab = "weight"))
      output$f=renderPlot(qqnorm(w3$weight,main="Normal QQ plot of weight of players",qqline(w3$weight)))
      output$g=renderPlot(qqnorm(w3$height,main="Normal QQ plot of Height of players"))
    }
    if(input$select=="player_data"){
      w4=read.csv("player_data.csv") 
      output$a=renderPlot(plot(w4$year_start,w4$year_end,main = "A scatter plot of year_start against year_end of players",xlab = "year_start",ylab = "year_end"))
    }
    if(input$select=="Seasons_statistics"){
      w5=read.csv("Seasons_Stats.csv") 
      output$a=renderPlot(plot(w5$Year,w5$FG,main = "A scatter plot of year against FG of players in different seasons",xlab = "year",ylab = "FG"))
      output$b=renderPlot(boxplot(w5$FG,main="A box plot of FG of players within different seasons",ylab="FG",col="pink"))
      output$d=renderPlot(plot(w5$Year,w5$PTS,main = "A scatter plot of year against points of players in different seasons",xlab = "year",ylab = "PTS"))
      output$e=renderPlot(plot(w5$Year,w5$AST,main = "A scatter plot of year against Assists of players in different seasons",xlab = "year",ylab = "AST")) 
      output$f=renderPlot(hist(w5$PF,breaks=10,col = "deepskyblue",main = "Histogram showing the distribution of Personal Fouls",xlab = "PF"))
      output$g=renderPlot(plot(w5$Year,w5$PER,main = "A scatter plot of year against Performance Efficiency Rating of players in different seasons",xlab = "year",ylab = "PER"))
    }
    #print(paste0("You have chosen: ", input$select))
  })
  
  
  output$players = renderTable({playerCSV})
  
  
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
  a = reactive({
    seasonstats[, colnames(seasonstats) == input$Choice1]
  })
  b= reactive({
    playerCSV[, colnames(playerCSV) == input$Choice4]
  })
  d = reactive({
    playerdata[, colnames(playerdata) == input$Choice5]
  })
  
  output$Out2 = renderPrint({
    
    summary(a())
  })
  output$Out3 = renderTable({
    return(a())
  })
  
  output$Out6 = renderPrint({
    summary(b())
  })
  output$Out7 = renderTable({
    return(b())
  })
  
  
  output$Out12 = renderPrint({
    summary(d())
  })
  output$Out13 = renderTable({
    return(d())
  })
  
  
 
  output$navPage = renderUI(
    if (!isTRUE(USER$Logged)) {
      navbarPage(title=p("NATIONAL BASKET BALL ASSOCIATION ANALYSIS"),br(),
      fluidRow(column(h4("ALREADY A MEMBER, LOGIN HERE"),width=8, offset = 2,
                      wellPanel(id = 'login',
                                textInput('username', h5('USERNAME')),
                                passwordInput('password', h5('PASSWORD')),
                                div(actionButton('login', 'Log in'), style='text-align: center;')
                      ),
                      textOutput("message"),
                      hr()
      )),
      fluidRow(
        
        
      )
      
      
        
        
      )
      
      
    } else {
      # Sidebar with a slider input for number of bins

      navbarPage(title=p("NATIONAL BASKET BALL ASSOCIATION ANALYSIS"),
                 tabPanel("SUMMARY", 
                          sidebarLayout(
                            sidebarPanel( id="sidebar",
                                          # player
                                          selectInput(
                                            inputId = "Choice4",
                                            "SELECT VARIABLE TO SUMMARIZE IN PLAYER DATASET",
                                            choices = colnames(playerCSV)[2:8],
                                            selected = NULL
                                          ),
                                          
                                          
                                          
                                          br(),br(),
                                          hr(),
                                          # seasons stats
                                          selectInput(
                                            inputId = "Choice1", 
                                            "SELECT VARIABLE TO SUMMARIZE IN THE SEASONS STATS DATASET",
                                            choices = colnames(seasonstats)[2:53],
                                            selected = NULL
                                          ),
                                          
                                          br(),br(),
                                          hr(),
                                          
                                          # player_data
                                          selectInput(
                                            inputId = "Choice5",
                                            "SELECT VARIABLE TO SUMMARIZE IN THE PLAYERDATA DATASET",
                                            choices = colnames(playerdata)[1:8],
                                            selected = NULL
                                          )
                                          
                            ),
                            mainPanel( id = "main",
                                       tabsetPanel(type = 'tabs', id = "summarytabs",
                                                   tabPanel('Players',value = "Choice4",
                                                            verbatimTextOutput("Out6"), id="choice3"
                                                            
                                                   ),
                                                   tabPanel('Season Statistics',value = "Choice1",verbatimTextOutput("Out2"), id="choice3"),
                                                   
                                                   tabPanel('Player Data',value = "Choice5", verbatimTextOutput("Out12"), id="choice3"
                                                            
                                                   )
                                       )
                            )
                          )),
                 tabPanel("PREDICTIONS"
                 ),
                 tabPanel("RELATIONSHIPS",
                          sidebarLayout(
                            sidebarPanel( id="sidebar",
                                          h4("choose the dataset from which you want to find the relationship, then click Finish"),
                                          selectInput("select", p("Choose dataset"), 
                                                      choices = list("player_data" , "Players",
                                                                     "Seasons_statistics" ), selected = 2)
                            ),
                            mainPanel(
                              
                              
                              
                              plotOutput("a"),
                              plotOutput("b"),
                              plotOutput("C"),
                              plotOutput("d"),
                              plotOutput("e"),
                              plotOutput("f"),
                              plotOutput("g"),
                              plotOutput("h")
                            )
                            
                          )
                 ),
                 tabPanel("COMPARISON",
                          # make sidebar with user inputs
                          sidebarPanel( id="sidebar",
                                        selectInput(inputId = 'player1_select', 
                                                    label = 'Player 1 (Bio)', 
                                                    choices = sort(unlist(pname)), 
                                                    selected = 'Cliff Barker'),
                                        
                                        selectInput(inputId = 'player2_select', 
                                                    label = 'Player 2 (Bio)',
                                                    choices = sort(unlist(pname)),
                                                    selected = '')
                          ),
                          sidebarPanel( id="sidebar",
                                        selectInput(inputId = 'p1data', 
                                                    label = 'Player 1 (Player Data)', 
                                                    choices = sort(unlist(playerdata$name)), 
                                                    selected = 'Cliff Barker'),
                                        
                                        selectInput(inputId = 'p2data', 
                                                    label = 'Player 2 (Player Data)',
                                                    choices = sort(unlist(playerdata$name)),
                                                    selected = '')
                          ),
                          sidebarPanel( id="sidebar",
                                        selectInput(inputId = 'p1stat', 
                                                    label = 'Player 1 (Statistics)', 
                                                    choices = sort(unlist(pname)), 
                                                    selected = 'Cliff Barker'),
                                        
                                        selectInput(inputId = 'p2stat', 
                                                    label = 'Player 2 (Statistics)',
                                                    choices = sort(unlist(pname)),
                                                    selected = '')
                          ),
                          
                          # output plots to main panel with tabs to select type
                          mainPanel(
                            tabsetPanel(type = 'tabs',
                                        tabPanel('Player Comparisons', 
                                                 tabsetPanel(type = 'tabs', id ="comparison_tabs",
                                                             tabPanel('Player Bio',value="biolink",tableOutput('compare'),tableOutput('compare1'),br(),br()
                                                                      
                                                             ),
                                                             tabPanel('Player Data', value = "pdatalink",tableOutput('player1data'),tableOutput('player2data'),br(),br()
                                                                      
                                                             ),
                                                             
                                                             tabPanel('Player Statistics',value = "pstatlink",tableOutput('p1st'),tableOutput('p2st'),br(),br()
                                                                      
                                                             ))
                                        ),
                                        tabPanel('List of Players',tableOutput('players'), read.csv("Players.csv", header=FALSE,sep=","))
                                        
                            )
                          )
                          
                          
                 ),
                 tabPanel("HELP",

                   
                   tags$div(
                     h3("Find help here"),

                     hr(),
                     p("Once you access the system, you are supposed to first login with the username and the password 
                       if you are a new member, you need to first signup with us. As long as you are able to login, you are able to access 
                       all the services, such as making summaries about the NBA data, making predictions, finding relationships,making comparisons,
                       obtaining the help and also logout."),
                     h4("HOW TO MAKE SUMMARIES ABOUT THE NBA DATA"),
                     p("There are 3 NBA datasets provided, with each having a number of different attributes. "),
                     p("The first dataset is called player, this contains player details that is to say player name, 
                       height, weight,collage, born, birth city and birth state."),
                     p("Once the your select any of the attributes in the player dataset the application should be able
                       to display a list of summaries about the attribute chosen."),
                     p("The second dataset is called the Season statistics this contains details about the previous seasons
                       in the NBA. This contains a number of attributes such as year, player, Pos(),Age, Tm(),G(),Gs(),MP(), PER(),TS(),
                       X3PAr(),FTr(), ORB(),DRB(),TRB(),AST(),STL(),BLK(),TOV(), USG(),blanl(),OWS(),DWS(),WS(),WS.48(),blank2(),OBPM(),
                       DBPM(),BPM(),VORP(),FG(),FGA(),FG.(),X3P(),X3PA(),X3P.(),X2P(),X2PA(),X2p.(),eFG(),FT(),FTA(),FT.()."),
                     p("The third dataset is called (player data) dataset this also contains details about the players.
                       the different attributes contained in the dataset are name, year start, year end, position, height, weight,
                       birth date and the college."),
                     p("One you select any of the attributes in any of the 3 dataset, you are able to view a summary about that
                       particular attribute."),
                     h4(strong("HOW TO MAKE PREDICTIONS BASING ON THE NBA DATA")),
                     h4(strong("HOW TO OBTAIN RELATIONSHIPS ABOUT THE NBA DATA")),
                     p("Once you access the relationships page, three options are provided with each option being a dataset.
                       You can either choose player_data, players or season statistics.Once you select player_data, the system will be able to 
                       plot a scatter plot of year_start against year_end. If you select the (player) dataset, the system will be able to plot the 
                       a number of graphs. That is to say, the histogram about the different attributes,box plots, scatter plots and normal QQ plots."),
                     h4(strong("HOW TO MAKE COMPARISONS BETWEEN THE DIFFERENT PLAYERS IN THE NBA DATASETS")),
                     p("Here the comparison can be made between two players. Player(Bio) displays all the players details in the .player dataset, in
                       this case you are suppossed to choose the two players the you may want to compare. The same applies to the rest of the dataset
                       that is to say (player data) and season stats"),
                     p("Once you are done you can logout this take you to the login page."),
                     h5("Was this article helpful if not you can contact us at: "),
                     hr()

                     
                     
              
 

                     )
                   
                   
                            
                            
                          ),
                 
                          
                          
                 
                 tabPanel("LOGOUT"
                          
                          
                 ),
                 
                 
                 
                 
                 
                 footer = h4("CopyRight @ Group21 (2018)")    
                 
                 
      )
      
      
      
      
    }
    
    
  )
  
  
  
  
  
  
  
  
  
  
  
  
  
  


  
  
  
  
  
})

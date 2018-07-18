library(shiny)
library(shinyjs)
library(shinydashboard)
library(shinyWidgets)
library(data.table)

playerCSV <- read.csv("Players.csv", header=TRUE,sep=",")
seasonstats <- read.csv("Seasons_Stats.csv", header=TRUE,sep=",")
playerdata <- read.csv("player_data.csv", header=TRUE,sep=",")
pname <- (playerCSV$Player)

library(sqldf)
nba5  <- read.csv("Seasons_Stats.csv")
nba4= nba5[nba5$Year %in% 2017,]

shinyServer(function(input, output,session) {
  
  observeEvent(input$home, {
    
    require(dplyr)
    
    nba5  <- read.csv("Seasons_Stats.csv")
    nba5= nba5[nba5$Year %in% 2017,]
    mydata = nba5%>% select(Year,Player,Tm,X,OWS,DWS,WS,PER,AST,PTS)%>% 
      filter(Year=="2017",Tm==input$home)%>% 
      mutate(spct=(OWS^2+WS^2+DWS^2)/(PER^2),expScore=round(spct*(PTS+AST)),diff=PTS-expScore)
    
    mydata[is.na(mydata)] <- 0
    PlayerToScore = mydata[ order(mydata$expScore, decreasing=TRUE), ]  
    Scorer=head(PlayerToScore)
    scorerData=Scorer%>% select(Player,Tm,expScore)
    output$i=renderTable(scorerData)
    #scorerData
    #output$a=renderPlot(ggplot(mydata,aes(expScore,PTS))+ geom_point()+stat_smooth(method="lm"))
    
    mod= lm(PTS ~ expScore,data=mydata )
    
  })
  
  observeEvent(input$away, {
    require(Lahman)
    require(dplyr)
    require(ggplot2)
    nba5  <- read.csv("Seasons_Stats.csv")
    nba5= nba5[nba5$Year %in% 2017,]
    mydata = nba5%>% select(Year,Player,Tm,X,OWS,DWS,WS,PER,AST,PTS)%>% 
      filter(Year=="2017",Tm==input$away)%>% 
      mutate(spct=(OWS^2+WS^2+DWS^2)/(PER^2),expScore=round(spct*(PTS+AST)),diff=PTS-expScore)
    
    mydata[is.na(mydata)] <- 0
    PlayerToScore = mydata[ order(mydata$expScore, decreasing=TRUE), ]  
    Scorer=head(PlayerToScore)
    scorerData=Scorer%>% select(Player,Tm,expScore)
    output$j=renderTable(scorerData)
    #scorerData
    #output$a=renderPlot(ggplot(mydata,aes(expScore,PTS))+ geom_point()+stat_smooth(method="lm"))
    
    mod= lm(PTS ~ expScore,data=mydata )
    
    
  })
  
  
  
 # setting up connections to the sqlite database
  db <- dbConnect(SQLite(), dbname="basketball.sqlite")
  
  
  USER <- reactiveValues(Logged = FALSE)
  
  #checking whether the username and the password exist in the database
  observeEvent(input$login, {
    if(input$username != "" && input$password != "" ){
    c<-sqldf("SELECT * FROM userDetails ", dbname = "basketball.sqlite")  
    nrow(c)
    rt<- 0;
    for (row in 1:nrow(c)) {
      username <- c[row, "userName"]
      passwordd  <- c[row, "password"]
      t<- grepl(input$username, username)
      f<- grepl(input$password, passwordd)
      if(t == TRUE && f == TRUE) {
        rt<- rt+1
      }
    }
    
    if(rt>0){
      USER$Logged <- TRUE

      output$well = renderText("You are logged in as")
      output$valid = renderText(input$username)
      
    }else{
      output$message = renderText("Sorry username or Password is incorrect")
      
      show("message")
      
      
      delay(30, hide("message", anim = TRUE, animType = "fade"))
      
    }
    }
  
  })
  
  # making plots of different categories
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

  })
  
  # making comparisons within different datasets
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
  
  # making summaries
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

  
  output$Out6 = renderPrint({
    summary(b())
  })

  
  
  output$Out12 = renderPrint({
    summary(d())
  })
  
  #logout function
  observeEvent(input$logout, {
    USER$Logged <- FALSE
  })


  
  
 # The interface to display once the user has just accessed the system
  output$navPage = renderUI(
    if (!isTRUE(USER$Logged)) {
      navbarPage(title=p("NATIONAL BASKET BALL ASSOCIATION ANALYSIS"),
                 
      fluidRow(column(h4("Already a member, please login here"),width=8, offset = 2,
                      hr(),

                      wellPanel(id = 'login2',
                                textInput('username', h5('USERNAME'),placeholder = "enter your username"),
                                passwordInput('password', h5('PASSWORD'),placeholder ="enter your password"),
                                div(actionButton('login', 'Login'), style='text-align: center;')
                      ),
                      textOutput("message"),
                      hr(),
                     div( h4("CopyRight @ Group21 (2018)"),
                          style='background-color:black;width:100%;height:100%'
                          )
                    
      )
      
      
      )
      
      
        
        
      )
      
      
    } 
    #The interface to display once the user has submitted the correct credentials
    else {
      # Sidebar with a slider input for number of bins

      navbarPage(title=h4("NATIONAL BASKET BALL ASSOCIATION ANALYSIS"),
                 

                 tabPanel("SUMMARY", 
                          
                          sidebarLayout(
                            
                            sidebarPanel( id="sidebar",
                                          img(src='user.png', align = "right",width="75",height="50"),
                                          span(textOutput("well"),textOutput("valid") ),
                                          hr(),
                                          

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
                                                   tabPanel(h5('Players'),value = "Choice4",
                                                            verbatimTextOutput("Out6"), id="choice3"
                                                            
                                                   ),
                                                   tabPanel(h5('Season Statistics'),value = "Choice1",verbatimTextOutput("Out2"), id="choice3"),
                                                   
                                                   tabPanel(h5('Player Data'),value = "Choice5", verbatimTextOutput("Out12"), id="choice3"
                                                            
                                                   )
                                       )
                            )
                          )),
                 tabPanel("PREDICTIONS",
                          sidebarLayout(
                            sidebarPanel( id="sidebar",
                              h3("PREDICTING THE MOST LIKELY PLAYERS TO SCORE"),
                              selectInput("home", p("Home Team"), 
                                          choices = nba4$Tm, selected = " "),
                              selectInput("away", p("Away Team"), 
                                          choices = nba4$Tm, selected = " ")
                            ),
                            mainPanel(
                              tabsetPanel( id = "predictiontabs",
                                tabPanel(h5('HOME'),tableOutput("i"),value="choicex"),
                                tabPanel(h5('AWAY'),tableOutput("j"),value="choicey")
                              )
                              

                              
                            )
                            
                          )
                 ),
                 tabPanel("RELATIONSHIPS",
                          sidebarLayout(
                            sidebarPanel( id="sidebar",
                                          
                                          
                                          p("choose the dataset to plot ", style="font-size:14pt"),
                                          selectInput("select", "", 
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
                          

                          mainPanel(
                            tabsetPanel(type = 'tabs',
                                        tabPanel(h5('Player Comparisons'), 
                                                 tabsetPanel(type = 'tabs', id ="comparison_tabs",
                                                             tabPanel(h5('Player Bio'),value="biolink",tableOutput('compare'),tableOutput('compare1'),br(),br()
                                                                      
                                                             ),
                                                             tabPanel(h5('Player Data'), value = "pdatalink",tableOutput('player1data'),tableOutput('player2data'),br(),br()
                                                                      
                                                             ),
                                                             
                                                             tabPanel(h5('Player Statistics'),value = "pstatlink",tableOutput('p1st'),tableOutput('p2st'),br(),br()
                                                                      
                                                             ))
                                        ),
                                        tabPanel(h5('List of Players'),tableOutput('players'), read.csv("Players.csv", header=FALSE,sep=","))
                                        
                            )
                          )
                          
                          
                 ),
                 tabPanel("HELP",

                   
                   tags$div(
                     h3("Here are the things you need to know about the System "),

                     hr(),
                     p("Once you access the system, you are supposed to first login with the username and the password 
                       if you are a new member, you need to first signup with us. As long as you are able to login, you are able to access 
                       all the services, such as making summaries about the NBA data, making predictions, finding relationships,making comparisons,
                       obtaining the help and also logout."),
                     h5("HOW TO MAKE SUMMARIES ABOUT THE NBA DATA",style="font-size:15pt"),
                     p("There are 3 NBA datasets provided, with each having a number of different attributes.
                     The first dataset is called player, this contains player details that is to say player name, 
                       height, weight,collage, born, birth city and birth state.Once the your select any of the attributes in the player dataset the application should be able
                       to display a list of summaries about the attribute chosen.The second dataset is called the Season statistics this contains details about the previous seasons
                       in the NBA. This contains a number of attributes such as year, player, Pos(position),Age, Tm(Team),G(Games),Gs(Games Started),
                     MP(minutes played), PER(player efficience rating),TS( True Shooting),
                       X3PAr(),FTr(free throw rate), ORB(offensive rebound),DRB(defensive rebound),TRB(Total rebound percentage),AST(Assists),STL(Steals),BLK(Blocked Shots),TOV(Turnover ), USG(Usage),
                      blanl(),OWS(offensive win share),DWS(defensive win share),WS(win share),WS.48(win share),blank2(),OBPM(offensive box plus minus),
                       DBPM(Defensive box plus minus),BPM(Box plus minus),VORP(Value over replacement player),FG(Field Goal),FGA(Field Goals Attempted),FG.(field goals),X3P(3- pointers),X3PA(3- pointer),
X3P.(3-pointers),X2P(2- pointers),X2PA(2-pointers),X2p.(2-ponters),
                         eFG(Effective Field Goal Percentage. 
                       This statistic adjusts for value of a 3-point field goal relative to a 2-point field goal. ((FGM + (0.5 * 3PM)) / FGA),
                       FT(Free Throw),FTA(Free Throws Attempted),FT.(free throws)."),
                     p("The third dataset is called (player data) dataset this also contains details about the players.
                       the different attributes contained in the dataset are name, year start, year end, position, height, weight,
                       birth date and the college."),
                
                     p("One you select any of the attributes in any of the 3 dataset, you are able to view a summary about that
                       particular attribute."),
                     h5("HOW TO MAKE PREDICTIONS BASING ON THE NBA DATA",style="font-size:15pt"),
                     p("Inorder to make predictions about the most likely player to score in the given team, you need to select between the
                       the home and away team. The system will be able to display those players for you"),
                     h5("HOW TO OBTAIN RELATIONSHIPS ABOUT THE NBA DATA",style="font-size:15pt"),
                     p("Once you access the relationships page, three options are provided with each option being a dataset.
                       You can either choose player_data, players or season statistics.Once you select player_data, the system will be able to 
                       plot a scatter plot of year_start against year_end. If you select the (player) dataset, the system will be able to plot the 
                       a number of graphs. That is to say, the histogram about the different attributes,box plots, scatter plots and normal QQ plots."),
                     h5("HOW TO MAKE COMPARISONS BETWEEN THE DIFFERENT PLAYERS IN THE NBA DATASETS",style="font-size:15pt"),
                     p("Here the comparison can be made between two players. Player(Bio) displays all the players details in the player dataset, in
                       this case you are suppossed to choose the two players you may want to compare. The same applies to the rest of the dataset
                       that is to say (player data) and season stats"),
                     p("Once you are done you can logout this take you to the login page."),
                     strong("Was this article helpful if not you can contact us at our email: group21@gmail.com"),
                     hr()


                     
                     
              
 

                     )
                   
                   
                            
                            
                          ),
                 
                          

                 tabPanel(

                   title = actionButton("logout", "Logout")
                   
                   
                 ),
                 
                 
                 
                 
                 
                 footer=(h4("CopyRight @ Group21 (2018)") ) 
                
                 
      )
      
      
      
      
    }
    
    
  )
  
  # changing the Summary tabs
  observeEvent(input$Choice4,{
    updateTabsetPanel(session, "summarytabs", selected = "Choice4") 
    
  }
    
  )
  
  observeEvent(input$Choice1,{
    updateTabsetPanel(session, "summarytabs", selected = "Choice1") 
    
  }
  
  
  
  )
  
  
  observeEvent(input$Choice5,{
    updateTabsetPanel(session, "summarytabs", selected = "Choice5") 
    
  }
  
  )
  
  # changing the comparison tabs
  observeEvent((input$player1_select),{

      updateTabsetPanel(session, "comparison_tabs", selected = "biolink")
    
  })
  observeEvent((input$player2_select),{
    
    updateTabsetPanel(session, "comparison_tabs", selected = "biolink")
    
  })
  
  
  
  observeEvent((input$p1data ),{
    # use tabsetPanel 'id' argument to change tabs

      updateTabsetPanel(session, "comparison_tabs", selected = "pdatalink")
    
  })
  observeEvent((input$p2data ),{
    # use tabsetPanel 'id' argument to change tabs
    
    updateTabsetPanel(session, "comparison_tabs", selected = "pdatalink")
    
  })
  
observeEvent((input$p1stat),{
    

      updateTabsetPanel(session, "comparison_tabs", selected = "pstatlink")
  })
  observeEvent((input$p2stat),{
    
    
    updateTabsetPanel(session, "comparison_tabs", selected = "pstatlink")
  })
  
  # changing the prediction tabpanels
  observeEvent(input$home,{
    updateTabsetPanel(session, "predictiontabs", selected = "choicex") 
    
  }
  
  )
  
  observeEvent(input$away,{
    updateTabsetPanel(session, "predictiontabs", selected = "choicey") 
    
  }
  
  )
  
  
})


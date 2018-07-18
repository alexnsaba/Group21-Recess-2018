library(shiny)
nba5  <- read.csv("Seasons_Stats.csv")
nba4= nba5[nba5$Year %in% 2017,]
# Define UI ----
ui <- fluidPage(
  tags$style(HTML("
                  @import url('//fonts.googleapis.com/css?family=Lobster|Cabin:400,700');
                  
                  body {
                  font-family: 'Times new Roman';
                  line-height: 1.1;
                  color:white;
                  background-color:#037881;
                  }
                  
                  ")),
  h1("NATIONAL BASKETBALL ASSOCIATION ANALYSIS SYSTEM"),
  br(),
  h3("PREDICTING THE PLAYERS TO SCORE"),
  selectInput("home", p("Home Team"), 
              choices = nba4$Tm, selected = " "),
  selectInput("away", p("Away Team"), 
              choices = nba4$Tm, selected = " "),
  tableOutput("a"),
  tableOutput("b")
  )
# Define server logic ----
server <- function(input, output) {
  #processing perfomance predictions
  observeEvent(input$home, {
    require(Lahman)
    require(dplyr)
    require(ggplot2)
    nba5  <- read.csv("Seasons_Stats.csv")
    nba5= nba5[nba5$Year %in% 2017,]
    mydata = nba5%>% select(Year,Player,Tm,X,OWS,DWS,WS,PER,AST,PTS)%>% 
      filter(Year=="2017",Tm==input$home)%>% 
      mutate(spct=(OWS^2+WS^2+DWS^2)/(PER^2),expScore=round(spct*(PTS+AST)),diff=PTS-expScore)
    
    mydata[is.na(mydata)] <- 0
    PlayerToScore = mydata[ order(mydata$expScore, decreasing=TRUE), ]  
    Scorer=head(PlayerToScore)
    scorerData=Scorer%>% select(Player,Tm,expScore)
    output$a=renderTable(scorerData)
    #scorerData
    #output$a=renderPlot(ggplot(mydata,aes(expScore,PTS))+ geom_point()+stat_smooth(method="lm"))
    
    mod= lm(PTS ~ expScore,data=mydata )
    
  })
  #processing position predictions
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
    output$b=renderTable(scorerData)
    #scorerData
    #output$a=renderPlot(ggplot(mydata,aes(expScore,PTS))+ geom_point()+stat_smooth(method="lm"))
    
    mod= lm(PTS ~ expScore,data=mydata )
    
    
  })
  
}


# Run the app ----
shinyApp(ui = ui, server = server)

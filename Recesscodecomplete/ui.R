#library(shinyjs)
seasonstats <- read.csv("Seasons_Stats.csv", header=TRUE,sep=",")
playerCSV <- read.csv("Players.csv", header=TRUE,sep=",")
playerdata <- read.csv("player_data.csv", header=TRUE,sep=",")
pname <- (playerCSV$Player)


shinyUI(fluidPage(
  
  
  tags$style(type = 'text/css', 
             '.navbar { background-color: black;color: white;}',
             '.navbar-default .navbar-brand{color: white;}',
             
             'body{ color: white; background-color:#008080;font-family:Sans-Serif;}',
             '#sidebar {
             background-color: #333333;
             }',
            ' #choice3{ background-color:white}',
            'h5{ color:black}',
            '#action{color:blue}',
            '#logout{color:white;float:right;background-color:purple}',
            'h4{text-align:center;font-size:15pt}',
            '#login{color:white;background-color:black }',
            'span{font-size:15pt}'
           
            
            
  ),
  #useShinyjs(),
  
 
  
  uiOutput("navPage")
))







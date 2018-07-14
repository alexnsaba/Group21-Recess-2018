#library(shinyjs)
seasonstats <- read.csv("Seasons_Stats.csv", header=TRUE,sep=",")
playerCSV <- read.csv("Players.csv", header=TRUE,sep=",")
playerdata <- read.csv("player_data.csv", header=TRUE,sep=",")
pname <- (playerCSV$Player)

shinyUI(fluidPage(
  
  
  tags$style(type = 'text/css', 
             '.navbar { background-color: black;color: white;}',
             '.navbar-default .navbar-brand{color: white;}',
             
             'body{ color: white; background-color:#008080}',
             '#sidebar {
             background-color: #333333;
             }',
            ' #choice3{ background-color:white}',
            'h5{ color:red}'
  ),
  #useShinyjs(),
  
 
  
  uiOutput("navPage")
))







library(shiny)

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
  h2("choose the dataset from which you want to find the relationship, then click Finish"),
  selectInput("select", p("Choose dataset"), 
              choices = list("player_data" , "Players",
                             "Seasons_statistics" ), selected = 2),
  submitButton("Finish"),
  
  plotOutput("a"),
  plotOutput("b"),
  plotOutput("C"),
  plotOutput("d"),
  plotOutput("e"),
  plotOutput("f"),
  plotOutput("g"),
  plotOutput("h")
  )
# Define server logic ----
server <- function(input, output) {
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
  
}


# Run the app ----
shinyApp(ui = ui, server = server)
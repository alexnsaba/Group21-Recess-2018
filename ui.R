ui <- fluidPage(
  
  
  
  
  tags$head(
   
    
    
    
    tags$style(HTML("
                    @import url('//fonts.googleapis.com/css?family=Lobster|Cabin:400,700');
                    
                    h1 {
                    font-family: 'Lobster', cursive;
                    font-weight: 500;
                    line-height: 1.1;
                    color:white;
                    }
                    
                    body {
                    background-color:#20B2AA;
                    }
     
                   footer{

                   background-color:black;
                   }  

                    column()
                    
                    
                    "))
    ),
  
  
  pageWithSidebar(
    headerPanel(''),
    sidebarPanel(		
      getTool("tool")
    ),
    mainPanel(
      textOutput("char")
    )
  )
  
  
  
 

    )

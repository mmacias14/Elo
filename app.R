#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(DT)

teams <- c("Air Force","Army","Navy")

ui <- fluidPage(
   
   # App title
   titlePanel("College CrossFit Rankings"),
   
   #User Inputs
   sidebarLayout(
      sidebarPanel(
        selectInput(inputId = "team1", label = "Team 1", choices = teams),
        selectInput(inputId = "team2", label = "Team 2", choices = teams),
        selectInput(inputId = "winner", label = "Winner", choices = teams),
        actionButton("actionButton", "Input Result")
        ), 
   # Show the data table
      mainPanel(
        h1("ELO Rankings"),
        dataTableOutput(outputId = "ELORankings"),
        h1("Matches Recorded"),
        dataTableOutput(outputId = "gameLog")
      )
   )
)

# Define server logic required to draw a histogram
server <- function(input, output){

  rankingsDF <- readRDS("ELORanking.rds")
  gamesDF <- readRDS("gameLog.rds")
  
  
  output$ELORankings <- DT::renderDataTable({
    rankingsDF <- rankingsDF[order(rankingsDF[,1]),]
    rankingsDF
  })
  
  output$gameLog <- DT::renderDataTable({
    
    if(nrow(gamesDF) > 0){
      colnames(gamesDF) <- c("Team 1", "Team 2", "Pre ELO 1", "Pre ELO 2", "Prob Win 1", "Prob Win 2", "Winner", "Post ELO 1", "Post ELO 2")
    }
    else{}
    
    gamesDF
    
  })
  
  observeEvent(
    input$actionButton, {
      
      validate(
        need(input$team1 != input$team2, ""))
      
      validate(
        need(input$winner == input$team1 | input$winner == input$team2, ""))
      
      rankingsDF <- readRDS("ELORanking.rds")
      gamesDF <- readRDS("gameLog.rds")
      
      K <- 16
      
      i <- match(input$team1, rankingsDF[,2])
      j <- match(input$team2, rankingsDF[,2])
      teamWin <- ifelse(input$winner == input$team1,1,0)
      
      preELO1 <- rankingsDF[i,3]
      preELO2 <- rankingsDF[j,3]
      probELO1 <- round(1/(1+10^((preELO2-preELO1)/400)),2)
      probELO2 <- round(1/(1+10^((preELO1-preELO2)/400)),2)
      
      rankingsDF[i,3] <- round(preELO1 + K*(teamWin-probELO1),1)
      rankingsDF[j,3] <- round(preELO2 + K*((1-teamWin)-probELO2),1)
      
      game <- cbind(input$team1,input$team2,preELO1,preELO2,probELO1,probELO2,input$winner,rankingsDF[i,3],rankingsDF[j,3])
      gamesDF <- rbind(gamesDF,game)
      
      ranking <- rank(-rankingsDF[,3], ties.method = "first")
      rankingsDF[,1] <- ranking
      
      rownames(rankingsDF) <- ranking
      
      output$ELORankings <- DT::renderDataTable({
        rankingsDF <- rankingsDF[order(rankingsDF[,1]),]
        rankingsDF
      })
      
      output$gameLog <- DT::renderDataTable({
        colnames(gamesDF) <- c("Team 1", "Team 2", "Pre ELO 1", "Pre ELO 2", "Prob Win 1", "Prob Win 2", "Winner", "Post ELO 1", "Post ELO 2")
        gamesDF
      })
      
      saveRDS(rankingsDF,"ELORanking.rds")
      
      saveRDS(gamesDF,"gameLog.rds")

  })
  

}

# Run the application 
shinyApp(ui = ui, server = server)


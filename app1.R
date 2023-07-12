#app1.R
library(haven)
library(tidyr)
library(dplyr)
library(lubridate)
library(DT)
library(shiny)
library(ggplot2)
library(gridExtra)

mat <- matrix(NA, nrow = 10, ncol = 3)
tr <- data.frame(mat)

for (i in seq(1,10)){
  
  tr[[i,1]] <- ifelse(i%%2, 'Cohort X','Cohort Y')
  tr[[i,2]] <- i
  tr[[i,3]] <- ifelse(i%%3, (-1)*(i+30), (-1)*(i+60))
}

tr$COHORT <- tr$X1
tr$USUBJID <- tr$X2
tr$bestpchg <- tr$X3


tr <- tr[c("USUBJID","bestpchg", "COHORT")]

tr$USUBJID <- factor(tr$USUBJID, 
                     levels = tr$USUBJID[order(tr$bestpchg, decreasing = TRUE)])
tr <- tr[order(-tr$bestpchg),]
tr1 <- tr[(tr$COHORT=='Cohort X'),]
tr2 <- tr[(tr$COHORT=='Cohort Y'),]
ui <- fluidPage( 
  mainPanel(plotOutput("plotgraph")
  ))
server <- function(input, output) {
  
  myPlot1 <- ggplot(tr1, aes( x = USUBJID, y = bestpchg)) + 
    labs(title = "Waterfall plot for Cohort X", 
         x = "Patient", y = "Best Percent Change from Baseline in Sum of 
Diameters") +
    theme(axis.text.x = element_blank()) +
    geom_col( width = 0.9) +
    ylim(-80,0)
  
  myPlot2 <- ggplot(tr2, aes( x = USUBJID, y = bestpchg)) + 
    labs(title = "Waterfall plot for Cohort Y", 
         x = "Patient", y = "Best Percent Change from Baseline in Sum of 
Diameters") +
    theme(axis.text.x = element_blank()) +
    geom_col( width = 0.9) +
    ylim(-80, 0)
  
  
  output$plotgraph <- renderPlot({ 
    
    ptlist <- list(myPlot1,myPlot2)
    grid.arrange(grobs=ptlist,ncol=2, nrow=1)
  })
  
}
shinyApp(ui=ui, server=server)

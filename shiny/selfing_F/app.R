library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  titlePanel("Inbreeding & F"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      sliderInput("f",
                  "Initial inbreeding (F0):",
                  min = 0,
                  max = 1,
                  step = 0.01,
                  value = 0.45),
      sliderInput("selfing",
                  "Selfing rate (s):",
                  min = 0,
                  max = 1,
                  step = 0.01,
                  value = 0.5),
      sliderInput("ngen",
                  "Number of Total Generations (T):",
                  min = 2,
                  max = 200,
                  step = 1,
                  value = 50)
    ),
    
    
    # Show a plot of the generated distribution
    mainPanel(
      plotOutput("genotypes")
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  require(ggplot2)
  
  output$genotypes <- renderPlot({
    
    f0 <- input$f
    s <- input$selfing
    ngen <- input$ngen
    
    df <- data.frame(T=0:ngen,F=NA)
    df$F[1] <- f0
    for( i in 2:(ngen+1)){
      t <- df$T[i]
      F <- (s/(2-s))*(1 - (s^t)/2) + f0*(s/2)^t
      df$F[i] <- F
    }
    p <- ggplot( df, aes(x=T,y=F) ) 
    p <- p + geom_line() + geom_point(size=2) + ylim(c(0,1)) 
    p <- p + theme_bw(base_size = 14) + xlab("Generation (t)") + ylab("Inbreeding (F)")
    p
  })
}

# Run the application 
shinyApp(ui = ui, server = server)


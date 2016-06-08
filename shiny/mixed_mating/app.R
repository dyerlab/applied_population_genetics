library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  titlePanel("Mixed Mating Model"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      sliderInput("freq",
                  "Allele Frequency (p):",
                  min = 0,
                  max = 1,
                  step = 0.01,
                  value = 0.45),
      sliderInput("selfing",
                  "Population selfing rate (s):",
                  min = 0,
                  max = 1,
                  step = 0.01,
                  value = 0.5)
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
    
    p <- input$freq
    q <- 1-p
    s <- input$selfing
    fAA <- fAB <- fBB <- rep(NA,20)
    fAA[1] <- p^2
    fAB[1] <- 2*p*q
    fBB[1] <- q^2
    
    for( i in 2:20) {
      fAA[i] <- s*(fAA[i-1] + fAB[i-1]/4) + (1-s)*p^2
      fAB[i] <- s*fAB[i-1]/2 + (1-s)*2*p*q
      fBB[i] <- s*(fBB[i-1] + fAB[i-1]/4) + (1-s)*q^2
    }
    df <- data.frame(Genotype=rep(c("AA","AB","BB"),each=20), 
                 Freq=c(fAA,fAB,fBB),
                 Generations=rep( 1:20,times=3) )
    p <- ggplot( df, aes(x=Generations,y=Freq,color=Genotype)) 
    p <- p + geom_line() + geom_point(size=2) + ylim(c(0,1)) 
    p <- p + theme_bw(base_size = 14)
    p
  })
}

# Run the application 
shinyApp(ui = ui, server = server)


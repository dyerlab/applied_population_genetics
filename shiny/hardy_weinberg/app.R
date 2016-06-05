library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  titlePanel("Expected Genotype Frequencies"),
  tags$p("2-Allele Locus at Hardy-Weinberg Equilibrium"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      sliderInput("p",
                  "Frequency of the 'A' Allele:",
                  min = 0,
                  max = 1,
                  step = 0.01,
                  value = 0.42)
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      plotOutput("genotype")
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  require(ggplot2)
  theme_set( theme_bw(base_size=16) )
  
  output$genotype <- renderPlot({
    p <- input$p
    q <- 1-p
    P <- p^2
    Q <- 2*p*q
    R <- q^2
    df <- data.frame( Genotype=factor(c("AA","AB","BB"),ordered=TRUE,levels=c("AA","AB","BB")), Frequency=c(P,Q,R))
    ggplot(df, aes(x=Genotype,y=Frequency)) + geom_bar( stat="identity", fill="#3182bd") +
      ylab("Expected Frequency")
  })
}

# Run the application 
shinyApp(ui = ui, server = server)


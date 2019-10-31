# Shiny App for estimating parent offspring regression

library(shiny);library(ggthemes);library(patchwork);library(cowplot);library(ggbeeswarm);library(tidyverse)


# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  titlePanel("Evolving Crickets - Estimating Heritability"),
  
  # Sidebar with a slider input for heritability value
  sidebarLayout(
    sidebarPanel(
      sliderInput("h2.sim",
                  "Heritability of calling rate:",
                  min = 0,
                  max = 1,
                  value = .4)
    ),
    
    # Main panel showing parent-offspring regression
    mainPanel(
      tabPanel('Parent-offspring regression',
               plotOutput("FatherOffspringPlot"),
               br(),
               h4("Linear regression values:", style = "color:blue"),
               tableOutput("FatherOffspringReg")
      )
      )
    )
  )


# Define server logic required to draw a histogram
data.maker <- function(noffs=20, 
                       nsire=20, 
                       mean.phen=15,
                       #sd.blup=1,
                       h2=0.4, 
                       seed=1 ) {
  set.seed(seed)
  
  # generate fathers breeding values
  father.blups=rnorm(nsire,0,sqrt(h2))
  father.val=mean.phen+father.blups+rnorm(nsire,0,sqrt(1-h2))
  
  # generate offspring phenotypic values 
  offs.val=matrix(NA, nsire, noffs)
  for (i in 1:nsire) {
    offs.val[i,]=as.numeric(.5*father.blups[i]+rnorm(noffs,0,sqrt(1-h2)/2)+mean.phen)
  }
  
  x=data.frame(cbind(offs.val,father.val))
  x$father.id=factor(factor(seq(1,nsire,1)))
  
  x = gather(x, offs.id, offs.val, 1:noffs, factor_key=TRUE)
  x = arrange(x, father.id)
  x
}


# df=data.maker()


server <- function(input, output) {
  
  #input <- list(rseed=1)
  
  #df=data.maker(h2=input$h2.sim)
  
  #df <- reactive({
  # data.maker(h2=input$h2.sim)
  #})
  
  #df=data.maker(h2=input$h2.sim)
  
  # Generate data and plot parent offspring regression
  output$FatherOffspringPlot <- renderPlot({
    
    df=data.maker(h2=input$h2.sim)
    
    # histogram of variance components
    a=ggplot(data=data.frame(val=c(input$h2.sim,1-input$h2.sim),
                           compo=factor(c("Genetic variance",
                                          "Environmental variance")),
                           x=rep(1,2)), 
           aes(x=x, y=val, fill=compo)) +
      geom_bar(stat="identity", colour="black") + scale_fill_wsj() + theme_test() +
      xlab("") + ylab("Proportion of Variance") +
      theme(axis.ticks.x = element_blank(), axis.text.x = element_blank(),
            legend.title=element_blank(),
            axis.title = element_text(face="bold", size=16),
            axis.text = element_text(size=14),
            aspect.ratio=1)
    
    # draw the parent-offspring regression with the specified heritability
    b=ggplot(df, aes(y=offs.val, x=father.val)) + geom_beeswarm(aes(color=father.id, size=3, alpha =.5)) + 
      scale_color_pander() + theme_test() +  geom_smooth(method = "lm", se=F) + 
      xlab("Father calling rate (nb. chirps/s)") + ylab("Offspring calling rate (nb. chirps/s)") +
      coord_fixed() + #xlim(12,17) + ylim(12,25) +
      #scale_y_continuous(limits = c(10,25))  + 
      #scale_x_continuous(limits = c(10,25)) +
      theme(legend.position = "none",
            axis.title = element_text(face="bold", size=16),
            axis.text = element_text(size=14),
            aspect.ratio=1)
    a+b+plot_layout(ncol = 2, widths = c(1,2))
      })
  
  output$FatherOffspringReg <- renderTable({
    results <- summary(lm(offs.val~father.val,data.maker(h2=input$h2.sim)))
    data.frame(Intercept=results$coefficients[1],
               Slope=results$coefficients[2])#,
               #Heritability=2*results$coefficients[2])
  })
}
  
  

# Run the application 
shinyApp(ui = ui, server = server)


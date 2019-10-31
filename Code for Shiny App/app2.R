# Shiny App for estimating response to selection
library(shiny);library(ggthemes);library(patchwork);library(cowplot);library(ggbeeswarm);library(tidyverse)


# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  titlePanel("Evolving Crickets - Response to Selection"),
  
  # Sidebar with a slider input for heritability value
  sidebarLayout(
    sidebarPanel(
      sliderInput("herit.slider",
                  "Heritability of Calling Rate:",
                  min = 0,
                  max = 1,
                  value = .4),
      
      sliderInput("selection.gradient.slider",
                  "Selection Threshold (Red Line)",
                  min = 0,
                  max = 30,
                  value = 20)
    ),
    
    
    # Show a plot of the generated distribution
    mainPanel(
      plotOutput("HistPlot"),
      br(),
      h4("Response to Selection:", style = "color:blue"),
      tableOutput("Breeders")
    )
  )
)


data.maker <- function(n=1000, 
                       mean.phen=15,
                       sd=5,
                       threshold=20,
                       h2=0.4,
                       seed=1) {
  set.seed(seed)
  
  # generate 1st generation values
  first.gen.val=rnorm(n,mean.phen,sd)
  
  # generate 2nd generation after selection
  s=mean(first.gen.val[first.gen.val>threshold])-mean.phen
  delta.z=h2*s
  sec.gen.val=first.gen.val+delta.z
  
  # generate 3rd generation after selection
  #s=mean(sec.gen.val[sec.gen.val>threshold])-mean.phen
  #delta.z=h2*s
  #third.gen.val=sec.gen.val+delta.z
  
  x=data.frame(phen.val=c(first.gen.val,sec.gen.val),#,third.gen.val),
               generation=factor(c(rep("Before selection (Parents)",n),
                            rep("After selection (Offspring)",n)),
                            #rep("2nd gen. after selection",n)),
                            levels = c("Before selection (Parents)", 
                                       "After selection (Offspring)"))) 
                                       #"2nd gen. after selection")),
  x$selected=factor(ifelse(x$phen.val>threshold,"Yes","No"),
                    levels = c("Yes","No"))
  x
}

# df=data.maker()


server <- function(input, output) {
  
  # Generate data and plot parent offspring regression
  output$HistPlot <- renderPlot({
    
    df=data.maker(h2=input$herit.slider,
                  threshold=input$selection.gradient.slider)
    
    df.vals=data.frame(z=c(mean(subset(df,generation=="Before selection (Parents)")$phen.val),
                           input$selection.gradient.slider,
                           mean(subset(df, generation=="Before selection (Parents)" & selected=="Yes")$phen.val)),
                       Values=factor(c("Population Mean","Threshold","Selected Population Mean"),
                                         levels = c("Population Mean","Threshold","Selected Population Mean")),
                       Color=factor(c("#000000","#f04546","#333333"),
                                    levels = c("#000000","#f04546","#333333")))
    
    a=ggplot(subset(df,generation=="Before selection (Parents)"), aes(x=phen.val)) + 
    geom_histogram(colour="black", fill="white", binwidth=3) + theme_test() + 
    geom_vline(data=df.vals,aes(xintercept=z, color=Values), linetype="dashed", size=1) +   
    scale_colour_manual(name="",
                        values=c(df.vals$Color)) +
  ylab("Frequency") + xlab("Calling rate (chirps/s)") + xlim(0,50) +
    theme(axis.title = element_text(face="bold", size=16),
          axis.text = element_text(size=14),
          legend.title=element_blank(),
          legend.position=c(.8, .7))
  
    
    # Show first two generations
    df.mean=data.frame(z=c(mean(df[1:1000,1]),
                           mean(df[1001:2000,1])),
                       generation=factor(c("Before selection (Parents)","After selection (Offspring)"),
                                         levels = c("Before selection (Parents)", 
                                                    "After selection (Offspring)")))
    
      b=ggplot(df[1:2000,], aes(x=phen.val, fill=generation)) + 
        geom_histogram(colour="black", binwidth=3) + theme_test() +
        facet_wrap(~generation, ncol = 1) + theme_test() + scale_fill_wsj() + 
        geom_vline(data=df.mean,aes(xintercept=z), color="black", linetype="dashed", size=1) +
        ylab("Frequency") + xlab("Calling rate (chirps/s)") + xlim(0,50) +
        theme(legend.position = "none",
              axis.title = element_text(face="bold", size=16),
              axis.text = element_text(size=14),
              strip.text = element_text(size=12, face="bold")) 
    a+b+plot_layout(nrow = 2, heights = c(1,2))
      
    
  })
  
  output$Breeders <- renderTable({
    df <- data.maker(h2=input$herit.slider,
                     threshold=input$selection.gradient.slider)
    table=data.frame(Parameters=c("Population Mean (z.pop)",
                            "Selected Population Mean (z.selected)",
                            "% Above Threshold",
                            "% Above Threshold"),
               Generation=c(rep("Before selection (Parents)",3),"After selection"),
               Value=c(round(mean(subset(df,generation=="Before selection (Parents)")$phen.val),1),
                       round(mean(subset(df,generation=="Before selection (Parents)" & selected=="Yes")$phen.val),1),
                       round(length(subset(df,generation=="Before selection (Parents)" & selected=="Yes")$phen.val)/1000*100,1),
                       round(length(subset(df,generation=="After selection (Offspring)" & selected=="Yes")$phen.val)/1000*100)),1)
    table[,1:3]
  })
}


# Run the application 
shinyApp(ui = ui, server = server)

#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(dplyr)
library(ggplot2)
library(DT)

# Define UI for application that draws a histogram
ui <- fluidPage(

  textInput("name", label = "Name", value = ""),
  numericInput("guess", label = "Guess", value = NA),
  submitButton("Submit Guess", icon("refresh")),
  checkboxInput("exclude", label="Exclude Outliers (Hit 'Submit Guess')", value = TRUE, width = NULL),
  tabsetPanel(
    tabPanel("Plot",plotOutput("plot")),
    tabPanel("Data",DTOutput("data")),
  )

)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
    data=reactive({
      
      ## Write out result
      
      write.table(
        data.frame(
          name=input$name,
          guess=input$guess,
          time=Sys.time()
        ),
        "log.csv",
        sep = ",",
        col.names = !file.exists("log.csv"),
        append = T,
        row.names=FALSE,
        quote=FALSE)
      
      ## Read in result
      
      data=read.csv("log.csv",header = TRUE) %>% 
        filter(!is.na(guess)) %>% 
        mutate(absdiff=abs(1644-guess)) %>% 
        arrange(absdiff)
      
      if(input$exclude){
        data=data %>% filter(guess<(quantile(data$guess,.75)+1.5*IQR(data$guess)) & guess>(quantile(data$guess,.25)-1.5*IQR(data$guess)))
      }
      
      data
      
      })
    
    output$data=renderDT({data()})
    
    output$plot=renderPlot({
      
      ggplot(data(), aes(x=guess)) +
        geom_histogram(color="black", 
                       fill="steelblue",
                       bins=20) +
        geom_vline(aes(xintercept=median(guess,na.rm=TRUE)),
                   color="blue", 
                   linetype="dashed", 
                   size=1) +
        geom_vline(aes(xintercept=1644),
                   color="red", 
                   linetype="solid", 
                   size=1) +
        annotate("text", x = median(data()$guess,na.rm=TRUE), y = 25, label = "Median") +
        annotate("text", x = 1644, y = 20, label = "True Value: 1644") +
        # geom_histogram(aes(y=..density..), colour="black", fill="white")+
        geom_density(alpha=.2, fill="#FF6666") +
        ylab("People Who Made that Guess") +
        xlab("Jellybean Guess")
      
    })
    

  
  
}

# Run the application 
shinyApp(ui = ui, server = server)

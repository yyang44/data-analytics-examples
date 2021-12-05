#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)


# STEP: LOAD DATA - Initial steps: 

## 1 Set working directory - Session > Set Working Directory > To Source File location

## 2 Load data with read.csv
hurrdata <- read.csv("/Users/yyj/Desktop/data200/lab/rshiny/atlantic.csv", header = TRUE, sep = ",")

## 3 Clean data
# Extract year and month and store them in separate columns
date.s <- as.character(hurrdata$Date)  #Creates a character string of the Data
hurrdata$Year <- substr(date.s,1,4)    #Extracts the year from the date string (position 1-4)
hurrdata$Month <- substr(date.s,5,6)   #Extracts the month from the date string (position 5-6)
hurrdata$Year <- as.numeric(as.character(hurrdata$Year)) #Convert the variable Year to numeric so we can easily plot it later

## 4 Load ggplot
library(ggplot2)
hurrdata$Maximum.Wind[hurrdata$Maximum.Wind < 0] <- NA

## 5 Sample the data
library(dplyr) #Always load dplyr after plyr, as I have.
hurrdata2 <- sample_n(hurrdata, 200, weight = NULL, replace=TRUE)

# STEP: DEFINE UI

# Define UI for application
ui <- fluidPage(

    # Application title
    titlePanel("Hurricane Data"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            selectInput("colors",
                        "Select a color:",
                        c("red","blue","yellow","black"))
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("hurrPlot")
        )
    )
)



# Define server logic required to create our graph
server <- function(input, output) {

    output$hurrPlot <- renderPlot({
        library(ggplot2)
        hurrgraph2 <- ggplot(data=hurrdata2, aes(x = Year, y=hurrdata2$Maximum.Wind, color=hurrdata2$Maximum.Wind)) + 
            #geom_line(linetype = "dashed")+ #This is a line graph, but we have too many points for that currently. 
            geom_point()+ #This adds the initial points
            theme(axis.text.x=element_text(angle=45,hjust=1,vjust=0.5))+
            scale_color_gradient(low=input$colors, high="red")+ #Create a color scale to show high wind speed
            theme(legend.position='bottom')+ #Put the legend on the bottom
            ylab("Maximum Wind (knots)")+ #Change the y-label
            ggtitle("Selected Annual Hurricane Data, 1851 - 2015")+ #Add a title
            theme(plot.title = element_text(lineheight=.8, face="bold")) #Modify the title
        hurrgraph2
        })
}

# Run the application 
shinyApp(ui = ui, server = server)

#Import the libraries needed
#Library to run RShiny
library(shiny)
#Library to create shinydashboard
library(shinydashboard)
library(readr)
library(ggplot2)
library(tibble)
library(lubridate)
library(stringr)
library(dplyr)


#Load the data

data <- read_csv("Weekly_Gasoline_Product_Supplied.csv")


#Reverse the order to January to December
data <- data[nrow(data):1,]

#Changing datatypes
#Changing Fiscal Week data type to numeric data type
data$`Fiscal Week` <- as.numeric(data$`Fiscal Week`)
#Changing Fiscal Year data type to factor data type
data$`Fiscal Year` <- factor(data$`Fiscal Year`)

#Renaming the column names
names(data)[1] <- "Fiscal_Year"
names(data)[2] <- "Fiscal_Week"
names(data)[3] <- "Current_Year_Production"
names(data)[4] <- "Previous_Year_Production"
names(data)[5] <- "Difference_From_Same_Week_Last_Year"
names(data)[6] <- "Current_Year_Cumulative_Production"
names(data)[7] <- "Cumulative_Difference"

#Separating 2020 data and creating year2020 dataset
#Returns index that has 2020 in the year column
str_which(data$Fiscal_Year, '2020')
#Slicing the original data to create year2020 dataset
year2020 <- slice(data, (1:52))

#Graph for 2020
ggplot(year2020, aes(x= Fiscal_Week, y= Current_Year_Production)) +
    geom_line() +
    labs(title = "2020 Gasoline Production",
         x = "Fiscal Week",
         y = "Gallons (Billions)") +
    #Change the scientific number into single digit numbers on y axis
    scale_y_continuous(breaks = c(0e+00, 1.5e+09, 2.0e+09, 2.5e+09),
                       labels = c("0", "1.5", "2.0", "2.5"))+
    theme(
        # Remove panel border
        panel.border = element_blank(),  
        # Remove panel grid lines
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        # Remove panel background
        panel.background = element_blank(),
        # Add axis line
        axis.line = element_line(colour = "black")) 

#Separating 2021 data and creating year2021 dataset
#Returns index that has 2021 in the year column
str_which(data$Fiscal_Year, '2021')
#Slicing the original data to create year2021 dataset
year2021 <- slice(data, (53:82))

#Graph for 2021
ggplot(year2021, aes(x= Fiscal_Week, y= Current_Year_Production)) +
    geom_line(color = "red") +
    labs(title = "2021 Gasoline Production",
         x = "Fiscal Week",
         y = "Gallons (Billions)") +
    #Change the scientific number into single digit numbers on y axis
    scale_y_continuous(breaks = c(0e+00, 2.1e+09, 2.2e+09, 2.3e+09, 2.4e+09, 2.5e+09, 2.6e+09, 2.7e+09),
                       labels = c("0", "2.1", "2.2", "2.3", "2.4", "2.5", "2.6", "2.7"))+
    theme(
        # Remove panel border
        panel.border = element_blank(),  
        # Remove panel grid lines
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        # Remove panel background
        panel.background = element_blank(),
        # Add axis line
        axis.line = element_line(colour = "black"))


#Both years together production year
ggplot(data, aes(x= Fiscal_Week, y= Current_Year_Production, group= Fiscal_Year, color= Fiscal_Year)) +
    geom_line() +
    labs(title = "Gasoline Production",
         x = "Fiscal Week",
         y = "Gallons (Billions)") +
    #Change the scientific number into single digit numbers on y axis
    scale_y_continuous(breaks = c(0e+00, 1.5e+09, 2e+09, 2.5e+09),
                       labels = c("0", "1.5", "2", "2.5"))+
    theme(
        # Remove panel border
        panel.border = element_blank(),  
        # Remove panel grid lines
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        # Remove panel background
        panel.background = element_blank(),
        # Add axis line
        axis.line = element_line(colour = "black"))

#Gasoline Difference between 2020 and 2021
#Cumulative_Difference is negative so I have to use abs() to get the absolute value
ggplot(year2021, aes(x= Fiscal_Week, y= abs(Cumulative_Difference),
                     #Ifelse statement if Fiscal_Week == 25, the color it
                     fill=factor(ifelse(Fiscal_Week =="25","Highest Value","Normal")))) +
    geom_bar(stat = "identity") +
    #Manually color the highest value and the rest the same color
    scale_fill_manual(name = "Hightest Value", values=c("green","grey50")) +
    labs(title = "Gasoline Difference Between Last Year",
         x = "Fiscal Week",
         y = "Gallons (Billions)") +
    #Change the scientific number into single digit numbers on y axis
    scale_y_continuous(breaks = c(0e+00, 2e+09, 4e+09, 6e+09),
                       labels = c("0", "2", "4", "6"))+
    theme(
        # Remove panel border
        panel.border = element_blank(),  
        # Remove panel grid lines
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        # Remove panel background
        panel.background = element_blank(),
        # Add axis line
        axis.line = element_line(colour = "black"))

#Creating the ui
#Use the fluidPage for the ui
ui <- fluidPage(
    #Added the ui title as "My story"
    titlePanel("My Story"),
    #Made all the text size to 20
    tags$head(tags$style('body {font-size: 20px}')),
    #Set it up as tabs so I can switch to different section of the story
    tabsetPanel(id = "tabs",
                #Name the first tab as "Question" and display the question of the assignment
                tabPanel(title = "Question",
                         #display the text from output$question in server()
                         textOutput("question"),
                ),
                #Name the second tab as "Pandemic" and display the setup sentence and the graph
                tabPanel( title = "Pandemic",
                          #Display the Setup sentence from output$setup in server()
                          textOutput("setup"),
                          #Display the graph from output$setup_graph in server()
                          plotOutput("setup_graph")
                ),
                #Name the third tab as "Vaccine Distribution" and display the conflict sentence and the graph
                tabPanel( title = "Vaccine Distribution",
                          #Display the conflict sentence from output$conflict in server()
                          textOutput("conflict"),
                          #Display the graph from output$conflict_graph in server()
                          plotOutput("conflict_graph")
                ),
                #Name the fourth tab as "Post-Pandemic Outlook" and display the resolution sentence and the graph
                tabPanel( title = "Post-Pandemic Outlook",
                          #Display the resolution sentence from output$end in server()
                          textOutput("resolution"),
                          #Display the graph from output$resolution_graph in server()
                          plotOutput("resolution_graph")
                )
    )
)


#Creating the server
#Have the input and output as the two argument for the server
server <- function(input, output)
{
    #Text for the question tab
    output$question <- renderText("How does the pandemic of Covid-19 affect the gasoline supply for the U.S?")
    
    #Text for the Pandemic tab
    output$setup <- renderText("As the threat of Covid-19 emerges, states began to issuing restrictions, lockdowns, and quarantines resulting in fewer people traveling.")
    
    #Text for the Vaccine Distribution tab
    output$conflict <- renderText("Late 2020 and the beginning of 2021 is when vaccines such as the Moderna and Pfrizer-BioNTech were beginning to distribute slowly.")
    
    #Text for the Post-Pandemic Outlook tab
    output$resolution <- renderText("The difference in gasoline supply was growing peaking at around 6 billion gallons at week 25. However, as more vaccines are being distributed and restrictions are being lifted, more people are traveling. Therefore, the gasoline supply difference will continue to shrink and the gasoline supply will recover to its normal level.")
    
    
    #Graph for the Pandemic tab
    output$setup_graph <- renderPlot({
        ggplot(year2020, aes(x= Fiscal_Week, y= Current_Year_Production)) +
            geom_line(color = "red") +
            labs(title = "2020 Gasoline Production",
                 x = "Fiscal Week",
                 y = "Gallons (Billions)") +
            #Change the scientific number into single digit numbers on y axis
            scale_y_continuous(breaks = c(0e+00, 1.5e+09, 2.0e+09, 2.5e+09),
                               labels = c("0", "1.5", "2.0", "2.5"))+
            theme(
                # Remove panel border
                panel.border = element_blank(),  
                # Remove panel grid lines
                panel.grid.major = element_blank(),
                panel.grid.minor = element_blank(),
                # Remove panel background
                panel.background = element_blank(),
                # Add axis line
                axis.line = element_line(colour = "black"))
        
    })
    #Graph for the Vaccine Distribution tab
    output$conflict_graph <- renderPlot({
        ggplot(data, aes(x= Fiscal_Week, y= Current_Year_Production, group= Fiscal_Year, color= Fiscal_Year)) +
            geom_line() +
            labs(title = "Gasoline Production",
                 x = "Fiscal Week",
                 y = "Gallons (Billions)") +
            #Change the scientific number into single digit numbers on y axis
            scale_y_continuous(breaks = c(0e+00, 1.5e+09, 2e+09, 2.5e+09),
                               labels = c("0", "1.5", "2", "2.5"))+
            theme(
                # Remove panel border
                panel.border = element_blank(),  
                # Remove panel grid lines
                panel.grid.major = element_blank(),
                panel.grid.minor = element_blank(),
                # Remove panel background
                panel.background = element_blank(),
                # Add axis line
                axis.line = element_line(colour = "black"))
        
    })
    
    #Graph for the Post-Pandemic Outlook tab
    output$resolution_graph <- renderPlot({
        #Cumulative_Difference is negative so I have to use abs() to get the absolute value
        ggplot(year2021, aes(x= Fiscal_Week, y= abs(Cumulative_Difference),
                             #Ifelse statement if Fiscal_Week == 25, the color it
                             fill=factor(ifelse(Fiscal_Week =="25","Highest Value","Normal")))) +
            geom_bar(stat = "identity") +
            #Manually color the highest value and the rest the same color
            scale_fill_manual(name = "Hightest Value", values=c("green","grey50")) +
            labs(title = "Cumulative Gasoline Difference Between Last Year",
                 x = "Fiscal Week",
                 y = "Gallons (Billions)") +
            #Change the scientific number into single digit numbers on y axis
            scale_y_continuous(breaks = c(0e+00, 2e+09, 4e+09, 6e+09),
                               labels = c("0", "2", "4", "6"))+
            theme(
                # Remove panel border
                panel.border = element_blank(),  
                # Remove panel grid lines
                panel.grid.major = element_blank(),
                panel.grid.minor = element_blank(),
                # Remove panel background
                panel.background = element_blank(),
                # Add axis line
                axis.line = element_line(colour = "black"))
    })
    
}

#Start running the RShiny App, taking in the ui and the server as the two argument
shinyApp(ui,server)

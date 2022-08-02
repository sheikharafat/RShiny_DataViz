
# --------------------------------------------------------------------------------#
#                                    Group Project                                #
#                                                                                 #
# Title: Transitions of Covid-19 Cases and Deaths of the United States 
#        over Time and States                                                     #
#                                                                                 #
# Student Name: Austin Edwards and Sheikh Yasir Arafat                            #                   #
#                                                                                 #
# STA 504: Advanced Data Visualization                                            #
#---------------------------------------------------------------------------------#


#-----------------------------------Set Working Directory-------------------------#
setwd("E:/Miami/Spring_22/STA 504/Group_Project")
#---------------------------------------------------------------------------------#
#-----------------------------Load the required Library---------------------------#
library(tidyverse)
library(lubridate)
library(shiny)
library(gridExtra)
library(ggthemes)
library(scales)
library(ggrepel)
library(stringr)
#---------------------------------------------------------------------------------#
#---------------------------------------------------------------------------------#
#----------------------------------Read the data sets-----------------------------#

Data_map<-read.csv("Data_map_final.csv")
Data_bar<-read.csv("Data_bar_final.csv", stringsAsFactors = TRUE)
#---------------------------------------------------------------------------------#
#------------------Convert the State Variable in capital Letter-------------------#
Data_bar <- 
  Data_bar %>% mutate(State = str_to_title(State))
#---------------------------------------------------------------------------------#
#--------------Matching the data variables name with used variables---------------#
my_month<-c("January"="Jan","February"="Feb","March"="Mar","April"="Apr",
            "May"="May","June"="Jun","July"="Jul","August"="Aug","September"="Sep"
            ,"October"="Oct","November"="Nov",
            "December"="Dec")
my_var<-c("Total Cases"="Final_Case","Total Deaths"="Final_Death")

austin_var<-c("Total Cases"="Final_Case","Total Deaths"="Final_Death")
austin_color<- c("Final_Case"="palegreen4", "Final_Death"="blue")
#---------------------------------------------------------------------------------#
#---------------------------------------------------------------------------------#
#---------------------------------Creating UI Interface---------------------------#
ui <- fluidPage(
  titlePanel("Transitions of Covid-19 Cases and Deaths of the United States over
             Time and States"),
  sidebarLayout( 
    sidebarPanel("Select the Following Three Variables",
                 selectInput(inputId = "Year",
                             label="Choose a Year:",
                             choices = unique(Data_map$Year)),
                 selectInput(inputId = "Month",
                             label="Choose a Month:",
                             choices = my_month),
                 selectInput(inputId = "Variable",
                             label = "Choose a Variable",choices=my_var)),
    mainPanel(plotOutput(outputId = "map"))),
  
  sidebarLayout( 
    sidebarPanel("Select the Following Three Variables",
                 selectInput(inputId = "Year_Austin",
                             label="Choose a Year:",
                             choices = unique(Data_bar$Year)),
                 selectInput(inputId = "State",
                             label="Choose a State:",
                             choices = unique(Data_bar$State)),
                 selectInput(inputId = "Variable_Austin",
                             label = "Choose a Variable",choices=austin_var)),
    mainPanel(#"main panel",
      plotOutput(outputId = "map_Austin")))
  
)
#---------------------------------------------------------------------------------#
#---------------------------------------------------------------------------------#
#--------------------------------Creating Server Function-------------------------#

server <- function(input, output) {
  
  output$map<- renderPlot({
    
    arafat_map<- Data_map %>% 
      filter(Year==input$Year,Month==input$Month) %>% 
      ggplot()+
      geom_polygon(aes_string(x="long",y="lat",group="group",fill=(input$Variable)))+
      theme_map() +
      coord_quickmap()+
      labs(title=paste("Covid-19 United States Total Cases Count for",
                       names(my_month)[my_month==input$Month],input$Year), 
           subtitle="Johns Hopkins University's Data")+
      theme(plot.title = element_text(hjust=0.5,size = 18,face="bold"),
            plot.subtitle = element_text(hjust=0.5,size = 14),
            legend.position = "right",
            legend.text=element_text(size=12))

    
    if(input$Variable=="Final_Case"){
      
      arafat_map+
        scale_fill_gradient(low="honeydew1",high="palegreen4",
                            name=names(my_var)[my_var==input$Variable],
                            labels = scales::comma,
                            trans = "log10")
    }
    
    else {
      arafat_map+
        scale_fill_gradient(low="dodgerblue",high="dodgerblue4",
                            name=names(my_var)[my_var==input$Variable],
                            labels = scales::comma,
                            trans = "log10")
      
    }
    
  })
  
  output$map_Austin<- renderPlot({
    Data_bar %>% 
      filter(Year==input$Year_Austin,State==input$State) %>% 
      ggplot(aes_string(x="Month",y=input$Variable_Austin))+
      geom_bar(stat="identity", fill = austin_color[input$Variable_Austin], width=0.5) +
      geom_text(aes_string(x="Month",y=input$Variable_Austin,
                           label =input$Variable_Austin), vjust=-0.5) +
      labs(title=paste("COVID-19", 
                       names(austin_var)[austin_var==input$Variable_Austin],
                       "for", input$State, "in", input$Year_Austin), 
           subtitle="Johns Hopkins University's Data") +
      scale_y_continuous(labels=scales::comma)+
      scale_x_discrete(limits = c("Jan","Feb","Mar", "Apr", "May", "Jun", "Jul",
                                  "Aug", "Sep", "Oct", "Nov","Dec"))+
      labs(x="Month",y=names(austin_var)[austin_var==input$Variable_Austin])+
      theme_hc()+
      theme(plot.title = element_text(hjust=0.5,size = 15,face="bold"),
            plot.subtitle = element_text(hjust=0.5,size = 14),
            legend.position = "right",
            legend.text=element_text(size=12))
    
  })
  
}

#---------------------------------------------------------------------------------#
#---------------------------------------------------------------------------------#
#----------------------------------Run the App------------------------------------#
shinyApp(ui=ui , server=server)
######### --------------------------------------------------------------- #########
######### --------------------------------------------------------------- #########




#==================================================================================#
#==================================================================================#
#                                       END                                        #
#==================================================================================#
#==================================================================================#
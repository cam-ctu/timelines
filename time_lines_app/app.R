#.
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
#.libPaths("V:/STATISTICS/STUDY PLANNING/R_libraryV3.5")
library(readxl)
library(ggplot2)
library(magrittr)
library(dplyr)






# Define UI for application that draws a histogram
ui <- fluidPage(
   
 
  
  
   # Application title
   titlePanel("Stats Timelines"),
 
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
     
      sidebarPanel(
        sliderInput("y_view", label = "Date Range:",
                    min = as.Date(Sys.time()-60*60*24*30*6),
                    max = as.Date(Sys.time()+60*60*24*365.25*10), 
                    value = as.Date(c(Sys.time(),Sys.time()+60*60*24*365.25*4)),
                    timeFormat = "%b %y", width='80%'
        ),
        
        selectizeInput("filter",label="Filter People",choices=NULL, multiple=TRUE,
                    #selected=list("Simon", "Wendi",  "Marianna","Rachel", "Corey"),
                    #selectize=TRUE, 
                    width='80%'
        )
      ),
      
      # Show a plot of the generated distribution
      mainPanel(
         plotOutput("distPlot")
      )
   )
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
  
   
  my_data <- reactive({
    
    #read in the tabs needed, also the category to create factors
    all_sheets <- excel_sheets("StudyPlanner.xlsx")
    
    statisticians <- read_xlsx("StudyPlanner.xlsx", sheet="Statisticians") |>
      filter(Active==1 & First %in% all_sheets ) |>
      pull(First)


    #sheet_names <- list("Simon", "Wendi",  "Marianna","Rachel", "Corey")#"Annabel","Holly",
    #setwd("V:/STATISTICS/NON STUDY FOLDER/Work Load")
    values <- read_xlsx("StudyPlanner.xlsx", sheet="Category")
    
    #remove the blank rows and join together
    read_in <- function(sheet_name){
      x <- read_xlsx("StudyPlanner.xlsx", sheet=sheet_name)[,1:3] #ignore extra columns like "notes"
      blanks <- apply(x,1, function(x){all(x==""|is.na(x))})
      x <- x[!blanks,]
      names(x) <- tolower(names(x))
      #x$study <- factor(x$study, levels=values$Study)
      x$event <- factor(x$event, levels=values$Event)
      x$person <- sheet_name
      x
    }
    
    data <- lapply( statisticians, read_in) %>% Reduce(rbind,.)
    
    study_order <- data %>% group_by(person, study) %>% summarize( value=unique(study))
    data$study <- factor(data$study, levels=study_order$value, ordered = TRUE)
    
    data
    
  })
  
  
  
   output$distPlot <- renderPlot({
     y_view <- as.POSIXct(input$y_view, format="%d/%m/%y")
     people_selected <- unlist(input$filter)
     
     data2 <- my_data() %>% filter( person %in% people_selected) %>% group_by(person, study) %>% 
       summarise(start_date = pmax(y_view[1],min(date, na.rm = TRUE)),
                 end_date=pmin(y_view[2],max(date, na.rm = TRUE))
       )
     
     
     df <- my_data() %>% filter( person %in% people_selected) 
     df %>% ggplot(aes(x=study, y=date,shape=event))+
       geom_linerange(aes(y=NULL,shape=NULL,ymin=start_date,ymax=end_date, colour=person), data=data2)+
       geom_point() + ylim(y_view) +coord_flip() + scale_shape_manual(values=1:nlevels(df$event))
   })
   
   if (!interactive()) {
     session$onSessionEnded(function() {
       stopApp()
       q("no")
     })
   }
   
   #sheet_names <- my_data()$person |> unique()
   #
  observe({updateSelectizeInput(session, 'filter', 
                        choices = my_data()$person |> unique(),#c("Simon", "Wendi",  "Marianna","Rachel", "Corey"), 
                       selected = my_data()$person |> unique(),#list("Simon", "Wendi",  "Marianna","Rachel", "Corey"),
                        server = TRUE)
  })
   
}

# Run the application 
shinyApp(ui = ui, server = server)


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
library(XML)
library(ggplot2)
library(dplyr)


# Specify the application port
options(shiny.host = "0.0.0.0")
options(shiny.port = 8180)



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
  observe({updateSelectizeInput(session, 'filter',
                                choices = my_data()$person |> unique(),#c("Simon", "Wendi",  "Marianna","Rachel", "Corey"),
                                selected = my_data()$person |> unique(),#list("Simon", "Wendi",  "Marianna","Rachel", "Corey"),
                                server = TRUE)
  })

  my_data <- reactive({
    # this line needs editing between the docker version and local run version
    file <- "/home/STATISTICS/NON STUDY FOLDER/Work Load/timelines/timelines.xml"
    #file <- "home/shiny-app/timelines.xml"
    df_xml <- xmlParse(file)
    statisticians <- xmlToDataFrame(df_xml, nodes=getNodeSet(df_xml, "//statisticians")) |> unique()
    tasks <- xmlToDataFrame(df_xml, nodes=getNodeSet(df_xml, "//tasks"))|> unique()
    studies <- xmlToDataFrame(df_xml, nodes=getNodeSet(df_xml, "//studies"))|> unique()
    df <- xmlToDataFrame(df_xml, nodes=getNodeSet(df_xml, "//timelines"))

    df <- df|> select(-ID) |> rename("ID"="task")|>
      left_join(tasks)|>
      select(-ID) |> rename("ID"="study")|>
      left_join(studies)|>
      select(-ID, - active) |> rename("ID"="statistician")|>
      left_join(statisticians)|>
      select(deadline, study, task, Forename)
    names(df) <- c("date","study","event","person")
    # work out which statistician has the most task in each study

    study_order <- df |> count(study, person) |>
      group_by(study) |>
      arrange(  -n) |>
      slice_head() |>
      arrange(person)

    # study_order <- df %>% group_by(person, study) %>% summarize( value=unique(study))
    df$study <- factor(df$study, levels=study_order$study, ordered = TRUE)
    df$event <- factor(df$event)
    df$date <- as.POSIXct(df$date)
    df


  })



   output$distPlot <- renderPlot({
     y_view <- as.POSIXct(input$y_view, format="%d/%m/%y")
     people_selected <- unlist(input$filter)

     data2 <- my_data() |>
       filter( person %in% people_selected) %>% group_by(person, study) |>
       summarise(start_date = pmax(y_view[1],min(date, na.rm = TRUE)),
                 end_date=pmin(y_view[2],max(date, na.rm = TRUE))
       )


     df <- my_data() |>  filter( person %in% people_selected)
     df |>  ggplot(aes(x=study, y=date,shape=event))+
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


}

# Run the application
shinyApp(ui = ui, server = server)


#.
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(xml2)
library(ggplot2)
library(dplyr)
library(purrr)
library(tidyr)

# Specify the application port
#options(shiny.host = "0.0.0.0")
#options(shiny.port = 8180)



# Define UI for application that draws a histogram
ui <- fluidPage(




   # Application title
   titlePanel("Stats Timelines"),

   # Sidebar with a slider input for number of bins
   sidebarLayout(

      sidebarPanel(
        fileInput("data", "Choose a Data File", accept = ".xml"),
        p("Default: 'V:/STATISTICS/NON STUDY FOLDER/Work Load/timelines/timelines.xml'"),
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

# Define server logic required to draw a figure
server <- function(input, output, session) {


  observe({updateSelectizeInput(session, 'filter',
                                choices = my_data()$person |> unique(),#c("Simon", "Wendi",  "Marianna","Rachel", "Corey"),
                                selected = my_data()$person |> unique(),#list("Simon", "Wendi",  "Marianna","Rachel", "Corey"),
                                server = TRUE)
  })

  my_data <- reactive({
    # this line needs editing between the docker version and local run version
    #file <- "/spare_not_backup/GitHub/timelines/timelines.xml"
    data_file <-input$data
    ext <- tools::file_ext(data_file$datapath)
    #file <- "~/STATISTICS/NON STUDY FOLDER/Work Load/timelines/timelines.xml"
    #df_xml <- read_xml(file)

    req(data_file)
    validate(need(ext == "xml", "Please upload an xml file"))
    df_xml <- read_xml(data_file$datapath)

    # Helper function to mimic xmlToDataFrame for a node set
    xml_to_df <- function(nodes) {
      map_dfr(nodes, ~ {
        children <- xml_children(.x)
        # Handle empty elements gracefully
        if (length(children) == 0) return(tibble())
        setNames(as.list(xml_text(children)), xml_name(children))
      })
    }

    # 2. Extract, convert, and deduplicate your data frames
    statisticians <- xml_find_all(df_xml, "//statisticians") |> xml_to_df() |> unique()
    tasks         <- xml_find_all(df_xml, "//tasks")         |> xml_to_df() |> unique()
    studies       <- xml_find_all(df_xml, "//studies")       |> xml_to_df() |> unique()
    df            <- xml_find_all(df_xml, "//timelines")     |> xml_to_df()



    df <- df|> select(-ID) |> rename("ID"="task")|>
      left_join(tasks)|>
      select(-ID) |> rename("ID"="study")|>
      left_join(studies )|>
      select(-ID, - active) |> rename("ID"="statistician")|>
      left_join(statisticians)|>
      select(deadline, study, task, Forename) |>
      drop_na()
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
       filter( person %in% people_selected) |>
       group_by(person, study, .drop=TRUE) |>
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
}

# Run the application
shinyApp(ui = ui, server = server)


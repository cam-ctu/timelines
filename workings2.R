

library(tidyverse)
library(XML)

file <- "~/STATISTICS/NON STUDY FOLDER/Work Load/timelines/timelines.xml"
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

study_order <- df %>% group_by(person, study) %>% summarize( value=unique(study))
df$study <- factor(df2$study, levels=study_order$value, ordered = TRUE)
df






## JUNK ###

renv::install("tidyverse")
renv::install("XML")


setwd("V:/STATISTICS/NON STUDY FOLDER/Work Load/timelines")

library(xml2)
renv::load()
renv::restore()
library(RODBC)

path <- normalizePath("TimeLines.accdb")
db <- odbcConnect(path)
db <- odbcConnectAccess2007("TimeLines.accdb")
sqlTables(db, tableType="TABLE")
df<- sqlFetch(db, "timelines")
studies <- sqlFetch(db, "studies")
tasks <- sqlFetch(db, "tasks")
statisticians <- sqlFetch(db, "statisticians")
odbcClose(db)



db <- odbcDriverConnect(connection="~/STATISTICS/NON STUDY FOLDER/Work Load/timelines/TimeLines.accdb")


db <- odbcDriverConnect("TimeLines.accdb")


#### old code in app.R


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

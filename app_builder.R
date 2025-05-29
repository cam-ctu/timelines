setwd("V:/STATISTICS/NON STUDY FOLDER/Work Load")
setwd("~/STATISTICS/NON STUDY FOLDER/Work Load")
shiny::runApp('time_lines_app.R', launch.browser=TRUE)
install.packages("shinylive")


shinylive::export("time_lines_app", "time_lines_site")
httpuv::runStaticServer("time_lines_site")

runApp("time_lines_app")
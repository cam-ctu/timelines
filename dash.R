data2 <- df |>
  group_by(person, study, .drop=TRUE) |>
  summarise(start_date =min(date, na.rm = TRUE),
            end_date=max(date, na.rm = TRUE)
  )


install.packages("shinylive")
library(shinylive)
shinylive::export("time_lines_app","site")
httpuv::runStaticServer("site_example")

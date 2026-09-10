#install.packages("shinylive")
library(shinylive)
shinylive::export("time_lines_app","docs")
#httpuv::runStaticServer("docs")
# Commit to Github and view on
# https://cam-ctu.github.io/timelines/
# Local rendering fails, not sure why.

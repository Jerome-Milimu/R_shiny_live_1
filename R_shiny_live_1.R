#Convert the shiny app into all the assets for running the app in the browser
shinylive::export(
  appdir = "shiny_cursor_ai", 
  destdir = "docs")


# With development version of httpuv, run shinylive app locally
#remotes::install_github("rstudio/httpuv") This was actiually installed from CRAN
httpuv::runStaticServer(
  "docs/", port=8008)

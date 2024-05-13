

.onLoad <- function(libname,pkgname){
  invisible(
    suppressMessages(
      suppressWarnings(
        
        lapply(
          c('sf', 'rjson', 'data.table', 'tidylog', 'lubridate', 'plotly','scales', 'tidyverse', 'rstudioapi', 'readxl', 'ggrepel', 'ggpmisc', 'hrbrthemes', 'ggpubr', 'shadowtext', 'flextable'),
          function(x){
            invisible(
              suppressPackageStartupMessages(
                library(x,quietly = TRUE,character.only = TRUE)
              )
            )
          })
        
      )
    )
  )
  
  
}
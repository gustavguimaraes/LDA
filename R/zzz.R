

.onLoad <- function(libname,pkgname){
  invisible(
    suppressMessages(
      suppressWarnings(
        
        lapply(
          c('sf', 'rjson', 'data.table', 'tidylog', 'lubridate','scales', 'tidyverse', 'rstudioapi', 'readxl', 'ggrepel', 'hrbrthemes', 'ggpubr', 'shadowtext', 'flextable'),
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

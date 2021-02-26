## ----Library----------------------------------------

# load all packages into the environment. there are more packages here than I ended up using,
# but the list just kept getting longer over time. could clean this up but doesnt seem worth
# the effort because no harm done
library(relaimpo)
library(plotly)
library(plyr)
library(robustbase)
library(sandwich)
library(lmtest)
library(graphics)
library(colorspace)
library(scales)
library(ggrepel)
library(here)
# library(ggtextures)
library(gridExtra)
library(ggpubr)
library(patternplot)
library(plotly)
library(flexdashboard)
library(DT)
library(naniar)
library(maps)
library(mapdata)
library(usmap)
library(ggmap)
library(maptools)
library(sf)
library(PerformanceAnalytics)
library(jtools)
library(ggstance)
library(huxtable)
library(readxl)
library(knitr)
library(tidyverse)
library(kableExtra)
library(ggthemes)
library(patchwork)

## ----Functions-------------------------------------------

# function to pull in all sheets from an excel spreadsheet and turn them into 
# separate data frames. should make for easier data analysis and manipulation 

ReadAllSheets <- function(filename, tibble = FALSE) {
  sheets <- readxl::excel_sheets(filename)
  x <- lapply(sheets, function(X) readxl::read_excel(filename, 
                                                     sheet = X, 
                                                     skip = 6))
  if(!tibble) x <- lapply(x, as.data.frame)
  names(x) <- sheets
  x
}

# function to turn my scenario titles (which are very convoluted and long) into 
# intelligible combinations of variables that I can filter on to better sort the 
# data and results

categorize <- function(table) {
  x <- table %>%
    mutate(h2req = case_when(
      str_detect(Scenario, "5p") ~ "5p",
      str_detect(Scenario, "10p") ~ "10p",
      str_detect(Scenario, "15p") ~ "15p",
      str_detect(Scenario, "20p") ~ "20p",
      str_detect(Scenario, "25p") ~ "25p",
      TRUE ~ "Other"
    )) %>%
    mutate_if(is.numeric, ~round(.,2)) %>%
    mutate_all(~replace(.,is.na(.),0))
}


## ----Library----------------------------------------

# load all packages into the environment. 
# there are more packages here than I ended up using,
# but the list just kept getting longer over time. 
# could clean this up but doesnt seem worth
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
                                                     skip = 1))
  if(!tibble) x <- lapply(x, as.data.frame)
  names(x) <- sheets
  x
}

# function to turn my scenario titles (which are very convoluted and long) into 
# intelligible combinations of variables that I can filter on to better sort the 
# data and results

categorize <- function(table) {
  x <- table %>%
    mutate(cost = case_when(
      str_detect(Scenario, "HighMid") ~ "Mid",
      str_detect(Scenario, "HighLo") ~ "Low",
      str_detect(Scenario, "fast") ~ "Rapid",
      TRUE ~ "Ref"
    )) %>%
    mutate(sys73 = case_when(
      str_detect(Scenario, "sys73") ~ "Sys73",
      TRUE ~ "Ref"
    )) %>%
    mutate(path = case_when(
      str_detect(Scenario, "old") ~ "Old",
      TRUE ~ "New"
    )) %>%
    mutate(cost = factor(cost, levels = c("Ref","Mid","Low","Rapid"))) %>%
    mutate(sys73 = factor(sys73, levels = c("Ref","Sys73"))) %>%
    mutate(path = factor(path, levels = c("Old","New"))) %>%
    mutate(Scenario = paste(path,".",sys73,".",cost,sep = "")) %>%
    mutate_if(is.numeric, ~round(.,2)) %>%
    mutate_all(~replace(.,is.na(.),0))
}

## ----Scales----------------------------------------

bottom1 <- theme(legend.position = "bottom")
bottom2 <- guides(color = guide_legend(nrow = 1), linetype = guide_legend(nrow = 1))

yt <- theme_bw() + 
  theme(strip.background = element_rect(colour = "black", fill = "white"),
        axis.text.x = element_text(angle = 45, hjust = 1, size = 9),
        axis.text.y = element_text(size = 9)) 

st <- theme_bw() + 
  theme(strip.background = element_rect(colour = "black", fill = "white"),
        axis.text.x = element_text(size = 9),
        axis.text.y = element_text(size = 9))

nolegend <- theme(legend.position='none')

noaxes <- theme(
  axis.text = element_blank(),
  axis.line = element_blank(),
  axis.ticks = element_blank(),
  panel.border = element_blank(),
  panel.grid = element_blank(),
  axis.title = element_blank()
)

x_cont <- scale_x_continuous(breaks = seq(2010,2050, by = 5), expand = c(0,1))
x_disc <- scale_x_discrete(breaks = seq(2010,2050, by = 5), expand = c(0,.2))
x_disc_l <- scale_x_discrete(breaks = seq(2010,2050, by = 5), expand = c(0,.2),
                             labels = c("2010", "","2020", "", "2030", "", "2040", "", "2050"))

x_cont_scenario <- scale_x_continuous(breaks = seq(2010,2050, by = 5), expand = c(0,1))
x_disc_scenario <- scale_x_discrete(breaks = seq(2010,2050, by = 5), expand = c(0,.2))


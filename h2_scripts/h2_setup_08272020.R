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
    mutate(co2 = case_when(
      str_detect(Scenario, "40C") ~ "40co2",
      str_detect(Scenario, "60C") ~ "60co2",
      str_detect(Scenario, "80C") ~ "80co2",
      TRUE ~ "Ref"
    )) %>%
    mutate(PEMcost = case_when(
      str_detect(Scenario, "P50") ~ "50PEM",
      str_detect(Scenario, "P80") ~ "80PEM",
      TRUE ~ "Ref"
    )) %>%
    mutate(rnwelc = case_when(
      str_detect(Scenario, "rnwelc") ~ "rnwelc",
      TRUE ~ "gridelc"
    )) %>%
    mutate(availtech = case_when(
      str_detect(Scenario, "elect") ~ "PEMonly",
      str_detect(Scenario, "nonext") ~ "noNextGen",
      TRUE ~ "All"
    )) %>%
    mutate(co2 = factor(co2, levels = levels_co2)) %>%
    mutate(PEMcost = factor(PEMcost, levels = levels_PEMcost)) %>%
    mutate(rnwelc = factor(rnwelc, levels = levels_rnwelc)) %>%
    mutate(availtech = factor(availtech, levels = levels_availtech)) %>%
    mutate(Scenario = paste(co2,".",rnwelc,".",availtech,".",PEMcost, sep = "")) %>%
    mutate_if(is.numeric, ~round(.,2)) %>%
    mutate_all(~replace(.,is.na(.),0))
}

elcprocess <- function(table) {
  x <- table %>%
    mutate(ProcessSet = case_when(
      str_detect(ProcessSet, "COAL") ~ "Coal",
      str_detect(ProcessSet, "HYD") ~ "Hydro",
      str_detect(ProcessSet, "NGA") ~ "Natural Gas",
      str_detect(ProcessSet, "NUK") ~ "Nuclear",
      str_detect(ProcessSet, "SOL") ~ "Solar",
      str_detect(ProcessSet, "WND") ~ "Wind",
      TRUE ~ "Other"
    )) 
}

trnprocess <- function(table) {
  x <- table %>%
    mutate(Process = case_when(
      str_detect(Process, "THH2") ~ "H2 Heavy Truck",
      str_detect(Process, "TLCH2") ~ "H2 Compact",
      str_detect(Process, "TLFH2") ~ "H2 Full",
      str_detect(Process, "TLTCOH2") ~ "H2 Crossover",
      str_detect(Process, "TLVH2") ~ "H2 Van",
      TRUE ~ "Other"
    )) 
}


## ----Scales----------------------------------------

bottom1 <- theme(legend.position = "bottom")
bottom2 <- guides(color = guide_legend(nrow = 1), linetype = guide_legend(nrow = 1))

yt <- theme_bw() + 
  theme(strip.background = element_rect(colour = "black", fill = "white"),
        axis.text.x = element_text(angle = 0, hjust = 1, size = 9),
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

## ----Factors----------------------------------------

levels_PEMcost <- c("Ref","50PEM","80PEM")
levels_availtech <- c("All","noNextGen","PEMonly")
levels_rnwelc <- c("gridelc","rnwelc")
levels_co2 <- c("Ref","40co2","60co2","80co2")



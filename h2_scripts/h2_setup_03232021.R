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
    mutate_if(is.numeric, ~round(.,2)) %>%
    mutate_all(~replace(.,is.na(.),0)) %>%
    mutate(Scenario = str_sub(Scenario,-4,-1)) %>%
    left_join(.,scenarios,by = "Scenario") %>%
    mutate(nz = factor(nz, levels = c("2080","2060","2050","ref"))) %>%
    mutate(trn = factor(trn, levels = c("ref","2050"))) %>%
    mutate(fcev = factor(fcev, levels = c("ref","10","20"))) %>%
    mutate(bev = factor(bev, levels = c("ref","ls"))) %>%
    mutate(td = factor(td, levels = c("ref","low","doe4","doe2"))) %>%
    mutate(fut = factor(fut, levels = c("ref","2030"))) %>%
    mutate(pem = factor(pem, levels = c("ref","70"))) %>%
    mutate(Scenario = paste(nz,".",trn,".",fcev,".",bev,".",td,".",fut,".",pem,sep = "")) %>%
    mutate(Scenario = case_when(
      str_detect(Scenario, "ref.ref.ref.ref.ref.ref.ref") ~ "Ref",
      TRUE ~ as.character(Scenario)
    )) 
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

col_scen <- c("Ref" = "black", 
             "sys80" = "cadetblue", 
             "tlth10" = "chocolate", 
             "trn100" = "darkgoldenrod2",
             "force" = "chartreuse4")


scen_fill <- scale_fill_manual(values = col_scen)
scen_color <- scale_color_manual(values = col_scen)

techlabels <- c(
  "H2CCNGA" = "Cen Nat Gas",
  "H2CCNGACCS" = "Cen Nat Gas CCS",
  "H2CFNGA" = "Cen Future Nat Gas",  
  "H2CFNGACCS" = "Cen Future Nat Gas CCS", 
  "H2DCETH" = "Dist Ethanol.", 
  "H2DCNGA" = "Dist Nat Gas", 
  "H2DFEPEM" = "Dist Future PEM",
  "H2DFETH" = "Dist Future Ethanol",
  "H2DFNGA" = "Dist Future Nat Gas")



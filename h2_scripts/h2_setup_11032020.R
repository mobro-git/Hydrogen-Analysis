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
    mutate(prod = case_when(
      str_detect(Scenario, "TL50") ~ "enduse",
      str_detect(Scenario, "TL70") ~ "enduse",
      str_detect(Scenario, "TL90") ~ "enduse",
      str_detect(Scenario, "TH50") ~ "enduse",
      str_detect(Scenario, "TH70") ~ "enduse",
      str_detect(Scenario, "TH90") ~ "enduse",
      str_detect(Scenario, "H2Overload") ~ "max",
      TRUE ~ "Ref"
    )) %>%
    mutate(qnt = case_when(
      str_detect(Scenario, "TL50") ~ "50LDV",
      str_detect(Scenario, "TL70") ~ "70LDV",
      str_detect(Scenario, "TL90") ~ "90LDV",
      str_detect(Scenario, "TH50") ~ "50HDV",
      str_detect(Scenario, "TH70") ~ "70HDV",
      str_detect(Scenario, "TH90") ~ "90HDV",
      str_detect(Scenario, "TL") ~ "allLDV",
      str_detect(Scenario, "TH") ~ "allHDV",
      str_detect(Scenario, "H2Overload") ~ "allTRN",
      TRUE ~ "Ref"
    )) %>%
    mutate(tech = case_when(
      str_detect(Scenario, "PEM") ~ "pem",
      TRUE ~ "All"
    )) %>%
    mutate(elc = case_when(
      str_detect(Scenario, "Rnw") ~ "rnw",
      TRUE ~ "grid"
    )) %>%
    mutate(rps = case_when(
      str_detect(Scenario, "RnwRPS") ~ "RPS",
      str_detect(Scenario, "Rnw") ~ "noRPS",
      TRUE ~ "none"
    )) %>%
    mutate(prod = factor(prod, levels = c("Ref","enduse","max"))) %>%
    mutate(qnt = factor(qnt, levels = c("Ref","50HDV","70HDV","90HDV","50LDV","70LDV","90LDV",
                                        "allHDV","allLDV","allTRN"))) %>%
    mutate(tech = factor(tech, levels = c("All","pem"))) %>%
    mutate(elc = factor(elc, levels = c("grid","rnw"))) %>%
    mutate(rps = factor(rps, levels = c("none","noRPS","RPS"))) %>%
    mutate(Scenario = paste(prod,".",qnt,".",tech,".",elc,".",rps,sep = "")) %>%
    mutate_if(is.numeric, ~round(.,2)) %>%
    mutate_all(~replace(.,is.na(.),0)) %>%
    filter(rps != "noRPS")
}

elcprocess <- function(table) {
  x <- table %>%
    mutate(Processset = case_when(
      str_detect(Processset, "COAL") ~ "Coal",
      str_detect(Processset, "HYD") ~ "Hydro",
      str_detect(Processset, "NGA") ~ "Natural Gas",
      str_detect(Processset, "NUK") ~ "Nuclear",
      str_detect(Processset, "SOL") ~ "Solar",
      str_detect(Processset, "WND") ~ "Wind",
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


## ----Levels for Factoring ----------------------------------------

levels_PEMcost <- c("Ref","50PEM","80PEM")
levels_availtech <- c("All","noNextGen","PEMonly")
levels_rnwelc <- c("gridelc","rnwelc")
levels_co2 <- c("Ref","40co2","60co2","80co2")


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

col_elc <- c(`Wind` = "#92CBF3", `Hydro` = "dodgerblue4", 
             `Solar` = "darkgoldenrod2", `Nuclear` = "darkorange3", 
             `Coal` = "gray9", `Natural Gas` = "darkslategray4", 
             `Coal CCS` = "gray", `Other` = "#F3C0D4")
col_cost<- c("#462300","#80470E","#B27941","#EEB67F","#7FBDEE","#367FB7","#034679")
col_qnt <- c("Ref" = "#6b4b3a", 
              "50HDV" = "#95eb31", 
              "70HDV" = "#579113", 
              "90HDV" = "#2d5400", 
              "50LDV" = "#f78b60", 
              "70LDV" = "#cc5747", 
              "90LDV" = "#911403",
              "allHDV" = "#7FBDEE",
              "allLDV" = "#367FB7",
              "allTRN" = "#034679")


elc_fill <- scale_fill_manual(values = col_elc)
elc_color <- scale_color_manual(values = col_elc)
qnt_color <- scale_color_manual(values = col_qnt)
qnt_fill <- scale_fill_manual(values = col_qnt)


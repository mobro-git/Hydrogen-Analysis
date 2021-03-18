## ----Setup------------------------------------------

setwd("SummaryWorkbook/")

source("library.R")
source("functions.R")
source("graphing.R")

myfiles <- list.files(pattern = "*.csv")
data <- lapply(myfiles, read_csv)
names(data) <- c("com","dummy","elc","emis","flex","ind","res","trn")

list2env(data, envir = globalenv())

setwd(..)

## ----Dummies------------------------------------------

dummy <- read_csv("Dummy Check.csv")

dummy_reg <- dummy %>% 
  group_by(Scenario,Attribute,Commodity,Process,Period,Region) %>%
  summarize(Pv = sum(Pv)) %>%
  ungroup()

dummy <- dummy_reg %>%
  group_by(Scenario,Attribute,Commodity,Process,Period) %>%
  summarize(Pv = sum(Pv)) %>%
  ungroup()

## ----Commercial Summary------------------------------------------

com <- read_csv("Commercial Summary.csv")

com_reg <- com %>% 
  group_by(Scenario,Attribute,Commodity,Process,Period,Region) %>%
  summarize(Pv = sum(Pv)) %>%
  ungroup()

## ----Electric Summary------------------------------------------

elc <- read_csv("ELC Summary.csv")

## ----Emissions Summary------------------------------------------

emis <- read_csv("Emissions Summary.csv")

## ----Flex Fuels------------------------------------------

flexfuel <- read_csv("ETHB20LPGXCNGX breakdown.csv")

## ----Industrial Summary------------------------------------------

ind <- read_csv("Industrial Summary.csv")

## ----Residential Summary------------------------------------------

res <- read_csv("Residential Summary.csv")

## ----Transportation Summary------------------------------------------

trn <- read_csv("Transportation Summary.csv")













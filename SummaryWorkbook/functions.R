# pulls in all sheets from Veda2.0 batch export as separate data frames

ReadAllSheets <- function(filename, tibble = FALSE) {
  sheets <- readxl::excel_sheets(filename)
  x <- lapply(sheets, function(X) readxl::read_excel(filename, 
                                                     sheet = X, 
                                                     skip = 1))
  if(!tibble) x <- lapply(x, as.data.frame)
  names(x) <- sheets
  x
}

# creates parameters out of scenario names and renames scenarios

categorize <- function(table) {
  x <- table %>%
    mutate(dist = case_when(
      str_detect(Scenario, "Dist") ~ "nodist",
      TRUE ~ "all"
    )) %>%
    mutate(scen = case_when(
      str_detect(Scenario, "Sys80") ~ "sys80",
      str_detect(Scenario, "10P") ~ "tlth10",
      str_detect(Scenario, "Force") ~ "force",
      str_detect(Scenario, "TRN100") ~ "trn100",
      TRUE ~ "Ref"
    )) %>%
    mutate(td = case_when(
      str_detect(Scenario, "fast") ~ "rapid",
      str_detect(Scenario, "low") ~ "low",
      str_detect(Scenario, "mid") ~ "mid",
      TRUE ~ "high"
    )) %>%
    mutate(td = factor(td, levels = c("rapid","low","mid","high"))) %>%
    mutate(dist = factor(dist, levels = c("all","nodist"))) %>%
    mutate(scen = factor(scen, levels = c("force","trn100","tlth10","sys80","Ref"))) %>%
    mutate(Scenario = paste(scen,".",dist,".",td,sep = "")) %>%
    mutate_if(is.numeric, ~round(.,2)) %>%
    mutate_all(~replace(.,is.na(.),0))
}

elcprocess <- function(table) {
  x <- table %>%
    mutate(Process = case_when(
      str_detect(Process, "COAL") ~ "Coal",
      str_detect(Process, "HYD") ~ "Hydro",
      str_detect(Process, "NGA") ~ "Natural Gas",
      str_detect(Process, "NUK") ~ "Nuclear",
      str_detect(Process, "SOL") ~ "Solar",
      str_detect(Process, "WNDOF") ~ "Offshore Wind",
      str_detect(Process, "WNDON") ~ "Terrestrial Wind",
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

emission <- function(table) {
  x <- table %>%
    mutate(Commodity = case_when(
      str_detect(Commodity, "CH4") ~ "CH[4]",
      str_detect(Commodity, "CO2") ~ "CO[2]",
      str_detect(Commodity, "NOX") ~ "NO[X]",
      str_detect(Commodity, "SO2") ~ "SO[2]",
      str_detect(Commodity, "PM25") ~ "PM[2.5]"
    )) 
}
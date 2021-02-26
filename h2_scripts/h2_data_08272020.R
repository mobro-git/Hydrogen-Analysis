## ----import data------------------------------------------

# points to the excel file of output from VEDA_BE, reads in the names of the sheets,
# and plugs the spreadsheet into the function I created to turn all sheets into
# separate dataframes in the current environment

results <- "h2_rawdata/h2data_carol082620.xls"
data_global <- ReadAllSheets(here(results))

## ----co2 emissions------------------------------------------

co2emissions <- as.data.frame(data_global$`CO2 Emissions`) %>% 
  select(-`2011`,-`Attribute`) %>%
  gather(`2010`, `2015`, `2020`, `2025`, `2030`, `2035`, `2040`, `2045`, `2050`, 
         key = "Year", value = "emissions") %>%
  filter(Commodity != "ETHCO2") %>%
  categorize() 

## ----so2 emissions------------------------------------------

so2emissions <- as.data.frame(data_global$`SO2 Emissions`) %>% 
  select(-`2011`,-`Attribute`) %>%
  gather(`2010`, `2015`, `2020`, `2025`, `2030`, `2035`, `2040`, `2045`, `2050`, 
         key = "Year", value = "emissions") %>%
  mutate_if(is.numeric, ~round(.,2)) %>%
  mutate_all(~replace(.,is.na(.),0)) %>%
  categorize()

## ----h2 production------------------------------------------------------

h2prod_reg <- as.data.frame(data_global$`H2Production`) %>% 
  gather(`2025`, `2030`, `2035`, `2040`, `2045`, `2050`, 
         key = "Year", value = "PJ") %>%
  categorize()

h2prod_cumreg <- h2prod_reg %>%
  group_by(Scenario, Year, Attribute, Region, co2, PEMcost, availtech, rnwelc) %>%
  summarize(PJ = sum(PJ)) %>%
  ungroup()

h2prod <- h2prod_reg %>%
  group_by(Scenario, Year, Attribute, Process, co2, PEMcost, availtech, rnwelc) %>%
  summarize(PJ = sum(PJ)) %>%
  ungroup()

h2prod_cum <- h2prod %>%
  group_by(Scenario,Year,Attribute, co2, PEMcost, availtech, rnwelc) %>%
  summarize(PJ = sum(PJ)) %>%
  ungroup()

## ----ELC and RNWELC------------------------------------------------------

elcrnwelc_reg <- as.data.frame(data_global$`RNWELCandELC`) %>% 
  select(-`2011`) %>%
  gather(`2010`, `2015`, `2020`, `2025`, `2030`, `2035`, `2040`, `2045`, `2050`, 
         key = "Year", value = "PJ") %>%
  categorize() %>%
  elcprocess()

elcrnwelc_regtotal <- elcrnwelc_reg %>%
  group_by(Scenario,Year,Region,co2,PEMcost,availtech,rnwelc) %>%
  summarize(PJ = sum(PJ)) %>%
  ungroup()

elcrnwelc_total <- elcrnwelc_regtotal %>%
  group_by(Scenario,Year,co2,PEMcost,availtech,rnwelc) %>%
  summarize(PJ = sum(PJ)) %>%
  ungroup()

elcrnwelc <- elcrnwelc_reg %>%
  group_by(Commodity,ProcessSet,Scenario,Year, co2, PEMcost, availtech, rnwelc) %>%
  summarize(PJ = sum(PJ)) %>%
  ungroup()

## ----h2 fuel use in transportation----------------------------------

h2trn_reg <- as.data.frame(data_global$`TrnH2Use`) %>%
  gather(`2025`, `2030`, `2035`, `2040`, `2045`, `2050`, 
         key = "Year", value = "PJ") %>% 
  categorize() %>%
  trnprocess()
h2trn_reg$Year <- as.numeric(h2trn_reg$Year)

h2trn <- h2trn_reg %>%
  group_by(Attribute,Commodity,Process,Scenario,Year,
           co2,PEMcost,availtech,rnwelc) %>%
  summarize(PJ = sum(PJ)) %>%
  ungroup()

## ----h2 vehicles in transportation sector -----------------------------------

h2car <- as.data.frame(data_global$H2TRNTechs) %>%
  select(-`2011`) %>%
  gather(`2010`, `2015`, `2020`, `2025`, `2030`, `2035`, `2040`, `2045`, `2050`, 
         key = "Year", value = "bnVMT") %>%
  categorize()

thDSLcombo <- h2car %>% filter(ProcessSet %in% c("TH-B20X", "TH-DSL")) %>%
  group_by(Commodity,Scenario,Year,co2,PEMcost,availtech,rnwelc) %>%
  summarize(bnVMT = sum(bnVMT)) %>%
  mutate(ProcessSet = "TH-DSL") %>%
  select(Commodity,ProcessSet,Scenario,Year,bnVMT,co2,PEMcost,availtech,rnwelc) %>%
  ungroup()

tlGSLcombo <- h2car %>% filter(ProcessSet %in% c("TL-E85X","TL-GSL")) %>%
  group_by(Commodity,Scenario,Year,co2,PEMcost,availtech,rnwelc) %>%
  summarize(bnVMT = sum(bnVMT)) %>%
  mutate(ProcessSet = "TL-GSL") %>%
  select(Commodity,ProcessSet,Scenario,Year,bnVMT,co2,PEMcost,availtech,rnwelc) %>%
  ungroup()

h2car_small <- h2car %>%
  filter(!ProcessSet %in% c("TH-B20X", "TH-DSL","TL-E85X","TL-GSL")) %>%
  select(Commodity,ProcessSet,Scenario,Year,bnVMT,co2,PEMcost,availtech,rnwelc)

h2carcombo <- rbind(h2car_small,thDSLcombo,tlGSLcombo) 

h2totalcar <- h2carcombo %>%
  group_by(Commodity,Scenario,Year,co2,PEMcost,availtech,rnwelc) %>%
  summarize(totVMT = sum(bnVMT)) %>%
  ungroup() %>%
  left_join(h2carcombo, by = c("Scenario","Commodity","Year","co2","PEMcost","rnwelc","availtech")) %>%
  mutate(mktshr = round((bnVMT/totVMT),2)) %>%
  select(Scenario,co2,PEMcost,availtech,rnwelc,Commodity,ProcessSet,Year, totVMT,bnVMT,mktshr)
  
  
  
  

## ----import data------------------------------------------

# points to the excel file of output from VEDA_BE, reads in the names of the sheets,
# and plugs the spreadsheet into the function I created to turn all sheets into
# separate dataframes in the current environment

results <- "h2_rawdata/Phase1A_resultssummary.xlsx"
data_global <- ReadAllSheets(here(results))

## ----Cases ran------------------------------------------

cases <- as.data.frame(data_global$casenames) %>% select(-Case)

## ----Scenarios------------------------------------------

scenarios <- as.data.frame(data_global$scenarios) %>% select(-Case)

## ----dummies------------------------------------------

dummy <- as.data.frame(data_global$`dummies`) %>% 
  gather(`2010`, `2015`, `2020`,`2045`,`2050`, 
         key = "Year", value = "dummy") %>%
  categorize() 

## ----transportation bev vs fcev------------------------------------------

tran <- as.data.frame(data_global$`FCEV vs BEV`) %>%
  select(-`2011`) %>%
  gather(`2010`, `2015`, `2020`, `2025`, `2030`, `2035`, `2040`, `2045`, `2050`,
         key = "Year", value = "use") %>%
  categorize() %>%
  mutate(sector = case_when(
    str_detect(Commodity, "TB") ~ "TB",
    str_detect(Commodity, "TC") ~ "TC",
    str_detect(Commodity, "TH") ~ "TH",
    str_detect(Commodity, "TL") ~ "TL",
    str_detect(Commodity, "TM") ~ "TM",
    TRUE ~ "Other")) %>%
  mutate(fuel = case_when(
    str_detect(Commodity, "H2") ~ "H2",
    str_detect(Commodity, "ELC") ~ "ELC",
    TRUE ~ "Other"))

tran_sum <- tran %>%
  group_by(Scenario,Year,nz,trn,fcev,bev,td,fut,pem,fuel) %>%
  summarize(use = sum(use)) %>%
  ungroup()
  

## ----h2 capacity------------------------------------------------------

 h2cap <- as.data.frame(data_global$`H2 Production Capacity`) %>% 
  gather(`2025`, `2030`, `2035`, `2040`, `2045`, `2050`,
         key = "Year", value = "cap") %>%
  categorize() %>%
  mutate(Process = factor(Process, levels = c("H2CFNGACCS","H2CFEPEM",
                                              "H2DCETH","H2DCNGA",
                                              "H2DFETH","H2DFNGA","H2DFEPEM"))) %>%
  mutate(Type = case_when(
    str_detect(Process, "H2D") ~ "Distributed",
    TRUE ~ "Centralized")) %>%
  mutate(Fuel = case_when(
    str_detect(Process, "ETH") ~ "Ethanol",
    str_detect(Process, "CCS") ~ "Natural Gas CCS",
    str_detect(Process, "NGA") ~ "Natural Gas",
    str_detect(Process, "PEM") ~ "Electrolysis",
    TRUE ~ "Other")) 

h2cap %>% filter(cap > 0) %>% filter(Fuel == "Natural Gas CCS") %>% view()


## ----h2 fuels------------------------------------------------------

h2fuel <- as.data.frame(data_global$`H2 Production Fuels`) %>%
  gather(`2025`, `2030`, `2035`, `2040`, `2045`, `2050`,
         key = "Year", value = "flo") %>%
  categorize() %>%
  mutate(Process = factor(Process, levels = c("H2CFNGACCS","H2CFEPEM",
                                              "H2DCETH","H2DCNGA",
                                              "H2DFETH","H2DFNGA","H2DFEPEM"))) %>%
  mutate(Type = case_when(
    str_detect(Process, "H2D") ~ "Distributed",
    TRUE ~ "Centralized")) %>%
  mutate(Fuel = case_when(
    str_detect(Process, "ETH") ~ "Ethanol",
    str_detect(Process, "CCS") ~ "Natural Gas CCS",
    str_detect(Process, "NGA") ~ "Natural Gas",
    str_detect(Process, "PEM") ~ "Electrolysis",
    TRUE ~ "Other")) 

h2prodemissions <- h2fuel %>%
  filter(Commodity %in% c("ELCH2OW","ELCH2OC","CO2S","INDCO2"))

h2prodinput <- h2fuel %>%
  filter(Commodity %in% c("INDNGA","INDELC","ETH"))
  
h2prod <- h2fuel %>%
  filter(Commodity %in% c("H2PROD","H2PUMP")) %>%
  group_by(Scenario,Year,nz,trn,fcev,bev,td,fut,pem,Type,Fuel) %>%
  summarize(flo = sum(flo)) %>%
  ungroup()

h2prod_sum <- h2prod %>%
  group_by(Scenario,Year,nz,trn,fcev,bev,td,fut,pem) %>%
  summarize(flo = sum(flo)) %>%
  ungroup()

## ----h2 t&d---------------------------------------------------

h2td <- as.data.frame(data_global$`H2 T&D`) %>%
  gather(`2025`, `2030`, `2035`, `2040`, `2045`, `2050`,
                                                   key = "Year", value = "flo") %>%
  categorize()

h2tdemissions <- h2td %>%
  filter(Commodity %in% c("INDCH4","INDCO","INDCO2","INDN2O","INDNOX","INDPM10","INDVOC"))

h2tdinput <- h2td %>%
  filter(Commodity %in% c("INDDSL","INDELC"))

h2tdtype <- h2td %>%
  filter(Commodity %in% c("H2LIQPUMP","H2GASPUMP"))

## ----system emissions---------------------------------------------------

co2 <- as.data.frame(data_global$CO2) %>%
  select(-`2011`) %>%
  gather(`2010`, `2015`, `2020`, `2025`, `2030`, `2035`, `2040`, `2045`, `2050`,
         key = "Year", value = "flo") %>%
  categorize()

co2sum <- co2 %>%
  filter(Commodity != "BIOECO2") %>%
  group_by(Scenario,Year,nz,trn,fcev,bev,td,fut,pem) %>%
  summarize(flo = sum(flo)) %>%
  ungroup

nox <- as.data.frame(data_global$NOx) %>%
  select(-`2011`) %>%
  gather(`2010`, `2015`, `2020`, `2025`, `2030`, `2035`, `2040`, `2045`, `2050`,
         key = "Year", value = "flo") %>%
  categorize()

noxsum <- nox %>%
  group_by(Scenario,Year,nz,trn,fcev,bev,td,fut,pem) %>%
  summarize(flo = sum(flo)) %>%
  ungroup

so2 <- as.data.frame(data_global$SO2) %>%
  select(-`2011`) %>%
  gather(`2010`, `2015`, `2020`, `2025`, `2030`, `2035`, `2040`, `2045`, `2050`,
         key = "Year", value = "flo") %>%
  categorize()

so2sum <- so2 %>%
  group_by(Scenario,Year,nz,trn,fcev,bev,td,fut,pem) %>%
  summarize(flo = sum(flo)) %>%
  ungroup

pm10 <- as.data.frame(data_global$PM10) %>%
  select(-`2011`) %>%
  gather(`2010`, `2015`, `2020`, `2025`, `2030`, `2035`, `2040`, `2045`, `2050`,
         key = "Year", value = "flo") %>%
  categorize()

pm10sum <- pm10 %>%
  group_by(Scenario,Year,nz,trn,fcev,bev,td,fut,pem) %>%
  summarize(flo = sum(flo)) %>%
  ungroup


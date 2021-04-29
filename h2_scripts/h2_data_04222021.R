## ----import data------------------------------------------

# points to the excel file of output from VEDA_BE, reads in the names of the sheets,
# and plugs the spreadsheet into the function I created to turn all sheets into
# separate dataframes in the current environment

results <- "h2_rawdata/results_20210419-152131.xlsx"
data_global <- ReadAllSheets(here(results))

## ----carol's excel data jpeg---------------------------------------------------

excel_image <- "h2_rawdata/phase1excel.JPG"

## ----Cases ran------------------------------------------

cases <- as.data.frame(data_global$casenames) %>% 
  select(-Case, -`Transportation CO2 net-zero by [trn]`) %>%
  filter(`System CO2 net-zero by [nz]` == "2060")

## ----Scenarios------------------------------------------

scenarios <- as.data.frame(data_global$scenarios) %>% select(-Case)

## ----dummies------------------------------------------

dummy <- as.data.frame(data_global$`Dummy Check`) %>% 
  gather(`2010`, `2015`, `2020`, 
         key = "Year", value = "dummy") %>%
  categorize() %>%
  filter(nz == "2060")

## ----h2 capacity------------------------------------------------------

 h2cap <- as.data.frame(data_global$`H2 Production Capacity`) %>% 
  gather(`2035`, `2040`, `2045`, `2050`,
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
    TRUE ~ "Other")) %>%
  filter(nz == "2060")

h2cap %>% filter(cap > 0) %>% filter(Fuel == "Natural Gas CCS") %>% view()

## ----h2 fuels------------------------------------------------------

h2fuel <- as.data.frame(data_global$`H2 Production Fuels`) %>%
  gather(`2035`, `2040`, `2045`, `2050`,
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
    TRUE ~ "Other")) %>%
  filter(nz == "2060")

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
  ungroup() %>%
  mutate(station500 = flo/0.05) %>%
  mutate(station1200 = flo/0.37) %>%
  mutate(station2000 = flo/1.38) %>%
  filter(nz == "2060")

stations <- data.frame(`Station Capacity` = c("500 kg/day", "1200 kg/day", "2000 kg/day"),
                       `Stations` = c(max(h2prod_sum$station500),
                                  max(h2prod_sum$station1200),
                                  max(h2prod_sum$station2000))) %>% 
  kbl() %>% kable_styling()

## ----h2 t&d---------------------------------------------------

h2td <- as.data.frame(data_global$`H2 T&D`) %>%
  gather(`2035`, `2040`, `2045`, `2050`, key = "Year", value = "flo") %>%
  categorize() %>%
  filter(nz == "2060")

h2tdemissions <- h2td %>%
  filter(Commodity %in% c("INDCH4","INDCO","INDCO2","INDN2O","INDNOX","INDPM10","INDVOC"))

h2tdinput <- h2td %>%
  filter(Commodity %in% c("INDDSL","INDELC"))

h2tdtype <- h2td %>%
  filter(Commodity %in% c("H2LIQPUMP","H2GASPUMP"))

## ----elc produced---------------------------------------------------

elcprod <- as.data.frame(data_global$`ELC Out`) %>%
  select(-`2011`) %>%
  gather(`2010`,`2015`,`2020`,`2025`,`2030`,`2035`, `2040`, `2045`, `2050`, 
         key = "Year", value = "elc") %>%
  categorize() %>%
  elcprocess() %>%
  group_by(Processset,Scenario,Year,nz,trn,fcev,bev,td,fut,pem) %>%
  summarize(elc = sum(elc)) %>%
  filter(nz == "2060")

## ----transportation---------------------------------------------------

trn <- as.data.frame(data_global$`Transportation Summary SHORT`) %>%
  select(-`2011`) %>%
  gather(`2010`,`2015`,`2020`,`2025`,`2030`,`2035`, `2040`, `2045`, `2050`, 
         key = "Year", value = "bnVMT") %>%
  categorize() %>%
  fuel() %>%
  trnsector() %>%
  group_by(Scenario,Year,nz,fcev,bev,td,fut,pem,Fuel,trnsector) %>%
  summarize(bnVMT = sum(bnVMT)) %>%
  filter(nz == "2060")

## ----storage---------------------------------------------------

storage <- as.data.frame(data_global$`Storage`) %>%
  gather(`2015`,`2020`,`2025`,`2030`,`2035`, `2040`, `2045`, `2050`, 
         key = "Year", value = "mcost") %>%
  categorize() %>%
  filter(nz == "2060")

## ----h2 commodity marginals---------------------------------------------------

h2marginals <- as.data.frame(data_global$`H2 Commodity Marginals`) %>%
  select(-`2011`) %>%
  gather(`2010`,`2015`,`2020`,`2025`,`2030`,`2035`, `2040`, `2045`, `2050`, 
         key = "Year", value = "mcost") %>%
  categorize() %>%
  filter(nz == "2060")

## ----h2 fuel use---------------------------------------------------

h2fueluse <- as.data.frame(data_global$`H2 Transportation Fuel Use`) %>%
  gather(`2035`, `2040`, `2045`, `2050`, 
         key = "Year", value = "FIn") %>%
  categorize() %>%
  mutate(sector = case_when(
    str_detect(Commodity, "TB") ~ "TB",
    str_detect(Commodity, "TC") ~ "TC",
    str_detect(Commodity, "TH") ~ "TH",
    str_detect(Commodity, "TL") ~ "TL",
    str_detect(Commodity, "TM") ~ "TM",
    str_detect(Commodity, "TR") ~ "TR",
    str_detect(Commodity, "TS") ~ "TS",
    TRUE ~ "Other")) %>%
  filter(nz == "2060")

## ----refineries---------------------------------------------------

refineries <- as.data.frame(data_global$`Refinery Summary`) %>%
  select(-`2011`) %>%
  gather(`2010`,`2015`,`2020`,`2025`,`2030`,`2035`, `2040`, `2045`, `2050`, 
         key = "Year", value = "FOut") %>%
  categorize() %>%
  mutate(Commodity = case_when(
    str_detect(Commodity, "ASP") ~ "Asphalt",
    str_detect(Commodity, "DSH") ~ "Distillate Heating Oil",
    str_detect(Commodity, "DSU") ~ "Highway Diesel",
    str_detect(Commodity, "GSC") ~ "Conventional Gasoline",
    str_detect(Commodity, "GSR") ~ "Reformulated Gasoline",
    str_detect(Commodity, "JTF") ~ "Jet Fuel",
    str_detect(Commodity, "LPG") ~ "Propane",
    str_detect(Commodity, "PFS") ~ "Petrochemical Feedstocks",
    str_detect(Commodity, "PTC") ~ "Petroleum Coke",
    str_detect(Commodity, "RFH") ~ "Residual Fuel Oil: High Sulfur",
    str_detect(Commodity, "RFL") ~ "Residual Fuel Oil: Low Sulfur",
    TRUE ~ "Other")) 

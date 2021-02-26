## ----import data------------------------------------------

# points to the excel file of output from VEDA_BE, reads in the names of the sheets,
# and plugs the spreadsheet into the function I created to turn all sheets into
# separate dataframes in the current environment

results <- "h2_rawdata/results11022020.xlsx"
data_global <- ReadAllSheets(here(results))

## ----dummies------------------------------------------
dummy <- as.data.frame(data_global$`dummy`) %>% 
  gather(`2010`, `2015`, `2020`, `2025`, `2030`, `2035`, `2040`, `2045`, `2050`, 
         key = "Year", value = "dummy") %>%
  categorize() %>%
  group_by(Scenario,Commodity,Year,prod,qnt,tech,elc,rps) %>%
  summarize(dummy = sum(dummy)) %>% 
  ungroup()


## ----co2 emissions------------------------------------------

co2emissions <- as.data.frame(data_global$`co2`) %>%
  select(-`2011`) %>%
  gather(`2010`, `2015`, `2020`, `2025`, `2030`, `2035`, `2040`, `2045`, `2050`,
         key = "Year", value = "emissions") %>%
  categorize() 

## ----pm2.5 emissions------------------------------------------

pm2.5emissions <- as.data.frame(data_global$`pm2.5`) %>%
  select(-`2011`) %>%
  gather(`2010`, `2015`, `2020`, `2025`, `2030`, `2035`, `2040`, `2045`, `2050`,
         key = "Year", value = "emissions") %>%
  categorize() 

## ----pm10 emissions------------------------------------------

pm10emissions <- as.data.frame(data_global$`pm10`) %>%
  select(-`2011`) %>%
  gather(`2010`, `2015`, `2020`, `2025`, `2030`, `2035`, `2040`, `2045`, `2050`,
         key = "Year", value = "emissions") %>%
  categorize()

## ----ch4 emissions------------------------------------------

ch4emissions <- as.data.frame(data_global$`ch4`) %>%
  select(-`2011`) %>%
  gather(`2010`, `2015`, `2020`, `2025`, `2030`, `2035`, `2040`, `2045`, `2050`,
         key = "Year", value = "emissions") %>%
  categorize() 

## ----so2 emissions------------------------------------------

so2emissions <- as.data.frame(data_global$`so2`) %>% 
  select(-`2011`) %>%
  gather(`2010`, `2015`, `2020`, `2025`, `2030`, `2035`, `2040`, `2045`, `2050`, 
         key = "Year", value = "emissions") %>%
  categorize() 

## ----nox emissions------------------------------------------

noxemissions <- as.data.frame(data_global$`nox`) %>% 
  select(-`2011`) %>%
  gather(`2010`, `2015`, `2020`, `2025`, `2030`, `2035`, `2040`, `2045`, `2050`, 
         key = "Year", value = "emissions") %>%
  categorize()

## ----rnwelc------------------------------------------

rnwelc_reg <- as.data.frame(data_global$`rnwelc`) %>%
  gather(`2015`, `2020`, `2025`, `2030`, `2035`, `2040`, `2045`, `2050`,
         key = "Year", value = "PJ") %>%
  categorize() %>%
  elcprocess()

rnwelc <- rnwelc_reg %>%
  group_by(Processset,Scenario,Year,prod,qnt,tech,elc,rps) %>%
  summarize(PJ = sum(PJ)) %>% 
  ungroup()

## ----elc production------------------------------------------

elc_reg <- as.data.frame(data_global$`ELC Summary`) %>%
  select(-`2011`) %>%
  gather(`2010`, `2015`, `2020`, `2025`, `2030`, `2035`, `2040`, `2045`, `2050`, 
         key = "Year", value = "PJ") %>%
  filter(Attribute == "VAR_FOut") %>%
  filter(Commodity == "ELC") %>%
  categorize() %>%
  elcprocess() %>%
  group_by(Processset,Scenario,Year,Region,prod,qnt,tech,elc,rps) %>%
  summarise(PJ = sum(PJ)) %>% 
  ungroup() %>%
  filter(Processset != "Other")

elc_reg_total <- elc_reg %>%
  group_by(Scenario,Year,Region,prod,qnt,tech,elc,rps) %>%
  summarise(PJ = sum(PJ)) %>% 
  ungroup()

elc <- elc_reg %>%
  group_by(Processset,Scenario,Year,prod,qnt,tech,elc,rps) %>%
  summarise(PJ = sum(PJ)) %>% 
  ungroup()


## ----h2 production capacity------------------------------------------------------

h2techcap <- as.data.frame(data_global$H2ProdCap) %>%
  gather(`2015`, `2020`, `2025`, `2030`, `2035`, `2040`, `2045`, `2050`, 
         key = "Year", value = "PJ") %>%
  filter(Attribute == "VAR_Cap") %>%
  categorize() %>%
  group_by(Scenario,Process,Year) %>%
  summarize(PJ = sum(PJ)) %>% 
  ungroup()

h2cumcap <- h2techcap %>%
  ungroup() %>%
  group_by(Scenario,Year) %>%
  summarize(PJ = sum(PJ)) %>% 
  ungroup()
  

## ----h2 fuels------------------------------------------------------

h2prodfuel <- as.data.frame(data_global$H2ProdFuels) %>%
  gather(`2015`, `2020`, `2025`, `2030`, `2035`, `2040`, `2045`, `2050`, 
         key = "Year", value = "flo") %>%
  categorize() %>%
  mutate(Process = factor(Process, levels = c("H2CCBG","H2CCCCCS","H2CCNGA","H2CFNGA",
                                              "H2CCEPEM","H2CFEPEM")))

h2emissions <- h2prodfuel %>%
  filter(Commodity %in% c("ELCH2OW","ELCH2OC","CO2S","INDCO2")) %>%
  group_by(Scenario,Commodity,Year,prod,qnt,tech,elc,rps) %>%
  summarize(flo = sum(flo)) %>% 
  ungroup()

h2inputs <- h2prodfuel %>%
  filter(Commodity %in% c("INDBIO","INDCOA","INDNGA","INDELC","RNWELC")) %>%
  group_by(Scenario,Commodity,Year,prod,qnt,tech,elc,rps) %>%
  summarize(flo = sum(flo)) %>% 
  ungroup()

h2prod_reg <- h2prodfuel %>%
  filter(Commodity == "H2PROD") %>%
  mutate(PJ = flo) %>%
  group_by(Scenario,Commodity,Process,Region,Year,prod,qnt,tech,elc,rps) %>%
  summarize(PJ = sum(PJ)) %>% 
  ungroup()

h2prod <- h2prod_reg %>%
  group_by(Scenario,Commodity,Process,Year,prod,qnt,tech,elc,rps) %>%
  summarize(PJ = sum(PJ)) %>% 
  ungroup()

h2prod_cum_reg <- h2prod_reg %>%
  group_by(Scenario,Commodity,Year,Region,prod,qnt,tech,elc,rps) %>%
  summarize(PJ = sum(PJ)) %>% 
  ungroup()

h2prod_cum <- h2prod_cum_reg %>%
  group_by(Scenario,Commodity,Year,prod,qnt,tech,elc,rps) %>%
  summarize(PJ = sum(PJ)) %>% 
  ungroup()

## ----water use---------------------------------------------------

elc_h2o_proc_com <- as.data.frame(data_global$`ELC Summary`) %>%
  select(-`2011`) %>%
  gather(`2010`, `2015`, `2020`, `2025`, `2030`, `2035`, `2040`, `2045`, `2050`, 
         key = "Year", value = "elcuse") %>%
  filter(Attribute == "VAR_FOut") %>%
  filter(Commodity %in% c("ELCH2OW", "ELCH2OC")) %>%
  categorize() %>%
  elcprocess() %>%
  filter(!Processset %in% c("Other","Solar")) %>%
  group_by(Processset,Scenario,Commodity,Year,prod,qnt,tech,elc,rps) %>%
  summarise(elcuse = sum(elcuse)) %>% 
  ungroup() 

h2_h2o_proc_com <- as.data.frame(data_global$H2ProdFuels) %>%
  gather(`2015`, `2020`, `2025`, `2030`, `2035`, `2040`, `2045`, `2050`, 
         key = "Year", value = "h2o") %>%
  filter(Attribute == "VAR_FOut") %>%
  filter(Commodity %in% c("ELCH2OW", "ELCH2OC")) %>%
  categorize() %>%
  group_by(Process,Scenario,Commodity,Year,prod,qnt,tech,elc,rps) %>%
  summarise(h2o = sum(h2o)) %>% 
  ungroup() 

elc_h2o_com <- elc_h2o_proc_com %>%
  group_by(Scenario,Commodity,Year,prod,qnt,tech,elc,rps) %>%
  summarise(elcuse = sum(elcuse)) %>% 
  ungroup()

h2_h2o_com <- h2_h2o_proc_com %>%
  group_by(Scenario,Commodity,Year,prod,qnt,tech,elc,rps) %>%
  summarise(h2o = sum(h2o)) %>% 
  ungroup()

h2o_com <- left_join(elc_h2o_com, h2_h2o_com, 
                     by = c("Scenario","Commodity","Year","prod","qnt","tech","elc","rps")) %>%
  mutate_all(~replace(.,is.na(.),0))

elc_h2o <- elc_h2o_com %>%
  group_by(Scenario,Year,prod,qnt,tech,elc,rps) %>%
  summarise(elcuse = sum(elcuse)) %>% 
  ungroup()

h2_h2o <- h2_h2o_com %>%
  group_by(Scenario,Year,prod,qnt,tech,elc,rps) %>%
  summarise(h2o = sum(h2o)) %>% 
  ungroup()

h2o <- left_join(elc_h2o, h2_h2o, 
                     by = c("Scenario","Year","prod","qnt","tech","elc","rps")) %>%
  mutate_all(~replace(.,is.na(.),0))

## ----h2 transportation use---------------------------------------------------

h2trn <- as.data.frame(data_global$`H2 Transportation Use`) %>%
  gather(`2030`, `2035`, `2040`, `2045`, `2050`, 
         key = "Year", value = "bnVMT") %>%
  filter(Attribute == "VAR_FIn") %>%
  filter(Process %in% c("THH2","TLFH2","TLCH2","TLTCOH2","TLVH2")) %>%
  categorize() %>%
  group_by(Scenario,Process,Commodity,Year,prod,qnt,tech,elc,rps) %>%
  summarize(bnVMT = sum(bnVMT)) %>% 
  ungroup()

## ----trn sector co2 emissions---------------------------------------------------

trnco2 <- as.data.frame(data_global$trnCO2emissions) %>%
  select(-`2011`) %>%
  gather(`2010`, `2015`, `2020`, `2025`, `2030`, `2035`, `2040`, `2045`, `2050`, 
         key = "Year", value = "emissions") %>%
  categorize() %>%
  group_by(Scenario,Commodity,Year,prod,qnt,tech,elc,rps) %>%
  summarize(emissions = sum(emissions)) %>% 
  ungroup()
  

## ----h2 vehicles in transportation sector -----------------------------------

h2car <- as.data.frame(data_global$TransportationSummary) %>%
  select(-`2011`) %>%
  gather(`2010`, `2015`, `2020`, `2025`, `2030`, `2035`, `2040`, `2045`, `2050`, 
         key = "Year", value = "bnVMT") %>%
  categorize() %>%
  group_by(Commodity,Processset,Scenario,Year,prod,qnt,tech,elc,rps) %>%
  summarize(bnVMT = sum(bnVMT)) %>% 
  ungroup()

thDSLcombo <- h2car %>% filter(Processset %in% c("TH-B20X", "TH-DSL")) %>%
  group_by(Commodity,Scenario,Year,prod,qnt,tech,elc,rps) %>%
  summarize(bnVMT = sum(bnVMT)) %>%
  mutate(Processset = "TH-DSL") %>%
  select(Commodity,Processset,Scenario,Year,bnVMT,prod,qnt,tech,elc,rps) %>%
  ungroup()

tlGSLcombo <- h2car %>% filter(Processset %in% c("TL-E85X","TL-GSL")) %>%
  group_by(Commodity,Scenario,Year,prod,qnt,tech,elc,rps) %>%
  summarize(bnVMT = sum(bnVMT)) %>%
  mutate(Processset = "TL-GSL") %>%
  select(Commodity,Processset,Scenario,Year,bnVMT,prod,qnt,tech,elc,rps) %>%
  ungroup()

h2car_small <- h2car %>%
  filter(!Processset %in% c("TH-B20X", "TH-DSL","TL-E85X","TL-GSL")) %>%
  select(Commodity,Processset,Scenario,Year,bnVMT,prod,qnt,tech,elc,rps)

h2carcombo <- rbind.data.frame(h2car_small,thDSLcombo,tlGSLcombo)

h2totalcar <- h2carcombo %>%
  group_by(Commodity,Scenario,Year,prod,qnt,tech,elc,rps) %>%
  summarize(totVMT = sum(bnVMT)) %>%
  ungroup() %>%
  left_join(h2carcombo, by = c("Scenario","Commodity","Year","prod","qnt","tech","elc","rps")) %>%
  mutate(mktshr = round((bnVMT/totVMT),2)) %>%
  select(Scenario,prod,qnt,tech,elc,rps,Commodity,Processset,Year,totVMT,bnVMT,mktshr) %>% 
  ungroup()
  
  
  
  

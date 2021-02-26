## ----import data------------------------------------------

# points to the excel file of output from VEDA_BE, reads in the names of the sheets,
# and plugs the spreadsheet into the function I created to turn all sheets into
# separate dataframes in the current environment

results <- "h2_rawdata/results02162021.xlsx"
data_global <- ReadAllSheets(here(results))

## ----PEM Costs------------------------------------------

pemcost <- as.data.frame(data_global$PEMcost) %>% 
  filter(Version == "Delta") %>%
  select(-TechID, -Version) %>%
  mutate_each(funs(percent), -Tech) %>%
  mutate_if(is.numeric, ~round(.,2)) 

## ----dummies------------------------------------------

dummy <- as.data.frame(data_global$`Dummy`) %>% 
  gather(`2010`, `2015`, `2020`, 
         key = "Year", value = "dummy") %>%
  categorize() %>%
  group_by(Scenario,Commodity,Year,dist,scen,td) %>%
  summarize(dummy = sum(dummy)) %>% 
  ungroup()


## ----co2 emissions------------------------------------------

co2emissions <- as.data.frame(data_global$CO2Emissions) %>%
  select(-`2011`) %>%
  gather(`2010`, `2015`, `2020`, `2025`, `2030`, `2035`, `2040`, `2045`, `2050`,
         key = "Year", value = "emissions") %>%
  categorize() %>%
  group_by(Scenario,Commodity,Year,dist,scen,td) %>%
  summarize(emissions = sum(emissions))


## ----h2 production capacity------------------------------------------------------

h2techcap_reg <- as.data.frame(data_global$H2ProdCap) %>%
  gather(`2025`, `2030`, `2035`, `2040`, `2045`, `2050`, 
         key = "Year", value = "PJ") %>%
  filter(Attribute == "VAR_Cap") %>%
  categorize() 

h2techcap <- h2techcap_reg %>%
  group_by(Scenario,Process,Year,dist,scen,td) %>%
  summarize(PJ = sum(PJ)) %>% 
  ungroup()

h2cumcap_reg <- h2techcap_reg %>%
  group_by(Scenario,Year,Region,dist,scen,td) %>%
  summarize(PJ = sum(PJ)) %>% 
  ungroup()

h2cumcap <- h2techcap %>%
  group_by(Scenario,Year,dist,scen,td) %>%
  summarize(PJ = sum(PJ)) %>% 
  ungroup()
  

## ----h2 fuels------------------------------------------------------

h2prodfuel <- as.data.frame(data_global$H2ProdFuel) %>%
  gather(`2025`, `2030`, `2035`, `2040`, `2045`, `2050`, 
         key = "Year", value = "flo") %>%
  categorize() %>%
  mutate(Process = factor(Process, levels = c("H2CCCCCS","H2CCNGA","H2CCNGACCS",
                                              "H2CFNGA","H2CFBG","H2CFNGACCS","H2CFEPEM","H2CFESO",
                                              "H2DCNGA","H2DCETH",
                                              "H2DFNGA","H2DFETH","H2DFEPEM")))

h2emissions <- h2prodfuel %>%
  filter(Commodity %in% c("ELCH2OW","ELCH2OC","CO2S","INDCO2")) %>%
  group_by(Scenario,Commodity,Year,scen,dist,td) %>%
  summarize(flo = sum(flo)) %>% 
  ungroup()

h2inputs <- h2prodfuel %>%
  filter(Commodity %in% c("INDBIO","INDCOA","INDNGA","INDELC")) %>%
  group_by(Scenario,Commodity,Year,scen,dist,td) %>%
  summarize(flo = sum(flo)) %>% 
  ungroup()

h2prod_reg <- h2prodfuel %>%
  filter(Commodity == "H2PROD") %>%
  mutate(PJ = flo) %>%
  group_by(Scenario,Commodity,Process,Region,Year,dist,scen,td) %>%
  summarize(PJ = sum(PJ)) %>% 
  ungroup()

h2prod <- h2prod_reg %>%
  group_by(Scenario,Commodity,Process,Year,dist,scen,td) %>%
  summarize(PJ = sum(PJ)) %>% 
  ungroup()

h2prod_cum_reg <- h2prod_reg %>%
  group_by(Scenario,Commodity,Year,Region,dist,scen,td) %>%
  summarize(PJ = sum(PJ)) %>% 
  ungroup()

h2prod_cum <- h2prod_cum_reg %>%
  group_by(Scenario,Commodity,Year,dist,scen,td) %>%
  summarize(PJ = sum(PJ)) %>% 
  ungroup()

h2pump_reg <- h2prodfuel %>%
  filter(Commodity == "H2PUMP") %>%
  mutate(PJ = flo) %>%
  group_by(Scenario,Commodity,Process,Region,Year,dist,scen,td) %>%
  summarize(PJ = sum(PJ)) %>% 
  ungroup()

h2pump <- h2pump_reg %>%
  group_by(Scenario,Commodity,Process,Year,dist,scen,td) %>%
  summarize(PJ = sum(PJ)) %>% 
  ungroup()

h2pump_cum_reg <- h2pump_reg %>%
  group_by(Scenario,Commodity,Year,Region,dist,scen,td) %>%
  summarize(PJ = sum(PJ)) %>% 
  ungroup()

h2pump_cum <- h2pump_cum_reg %>%
  group_by(Scenario,Commodity,Year,dist,scen,td) %>%
  summarize(PJ = sum(PJ)) %>% 
  ungroup()

## ----h2 transportation use---------------------------------------------------

h2trn <- as.data.frame(data_global$`H2TrnUse`) %>%
  gather(`2025`,`2030`, `2035`, `2040`, `2045`, `2050`, 
         key = "Year", value = "bnVMT") %>%
  filter(Attribute == "VAR_FIn") %>%
  filter(Process %in% c("THH2","TLFH2","TLCH2","TBTH2FC")) %>%
  categorize() %>%
  group_by(Scenario,Process,Commodity,Year,dist,scen,td) %>%
  summarize(bnVMT = sum(bnVMT)) %>% 
  ungroup()

## ----trn sector co2 emissions---------------------------------------------------

trnco2 <- as.data.frame(data_global$TrnCO2) %>%
  select(-`2011`) %>%
  gather(`2010`, `2015`, `2020`, `2025`, `2030`, `2035`, `2040`, `2045`, `2050`, 
         key = "Year", value = "emissions") %>%
  categorize() %>%
  group_by(Scenario,Commodity,Year,dist,scen,td) %>%
  summarize(emissions = sum(emissions)) %>% 
  ungroup()
  
## ----h2 t&d---------------------------------------------------


h2td_reg <- as.data.frame(data_global$H2TD) %>%
  gather(`2025`, `2030`, `2035`, `2040`, `2045`, `2050`, 
         key = "Year", value = "flo") %>%
  categorize() %>%
  filter(Commodity %in% c("H2PUMP","H2LIQPUMP","H2GASPUMP","H2PROD")) %>%
  filter(Attribute == "VAR_FIn")

h2td <- h2td_reg %>%
  group_by(Scenario,Commodity,Process,Attribute,Year,dist,scen,td) %>%
  summarize(flo = sum(flo)) %>%
  ungroup()

## ----tl and th co2---------------------------------------------------

tlthco2_process <- as.data.frame(data_global$TLTHCO2) %>%
  select(-`2011`) %>%
  gather(`2010`, `2015`, `2020`, `2025`, `2030`, `2035`, `2040`, `2045`, `2050`, 
         key = "Year", value = "emissions") %>%
  categorize()

tlthco2 <- tlthco2_process %>%
  group_by(Commodity,Scenario,Year,scen,dist,td) %>%
  summarize(emissions = sum(emissions)) %>%
  ungroup()

## ----t&d cost cases---------------------------------------------------

tdcost_tech <- as.data.frame(data_global$h2prodTD) %>%
  gather(`2025`, `2030`, `2035`, `2040`, `2045`, `2050`, 
         key = "Year", value = "PJ") %>%
  categorize()

tdcost_cum <- tdcost_tech %>%
  group_by(Scenario,Year,dist,scen,td) %>%
  summarize(PJ = sum(PJ)) %>% 
  ungroup()

## ----t&d data---------------------------------------------------

hdsam <- as.data.frame(data_global$HDSAM)
round(hdsam$`LCOE $/kg H2`,2)
paste('$',formatC(hdsam$`LCOE $/kg H2`, big.mark=',', format = 'f'))
h2a <- as.data.frame(data_global$H2A)
round(h2a$`LCOE $/kg H2`,2)
paste('$',formatC(h2a$`LCOE $/kg H2`, big.mark=',', format = 'f'))
doe <- as.data.frame(data_global$DOE)
round(doe$`LCOE $/kg H2`,2)
paste('$',formatC(doe$`LCOE $/kg H2`, big.mark=',', format = 'f'))
tdcosts <- as.data.frame(data_global$tdcosts)
round(tdcosts$`LCOE $/kg H2`,2)
paste('$',formatC(tdcosts$`LCOE $/kg H2`, big.mark=',', format = 'f'))

allcosts <- as.data.frame(data_global$allcost)
  




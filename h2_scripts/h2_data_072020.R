## ----import data------------------------------------------

# points to the excel file of output from VEDA_BE, reads in the names of the sheets,
# and plugs the spreadsheet into the function I created to turn all sheets into
# separate dataframes in the current environment

results <- "h2_rawdata/h2data_072020.xlsx"
data_global <- ReadAllSheets(results)

## ----co2 emissions------------------------------------------

co2emissions_reg <- as.data.frame(data_global$`CO2 Emissions`) %>% 
  select(-`2011`,-`Attribute`) %>%
  gather(`2010`, `2015`, `2020`, `2025`, `2030`, `2035`, `2040`, `2045`, `2050`, 
         key = "Year", value = "emissions") %>%
  filter(Commodity != "ETHCO2") %>%
  mutate_if(is.numeric, ~round(.,2)) %>%
  mutate_all(~replace(.,is.na(.),0))

co2emissions <- co2emissions_reg %>%
  group_by(Scenario, Commodity, Year) %>%
  summarize(emissions = sum(emissions))


## ----h2 production------------------------------------------------------

h2prod_cap_reg <- as.data.frame(data_global$`H2 Production Capacity`) %>% 
  gather(`2015`, `2020`, `2025`, `2030`, `2035`, `2040`, `2045`, `2050`, 
         key = "Year", value = "cap") %>% 
  mutate_if(is.numeric, ~round(.,2)) %>%
  mutate_all(~replace(.,is.na(.),0))

h2prod_cumcap_reg <- h2prod_cap_reg %>%
  group_by(Scenario, Year, Attribute, Region) %>%
  summarize(cap = sum(cap)) %>%
  ungroup()

h2prod_cap <- h2prod_cap_reg %>%
  group_by(Scenario, Year, Attribute, Process) %>%
  summarize(cap = sum(cap)) %>%
  ungroup()

h2prod_cumcap <- h2prod_cap %>%
  group_by(Scenario,Year,Attribute) %>%
  summarize(cap = sum(cap)) %>%
  ungroup()

## ----h2 production - fuel use------------------------------------------------------

h2prodfuel_reg <- as.data.frame(data_global$`H2 Production Fuels`) %>% 
  gather(`2015`, `2020`, `2025`, `2030`, `2035`, `2040`, `2045`, `2050`, 
         key = "Year", value = "use") %>% 
  mutate_if(is.numeric, ~round(.,2)) %>%
  mutate_all(~replace(.,is.na(.),0))

h2prodfuel <- h2prodfuel_reg %>%
  group_by(Scenario, Process, Commodity, Year, Attribute) %>%
  summarize(use = sum(use)) %>%
  ungroup()

## ----h2 t&d------------------------------------------------------

h2td_reg <- as.data.frame(data_global$`H2 T&D`) %>%
  gather(`2015`, `2020`, `2025`, `2030`, `2035`, `2040`, `2045`, `2050`, 
         key = "Year", value = "use") %>% 
  mutate_if(is.numeric, ~round(.,2)) %>%
  mutate_all(~replace(.,is.na(.),0))
h2td_reg$Year <- as.numeric(h2td_reg$Year)

h2td <- h2td_reg %>%
  group_by(Scenario, Process, Commodity, Year) %>%
  summarize(use = sum(use)) %>%
  ungroup()

## ----h2 in transportation------------------------------------------------------

h2trn_reg <- as.data.frame(data_global$`H2 Transportation Use`) %>%
  gather(`2015`, `2020`, `2025`, `2030`, `2035`, `2040`, `2045`, `2050`, 
         key = "Year", value = "PJ") %>% 
  mutate_if(is.numeric, ~round(.,2)) %>%
  mutate_all(~replace(.,is.na(.),0))
h2trn_reg$Year <- as.numeric(h2trn_reg$Year)

h2trn <- h2trn_reg %>%
  group_by(Scenario, Commodity, Process, Attribute, Year) %>%
  summarize(PJ = sum(PJ)) %>%
  ungroup()

## ----transportation mix------------------------------------------------------

trn_reg <- as.data.frame(data_global$`Transportation Summary`) %>%
  select(-`2011`) %>%
  gather(`2010`, `2015`, `2020`, `2025`, `2030`, `2035`, `2040`, `2045`, `2050`, 
         key = "Year", value = "PJ") %>% 
  mutate_if(is.numeric, ~round(.,2)) %>%
  mutate_all(~replace(.,is.na(.),0))
trn_reg$Year <- as.numeric(trn_reg$Year)

trn <- trn_reg %>%
  group_by(Scenario, Attribute, ProcessSet, Year) %>%
  summarize(PJ = sum(PJ)) %>%
  ungroup()




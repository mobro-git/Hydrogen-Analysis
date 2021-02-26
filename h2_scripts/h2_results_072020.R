## Load Setup and Data Scripts ----

# allows me to quickly run my two previous scripts when I make changes in them or
# if I'm just going to work with results and need to pull in the data

# source("h2_Scripts/h2_setup_072020.R")
# source("h2_Scripts/h2_data_072020.R")

## CO2 Emissions ----

co2emissions_graph <- co2emissions %>% 
  filter(Commodity %in% c("COMCO2", "ELCCO2", "RESCO2", "TRNCO2", "INDCO2")) %>%
  ggplot() +
  geom_line(aes(x = Year, y = emissions, color = Scenario, group = Scenario)) +
  facet_wrap(~Commodity, scales = "free_y") +
  labs(x = "Year", y = "CO2 Emissions (Mt)", title = "CO2 Emissions by Scenario")
  

## H2 Production ----

## ~ Total Capacity ----

# separated by technology

h2prod_totalcap_tech <- h2prod_cap %>% filter(Attribute == "VAR_Cap") %>%
  ggplot() +
  geom_line(aes(x = Year, y = cap, color = Scenario, group = Scenario)) +
  facet_wrap(~Process, scales = "free_y") +
  labs(x = "Year", y = "Total Capacity (PJ)", title = "H2 Total Production by Technology")

# cumulative over all technologies

h2prod_totalcap <- h2prod_cumcap %>% filter(Attribute == "VAR_Cap") %>%
  ggplot() +
  geom_line(aes(x = Year, y = cap, color = Scenario, group = Scenario)) +
  labs(x = "Year", y = "Total Capacity (PJ)", title = "H2 Total Production by Scenario")

## ~ New Capacity ----

# separated by technology

h2prod_newcap_tech <- h2prod_cap %>% filter(Attribute == "VAR_Ncap") %>%
  ggplot() +
  geom_line(aes(x = Year, y = cap, color = Scenario, group = Scenario)) +
  facet_wrap(~Process, scales = "free_y") +
  labs(x = "Year", y = "New Capacity (PJ)", title = "H2 New Production Capacity by Technology")

# cumulative over all technologies

h2prod_newcap <- h2prod_cumcap %>% filter(Attribute == "VAR_Ncap") %>%
  ggplot() +
  geom_line(aes(x = Year, y = cap, color = Scenario, group = Scenario)) +
  labs(x = "Year", y = "New Capacity (PJ)", title = "H2 New Production by Scenario")


## ~ Fuel Use ----

h2fueluse <- h2prodfuel %>% 
  filter(Commodity %in% c("ELCH2OC", "ELCH2OW","INDELC","INDNGA")) %>%
  ggplot() +
  geom_line(aes(x = Year, y = use, color = Scenario, group = Scenario)) +
  facet_grid(Commodity~Process, scales = "free_y") +
  labs(y = "Fuel Use", title = "Fuel Use for H2 Production")

## ~ Production ----

h2prodbytech <- h2prodfuel %>% 
  filter(Commodity == "H2PROD") %>%
  ggplot() +
  geom_line(aes(x = Year, y = use, color = Scenario, group = Scenario)) +
  facet_grid(~Process, scales = "free_y") +
  labs(y = "PJ", title = "H2 Production by Technology")

## T&D----

h2td_graph <- h2td %>%
  ggplot() +
  geom_line(aes(x = Year, y = use, color = Scenario, group = Scenario)) +
  facet_grid(~Process)

## H2 in Transportation----

h2trn_type <- h2trn %>%
  filter(Commodity %in% c("TLH2", "THH2")) %>%
  filter(!Process %in% c("SCTHH2", "SCTLH2")) %>%
  ggplot() +
  geom_line(aes(x = Year, y = PJ, group = Scenario, color = Scenario)) +
  facet_grid(Commodity~Process)
  
## Transportation Mix----

trn_mix <- trn %>%
  filter(Attribute == "VAR_FIn") %>%
  filter(ProcessSet %in% c("TH-DSL", "TH-H2")) %>%
  select(-Attribute) %>%
  ggplot() +
  geom_line(aes(x = Year, y = PJ, color = Scenario, group = Scenario)) +
  facet_wrap(~ProcessSet)






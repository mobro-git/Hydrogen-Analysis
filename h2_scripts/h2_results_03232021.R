## Load Setup and Data Scripts ----

# allows me to quickly run my two previous scripts when I make changes in them or
# if I'm just going to work with results and need to pull in the data

# source("h2_Scripts/h2_setup_01062021.R")
# source("h2_Scripts/h2_data_01062021.R")

## Dummies -----

dummies <- dummy %>% 
  ggplot() +
  geom_line(aes(x = Year, y = dummy, color = nz, group = Scenario)) +
  labs(x = "Year", y = "Dummy Value", title = "Dummies") +
  yt +
  x_disc_l +
  bottom1

      # Dummies in 2050 case, but nz 2060 and 2080 solve fine

## CO2 Emissions -----

co2_enduse <- co2 %>%
  filter(Commodity %in% c("ELCCO2", "TRNCO2", "INDCO2","REFCO2")) %>%
  ggplot() +
  geom_line(aes(x = Year, y = flo, color = nz, group = Scenario), size = 0.75) +
  facet_wrap(~Commodity, scales = "free_y") +
  labs(x = "Year", y = "CO2 Emissions (Mt)", title = "CO2 Emissions by Scenario and Sector") +
  yt +
  x_disc_l +
  bottom1

      # mostly follows NZ trend, quiker to NZ, steeper decline in CO2 emissions
      # trn nz 2050 changes it up a little for 2050, a but more so for 2080

co2_2060 <- co2 %>%
  filter(nz == "2060") %>%
  filter(Commodity %in% c("ELCCO2", "TRNCO2", "INDCO2","REFCO2")) %>%
  ggplot() +
  geom_line(aes(x = Year, y = flo, color = fcev, group = Scenario), size = 0.75) +
  facet_grid(Commodity~bev, scales = "free_y") +
  labs(x = "Year", y = "CO2 Emissions (Mt)", title = "CO2 Emissions by Scenario and Sector") +
  yt +
  x_disc_l +
  bottom1

    # looking just at 2060, the majority of cases run, biggest difference comes from
    # trn nz 2050 case

## SO2 Emissions -----

so2_enduse <- so2 %>%
  filter(Commodity %in% c("ELCSO2", "TRNSO2", "INDSO2","REFSO2")) %>%
  ggplot() +
  geom_line(aes(x = Year, y = flo, color = nz, group = Scenario), size = 0.75) +
  facet_wrap(~Commodity, scales = "free_y") +
  labs(x = "Year", y = "SO2 Emissions (kt)", title = "SO2 Emissions by Scenario and Sector") +
  yt +
  x_disc_l +
  bottom1

    # mostly follows NZ trend, quiker to NZ, steeper decline in SO2 emissions

so2_2060 <- so2 %>%
  filter(nz == "2060") %>%
  filter(Commodity %in% c("ELCSO2", "TRNSO2", "INDSO2","REFSO2")) %>%
  ggplot() +
  geom_line(aes(x = Year, y = flo, color = fcev, group = Scenario), size = 0.75) +
  facet_grid(Commodity~bev, scales = "free_y") +
  labs(x = "Year", y = "SO2 Emissions (kt)", title = "SO2 Emissions by Scenario and Sector") +
  yt +
  x_disc_l +
  bottom1

    # ref fcev costs slows indso2 reductions but speeds up refso2 reductions

## NOx Emissions -----

nox_enduse <- nox %>%
  filter(Commodity %in% c("ELCNOX", "TRNNOX", "INDNOX","REFNOX")) %>%
  ggplot() +
  geom_line(aes(x = Year, y = flo, color = nz, group = Scenario), size = 0.75) +
  facet_wrap(~Commodity, scales = "free_y") +
  labs(x = "Year", y = "NOX Emissions (kt)", title = "NOX Emissions by Scenario and Sector") +
  yt +
  x_disc_l +
  bottom1

nox_2060 <- nox %>%
  filter(nz == "2060") %>%
  filter(Commodity %in% c("ELCNOX", "TRNNOX", "INDNOX","REFNOX")) %>%
  ggplot() +
  geom_line(aes(x = Year, y = flo, color = fcev, group = Scenario), size = 0.75) +
  facet_grid(Commodity~bev, scales = "free_y") +
  labs(x = "Year", y = "NOX Emissions (kt)", title = "NOX Emissions by Scenario and Sector") +
  yt +
  x_disc_l +
  bottom1

## PM10 Emissions -----

pm10_enduse <- pm10 %>%
  filter(Commodity %in% c("ELCPM10", "TRNPM10", "INDPM10","REFPM10")) %>%
  ggplot() +
  geom_line(aes(x = Year, y = flo, color = nz, group = Scenario), size = 0.75) +
  facet_wrap(~Commodity, scales = "free_y") +
  labs(x = "Year", y = "PM10 Emissions (kt)", title = "PM10 Emissions by Scenario and Sector") +
  yt +
  x_disc_l +
  bottom1

pm10_2060 <- pm10 %>%
  filter(nz == "2060") %>%
  filter(Commodity %in% c("ELCPM10", "TRNPM10", "INDPM10","REFPM10")) %>%
  ggplot() +
  geom_line(aes(x = Year, y = flo, color = fcev, group = Scenario), size = 0.75) +
  facet_grid(Commodity~bev, scales = "free_y") +
  labs(x = "Year", y = "PM10 Emissions (kt)", title = "PM10 Emissions by Scenario and Sector") +
  yt +
  x_disc_l +
  bottom1

## H2 Production -----

h2totalprod <- h2prod_sum %>%
  ggplot() +
  geom_line(aes(x = Year, y = flo, color = nz, group = Scenario), size = 0.75) +
  labs(x = "Year", y = "PJ H2", title = "Total H2 Production",color = "Net-zero CO2") +
  yt +
  x_disc_l +
  bottom1

      # nz2080 doesnt bring in h2, but if trn is forced to minimize h2 has best scenario
      # the longer the timeframe for emissions reductions, the better h2 does
      # nz cases are the big changes, ref with large h2-specific changes dont bring in a ton of h2

h2totalprod_tech <- h2prod %>%
  ggplot() +
  geom_line(aes(x = Year, y = flo, color = nz, group = Scenario), size = 0.75) +
  facet_grid(Type~Fuel, scales = "free_y") +
  labs(x = "Year", y = "PJ H2", title = "Total H2 Production") +
  yt +
  x_disc_l +
  bottom1

h2totalprod_tech_2060 <- h2prod %>%
  filter(nz == "2060") %>%
  filter(trn == "ref") %>%
  ggplot() +
  geom_line(aes(x = Year, y = flo, color = fcev, linetype = bev, group = Scenario), size = 0.75) +
  facet_grid(Fuel~Type+td) +
  labs(x = "Year", y = "PJ H2", title = "Total H2 Production: Net-zero CO2 by 2060", 
       linetype = "BEV Cost", color = "FCEV Cost",
       caption = "Note: Centralized Ethanol and Distributed Natural Gas with CCS are not options within the database") +
  yt +
  x_disc_l +
  bottom1


h2prod %>%
  filter(nz == "2060") %>%
  filter(trn == "ref") %>%
  filter(Fuel != "Ethanol") %>%
  ggplot() +
  geom_line(aes(x = Year, y = flo, color = fcev, linetype = bev, group = Scenario), size = 0.75) +
  facet_grid(Fuel~Type+fut) +
  labs(x = "Year", y = "PJ H2", title = "Total H2 Production: Net-zero CO2 by 2060", 
       linetype = "BEV Cost", color = "FCEV Cost",
       caption = "Note: Centralized Ethanol and Distributed Natural Gas with CCS are not options within the database") +
  yt +
  x_disc_l +
  bottom1

h2prod %>%
  filter(nz == "2060") %>%
  filter(trn == "ref") %>%
  filter(Fuel != "Ethanol") %>%
  ggplot() +
  geom_line(aes(x = Year, y = flo, color = fcev, linetype = bev, group = Scenario), size = 0.75) +
  facet_grid(Fuel+fut~Type+td) +
  labs(x = "Year", y = "PJ H2", title = "Total H2 Production: Net-zero CO2 by 2060", 
       linetype = "BEV Cost", color = "FCEV Cost",
       caption = "Note: Centralized Ethanol and Distributed Natural Gas with CCS are not options within the database") +
  yt +
  x_disc_l +
  bottom1

    ---------------------------------# cheaper fcev gets us more h2 production

h2totalprod_tech_td_2060 <- h2prod %>%
  filter(nz == "2060") %>%
  filter(trn == "ref") %>%
  filter(!Fuel %in% c("Ethanol","Natural Gas")) %>%
  ggplot() +
  geom_line(aes(x = Year, y = flo, color = fcev, linetype = bev, group = Scenario), size = 0.75) +
  facet_grid(Fuel~Type+td) +
  labs(x = "Year", y = "PJ H2", title = "Total H2 Production: Net-zero CO2 by 2060",
       subtitle = "T&D Cost Comparison",
       linetype = "BEV Cost", color = "FCEV Cost",
       caption = "Note: Centralized Ethanol and Distributed Natural Gas with CCS are not options within the database") +
  yt +
  x_disc_l +
  bottom1

## Transportation -----

fcevs <- tran_sum %>% filter(fuel == "H2") %>%
  ggplot() +
  geom_line(aes(x = Year, y = use, color = nz, group = Scenario), size = 0.75) +
  facet_grid(bev~fcev) +
  labs(x = "Year", y = "PJ", title = "Hydrogen use in Transportation") +
  yt +
  x_disc_l +
  bottom1

fcevs_2060_h2 <- tran_sum %>% filter(fuel == "H2") %>% filter(nz == "2060") %>%
  ggplot() +
  geom_line(aes(x = Year, y = use, color = td, group = Scenario), size = 0.75) +
  facet_grid(fcev~bev, labeller = label_both) +
  labs(x = "Year", y = "PJ", title = "Hydrogen use in Transportation: Net Zero 2060", color = "H2 T&D Cost") +
  yt +
  x_disc_l +
  bottom1

fcevs_2060_elc <- tran_sum %>% filter(fuel == "ELC") %>% filter(nz == "2060") %>%
  ggplot() +
  geom_line(aes(x = Year, y = use, color = trn, group = Scenario), size = 0.75) +
  facet_grid(fcev~bev, labeller = label_both) +
  labs(x = "Year", y = "PJ", title = "Electricity use in Transportation: Net Zero 2060", 
       color = "Transportation Net-Zero CO2 by 2050") +
  yt +
  x_disc_l +
  bottom1








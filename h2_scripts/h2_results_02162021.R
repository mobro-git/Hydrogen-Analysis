## Load Setup and Data Scripts ----

# allows me to quickly run my two previous scripts when I make changes in them or
# if I'm just going to work with results and need to pull in the data

# source("h2_Scripts/h2_setup_01062021.R")
# source("h2_Scripts/h2_data_01062021.R")

## Dummies -----

dummies <- dummy %>% 
  ggplot() +
  geom_line(aes(x = Year, y = dummy, color = scen, group = Scenario, linetype = dist)) +
  facet_grid(~Commodity, scales = "free_y") +
  labs(x = "Year", y = "Dummy Value", title = "Dummies") +
  yt +
  x_disc_l +
  bottom1 +
  scen_color

      # No dummies in any case beyond the reference case

## CO2 Emissions -----

## ~all CO2 Emissions -----

co2emissions_enduse <- co2emissions %>% 
  filter(scen != "force") %>% filter(td == "high") %>%
  filter(Commodity %in% c("ELCCO2", "TRNCO2", "INDCO2","REFCO2")) %>%
  ggplot() +
  geom_line(aes(x = Year, y = emissions, color = scen, group = Scenario, linetype = dist), size = 0.75) +
  facet_wrap(~Commodity, scales = "free_y") +
  labs(x = "Year", y = "CO2 Emissions (Mt)", title = "CO2 Emissions by Scenario and Sector") +
  yt +
  x_disc_l +
  bottom1 +
  scen_color

      # electric and industrial emissions increase due to TRN electrification and hydrogen switch.
      # industrial emissions rise most when a lot of hydrogen comes in with distributed production

## H2 Production -----

## ~total -----

h2totalprod <- h2cumcap %>% filter(scen != "force") %>% filter(td == "high") %>%
  ggplot() +
  geom_line(aes(x = Year, y = PJ, color = scen, group = Scenario, linetype = dist), size = 0.75) +
  labs(x = "Year", y = "PJ H2", title = "Total H2 Production", linetype = "Production Techs",
       color = "Scenario") +
  yt +
  x_disc_l +
  bottom1 +
  scen_color

      # In the required 10% h2 scenario, ~2% more H2 comes in when there is distributed production
      # In the transportation sector CO2 reduction case, H2 proliferates with distributed production
      # The sys80 case isnt bringing in H2 with the increased T&D costs or with distributed production
      # It's choosing to reduce emissions in other sectors and electrify in trn sector

## ~by technology -----

h2prodbytech <- h2techcap %>% filter(scen != "force") %>%
  ggplot() +
  geom_line(aes(x = Year, y = PJ, color = scen, group = Scenario, linetype = dist)) +
  facet_wrap(~Process, labeller = labeller(Process = techlabels)) +
  labs(x = "Year", y = "PJ H2", title = "H2 Production by Technology - Positive Emissions") +
  yt +
  x_disc_l +
  bottom1 +
  scen_color

      # Natural gas smr is the predominant technology. With system-wide CO2 reductions, CCS is used and
        # some PEM in the future. But in these cases only the bare minimum h2 is used
      # distributed ethanol also comes in significantly alongside natural gas.
  
h2prodbytech_reg <- h2techcap_reg %>% 
  filter(scen != "force") %>%
  ggplot() +
  geom_line(aes(x = Year, y = PJ, color = scen, group = Scenario, linetype = dist)) +
  facet_grid(Region~Process, scales = "free_y") +
  labs(x = "Year", y = "PJ H2", title = "H2 Production by Technology") +
  yt +
  x_disc_l +
  bottom1 +
  scen_color
  
      # centralized current and future nga smr only comes into region 7
      # distributed goes into all regions in both tlth10 and trn100 cases

## ~t&d cost cases -----

tdtotals <- tdcost_cum %>%
  ggplot() +
  geom_line(aes(x = Year, y = PJ, color = td, group = Scenario, linetype = dist), size = 0.75) +
  facet_wrap(~scen) +
  labs(x = "Year", y = "PJ H2", title = "Total H2 Production", linetype = "Production Techs",
       color = "T&D Costs") +
  yt +
  x_disc_l +
  bottom1 

tdtechs <- tdcost_tech %>% filter(scen == "trn100") %>%
  filter(scen != "force") %>%
  ggplot() +
  geom_line(aes(x = Year, y = PJ, color = td, group = Scenario, linetype = dist)) +
  facet_wrap(~Process, labeller = labeller(Process = techlabels)) +
  labs(x = "Year", y = "PJ H2", title = "H2 Production by Technology: trn100", linetype = "Production Techs",
       color = "T&D Costs") +
  yt+
  x_disc_l +
  bottom1 

## ~t&d cost comparison -----

costcomparison <- allcosts %>%
  ggplot() +
  geom_point(aes(y = `LCOE $/kg H2`, x = Year, color = Source)) +
  geom_text_repel(aes(y = `LCOE $/kg H2`, x = Year, color = Source, label = Data), size = 2.5) +
  labs(title = "H2 Production, Delivery, and Dispensing Cost Comparison")

  
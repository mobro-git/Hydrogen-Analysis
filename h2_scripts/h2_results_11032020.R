## Load Setup and Data Scripts ----

# allows me to quickly run my two previous scripts when I make changes in them or
# if I'm just going to work with results and need to pull in the data

# source("h2_Scripts/h2_setup_072020.R")
# source("h2_Scripts/h2_data_072020.R")

## CO2 Emissions -----

## ~all CO2 Emissions -----

co2emissions_enduse <- co2emissions %>% 
  filter(Commodity %in% c("ELCCO2", "TRNCO2", "INDCO2")) %>%
  filter(prod %in% c("enduse","Ref")) %>%
  filter(rps != "noRPS") %>%
  ggplot() +
  geom_line(aes(x = Year, y = emissions, color = qnt, group = Scenario)) +
  facet_grid(elc+tech~Commodity, scales = "free_y") +
  labs(x = "Year", y = "CO2 Emissions (Mt)", title = "CO2 Emissions by Scenario",subtitle="Required enduses") +
  yt +
  x_disc_l +
  bottom1 +
  qnt_color

## H2 Production ----

## ~ Total H2  ----

h2prod_cum_all <- h2prod_cum %>%
  ggplot() +
  geom_line(aes(x = Year, y = PJ, color = qnt, linetype = tech, group = Scenario), size = 0.75) +
  facet_grid(prod~elc, scales = "free_y") +
  labs(x = "Year", y = "PJ H2", title = "Total H2 Production") +
  yt +
  x_disc_l +
  qnt_color

    # looking from grid to rnw for max cases, rnw limits potential h2 production, run out of rnw capacity 

## ~ By technology ----

h2prod_tech_all <- h2prod %>% ungroup() %>%
  ggplot() +
  geom_line(aes(x = Year, y = PJ, color = qnt, linetype = tech, group = Scenario),size = 0.75) +
  facet_grid(prod+elc~Process, scales = "free_y") +
  labs(x = "Year", y = "PJ H2", title = "H2 Production by Scenario", color = "H2 Quantity") +
  yt +
  x_disc_l +
  bottom1 +
  qnt_color

      # natural gas h2 production far surpasses pem only production - cheaper
      # at max requirement, some biogas, coal w/ ccs, and pem comes in, but predominantly NGA 


h2prod_tech_pem <- h2prod %>% 
  filter(tech == "pem") %>%
  ggplot() +
  geom_line(aes(x = Year, y = PJ, color = qnt, group = Scenario),size = 0.75) +
  facet_grid(prod+elc~Process) +
  labs(x = "Year", y = "PJ H2", title = "H2 Production by Tech - PEM Only") +
  yt +
  x_disc_l +
  qnt_color

    # if we look at just the PEM technologies, if we take elc from the grid, we achieve higher capacities from PEM
    # requiring renewable elc to be used for electrolysis limits our h2 production ability, hit max capacity
     

## ~ Regional production  ----

h2prod_tech_reg <- h2prod_reg %>%
  ggplot() +
  geom_line(aes(x = Year, y = PJ, color = qnt, group = Scenario), size = 0.75) +
  facet_grid(Process~Region) +
  labs(x = "Year", y = "PJ H2", title = "H2 Production by Region") +
  yt +
  x_disc_l +
  nolegend +
  qnt_color

    # almost all NGA comes in region 7 (tx, ok, ar, la)
    # almost all current PEM region 6 (ky, tn, ms, al)

h2prod_all_reg <- h2prod_cum_reg %>% 
  filter(tech == "pem" & elc == "rnw") %>%
  ggplot() +
  geom_line(aes(x = Year, y = PJ, color = qnt, group = Scenario), size = 0.75) +
  facet_grid(tech+elc~Region, scales = ("free_y")) +
  labs(x = "Year", y = "PJ H2", title = "H2 Production by Region") +
  yt +
  x_disc_l +
  qnt_color

    # region 6 only has high production when using grid elc, not when rnw is required
    # regions 5, 7, 8, 9 have consistently higher renewable h2 production

    
## ELC Production ----

## ~ Total ----

totalelc <- elc %>%
  ggplot() +
  geom_line(aes(x = Year, y = PJ, group = Scenario, color = qnt, linetype = tech), size = 0.75) +
  labs(x = "Year", y = "PJ ELC", title = "Total Electricity Production") +
  facet_grid(Processset~prod+elc) +
  yt +
  x_disc_l +
  qnt_color

    # this is just looking at electricity that stays on the grid
    # in really high penetration scenarios, natural gas and nuclear make up the
    # slack when renewables are used for h2 production
  
regtotalelc <- elc_reg %>%
  ggplot() +
  geom_line(aes(x = Year, y = PJ, group = Scenario, color = qnt, linetype = tech), size = 0.75) +
  facet_grid(Processset~Region, scales = "free_y") +
  labs(x = "Year", y = "PJ ELC", title = "Total Electricity Production") +
  yt +
  x_disc_l +
  qnt_color

    # this is just looking at electricity that stays on the grid
    # region 6 has the most dramatic generation shift to increase nuclear and natural gas
    # some solar and wind upticks

## ~ Rnwelc ----

totalrnwelc <- rnwelc %>%
  ggplot() +
  geom_line(aes(x = Year, y = PJ, group = Scenario, color = qnt, linetype = tech), size = 0.75) +
  labs(x = "Year", y = "PJ ELC", title = "Total Renewable Electricity Production") +
  facet_grid(Processset~prod) +
  yt +
  x_disc_l +
  qnt_color

    # this is just looking at renewable elc going to hydrogen production
    # more coming from wind than solar

regtotalrnwelc <- rnwelc_reg %>%
  ggplot() +
  geom_line(aes(x = Year, y = PJ, group = Scenario, color = qnt, linetype = tech), size = 0.75) +
  facet_grid(Processset~Region, scales = "free_y") +
  labs(x = "Year", y = "PJ ELC", title = "Total Renewable Electricity Production") +
  yt +
  x_disc_l +
  qnt_color

    # this is just looking at renewable elc going to hydrogen production
    # most solar coming from regions 7,8,9
    # wind assets more spread out


## Water consumption ----

## ~ H2 production ----

h2h2o <- h2_h2o_proc_com %>%
  ggplot() +
  geom_line(aes(x = Year, y = h2o, group = Scenario, color = qnt, linetype = tech), size = 0.75) +
  labs(x = "Year", y = "Water Use (mil gal)", title = "Water consumption and withdrawal",subtitle = "H2 Production") +
  facet_grid(Process~Commodity) +
  yt +
  x_disc_l +
  qnt_color

  # most h2 prod water use is consumption, not returned to the system
  # highest from electrolysis, though nga is still a decently high water consumer. coal uses some too. future natural gas is more water efficient.

## ~ Electric sector ----

elch2o <- elc_h2o_proc_com %>%
  ggplot() +
  geom_line(aes(x = Year, y = elcuse, group = Scenario, color = qnt, linetype = tech), size = 0.75) +
  labs(x = "Year", y = "Water Use (mil gal)", title = "Total Renewable Electricity Production",subtitle = "Electricity Production") +
  facet_grid(Processset~Commodity) +
  yt +
  x_disc_l +
  qnt_color

  # largest differences in natural gas consumption and withdrawal due to increase in capacity to compensate for renewables drawn for h2 production

# H2 water use is significant, but not as significant as electric sector water use.
















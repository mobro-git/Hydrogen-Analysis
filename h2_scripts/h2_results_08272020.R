## Load Setup and Data Scripts ----

# allows me to quickly run my two previous scripts when I make changes in them or
# if I'm just going to work with results and need to pull in the data

# source("h2_Scripts/h2_setup_072020.R")
# source("h2_Scripts/h2_data_072020.R")

## CO2 Emissions -----

## ~all CO2 Emissions -----

co2emissions_all <- co2emissions %>% 
  filter(Commodity %in% c("COMCO2", "ELCCO2", "TRNCO2", "INDCO2")) %>%
  ggplot() +
  geom_line(aes(x = Year, y = emissions, color = Scenario, group = Scenario)) +
  facet_wrap(~Commodity, scales = "free_y") +
  labs(x = "Year", y = "CO2 Emissions (Mt)", title = "CO2 Emissions by Scenario") +
  yt +
  x_disc_l +
  nolegend

co2emissions_PEMonly <- co2emissions %>% 
  filter(Commodity %in% c("COMCO2", "ELCCO2", "TRNCO2", "INDCO2")) %>%
  filter(availtech == "PEMonly") %>%
  ggplot() +
  geom_line(aes(x = Year, y = emissions, color = Scenario, group = Scenario)) +
  facet_wrap(~Commodity, scales = "free_y") +
  labs(x = "Year", y = "CO2 Emissions (Mt)", 
       title = "CO2 Emissions by Scenario - PEM H2 Production") +
  yt +
  x_disc_l +
  nolegend

co2emissions_nonextgen <- co2emissions %>% 
  filter(Commodity %in% c("COMCO2", "ELCCO2", "TRNCO2", "INDCO2")) %>%
  filter(availtech == "noNextGen") %>%
  ggplot() +
  geom_line(aes(x = Year, y = emissions, color = Scenario, group = Scenario)) +
  facet_wrap(~Commodity, scales = "free_y") +
  labs(x = "Year", y = "CO2 Emissions (Mt)", 
       title = "CO2 Emissions by Scenario - No Next Gen H2 Production") +
  yt +
  x_disc_l +
  nolegend
  
              # likely that increased ELC need in transportation due to the trnco2 caps
              # causes the commercial and industrial sector to use a greater amount of
              # natural gas in place of electricity, would increase sector CO2 emissions

## ~COM and IND sector CO2 Emissions -----

comco2emissions_all <- co2emissions %>% 
  filter(Commodity == "COMCO2") %>%
  ggplot() +
  geom_line(aes(x = Year, y = emissions, color = co2, linetype = availtech, 
                group = Scenario)) +
  facet_grid(PEMcost~rnwelc) +
  labs(x = "Year", y = "CO2 Emissions (Mt)", 
       title = "Commercial CO2 Emissions by Scenario") +
  yt +
  x_disc_l

indco2emissions_all <- co2emissions %>% 
  filter(Commodity == "INDCO2") %>%
  ggplot() +
  geom_line(aes(x = Year, y = emissions, color = co2, linetype = availtech, 
                group = Scenario)) +
  facet_grid(PEMcost~rnwelc) +
  labs(x = "Year", y = "CO2 Emissions (Mt)", 
       title = "Industrial CO2 Emissions by Scenario") +
  yt +
  x_disc_l

## ~ELC CO2 Emissions -----

elcco2emissions_all <- co2emissions %>% 
  filter(Commodity == "ELCCO2") %>%
  ggplot() +
  geom_line(aes(x = Year, y = emissions, color = co2, linetype = availtech, 
                group = Scenario)) +
  facet_grid(PEMcost~rnwelc) +
  labs(x = "Year", y = "CO2 Emissions (Mt)", 
       title = "Electricity CO2 Emissions by Scenario") +
  yt +
  x_disc_l

            # requiring rnwelc reduces emissions from the electric sector vs grid elc
            # tighter trnco2 constraints increase elcco2 emissions

## ~TRN CO2 Emissions -----

trnco2emissions_all <- co2emissions %>% 
  filter(Commodity == "TRNCO2") %>%
  ggplot() +
  geom_line(aes(x = Year, y = emissions, color = co2, linetype = availtech, 
                group = Scenario)) +
  facet_grid(PEMcost~rnwelc) +
  labs(x = "Year", y = "CO2 Emissions (Mt)", 
       title = "Transportation CO2 Emissions by Scenario") +
  yt +
  x_disc_l

          # all as expected, except we see that nonextgen gets us to lower emissions
          # vs PEM only in the 80% trnco2 scenario

trnco2emissions_PEMonly <- co2emissions %>% 
  filter(Commodity == "TRNCO2") %>%
  filter(PEMcost == "50PEM" | PEMcost == "80PEM") %>%
  ggplot() +
  geom_line(aes(x = Year, y = emissions, color = co2, linetype = PEMcost, 
                group = Scenario)) +
  facet_grid(~rnwelc) +
  labs(x = "Year", y = "CO2 Emissions (Mt)", 
       title = "Transportation CO2 Emissions by Scenario - PEM Only") +
  yt +
  x_disc_l

          # reducing the cost of electrolyzers from 50 to 80 
          # doesnt decrease trnco2 emissions

## SO2 Emissions -----

## ~all SO2 Emissions -----

so2emissions_all <- so2emissions %>%
  ggplot() +
  geom_line(aes(x = Year, y = emissions, color = Scenario, group = Scenario)) +
  facet_wrap(~Commodity, scales = "free_y") +
  labs(x = "Year", y = "SO2 Emissions (kt)", title = "SO2 Emissions by Scenario") +
  yt +
  x_disc_l

so2emissions_change <- so2emissions %>% 
  filter(Commodity %in% c("COMSO2", "INDSO2", "REFSO2", "RSSSO2")) %>%
  ggplot() +
  geom_line(aes(x = Year, y = emissions, color = Scenario, group = Scenario)) +
  facet_wrap(~Commodity, scales = "free_y") +
  labs(x = "Year", y = "SO2 Emissions (kt)", title = "SO2 Emissions by Scenario") +
  yt +
  x_disc_l +
  nolegend

        # trnco2 caps increase so2 emissions in COM & IND at 80%
        # trnco2 caps decrease so2 emissions in REF & RSS 
        # (resource supply) at cap intervals

## ~BIOESO2 Emissions -----

bioeco2emissions_all <- so2emissions %>% 
  filter(Commodity == "BIOESO2") %>%
  ggplot() +
  geom_line(aes(x = Year, y = emissions, color = co2, linetype = availtech, 
                group = Scenario)) +
  facet_grid(PEMcost~rnwelc) +
  labs(x = "Year", y = "SO2 Emissions (kt)", 
       title = "Bioenergy SO2 Emissions by Scenario") +
  yt +
  x_disc_l

## H2 Production ----

## ~ By technology ----

h2prod_tech_all <- h2prod %>%
  ggplot() +
  geom_line(aes(x = Year, y = PJ, color = Scenario, group = Scenario)) +
  facet_wrap(~Process) +
  labs(x = "Year", y = "PJ H2", title = "H2 Production by Scenario") +
  yt +
  x_disc_l +
  nolegend

      # natural gas h2 production far surpasses pem only production - cheaper

h2prod_tech_nonextgen <- h2prod %>% 
  filter(availtech == "noNextGen") %>%
  ggplot() +
  geom_line(aes(x = Year, y = PJ, color = co2, group = Scenario)) +
  facet_wrap(~Process) +
  labs(x = "Year", y = "PJ H2", title = "H2 Production by Tech - No Next Gen") +
  yt +
  x_disc_l

h2prod_tech_pem <- h2prod %>% 
  filter(availtech == "PEMonly") %>%
  ggplot() +
  geom_line(aes(x = Year, y = PJ, color = PEMcost, linetype = rnwelc, group = Scenario)) +
  facet_grid(co2~Process) +
  labs(x = "Year", y = "PJ H2", title = "H2 Production by Tech - PEM Only") +
  yt +
  x_disc_l

## ~ Total H2  ----

h2prod_cum_all <- h2prod_cum %>%
  ggplot() +
  geom_line(aes(x = Year, y = PJ, color = availtech, linetype = rnwelc, 
                group = Scenario)) +
  facet_grid(PEMcost~co2) +
  labs(x = "Year", y = "PJ H2", title = "Total H2 Production") +
  yt +
  x_disc_l
      
      # nonextgen/gridelc allows natural gas, cheaper, elicits more h2 production
      # pemonly grid gets more h2 production than when rnwelc is required/more expensive

## ~ Regional production  ----

h2prod_tech_reg <- h2prod_reg %>%
  ggplot() +
  geom_line(aes(x = Year, y = PJ, color = Scenario, group = Scenario)) +
  facet_grid(Region~Process) +
  labs(x = "Year", y = "PJ H2", title = "H2 Production by Region") +
  yt +
  x_disc_l +
  nolegend

h2prod_cum_reg <- h2prod_cumreg %>%
  ggplot() +
  geom_line(aes(x = Year, y = PJ, color = Scenario, group = Scenario)) +
  facet_grid(~Region) +
  labs(x = "Year", y = "PJ H2", title = "H2 Production by Region") +
  yt +
  x_disc_l

      # when pemonly, all regions produce. if nga smr allowed, only region 7 produces
      # need to fix inter-state trade parameters and costs

## ELC Production ----

## ~ Total ----

totalelc <- elcrnwelc_total %>%
  ggplot() +
  geom_line(aes(x = Year, y = PJ, group = Scenario, color = Scenario)) +
  labs(x = "Year", y = "PJ ELC", title = "Total Electricity Production") +
  yt +
  x_disc_l +
  nolegend
  
    # trnco2 caps increase elc production, PEM only has greater affect than nonextgen

regtotalelc <- elcrnwelc_regtotal %>%
  ggplot() +
  geom_line(aes(x = Year, y = PJ, group = Scenario, color = Scenario)) +
  facet_grid(~Region, scales = "free_y") +
  labs(x = "Year", y = "PJ ELC", title = "Total Electricity Production") +
  yt +
  x_disc_l +
  nolegend

    # trnco2 caps increase elc production in all regions
    # figure out why region 6 is different than the rest

## ~ Process ----

elctechs_all <- elcrnwelc %>% 
  ggplot() +
  geom_line(aes(x=Year, y=PJ, color=Scenario, group=Scenario)) +
  facet_grid(ProcessSet~Commodity, scales = "free_y") +
  labs(x = "Year", y = "PJ ELC", title = "Total Electricity Production by Technology") +
  yt +
  x_disc_l +
  nolegend

elctechs <- elcrnwelc %>% 
  filter(ProcessSet %in% c("Coal","Natural Gas","Wind","Solar")) %>%
  ggplot() +
  geom_line(aes(x=Year, y=PJ, color=co2, linetype=PEMcost, group=Scenario)) +
  facet_grid(ProcessSet~Commodity+availtech) +
  labs(x = "Year", y = "PJ ELC", title = "Total Electricity Production") +
  yt +
  x_disc_l

      # PEM requirement increases ELC production, predominantly natural gas
      # even in the renewable elc only scenarios
      # shift to elc in trn, not only for h2 production

## H2 Transportation use ----

## ~ Total ----

trnh2_reg <- h2trn_reg %>%
  filter(Attribute == "VAR_FIn") %>%
  ggplot() +
  geom_line(aes(x=Year, y=PJ, color = Scenario, group = Scenario)) +
  facet_grid(Process~Region) +
  labs(x = "Year", y = "PJ H2", title = "Transportation H2 Use") +
  yt +
  x_disc_l +
  nolegend

      # H2 busses still too expensive, dont come in
      # can work on finding cost point for market penetration

trnh2 <- h2trn %>%
  filter(Attribute == "VAR_FIn") %>%
  ggplot() +
  geom_line(aes(x=Year, y=PJ, color = PEMcost, linetype = rnwelc, group = Scenario)) +
  facet_grid(availtech+co2~Process, scales = "free_y") +
  labs(x = "Year", y = "PJ H2", title = "Transportation H2 Use") +
  yt +
  x_disc_l

## ~ HDV ----

hdvh2 <- h2trn %>%
  filter(Attribute == "VAR_FIn") %>%
  filter(Commodity == "THH2") %>%
  ggplot() +
  geom_line(aes(x=Year, y=PJ, color = PEMcost, linetype = rnwelc, group = Scenario)) +
  facet_grid(co2~availtech, scales = "free_y") +
  labs(x = "Year", y = "PJ H2", title = "HDV H2 Use - Heavy Truck") +
  yt

      # HDV only has meaningful market penetration with 80% trnco2 caps
      # PEM requirement is still too expensive, reduces HDV h2 use

## ~ LDV ----

ldvh2 <- h2trn %>%
  filter(Attribute == "VAR_FIn") %>%
  filter(Commodity == "TLH2") %>%
  ggplot() +
  geom_line(aes(x=Year, y=PJ, color = co2, linetype = rnwelc, group = Scenario)) +
  facet_grid(Process~availtech+PEMcost, scales = "free_y") +
  labs(x = "Year", y = "PJ H2", title = "LDV H2 Use") +
  yt

      # Compact cars have meaningful market penetration in all cases
      # crossovers only come in if PEM isnt required
      # CO/F/V come in at different amounts depending on pemcost and co2

## H2 Vehicles  ----

## ~ Demand met ----

trndemand_all <- h2totalcar %>%
  ggplot() +
  geom_line(aes(x=Year,y=bnVMT,color=Scenario,group=Scenario)) +
  facet_wrap(~ProcessSet, scales = "free_y") +
  labs(x = "Year", y = "billion VMT", title = "Transportation Mix") +
  yt + x_disc_l

tcdemand <- h2totalcar %>%
  filter(Commodity == "TC") %>%
  filter(ProcessSet !="TC-B20X") %>%
  ggplot() +
  geom_line(aes(x=Year,y=bnVMT,color=Scenario,group=Scenario)) +
  facet_wrap(~ProcessSet, scales = "free_y") +
  labs(x = "Year", y = "billion VMT", title = "Transportation Mix - Commercial") +
  yt + x_disc_l +
  nolegend

thdemand <- h2totalcar %>%
  filter(ProcessSet %in% c("TH-DSL","TH-GSL","TH-LPG")) %>%
  ggplot() +
  geom_line(aes(x=Year,y=bnVMT,color=Scenario,group=Scenario)) +
  facet_wrap(~ProcessSet, scales = "free_y") +
  labs(x = "Year", y = "billion VMT", title = "Transportation Mix - Heavy Duty") +
  yt + x_disc_l + nolegend

tldemand <- h2totalcar %>%
  filter(ProcessSet %in% c("TL-ELC","TL-GSL","TL-H2")) %>%
  ggplot() +
  geom_line(aes(x=Year,y=bnVMT,color=Scenario,group=Scenario)) +
  facet_wrap(~ProcessSet, scales = "free_y") +
  labs(x = "Year", y = "billion VMT", title = "Transportation Mix - Light Duty") +
  yt + x_disc_l + nolegend

tmdemand <- h2totalcar %>%
  filter(Commodity == "TM") %>%
  ggplot() +
  geom_line(aes(x=Year,y=bnVMT,color=Scenario,group=Scenario)) +
  facet_wrap(~ProcessSet, scales = "free_y") +
  labs(x = "Year", y = "billion VMT", title = "Transportation Mix - Medium Duty") +
  yt + x_disc_l

## ~ Market share ----

trnmrk_all <- h2totalcar %>%
  ggplot() +
  geom_line(aes(x=Year,y=mktshr,color=Scenario,group=Scenario)) +
  facet_wrap(~ProcessSet, scales = "free_y") +
  labs(x = "Year", y = "% Demand Met", title = "Transportation Mix") +
  yt + x_disc_l

tcmrk <- h2totalcar %>%
  filter(Commodity == "TC") %>%
  ggplot() +
  geom_line(aes(x=Year,y=mktshr,color=Scenario,group=Scenario)) +
  facet_wrap(~ProcessSet, scales = "free_y") +
  labs(x = "Year", y = "% Demand Met", title = "Transportation Mix - Commercial") +
  yt + x_disc_l

thmrk <- h2totalcar %>%
  filter(Commodity == "TH") %>%
  ggplot() +
  geom_line(aes(x=Year,y=mktshr,color=Scenario,group=Scenario)) +
  facet_wrap(~ProcessSet, scales = "free_y") +
  labs(x = "Year", y = "% Demand Met", title = "Transportation Mix - Heavy Duty") +
  yt + x_disc_l

tlmrk <- h2totalcar %>%
  filter(Commodity == "TL") %>%
  ggplot() +
  geom_line(aes(x=Year,y=mktshr,color=Scenario,group=Scenario)) +
  facet_wrap(~ProcessSet, scales = "free_y") +
  labs(x = "Year", y = "% Demand Met", title = "Transportation Mix - Light Duty") +
  yt + x_disc_l

tmmrk <- h2totalcar %>%
  filter(Commodity == "TM") %>%
  ggplot() +
  geom_line(aes(x=Year,y=mktshr,color=Scenario,group=Scenario)) +
  facet_wrap(~ProcessSet, scales = "free_y") +
  labs(x = "Year", y = "% Demand Met", title = "Transportation Mix - Medium Duty") +
  yt + x_disc_l

tcshares <- h2totalcar %>%
  filter(Commodity == "TC") %>%
  filter(ProcessSet %in% c("TC-B20X","TC-E85X","TC-LPG")) %>%
  ggplot() +
  geom_line(aes(x=Year,y=mktshr,color=availtech,linetype=PEMcost,group=Scenario)) +
  facet_grid(ProcessSet~co2, scales = "free_y") +
  labs(x = "Year", y = "% Demand Met", title = "Transportation Mix - Commercial") +
  yt + x_disc_l

thshares <- h2totalcar %>%
  filter(Commodity == "TH") %>%
  filter(ProcessSet %in% c("TH-DSL","TH-GSL","TH-LPG")) %>%
  ggplot() +
  geom_line(aes(x=Year,y=mktshr,color=availtech,linetype=PEMcost,group=Scenario)) +
  facet_grid(ProcessSet~co2, scales = "free_y") +
  labs(x = "Year", y = "% Demand Met", title = "Transportation Mix - Heavy Duty") +
  yt + x_disc_l

tlshares <- h2totalcar %>%
  filter(Commodity == "TL") %>%
  filter(ProcessSet %in% c("TL-ELC","TL-GSL","TL-H2")) %>%
  ggplot() +
  geom_line(aes(x=Year,y=mktshr,color=availtech,linetype=PEMcost,group=Scenario)) +
  facet_grid(ProcessSet~co2, scales = "free_y") +
  labs(x = "Year", y = "% Demand Met", title = "Transportation Mix - Light Duty") +
  yt + x_disc_l

      # need explanation for why tlelc dips off in reference case

tmshares <- h2totalcar %>%
  filter(Commodity == "TM") %>%
  filter(ProcessSet %in% c("TM-B20X","TM-DSL","TM-GSL")) %>%
  ggplot() +
  geom_line(aes(x=Year,y=mktshr,color=availtech,linetype=PEMcost,group=Scenario)) +
  facet_grid(ProcessSet~co2, scales = "free_y") +
  labs(x = "Year", y = "% Demand Met", title = "Transportation Mix - Medium Duty") +
  yt + x_disc_l



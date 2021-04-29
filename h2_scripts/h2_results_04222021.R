
## Dummies -----

dummies <- dummy %>% 
  ggplot() +
  geom_line(aes(x = Year, y = dummy, color = nz, group = Scenario)) +
  labs(x = "Year", y = "Dummy Value", title = "Dummies") +
  facet_grid(~Commodity) +
  yt +
  x_disc_l +
  bottom1

      # all dummies same as reference case

## H2 Production -----

h2totalprod <- h2prod_sum %>%
  filter(nz == "2060") %>%
  ggplot() +
  geom_line(aes(x = Year, y = flo, color = fcev,group = Scenario), size = 0.75) +
  labs(x = "Year", y = "PJ H2", title = "Total H2 Production",
       color = "FCEV Cost Reduction (%)") +
  yt +
  bottom1

      # FCEV shows clear distinction, the cheaper the FCEV the more use
      # no NZ but with highest incentive elsewhere brings in H2, but not as much as reference NZ60

h2totalprod_tech <- h2prod %>%
  filter(nz == "2060") %>%
  ggplot() +
  geom_line(aes(x = Year, y = flo, color = fcev,group = Scenario), size = 0.75) +
  facet_grid(Type~Fuel) +
  labs(x = "Year", y = "PJ H2", title = "Total H2 Production by Technology",
       color = "FCEV Cost Reduction (%)") +
  yt +
  bottom1

      # cheaper fcev gets us more h2 production
      # more distributed vs centralized production
      # distributed ethanol and natural gas are used without nz target but switch to
        # electrolyis and ngaccs with nz60 target

h2totalprod_tech_2060 <- h2prod %>% mutate(tech = paste(Type, Fuel)) %>%
  filter(tech %in% c("Centralized Natural Gas CCS","Distributed Electrolysis")) %>%
  filter(nz == "2060") %>%
  ggplot() +
  geom_line(aes(x = Year, y = flo, color = fcev,linetype = pem, group = Scenario), size = 0.75) +
  facet_grid(~tech) +
  labs(x = "Year", y = "PJ H2", title = "Total H2 Production by PEM Cost Reduction",
       color = "FCEV Cost Reduction (%)",linetype = "PEM Costs") +
  yt +
  bottom1

      # having advanced techs available earlier brings in electrolysis vs natural gas ccs earlier
      # most visible with 10%, but similar trend for reference
      # total H2 produced doesnt change much, but which techs are used changes


## Refineries -----

refineries_graph <- refineries %>%
  ggplot() +
  geom_line(aes(x = Year, y = FOut, color = nz, group = Scenario), size = 0.75) +
  facet_wrap(~Commodity) 


## Electricity -----

electricity <- elcprod %>%
  filter(nz == "2060") %>%
  filter(Processset %in% c("Coal","Natural Gas","Solar","Wind")) %>%
  ggplot() +
  geom_line(aes(x=Year,y=elc,color=fcev,group=Scenario),size=0.75) +
  facet_wrap(~Processset, scales = "free_y") +
  labs(x = "Year", y = "PJ Electricity", title = "Electricity Production by Fuel",
       color = "FCEV Cost Reduction (%)") +
  yt +
  bottom1
  
## Transportation -----

h2cars_all <- trn %>% 
  filter(Fuel == "Hydrogen") %>%
  ggplot() +
  geom_line(aes(x=Year,y=bnVMT,color=fcev,group=Scenario)) +
  facet_wrap(~trnsector, scales = "free_y") +
  labs(x="Year",y="Billion VMT",title="Hydrogen in the Transportation Sector",
       color = "FCEV Cost Reduction (%)") +
  yt +
  x_disc_l +
  bottom1

h2cars_comparison <- trn %>% 
  filter(Fuel == "Hydrogen") %>%
  ggplot() +
  geom_line(aes(x=Year,y=bnVMT,color=fcev,group=Scenario)) +
  facet_wrap(~trnsector) +
  labs(x="Year",y="Billion VMT",title="Hydrogen in the Transportation Sector",
       color = "FCEV Cost Reduction (%)") +
  yt +
  x_disc_l +
  bottom1

ldv_air <- trn %>%
  filter(trnsector %in% c("Air","Light Duty")) %>%
  filter(Fuel %in% c("Diesel","Electric","Gasoline","Hydrogen","Jet Fuel")) %>%
  ggplot() +
  geom_line(aes(x=Year,y=bnVMT,color=fcev,group=Scenario)) +
  facet_grid(trnsector~Fuel, scales = "free_y") +
  yt +
  x_disc_l +
  bottom1

trn_others <- trn %>%
  filter(trnsector %in% c("Bus","Heavy Duty","Rail","Medium Duty","Shipping")) %>%
  filter(Fuel %in% c("Diesel","Electric","Gasoline","Hydrogen")) %>%
  ggplot() +
  geom_line(aes(x=Year,y=bnVMT,color=fcev,group=Scenario)) +
  facet_grid(trnsector~Fuel, scales = "free_y") +
  yt +
  x_disc_l +
  bottom1





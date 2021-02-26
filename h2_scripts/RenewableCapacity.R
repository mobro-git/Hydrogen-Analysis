
library(tidyverse)
library(here)

ReadAllSheets <- function(filename, tibble = FALSE) {
  sheets <- readxl::excel_sheets(filename)
  x <- lapply(sheets, function(X) readxl::read_excel(filename, 
                                                     sheet = X, 
                                                     skip = 1))
  if(!tibble) x <- lapply(x, as.data.frame)
  names(x) <- sheets
  x
}

results <- "h2_rawdata/results_renewablescapacity.xlsx"
data_global <- ReadAllSheets(here(results))

windcap <- as.data.frame(data_global$`Wind Capacity`)
solcap <- as.data.frame(data_global$`Solar Capacity`)
windbnd <- as.data.frame(data_global$`Wind Bound`) %>% filter(Scenario == "Reference_1026") %>% select(-Scenario, -Attribute)
solbnd <- as.data.frame(data_global$`Solar Bound`) %>% filter(Scenario == "Reference_1026") %>% select(-Scenario, -Attribute)
rnwelc <- as.data.frame(data_global$`Renewable ELC`)

wind <- left_join(windcap, windbnd, by = c("Process","Region")) %>%
  mutate(diff = wndbnd - wndcap) %>%
  filter(diff > 0)

# wind maxed out except in 2010 and 2011

solar <- left_join(solcap, solbnd, by = c("Process","Region")) %>%
  mutate(diff = solbnd - solcap) %>%
  filter(diff > 0) %>%
  gather(solbnd, solcap, diff, key="sol", value="amt")

ggplot(solar %>% filter(sol == "diff") %>% filter(Region == "R1")) +
  geom_bar(aes(x=Period, y=amt, fill=sol), stat="identity", position="dodge") +
  facet_grid(Scenario~Process)

ggplot(solar %>% filter(sol == "diff") %>% filter(Region == "R2")) +
  geom_bar(aes(x=Period, y=amt, fill=sol), stat="identity", position="dodge") +
  facet_grid(Scenario~Process)

ggplot(solar %>% filter(sol == "diff") %>% filter(Region == "R3")) +
  geom_bar(aes(x=Period, y=amt, fill=sol), stat="identity", position="dodge") +
  facet_grid(Scenario~Process)

ggplot(solar %>% filter(sol == "diff") %>% filter(Region == "R4")) +
  geom_bar(aes(x=Period, y=amt, fill=sol), stat="identity", position="dodge") +
  facet_grid(Scenario~Process)

ggplot(solar %>% filter(sol == "diff") %>% filter(Region == "R5")) +
  geom_bar(aes(x=Period, y=amt, fill=sol), stat="identity", position="dodge") +
  facet_grid(Scenario~Process)

ggplot(solar %>% filter(sol == "diff") %>% filter(Region == "R6")) +
  geom_bar(aes(x=Period, y=amt, fill=sol), stat="identity", position="dodge") +
  facet_grid(Scenario~Process)

ggplot(solar %>% filter(sol == "diff") %>% filter(Region == "R7")) +
  geom_bar(aes(x=Period, y=amt, fill=sol), stat="identity", position="dodge") +
  facet_grid(Scenario~Process)

ggplot(solar %>% filter(sol == "diff") %>% filter(Region == "R8")) +
  geom_bar(aes(x=Period, y=amt, fill=sol), stat="identity", position="dodge") +
  facet_grid(Scenario~Process)

ggplot(solar %>% filter(sol == "diff") %>% filter(Region == "R9")) +
  geom_bar(aes(x=Period, y=amt, fill=sol), stat="identity", position="dodge") +
  facet_grid(Scenario~Process)




                  
library(tidyverse)
library(HHSKwkl)
# library(glue)

# library(sf)
# library(leaflet)

theme_set(hhskthema())

fys_chem <- data_online("fys_chem.rds")
# parameters <- data_online("parameters.rds")
# meetpunten <- data_online("meetpunten.rds")

fys_chem %>% 
  filter(parnr %in% c(415, 429)) %>% 
  # select(mp, datum, par, waarde) %>% 
  summarise(waarde = mean(waarde), .by = c(mp, datum, par)) %>% 
  pivot_wider(names_from = par, values_from = waarde) %>% 
  rename(fluoroprobe = 3, biovolume = 4) %>% 
  filter(!is.na(fluoroprobe), !is.na(biovolume)) %>% 
  add_jaar() %>% 
  ggplot(aes(biovolume * 3, fluoroprobe)) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", colour = grijs_m) +
  geom_point() +
  scale_x_continuous(limits = c(0, NA), expand = expansion(mult = c(0, 0.05))) +
  scale_y_continuous(limits = c(0, NA), expand = expansion(mult = c(0, 0.05))) +
  coord_cartesian(xlim = c(0, 100)) +
  facet_wrap(~jaar) +
  labs(title = "Fluoroprobe geeft hogere waarden dan biovolume * 3",
       caption = "Lijn geeft 1:1 verhouding weer")
  

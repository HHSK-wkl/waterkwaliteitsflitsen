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
  coord_cartesian(xlim = c(0, 105), ylim = c(0, 280)) +
  facet_wrap(~jaar) +
  labs(title = "Fluoroprobe geeft hogere waarden dan biovolume * 3",
       caption = "Lijn geeft 1:1 verhouding weer")


fys_chem %>% 
  filter(parnr %in% c(415, 429)) %>% 
  # select(mp, datum, par, waarde) %>% 
  summarise(waarde = mean(waarde), .by = c(mp, datum, par)) %>% 
  pivot_wider(names_from = par, values_from = waarde) %>% 
  rename(fluoroprobe = 3, biovolume = 4) %>% 
  filter(!is.na(fluoroprobe), !is.na(biovolume)) %>% 
  add_jaar() %>% 
  filter(fluoroprobe > 12 | biovolume > 4) %>% 
  ggplot(aes(biovolume * 3, fluoroprobe)) +
  # geom_abline(slope = 1, intercept = 0, linetype = "dashed", colour = grijs_m) +
  geom_hline(yintercept = 12, linetype = "dashed", colour = "red") +
  geom_vline(xintercept = 12, linetype = "dashed", colour = "red") +
  geom_point() +
  scale_x_continuous(limits = c(0, NA), expand = expansion(mult = c(0, 0.05))) +
  scale_y_continuous(limits = c(0, NA), expand = expansion(mult = c(0, 0.05))) +
  coord_cartesian(xlim = c(0, 70), ylim = c(0, 70)) +
  facet_wrap(~jaar, axes = "all") +
  labs(title = "Fluoroprobe geeft hogere waarden dan biovolume * 3",
       caption = "Lijn geeft waarschuwingsgrens")
  


zwemlocaties <- c("S_0124", "S_0058", "S_0131", "S_1120", "S_1124", "S_0128", "K_1102", "S_0152")

fys_chem %>% 
  filter(parnr %in% c(415, 429)) %>% 
  # select(mp, datum, par, waarde) %>% 
  summarise(waarde = mean(waarde), .by = c(mp, datum, par)) %>% 
  pivot_wider(names_from = par, values_from = waarde) %>% 
  rename(fluoroprobe = 3, biovolume = 4) %>% 
  filter(!is.na(fluoroprobe), !is.na(biovolume)) %>% 
  add_jaar() %>% 
  # filter(jaar > 2024) %>% 
  filter(mp %in% zwemlocaties) %>% 
  mutate(waarschuwing_fluo = fluoroprobe > 12,
         waarschuwing_biovol = biovolume > 4) %>% 
  count(waarschuwing_fluo, waarschuwing_biovol)
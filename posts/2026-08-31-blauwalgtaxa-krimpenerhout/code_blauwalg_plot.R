library(tidyverse)
library(HHSKwkl)
library(glue)

theme_set(hhskthema())

fys_chem <- data_online("fys_chem.rds")
parameters <- data_online("parameters.rds")
meetpunten <- data_online("meetpunten.rds")

fys_chem %>% 
  filter(mp == "K_1102") %>% 
  filter(eenheid == "mm3/l") %>% 
  filter_out(parnr %in% c(400, 429, 441)) %>% 
  add_jaar() %>% 
  filter(jaar > 2018) %>%
  left_join(parameters) %>% 
  mutate(soort = fct_lump(str_remove(parnaamlang, " biovolume"), prop = 0.03, w = waarde, other_level = "Overig")) %>% 
  summarise(waarde = sum(waarde), .by = c(datum, jaar, soort)) %>% 
  ggplot(aes(datum, waarde, fill = soort)) +
  geom_area(color = NA) +
  # geom_point(position = "stack") +
  scale_fill_discrete(palette = "Set1") +
  scale_y_continuous(limits = c(0, NA), expand = expansion(mult = c(0, 0.05))) +
  # geom_vline(xintercept = ymd("20260810")) +
  facet_wrap(~jaar, scales = "free_x") +
  theme(legend.position = "bottom") +
  labs(title = "Ontwikkeling blauwalg soorten Krimpenerhout",
       y = "Biovolume - mm3/l")

# ladda paket, alla i pkg-vektorn laddas i for-loopen nedan
pkg <- c("tidyverse", "openxlsx")
lapply(pkg, library, character.only = TRUE)

imp <- openxlsx::read.xlsx("PaymentCaseFromFinland_data.xlsx") %>% 
  as_tibble()

colors <- c(red = "#ea5545", pink = '#f46a9b', '#ef9b20', '#edbf33', '#ede15b', '#bdcf32', '#87bc45', blue='#27aeef', purple = '#b33dc6')

by_age_tbl <- imp %>% 
  count(AgeClass, FeatureInvestigated, customers, name = "Antal") %>%
  mutate(AntalCustomers = customers*Antal) %>% 
  group_by(AgeClass) %>% 
  mutate(Andel = 100*AntalCustomers/sum(AntalCustomers)) %>% 
  ungroup() %>% 
  filter(FeatureInvestigated == 1) %>% 
  mutate(AgeClass = factor(AgeClass, levels = c("Missing", "Group1", "Group2", "Group3", "Group4")))

slide1_by_age <- ggplot(by_age_tbl, aes(AgeClass, Andel)) +
  geom_bar(stat = "identity", fill = colors[["blue"]]) +
  theme(panel.background = element_blank(),
    axis.ticks = element_blank(),
    axis.title = element_blank(),
    panel.grid.major.y = element_line(color = "grey"))

tbl_2_by_rating <- imp %>% 
  count(Rating, FeatureInvestigated, customers, name = "Antal") %>% 
  mutate(AntalCustomers = customers*Antal) %>% 
  group_by(Rating) %>% 
  mutate(Andel = 100*AntalCustomers/sum(AntalCustomers)) %>% 
  ungroup() %>% 
  filter(FeatureInvestigated == 1) %>% 
  mutate(Rating = factor(Rating, levels = c("missing", "Ok", "Good", "Star")))

slide_2_by_rating <- ggplot(tbl_2_by_rating, aes(Rating, Andel)) +
  geom_bar(stat = "identity", fill = colors[["blue"]]) +
  theme(panel.background = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank(),
        panel.grid.major.y = element_line(color = "grey"))

tbl_3_cube_rating <- imp %>% 
  select(cube_level, customers, FeatureInvestigated) %>% 
  group_by(FeatureInvestigated, cube_level) %>% 
  summarize(customers = sum(customers)) %>% 
  ungroup() %>% 
  mutate(cube_level = na_if(cube_level, "?") %>% as.numeric(),
         HasPaymentType = ifelse(FeatureInvestigated == 1, 'Yes', 'No')) %>% 
  group_by(HasPaymentType) %>% 
  mutate(Andel = 100*customers/sum(customers)) %>% 
  select(HasPaymentType, cube_level, Andel) %>%
  filter(!is.na(cube_level)) %>% 
  pivot_wider(names_from = HasPaymentType, values_from = Andel)

#skrota denna? ta ett snitt bara. kanske räkna antal försäkringar bara. totalt. 
slide_3_cube_rating <- ggplot(tbl_3_cube_rating %>% mutate(No = -No), aes(x = cube_level)) +
  geom_bar(aes(y = No), stat = "identity", fill = colors[["purple"]]) +
  geom_bar(aes(y = Yes), stat = "identity", fill = colors[["red"]]) +
  coord_flip() +
  scale_y_continuous(labels = abs) +
  theme(panel.background = element_blank(),
        panel.grid.major.y = element_line(color = "grey"),
        axis.ticks = element_blank())+
  labs(x = "", y = "Cube level")


contact <- imp %>% 
  group_by(FeatureInvestigated) %>% 
  summarize(`AndelMedEmail` = 100*mean(contact_info_edm, na.rm = TRUE),
            `AndelMedTelefon` = 100*mean(contact_info_tm, na.rm = TRUE))
  



# users of this method are older, show signs of being less technically proficient, probably difficult to change method
# they are lucrative and cheap and we might want to keep them
# they may not find it easy to change and we might lose their business or goodwill at least

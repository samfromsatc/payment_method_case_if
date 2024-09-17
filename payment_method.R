# ladda paket, alla i pkg-vektorn laddas i for-loopen nedan
pkg <- c("tidyverse", "openxlsx", "extrafonts")
lapply(pkg, library, character.only = TRUE)

imp <- openxlsx::read.xlsx("PaymentCaseFromFinland_data.xlsx") %>% 
  as_tibble()

#sanity check: har jag förstått d.tasetet korrekt? en rad per kombination av variabelutfall och customers är hur många som har den kombinationen?
count(imp, across(-customers)) %>% count(n) # ja det finns bara en rad per kombination av de andra kolumnerna?

grunddata <- imp %>% 
  mutate(Users = ifelse(FeatureInvestigated == 1, 'Anv?ndare', '?vriga'))

colors <- c(red = "#ea5545", pink = '#f46a9b', '#ef9b20', '#edbf33', '#ede15b', '#bdcf32', '#87bc45', blue='#27aeef', purple = '#b33dc6')

custom_theme <- function(){
theme(panel.background = element_blank(),
  axis.ticks = element_blank(),
  panel.grid.major.y = element_line(color = "grey"),
  text = element_text(family = "Helvetica"))
}

font_import()
loadfonts(device="win")

by_age_tbl <- imp %>% 
  group_by(AgeClass, FeatureInvestigated) %>%
  summarize(Antal = sum(customers, na.rm = TRUE)) %>% 
  group_by(AgeClass) %>% 
  mutate(Andel = 100*Antal/sum(Antal)) %>% 
  ungroup() %>% 
  filter(FeatureInvestigated == 1) %>% 
  mutate(AgeClass = factor(AgeClass, levels = c("Missing", "Group1", "Group2", "Group3", "Group4")))

slide1_by_age <- ggplot(by_age_tbl, aes(AgeClass, Andel)) +
  geom_bar(stat = "identity", fill = colors[["blue"]]) +
  custom_theme()  +
  theme(axis.title = element_blank())

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


tbl_4_contact <- imp %>% 
  group_by(HarAnvantMetoden = ifelse(FeatureInvestigated == 1, 'Användare', 'Övriga')) %>% 
  summarize(`Email` = 100*mean(contact_info_edm, na.rm = TRUE),
            `Telefon` = 100*mean(contact_info_tm, na.rm = TRUE)) %>% 
  pivot_longer(c(Email, Telefon), values_to = 'Andel', names_to = 'KontaktInfo')

slide_4_contact <- ggplot(tbl_4_contact, aes(x = KontaktInfo, y = Andel, fill = HarAnvantMetoden)) +
  geom_bar(stat = "identity", position = "dodge") +
  theme(panel.background = element_blank(),
        axis.ticks = element_blank(),
        axis.title.x = element_blank(),
        legend.title = element_blank(),
        panel.grid.major.y = element_line(color = "grey"))
  
tbl_5_duration <- grunddata %>% 
  group_by(DurationClass, Användare) %>% 
  summarize(Antal = sum(customers, na.rm = TRUE)) %>% 
  group_by(DurationClass) %>% 
  mutate(Andel = 100*Antal/sum(Antal)) %>% 
  ungroup() %>% 
  mutate(DurationClass = DurationClass %>% 
           stringr::str_replace_all("to", "till ") %>% 
           stringr::str_squish() %>% 
           factor(levels = c("0", "1", "2", "3", "4", "5 till 6", "7 till 9", "10 till 14", "15 till 19", "20 till 29", "30+", "Missing"))) %>% 
  filter(Anv?ndare == 'Anv?ndare')

slide_5_duration <- ggplot(tbl_5_duration, aes(x = DurationClass, y = Andel)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.2), fill = colors[["blue"]]) +
  custom_theme() +
  theme(axis.title = element_blank())



grunddata %>% 
  filter(FeatureInvestigated == 1) %>% 
  mutate(across(c(product1, product2, product3, product4),
                function(x) ifelse(x>0, 1, 0), .names = "{col}_binary")) %>% 
  mutate(across(c(product1, product2, product3, product4, product1_binary, product2_binary, product3_binary, product4_binary), function(x) x*customers)) %>% 
  
  
 
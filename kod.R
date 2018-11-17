library(tidyverse)
species_data <- read_csv("../HW_data/SpeciesObservations-2018-11-07-14-23-54.csv")
glimpse(species_data)
names(species_data) <- gsub(" ", "_", names(species_data))
species_data <- species_data %>%
  mutate(Start = as.Date(Start),
         End = as.Date(End),
         Uncertain_determination = as.logical(Uncertain_determination), 
         Date = Start)
birds_data <-filter(species_data, Organism_group == "Fåglar")
birds_data <- birds_data %>%
  mutate(Date = format(Date,format="%b")) %>%
  filter(Date == "jan" | Date == "jul") %>%
  count(Common_name) %>%
  arrange(desc(n))
species_data_weekdays <- species_data %>%
  mutate(Weekday = weekdays(Date)) %>%
  group_by(Weekday)
ggplot(species_data_weekdays, mapping = aes(x = factor(Weekday,
                                          levels= c("måndag","tisdag", "onsdag", "torsdag", "fredag", "lördag", "söndag")))) +
         geom_bar() +
  labs(x = "Veckodag",y = "Antal")
ggplot(species_data,mapping = aes(Weekday)) +
  geom_bar()
species_data_lovsangare <- species_data %>%
  filter(Scientific_name == "Phylloscopus trochilus")%>%
  arrange(Date)%>%
  select( Recorded_by, Date)
head(species_data_lovsangare,5)

species_data_lov_talg <- species_data %>%
  filter(Common_name %in% c("lövsångare", "talgoxe"))%>%
  mutate(Month = months(Date))%>%
  group_by(Month)%>%
  count(Common_name)%>%
  ggplot(mapping = aes(x=Month,y=n,color=Common_name)) + 
  geom_point() +
  labs(x="Månad", y="Antal observerade", color="Fågel")
species_data_lov_talg  
as.Date.POSIXlt()

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


Apartment_prices <- read_csv("../HW_data/booli_sold.csv")
asdf<-lm(data = Apartment_prices, soldPrice ~ livingArea)
asdf

ggplot(asdf, mapping = aes(x = soldPrice,y = livingArea)) +
  geom_smooth(se=FALSE) +
  labs(x="Säljpris", y = "Boarea") + 
  geom_point()
Apartment_prices_trends1 <- Apartment_prices %>%
  mutate(soldDate = format(soldDate,format="%Y"))%>%
  mutate(price_per_sqm = soldPrice / livingArea)%>%
  ggplot(mapping = aes(x=soldDate, y=price_per_sqm)) + 
  geom_point(na.rm=T) +
  labs(x="Säljår", y="Pris per kvadratmeter")
Apartment_prices_trends1
Apartment_prices_trends2 <- Apartment_prices %>%
  mutate(soldDate = format(soldDate,format="%b"))%>%
  mutate(price_per_sqm = soldPrice / livingArea)%>%
  ggplot(mapping = aes(x=soldDate, y=price_per_sqm)) + 
  geom_point(na.rm=T) +
  labs(x="Säljmånad", y="Pris per kvadratmeter")
Apartment_prices_trends2

aspect_tabell<- Apartment_prices %>%
  mutate(soldDate = format(soldDate,format="%b"))%>%
  mutate(price_per_sqm = soldPrice / livingArea)%>%
  group_by(soldDate)%>%
  summarise(
    medelpris = mean(price_per_sqm, na.rm=TRUE))
knitr::kable(aspect_tabell)


ggplot(data=Apartment_prices, mapping = aes(rooms)) + 
  geom_histogram() +
  labs(x = "Antal rum", y = "Count")


ggplot(data=Apartment_prices, mapping = aes(x=source.name,y = soldPrice)) +
  geom_boxplot() +
  labs("x=mäklarfirma",y="Säljpris")


Apartment_prices_between <- Apartment_prices %>%
  mutate(soldPrice)%>%
  select(soldPrice)%>%
  filter(between(soldPrice, 0, 2000000),between(soldPrice,2000001,4000000))
#filter by between sale prices, ncount dem
Apartment_prices_between
test1 <- Apartment_prices %>%
  select(soldPrice) %>%
  filter(between(soldPrice,0,2000000))

test2 <- Apartment_prices %>%
  select(soldPrice) %>%
  filter(between(soldPrice,2000001,4000000))
  
test3 <- Apartment_prices %>%
  select(soldPrice) %>%
  filter(between(soldPrice,4000001,6000000))


test4 <- Apartment_prices %>%
  select(soldPrice) %>%
  filter(between(soldPrice,6000001,8000000))
ggplot(data=test1,mapping = aes(soldPrice)) +
  geom_histogram()

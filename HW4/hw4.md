HW4
================
Philip Sierpinski
1 december 2018

R Markdown
----------

``` r
library(RSQLite)
library(DBI)
library(tidyverse)
library(dbplyr)
library(plyr)
```

``` r
#my query for the database.
my_db <- DBI::dbConnect(RSQLite::SQLite(), "../HW_data/system.sqlite")
dbListTables(my_db) #checking tables in my_db
```

    ## [1] "stock" "store"

``` r
stock <- dbGetQuery(my_db, "SELECT * FROM stock") #to view the data
store <- dbGetQuery(my_db, "SELECT * FROM store") #to view the data
```

``` r
#my query for butik_ombud in SQL
butik_ombud <- dbGetQuery(my_db, "SELECT Address5 AS county, COUNT(Address5) AS Number, Typ AS Type
                          FROM store
                          GROUP BY Address5,Typ"
                          )
glimpse(butik_ombud)
```

    ## Observations: 43
    ## Variables: 3
    ## $ county <chr> NA, "Blekinge län", "Blekinge län", "Dalarnas län", "Da...
    ## $ Number <int> 0, 6, 10, 18, 26, 3, 7, 12, 25, 10, 13, 11, 46, 16, 12,...
    ## $ Type   <chr> "Ombud", "Butik", "Ombud", "Butik", "Ombud", "Butik", "...

``` r
#my query for assortment in SQL
assortment <- dbGetQuery(my_db, "SELECT Nr AS Store, Address5 AS county, Address4 as City, stock.Varugrupp AS Product, stock.AntalProdukter
FROM store JOIN stock
ON store.Nr = stock.ButikNr")
```

If there are more stores than agents we get a higher "ratio".

``` r
type_ombud<- butik_ombud%>%
  filter(Type=="Ombud")

type_butik<- butik_ombud%>%
  filter(Type=="Butik")%>%
  left_join(type_ombud, by="county")%>%
  mutate(Antal_Butiker = Number.x, Antal_Ombud = Number.y)%>%
  select(county,Antal_Butiker,Antal_Ombud)%>%
  mutate(county = as.factor(county))%>%
  mutate(Ratio = Antal_Butiker / Antal_Ombud)
levels(type_butik$county) <- c("Blekinge", "Dalarnas", "Gotlands", "Gävleborgs", "Hallands", "Jämtlands", "Jönköpings", "Kalmar", "Kronobergs", "Norrbottens", "Skåne", "Stockholms", "Södermanlands", "Uppsala", "Värmlands", "Västerbottens", "Västernorrlands", "Västmanlands","Västra Götalands", "Örebro", "Östergötlands")


counties <- read_csv("../HW_data/counties.csv")
```

    ## Parsed with column specification:
    ## cols(
    ##   .id = col_integer(),
    ##   long = col_double(),
    ##   lat = col_double(),
    ##   order = col_integer(),
    ##   hole = col_logical(),
    ##   piece = col_integer(),
    ##   id = col_integer(),
    ##   group = col_double(),
    ##   LnKod = col_integer(),
    ##   LnNamn = col_character()
    ## )

``` r
counties<-counties %>%
  mutate(county = as.factor(LnNamn))%>%
  select(-LnNamn)%>%
  left_join(type_butik, by="county")



ggplot(counties) + geom_polygon(aes(x = long, y = lat, group = group, fill = Ratio)) +
    coord_fixed() +
    theme_bw()
```

![](hw4_files/figure-markdown_github/unnamed-chunk-5-1.png)

The visual cluster does show a few clusters present.

Komplettering - Product Patterns
--------------------------------

``` r
my_scale <- function(x)((x-mean(x))/sd(x))

assortment_wide <- assortment %>%
  group_by(Store)%>%
  mutate_if(is.numeric, funs(. / sum(AntalProdukter))) %>% #räknar ut andelen
  spread(key = Product,value = AntalProdukter)

assortment_wide[is.na(assortment_wide)] <- 0 #replace NA's med 0


x<-assortment_wide[,4:21]%>% #scalar alla produkter
  mutate_all(my_scale)

SVD <- svd(x)

u <- SVD$u
colnames(u) <- paste0("col" ,c(1:ncol(u)))

u<- as.data.frame(u)

decomp <- assortment_wide %>% 
  bind_cols(assortment_wide, u[, 1:2])

decomp_graph_cols <- ggplot(data=decomp, aes(col1,col2))  +
  geom_point()

decomp_graph_cols #plottar 
```

![](hw4_files/figure-markdown_github/unnamed-chunk-6-1.png)

``` r
decomp_identify <- decomp %>% 
  filter(col1 >= 0.1 & col2>=0.3) #identifierar outliers 
```

The outliers as seen are:

``` r
#identifying outliers
decomp_identify
```

    ## # A tibble: 2 x 44
    ## # Groups:   Store [2]
    ##   Store county City  `Akvavit och Kr~ Alkoholfritt   Cider  Cognac
    ##   <chr> <chr>  <chr>            <dbl>        <dbl>   <dbl>   <dbl>
    ## 1 0110  Stock~ STOC~          0.00504       0.0756 0.00840 0.00168
    ## 2 0166  Stock~ STOC~          0.00613       0.0773 0.00859 0.00245
    ## # ... with 37 more variables: `Gin och Genever` <dbl>, `Glögg och
    ## #   Glühwein` <dbl>, Likör <dbl>, `Mousserande vin` <dbl>, Portvin <dbl>,
    ## #   Rom <dbl>, Rosévin <dbl>, `Rött vin` <dbl>, Sherry <dbl>,
    ## #   Whisky <dbl>, `Vitt vin` <dbl>, `Vodka och Brännvin` <dbl>, Öl <dbl>,
    ## #   Övrigt <dbl>, Store1 <chr>, county1 <chr>, City1 <chr>, `Akvavit och
    ## #   Kryddat brännvin1` <dbl>, Alkoholfritt1 <dbl>, Cider1 <dbl>,
    ## #   Cognac1 <dbl>, `Gin och Genever1` <dbl>, `Glögg och Glühwein1` <dbl>,
    ## #   Likör1 <dbl>, `Mousserande vin1` <dbl>, Portvin1 <dbl>, Rom1 <dbl>,
    ## #   Rosévin1 <dbl>, `Rött vin1` <dbl>, Sherry1 <dbl>, Whisky1 <dbl>, `Vitt
    ## #   vin1` <dbl>, `Vodka och Brännvin1` <dbl>, Öl1 <dbl>, Övrigt1 <dbl>,
    ## #   col1 <dbl>, col2 <dbl>

``` r
clusters <- decomp %>%
  ungroup(Store) %>% 
  select(col1, col2) %>%
  dist() %>%
  hclust()
plot(clusters)
```

![](hw4_files/figure-markdown_github/unnamed-chunk-8-1.png)

``` r
clusters_grouping <- cutree(clusters, 6) ##delar in i 6 clusters
assortment2 <- cbind(decomp, ClusterGroup = factor(clusters_grouping)) 


ggplot(data=assortment2,aes(col1,col2, color = ClusterGroup)) +
  geom_point()
```

![](hw4_files/figure-markdown_github/unnamed-chunk-8-2.png)

Applying hierarchical clustering to the data divides the set into distinct groups. I'd say the colored clusters do agree pretty well with the visual clusters in the previous plot. ClusterGroup 3 (green) does however seem to have several "mini-clusters" within itself.

``` r
x_tran <- t(x)

SVD_tran <- svd(x_tran)

u_tran <- SVD_tran$u
u_tran <- as.data.frame(u_tran)

rownames(u_tran) <- rownames(x_tran) #behövs för att kunna plotta heatmapen korrekt senare

u_tran2 <- SVD_tran$u  #behövs för att sortera produkter enligt cluster group
u_tran2 <- as.data.frame(u_tran2)



clusters_tran2 <- u_tran2 %>%
  select(V1, V2) %>%
  dist() %>%
  hclust()

clusters_tran <- u_tran %>% 
  select(V1, V2) %>%
  dist() %>%
  hclust()
plot(clusters_tran) #
```

![](hw4_files/figure-markdown_github/unnamed-chunk-9-1.png)

``` r
clusters_grouping_tran <- cutree(clusters_tran, 5) # ser indelning
```

``` r
clusterordning<- cutree(clusters_tran2, 5)
col_names <- colnames(assortment_wide[4:21]) #column names for heatmap


korrekt_ordning<- as.data.frame(col_names) #to make sure col names are ordered correctly
korrekt_ordning <- korrekt_ordning %>% 
  mutate(cluster=clusterordning) %>% 
  arrange(cluster)
```

``` r
type_and_clusters <- decomp %>%
  select(col_names,Store) %>% 
  gather(col_names, key = Product, value = andel) %>% 
  mutate(skalerad_andel = my_scale(andel), Product = factor(Product, levels=(korrekt_ordning$col_names)))
#gör om Product till factor för att kunna ordna dem enligt cluster grupp
type_and_clusters %>% 
  ggplot(aes(x = Store, y = Product)) + geom_tile(aes(fill = skalerad_andel)) + scale_fill_gradient2() + xlab("Stores") + scale_y_discrete(labels = paste0(korrekt_ordning$col_names, 
                                  as.character(korrekt_ordning$cluster)))
```

![](hw4_files/figure-markdown_github/unnamed-chunk-11-1.png)

In the heatmap above you can see the what cluster each type of drink belongs to. You can also tell what type of drinks are most available in stores. Rött vin, Vitt vin and Öl represent a large portion of most stores inventory.

``` r
#plotting dendrogram of transpose of x
plot(clusters_tran)
```

![](hw4_files/figure-markdown_github/unnamed-chunk-12-1.png)

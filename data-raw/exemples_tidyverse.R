##exemples d utilisation du tidyverse
library(tidyverse)



consos <- readRDS('data/consos_clean.RDS')


input <- list(region = 'Bretagne')

choix_possibles <- consos %>% filter(nom_region == input$region) %>%
  pull(nom_departement) %>% unique()

##select : selectionner des colonnes
consos %>% select(conso_totale_residentiel_mwh_,conso_totale_professionnel_mwh_) %>%
  head()

##contains
consos %>% select(starts_with('conso')) %>% head()


###filter  : filtrer les lignes 
consos %>%
  filter(nom_departement == 'Nord', annee == 2014)

##distinct : selectionnner les valeurs distinctes, dedoublonner 
consos %>% distinct(nom_region,annee)

##arrange : trier 
consos %>% distinct(nom_region,annee) %>% arrange(nom_region)

##mutate : rajotuer des colonnes
consos2 <- consos %>%
  select(conso_totale_residentiel_mwh_ , conso_totale_professionnel_mwh_ ) %>%
  mutate(res_et_prod = conso_totale_residentiel_mwh_ + conso_totale_professionnel_mwh_) 

##rename : renommer des colonnes
consos_rnm <- consos2 %>%
  rename(resid = conso_totale_residentiel_mwh_)


###join : jointure de table 

##les noms de departements de bretagne
bretagne <- consos %>% 
  filter(nom_region == 'Bretagne') %>%
  distinct(nom_departement) %>%
  mutate(coucou = 'coucou')


consos_bretagne <- consos %>%
  inner_join(bretagne)


##pivoter les tables 
consos_long <- consos %>%
  select(contains('conso_t'), 'nom_departement','annee') %>%
  pivot_longer(-c('annee', "nom_departement"))


##pivot long a large
consos_large <- consos_long %>%
  pivot_wider( names_from = 'name', values_from =  'value')
head(consos_large)


head(consos_long)


##recuperer la region
region <- consos %>% 
  filter(nom_departement == input$dep) %>%
  filter(annee %in% input$annee)  %>%
  distinct(annee, nom_region)

##recuperer tous les autres departemetns de la meme region 
consos_region <- consos %>%
  inner_join(region, by = c('annee',"nom_region"))

##selectionner seulement l'annee, le dep et les consos moyennes
consos_region <- consos_region %>%
  select(annee, nom_departement, contains('conso_moyenne'))


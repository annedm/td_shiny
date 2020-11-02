# libraries
library(shiny)
library(dplyr)
library(shinydashboard)
library(DT)
library(tidyverse)
library(plotly)


# Preparation des données -------------------------------------------------

consos <- readRDS('data/consos_clean.RDS')

##Consos mailles régionales pour l onglet regions

#input <- list(dep = 'Doubs', annee = 2011:2017)

# ui ----------------------------------------------------------------------


# Define UI for application that draws a histogram
ui <- dashboardPage(
  
  dashboardHeader(
    title="Analyses des consommations electriques"
  ),
  
  dashboardSidebar(
    # Choix du département 
    selectInput("dep",
                "Choisissez votre departement:",
                choices = levels(consos$nom_departement),
                selected = 'Doubs'),
    # Choix de l'année 
    selectInput("annee",
                "Choisissez votre année:",
                choices = sort(unique(consos$annee)),
                multiple = TRUE,
                selected = sort(unique(consos$annee)))
  ),
  
  dashboardBody(
    h5(textOutput('nom_dep')
       , dataTableOutput('ma_table',width = "40%"))
    
    
    # barplots des repartition par secteur et par an 
    , plotOutput('repartition')
    
    # courbes des evolutions des differents secteurs
    ,plotOutput('evolution')
    
    #le boxplot des consos moyennes des departements de la region
    ,plotOutput('boxplot_conso_moyenne')
  )  
) 




#####TODO: rajouter les onglets suivants :
#####Analyse des determinants de la conso
#####Cartographie




# Server ------------------------------------------------------------------


# Define server logic required to draw a histogram
server <- function(input, output) {
  
  output$nom_dep <- renderText({input$dep})
  
  # Cette fonction filtre le jeu de données entier
  # pour ne garder que ce qui est intéressant
  
  
  filtre <- reactive({
    ##TODO: rajouter aussi un filtre sur les annees
    out <- consos %>% 
      filter(nom_departement == input$dep) %>%
      filter(annee %in% input$annee)
    
    ##ne garder que les colonnes qui nous interessent
    out <-  out %>%
      select(annee,  conso_totale_residentiel_mwh_,
             conso_totale_professionnel_mwh_,
             conso_totale_agriculture_mwh_,
             conso_totale_tertiaire_mwh_,
             conso_totale_autres_mwh_)
    
    out 
    
  })
  
  
  get_departement_region <- reactive({
    
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
    
     
    print(head(consos_region))
     consos_region
    
  })
  
  ##Creation de la table a afficher
  
  
  output$ma_table <- renderDataTable({
    out <-  filtre() %>%
      mutate_all(list(round)) %>%
      rename_all(list(str_replace), pattern= 'conso_totale_', replacement = '')
    #print(out)
    out
  } )
  
  ##barplots par annee, couleurs par secteur d'activite
  output$repartition <- renderPlot({
    
    df_filtre <- filtre()  %>%
      tidyr::pivot_longer(-c("annee"))
    
    
    ggplot(df_filtre) +
      geom_bar(stat = 'identity') +
      aes(y  = value, x = annee, fill = name)
    
  }) 
  
  ##graphique des courbes 
  output$evolution <- renderPlot({
    df <- filtre() %>%
      tidyr::pivot_longer(-c("annee"))
    
    
    fig <- ggplot(  df) + 
      aes(y = value, x = annee, color = name)+
      geom_line() + 
      theme_bw()+
      theme(legend.position = 'bottom')
    
    
    fig
  })
  
  ###boxplot des consos moyennes
  output$boxplot_conso_moyenne <- renderPlot({
    
    
    df <- get_departement_region() %>%
      pivot_longer(-c("annee", "nom_departement")) %>%
      mutate(annee = as.character(annee),
             name = str_replace(name, pattern = 'conso_moyenne_', rep = '') %>%
               str_replace(pattern = 'mwh_', rep = '')
               )  
      
    
    
    ggplot(df) +
      geom_boxplot() + 
      facet_wrap(~ name  , scales = 'free') +
      aes(y = value,   x = annee,  fill = annee)+
      theme(legend.position = 'bottom' ) + 
      ggtitle('Le titre') + 
      ylab('les ordonneees') + 
      xlab('les abscisse')
    
    
  })
}




# Run the application 
shinyApp(ui = ui, server = server)

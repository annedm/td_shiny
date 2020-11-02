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
  )
  ##TODO : répartition des consos par secteur et année
  ##TODO: évolution des consos par secteur au cours du temps
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
}




# Run the application 
shinyApp(ui = ui, server = server)

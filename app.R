# libraries
library(shiny)
library(dplyr)
library(shinydashboard)
library(DT)


# Preparation des données -------------------------------------------------

consos <- readRDS('data/consos_clean.RDS')

##Consos mailles régionales pour l onglet regions



# ui ----------------------------------------------------------------------


# Define UI for application that draws a histogram
ui <- dashboardPage(
  
  dashboardHeader(
    title="Analyses des consommations electriques"
  ),
  
  dashboardSidebar(
    sidebarPanel(
      # Choix du département 
      selectInput("dep",
                  "Choisissez votre departement:",
                  choices = levels(consos$nom_departement),
                  selected = 'Doubs')
    )
      # Choix de l'année 
  ),
  
  dashboardBody(
    h5('DataTable'), 
    h3(textOutput('nom_dep'), dataTableOutput('ma_table'))
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
    consos %>% 
      filter(nom_departement == input$dep)
  })
  
  ##Creation de la table a afficher
  ##TODO : remplacer par un datatable (dans server et ui)
  ##TODO: prendre toute la table et pas les six premieres lignes 
   output$ma_table <- renderDataTable({
   out <-  filtre() %>%
     select(- contains('superficie'),
            - contains('residences'),
            - contains('taux')
            ,- contains('geos'))
   print(out)
   out
  } )
  
}




# Run the application 
shinyApp(ui = ui, server = server)

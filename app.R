# libraries
library(shiny)
library(dplyr)
library(shinydashboard)
library(DT)
library(tidyverse)
library(plotly)


# Preparation des données -------------------------------------------------

consos <- readRDS('data/consos_clean.RDS')

# variables quali d'interet dans le deuxieme onglet
var_quali <- c('nom_region','annee', 'nom_departement')
# variables quanti d'interet dans le deuxieme onglet
var_quanti <-colnames(consos)[c(1,21,30:46)]


#pour pouvoir faire des tests (a virer)
#input <- list(dep = 'Doubs', annee = 2011:2017)

# ui ----------------------------------------------------------------------


# Define UI for application that draws a histogram
ui <- dashboardPage(
  
  dashboardHeader(
    title="Analyses des consommations electriques"
  ),
  
  dashboardSidebar(
    
    sidebarMenu(
      menuItem("Département", tabName = "departement"),
      menuItem("Analyse résidentielle", tabName = "resid")
    ),
    
    # Choix du département 
    selectInput("region",
                "Choisissez votre region:",
                choices = unique(consos$nom_region),
                selected = 'Bretagne'),
    
    
    ##ui conditionnelle
    uiOutput('ui_departement'),
    # Choix de l'année 
    selectInput("annee",
                "Choisissez votre année:",
                choices = sort(unique(consos$annee)),
                multiple = TRUE,
                selected = sort(unique(consos$annee))),
    
    
    ###les inputs du second onglet
    selectInput('var_qual',
                'Variable qualitative',
                choices = var_quali,
                selected = var_quali[1]),
    
    selectInput('var_quant',
                'Variable quantitative',
                choices = var_quanti,
                selected = var_quanti[1]),
    
    ##action button 
    actionButton(inputId = 'action','Lancer l analyse')
    
  ),
  
  dashboardBody(
    tabItems(
      #Premier onglet : mon departement
      tabItem('departement',
      h5(textOutput('nom_dep')
         , dataTableOutput('ma_table',width = "40%"))
      
      
      # barplots des repartition par secteur et par an 
      , plotOutput('repartition')
      
      # courbes des evolutions des differents secteurs
      ,plotOutput('evolution')
      
      #le boxplot des consos moyennes des departements de la region
      ,plotOutput('boxplot_conso_moyenne')
      
      ),
      #Deuxieme onglet: analyse residentielle
      tabItem('resid',
              h5('Analyse des déterminants des consommations résidentielles'),
              
              plotOutput('densites'),
              plotOutput('scatterplots')
              
              )
      
    )#tabitems
    
  )   #dashboardbody
)  #shinydashboards




#####TODO: rajouter les onglets suivants :
#####Analyse des determinants de la conso
#####Cartographie




# Server ------------------------------------------------------------------


# Define server logic required to draw a histogram
server <- function(input, output) {

  

# Premier onglet: analyse du departement ----------------------------------

    
  output$nom_dep <- renderText({input$dep})
  

  # Ui interactive de departement qui choisit parmi la region 
  
  
  output$ui_departement <-  renderUI({
    
    
    ##selectionner les bons departements
    choix_possibles <- consos %>% 
      filter(nom_region == input$region) %>%
      pull(nom_departement) %>%
      unique()
    
    selectInput("dep",
        "Choisissez votre departement:",
        choices = choix_possibles,
        selected = choix_possibles[1])
  })
    
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
    
    
    df_reg <- get_departement_region() %>%
      pivot_longer(-c("annee", "nom_departement")) %>%
      mutate(annee = as.character(annee),
             name = str_replace(name, pattern = 'conso_moyenne_', rep = '') %>%
               str_replace(pattern = 'mwh_', rep = '')
               )  
      
    
    
    ggplot() +
      geom_boxplot(data = df_reg,
                   aes(y = value,   x = annee)) + 
      geom_point(data = df_reg %>% 
                  filter(nom_departement == input$dep),
                aes(y = value, x = annee),
                col = 'red'
                ) +
      
      facet_wrap(~ name  , scales = 'free') +
      
      theme_bw() + 
      theme(legend.position = 'bottom' ) + 
      ggtitle('Le titre') + 
      ylab('les ordonneees') + 
      xlab('les abscisse')
  })
    
    

# Deuxieme onglet : analyse des residentiels ------------------------------
  ##filtre sur les annees, bouge seuleemnt quand appui sur go

  graphe_densite <- eventReactive(input$action,{
    
    print(input$var_qual)
    ##densite selon une variable quali 
    a_tracer <- consos %>% 
             filter(annee %in% input$annee)%>%
      mutate_at(input$var_qual, as.factor)
    
    
    
    ggplot(a_tracer)+
      aes_string( 'conso_moyenne_residentiel_mwh_',
                  colour = input$var_qual)+
      geom_density( )+
      ggtitle(paste0('conso moyenne residentielle selon ',
                     input$var_qual))+
      theme_bw() +
      theme(legend.position = 'bottom')
    
    
  })
  output$densites <- renderPlot({
    graphe_densite()
  })
    
  output$scatterplots <- renderPlot({
    
    #scatterplot selon une variable quanto
    
    ggplot(consos %>% 
             filter(annee %in% input$annee))+
      aes_string( y = 'conso_moyenne_residentiel_mwh_',
                  colour = input$var_qual,
                  x = input$var_quant)+
      geom_point( )+
      ggtitle(paste0('conso moyenne residentielle selon ',
                     input$var_quant, 'et ', input$var_qual))+
      theme_bw() +
      theme(legend.position = 'bottom')
  })
    

}




# Run the application 
shinyApp(ui = ui, server = server)

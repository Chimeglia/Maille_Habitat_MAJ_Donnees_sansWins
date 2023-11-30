library(shiny)
rm(list=ls())


Resultat=function(nb_mailles,population_min,nb_secteurs,listeIndicateurs_maillage,listeIndicateurs_sectorisation) {
  print("démarrage")

  
  load("Données/Donnees_Logement_MAJ.Rdata")
  Donnees_Logement=Donnees_Logement_MAJ
  Import_carteCommunesCorses=st_read("Données/SHP_COMMUNES/COMMUNE.shp")
  carteCommunesCorses=Import_carteCommunesCorses %>% select(INSEE_COM,geometry)
  colnames(carteCommunesCorses)[1]="CODGEO"
  carteCommunesCorses=carteCommunesCorses %>% arrange(CODGEO)
  carteCommunesCorses <- st_transform(carteCommunesCorses, crs = 4326)
  
  Indicateurs_Complet= Donnees_Logement %>% 
    mutate(indic.transac = round(100 * nb_mut / LOG, 1),
           indic.menag = round(POP/RP, 1),
           indic.rs = round(100 * RSECOCC / LOG, 1),
           indic.lv = round(100 * LOGVAC / LOG, 1),
           indic.jeun = round(jeun / anc, 1),
           indic.social = round(100 * social / tot_lgmt, 1),
           indic.prop = round(100 * proprietaire / RP, 1),
           indic.duroc = duroc,
           indic.prix = round(100 * moyenne_prix/rev_ucm, 1),
           indic.airbnb.nb_lgmt=round(100*Loc.actives/LOG, 1),
           indic.airbnb.revTot_annee=rev.LTM_tot,
           indic.airbnb.revMoy_lgt=rev.LTM_moy_lgt,
           indic.log=LOG) %>% 
    select(CODGEO,LIBGEO,POP,LOG,contains("indic")) %>% 
    mutate_if(is.numeric, funs(ifelse(is.na(.), 0, .)))
  
  
  Indicateurs_parCommune= Indicateurs_Complet %>% 
    select(CODGEO,LIBGEO,POP,LOG,listeIndicateurs_maillage)
  
  
  
  listeIndicateurs=Indicateurs_parCommune %>% select(contains("indic")) %>% names()
  Indicateurs_parCommune_valeurs=Indicateurs_parCommune[listeIndicateurs] 
  carteCommunesCorses_spdf=carteCommunesCorses %>% left_join(Indicateurs_parCommune,by="CODGEO")%>% select(CODGEO,LIBGEO,POP) %>%  as("Spatial")
  table_voisinage=poly2nb(carteCommunesCorses_spdf,queen=F)
  ACP_parCommunes <- Indicateurs_parCommune_valeurs %>%  PCA(graph = F)
  nb_dim <- which(ACP_parCommunes$eig[, 3] > 80)[1] 
  ACP_parCommunes <- Indicateurs_parCommune_valeurs %>%  PCA(graph = F,ncp = nb_dim)
  donnees_pondérations=ACP_parCommunes$ind$coord
  pondérations=nbcosts(table_voisinage, data = donnees_pondérations)
  table_voisinage_pondérée=nb2listw(table_voisinage, glist = pondérations, style = "C")
  arbre_poids_minimal <- mstree(table_voisinage_pondérée)
  
  resultat_skater=skater(edges = arbre_poids_minimal[,1:2],data=donnees_pondérations,ncuts=nb_mailles-1,crit=population_min,vec.crit=Donnees_Logement$POP)
  
  carteMailles=carteCommunesCorses %>% mutate(maille=resultat_skater$groups) %>%  group_by(maille) %>%  summarise(geometry = st_union(geometry))
  
  Maillage=data.frame(CODGEO=Indicateurs_parCommune$CODGEO,maille=resultat_skater$groups)

  print("maillage terminé")    
  
    
  Indicateurs_parMaille=Maillage %>% 
    left_join(Donnees_Logement, by="CODGEO") %>% 
    group_by(maille) %>% 
    summarise_if(is.numeric, funs(sum(., na.rm = TRUE))) %>% 
    mutate(indic.transac = round(100 * nb_mut / LOG, 1),
           indic.menag = round(POP/RP, 1),
           indic.rs = round(100 * RSECOCC / LOG, 1),
           indic.lv = round(100 * LOGVAC / LOG, 1),
           indic.jeun = round(jeun / anc, 1),
           indic.social = round(100 * social / tot_lgmt, 1),
           indic.prop = round(100 * proprietaire / tot_lgmt, 1),
           indic.duroc = duroc,
           indic.prix = round(100 * moyenne_prix/rev_ucm, 1),
           indic.airbnb.nb_lgmt=round(100*Loc.actives/LOG, 1),
           indic.airbnb.revTot_annee=rev.LTM_tot,
           indic.airbnb.revMoy_lgt=rev.LTM_moy_lgt,
           indic.log=LOG) %>%  
    select(maille,POP,LOG,listeIndicateurs_sectorisation) %>% 
    mutate_if(is.numeric, funs(ifelse(is.na(.), 0, .)))
  
  
  
  
  Indicateurs_parMaille_valeurs=Indicateurs_parMaille %>% select(contains("indic"))
  acp_parMailles=PCA(Indicateurs_parMaille_valeurs,ncp=5,graph=F)
  hc4 <- HCPC(acp_parMailles, nb.clust = nb_secteurs, consol = F, graph = T)
  cl4 <- data.frame(maille = Indicateurs_parMaille$maille, cl_maille4 = hc4$data.clust[,"clust"])
  
  print("sectorisation terminée") 
  
  carteMaillesSecteurs <- ms_simplify(left_join(carteMailles, cl4, by = "maille"))
  
  carteCommunesCorses_merged <- left_join(carteCommunesCorses, Donnees_Logement, by = "CODGEO")
  carteCommunesCorses_sf <- ms_simplify(st_as_sf(carteCommunesCorses_merged))
  
  
  
  Indicateurs_ac_mailles_et_secteurs=Indicateurs_Complet %>% 
    left_join(Maillage, by="CODGEO") %>% 
    left_join(cl4, by="maille")
  
 
  Secteur_Ajaccio = Indicateurs_ac_mailles_et_secteurs %>%
    filter(CODGEO == "2A004") %>%
    select(cl_maille4) %>%
    pull()
  
  Secteur_Lozzi= Indicateurs_ac_mailles_et_secteurs %>%
    filter(CODGEO == "2B147") %>%
    select(cl_maille4) %>%
    pull()
  
  Secteur_POVO= Indicateurs_ac_mailles_et_secteurs %>%
    filter(CODGEO == "2A247") %>%
    select(cl_maille4) %>%
    pull()
  
  
  palette=rep(NA,nb_secteurs)
  
  
  
  palette[Secteur_Ajaccio]="deeppink"
  palette[Secteur_Lozzi]="seagreen3"
  palette[Secteur_POVO]="orange2"
  

  couleurs_sup <- switch(
    as.character(nb_secteurs),
    "4" = "blueviolet",
    "5" = c("blueviolet", "coral1"),
    "6" = c("blueviolet", "coral1", "cadetblue1")
  )
  
  palette[which(is.na(palette))]=couleurs_sup
  

  
  #palette = c("seagreen3", "orange2","blueviolet", "deeppink")
  
  print("Tracé de la carte") 
  
  # Création d'une palette de couleurs pour les secteurs
  palSecteurs = colorFactor(palette, domain = carteMaillesSecteurs$cl_maille4)
  
  
  # Création d'une carte des communes avec un trait gris fin
  carteCommunes = leaflet(carteCommunesCorses_sf) %>%
    addPolygons(fillColor = "white",
                fillOpacity = 0.8,
                color = "gray",
                weight = 1)
  
  # Ajout des mailles avec un trait noir plus épais et colorées selon le secteur
  carte_MAJ=carteCommunes %>%
    addPolygons(data = carteMaillesSecteurs,
                fillColor = ~palSecteurs(cl_maille4),
                fillOpacity = 0.8,
                color = "black",
                weight = 2)
  
  print("Carte effectuée") 
  
  Nom_cartes_resultat="Résultats/Resultat_temp.html"
  saveWidget(carte_MAJ, file = Nom_cartes_resultat)
  print("Fonction terminée") 
  
}






liste_indicateurs_complete=c("indic.transac","indic.menag","indic.rs","indic.lv","indic.jeun","indic.social","indic.prop","indic.duroc","indic.prix","indic.airbnb.nb_lgmt","indic.airbnb.revTot_annee","indic.airbnb.revMoy_lgt","indic.log")
listeIndicateurs_maillage_ini=c("indic.transac","indic.menag","indic.rs","indic.lv","indic.jeun","indic.social","indic.prop","indic.duroc","indic.prix","indic.airbnb.nb_lgmt","indic.airbnb.revMoy_lgt")
listeIndicateurs_sectorisation_ini=c("indic.transac","indic.menag","indic.rs","indic.lv","indic.jeun","indic.social","indic.prop","indic.duroc","indic.prix","indic.airbnb.nb_lgmt","indic.airbnb.revTot_annee","indic.log")



# 
# ui <- fluidPage(
#   titlePanel("Génération de la typologie"),
#   
#   mainPanel(
#     width="100%",
#     fluidRow(br()),
#     fluidRow(
#       column(
#         width = 6,
#         offset=0,
#         wellPanel(
#           numericInput("nb_mailles", "Nombre de mailles :", value = 36),
#           numericInput("population_min", "Population minimale par maille :", value = 2500),
#           numericInput("nb_secteurs", "Nombre de secteurs :", value = 4)
#         )
#       )
#     ),
#     
#     fluidRow(br()),
#     
#     fluidRow(
#       column(
#         width = 5,
#         offset=0,
#         checkboxGroupInput("liste_ind_1", "Indicateurs utilisés pour le maillage :", choices = liste_indicateurs_complete, selected = listeIndicateurs_maillage_ini)
#       ),
#       
#       column(
#         width = 6,
#         checkboxGroupInput("liste_ind_2", "Indicateurs utilisés pour la sectorisation :", choices = liste_indicateurs_complete, selected = listeIndicateurs_sectorisation_ini)
#       )
#     ),
#     
#     fluidRow(br()),
#     
#     fluidRow(
#       column(
#         width = 6,
#         actionButton("Generer", "Générer la typologie")
#       )
#     )
#   )
# )
# 
# 
# server <- function(input, output) {
#   # Observer le bouton "Valider"
#   observeEvent(input$Generer, {
#     nb_mailles=input$nb_mailles
#     population_min =input$population_min
#     nb_secteurs=input$nb_secteurs
#     print(nb_secteurs)
#     listeIndicateurs_maillage <- input$liste_ind_1
#     listeIndicateurs_sectorisation <- input$liste_ind_2
#     
#     Resultat(nb_mailles,population_min,nb_secteurs,listeIndicateurs_maillage,listeIndicateurs_sectorisation)
# 
#   })
# }
# 
# 
# 
# shinyApp(ui = ui, server = server)








# Création de l'interface utilisateur (UI)
ui <- fluidPage(
  titlePanel("Génération de la typologie"),
  
  # Utilisation de tabsetPanel pour les onglets
  tabsetPanel(
    # Onglet 1
    tabPanel("Paramètres",
             mainPanel(
               width="100%",
               fluidRow(br()),
               fluidRow(
                 column(
                   width = 6,
                   offset=0,
                   wellPanel(
                     numericInput("nb_mailles", "Nombre de mailles :", value = 36),
                     numericInput("population_min", "Population minimale par maille :", value = 2500),
                     numericInput("nb_secteurs", "Nombre de secteurs :", value = 4)
                   )
                 )
               ),
               
               fluidRow(br()),
               
               fluidRow(
                 column(
                   width = 5,
                   offset=0,
                   checkboxGroupInput("liste_ind_1", "Indicateurs utilisés pour le maillage :", choices = liste_indicateurs_complete, selected = listeIndicateurs_maillage_ini)
                 ),
                 
                 column(
                   width = 6,
                   checkboxGroupInput("liste_ind_2", "Indicateurs utilisés pour la sectorisation :", choices = liste_indicateurs_complete, selected = listeIndicateurs_sectorisation_ini)
                 )
               ),
               
               fluidRow(br()),
               
               fluidRow(
                 column(
                   width = 6,
                   actionButton("Generer", "Générer la typologie")
                 )
               )
             )
    ),
    
    # Onglet 2
    tabPanel("Résultats",
             # Contenu du deuxième onglet
             htmlOutput("carteOutput")
    )
  )
)

# Définition de la logique du serveur
server <- function(input, output) {
  # Observer le bouton "Valider"
  observeEvent(input$Generer, {
    nb_mailles <- input$nb_mailles
    population_min <- input$population_min
    nb_secteurs <- input$nb_secteurs
    print(nb_secteurs)
    listeIndicateurs_maillage <- input$liste_ind_1
    listeIndicateurs_sectorisation <- input$liste_ind_2
    
    addResourcePath("Résultats", "C:/Users/jvluc/Mon Drive/Travail/Maille habitat/Maille_Habitat_MAJ_Donnees_sansWins/www/Résultats")
    Resultat(nb_mailles, population_min, nb_secteurs, listeIndicateurs_maillage, listeIndicateurs_sectorisation)
    
    # Afficher la carte dans le deuxième onglet
    output$carteOutput <- renderUI({
      chemin_html="C:/Users/jvluc/Mon Drive/Travail/Maille habitat/Maille_Habitat_MAJ_Donnees_sansWins/Résultats/Resultat_temp.html"
      chemin_html="Résultats/Resultat_temp.html"
      print(chemin_html)
      #includeHTML(chemin_html)
      tags$iframe(src = chemin_html, width = "100%", height = "600px")
    })
  })
}

# Création de l'application Shiny
shinyApp(ui = ui, server = server)


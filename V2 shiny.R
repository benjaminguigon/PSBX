#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

#Importation des librairies utiles pour le developpement du Server et de l'UI
library(shiny)
library(ggplot2)
library(plotly)
library(plyr)
library(flexdashboard)
library(cowplot)
library(shinydashboard)

#---------------------------------------------------------------------------

# DEBUT DE LA PARTIE UI
ui <- dashboardPage(skin = "red", # Choix de la couleur de l'en tête
    dashboardHeader(title = "Dashboard général"), # Création d'n dashboard général dans lequel 
    # l'on créera nos différentes partie de l'interface
    
    dashboardSidebar( "Choix des pages", # Création d'une sidebar pour pouvoir jongler 
    # entre plusieurs onglet dans le Dashboard
                      sidebarMenu( # Menu permettant de créer les différents onglets du dashboard
                                    # https://fontawesome.com/icons?from=io retrouver tous les icones disponibles
                                   menuItem("Iris", tabName = "iris", icon = icon("tree")), # Onglet 1
                                   menuItem("Cars", tabName = "cars", icon = icon("car")), # Onglet 2
                                   menuItem("Machine Learning", tabName = "M_L", icon = icon("database")))), # Onglet 3
    #---------------------------------------------------------------------------
    dashboardBody( # Permet de créer une fenêtre principale 
        tabItems( # Permet de créer plusieurs graph dans la même fenêtre mais pas dans les mêmes onglets
            # Attention, il y a un "s" au tabItems pour spécifié qu'il y aura plusieurs Items
            #--------------------------------------------------------------------
            # Création de la page Iris
            tabItem("iris",
                    fluidPage(h1("Iris"), # Création d'un titre à la page avec les balises HTML 
                    box(box(plotOutput("correlation_plot"),width = 8), # Création d'un 1ère box pour afficher mon Graph de correlation
                    box(selectInput("features","Features:", c("Sepal.Width","Petal.Length","Petal.Width")), width = 4), #
                    box(plotOutput("correlation_plot_2", width = "100%"), width = 12), # Affichage d'un 2ème graph sur une 2ème box
                    box(plotOutput("correlation_plot_3", width = "100%"), width = 12), # Affichage d'un 3ème graph sur une 3ème box
                    width = 12,height = "1350px"))), # Gestion des tailles manuelles laborieuse mais utile l'aspect esthétique
            #--------------------------------------------------------------------
            # Création de la page Cars
            tabItem("cars",
                    fluidPage(h1("Cars")), 
                    dataTableOutput("carstable") # Affichage d'une table directement importée depuis le server via la commande "render"
                    ),
            #--------------------------------------------------------------------
            # Création de la page Machine learning
            tabItem("M_L",# N Nom de la page reliée au menu item plus haut
                    fluidPage(h1("Machine learning with Shiny"),
                              wellPanel( # Création d'une barre d'onlet intern à l'item Machine Learning
                                  tabsetPanel(
                                      #--------------------------------------------------------------------
                                        # Onglet 1
                                        tabPanel("Exemple 1", # Nom de l'onglet
                                                 # Choix d'un Input numérique pour fixer la valeur de la Standard Deviation avec une valeur
                                                 # initiale de 3
                                                 box(numericInput(
                                                     "SD",label = "Standard Deviation", value = 3), height = "100px"),
                                                 # Choix d'un Input numérique pour fixer la valeur de la taille de l'echantillon avec une valeur
                                                 # initiale de 50 et un pas d'incrémentation de 10
                                                 box(numericInput(
                                                     "Sample",label = "Sample size", value = 50,step = 10), height = "100px"),
                                                 # Affichage du graph après récupération des Inputs
                                                 box(plotOutput("Tableau_1",width = "100%"), width = 12)
                                                 # width = "100%" permet de gerer la taille du graphe et width = 12 permet de gerer la taille de la box
                                                 # il faut que les 2 coïncident bien sinon shiny peut écraser les autres graphes de la page
                                                 ),
                                      #--------------------------------------------------------------------
                                       # Onglet 2
                                       tabPanel("Exemple 2",
                                                # Affichage du graph configuré dans la partie serveur au nom de "Tableau_2"
                                                 plotOutput("Tableau_2", height = "600px")
                                                 ),
                                      #--------------------------------------------------------------------
                                        # Onglet 3
                                        tabPanel("Exemple 3",
                                                 # Affichage du graph configuré dans la partie serveur au nom de "Tableau_3"
                                                 plotOutput("Tableau_3", height = "500px")
                                                 ),
                                      #--------------------------------------------------------------------
                                        # Onglet 4
                                        tabPanel("Exemple 4",
                                                 # Affichage du graph configuré dans la partie serveur au nom de "Tableau_4"
                                                 plotOutput("Tableau_4", height = "500px")
                                                ),
                                      #--------------------------------------------------------------------
                                        # Onglet 5
                                        tabPanel("Exemple 5",
                                                 # Choix d'un Input numérique via une reglette pour fixer la valeur du paramètre k
                                                 # avec un minimum de 1, maximum de 200, une valeur initale de 0 et un pas de 10
                                                 # l'Input s'appelle "bins" pour être récuperée dans la partie serveur
                                                 sliderInput(
                                                     "bins","Sample size",min = 1,max = 200,value = 0, step = 10),
                                                 plotOutput("Tableau_5")
                                        ),
                                      #--------------------------------------------------------------------
                                        # Onglet 6
                                        tabPanel("Exemple 6",
                                                 # Pareil que pour l'onglet 5 avec une box numérique comme dans l'onglet 1
                                                 box(numericInput("Val","Valeur de k:", 20, min = 1, max = 20, step = 1)),
                                                 box(sliderInput(
                                                     "bins_2","Parameter",min = 0,max = 100,value = 0, step = 5),height = "100px"),
                                                 box(plotOutput("Tableau_6", width = "100%"), width = 12)
                                        )
                                      #--------------------------------------------------------------------
                                  )),
                              ))
        )
    ),
)
# FIN DE LA PARTIE UI
#----------------------------------------------------------------------------------------------------------------------------------------
# DEBUT DE LA PARTIE SERVEUR
server <- function(input, output) {

    # Paramètres généraux pour plusieurs affichage
    set.seed(955)
    # Création d'un data Frame a 2 colonne qui suivent des lois normales
    dat <- data.frame(cond = rep(c("A", "B"), each=10), xvar = 1:20 + rnorm(20,sd=3), yvar = 1:20 + rnorm(20,sd=3))
    n <- 20
    # 2 repartions de normale de paramètre n fixé plus haut
    x1 <- rnorm(n); x2 <- rnorm(n)
    y1 <- 2 * x1 + rnorm(n)
    y2 <- 3 * x2 + (2 + rnorm(n))
    A <- as.factor(rep(c(1, 2), each = n))
    df <- data.frame(x = c(x1, x2), y = c(y1, y2), A = A)
    # Création d'un modèle linéaire entre y et x
    fm <- lm(y ~ x + A, data = df) 
    
    #---------------------------------------------------------------------------
    # Création du lien entre le server et l'UI
    # Mise en place du 1er onglet, Iris
    output$correlation_plot = renderPlot({
        # Affichage du graph 1 entre les données et le type de caractéristiques de l'iris choisie
        plot(iris$Sepal.Length, iris[[input$features]], xlab = "Sepal length",
             ylab = "Feature") 
    })
    
    output$correlation_plot_2 = renderPlot({
        # Affichage du graph 2, modèle linéaire entre les 2 mêmes caracteristiques de chaque espèce
        # Chaque partie sert à ajouter un spécificité au graphe, aes augmente la visibilité des variables en jeu
        # geom_point affiche le nuage de point
        #facet_grid permet d'afficher les 3 espèces de fleurs sur le même graphe
        ggplot(iris, aes(Sepal.Length, Sepal.Width)) + 
            geom_point() + facet_grid(. ~ Species) +
            stat_smooth(method = "lm") +
            background_grid(major = 'y', minor = "none") +
            panel_border()
    })
    output$correlation_plot_3 = renderPlot({
        # Affichage du graph 3, modèle linéaires entre les 2 mêmes caracteristiques de chaque espèce
        # Pareil que pour le renderPlot supérieur
        ggplot(iris, aes(Petal.Length, Petal.Width)) + 
            geom_point() + facet_grid(. ~ Species) +
            stat_smooth(method = "lm") +
            background_grid(major = 'y', minor = "none") + 
            panel_border()
    })
    
    #---------------------------------------------------------------------------
    # Configuration de la page Cars 
    # Lien entre l'UI et le Server rapide via les fonctions render 
    # Affichage de la table mtcars
    output$carstable = renderDataTable(mtcars)
    
    #---------------------------------------------------------------------------
    # Création de la page Machine Learning
    output$Tableau_1 = renderPlot({
        # Graphe d'affichage de correlation entre 2 variables qui suivent une lois normale
        # on retrouve le "input$Sample" qui est l'input appelé "Sample" dans la partie UI
         dat_1 <- data.frame(cond = rep(c("A", "B"), each=(input$Sample+input$Sample)), xvar = 1:input$Sample + rnorm(input$Sample,sd=input$SD), yvar = 1:input$Sample + rnorm(input$Sample,sd=input$SD))
         ggplot(dat_1, aes(x=xvar, y=yvar)) + geom_point(shape=4)  
    })
    output$Tableau_2 = renderPlot({
        # Affichage du Data Frame dat selon 2 méthodes, Lm (modèle de regression linéaire) 
        #et loess (modèle de regression polynomial)
        A = ggplot(dat, aes(x=xvar, y=yvar)) +geom_point(shape=2) + geom_smooth(method= lm) 
        B = ggplot(dat, aes(x=xvar, y=yvar)) +geom_point(shape=1) +geom_smooth(method = loess)
        ggdraw()+ draw_plot(B, 0, 0, 1, .5) +draw_plot(A, 0, .5, 1, .5) # Organisation des graphes pour avoir une meilleure
        # visibilité des graphes via la fonction draw_plot qui gère la disposition
    })
    output$Tableau_3 = renderPlot({
        #ggplot(dat, aes(x=xvar, y=yvar)) +geom_point(shape=1) +geom_smooth(method = loess)
        df <- data.frame(x <- rchisq(1000, 10, 10),
                         y <- rnorm(1000))
        ggplot(df, aes(x, y)) + 
            geom_point(alpha = 0.5) + 
            geom_density_2d(colour = "red") + 
            theme(panel.background = element_rect(fill = 'white'))
       # ggplotly(p)
    })
    output$Tableau_4 = renderPlot({
        # Double regression linéaire 
        ggplot(data = cbind(df, pred = predict(fm)), aes(x = x, y = y, color = A)) + 
            geom_point() + 
            geom_line(aes(y = pred))
    })
    output$Tableau_5 = renderPlot({
    # Affichage de 3 courbes suivant une fonction Gamma sur un même nombre de point mais avec un 
    # paramètre Gamma différents
        dfGamma = data.frame(nu075 = rgamma(100+input$bins, 0.75),nu1 = rgamma(100+input$bins, 1),nu2 = rgamma(100+input$bins, 2)) #Data frame de 3 colonnes chacune de 100 éléments
        dfGamma = stack(dfGamma) #Met bout à bout les 3 colonnes de longueur 100 pour former un 1 colonne de 300 éléments
        ggplot(dfGamma, aes(x = values)) + stat_density(aes(group = ind, color = ind),position="identity",geom="line")
        # Affiche les 3 courbes gamma associées aux 3 colonnes
    })
    output$Tableau_6 = renderPlot({
        # Affiche la distribution d'une lois Chi-2 suivant 2 inputs qui sont la valeur du k 
        # et la taille de l'échantillon
        x_dchisq <- seq(0, 5+input$bins_2, by = 0.1)  
        y_dchisq <- dchisq(x_dchisq, df = input$Val)
        dftest = data.frame(y_dchisq1 <- dchisq(x_dchisq, df = 5), y_dchisq2 <- dchisq(x_dchisq, df = 6))
        dftest = stack(dftest)
        plot(y_dchisq)
    })
}
# Run the application 
shinyApp(ui = ui, server = server)









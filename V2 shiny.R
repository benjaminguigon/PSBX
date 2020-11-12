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

# Define UI for application that draws a histogram

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
                    fluidPage(h1("Iris")), # Création d'un titre à la page avec les balises HTML 
                    box(plotOutput("correlation_plot"),width = 8), # Création d'un 1ère box pour afficher mon Graph de correlation
                    box(selectInput("features","Features:", c("Sepal.Width","Petal.Length","Petal.Width")), width = 4), #
                    box(plotOutput("correlation_plot_2")) # Affichage d'un 2ème graph sur une 2ème box
                    ),
            #--------------------------------------------------------------------
            # Création de la page Cars
            tabItem("cars",
                    fluidPage(h1("Cars")), 
                    dataTableOutput("carstable")
                    ),
            #--------------------------------------------------------------------
            # Création de la page Machine learning
            tabItem("M_L",
                    fluidPage(h1("Machine learning with Shiny"),
                              wellPanel(
                                  tabsetPanel(
                                      #--------------------------------------------------------------------
                                        tabPanel("Exemple 1",
                                                 box(numericInput(
                                                     "SD",label = "Standard Deviation", value = 3)),
                                                 box(numericInput(
                                                     "Sample",label = "Sample size", value = 50,step = 10)),
                                                 plotOutput("Tableau_1")
                                                 ),
                                      #--------------------------------------------------------------------
                                        tabPanel("Exemple 2",
                                                 plotOutput("Tableau_2")
                                                 ),
                                      #--------------------------------------------------------------------
                                        tabPanel("Exemple 3",
                                                 plotOutput("Tableau_3")
                                                 ),
                                      #--------------------------------------------------------------------
                                        tabPanel("Exemple 4",
                                                 plotOutput("Tableau_4")
                                                ),
                                      #--------------------------------------------------------------------
                                        tabPanel("Exemple 5",
                                                 sliderInput(
                                                     "bins","Parameter",min = 1,max = 200,value = 0, step = 10),
                                                 plotOutput("Tableau_5")
                                        ),
                                      #--------------------------------------------------------------------
                                        tabPanel("Exemple 6",
                                                 box(numericInput("Val","Valeur de k:", 20, min = 1, max = 20, step = 1),
                                                 sliderInput(
                                                     "bins_2","Parameter",min = 0,max = 50,value = 0, step = 5)),
                                                 box(plotOutput("Tableau_6"))
                                        )
                                      #--------------------------------------------------------------------
                                  )),
                              ))
        )
    ),
)


server <- function(input, output) {

    set.seed(955)
    dat <- data.frame(cond = rep(c("A", "B"), each=10), xvar = 1:20 + rnorm(20,sd=3), yvar = 1:20 + rnorm(20,sd=3))
    n <- 20
    x1 <- rnorm(n); x2 <- rnorm(n)
    y1 <- 2 * x1 + rnorm(n)
    y2 <- 3 * x2 + (2 + rnorm(n))
    A <- as.factor(rep(c(1, 2), each = n))
    df <- data.frame(x = c(x1, x2), y = c(y1, y2), A = A)
    fm <- lm(y ~ x + A, data = df)
    
    output$correlation_plot = renderPlot({
        plot(iris$Sepal.Length, iris[[input$features]], xlab = "Sepal length",
             ylab = "Feature") 
        #ggplot(iris, aes(Sepal.Length, Sepal.Width)) + 
         #      geom_point() + facet_grid(. ~ Species) +
          #     stat_smooth(method = "lm") +
           #    background_grid(major = 'y', minor = "none") + 
            #   panel_border()
    })
    
    output$correlation_plot_2 = renderPlot({
        ggplot(iris, aes(Sepal.Length, Sepal.Width)) + 
            geom_point() + facet_grid(. ~ Species) +
            stat_smooth(method = "lm") +
            background_grid(major = 'y', minor = "none") + 
            panel_border()
            
    })
    
    output$carstable = renderDataTable(mtcars)
    
    output$Tableau_1 = renderPlot({
         dat_1 <- data.frame(cond = rep(c("A", "B"), each=(input$Sample+input$Sample)), xvar = 1:input$Sample + rnorm(input$Sample,sd=input$SD), yvar = 1:input$Sample + rnorm(input$Sample,sd=input$SD))
         #A = ggplot(dat_1, aes(x=xvar, y=yvar)) + geom_point(shape=2) 
         #B = ggplot(iris, aes(Sepal.Length, Sepal.Width)) + 
         #    geom_point() + facet_grid(. ~ Species) +
          #   stat_smooth(method = "lm") +
           #  background_grid(major = 'y', minor = "none") + # grilles horiz.
            # panel_border()
         ggplot(dat_1, aes(x=xvar, y=yvar)) + geom_point(shape=2)  
        #ggdraw()+ draw_plot(A) +draw_plot(B, 0, .5, 1, .5)
        #ggdraw()+ draw_plot(A, 0, 0, 1, .5) +draw_plot(B, 0, .5, 1, .5)
        
    })
    
    output$Tableau_2 = renderPlot({
        A = ggplot(dat, aes(x=xvar, y=yvar)) +geom_point(shape=2) + geom_smooth(method= lm) 
        B = ggplot(dat, aes(x=xvar, y=yvar)) +geom_point(shape=1) +geom_smooth(method = loess)
        ggdraw()+ draw_plot(B, 0, 0, 1, .5) +draw_plot(A, 0, .5, 1, .5)
    })
    
    output$Tableau_3 = renderPlot({
        #ggplot(dat, aes(x=xvar, y=yvar)) +geom_point(shape=1) +geom_smooth(method = loess)
        df <- data.frame(x <- rchisq(1000, 10, 10),
                         y <- rnorm(1000))
        
        ggplot(df, aes(x, y)) + 
            geom_point(alpha = 0.5) + 
            geom_density_2d() + 
            theme(panel.background = element_rect(fill = '#ffffff'))
       # ggplotly(p)
    })
    
    output$Tableau_4 = renderPlot({
        ggplot(data = cbind(df, pred = predict(fm)), aes(x = x, y = y, color = A)) + geom_point() + geom_line(aes(y = pred))
    })
    
    output$Tableau_5 = renderPlot({
        dfGamma = data.frame(nu75 = rgamma(100+input$bins, 0.75),nu1 = rgamma(100+input$bins, 1),nu2 = rgamma(100+input$bins, 2)) #Data frame de 3 colonnes chacune de 100 éléments
        dfGamma = stack(dfGamma) #Met bout à bout les 3 colonnes de longueur 100 pour former un 1 colonne de 300 éléments
        ggplot(dfGamma, aes(x = values)) + stat_density(aes(group = ind, color = ind),position="identity",geom="line")
        #Affiche  les 3 colonne du data frame original pour afficher 3 courbes gamma associées aux 3 colonnes
    })
    
    output$Tableau_6 = renderPlot({
        
        #dchisq = Density
        x_dchisq <- seq(0, 5+input$bins_2, by = 0.1)  
        y_dchisq <- dchisq(x_dchisq, df = input$Val)
        
        dftest = data.frame(y_dchisq1 <- dchisq(x_dchisq, df = 5), y_dchisq2 <- dchisq(x_dchisq, df = 6))
        dftest = stack(dftest)
        #ggplot(dftest, aes(x = values)) + stat_density(aes(group = ind, color = ind),position="identity",geom="line")
        plot(y_dchisq)
        #dfChi = data.frame(Chi1 = rchisq(10+input$bins_2,1), Chi2 = rchisq(10+input$bins_2,2), Chi3 = rchisq(10+input$bins_2,3), Chi4 = rchisq(10+input$bins_2,4),
        #                   Chi6 = rchisq(10+input$bins_2,6),Chi9 = rchisq(10+input$bins_2,9),Chi12 = rchisq(10+input$bins_2,12)) #Data frame de 3 colonnes chacune de 100 éléments
        #dfChi = stack(dfChi)  
        #ggplot(dfChi, aes(x = values)) + stat_density(aes(group = ind, color = ind),position="identity",geom="line")
    })
    
}
# Run the application 
shinyApp(ui = ui, server = server)




#sidebarLayout(
#   sidebarPanel(
#sliderInput("bins","Parameter",min = 1,max = 50,value = 30)
#  ),
# Show a plot of the generated distribution
# mainPanel(

#)
#)







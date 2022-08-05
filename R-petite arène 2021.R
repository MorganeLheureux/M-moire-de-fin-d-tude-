####################################################################################
### Analyse Petite Arène ###
####################################################################################



library(dplyr)
library(reshape2)
library(ggplot2)
library(smooth)
library(Mcomp)
library(forecast)
library(lme4)
library(glmmTMB)
library(car)

###################################Importation du jeux de données######################################

library(readxl)
Petite_2021 <- read_excel("C:/Users/lheur/Desktop/GOT0130/Petite_2021.xlsx", , 
                          col_types = c("text", "text", "text", 
                                        "text", "numeric", "numeric", "numeric", 
                                        "numeric", "numeric", "numeric"))
                              
Fil = Petite_2021

View(Fil)

#################### CONTENU DU JEU DE DONNEES #######################################################

# Le nom des varaibles contenues dans le jeu de données
names(Fil)

# La nature des variables
str(Fil)

# Afficher le jeu de données
Fil

##################### OBJECTIF DE L'ANALYSE STATISTIQUE ##############################################

# # Question  : Est- ce qu'en fonction de la période de la journée une même souche va être plus dispersive? 
#               Quelle va être la souche la plus dispersive ? 
# Variable à expliquer : le coefficient de dispersion
# Variables explicatives : 
#     - Facteur : la souche, la période de la journée, 
#     - Covariable : la température
#
#
######################Analyse exploratoire##################################
# A - Données aberrantes et distribution de la variable dépendante Y 

#Detection de valeurs aberrantes

par(mfrow=c(1,2))
# Boîte à moustaches
boxplot(Fil$D_Turchin, outline = FALSE
        ,col='red'
        ,ylab='Coefficient de diffusion')
# Cleveland plot - valeurs aberrantes
dotchart(Fil$D_Turchin
         ,pch=16
         ,col='red')

# Conclure : Il n'y a pas de valeur abérrante dans ce jeu de données 


##Distribution des valeurs de la variable à expliquer Y

#Histogramme
hist(Fil$D_Turchin
     ,breaks=8
     , col='red'
     , xlab='Coefficient de diffusion '
     , ylab='Effectifs')
#Visuellement l'histogramme ressemble à une poisson mais j'ai fait le modèle poisson et ça n'a pas été concluant
#Vu l'ordre de grandeur de D_Turchin, j'ai passer en log les données

hist(log(Fil$D_Turchin)
   ,breaks=8
     , col='red'
     , xlab='Coefficient de diffusion '
     , ylab='Effectifs')

# Ici on a bien ne représentation qui semble être gaussian 

# Quantile-Quantile Plot
qqnorm(log(Fil$D_Turchin)
       , col='red'
       , pch=16)
qqline(log(Fil$D_Turchin))

# Ces différentes représentations graphiques suggèrent que la variable à expliquer suit une loi normale

# B2 - Si X est un facteur : Nombre de modalités et nombre d'individus par modalité
Fil$Strain<-as.factor(Fil$Strain)
Fil$Time<-as.factor(Fil$Time)

summary(Fil$Strain)
summary(Fil$Time)

table(Fil$Strain)
table(Fil$Time)
#Ce tableau peut permettre l'identification d'une colinéarité, c'est croisé et équilibré donc pas colinéaire 


# C - Relation(s) entre Y et X

boxplot( Fil$D_Turchin~ Fil$Strain, outline = FALSE, las=2,
         ylab = "Coeffcient de diffusion",
         xlab = "Souche",
         main = "")
boxplot( Fil$D_Turchin~ Fil$Time, outline = FALSE,
         ylab = "Coefficient de diffusion",
         xlab = "Période de la jounée",
         main = "")
#effet possible des souches sur le coefficient de diffusion

# D - Interactions entre les X

boxplot(Fil$D_Turchin~Fil$Strain*Fil$Time,
        varwidth = TRUE,
        ylab = "Taille",
        main = "")
#Trop de variable pour que le graph soit lisible

# E - Colinearité entre les X, si plusieurs X avoir on doit faire attention à la colinéarité, 
Fil$Temperature<-as.numeric(Fil$Temperature)

par(mfrow=c(1,2))
boxplot(Fil$Temperature~Fil$Strain,
        col='red')
boxplot(Fil$Temperature~Fil$Time,
        col='red')

##################### MODELISATION STATISTIQUE ##################################################

# Modèle complet Gaussian avec l'ensemble des intéractions
mod1<-glm(log(Fil$D_Turchin)~Fil$Strain+Fil$Time+Fil$Temperature+Fil$Strain:Fil$Time+Fil$Strain:Fil$Temperature+Fil$Time:Fil$Temperature+Fil$Strain:Fil$Temperature:Fil$Time,family=gaussian(link=identity))
drop1(mod1,test='F')
summary(mod1)
# Modèle complet Gaussian 
mod2<-glm(log(Fil$D_Turchin)~Fil$Strain+Fil$Time+Fil$Temperature+Fil$Strain:Fil$Time+Fil$Strain:Fil$Temperature+Fil$Time:Fil$Temperature,family=gaussian(link=identity))
drop1(mod2,test='F')
summary(mod2)
# Comparaison de la vraisemblance des deux modèles
AIC(mod1,mod2)
#l'AIC le plus faible est plus le modèle 2 donc on conserve le modèle 2

#Nouveau modèle complet Gaussian  
mod3<-glm(log(Fil$D_Turchin)~Fil$Strain+Fil$Time+Fil$Temperature+Fil$Strain:Fil$Time+Fil$Strain:Fil$Temperature,family=gaussian(link=identity))
drop1(mod3,test='F')

# Comparaison de la vraisemblance des deux modèles
AIC(mod2,mod3)
#Le modèle 2 expliqe mieux le coeffcient de diffusion que le modèle 3
#Nouveau modèle complet Gaussian sans intéraction
mod4<-glm(log(Fil$D_Turchin)~Fil$Strain+Fil$Time+Fil$Temperature,family=gaussian(link=identity))
drop1(mod4,test='F')

# Comparaison de la vraisemblance des deux modèles
AIC(mod2,mod4)

#Le modèle 4 expliqe mieux le coeffcient de diffusion que le modèle 2
summary(mod4)


##################### VALIDATION DE L'ANALYSE ##################################################
# 1. Normalité des résidus
# 2. Homogeneité des variances
# 3. Individus influants

# 1. Normalité des résidus
par(mfrow=c(1,2))
hist (residuals(mod4)
      , col='red'
      , xlab='Valeurs des résidus'
      , ylab='Effectifs')
qqnorm(residuals(mod4)
       , col='red'
       ,pch=16)
qqline(residuals(mod4))

# C'est vérifié car l'historgamme et la courbe suivent  bien normal,

# 2. Homogeneité des variances
par(mfrow=c(1,2))
plot(residuals(mod4)~fitted(mod4)
     , col='red'
     , pch=16
     , xlab = "Fitted values",
     ylab = "Residuals", 
     main = "Homogeneity?")
abline(h = 0, v = 0, lty = 2)

plot( residuals(mod4)~ Fil$Strain
      , col='red'
      , pch=16
      , ylab = "Résidus"
      , xlab = "D_Turchin"
      , main = "")
abline(h = 0, lty = 2)

plot( residuals(mod4)~ log(Fil$D_Turchin)
      , col='red'
      , pch=16
      , ylab = "Résidus"
      , xlab = "D_Turchin"
      , main = "")
abline(h = 0, lty = 2)

#L'hypothèse est-elle  vérifiée ?

# 3. Individus influants

par(mfrow = c(1, 1))
plot(cooks.distance(mod4), type = "h", ylim = c(0, 1))
abline(h = 1, col = 2,lwd = 3)

#Une approche graphique plus immédiate
plot(mod4,
     pch=16,
     col="red")

#################Graphique###################
#Les souches par rapport au coef de diffusion
plot(Fil$D_Turchin~Fil$Strain,  outline = FALSE, las=2
     ,type='n'
     , xlab='Souches'
     , ylab='Coefficient de diffusion')
#Les souches par rapport à la vitesse moyenne 
plot(Fil$MeanSpeed~Fil$Strain,  outline = FALSE, las=2
     ,type='n'
     , xlab='Souches'
     , ylab='Vitesse moyenne')

#Les souches par rapport à la sinuosité moyenne 
plot(Fil$MeanSinuosity~Fil$Strain,  outline = FALSE, las=2
     ,type='n'
     , xlab='Souches'
     , ylab='Sinuosité moyenne')

#Les souches par rapport au taux d'activité
plot(Fil$Activity_Rate~Fil$Strain,  outline = FALSE, las=2
     ,type='n'
     , xlab='Souches'
     , ylab='Taux d activité')
######Autres pour montrer visuellement que pas d'effet########

#Pas d'effet de la période de la journée 
plot(Fil$D_Turchin~Fil$Time,  outline = FALSE, las=2
     ,type='n'
     , xlab='Période de la journée'
     , ylab='Coefficient de diffusion ')

plot(Fil$Activity_Rate~Fil$Time,  outline = FALSE, las=2
     ,type='n'
     , xlab='Période de la journée'
     , ylab='Taux dactivité ')

plot(Fil$MeanSpeed~Fil$Time,  outline = FALSE, las=2
     ,type='n'
     , xlab='Période de la journée'
     , ylab='Vitesse moyenne ')

plot(Fil$MeanSinuosity~Fil$Time,  outline = FALSE, las=2
     ,type='n'
     , xlab='Période de la journée'
     , ylab='sinuosité moyenne ')

#Pas d'effet de la température
plot(Fil$D_Turchin~Fil$Strain,  outline = FALSE, las=2
     ,type='n'
     , xlab='Souches'
     , ylab='Coefficient de diffusion')
points(Fil$D_Turchin[Fil$Temperature=="24"]~Fil$Strain[Fil$Temperature=="24"],col='aquamarine',pch=16)
points(Fil$D_Turchin[Fil$Temperature=="28"]~Fil$Strain[Fil$Temperature=="28"],col='blue',pch=16)

plot(Fil$MeanSpeed~Fil$Strain,  outline = FALSE, las=2
     ,type='n'
     , xlab='Souches'
     , ylab='Vitesse moyenne')
points(Fil$MeanSpeed[Fil$Temperature=="24"]~Fil$Strain[Fil$Temperature=="24"],col='aquamarine',pch=16)
points(Fil$MeanSpeed[Fil$Temperature=="28"]~Fil$Strain[Fil$Temperature=="28"],col='blue',pch=16)


plot(Fil$MeanSinuosity~Fil$Strain,  outline = FALSE, las=2
     ,type='n'
     , xlab='Souches'
     , ylab='Sinuosité moyenne')
points(Fil$MeanSinuosity[Fil$Temperature=="24"]~Fil$Strain[Fil$Temperature=="24"],col='aquamarine',pch=16)
points(Fil$MeanSinuosity[Fil$Temperature=="28"]~Fil$Strain[Fil$Temperature=="28"],col='blue',pch=16)

plot(Fil$Activity_Rate~Fil$Strain,  outline = FALSE, las=2
     ,type='n'
     , xlab='Souches'
     , ylab='Taux d activité')

points(Fil$Activity_Rate[Fil$Temperature=="24"]~Fil$Strain[Fil$Temperature=="24"],col='aquamarine',pch=16)
points(Fil$Activity_Rate[Fil$Temperature=="28"]~Fil$Strain[Fil$Temperature=="28"],col='blue',pch=16)


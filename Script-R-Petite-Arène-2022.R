####################################################################################
### Analyse Petite Ar?ne ###
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

###################################Importation du jeux de donn?es######################################

library(readxl)
Petite_2022 <- read_excel("C:/Users/lheur/Desktop/GOT0130/Petite_2022.xlsx", , 
                          col_types = c("text", "text", "text", 
                                        "numeric", "numeric", "numeric", "numeric", 
                                        "numeric", "numeric", "numeric","numeric"))
                              
Fil = Petite_2022

View(Fil)

#################### CONTENU DU JEU DE DONNEES #######################################################

# Le nom des varaibles contenues dans le jeu de donn?es
names(Fil)

# La nature des variables
str(Fil)

# Afficher le jeu de donn?es
Fil

##################### OBJECTIF DE L'ANALYSE STATISTIQUE ##############################################

# # Question  : Est- ce qu'en fonction de la p?riode de la journ?e une m?me souche va ?tre plus dispersive? 
#               Quelle va ?tre la souche la plus dispersive ? 
# Variable ? expliquer : le coefficient de dispersion
# Variables explicatives : 
#     - Facteur : la souche, la p?riode de la journ?e, 
#     - Covariable : la temp?rature
#
#
######################Analyse exploratoire##################################
# A - Donn?es aberrantes et distribution de la variable d?pendante Y 

#Detection de valeurs aberrantes

par(mfrow=c(1,2))
# Bo?te ? moustaches
boxplot(Fil$D_Turchin, outline = FALSE
        ,col='red'
        ,ylab='Coefficient de diffusion')
# Cleveland plot - valeurs aberrantes
dotchart(Fil$D_Turchin
         ,pch=16
         ,col='red')

# Conclure : Il n'y a pas de valeur ab?rrante dans ce jeu de donn?es 


##Distribution des valeurs de la variable ? expliquer Y

#Histogramme
hist(Fil$D_Turchin
     ,breaks=8
     , col='red'
     , xlab='Coefficient de diffusion '
     , ylab='Effectifs')
#Visuellement l'histogramme ressemble ? une poisson mais j'ai fait le mod?le poisson et ?a n'a pas ?t? concluant
#Vu l'ordre de grandeur de D_Turchin, j'ai passer en log les donn?es

hist(log(Fil$D_Turchin)
   ,breaks=8
     , col='red'
     , xlab='Coefficient de diffusion '
     , ylab='Effectifs')

# Ici on a bien ne repr?sentation qui semble ?tre gaussian 

# Quantile-Quantile Plot
qqnorm(log(Fil$D_Turchin)
       , col='red'
       , pch=16)
qqline(log(Fil$D_Turchin))

# Ces diff?rentes repr?sentations graphiques sugg?rent que la variable ? expliquer suit une loi normale

# B2 - Si X est un facteur : Nombre de modalit?s et nombre d'individus par modalit?
Fil$Strain<-as.factor(Fil$Strain)
Fil$Time<-as.factor(Fil$Time)

summary(Fil$Strain)
summary(Fil$Time)

table(Fil$Strain)
table(Fil$Time)
#Ce tableau peut permettre l'identification d'une colin?arit?, c'est crois? et ?quilibr? donc pas colin?aire 


# C - Relation(s) entre Y et X

boxplot( log(Fil$D_Turchin)~ Fil$Strain, outline = FALSE, las=2,
         ylab = "Coeffcient de diffusion",
         xlab = "Souche",
         main = "")
boxplot( log(Fil$D_Turchin)~ Fil$Time, outline = FALSE,
         ylab = "Coefficient de diffusion",
         xlab = "P?riode de la joun?e",
         main = "")
#effet possible des souches sur le coefficient de diffusion

# D - Interactions entre les X

boxplot(log(Fil$D_Turchin)~Fil$Strain*Fil$Time,
        varwidth = TRUE,
        ylab = "Taille",
        main = "")
#Trop de variable pour que le graph soit lisible

# E - Colinearit? entre les X, si plusieurs X avoir on doit faire attention ? la colin?arit?, 
Fil$Temperature<-as.numeric(Fil$Temperature)

par(mfrow=c(1,2))
boxplot(Fil$Strain~Fil$Time,
        col='red')


##################### MODELISATION STATISTIQUE ##################################################

# Mod?le complet Gaussian avec l'ensemble des int?ractions
mod1<-glm(log(Fil$D_Turchin)~Fil$Strain+Fil$Time+Fil$Strain:Fil$Time,family=gaussian(link=identity))
drop1(mod1,test='F')
summary(mod1)
# Mod?le complet Gaussian 
mod2<-glm(log(Fil$D_Turchin)~Fil$Strain+Fil$Time,family=gaussian(link=identity))
drop1(mod2,test='F')
summary(mod2)
# Comparaison de la vraisemblance des deux mod?les
AIC(mod1,mod2)
#l'AIC le plus faible est plus le mod?le 1 donc on conserve le mod?le 1

#Le mod?le 1 expliqe mieux le coeffcient de diffusion que le mod?le 2
summary(mod1)
################## Comparaison entre les souches #################################################

M <- aov(MeanSpeed~Strain, data= Fil)#First ANOVA model with every Strain2s
anova(M)

## Load this library so that you can run a tuckey test ##
library(agricolae)

HSD.test(M, "Strain")

print(HSD.test(M, "Strain"))

library(colorspace)

colors<- data.frame(type=Fil)

colors$col <- rainbow_hcl(length(colors$Fil))
Fil$D_Turchin<-as.numeric(Fil$D_Turchin)
par(mfrow=c(1,1))
boxplot(Fil$D_Turchin~Fil$Strain ,xlab="Souches",ylab="Coefficient de diffusion", col=colors$col,  outline = FALSE, las=2)



##################### VALIDATION DE L'ANALYSE ##################################################
# 1. Normalit? des r?sidus
# 2. Homogeneit? des variances
# 3. Individus influants

# 1. Normalit? des r?sidus
par(mfrow=c(1,2))
hist (residuals(mod1)
      , col='red'
      , xlab='Valeurs des r?sidus'
      , ylab='Effectifs')
qqnorm(residuals(mod1)
       , col='red'
       ,pch=16)
qqline(residuals(mod4))

# C'est v?rifi? car l'historgamme et la courbe suivent  bien normal,

# 2. Homogeneit? des variances
par(mfrow=c(1,2))
plot(residuals(mod1)~fitted(mod1)
     , col='red'
     , pch=16
     , xlab = "Fitted values",
     ylab = "Residuals", 
     main = "Homogeneity?")
abline(h = 0, v = 0, lty = 2)

plot( residuals(mod1)~ Fil$Strain
      , col='red'
      , pch=16
      , ylab = "R?sidus"
      , xlab = "D_Turchin"
      , main = "")
abline(h = 0, lty = 2)

plot( residuals(mod1)~ log(Fil$D_Turchin)
      , col='red'
      , pch=16
      , ylab = "R?sidus"
      , xlab = "D_Turchin"
      , main = "")
abline(h = 0, lty = 2)

#L'hypoth?se est-elle  v?rifi?e ?

# 3. Individus influants

par(mfrow = c(1, 1))
plot(cooks.distance(mod1), type = "h", ylim = c(0, 1))
abline(h = 1, col = 2,lwd = 3)

#Une approche graphique plus imm?diate
plot(mod1,
     pch=16,
     col="red")

#################Graphique###################
#Les souches par rapport au coef de diffusion
plot(log(Fil$D_Turchin)~Fil$Strain,  outline = FALSE, las=2
     ,type='n'
     , xlab='Souches'
     , ylab='Coefficient de diffusion')
#Les souches par rapport ? la vitesse moyenne 
plot(Fil$MeanSpeed~Fil$Strain,  outline = FALSE, las=2
     ,type='n'
     , xlab='Souches'
     , ylab='Vitesse moyenne')

#Les souches par rapport ? la sinuosit? moyenne 
plot(Fil$MeanSinuosity~Fil$Strain,  outline = FALSE, las=2
     ,type='n'
     , xlab='Souches'
     , ylab='Sinuosit? moyenne')

#Les souches par rapport au taux d'activit?
plot(Fil$Activity_Rate~Fil$Strain,  outline = FALSE, las=2
     ,type='n'
     , xlab='Souches'
     , ylab='Taux d activit?')
######Autres pour montrer visuellement que pas d'effet########

#Pas d'effet de la p?riode de la journ?e 
plot(Fil$D_Turchin~Fil$Time,  outline = FALSE, las=2
     ,type='n'
     , xlab='P?riode de la journ?e'
     , ylab='Coefficient de diffusion ')

plot(Fil$Activity_Rate~Fil$Time,  outline = FALSE, las=2
     ,type='n'
     , xlab='P?riode de la journ?e'
     , ylab='Taux dactivit? ')

plot(Fil$MeanSpeed~Fil$Time,  outline = FALSE, las=2
     ,type='n'
     , xlab='P?riode de la journ?e'
     , ylab='Vitesse moyenne ')

plot(Fil$MeanSinuosity~Fil$Time,  outline = FALSE, las=2
     ,type='n'
     , xlab='P?riode de la journ?e'
     , ylab='sinuosit? moyenne ')

#Pas d'effet du r?plicat
plot(Fil$D_Turchin~Fil$Strain,  outline = FALSE, las=2
     ,type='n'
     , xlab='Souches'
     , ylab='Coefficient de diffusion')
points(Fil$D_Turchin[Fil$N_Rep=="1"]~Fil$Strain[Fil$N_Rep=="1"],col='aquamarine',pch=16)
points(Fil$D_Turchin[Fil$N_Rep=="2"]~Fil$Strain[Fil$N_Rep=="2"],col='blue',pch=16)

plot(Fil$MeanSpeed~Fil$Strain,  outline = FALSE, las=2
     ,type='n'
     , xlab='Souches'
     , ylab='Vitesse moyenne')
points(Fil$MeanSpeed[Fil$N_Rep=="1"]~Fil$Strain[Fil$N_Rep=="1"],col='aquamarine',pch=16)
points(Fil$MeanSpeed[Fil$N_Rep=="2"]~Fil$Strain[Fil$N_Rep=="2"],col='blue',pch=16)


plot(Fil$MeanSinuosity~Fil$Strain,  outline = FALSE, las=2
     ,type='n'
     , xlab='Souches'
     , ylab='Sinuosit? moyenne')
points(Fil$MeanSinuosity[Fil$N_Rep=="1"]~Fil$Strain[Fil$N_Rep=="1"],col='aquamarine',pch=16)
points(Fil$MeanSinuosity[Fil$N_Rep=="2"]~Fil$Strain[Fil$N_Rep=="2"],col='blue',pch=16)

plot(Fil$Activity_Rate~Fil$Strain,  outline = FALSE, las=2
     ,type='n'
     , xlab='Souches'
     , ylab='Taux d activit?')

points(Fil$Activity_Rate[Fil$N_Rep=="1"]~Fil$Strain[Fil$N_Rep=="1"],col='aquamarine',pch=16)
points(Fil$Activity_Rate[Fil$N_Rep=="2"]~Fil$Strain[Fil$N_Rep=="2"],col='blue',pch=16)


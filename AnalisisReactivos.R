#Análisis de reactivos usando TCT
#Proyecto final de Taller de Investigación 1: Seminario de Medición y Evaluación en Educación a cargo de Leydy Aleen Erazo y Ramsés Vásquez Lira
#Código creado por Scarlett Escudero

#Paquetes requeridos
library(mirt) 
library(sjPlot)
library(CTT)
library(ShinyItemAnalysis)
library(itemanalysis)
library(psychometric) 
library(psych) 
library(tidyverse)

#Carga de datos
data_ACA <- read.csv("C:/Users/scarl/Documents/UNAM/QuintoSemestre/Taller_Invest/AnalisisDatos/ACA_III.csv")
data_NyA <- read.csv("C:/Users/scarl/Documents/UNAM/QuintoSemestre/Taller_Invest/AnalisisDatos/NyA.csv")

data_ACA <- data_ACA[,- c(1,2,3,4,5)]
ACA <- data_ACA[,- c(1,3,5,7,9,11,13,15,17,19)]; View(ACA)
Clave_ACA <- c("A","B","B","C","D","B","D","B","C","C")

data_NyA <- data_NyA[-50,- c(1,2,3,4,5)]
NyA <- data_NyA[,- c(1,3,5,7,9,11,13,15,17,19)]; View(NyA)
Clave_NyA <- c("A","B","D","A","B","B","C","C","B","D")

#Análisis de ACA III
iteman_ACA <- sjt.itemanalysis(ACA); iteman_ACA
D33_ACA <- as.data.frame(gDiscrim(ACA, k = 3, l = 1, u = 3)); D33_ACA
Discrim_ACA <- as.data.frame(discrim(ACA)); Discrim_ACA
alpha(ACA)

DDplot(ACA)

DistractorAnalysis(data_ACA[,- c(2,4,6,8,10,12,14,16,18,20)], Clave_ACA)

plotDistractorAnalysis(data_ACA[,- c(2,4,6,8,10,12,14,16,18,20)], Clave_ACA, num.groups = 3, item = 1, multiple.answers = F)
plotDistractorAnalysis(data_ACA[,- c(2,4,6,8,10,12,14,16,18,20)], Clave_ACA, num.groups = 3, item = 2, multiple.answers = F)
plotDistractorAnalysis(data_ACA[,- c(2,4,6,8,10,12,14,16,18,20)], Clave_ACA, num.groups = 3, item = 3, multiple.answers = F)
plotDistractorAnalysis(data_ACA[,- c(2,4,6,8,10,12,14,16,18,20)], Clave_ACA, num.groups = 3, item = 4, multiple.answers = F)
plotDistractorAnalysis(data_ACA[,- c(2,4,6,8,10,12,14,16,18,20)], Clave_ACA, num.groups = 3, item = 5, multiple.answers = F)
plotDistractorAnalysis(data_ACA[,- c(2,4,6,8,10,12,14,16,18,20)], Clave_ACA, num.groups = 3, item = 6, multiple.answers = F)
plotDistractorAnalysis(data_ACA[,- c(2,4,6,8,10,12,14,16,18,20)], Clave_ACA, num.groups = 3, item = 7, multiple.answers = F)
plotDistractorAnalysis(data_ACA[,- c(2,4,6,8,10,12,14,16,18,20)], Clave_ACA, num.groups = 3, item = 8, multiple.answers = F)
plotDistractorAnalysis(data_ACA[,- c(2,4,6,8,10,12,14,16,18,20)], Clave_ACA, num.groups = 3, item = 9, multiple.answers = F)
plotDistractorAnalysis(data_ACA[,- c(2,4,6,8,10,12,14,16,18,20)], Clave_ACA, num.groups = 3, item = 10, multiple.answers = F)

#Análisis de Neurobiología y Adaptación
iteman_NyA <- sjt.itemanalysis(NyA); iteman_NyA
D33_NyA <- as.data.frame(gDiscrim(NyA, k = 3, l = 1, u = 3)); D33_NyA 
Discrim_NyA <- as.data.frame(discrim(NyA)); Discrim_NyA
alpha(NyA)

DDplot(NyA)

DistractorAnalysis(data_NyA[,- c(2,4,6,8,10,12,14,16,18,20)], Clave_NyA)

plotDistractorAnalysis(data_NyA[,- c(2,4,6,8,10,12,14,16,18,20)], Clave_NyA, num.groups = 3, item = 1, multiple.answers = F)
plotDistractorAnalysis(data_NyA[,- c(2,4,6,8,10,12,14,16,18,20)], Clave_NyA, num.groups = 3, item = 2, multiple.answers = F)
plotDistractorAnalysis(data_NyA[,- c(2,4,6,8,10,12,14,16,18,20)], Clave_NyA, num.groups = 3, item = 3, multiple.answers = F)
plotDistractorAnalysis(data_NyA[,- c(2,4,6,8,10,12,14,16,18,20)], Clave_NyA, num.groups = 3, item = 4, multiple.answers = F)
plotDistractorAnalysis(data_NyA[,- c(2,4,6,8,10,12,14,16,18,20)], Clave_NyA, num.groups = 3, item = 5, multiple.answers = F)
plotDistractorAnalysis(data_NyA[,- c(2,4,6,8,10,12,14,16,18,20)], Clave_NyA, num.groups = 3, item = 6, multiple.answers = F)
plotDistractorAnalysis(data_NyA[,- c(2,4,6,8,10,12,14,16,18,20)], Clave_NyA, num.groups = 3, item = 7, multiple.answers = F)
plotDistractorAnalysis(data_NyA[,- c(2,4,6,8,10,12,14,16,18,20)], Clave_NyA, num.groups = 3, item = 8, multiple.answers = F)
plotDistractorAnalysis(data_NyA[,- c(2,4,6,8,10,12,14,16,18,20)], Clave_NyA, num.groups = 3, item = 9, multiple.answers = F)
plotDistractorAnalysis(data_NyA[,- c(2,4,6,8,10,12,14,16,18,20)], Clave_NyA, num.groups = 3, item = 10, multiple.answers = F)

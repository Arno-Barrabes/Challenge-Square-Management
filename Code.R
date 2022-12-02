rm(list=ls(all=TRUE))
library(readr)

#https://www.fao.org/faostat/fr/#data/QCL
#france - superperficie/production - raisins (dans cultures primaires)  - toutes les années
Data1 <- read_delim("Data1.csv", delim = ";", escape_double = FALSE, trim_ws = TRUE)
Data1 <- Data1[,c(6,10:12)]
Data_superficie_récoltée <- Data1[1:60,]
Data_production <- Data1[61:120,]

#https://www.fao.org/faostat/fr/#data/QCL
#france - rendements - raisins (dans cultures primaires)  - toutes les années
Data_rendement <- read_delim("Data2.csv", delim = ";", escape_double = FALSE, trim_ws = TRUE)
Data_rendement <- Data_rendement[,c(6,10:12)]

#https://www.fao.org/faostat/fr/#data/PP
#france - Prix à la production (SLC/tonne et USD/tonne) - Raisins - toutes les années
Data3 <- read_delim("Data3.csv", delim = ";", escape_double = FALSE, trim_ws = TRUE)
Data3 <- Data3[,c(6,10,13,14)]
Data_prix_prod_SLC <- Data3[1:30,]
Data_prix_prod_USD <- Data3[31:60,]

#https://www.fao.org/faostat/fr/#data/QV
#france - produits brut (mile MC et US) - raisins - toutes les années
Data4 <- read_delim("Data4.csv", delim = ";", escape_double = FALSE, trim_ws = TRUE)
Data4 <- Data4[,c(6,10:12)]
Data_valeur_prod_SLC <- Data4[1:30,]
Data_valeur_prod_USD <- Data4[31:60,]

rm(Data1,Data3,Data4)

Data_60 <- merge(Data_production, Data_superficie_récoltée, by = "Année")
Data_60 <- merge(Data_60, Data_rendement, by = "Année")

Data_30 <- merge(Data_prix_prod_USD, Data_prix_prod_SLC, by="Année")
Data_30 <- merge(Data_30, Data_valeur_prod_USD, by="Année")
Data_30 <- merge(Data_30, Data_valeur_prod_SLC, by="Année")

#normalisation des données selon la norme min/max.

min_60_a <- min(Data_60[,4])
max_60_a <- max(Data_60[,4])
min_60_b <- min(Data_60[,7])
max_60_b <- max(Data_60[,7])
min_60_c <- min(Data_60[,10])
max_60_c <- max(Data_60[,10])

min_30_a <- min(Data_30[,4])
max_30_a <- max(Data_30[,4])
min_30_b <- min(Data_30[,7])
max_30_b <- max(Data_30[,7])
min_30_c <- min(Data_30[,10])
max_30_c <- max(Data_30[,10])
min_30_d <- min(Data_30[,13])
max_30_d <- max(Data_30[,13])

norm_mM <- function(x,min,max){
  return ((x-min)/(max-min))
}

for(i in 1:60){
  Data_60[i,4] <- norm_mM(Data_60[i,4],min_60_a,max_60_a)
  Data_60[i,7] <- norm_mM(Data_60[i,7],min_60_b,max_60_b)
  Data_60[i,10] <- norm_mM(Data_60[i,10],min_60_c,max_60_c)
}

for(i in 1:30){
  Data_30[i,4] <- norm_mM(Data_30[i,4],min_30_a,max_30_a)
  Data_30[i,7] <- norm_mM(Data_30[i,7],min_30_b,max_30_b)
  Data_30[i,10] <- norm_mM(Data_30[i,10],min_30_c,max_30_c)
  Data_30[i,13] <- norm_mM(Data_30[i,13],min_30_d,max_30_d)
}

rm(min_30_a,max_30_a,min_30_b,max_30_b,min_30_c,max_30_c,min_30_d,max_30_d,min_60_a,max_60_a,min_60_b,max_60_b,min_60_c,max_60_c)


#plot(Data_production$Année,Data_production$Valeur,type = 'l',ylab="Production de raisins (en tonnes)", xlab="Années",col=2)
#plot(Data_superficie_récoltée$Année,Data_superficie_récoltée$Valeur,type = 'l',ylab="Superficie récoltée (en ha)", xlab="Années",col=2)
#plot(Data_rendement$Année,Data_rendement$Valeur,type = 'l',ylab="Rendement (en hg/ha)", xlab="Années",col=2)
#
#plot(Data_60$Année,Data_60$Valeur.x,type = 'l', col="blue", pch="o", lty=1, ylab="y" )
#points(Data_60$Année,Data_60$Valeur.y, col="red", type = 'l')
#points(Data_60$Année,Data_60$Valeur, col="purple",type = 'l')
#legend(1,100,legend=c("y1","y2","y3"), col=c("blue","red","darkgreen"),lty=c(1,2,3), ncol=1)
#
#
#
#plot(Data_production$Année,log(Data_production$Valeur),type = 'l',ylab="Production (en tonnes)", xlab="Années",col=2)
#plot(Data_superficie_récoltée$Année,log(Data_superficie_récoltée$Valeur),type = 'l',ylab="Superficie récoltée (en ha)", xlab="Années",col=2)
#plot(Data_rendement$Année,log(Data_rendement$Valeur),type = 'l',ylab="Rendement (en hg/ha)", xlab="Années",col=2)
#
#plot(Data_production$Année,Data_production$Valeur,type='l',col=2,axes=F,ylab="", xlab="") 
#axis(1,at=seq(15.4,16.4,by=0.2))
#axis(1,at=seq(1961,2021,by=10))
#par(new=T)
#plot(Data_superficie_récoltée$Année,Data_superficie_récoltée$Valeur, col=1,type='l',axes = F,ylab="", xlab="")
#axis(4,at=seq(13.5,14.2,by=0.1))
#par(new=T)
#plot(Data_rendement$Année,Data_rendement$Valeur, col=3,type='l',axes = F,ylab="", xlab="")
#legend("bottomleft", c("Production","Superficie récoltée","Rendement"),col = c(2, 1,3),lty=c(1,1))



  


#comment gérer le nombre d'observations différents ?

#produire graphique à partir d'une année charnière et faire base 100
#année charnière choisie 

Data_30_base <- Data_30
colnames(Data_30_base) <- c("Année", "Élem1", "Unit1", "Val1", "Élem2", "Unit2", "Val2", "Élem3", "Unit3", "Val3", "Élem4", "Unit4", "Val4")

for (i in 1:30){
  Data_30_base[i,4] <- Data_30[i,4]/Data_30[1,4] 
  Data_30_base[i,7] <- Data_30[i,7]/Data_30[1,7]
  Data_30_base[i,10] <- Data_30[i,10]/Data_30[1,10]
  Data_30_base[i,13] <- Data_30[i,13]/Data_30[1,13]
}

Data_60_base <- Data_60

for (i in 1:60){
  Data_60_base[i,4] <- Data_60[i,4]/Data_60[31,4] 
  Data_60_base[i,7] <- Data_60[i,7]/Data_60[31,7]
  Data_60_base[i,10] <- Data_60[i,10]/Data_60[31,10]
}


# graphiques avec année de référence : 1991

plot(Data_60_base$Année,Data_60_base$Valeur.x,main="Évolution de la production de raisin de 1961 à 2020",xlab = "Année",ylab = "Production (base 1991)",type = 'l', col="blue", pch="o",font.main=2)
abline(h = 1, col="red", lty=2)

plot(Data_60_base$Année,Data_60_base$Valeur.y,main="Évolution de la superficie récoltée de 1961 à 2020",xlab = "Année",ylab = "Superficie récoltée (base 1991)",type = 'l', col="blue", pch="o",font.main=2)
abline(h = 1, col="red", lty=2)

plot(Data_60_base$Année,Data_60_base$Valeur,main="Évolution du rendement de 1961 à 2020",xlab = "Année",ylab = "Rendement (base 1991)",type = 'l', col="blue", pch="o",font.main=2)
abline(h = 1, col="red", lty=2)

plot(Data_30_base$Année,Data_30_base$Val1,main="Évolution du prix à la production de 1991 à 2020",xlab = "Année",ylab = "Prix (base 1991)",type = 'l', col="blue", pch="o",font.main=2)
abline(h = 1, col="red", lty=2)

plot(Data_30_base$Année,Data_30_base$Val3,main="sous-titre",xlab = "Année",ylab = "Base 1991",type = 'l', col="blue", pch="o",font.main=2)
abline(h = 1, col="red", lty=2)



# trouver des explications aux évoltuions de la production, rendement et superficie. le climat la dedans ? ou autre raisons, politiques ou agricoles
# SLC : solice, crpyto monnaie pas intéressante

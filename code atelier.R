
##############################################################################################
##
## Code R pour l'atelier en biostatistique de l'école d'été de l'UdeM en mathématiques 
## 17 Juin 2022
## Préparé par Prof. Janie Coulombe
##
##############################################################################################


rm(list=ls(all=TRUE)) ## pour supprimer toute variable ou jeu de données préalablement créés


############################################################################################

#############################################
## Partie 1: Paradoxe de simpson/confusion ##
#############################################


set.seed(2455)            ## J'ai mis un germe pour que l'on obtienne tous les mêmes résultats
age_pays1<- ceiling( rnorm( n=1500000, mean=45, sd= 15)) ## Simulation âge
age_pays1[age_pays1<1]<-1 ## Modifier pour avoir une distribution raisonnable (enlever âges <1)
boxplot(age_pays1)        ## Boîte à moustache âge pays 1

age_pays2<- ceiling( rnorm( n=1500000, mean=60, sd= 20)) ## Âge pays 2
age_pays2[age_pays2<1]<-1 ## Modifier pour avoir une distribution raisonnable (enlever âges <1)
age_pays2[age_pays2>119]<-119 ## Modifier pour avoir une distribution raisonnable (enlever âges >119)
boxplot(age_pays2)
 


boxplot(age_pays1, age_pays2, xlab='Pays', ylab='Age')

pays<- c(rep(1,1500000), rep(2,1500000))
age<- c(age_pays1, age_pays2)
gg<- exp( -6.5 + 0.04*age)/(1+exp(-6.5 + 0.04*age))
mortalite<- rbinom( n=3000000, size=1, prob= gg)
table(mortalite, pays)
 

# taux marginal: 

16342/(16342+1483658)*100
33603/(33603+1466397)*100
 
# par groupe age 

age_group<- ifelse( age< 5, 1, 
             ifelse( age< 10, 2, 
             ifelse( age< 15, 3,
             ifelse( age<20, 4,
             ifelse(age<25, 5,
             ifelse(age<30, 6,
             ifelse(age<35, 7,             
		 ifelse(age<40, 8,
             ifelse(age<45, 9,
             ifelse(age<50, 10,
             ifelse(age<55, 11,
             ifelse(age<60, 12,
             ifelse(age<65, 13,
             ifelse(age<70, 14,
             ifelse(age<75, 15,
             ifelse(age<80, 16,
             ifelse(age<85, 17,
             ifelse(age<90, 18,
             ifelse(age<95, 19,
             ifelse(age<100, 20, 21 ))))))))))))))))))) )

table(age_group, pays)

dat<- data.frame( mortalite, pays, age_group)
colnames(dat)

for(i in 1:21){ 

 dat1<- dat[dat$age_group ==i,]

 print(c('Tranche Age',i))

 print('pays 1 taux')
 print( table( dat1$mortalite, dat1$pays)[2]/ ( table( dat1$mortalite, dat1$pays)[2] + table( dat1$mortalite, dat1$pays)[1])*100    )

 print('pays 2 taux')
 print( table( dat1$mortalite, dat1$pays)[4]/ ( table( dat1$mortalite, dat1$pays)[4] + table( dat1$mortalite, dat1$pays)[3])*100    )


 }
 
boxplot( age_group ~ mortalite, xlab='MORTALITÉ', ylab="Groupe d'âge (1 à 21)"  )

############################################################################################

#####################################
## Partie 2: Paradoxe de l'amitié  ##
#####################################

## install.packages('networkD3') # sur votre ordi maison: télécharger librairie nécessaire
library(networkD3)

dataset <- cbind(  ## la fonction cbind colle les colonnes ensemble une à côté de l'autre 
  ## Personne 1 de chaque paire
  c( rep( 'Janie', 8) , 
     rep( 'Nicolas', 6), 
     rep( 'Dominic' , 3 ),
     rep( 'Danielle' , 2 ), ## premieres personnes dans chaque paire
     rep('Louis', 1)        
     ) , 
  
  ## Personne 2 de chaque paire
  c( 'Nicolas', 'Danielle','Louis', 'Renald' , 'Linda', 'Jean-Philippe', 'Dominic', 'Morgane',  
       'Danielle','Louis',  'Linda', 'Jean-Philippe', 'Dominic', 'Morgane',
      'Linda', 'Jean-Philippe', 'Morgane',
       'Louis','Renald', 
       'Renald') , ## deuxieme personne de la paire
  
  ## Poids de chaque relation
  c( 0.95, 0.8, 0.4, 0.5, 0.2, 0.1, 0.1, 0.05,
       0.5,  0.1, 0.8, 0.9, 0.85, 0.4, 
       0.75, 0.8, 0.95,
       0.85, 0.65,  
        0.4  
     )  
  )

colnames(dataset)<- c('Paire_1', 'Paire_2' ,'Poids')   ## assigner des noms à chaque variable dans le jeu de données
dataset2<- data.frame(dataset) ## mettre en data.frame, sinon la fonction simpleNetwork ne fonctionne pas
head(dataset2)

simpleNetwork(dataset2[,1:2])                                  ## voir le réseau en ligne sur le site web ou encore sur Rstudio
M=(rbind(as.matrix(dataset2[,1:2]),as.matrix(dataset2[,2:1]))) ## toutes les paires possibles
nodes=unique(M[,1])                                            ## garder les paires différentes

## considérer toutes les paires dans M
M=(rbind(as.matrix(dataset2[,1:2]),as.matrix(dataset2[,2:1])))

## Garder les paires distinctes
nodes=unique(M[,1])                

## Fonction friends trouve toutes les paires contenant une personne donnée
friends = function(x) as.character(M[which(M[,1]==x),2])

## Fonction nb_friends compte le nombre d'amis de 
## chaque ami d'une personne, e.g. nb_friends(friends('Janie')) montre
## le nombre d'amis de chaque ami de Janie

nb_friends = Vectorize(function(x) length(friends(x)))

## Fonction friends_of_friends et nb_friends_of_friends permettent de calculer nombre d'amis des amis

friends_of_friends = function(y) (Vectorize(function(x) length(friends(x)))(friends(y)))
nb_friends_of_friends = Vectorize(function(x) mean(friends_of_friends(x)))

## Ex. avec Janie:

nb_friends('Janie')
friends_of_friends('Janie')
 nb_friends_of_friends('Janie')

## Pour le graphe complet:

Nb  = nb_friends(nodes)
Nb2 = nb_friends_of_friends(nodes)  ## ci-dessous: histogramme du nombre de contacts pour chaque personne ainsi que pour les amis
hist(Nb,breaks=0:40,col=rgb(1,0,0,.2),border="white",probability = TRUE, xlim=c(0,20), ylab='Proportion', xlab='Nombre', main='Rose: Chaque individu. Bleu: Les amis de chaque individu')
hist(Nb2,breaks=0:40,col=rgb(0,0,1,.2),border="white",probability = TRUE,add=TRUE)
lines(density(Nb),col="red",lwd=2)
lines(density(Nb2),col="blue",lwd=2)

mean(Nb) ## nombre moyen de contacts pour chaque membre du réseau
mean(Nb2) ## nombre moyen de contacts pour chaque ami du réseau

############################################################################################

###################################
## Partie 3: Paradoxe de Berkson ##
###################################

set.seed(416667818) ## j'ai mis un germe car sinon nous 
                    ## n'obtiendrons pas tous la même réponse ici - vous pouvez le changer éventuellement si vous le désirez
                    ## germe de l'atelier de JC: 416667818

taille<- 100000 ## taille d'échantillon
HA1P6N<- rbinom (n=taille, size=1, prob= 0.05 ) ## simuler la variable HA1P6N

expit<- function(h){exp(h)/(1+exp(h))}
prob_covid<- expit( -5 + 0.05*HA1P6N) 
covid19<- rbinom( n=taille, size=1, prob= prob_covid) ## simuler la variable COVID19 selon bernoulli
table(covid19) ## voir le nombre de covid19 dans l'échantillon

prob_hospit<- expit( -5  +  1.5*HA1P6N + 0.05*covid19)
hospit<- rbinom( n=taille, size=1, prob=prob_hospit) ## simuler la variable hospitalisation 
table(hospit) ## voir le nombre d'hospitalisations dans l'échantillon

## voir la distribution de covid19 à travers ceux qui ont HA1P6N et ne l'ont pas, chez les hospitalisés seulement
table( covid19[hospit==1], HA1P6N[hospit==1])
 
## calcul du taux (Tableau dans les diapositives powerpoint)
print ((table( covid19[hospit==1], HA1P6N[hospit==1])[4]  / (table( covid19[hospit==1], HA1P6N[hospit==1])[4] + table( covid19[hospit==1], HA1P6N[hospit==1])[3]))/
(table( covid19[hospit==1], HA1P6N[hospit==1])[2]  / (table( covid19[hospit==1], HA1P6N[hospit==1])[2] + table( covid19[hospit==1], HA1P6N[hospit==1])[1])) )

## voir la distribution de covid19 à travers ceux qui ont HA1P6N et ne l'ont pas, chez la population globale
table( covid19, HA1P6N)
 
## calcul du taux dans la population entière
print( (table( covid19, HA1P6N)[4]  / (table( covid19, HA1P6N)[4] + table( covid19, HA1P6N)[3]))/
( table( covid19, HA1P6N)[2]  / (table( covid19, HA1P6N)[2] + table( covid19, HA1P6N)[1])) )



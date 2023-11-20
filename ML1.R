##### TUTTE LE LIBRERIE #####

library(GGally)
library(tidyverse)
library(ggplot2)
library(rgl)
library(mvtnorm)
library(cluster)
library(ggfortify)
library(MASS)
library(PRROC)
library(class)
library(tree)
library(ISLR)
library(randomForest)
library(pls)
library(glmnet)
library(boot)

# ------------------------------------------------------------------------------ Comandi base #####

# Comandi base

##### Ripulire tutto ####
rm(list = ls())
graphics.off()
cat("\014") # ripulire la console

##### Leggere e modificare DATABASE #####

db = read.csv("nome_file.csv", sep = "\t", header = TRUE)

db = read.table # in cas di file.txt

db$nome_colonna = NULL # Eliminare una colonna

##### Calcolare indici di POSIZIONE e DISPERSIONE #####

# *** INDICI POSIZIONE ***
min(dati$Colesterolo) 
max(dati$Colesterolo)
range(dati$Colesterolo)
mean(dati$Colesterolo)
median(dati$Colesterolo)
quantile(dati$Colesterolo, c(0.25, 0.75)) # quartili a 0.25 e 0.75

summary(dati$Colesterolo) # per visualizzarli tutti

# *** INDICI DISPERSIONE ***
var(dati$Colesterolo) # varianza campionaria
sd(dati$Colesterolo) # standard deviation
IQR(dati$Colesterolo) # Range interquartile

##### Attivare device grafico e ingrandire lo spazio per più grafici #####
quartz()
par(mfrow = c(1,2)) # sara' un device 1x2, quindi c'e' spazio per 2 grafici.

##### ISTOGRAMMA #####
hist(dati$Colesterolo, main = "Istogramma del Colesterolo", xlab = 'Valore colesterolo', ylab = 'Frequenza', col = "orange")

# Se volessi mettere la probabilità sull'asse y devo aggiungere: hist(....., prob = TRUE, ....)

# Se volessi plottare la media/mediana/moda... sull'istogramma:
abline(v= median(dati$Colesterolo), col = 'red')
abline(v= mean(dati$Colesterolo), col = 'green')

##### BOXPLOT #####
boxplot(dati$Colesterolo, main = 'Boxplot del Colesterolo' , col = "forestgreen")

##### Selezionare righe secondo una condizione #####
dati_M = dati[dati$Sesso == "M", ]
dati_F = dati[dati$Sesso == "F", ]
# (secondo più condizioni --> Ddevo scerivere nell'argomento: cond1 & cond2)

##### FREQUENZE ASSOLUTE e RELATIVE #####
dati$Sesso = factor(dati$Sesso) # Così posso contarli perchè prima sono una stringa
f_ass = table(dati$Sesso) 
f_ass

f_rel = f_ass/length(dati$Sesso) #l f_relative è uguale alla frequenza assoluta / la lunghezza dei dati
f_rel

##### BARPLOT #####
barplot(f_ass, col = c( 'pink', 'blue' ), ylab = 'Freq. assolute')
barplot(f_rel, col = c( 'pink', 'blue' ), ylab = 'Freq. relative')

##### GRAFICO A TORTA #####
pie(f_ass, col = c( 'pink', 'blue' ))


##### RENDERE COUNTABLE UNA VARIABILE #####
db$variabile = factor(db$variabile)

##### PARTE A TEST #####

# OCCHIO A COSA E' X E COSA E' Y (di solito c'è un esercizio dove x = prima e y = dopo)

nx = 
ux = 
sd2x = 

ny = 
uy = 
sd2y = 

s2pooled = ((nx-1)*sd2x + (ny-1)*sd2y)/(nx+ny-2)
t = (ux-uy)/sqrt(s2pooled*((1/nx)+(1/ny))) # statistica test (CONSIDERARLO IN MODULO)
pval = (1 - pt(abs(t), nx+ny-2))
pval

# TEST UNILATERO --> pval = 2*(1 - pt(abs(t), nx+ny-2))

# se p-val > livello significatività --> ACCETTO HO
# se p-val < livello significtività --> RIFIUTO HO


# ------------------------------------------------------------------------------ LAB 2 #####

#  DISRIBUZIONE                 RIPARTIZIONE  QUANTILE    DENSITA'    
#  Normal	                      pnorm	        qnorm	      dnorm	               
#  Student t	                  pt	          qt	        dt	    

##### Funzione NORMALE #####

mu = 2 # media
sigma2 = 6 # varianza
sigma = sqrt(sigma2) # per farla riconoscere ad R

# Calcolo la DENSITA' in un VETTORE EQUISPAZIATO su mu +- 4sigma, con 2000 elementi
t = seq(mu - 4*sigma, mu + 4*sigma, length.out = 2000) # vettore equispaziato
t
f = dnorm(t, mean = mu, sd = sigma)                    # densità

# Calcolo la FUNZIONE DI RIPARTIZIONE
P = pnorm(t, mean = mu, sd = sigma)
P

# Se voglio i QUANTILI? 
# Mi ricordo che la funzione quantile ha per dominio l'intervallo [0,1], qualunque sia la distribuzione
q = seq(1e-6,1-1e-6,length.out = 1000) # Non metto 0 e 1 per ragioni di stabilità numerica
Q = qnorm(q, mean = mu, sd = sigma)

# Plotto tutto insieme
quartz()
par (mfrow = c(1,3))
plot(t,f, col = "red", type = "l", lwd = 4, main = "Densità")
abline(v = mu, col = "black", lwd = 2)
plot(t,P, col = "forestgreen", type = "l", lwd = 2, main = "Ripartizione")
plot(q,Q, col = "blue", type = "l", lwd = 2, main = "Quantile")

##### Funzione T-STUDENT #####

gradi = 8 # GRADI DI LIBERTA' che voglio
t = seq(-6, 6, length.out = 2000) # Creo il vettore
f = dt(t, df = gradi) # densità

P = pt(t, df = gradi) # funzione di RIPARTIZIONE

q = seq(1e-6,1-1e-6,length.out = 1000)
Q = qt(q, df = gradi) # QUANTILI

# Plotto tutto insieme
quartz()
par (mfrow = c(1,3)) #Grafico ad una riga, 3 colonne
plot(t,f, col = "red", type = "l", lwd = 2, main = "Densità")
plot(t,P, col = "forestgreen", type = "l", lwd = 2, main = "Ripartizione")
plot(q,Q, col = "blue", type = "l", lwd = 2, main = "Quantile")

##### Confronto due medie #####

tapply(dati$peso, dati$sesso, mean) # confronto le medie del peso dei M e delle F

##### ESPLORAZIONE GRAFICA DATI MULTIVARIATI #####

quartz()
plot(dati) # Soluzione grafica standard già vista

# Altre librerie grafice

library(GGally)
ggpairs(dati, aes(alpha = 0.4, color = Sesso))

# Se la domanda ci chiede quale variabile utilizzare per l'ANOVA --> fare le prove delle variabili con ggpairs e vedere quale presenta più differenze --> quindi scegliere quella

# Se per esempio nella funzione ggpairs volessi solamente usare alcune variabili del dataset, all'interno della funzione
# ggpairs devo evidenziare il vettore delle variabili che voglio usare:
# Esempio: volio usare solamente le variabili 1, 3 e 7 del dataset chiamato "dati": ggpairs(dati[,c(1,3,7)])

library(rgl) # 3D
colors = as.numeric(dati$Sesso)
colors
plot3d(dati$Peso, dati$Colesterolo, dati$Pressione, col = colors)

##### VERIFICA NORMALITA' DATI #####

# Ci troviamo spesso nella necessità di verificare se un campione può essere assunto (approssimativamente) gaussiano.

# Esempio dati colesterolo
col = dati$Colesterolo
mu_hat = mean(col) # media
sd_hat = sd(col) # standard deviation

# Ho 3 STRUMENTI:

# STRUMENTO 1 (grafico)

# Visualizziamo l'istogramma e sovrapponiamo la densità normale (con stessa media e sd)
t = seq(min(col), max(col), length.out = 1000)
f = dnorm(t, mean = mu_hat, sd = sd_hat) # densità normale a cui dò in input t, media e sd
quartz()
hist(col, breaks = 15, col = "gold", probability = T)
lines(t, f, col = "red", type = "l", lwd = 2) # Se vedo che la CURVA è approssimativamente una GAUSSIANA, allora i dati sono distribuiti normalmente

# STRUMENTO 2 (grafico)

quartz()
qqnorm(col)
qqline(col, col = "red") ## In caso di normalità perfetta, i dati GIACCIONO sulla retta

# STRUMENTO 3 (test di SHAPIRO)

# Metodo quantitativo, test di Shapiro (per numerosità < 5000)
# H0 : X~N   vs H1: dati non gaussiani
# per P-VALUE BASSI, rifiutiamo l'ipotesi di normalità dei dati
# per P-VALUE ALTI, H0 non può essere rifiutata: i dati provengono da una distribuzione distribuita normalmente
shapiro.test(col)
# Se non c'è evidenza per rifiutare H0, si può lavorare sotto l'ipotesi di gaussianità()

##### TEST di IPOTESI sulla MEDIA di una SINGOLA POPOLAZIONE #####

# STEP DA FARE:
# 1) verifico ipotesi di lavoro
# 2) formulo correttamente ipotesi nulla e alternativa
# 3) calcolo p-value con le formule corrispondenti

# CASO 1: popolazione distribuita normalmente: t-test

# Si supponga di voler aiutare un medico a dimostrare che i pazienti hanno 
# in media un livello di colesterolo inferiore a 200

# Funzione t.test, prestando attenzione alla sintassi:

t.test(dati$Colesterolo,mu = 200,alternative = "less") # UNILATERO A SINISTRA
t.test(dati$Colesterolo,mu = 200,alternative = "greater") # UNILATERO A DESTRA
t.test(dati$Colesterolo,mu = 200,alternative = "two.sided") # BILATERO

# INTERVALLO DI CONFIDENZA PER LA MEDIA del colesterolo #
alpha = 0.1
quantile = qt(1-alpha/2, df = n-1) # qui utilizzo i quantili (qt anzichè pt)
c(media_campionaria - quantile/sqrt(n)*dev_std_campionaria, 
  media_campionaria + quantile/sqrt(n)*dev_std_campionaria)

# CASO 2: popolazione non normale ma osservazioni numerose: z-test

# Proviamo con il peso, che non è distribuito normalmente. 
# Un medico sostiene che i pazienti pesino in media 157 libbre,
# sottoponiamo l'affermazione a verifica d'ipotesi con uno z-test asintotico

# Ho sostituito pnorm al posto di pt

# TEST BILATERO
# H0: mu=157 vs H1: mu!=157
mu_0 = 157
n = length(dati$Peso)
media_campionaria = mean(dati$Peso)
dev_std_campionaria = sd(dati$Peso)
t = (media_campionaria - mu_0)*sqrt(n)/dev_std_campionaria #Statistica test
pval = 2*(1-pnorm(abs(t))) # Calcolo il pvalue del test bilatero
pval

# Anche in questo caso è possibile formulare TEST UNILATERO a DESTRA e SINISTRA
# ricalcando i casi visti in precedenza, con l'accorgimento di sostituire pnorm al posto di pt

# Anche gli intervalli di confidenza di costruiscono in maniera analoga:
# utilizzando qnorm

# INTERVALLO DI CONFIDENZA PER LA MEDIA del peso #
alpha = 0.05
quantile = qnorm(1-alpha/2) 
n = length(dati$Peso)
c(media_campionaria - quantile/sqrt(n)*dev_std_campionaria, 
  media_campionaria + quantile/sqrt(n)*dev_std_campionaria)

##### TEST di CONFRONTO tra MEDIE di popolazioni indipendenti #####

# 1) Verificare se i dati sono distribuiti NORMALMENTE
# Una volta verificato che i dati sono distribuiti normalmente procediamo:

# 2) Varianze UGUALI o DIVERSE? --> TEST DI FISHER ( basta saper leggere il p-value )
var.test(colesterolo_uomini, colesterolo_donne)

# CASO A VARIANZE NON NOTE

# Vogliamo testare: 
# H0: muF = muM vs H1: muF != muM
# Scorciatoia: Usare la funzione t.test
t.test(colesterolo_uomini,colesterolo_donne, var.equal = T) # T = TRUE
# nota: esiste anche var.equal = F per il caso in cui le varianze siano diverse

# CASO A VARIANZE NOTE

# date le nostre due popolazioni normali
# SE LE DUE POPOLAZIONI NON SONO NORMALI (e ce ne accorgiamo dallo shapiro.test)--> 

# supponiamo che le varianze siano NOTE e indichiamole con var_M e var_F
var_M = 150
var_F = 110
differenza_medie = mean(colesterolo_uomini) - mean(colesterolo_donne)
n_uomini = length(colesterolo_uomini)
n_donne = length(colesterolo_donne)
z = differenza_medie / sqrt(var_M/n_uomini + var_F/n_donne)
pvalue = 2*(1-pnorm(abs(z)))
pvalue

# VARIANZE NON NOTE

x = dati$peso

var(x) # sarà la varianza dei dati

# DATI NON NORMALI

# trovo come prima:

var_M 
var_F
n_uomini = length(colesterolo_uomini)
n_donne = length(colesterolo_donne)

s2p = 1/(n_uomini + n_donne - 2)  * ((n_uomini-1)*var_M + (n_donne-1)*var_F)
sp = sqrt(s2p)
t = (mean(donne$peso) - mean(uomini$peso)) / sp / sqrt (1/n_uomini + 1/n_donne)
pval = (pnorm(t))
pval

##### ANOVA #####

# Esempio: Cominciamo con un semplice e famoso dataset, costituito da osservazioni sul peso dei polli
dati_polli

# Ciò che vogliamo capire è: il nutrimento dato ai polli ha influenza sul loro peso medio?

# Esploriamo i dati: con pochi dati è meglio vedere se i dati sono tutti bilanciati
table(dati_polli$feed)
# ok, le classi sono bilanciate

# (1) Tutte le distribuzioni sono normali --> shapiro --- ANOVA fatta sul "peso" scegliendo la "specie" come FATTORE
dati %>% group_by(specie) %>% summarise (p_val_shap = (shapiro.test(peso))$p.value)

# Posso anche fare shapiro.test(db_gruppo1$variabile_studio) insieme a shapiro.test(db_gruppo2$variabile_studio) e vedere se i p-value sono alti e quindi se i dati sono normalizzati

# P-VALUE DI OGNI GRUPPO ALTI? --> DISTRIBUZIONE NORMALE

# (2) Tutte le varianze sono uguali --- ANOVA fatta sul "peso" scegliendo la "specie" come FATTORE
bartlett.test(dati_polli$peso ~ dati_polli$specie) # p-value dovrebbe essere > o al massimo vicino a 0,05

# Procediamo con l'ANOVA:

Anova_polli = aov(peso ~ specie, data = dati_polli)
summary(Anova_polli)
# P-VALUE BASSO --> Pr(>F) --> RIFIUTO H0
# C'è evidenza per affermare che almeno uno dei gruppi abbia media diversa dagli altri

# F value --> valore statistica F

# Vincolo tau_1 = 0
tapply(dati_polli$weight, dati_polli$feed, mean)
Anova_polli$coefficients

# La STIMA della COMUNE VARIANZA si ricava facilmente dalla somma delle varianze nei gruppi (stima varianza errore)
# Sarebbe R2

S = sum(mod$residuals ^ 2) / mod$df.residual
S

# INTERVALLI ANOVA

alpha = 0.05 # capire bene cosa minchia è questo alpha
source("Confronti_multipli_da_anova.R")
intervalli_anova = Confronti_multipli_da_anova(Anova_polli, alpha = alpha)
intervalli = intervalli_anova$intervals
grafico_intervalli = intervalli_anova$plot
intervalli
plot(grafico_intervalli)

# BONFERRONI (altro metodo per gli INTERVALLI)

intervalli_anova = Confronti_multipli_da_anova(Anova_polli, alpha = alpha, bonferroni = TRUE)
intervalli = intervalli_anova$intervals
grafico_intervalli = intervalli_anova$plot
intervalli
plot(grafico_intervalli)

# In entrambi i casi (sia con la correzione per Bonferroni che senza), 
# le differenze in medie sono significativamente diverse da zero.



# ------------------------------------------------------------------------------ LAB 3 #####
##### MATRICE COVARIANZA #####

cov(dati_numerici)

##### MATRICE CORRELAZIONE (COEFFICIENTE CORRELAZIONE) #####

cor(dati_numerici)

# Se il coefficiente di correlazione è alto IN MODULO, esso può provocare problemi al modello lineare --> problemi di collinearità, con grande aumento della varianza della stima dei coefficienti del modello

##### PCA #####

library(rgl)
library(mvtnorm)

# 1) Leggere il file adeguatamente (separatori)
# 2) Separare le variabili numeriche

dati_numerici = dataset_iniziale[,-c(1,2)] # creo un vettore di variabili numeriche prendendo il dataset principale e togliendo le colonne dall'1 al 2 (che non sono numeriche) 

# 3) Vedo se c'è corrrelazione (ovvero se sono più o meno disposte lungo una retta)

x11()
pairs(dati_numerici) # Vedo se c'e' forte correlazione --> ovvero se le osservazioni sono diposte lungo una retta

# 4) Vedere le varianze

x11()
boxplot(dati_numerici, col = "gold")

# se sono tutte molto diverse --> BISOGNA STANDARDIZZARE I DATI
# altrimenti, se sono più o meno simili vanno bene

# 4) STANDARDIZZARE I DATI E PCA

data2pc = scale(dati_numerici) # STANDARDIZZO

pc = princomp(data2pc, scores = TRUE) # Faccio la PCA
summary(pc)

# GRAFICO VARIANZA SPIEGATA per provare a proporre una riduzione dimensionale

x11()
plot(cumsum(pc$sdev^2)/sum(pc$sde^2), type='b', axes=F, xlab='asse_x',
     ylab='asse_y', ylim=c(0,1))
box()
axis(2,at=0:10/10,labels=0:10/10)
axis(1,at=1:ncol(data2pc),labels=1:ncol(data2pc),las=2) 

# 5) Loadings

load.prod = pc$loadings

# Plotto le prime due componenti principali

x11()
par(mar = c(1,4,0,2), mfrow = c(2,1))
for(i in 1:2) barplot(load.prod[,i], ylim = c(-1, 1))

# Da qui faccio l'interpretazione delle componenti

# BIPLOT PCA

x11()
biplot(pc)

# Se ci chiede di proporre una riduzione del dataset, lo vediamo dalla riga CUMULATIVE PROPORTION dell'output della PCA dove ci sono tutte le componenti
# se per esempio le prime 2 componenti comprendono per esempio l'80% del database (è un esempio), allora lo riduciamo alle prime due componenti.

# *** PCR - PRINCIPAL COMPONENT REGRESSION

mod_pcr = pcr(y ~., data = db, scale = T) # metto scale = T se voglio i dati standardizzati
summary(mod_pcr)

# OUTPUT --> X --> varianza cumulata

mod_pcr$coefficients # coefficienti

# ------------------------------------------------------------------------------ LAB 4 #####
##### CLUSTERING: K-MEANS e BONTA' CLUSTERING #####

library(rgl)
library(cluster)
library(ggfortify)

# 1) Leggo il file coin i dati e lo plotto in un grafico cartesiano

osservazioni = read.csv("nome_file")
quartz()
plot(osservazioni, pch = 19, xlab='nome_asse_x', ylab='nome_asse_y')

# Se i dati sono in 3D, devo usare la funzione plot3d()

# 2) Scelgo k e applico la funzione KMEANS per identificare i clusters

# SCELTA DEL MIGLIOR K

b = w = NULL
for(k in 1:10){ # loop on k
  result.k = kmeans(db, k)
  w = c(w, result.k$tot.withinss)  
  b = c(b, result.k$betweenss)
}

par(mfrow=c(1,2))
plot(1:10, b/(w+b),type='b', xlab='clusters', ylab='between/tot', main='Choice of k', ylim=c(0,1),col='orange')
plot(1:10, w/(w+b),type='b', xlab='clusters', ylab='within/tot', main='Choice of k', ylim=c(0,1),col='purple')

# Vedo quanti k mi contengono la maggior parte delle osservazioni

# APPLICO L'ALGORITMO

risultato = kmeans(osservazioni, centers = 2) # dove centers = k scelto

# 3) Plotto i clusters trovati

x11()
vettore_colori = c('royalblue','red')
plot(osservazioni, col = vettore_colori[risultato$cluster], main="K-Means Clustering Results with K=2", pch=20, cex=2, xlab='X.1', ylab='X.2')

# Altri dati che calcoliamo per valutare la BONTA' DEL CLUSTERING

risultato$size # size dei cluster trovati
risultato$cluster
risultato$centers # centri dei cluster trovati
risultato$totss
risultato$withinss
risultato$tot.withinss # ovvero sum(risultato$withinss)
risultato$betweenss

# Possiamo vedere come cambiano le cose aggiungendo nstart (che specifica quante volte ripetiamo kmeans per diverse inizializzazioni)

risultato = kmeans(nidi,2) 
risultato$tot.withinss # inizializzazione random, cambia ogni volta
risultato = kmeans(nidi,3,nstart=20) # R seleziona il clustering con MINOR tot.withinss
risultato$tot.withinss # tipicamente inferiore al caso singola istanza
# infatti, noi vorremmo scegliere la configurazione che MINIMIZZA 
# la somma dei quadrati intra-cluster totale

# *** VALUTARE BONTA' CLUSTERING ***

# 1) Grafico della WITHIN SUM OF SQUARES  (tot.withinss)

tot.withinss = NULL
for(i in 1:10) {
  tot.withinss = c(tot.withinss, kmeans(osservazioni,i, nstart=20)$tot.withinss) 
}
x11()
plot(1:10,tot.withinss, type='b', xlab='K')

# Ci aspettiamo andamento monotono decrescente, troviamo un gomito (elbow)?
# Nell'asse delle x troviamo il numero dei cluster k, dobbiamo attenzionare quando
# cambia di molto, e non quando i pallini rimangono vicini

# 2) Grafico della SILHOUETTE per k = 3 e k = 2
# Si ricorda che:
# Silhouette di circa 1 --> i clusters sono molto densi e ben separati
# Silhouette di circa 0 --> i clusters si sovrappongono 
# Silhouette < 0 --> i clusters potrebbero essere sbagliati/non corretti

x11()
silhouette_3cluster = silhouette(risultato$cluster , dist(osservazioni) )
plot(silhouette_3cluster, col = 1:3, main = 'Silhouette plot per k = 3')
abline(v = mean(silhouette_3cluster[,3]))

x11()
km.output = kmeans(osservazioni, centers = 2, nstart = 20)
silhouette_2cluster = silhouette(km.output$cluster , dist(osservazioni) )
plot(silhouette_2cluster, col = 1:2,  main = 'Silhouette plot per k = 2')
abline(v = mean(silhouette_2cluster[,3]))

##### CLUSTERING GERARCHICO (AGGLOMERATIVO) #####

library(cluster)

# 1) Leggo il file coin i dati e lo plotto in un grafico cartesiano

osservazioni = read.csv("nome_file")
quartz()
plot(osservazioni, pch=19, xlab='nome_asse_x', ylab='nome_asse_y')

# 2) Decido il tipo di DISTANZA da utilizzare

# DISTANZA EUCLIDEA

distanze_euclidee = dist(osservazioni, method = 'euclidean')

# DISTANZA MANHATTAN

distanze_manhattan = dist(osservazioni, method = 'manhattan')

# 3) Creo la MATRICE DELLE DISTANZE sulla base delle distanze calcolate prima

matrice_distanze = as.matrix(distanze_euclidee)
matrice_distanze[1:7,1:7]

# 4) Decido il LINKAGE METHOD:

# COMPLETE LINKAGE: distanza tra cluster è distanza tra i punti più vicini
cluster.complete = hclust(distanze_euclidee, method="complete")

# AVERAGE LINKAGE: distanza tra cluster è distanza tra i centroidi
cluster.average = hclust(distanze_euclidee, method="average")

# SINGLE LINKAGE: distanza tra cluster è distanza tra i punti più lontani
cluster.single = hclust(distanze_euclidee, method="single")

# 5) Plotto il DENDROGRAMMA (uno per ogni metodo di linkage)

x11()
par(mfrow=c(1,3))
plot(cluster.complete, main="Complete Linkage", xlab="", sub="", cex=.9)
plot(cluster.average, main="Average Linkage", xlab="", sub="", cex=.9)
plot(cluster.single, main="Single Linkage", xlab="", sub="", cex=.9)

# 6) TAGLIO del dendrogramma

# Devo scegliere a che livello tagliare (es.: k = 4)

taglio_complete = cutree(cluster.complete, 4)
table(taglio_complete)

taglio_average = cutree(cluster.average, 4)
table(taglio_average)

taglio_single = cutree(cluster.single, 4)
table(taglio_single)

# Medie dei gruppi (esempio --> k = 2 quindi ci sono 2 cluster)

colMeans(dati[taglio_... == 1,])  # medie nel gruppo 1
colMeans(dati[taglio_... == 2,])  # medie nel gruppo 2

# Plot dei clusters identificati (esempio --> k = 2 quindi ci sono 2 cluster)
ggpairs(dati, aes(col=as.factor(taglio_...)))

# Posso vedere come cambiano i clusters a seconda del metodo linkage

x11()
par(mfrow = c(1,3))
plot(osservazioni, col = taglio_complete)
plot(osservazioni, col = taglio_average)
plot(osservazioni, col = taglio_single)

# Posso vedere come cambia il grafico con un taglio diverso, per capire quale metodo linkage pecca

# I cluster ellissoidali tendono ad essere meglio identificati 
# dal single linkage.

# ------------------------------------------------------------------------------ LAB 5 #####
##### REGRESSIONE LINEARE SEMPLICE --> DIAGNOSTICA --> INFERENZA #####

# REGRESSIONE LINEARE --> lm()
# REGRESSIONE LOGISTICA --> glm() --> CREO UN DB MODIFICATO DOVE METTO SOLO LE VARIABILI CHE VOGLIO POI NEL MODELLO!!! (altrimenti mi da errore)

# Leggiamo il file con i dati da analizzare

# REGRESSIONE LINEARE SEMPLICE con la funzione lm()

db # database

## x = data$Education --> covariata
## y = data$Income --> variabile studio
## n = length(x)
## p = 1
## beta1 =  cor(x,y) * sd(y)/sd(x)
## beta0 = mean(y) - beta1*mean(x)

primo_modello = lm(Income ~ Education, data = db)

par(mfrow = c(2,2))
plot(mod) # per vedere l'eteroschedasticità dei residui --> eteroschedasticità = DIVERSA VARIANZA

primo_modello$coefficients

beta0 = primo_modello$coefficients[1]
beta1 = primo_modello$coefficients[2]

# Traccio la RETTA DI REGRESSIONE

x = db$Education
y = db$Income
plot(x, y, xlab = "Education", ylab = "Income", main = "Titolo")
abline(beta0, beta1)

# Calcolo i valori fittati

y_hat_by_hand = beta0 + beta1*db$Education
y_hat = primo_modello$fitted.values
y_hat_by_hand
y_hat

# Calcolo i RESIDUI:

eps_hat = y - y_hat 

# Calcolo i GRADI DI LIBERTA' del modello:

p = 1
n = dim(db)[1]
dof = n - p - 1
dof

# Stimo la VARIANZA RESIDUALE:

sigma_hat = sum(eps_hat^2)/dof
sigma_hat
sqrt(sigma_hat)

# Calcolo di R^2 (0 < R^2 < 1) --> percentuale di variabilità --> !!!(la troviamo nel summary del modello)!!! (se vogliamo la percentuale bisogna fare il valore del summary * 100)

R2 = 1 - sum(eps_hat^2)/ sum((y - mean(y))^2)
R2
R2_adj = 1 - (n-1)/(n-p-1) * sum(eps_hat^2)/ sum((y - mean(y))^2)
R2_adj

summary(primo_modello) # riassunto di tutti i valori trovati

# Per ottenere tutti i possibili output che ci servono dal modello lineare costruito:
names(primo_modello)
primo_modello$coefficients  # valori stimati dei beta
primo_modello$residuals     # residui : epsilon_hat
primo_modello$rank          # p + 1 
primo_modello$fitted.values # valori fittati: y_hat
primo_modello$df.residual   # gradi di liberta' del modello: n - p - 1 
primo_modello$model         # Il data frame dei dati
vcov(primo_modello)         # Matrice di covarianza dei beta
rstandard(primo_modello)    # Residui standardizzati

# *** DIAGNOSTICA DATI *** --> vediamo se possiamo fare inferenza:

# DIAGNOSTICA = CONTROLLARE LE ASSUNZIONI

# 1) NORMALITA' dei residui:

shapiro.test(eps_hat)

# P-VALUE ALTO --> OK
# P-VALUE BASSO --> NON OK

# 2) OMOSCHEDASTICITA' dei residui:

plot(db$Education, scale(eps_hat))
abline(h = 0)

# Dobbiamo attenzionare se c'è un trend in particolare, se non c'è --> tutto ok

# Ulteriori grafici utili per la diagnostica sui residui:

par(mfrow = c(2,2))
plot(primo_modello)

# Per vedere se i residui di un modello sono o no distribuiti normalmente:
shapiro.test(mod$residuals) # se il p-value è basso --> non sono normalmente distribuiti

# *** INFERENZA DATI ***

# Intervalli di confidenza per i beta:

alpha = 0.05
confint(primo_modello, level = 1 - alpha)

# Possiamo correggere per la nostra dimensionalità (p+1):

confint(primo_modello, level = 1 - alpha/2)  # in generale: 1 - alpha/(p+1)

# Intervalli di confidenza e predizione per una nuova osservazione x0:

x_0 = 16.5
# Se volessi generare 20 nuove osservazioni --> X.new = data.frame(variabile = seq(min(variabile), max(variabile), len = 20))
# mettere poi nelle seguenti funzioni come secondo argomento X.new

# Se invece ci dice quali valori nuovi mettere, faccio per esempio:
# newdata = data.frame(paese = 1, etnia = "Ispanici", anni = 0.1, reddito = -0.05, popolazione = 1.2, prezzo = -1.3)
# predict(model, newdata = newdata, interval = "prediction", level = 0.95) # al 95% di confidenza

IC = predict(primo_modello, data.frame(Education = x_0), interval = "confidence", level = 1 - alpha) # intervallo per la media

IP = predict(primo_modello, data.frame(Education = x_0), interval = "prediction", level = 1- alpha) # intervallo di predizione

IC
IP

# Riportiamo graficamente gli intervalli puntuali di predizione e confidenza:

X0   = data.frame(Education = seq(10, 22, length=100)) # esempio con 100 nuovi punti

# Creiamo 100 "nuovi" punti e interpoliamo:

Conf = predict(primo_modello, X0, interval='confidence')
Pred = predict(primo_modello, X0, interval='prediction')

plot(db$Education, db$Income, pch=19, xlab = 'Education', ylab = 'Income')
lines(X0[,1], Conf[,'fit'], lwd=1.5)
lines(X0[,1], Conf[,'lwr'], lty=2, col='red', lwd=3)
lines(X0[,1], Conf[,'upr'], lty=2, col='red', lwd=3)

lines(X0[,1], Pred[,'lwr'], lty=3, col='deepskyblue', lwd=3)
lines(X0[,1], Pred[,'upr'], lty=3, col='deepskyblue', lwd=3)

legend('topleft', legend = c('Fit', 'Confidence Interval', 'Prediction Interval'), lwd=2,
       lty = c(1,2,3) , col = c('black', 'red', 'deepskyblue') )


##### REGRESSIONE LINEARE MULTIPLA --> DIAGNOSTICA --> INFERENZA #####

# Nella regressione lineare multipla abbiamo più covariate

# db$Income --> variabile studio
# db$Education --> covariata
# db$Seniority --> covariata

open3d()
plot3d(db$Education, db$Seniority, db$Income, col = "red")

secondo_modello = glm(Income ~ Education + Seniority,family = binomial(link = logit), data = db)
summary(secondo_modello)

# 3) Posso plottare il PIANO DI REGRESSIONE:

# open3d()
# plot3d(db$Education, db$Seniority, db$Income, size=5) 
# beta = secondo_modello$coefficients
# planes3d(a = c(-beta[2],- beta[3],1), d = -beta[1], col = "green", alpha = 0.4)
# y_hat = secondo_modello$fitted.values
# coord_fitted = as.matrix(cbind(db[ ,1:2], y_hat))
# points3d(coord_fitted, col='forestgreen', size=2)
# for ( i in 1:n) lines3d(rbind(as.numeric(db[i,1:3]),coord_fitted[i,]), col = "red")

# *** DIAGNOSTICA: Controllo le assunzioni: ***

# 1) Normalità dei residui:

shapiro.test(secondo_modello$residuals)

# P-VALUE ALTO --> OK
# P-VALUE BASSO --> NON OK

# 2) Omoschedasticità dei residui:

par(mfrow = c(2,2))
plot(secondo_modello)

# Dobbiamo attenzionare se c'è un trend in particolare, se non c'è --> tutto ok

# Verificate le 2 condizioni --> possiamo fare INFERENZA (è la stessa della regressione lineare semplice)

# Se per esempio il modello da fare è:

# y = beta_0 + beta_1*Education + beta_2*Seniority + beta_3*Education*Seniority

third_model = lm(Income ~ Education + Seniority + Education*Seniority, data = db)

# oppure:

# y = beta_0 + beta_1*Education + beta_2*Seniority + beta_3*(Education)^2

fourth_model = lm(Income ~ Education + Senority + I(Education^2), data=data)

# ------------------------------------------------------------------------------ LAB 6 #####
##### REGRESSIONE LINEARE MULTIPLA CON 1 PREDITTORE CATEGORICO (DUMMY VARIABLES)#####

db # database

Y = db$variabile_studio
X = db$covariata

plot(X, Y, main='Scatterplot di Y vs X', lwd=2)

# Y = beta_0 + beta_1 * X + Eps
result = lm(Y ~ X)
summary(result)

# Aggiungiamo la retta di regressione
coef = result$coef
plot(X,Y)
abline(coef[1], coef[2], lwd=2)

# Vogliamo aggiungere una variabile dummy (è una variabile categorica che assume valori 0 o 1)

# Y = beta0  +  beta1 * dummy  +  beta2 * X  +  beta3 * dummy * X  +  eps

db$dummy = factor(db$dummy)
model = lm(Y ~ dummy + X + dummy:X, data = db)
summary(model)

# Facciamo la diagnostica

shapiro.test(model$residuals)

##### REGRESSIONE LINEARE MULTIPLA CON PIU' PREDITTORI CATEGORICI #####

# model --> Y = beta0 + beta1*dummy1 + beta2*dummy2 + beta3*X + beta4*dummy1*X + beta5*dummy2*X + eps

db$dummy1 = factor(db$dummy1)
db$dummy2 = factor(db$dummy2)

model = lm(Y ~ dummy1 + dummy2 + X + X:dummy1 + X:dummy2, data = db)
summary(model)

# Facciamo la diagnostica

shapiro.test(model$residuals)

##### STEPWISE FORWARD - BACKWARD - MIXED - SELECTION #####

# Definisco il modello minimo con solo l'intercetta,
# passando però tutto il dataset alla funzione
# questo permetterà a stepAIC di sapere dove prendere le variabili da
# aggiungere fino a raggiungere l'upper bound

# # N.B.: Posso aggiungere all'argomento delle seguenti funzioni il valore di un k = numero

# FORWARD

model = lm(...) # modello base utilizzato

miminal_model = lm(variabile_studio ~ 1, data = db) # modello minimo da cui parto

forward = stepAIC(miminal_model,direction="forward",
                  scope=list(lower = miminal_model, upper = model))
forward$anova
summary(forward)
par(mfrow = c(2,2))
plot(forward)

# BACKWARD

backward = stepAIC(model, details = T, direction = "backward") # dove "model" è il modello lm utilizzato
backward$anova
summary(backward)
par(mfrow = c(2,2))
plot(backward)

# MIXED

both = stepAIC(model,details = T, direction = "both") # dove "model" è il modello lm utilizzato
both$anova
summary(both)
par(mfrow = c(2,2))
plot(both)

##### CROSS-VALIDAZIONE #####

set.seed(1)

# Per esempio, questo fitta il modello lineare seguente:
# Y = beta.0 + beta.1*X + eps
glm.fit = glm(Y ~ X, data = db)
summary(glm.fit)

# CROSS-VALIDAZIONE LEAVE-ONE-OUT

cv.err = cv.glm(db, glm.fit) # NB: non stiamo specificando nulla su K, quindi K = n!
cv.err$delta[1]  # L'errore in crossvalidazione

# CROSS-VALIDAZIONE K-FOLD

cv.err = cv.glm(db,glm.fit, K = 10)
cv.err$delta[1]

# ------------------------------------------------------------------------------ LAB 7 #####
##### PCA REGRESSION (COLLINEARITA' SHRINKAGE) #####

# *** PCA ***
pca = princomp(regressori, scores=T)  # --> mettere i regressori (ovvero le variabili del db che voglio studiare)
summary(pca)

# Per risolvere problemi di collinearità --> Metodi di Shrinkage (Ridge e Lasso, ad esempio) e anche Eliminazione Stepwise

db # database

model_ols = lm(variabile_studio ~. , data = db)
summary(model_ols) 

# Vedo tutti i p-value
# Noto che ce ne sono molti che non sono statisticamente significativi
# Vorrei usare solamente le variabili che spiegano una parte significativa della risposta
# Ma guardare solo ai p-value non basta

vif(model_ols) # Variance Inflation Factor : valutare la collinearità # se c'è un vif molto alto --> quella covariata ha un grande problema di collinearità

# PCA REGRESSION
# La faccio solo su variabili numeriche --> elimino dal dataset tutte quelle categoriche con il comando  = NULL

db_numerico # db iniziale di sole variabili numeriche

model_ols_num = lm(variabile_studio ~. , data = db_numerico)
summary(model_ols_num)
vif(model_ols_num)

boxplot(db_numerico) # verifico se bisogna standardizzare le variabili

# se i box plot sono diversi devo standardizzare con la funzione "scale"

db_numerico$... = scale(db_numerico$...)

pca = princomp(regressori, scores=T) # Facciamo la PCA --> mettere i regressori (ovvero le variabili del db che voglio studiare)
summary(pca)

# Per vedere la percentuale di variabilità --> dal summary "Proportion Of Variance"

plot(cumsum(pca$sd^2)/sum(pca$sd^2), type='b', axes=F, xlab='number of components', 
     ylab='contribution to the total variance', ylim=c(0,1))
abline(h=1, col='blue')
abline(h=0.8, lty=2, col='blue')
box()
axis(2,at=0:10/10,labels=0:10/10)
axis(1,at=1:ncol(regressori),labels=1:ncol(regressori),las=2) # definire i regressori

pca$loadings # visualizzo i loadings

boxplot(pca$scores) # visualizzo boxplot degli scores

# scelgo a caso un k / oppure scelgo la crossvalidazione vista nello scorso laboratorio

k = 5

data_for_regression = data.frame(cbind(variabile_studio = db_num$variabile_studio, pca$scores[,1:k]))

# Fittiamo il nostro modello lineare con le prime 5 componenti:

pca_model = lm(variabile_studio ~., data = data_for_regression)
summary(pca_model)

vif(pca_model) # VIF pari a 1 rende minima la varianza dei beta --> questo è il caso in cui tutti i predittori sono scorrelati

# Quali sono i coefficienti rispetto alle variabili originali?

load = pca$loadings[,1:k]
load

beta_original = load %*% c(coef(pca_model)[2:(k + 1)]) #partiamo da 2 perchè la prima e' l'intercetta!
beta_original

validationplot(pcr_model, val.type = "RMSEP", estimate = "all", legendpos = "topright")
axis(side=1, at=0:16, labels = 0:16,cex.axis=0.7) # metto a confronto le stime fatte sul train con ciò che ho fatto fino ad ora

# Possiamo fare la predizione:

predict(pcr_model, newdata = db_num, ncomp = 4) # occhio a metter il giusto numero delle componenti

##### RIDGE E LASSO #####

# RIDGE

db # database

x = model.matrix(variabile_studio~. , data = db)[,-1] # se non devo considerare una variabile --> data = db_new --> dove db_new è il nuovo db con le sole variabili che voglio usare

lambdas = c(10^seq(10,-2,length.out = 100) ,0) # definisco un lambda per cui voglio fittare il modello (lungo 101 e DECRESCENTE)

# Il lambda lo scelgo con la cross-validazione

lambdas = 10^seq(5,-3,length = 100)

# Faccio il RIDGE:
set.seed(1000)
model_ridge = glmnet(x, db$variabile_studio, lambda = lambdas, alpha = 0) # fare il comando di x

plot(model_ridge, xvar='lambda',label=TRUE,col =  rainbow(dim(x)[2]))
legend('bottomright', dimnames(x)[[2]], col =  rainbow(dim(x)[2]), lty=1, cex=.5)

coef(model_ridge) # coefficienti per ogni lambda

coef(model_ridge)[,50] # coefficienti per il 50esimo lambda

# Più velocemente vado a zero, meno le covariate sono "significative"/"importanti".
# Tuttavia nella Ridge regression i coefficienti non andranno mai a zero, per costruzione.
# Notiamo anche che per lambda = 0 abbiamo al stima OLS.

plot(model_ridge, label = T, xvar = "dev") # Pseudo R^2 sull'ascissa

# Il lambda di prima era scelto arbitrariamente,
# Posso trovare lmbda o con cross validazione (vedi prima) o con il cv
set.seed(1000)
cv.model_ridge = cv.glmnet(x,data$variabile_studio, lambda = lambdas,alpha = 0, nfolds = 10) # nfolds = 10 per default (il minimo può essere 3)

plot(cv.model_ridge)

bestlam = cv.model_ridge$lambda.min
bestlam # lambda in corrispondenza del minor MSE in CV

indbest = which(cv.model_ridge$lambda == bestlam) # Selezioniamo il modello corrispondente a quel lambda
indbest # Modello corrispondente a quel lambda

reglam = cv.model_ridge$lambda.1se  
reglam # il piu' grande lambda

indreg = which(cv.model_ridge$lambda == reglam)
indreg # Modello corrispondente a quel lambda

coef(model_ridge) # coefficienti modello

coef(model_ridge)[,c(indbest,indreg)]
summary(model_ols)
cv.model_ridge$cvm[c(indbest,indreg)]
cv.model_lasso$cvm[c(indbest,indreg,101)] # Confronto MSE migliore, regolarizzato, originale

# LASSO
set.seed(1000)
lasso.mod = glmnet(x, db$variabile_studio, alpha=1, lambda = lambdas)

plot(lasso.mod, xvar='lambda', label=TRUE, col =  rainbow(dim(x)[2]))
legend('bottomright', dimnames(x)[[2]], col =  rainbow(dim(x)[2]), lty=1, cex=.5)

# Select the best lambda via CV
set.seed(1000)
cv.out = cv.glmnet(x, db$variabile_studio,alpha=1,nfolds=3,lambda=lambda.grid) 

plot(cv.out)

bestlam2 <- cv.out$lambda.1se
bestlam2 # --> VALORE LAMBDA MODELLO PIU' REGOLARIZZATO
log(bestlam2)

# now we can inspect the resulting model
coef.lasso <- predict(lasso.mod, s=bestlam2, type='coefficients')
coef.lasso

# Da qui vediamo quali covariate (e anche intercette in caso) trattiene il modello, ovvero trattiene quelle dove spunta il valore e non il punto.

# Le regressioni OLS e Ridge (per il lambda scelto) hanno parametri per lo più identici, 
# mentre le regressione Lasso ha portato alcuni parametri a 0 (horsepower e weight sono)
# molto vicini a 0 ed hanno un effect size molto piccolo sulla risposta. I beta di year e origin 
# si sono avvicinati verso zero, riducendo la varianza spiegata dal modello e facendo selezione 
# delle variabili a discapito di una bias più grande 
# (in quantità accettabile, entro 1sd dal modello che minimizza il CV-MSE) 

# ------------------------------------------------------------------------------ LAB 8 #####
##### REGRESSIONE LOGISTICA (SEMPLICE E MULTIPLA) #####

# # Y_i ~ B(p_i)
# logit(p_i) = beta0 + beta1 x_i    i=1,...,n
# dove logit(p_i) = log(p_i / (1-p_i)) per 0<p_i<1
# da cui si ottiene hatp_i = exp( hatbeta0 + hatbeta1 x_i ) / (1 + exp( hatbeta0 + hatbeta1 x_i ))

dati # Importo il dataset

# Regressione logistica --> abbiamo una variabile categorica (0,1)
# e dobbiamo capire la correlazione di questa variabile categorica con le altre del dataset

# Esempio
# dataset --> "dati"
# variabile catagorica (binaria) --> "indice"

table(dati$indice) # proporzioni classi (0,1) nel database

table(dati$var1, dati$indice) # proporzioni tra la variabile chiamata "var1" e la variabile "indice"

plot(dati$var1, dati$indice) # plot delle proporzioni tra la variabile chiamata "var1" e la variabile "indice"

# *** REGRESSIONE LOGISTICA SEMPLICE ***

# age --> variabile PREDITTORE
# dati --> dataset
# indice --> variabile categorica

mod = glm(indice ~ age, family=binomial(link=logit), data = dati)
summary(mod)

mod$coefficients # COEFFICIENTI del modello visti nel summary --> vediamo i coefficienti e quindi capiamo quanto incidono

mod$linear.predictors[1:20] # PREDITTORI LINEARI prime 20 (prime 20 è solo un esempio)

mod$fitted.values[1:20] # probabilità prime 20 osssservazioni

# Se t = logit(p) allora p = exp(t)/(1 + exp(t))
exp(mod$linear.predictors[1:20])/(1 + exp(mod$linear.predictors[1:20]))

# Con queste probabilità possiamo tracciare la CURVA SIGMOIDALE stimata:
par(cex = 1.5)
punti_x = (intervalli[1:10] + intervalli[2:11]  )/2
plot(punti_x, prop, pch = 18)
abline(h = 0)
abline(h = 1)
lines(sort(dati$age),sort(mod$fitted.values),col = "red")

# Dobbiamo vedere se la curva passa attraverso i punti nel grafico o no

# PREDIZIONE:

pred = predict(mod, db, type = "response") # notare type = 'response': così troviamo la probabilità

# Curva ROC

pred = mod$fitted.values
PRROC_obj <- roc.curve(scores.class0 = pred, weights.class0 = as.numeric(paste(db$variabile_studio)), curve = TRUE) #
plot(PRROC_obj)  # se non va, prova a mettere: weights.class0 = db$variabile_studio

# Qual è il migliore p0 per classificare? Tipicamente si
# scelgono i punti più vicini all'angolo in alto a sinistra,
# ma questo comunque dipende fortemente dal problema reale

# Scelta SOGLIA PER CLASSIFICAZIONE

p = seq(0.2,0.9, by = 0.05)
sens = spec = acc = aper = rep(0,length(p))
for ( i in 1:length(p))
{
  pred_class=ifelse(pred>p[i],1,0) 
  misclass_table=as.matrix(table(pred_class, db$indice))
  sens[i] = misclass_table[2,2]/(misclass_table[2,2] + misclass_table[1,2])
  spec[i] = misclass_table[1,1]/(misclass_table[1,1] + misclass_table[2,1])
  acc[i]  = (misclass_table[1,1] + misclass_table[2,2])/dim(db)[1]
  aper[i] = 1 -acc[i]
}

plot(p, sens, col = "red", type = "l", ylim = c(0,1))
lines(p, spec, col = "green", type = "l")
lines(p, acc, col = "salmon", type = "l" )
lines(p, aper, col = "purple", type = "l")
legend("topleft", fill = c("red","green","salmon", "purple"), legend = c("sensitivity","specificity","accuracy","aper"))

# Se volessi calcolare gli indicatori attraverso la TABELLA DI MISCLASSIFICAZIONE, devo fare:

pred = mod$fitted.values
pred_class = ifelse(pred > 0.5,1,0) # se la p > soglia classifico come sopravvissuto, altrimenti no
table(pred_class, db$variabile_studio)
misclass_table = as.matrix(table(pred_class, db$variabile_studio)) # METTERE IL DB COMPLETO
misclass_table

sens = misclass_table[2,2]/(misclass_table[2,2] + misclass_table[1,2]) # tpr (o anche recall)
sens

spec = misclass_table[1,1]/(misclass_table[1,1] + misclass_table[2,1]) # tnr
spec

fpr = 1 - spec
fpr

acc  = (misclass_table[1,1] + misclass_table[2,2])/dim(db)[1]
acc

aper = 1 - acc
aper

# *** INTERPRETAZIONE DEI COEFFICIENTI (OR) ***

summary(mod)
coef(mod)[2]
exp(1*coef(mod)[2]) # OR per incremento di un anno di età (variabile "age" usata in mod)
exp(10*coef(mod)[2]) # OR per incremento di 10 anni di età (variabile "age" usata in mod)

# N.B. !!! NON E' COME VARIA LA PROBABILITA' !!!

# *** REGRESSIONE LOGISTICA MULTIPLA ***

# Adesso facciamo la stessa cosa ma con 2 variabili: "var1" e "var2"

# indice --> variabile categorica
# dati --> dataset
# var1
# var2

mod = glm(indice ~ var1 + var2, family = binomial(link=logit), data = dati)   
summary(mod)

# Si possono usare le formule per la regressione lineare semplice (predizione, curva OR...) basta che il modello usato negli argomenti sia MULTIPLO 

anova(mod, test="Chisq") # in questo modo capiamo meglio la significatività delle variabili

# Interpretazione degli ODDS Ratios dei vari predittori
# Quant'è l'ODD Ratio tra sesso maschile e femminile? E per una comorbidità in più?

# Per il sesso maschile:
exp(coef(mod)["sex1"]) # odds ratio tra uomini e donne

# SE VOLESSI STIMARE L'IMPATTO DI 10 ANNI IN PIU', PER ESEMPIO:

coef = mod$coefficients
coef.year = coef["year"]
ans = 10 * coef.year
print(ans)

# Per le comorbidità
exp(coef(mod)["n_com"]*1) #odds ratio per una comorbidità in più

exp(coef(mod)["age"]*10) # odds ratio per un aumento di età di 10 anni

mod$linear.predictors[1:20] # valori stimati dei logit

mod$fitted.values[1:20] # Valori stimati per la probabilità di decesso 

confint.default(mod, level = 1-alpha) # Intervalli di confidenza per i beta

# Proviamo a fare qualche previsione CON NUOVI DATI

newdata = data.frame(age = 95, sex = "1", n_com = 2, n_pro = 2) # oppure --> age = as.factor(95)
predict(mod, newdata = newdata,type = "response")

# La quantità AUC, area under the curve è una misura della bontà generale del classificatore basato 
# sulle probabilità: AUC=0.5 corrisponde ad un classificatore inutile (più propriamente, un classificatore 
# che assegna le etichette a caso) mentre AUC = 1 corrisponde ad un classificatore perfetto 
# (o almeno, perfetto sul training set). Valori più vicini a 1 corrispondono quindi a modelli migliori.

##### K-NEAREST NEIGHBORS (KNN) #####

# Nella classificazione K-NN, viene stimata la Probabilità che un punto nello spazio delle covariate 
# sia associato ad una classe (un’etichetta), in base alla proporzione di elementi delle k osservazioni 
# più vicine che appartengono a quella classe. 
# Di conseguenza, un punto è classificato in base a un voto di pluralità dei suoi vicini. 
# Se k = 1, l’oggetto viene semplicemente assegnato alla classe di quel singolo vicino più prossimo.

k = 3 # arbitrario

# dataset iniziale = db

# Se volessi usare un db con solamente le variabili che nel db iniziale si trovano alla colonna numero 2, 7, 9, 10:

data.knn = db[, c(2,7,9,10)]

# Classificazione KNN

result.knn = knn(train = data.knn[,1:2], test = data.knn[,1:2], cl = data.knn$variabile_studio, k = k, prob = T) # considero train e test uguali e prendo le colonne nel posto 1 e 2

result.knn # Risultato della classificazione:

attributes(result.knn)$prob # Risultati del "voto":

# Visualizziamo la MISCLASSIFICATION TABLE:

# N.B.: in questo caso, abbiamo classificato il dataset "data.knn" in base alla variabile "variabile_studio"

table(pred = result.knn, true = data.knn$variabile_studio)

# Cosa accade quando VARIA K?

## Potremmo scegliere K con i soliti metodi (test set, cross validazione e così via)

## NOTA: 
# Si noti che all’aumentare di k le frontiere di classificazione tendono a diventare più regolari, (meno frastagliate), 
# poichè l’impatto delle etichette delle singole osservazioni viene progressivamente ridotto.
# Al limite in cui k=n, una sola classe, quella maggioritaria, viene proposta per ogni punto da classificare

# riassumendo
# Se k si riduce il bias diminuisce ma la varianza aumenta: k troppo piccoli producono seri rischi di overfitting
# Se k aumenta il bias aumenta e la varianza diminuisce. k troppo grandi tipicamente producono classificatori di modeste prestazioni.


# ------------------------------------------------------------------------------ LAB 9 #####
##### ALBERI DI REGRESSIONE #####

set.seed(02091991) # Setto il seed

mele # dataset

# TRASFORMO LE VARIABILI NON NUMERICHE con la funzione factor

db$variabile = factor(db$variabile)

# Plot dei dati

plot(mele$asse_x, mele$asse_y)
points(mele[mele$var_categoria == 'categoria_1',2:3], col='red', pch=19) # 2:3 è la posizione delle variabili "asse_x" e "asse_y" nel dataset (si conta apartire da 1)
points(mele[mele$var_categoria == 'categoria_2',2:3], col='green', pch=19)

# Divido il dataset in 2 parti: TRAINING e TEST

train = sample(1:nrow(mele), nrow(mele)/2) # se voglio 50% e 50%
train = sample(1:nrow(mele), round(0.7*nrow(mele)))  # 70% di righe per il training

mele_train = mele[train,]
mele_test = mele[-train,]

# FATTORIZZARE LE VARIABILI DEI NUOVI DB

tree.mele = tree(var ~.,data = mele_train) # var --> variabile che voglio analizzare (lasciare ~. se non voglio che ci siano altre variabili nel mio modello)
summary(tree.mele)

# Visualizzo l'albero:

plot(tree.mele)
text(tree.mele, pretty=0)

# Predizione sull'albero senza pruning:

# Il comando predict funziona come al solito, 
# si passa il modello e il data frame su cui si vogliono fare le predizioni

yhat = predict(tree.mele, newdata= mele_test)

# Visualizzo le vere risposte e le predizioni:

plot(mele_test$variabile_studio, yhat)
abline(0,1)   # Idealmente è tutto vicino alla bisettrice

# Valutare l'MSE sul test set:

mean((yhat-mele_test$variabile_studio)^2) # test set MSE ("variabile_studio" è la variabile che sto studiando)

##### PRUNING E CROSS VALIDAZIONE #####


# Vogliamo trovare il migliore albero con 7 nodi terminali:

prune.mele = prune.tree(tree.mele, best = 7)
summary(prune.mele)

# Visualizzo:

plot(prune.mele)
text(prune.mele, pretty = 0)

# Ma come facciamo a scegliere il "migliore"? Cross validazione!

# prune.tree --> alberi di REGRESSIONE 
# prune.misclass --> alberi di CLASSIFICAZIONE

cv.mele = cv.tree(tree.mele, FUN = prune.tree)
plot(cv.mele$size,cv.mele$dev,type='b')  ## qual e' la size migliore? --> quella dove il grafico inizia a stabilizzarsi

# Presa la decisione, potiamo l'albero:

# In questo caso, la size migliore è = 3

prune.mele = prune.tree(tree.mele, best = 3)

plot(prune.mele)
text(prune.mele, pretty=0)

# Visualizziamo dunque la variabili piu' importanti:

# In questo caso, e variabili più importanti sono la 14, 6 e la 13

ggpairs(mele[,c(14,6,13)])

# PREDIZIONE sul db di TEST: (se volessi fare una predizione sul train, basta cambiare nella seguente funzione il database)

mele.pred = predict(prune.mele, newdata = mele_test)

table(mele.pred, mele_test$target) # target sarebbe la variabile che indica la "categoria" delle osservazioni (quella che vogliamo studiare --> che mettiamo a sinistra della funzione)

misclass_table = as.matrix(table(mele.pred, mele_test$target))

# Se per esempio faccio un modello, e voglio prevedere (probabilità) un evento con certe caratteristiche:
# newdata = data.frame(gpa = 6.5 , gender = "0" , sport = 1) # caratteristiche
# predict(model, newdata = newdata, type = "response") # probabilità

# Calcolo indicatori:

acc  = (misclass_table[1,1] + misclass_table[2,2] + misclass_table[3,3] + misclass_table[4,4] )/sum(misclass_table) # perchè, in questo caso, la variabile target assume 4 possibili valori
acc

aper = 1 - acc
aper

mean((mele.pred - mele_test)^2) # test set MSE

##### ALBERI DI CLASSIFICAZIONE #####

set.seed(02091991) # setto il seed

motori # dataset

# Facciamo un istogramma e dividiamo il dataset con una linea:

# La variabile che considero è "Sales" e dico:

# Sales > 8 -->  alto
# Sales <= 8 --> basso

# Costruiamo la variabile categorica corrispondente:

motori$High = ifelse(motori$Sales <= 8, 1, 0)
motori$High = as.factor(motori$High)
table(motori$High)

# Se abbiamo un modello e vogliamo creare una variabile categorica (stimatore) data una certa soglia (per esempio = 0,4)

pred_test = predict(mod, newdata = db_test, type = "response")
class = ifelse(pred_test >= 0.4, 1, 0)
tab = table(class, db_test$variabile_studio) # errore di classificazione
tab
# output
# 1 2
# 3 4

(2+3)/(1+2+3+4) # errore

# Fittiamo un albero con tutte le altre variabili, utilizziamo un training:

train = sample(1:nrow(motori), 200)  ## 200 righe per training --> 400 - 200 = 200 righe anche per il test
motori_train = motori[train,]
motori_test = motori[-train,]

tree.motori = tree(High~.-Sales, motori_train)
summary(tree.motori)

# Plotto l'albero:

plot(tree.motori) # se non va, prova a factorizzare la variabile studio
text(tree.motori, pretty = 0)

# Vediamo gli errori sull'insieme di test e su quello di training:

tree.pred_test = predict(tree.motori, motori_test, type = "class")
tree.pred_training = predict(tree.motori, motori_train, type = "class")

# Misclassification table sul test set:

tab = table(tree.pred_test,motori_test$High)
tab

# Accuratezza:

(tab[1,1]+tab[2,2])/200 # 200 perchè è la grandezza del test

# Misclassificazione:

(tab[1,2]+tab[2,1])/200

# Sul train

# Misclassification table:

tab = table(tree.pred_training, motori_train$High)

# Accuratezza:

(tab[1,1]+tab[2,2])/200 

# Misclassificazione:

(tab[1,2]+tab[2,1])/200

# Commento dicendo come variano gli errori sul training e sul test (se sono simili oppure no e quindi sono ingannevoli)

# *** PRUNING e CROSS-VALIDAZIONE

cv.motori = cv.tree(tree.motori, FUN = prune.misclass)
cv.motori

# Visualizzo il plot:

plot(cv.motori$size, cv.motori$dev, type = "b")

# oppure

plot(cv.motori) 

# Vediamo l'albero selezionato:

prune.motori = prune.misclass(tree.motori, best=9)
summary(prune.motori)

plot(prune.motori)
text(prune.motori, pretty = 0)

# Prediction su test:

tree.pred = predict(prune.motori, motori_test, type="class")
tree.pred[1:6]

tree.pred.p = predict(prune.motori, motori_test)
head(tree.pred.p)

tab = table(tree.pred, motori_test$High)

# Errore

(tab[1,1]+tab[2,2])/(tab[1,1]+tab[2,2]+tab[2,1]+tab[1,2])

# abbiamo perso pochissimo in termini di performance
# sul test set, ma ridotto di molto la complessità!

##### BAGGING E RANDOM FOREST #####

set.seed(2) # setto il seed

Boston # dataset

train = sample(1:nrow(Boston), nrow(Boston)*2/3)
Boston_train = Boston[train,]
Boston_test =  Boston[-train,]

# Albero di regressione:

tree.boston = tree(medv~., data = Boston, subset = train)
summary(tree.boston)

plot(tree.boston)
text(tree.boston, pretty = 0)

# Plot logico:

tree.boston
# Riporta tutte le condizioni di splitting, Con * si indicano i nodi terminali

# *** BAGGING ***

# Il bagging è una random forest, in cui si tengono sempre tutte le variabili ad ogni albero

dim(Boston)

# mtry = 13 --> numero di variabili da utilizzare nell'albero --> faccio bagging:

bag.boston = randomForest(medv~. , data = Boston_train, mtry = 13, ntree = 500, importance = TRUE)
bag.boston # mi porta la stima MSE in OOB all'ultimo albero

# Cioè:

bag.boston$mse[500]

plot(bag.boston)

importance(bag.boston)

# Se voglio selezionare il parametro mtry tra quelli possibili, in un db di 14 colonne
# facendo ogni volta il set.seed, con una stima oob < 0.55, con 1000 alberi:

oob = rep(0,13)
for (m in 1:13){
  set.seed(1000)
  model = randomForest(variabile_studio~., data = db, ntree = 1000, mtry = m)
  oob[m] = model$mse[1000]
  print(m)
} 
plot(1:13,oob)
abline(h = 0.55) # h è il valore soglia che ci dice il testo
oob[numero] # dove numero è il valore dell'oob che sta sotto la soglia del testo --> lo vedo io quale è dal grafico (numero del primo pallino che sta sotto la linea (che sarebbe la soglia))

# PREDIZIONE sul test set:

yhat.bag = predict(bag.boston,newdata=Boston_test)

# STIME sul test set:

plot(yhat.bag, Boston_test$medv)
abline(0,1)

# MSE di test:

mean((yhat.bag-Boston_test$medv)^2) 

# PARAMETER SELECTION: 
# numero di alberi (ntree) (non è un parametro sensibile di overfitting)

# Proviamo con 1000 alberi
bag.boston = randomForest(medv~., data = Boston, subset = train, mtry = 13, ntree = 1000)
bag.boston

yhat.bag = predict(bag.boston,newdata=Boston_test)
mean((yhat.bag-Boston_test$medv)^2)

# Vedo come cambia l'errore

# Vediamo cosa accade al variare del numero di alberi:

plot(bag.boston)   ## errore OOB, non c'è overfitting se ntree cresce

# L'MSE è praticamente costante da un certo punto in poi.

# *** RANDOM FORESTS ***

# E' esattamente identico, ma cambiamo il parametro mtry, riducendolo

# Proviamo con 6 variabili alla volta:

rf.boston = randomForest(medv~., data = Boston_train, mtry = 6, importance = TRUE, ntree = 1000)  

# Default:
# mtry = p/3 --> REGRESSIONE
# mtry = sqrt(p) --> CLASSIFICAZIONE

rf.boston

# Predizione sul test set:

yhat.rf = predict(rf.boston, newdata = Boston_test)

# Mean Square Prediction Error:

mean((yhat.rf-Boston_test$medv)^2)

plot(rf.boston)

# Vedo come è cambiato l'errore

# importance()
# La funzione prende una random forest e valuta l'importanza delle variabili, 
# in termini di purezza totale dei nodi terminali 
# (per purezza intendiamo la RSS per la regressione e l'indice di Gini per la classificazione)
# e la diminuzione in errore OOB (MSE per regressione e error rate per classificazione).
# In particolare:
# %IncMse: indica di quanto aumenterebbe, in percentuale, l'MSE in OOB se la variabile 
#          in questione non avesse effetto sul modello
# %IncNodePurity: indica quanto decresce l'impurità dei nodi quando si splitta per la variabile 
#                 Gini per classificazione e RSS per regressione

importance(rf.boston)

# Possiamo plottare l'importanza delle variabili:

varImpPlot(rf.boston)

# Selezione dei parametri
# numero di alberi (ntree, non sensibile), numero di variabili (mtry, sensibile di overfitting)

## Troviamo l'errore OOB e di test in corrispondenza di diversi valori di mtry:

oob.err = rep(0,13)
test.err = rep(0,13)

for(mtry in 1:13){
  # Fittiamo il modello con mtry dato
  fit = randomForest(medv~.,data=Boston_train,mtry=mtry,ntree=1000)
  # salvo l'ultimo OOB stimato
  oob.err[mtry] = fit$mse[1000]
  ## Provo a fare il confronto con l'insieme di test
  pred = predict(fit,Boston[-train,])
  test.err[mtry] = mean((Boston_test$medv - pred)^2)
  # vediamo a che punto siamo
  cat(mtry," ")  
}
oob.err
test.err

matplot(1:mtry,cbind(test.err,oob.err),pch=19,col=c('red','blue'),type='b',ylab="Mean Squared Error")
legend('topright',legend=c("Test","OOB"),pch=19,col=c('red','blue'))

# mtry=5 (potrebbe cambiare)
fit.best = randomForest(medv~.,data=Boston,subset=train,mtry=5,ntree=1000)
# predizione e mse
mean((Boston[-train,'medv']-predict(fit,Boston[-train,]))^2)

## Nota: Grazie alla possibilità di fare OOB già nella costruzione della RF, 
## si possono tenere tutti i 500 dati come training e analizzare l'OOB error 
## come stime dell'errore del test, quale ?.
## Dividendo 500 osservazioni tra training e test i risultati potrebbero essere 
## molto sensibili a variazioni (date proprio dalla suddivisione).

oob.err = rep(0,13)
for(mtry in 1:13){
  # Fittiamo il modello con mtry dato
  fit = randomForest(medv~.,data=Boston,mtry=mtry,ntree=1000)
  # salvo l'ultimo OOB stimato
  oob.err[mtry] = fit$mse[1000]
  # vediamo a che punto siamo
  cat(mtry," ")  
}
oob.err

matplot(1:13, oob.err,pch=19, type = "b")

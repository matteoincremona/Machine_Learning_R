library(knitr)
library(corrplot)
library(ggplot2)
library(sjPlot)
library(stargazer)
library(MatchIt)
library(MASS)
library(mvtnorm)
library(ivreg)
library(rdrobust)
library(rdd)
knitr::opts_chunk$set(echo = TRUE, message = FALSE)
set.seed(1) # settare sempre il seed

#  ANALISI ESPLORATIVA - INFORMAZIONI DI UNA VARIABILE DI UN DATASET (ovvero su una colonna)

# Nome dataset = db

str(db) # controllo generale database

# Nome variabile = mele

sd(db$mele) # deviazione standard

class(db$mele) # classe

ggplot(db, aes(y = mele)) + 
  geom_boxplot()             # distribuzione della variabile

ggplot(db, aes(x = reorder(Nazione, - mele, FUN = median), y = mele)) +                  # visualizzo la distribuzione
  geom_boxplot() + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))   # delle mele in base alla variabile "Nazione"

ggplot(db, aes(x = Classe, y = mele, fill = Classe)) +  # visualizzo le mele trattate vs non trattate (in base alla variabile binaria "Classe")
  geom_boxplot()                                        # da qui capisco se i trattati sono diversi dai non trattati --> vedo se le candele sono alla stessa altezza oppure no    

# Bisogna fare diversi ggplot come l'ultimo sopra e capire pre quale variabile del dataset posso comparare i dati
# I due gruppi DEVONO ESSERE SIMILI -> i due plot devono essere quasi uguali


#  ANALISI PRE-MATCHING

# Creo i TRATTATI e i NON TRATTATI

# Nel databes ci sarà una variabile binaria che vale 0 o 1, per esempio la chiamiamo "trattamento"

db_trattati = db[which(db$trattamento == 1), ] # TRATTATI --> variabile binaria "trattamento" == 1

db_non_trattati = db[which(db$trattamento == 0), ] # NON TRATTATI --> variabile binaria "trattamento" == 0

# Faccio il T-TEST (sulla variabile "mele")

t.test(db_trattati$mele, db_non_trattati$mele) # trovo il p-value del t-test

# P-VALUE BASSO (> 0,05) --> rifiuto ipotesi nulla --> i 2 gruppi hanno medie diverse --> i 2 gruppi non sono comparabili per la variabile "mele"
# P-VALUE ALTO (< 0,05) --> i 2 gruppi sono comparabili secondo la variabile "mele"

# Posso ripetere il T-TEST per vedere se i 2 gruppi sono comparabili per le altre variabili del database
# Se vedo che per la maggior parte delle varabili i 2 gruppi non sono comparabili --> I DATI NON SONO RANDOMIZZATI


# NAIVE ESTIMATOR (NE)

naive_estimator = mean(db_trattati$mele) - mean(db_non_trattati$mele) # mi dice se c'è una crescita o una decrescita tra trattati vs non trattati
naive_estimator * 100 # in %

# Normalizzare ogni variabile in base alla sua media e alla sua sd

# ALL'ESAME CHIEDERE SE BISOGNA NORMALIZZARE O NO, ALTRIMENTI VAI A RIGA 70

mele_norm = scale(db$mele)

# ...farlo per tutte le variabili del dataset...

# *** OLS

# Creare la funzione del modello di regressione

# funzione = variabile dipendente ~ variabile trattamento + fattore controllo 1 + fattore controllo 2 + ...

# Chiamiamo la nostra funzione --> "f_ne"

mod_ne = lm(f_ne, data = db)  # Faccio l'OLS
summary(mod_ne)

# L'output mi da una tabella che ha nelle righe le variabili della funzione
# e nelle colonne:

# ESTIMATE --> l'impatto se è positivo o negativo (serebbero i beta)
# STD. ERROR
# T-VALUE
# PR
# *** --> importanza statistica dell'impatto (3 stelline fino a * --> impatto importante)

# Posso calcolare manualmente il BIAS:

# bias = cor(variabile, termine d'errore eps_1)*sd(termine d'errore eps_1)/sd(variabile)
# bias = estimate (coeff. OLS) della varabile - il suo coefficiente del modello (il coefficiente della variabile nella funzione)


# *** PSM - Propensity Score Matching - vado a modificare il database

# La funzione dell'esempio è --> db$trattamento ~ db$GDPpc + db$Accumulo_Capitale + db$Scolarizzazione + db$Crescita_Popolazione

# funzione PSM = variabile trattamento ~ fattore controllo 1 + fattore controllo 2 + ...

# ATTENZIONE --> "trattamento" = variabile binaria

psm_result = matchit(
  db$trattamento ~ db$GDPpc + db$Accumulo_Capitale + db$Scolarizzazione + db$Crescita_Popolazione, 
  method = "nearest", distance = "logit") # il metodo ci viene detto all'esame

db_post_psm = match.data(psm_result, data = db)

# Creo 2 database diversi secondo la variabile binaria che la chiamo per esempio "trattamento"

db_post_psm_trattati  = db_post_psm[which(db_post_psm$trattamento == 1), ]
db_post_psm_non_trattati = db_post_psm[which(db_post_psm$trattamento == 0), ]

# Ripeto i t-test sui 2 nuovi database secondo la variabile "mele"

t.test(db_post_psm_trattati$mele, db_post_psm_non_trattati$mele) # vedo il p-value per capire se i 2 gruppi sono comparabili 

ggplot(db_post_psm, aes(x = Classe, y = mele, fill = Classe)) + 
  geom_boxplot()

# In pratica, stiamo rifacendo lo stesso procedimento di prima, solamente con 2 database modificati dal PSM

# Lo devo fare per tutte le variabili e vedo se per la maggior parte delle covariate se i due gruppi sono simili o no

#  Devo rifare tutti gli stessi procedimenti dalla riga 48-86

# Ricalcolo il nuovo naive estimator post psm, ri-normalizzo, faccio la nuova funzione, ri-applico il modello di regressione

naive_estimator_post_psm = mean(db_post_psm_trattati$mele) - mean(db_post_psm_non_trattati$mele)
naive_estimator_post_psm 

f_post # nuova funzione (dovrebbe esserre uguale a f_ne fatta prima del PSM)

mod_post_psm = lm(f_post, data = db_post_psm)  # Faccio il MODELLO LINEARE (linear model)
summary(mod_post_psm)

# Vedo l'estimate della variabile se è cambiato tra pre e post PSM (vedo se cambia l'estiamte e le stelline ***)

# *** MODELLO IV - INSTRUMENTAL VARIABLE

# Abbiamo una funzione a 2 stadi: funzione OLS senza strumenti | variabili esogene + strumento/i del modello

# Nel secondo stadio NON mettere le variabili endogene (trucchetto esame --> di solito c'è scritto la variabile "mele" è utilizzata come variabile strumentale per la variabile "pere" --> nel secondo stadio devo mettere "mele" e non "pere" in quanto endogena)

# Esempio --> y ~ x_1 + x_2 + x_3 | x_1 + x_2 + z

# x_1 , x_2 --> variabili esogene
# x_3 --> variabile endogena
# z --> strumento (variabile strumentale) (posso averne più di uno)

mod_iv = ivreg(y ~ x_1 + x_2 + x_3 | x_1 + x_2 + z) # scrivi le funzioni per intero dentro la funzione ivreg e non prima
summary(mod_iv, diagnostics = TRUE)

# L'Output ci da gli estimate... e pure i test di diagnostica (diagnostic test):

# Weak-instrument --> ci dice se lo strumento ha un impatto rilevante sulla variabile rilevata endogena / ipotesi nulla che abbia una bassa correlazione con la variabile endogena --> se rifiuto siamo contenti --> vedo se il Weak instrument è statisticamente significativo in base alle stelline --> se lo è --> la variabile strumentale è rilevante, altrimenti no
# Wu-Hausman --> ci dice se entrambi i modelli (OLS e IV) sono consistenti / ipotesi nulla che entrambi i modelli sono consistenti --> rifiuto o accetto in base al p-value --> se rifiuto siamo contenti --> se entrambi i modelli sono consistenti, vado a preferire OLS perchè più efficiente e spicifico --> se è statisticamente significativo, allora rigetto l'ipotesi che OLS è unbiased --> se è statisticamente significativo, IV è preferibile a OLS
# Sargan --> se non me lo stima è perchè il numero di strumenti usato è = al numero di variabili endogene del modello / ipotesi nulla che c'è assenza di over restriction --> sono felice se l'accetto

stargazer(mod_ols, mod_iv, type='text') # confronto dei 2 modelli


# *** RDD - Regression Discontinuity Design

# SHARP RDD

# Una volta che ho il database, ci serve sapere solamente:

y = db$outcome # variabile outcome --> quella che vogliamo studiare
x = db$running # running variable --> variabile rispetto alla quale viene assegnato il trattamento
covs = db$covariata # viene specificata se usarla

rdd_model_sharp = rdrobust(y, x, covs = se non è specificata non usarla, c = 0.75, p = 2, kernel = "triangular") # dove c = cutoff / p = grado del polinomio che approssima la funzione nell'intorno del cutoff / kernel = di default è "triangular", ci verrà specificato all'esame quale usare
summary(rdd_model_sharp)

rdplot(y, x, c = 0.75, p = 2, x.label = "Variabile Assegnazione", y.label = "Nome della variabile outcome")

# Se devo vedere il coeff. di una variabile sempre con questo modello --> CAMBIO e rifaccio lo stesso modello

# L'output ci restituisce come colonne:

# a sinistra --> elementi del db sotto il treshold (cutoff)
# a destra --> elementi del db sopra il treshold

# e come righe:

# numero di elementi dei 2 gruppi
# numero effettivo di osservazioni che viene utilizzato (infatti non utilizza tutti gli elementi del db)

# in fondo all'output poi troviamo:

# Coef. = coefficiente dell'impatto del trattamento tra i 2 gruppi --> ATTENZIONE AL SEGNO: se i trattati sono quelli dell'intorno inferiore e il segno è negativo --> allora l'impatto è positivo (e viceversa)
# P > |z| = ci dice l'importanza statistica dell'effetto --> più è vicino allo 0 e più l'effetto è statisticamente significativo --> se giè è = 0.1, non è statisticamente significativo

#  ** IMPORTANTE: dobbiamo confrontare la P > |z| con il livello di significatività della domanda:
# SE P>|z| è > livello significatività --> NON statisticamente significativo
# SE P>|z| è < livello significatività --> statisticamente significativo

# Si possono fare altri modelli uguali a quello precedente cambiando solamente il p (se prima abbiamo fatto 2, proviamo con p = 3 per esempio)

# Se anche con un p maggiore l'impatto c'è ed è statisticamente importante, il modello è robusto

# Una volta fatto il primo modello, vogliamo accertarci che non ci siano altre differenze date da altre covariate nei trattati e non trattati
# Non vogliamo quindi che altre covariate influiscano sui dati e che quindi il modello ci dia informazioni sbagliate

# Ri-faccio quindi dei nuovi RDD dove vado a cambaire la y
# A rotazione metto la y = alle altre variabili del modello e plotto i grafici

# L'obiettivo è non avere DISCONTINUITA' nei grafici nel cutoff
# La linea rossa deve essere quasi continua (c'è una piccola discontinuità ma non deve essere eccessiva)

# Se ci sono discontinuità --> il modello precedente è poco affidabile in quanto ci sonoa altre variabili che influenzano i dati
# se invece non ci sono discontinuità --> il modello iniziale va bene

# McCrary Test: Devo anche verificare che la DENSITA' delle osservazioni nell'intorno del cutoff NON deve essere statisticamente significativa

DCdensity(x, cutpoint = 0.75) # dove x = variabile running / cutpoint = cutoff

# Ci da come output un grafico e noi vogliamo che non ci sia troppa discontinuità nel cutoff della linea intera (che sarebbe la densità)
# Ci da anche il p-value come output --> p-value basso --> rifiuto l'ipotesi nulla di assenza di discontinuità delle densità
# p- value alto --> accetto l'ipotesi nulla di assenza di discontinuità

classe = factor(db$Classe) # la variabile "Classe" è nel db e ha come valori "trattato" e "non trattato"

# Plottiamo il grafico:

ggplot(data = db, aes(x = nome_variabile_running, y = nome_variabile_outcome)) + geom_point(aes(colour = classe)) + geom_vline(xintercept = 0.75, linetype ="dashed", color = "red")  + theme(axis.line = element_line(colour = "black") , 
                                                                                                                                                                                       panel.grid.major = element_blank(), 
                                                                                                                                                                                       panel.grid.minor = element_blank(), 
                                                                                                                                                                                       panel.border = element_blank())

# Da quest'ultimo grafico notiamo se il modello SHARP è corretto o no da usare perchè il taglio dato dal cutoff deve essere netto:
# Ci sono alcuni 1 a destra del cutoff e alcuni 0 a sinistra del cutoff --> CI SONO DELLE ECCEZIONI (i due gurppi trattati e non trattati non sono perfettamente divisi ma ci sono alcuni trattati dalla parte dei non trattati e viceversa)

# QUANDO CI SONO ECCEZIONI --> Utilizziamo la FUZZY RDD
# Se non ci sono eccezzioni --> va bene la SHARP

# *** FUZZY RDD

# Per la fuzzy ci servono:

y = db$outcome # variabile outcome --> quella che vogliamo studiare
x = db$running # running variable --> variabile rispetto alla quale viene assegnato il trattamento
z = db$strumento # variabile strumentale

rdd_model_fuzzy = rdrobust(y, x, c = 0.75, fuzzy = z, p = 2) # come prima solo che nell'argomento aggiungo z che è la variabile strumento
summary(rdd_model_fuzzy)

# Come per la sharp, rifaccio il modello cambiando la variabile outcome y per vedere se le altre variabili non influenzano il modello

# Traggo le conclusioni guardando gli effetti e l'importanza statistica di quest'ultimi

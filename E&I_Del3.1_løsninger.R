#E&I: Imputering, Del 1 ------------------------------------------------------------------------------------------------
#Datasett er basert p? Tabell 8.3 fra L.Lohr "Sampling Design and Analysis" bok
offer <- data.frame(person = seq(1:20), #person number
                     alder = c(47, 45, 19, 21, -24, 41, 36, 50, 53, 17, 53, 21, 18, 34, 44, 45, 54, -55, 29, 32), #alder av personen
                     kjoenn = c("M", "F", "M", "F", "M", "kvinne", "M", "M", "F", "M", "F", "F", "KVinne", "M", "M", "M", "F", "F", "F", "F"), #M - menn, F - kvinne
                     aar_utdanning = c(16, NA, 20, NA, 12, NA, 20, 12, 13, 10, 12, 12, 11, 16, 14, 11, 14, 10, 12, 10), #antall aar for gjennomgått utdanning
                     kriminalitetsoffer = c(0, NA, 0, 1, 0, 0, 1, 0, 0, NA, 0, 0, 1, 1, 0, 0, 0, 0, NA, 0), #viser om man har v?rt en kriminalitets offer #1 - ja, 0 - nei
                     voldskriminalitetsoffer = c(0, 1, 0, 1, 1, 0, NA, 0, NA, NA, 0, 0, NA, 0, 0, 0, 0, 0, 0, 0)) #viser om man har v?rt en voldskriminalitetsoffer  #1 - ja, 0 - nei
save(offer, file = "offer.RData")
#Task1.1----
#a. Sjekk hvor mange tome(NA) verdier finnes i datasett offer og for hvilke variabler. 
#b. Se om det finnes negative verdier(<0) der de ikke skulle v?re.   
#c. Se om >unifisering< av kategorikal variabel trenges
#d. finnes det uteligger eller verdier som ikke henger sammen i arr_utdanning?
#!tips: Det er mulig å kjøre summary() og plot alder vs aar_utdanning eller validator() og confront() fra pakke validate 
summary(offer)
plot(offer$alder, offer$aar_utdanning)

#Task1.2----
#med bruk av dcmodify pakke spesifiser logger og rett feil som du funnet i Task1.b og c. Sjekk med summary om data er korrekt n?. 
#Der aar_utdanning var st?rre enn alder (fra Task.d) sett NA verdi
#Lag resultater i samme datasett
#install.packages("dcmodify")
library("dcmodify")
m1_2  <- modifier( if (alder  <  0)  alder <- abs(alder),   
                if (toupper(kjoenn) == "KVINNE") kjoenn <- "F",
                if (alder <= aar_utdanning) aar_utdanning <- NA)
offer  <-  modify(offer, m1_2)
summary(offer)
plot(offer$alder, offer$aar_utdanning)


#Task1.3----
#Hvis voldskriminalitetsoffer likt 1, da m? kriminalitetsoffer v?re 1, og hvis kriminalitetsoffer likt 0 da voldskriminalitetsoffer m? v?re 0 og. 
#korrekt feil & fyll inn manglende verdier med bruk av disse deduktive regler
#Lag resultater i samme datasett
head(offer)
m1_3 <- modifier( if (voldskriminalitetsoffer == 1 & (is.na(kriminalitetsoffer)| kriminalitetsoffer == 0) )  kriminalitetsoffer <- 1,   
            if (kriminalitetsoffer == 0 & (is.na(voldskriminalitetsoffer)| voldskriminalitetsoffer == 1) ) voldskriminalitetsoffer <- 0 )
offer  <-  modify(offer, m1_3)

#samme med modify_so
library(magrittr)
offer %<>% modify_so( if (voldskriminalitetsoffer == 1 & (is.na(kriminalitetsoffer)| kriminalitetsoffer == 0) )  kriminalitetsoffer <- 1,   
                if (kriminalitetsoffer == 0 & (is.na(voldskriminalitetsoffer)| voldskriminalitetsoffer == 1) ) voldskriminalitetsoffer <- 0 )

#Task1.4----
#imputer aar_utdaning der mangler verdier. Bruk gjennomsnitt-imputering.
#her kan brukes b?de dcmodify og simputation pakke'
#Lag resultater til en ny datasett: offer_1_4
#med dcmodify
offer %>% modify_so( if (is.na(aar_utdanning))  aar_utdanning <- round(mean(offer$aar_utdanning, na.rm =TRUE)) ) -> offer_1_4

#med simputation
#install.packages("simputation")
library("simputation")
offer %>% impute_proxy(aar_utdanning ~ mean(aar_utdanning, na.rm =TRUE)) -> offer_1_4

#E&I: Imputering, Del 2 ------------------------------------------------------------------------------------------------
#Task2.1----
#Del personer i celler og bruk stratifisert gjennomsnitt-imputering for aar_utdanning. Bruk pakke simputation
#Grupper spesifiseres etter kj?nn og alder <=34 & > 34
#Lag resultater til en ny datasett: offer_2_1
offer$celle <- paste0(offer$kjoenn, sign(offer$alder > 34))
library("simputation")
offer %>% impute_proxy(aar_utdanning ~ mean(aar_utdanning, na.rm =TRUE)|celle) -> offer_2_1

#Task2.2----
#Bruk samme datasett som i ?velser 1, Task 4. Bruk n?rmest nabo hotdeck imputering for variabel kriminalitetsoffer og voldskriminalitetsoffer.
#Bruk alder og kjønn for å beregne avstand
set.seed(1000)
offer %>% impute_knn(aar_utdanning + kriminalitetsoffer + voldskriminalitetsoffer ~ alder + kjoenn, pool = "complete", k = 1) -> offer_2_2

#NO: for ? kontrollere hvem ble bruk som nabo kan oppretes variabel som inneholder person nummer med satt NA verdier der kriminalitetsoffer mangler: 
t <- offer
t$nabo <- t$person
t$nabo[is.na(t$aar_utdanning)|is.na(t$kriminalitetsoffer)|is.na(t$voldskriminalitetsoffer)] <- NA
set.seed(1000)
t %>% impute_knn(aar_utdanning + kriminalitetsoffer + voldskriminalitetsoffer + nabo ~ alder + kjoenn, pool = "complete", k = 1) #-> offer_2_3

#Task2.3----
#Bruk samme datasett som i ?velser 1, Task 4. Bruk Linear regresjon imputering for variabel kriminalitetsoffer og voldskriminalitetsoffer.
offer %>% impute_lm(aar_utdanning + kriminalitetsoffer + voldskriminalitetsoffer ~ alder + kjoenn) -> offer_2_3

#Task2.4----
#Samme som i 2.3, men bruk predictive mean matching(pmm)
#Lag resultater til en ny datasett: offer_2_4
set.seed(1000)
offer %>% impute_pmm(aar_utdanning + kriminalitetsoffer + voldskriminalitetsoffer ~ alder + kjoenn, predictor = impute_lm, pool = "complete") -> offer_2_4

#Task2.5----
#plot resultater not aar utdanning
offer[rule,]
rule <- t$kjoenn == "F"
plot(offer$alder[rule], offer$aar_utdanning[rule])
plot(offer$alder[rule], offer_2_1$aar_utdanning[rule])
plot(offer$alder[rule], offer_2_2$aar_utdanning[rule])
plot(offer$alder[rule], offer_2_3$aar_utdanning[rule])
plot(offer$alder[rule], offer_2_4$aar_utdanning[rule])

#E&I: Imputering, Del 3 ------------------------------------------------------------------------------------------------
#Task1----
#Bruk rÃ offerdata og trenkk endringer i Task1.2, 1.3 og med bruk av lumberjack pakke svar på de neste spørsmålene:
#a.finn prosent personer som fikk misnt 1 editering
#b.finn navn og antall variabler som ble editert
#c.finn prosent editerte i hver variabel fra b.
#d.finn steget der endringen i totalen av aar_utdaning ble den st?rste

#install.packages("lumberjack")
library(lumberjack)
library(dplyr)
load("offer.RData")
offer %>>%
  start_log(cellwise$new(key = "person")) %>>%
  start_log(expression_logger$new(total_utd = sum(aar_utdanning, na.rm = TRUE), count_tom = sum(is.na(aar_utdanning)))) %>>%
  modify(m1_2) %>>%
  modify(m1_3) %>>%
  impute_pmm(aar_utdanning + kriminalitetsoffer + voldskriminalitetsoffer ~ alder + kjoenn, predictor = impute_lm) %>>%
  dump_log("cellwise", file = "offer_3_1_cellwise.csv") %>>%
  dump_log("expression_logger", file = "offer_3_1_expr.csv")
  
offer_3_1_celllwise <- read.csv("offer_3_1_cellwise.csv")
offer_3_1_expr <- read.csv("offer_3_1_expr.csv")

#a.finn prosent personer som fikk misnt 1 editering
length(unique(offer_3_1_celllwise$key)) / nrow(offer) * 100
#b.finn navn og antall variabler som ble editert
unique(offer_3_1_celllwise$variable)
length(unique(offer_3_1_celllwise$variable))
#c.finn prosent editerte i hver variabel fra b.
offer_3_1_celllwise %>% group_by(variable) %>% summarise(n_endringer = n_distinct(key))
#d.finn steget der endringen i totalen av aar_utdaning ble den st?rste
offer_3_1_expr %<>% mutate(diff = total_utd - lag(total_utd)) %>% modify_so(if(is.na(diff)) diff <- 0)
max_steg <- match(max(offer_3_1_expr$diff), offer_3_1_expr$diff, )
offer_3_1_expr[(max_steg-1):max_steg, ]

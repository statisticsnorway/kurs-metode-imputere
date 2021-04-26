#E&I kurs: imputering, 27.04.2021----
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
#Eksempler----
#Load pakker----
#install.packages("dcmodify")
library(dcmodify)
#install.packages("magrittr")
library(magrittr)
#install.packages("simputation")
library(simputation)
#install.packages("lumberjack")
library(lumberjack)
library(dplyr)

#Del1----
#Load data----
data(women)
women$id <- 1:nrow(women)
women

#Introdusere feil----
women$height[10] <- round(women$height[10]*0.0833, 1) #svare i foot istedenor inches
women$height[8] <- -women$height[8] #tastet inn -
women %<>% mutate(bmi = weight/height^2)
women$bmi[c(2,5)] <- NA

#dcmodify----
plot(women$height, women$weight)
summary(women)

hist(women$bmi)
plot(women$height, women$bmi)

#Var1
m <- modifier(if(height < 0) height <- abs(height),
              if(height < 10) height <- round(height/0.0833),
              if(is.na(bmi) | bmi > 1) bmi <-  weight/height^2)
women_out1 <- modify(women, m)

#var2
women %>% modify_so(if(height < 0) height <- abs(height),
                    if(height < 10) height <- round(height/0.0833)) -> women_out1
#Var3
women %>% modify(m) -> women_out1

plot(women_out1$height, women_out1$weight)
plot(women_out1$bmi, women_out1$height)
hist(women_out1$bmi)
summary(women_out1)

#Del 2----
#Load data----
data(women)
women$id <- 1:nrow(women)

#Introdusere feil----
women %<>% mutate(bmi = weight/height^2)
women$bmi[c(2,5,8, 9)] <- NA


#simputation----
plot(women$height, women$bmi)

#gjennomsnitts-imputering
women %>% impute_proxy(bmi ~ mean(bmi, na.rm = TRUE)) -> women_out2_1
plot(women_out2_1$height, women_out2_1$bmi)

#stratifisert gjennomsnits-imputering
women %<>% mutate(strata = floor(women$height/5))
women %>% impute_proxy(bmi ~ mean(bmi, na.rm = TRUE)|strata) -> women_out2_2
plot(women_out2_2$height, women_out2_2$bmi)

#knn
women %>% impute_knn(bmi ~ weight + height, k = 1) -> women_out2_3
plot(women_out2_3$height, women_out2_3$bmi)

#linear regresjon imputering
women %>% impute_lm(bmi ~ weight + height) -> women_out2_4
plot(women_out2_4$height, women_out2_4$bmi)

#pmm
women %>% impute_pmm(bmi ~ weight + height, predictor = impute_lm) -> women_out2_5
plot(women_out2_5$height, women_out2_5$bmi)


#Del3----
#Load data----
data(women)
women$id <- 1:nrow(women)

#Introdusere feil----
women$height[10] <- round(women$height[10]*0.0833, 1) #svare i foot istedenor inches
women$height[8] <- -women$height[8] #tastet inn -
women %<>% mutate(bmi = weight/height^2)
women$bmi[c(2,5)] <- NA

#lumberjack----
women_out3 <- women %>>%
  start_log(simple$new()) %>>%
  start_log(cellwise$new(key = "id")) %>>%
  start_log(expression_logger$new(sum_heigt=sum(height), sd_height=sd(height))) %>>%
  start_log(filedump$new(dir = paste0(getwd(), "/filedump_res"))) %>>%
  modify_so(if(height < 0) height <- abs(height)) %>>%
  modify_so(if(height < 10) height <- round(height/0.0833)) %>>%
  modify_so(if(is.na(bmi) | bmi > 1) bmi <-  weight/height^2) %>>%
  dump_log("simple", file = "simple.csv", stop = FALSE)  %>>%
  dump_log("cellwise")  %>>%
  dump_log("expression_logger") %>>%
  dump_log("filedump") 

d1 <- read.csv("simple.csv")
d1

d2 <- read.csv("._cellwise.csv")
d2
d2 %>% group_by(variable) %>% summarise(c = n_distinct(key), #antall enheter endret
                                       imp_rate_nw = n_distinct(key)/nrow(women)) #imputerings rate (unweighted)
d2 %>% 
  group_by(variable, key) %>% summarise(last_val = last(new)) %>% 
  group_by(variable) %>% summarise(imp_val = sum(last_val)) -> res
res$imp_val[2]/sum(women_out3$height) #imputeringsrate (weighted)

d2 %>% group_by(key) %>% summarise(c = n_distinct(variable)) #hvilken enhet har fått flest endringer
d2 %>% group_by(expression) %>% summarise(c = n()) #hvilken kontroll har funnest flest enheter til å endre

d3 <- read.csv("._expression.csv")
d3
(d3$sum_heigt[nrow(d3)] - d3$sum_heigt[1])/d3$sum_heigt[1] * 100 #% endringer mellom ny og rå data

library(MuMIn)
library(caret)
library(broom)
library(readxl)
library(tidyverse)


##cargamos la base de datos

df<- read_excel("C:/Users/carlo/Documents/github_projects/sanpablotregua_PL_N.A/datos/plantaciones_rauli.xlsx")

##primero vamos a ajustar rauli para el rodal 1 

rodal1_na_train <- read_excel("C:/Users/carlo/Documents/github_projects/sanpablotregua_PL_N.A/datos/plantaciones_rauli.xlsx") %>% 
                   filter(rodal=="Rodal1", !is.na(ht),spp=="NA")

# MODELOS A EVALUAR raulí rodal 1
#lineal simple
fit1<-lm(ht~ dap,data = rodal1_na_train )
fit2<-lm(ht~dap+I(dap^2),data=rodal1_na_train )
fit3<-lm(ht~I(1/dap),data=rodal1_na_train )
fit4<-lm(I(log(ht))~I(log(dap)),data=rodal1_na_train )
fit5<-lm(I(log(ht))~-I(0.5*dap), data=rodal1_na_train )
fit6<-lm(I(log(ht))~I(1/sqrt(dap)),data = rodal1_na_train )

##loop para meter los 6 modelos en un "tibble"

DF<-tibble(Formula=rep(NA,6),Model=rep(NA,6),K=rep(NA,6),R_2_train=rep(NA,6), R_2_test=rep(NA,6), AICc=rep(NA,6))
DF$Formula<-c("ht~dap","ht~dap+I(dap^2)","ht~I(1/dap)","I(log(ht))~I(log(dap))","I(log(ht))~I(0.5*dap)","I(log(ht))~I(1/sqrt(dap))")

for(i in 1:nrow(DF)){
  DF$Model[i]<-lm(as.formula(DF$Formula[i]),data=rodal1_na_train ) %>% list()
  DF$R_2_train[i]<-DF$Model[i][[1]] %>% glance() %>% pull(r.squared)
  DF$K[i]<-DF$Model[i][[1]] %>% glance() %>% pull(df)
  DF$AICc[i]<-DF$Model[i][[1]] %>% AICc
}

# MODELOS A EVALUAR roble rodal 1

rodal1_no_train <- read_excel("C:/Users/carlo/Documents/github_projects/sanpablotregua_PL_N.A/datos/plantaciones_rauli.xlsx") %>% 
  filter(rodal=="Rodal1", !is.na(ht),spp=="NO")

#lineal simple
fit1_no<-lm(ht~ dap,data = rodal1_no_train )
fit2_no<-lm(ht~dap+I(dap^2),data=rodal1_no_train )
fit3_no<-lm(ht~I(1/dap),data=rodal1_no_train )
fit4_no<-lm(I(log(ht))~I(log(dap)),data=rodal1_no_train )
fit5_no<-lm(I(log(ht))~-I(0.5*dap), data=rodal1_no_train )
fit6_no<-lm(I(log(ht))~I(1/sqrt(dap)),data = rodal1_no_train )

##loop para meter los 6 modelos en un "tibble"

DF_no<-tibble(Formula=rep(NA,6),Model=rep(NA,6),K=rep(NA,6),R_2_train=rep(NA,6), R_2_test=rep(NA,6), AICc=rep(NA,6))
DF_no$Formula<-c("ht~dap","ht~dap+I(dap^2)","ht~I(1/dap)","I(log(ht))~I(log(dap))","I(log(ht))~I(0.5*dap)","I(log(ht))~I(1/sqrt(dap))")

for(i in 1:nrow(DF)){
  DF_no$Model[i]<-lm(as.formula(DF_no$Formula[i]),data=rodal1_no_train ) %>% list()
  DF_no$R_2_train[i]<-DF_no$Model[i][[1]] %>% glance() %>% pull(r.squared)
  DF_no$K[i]<-DF_no$Model[i][[1]] %>% glance() %>% pull(df)
  DF_no$AICc[i]<-DF_no$Model[i][[1]] %>% AICc
}

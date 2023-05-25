library(MuMIn)
library(caret)
library(broom)
library(readxl)
library(tidyverse)


##cargamos la base de datos

df<- read_excel("C:/Users/carlo/Documents/github_projects/sanpablotregua_PL_N.A/datos/plantaciones_rauli.xlsx")
##############################################################################---####################################33
##primero vamos a ajustar rauli para el rodal 1 y 2 

na_train <- read_excel("C:/Users/carlo/Documents/github_projects/sanpablotregua_PL_N.A/datos/plantaciones_rauli.xlsx") %>% 
                   filter( !is.na(ht),spp!="NO" & spp!="EMC")

# MODELOS A EVALUAR raulí rodal 1 Y 2
#lineal simple
fit1<-lm(ht~ dap,data = na_train )
fit2<-lm(ht~dap+I(dap^2),data=na_train)
fit3<-lm(ht~I(1/dap),data=na_train )
fit4<-lm(I(log(ht))~I(log(dap)),data=na_train)
fit5<-lm(I(log(ht))~-I(0.5*dap), data=na_train)
fit6<-lm(I(log(ht))~I(1/sqrt(dap)),data = na_train)

##loop para meter los 6 modelos en un "tibble" y seleccionar el mejor

DF<-tibble(Formula=rep(NA,6),Model=rep(NA,6),K=rep(NA,6),R_2_train=rep(NA,6), R_2_test=rep(NA,6), AICc=rep(NA,6))
DF$Formula<-c("ht~dap","ht~dap+I(dap^2)","ht~I(1/dap)","I(log(ht))~I(log(dap))","I(log(ht))~I(0.5*dap)","I(log(ht))~I(1/sqrt(dap))")

for(i in 1:nrow(DF)){
  DF$Model[i]<-lm(as.formula(DF$Formula[i]),data=na_train ) %>% list()
  DF$R_2_train[i]<-DF$Model[i][[1]] %>% glance() %>% pull(r.squared)
  DF$K[i]<-DF$Model[i][[1]] %>% glance() %>% pull(df)
  DF$AICc[i]<-DF$Model[i][[1]] %>% AICc
}
##aqui voy a filtar la base de datos original para todas las especies menos roble y predecir en función del fit2

df_na<-df %>% filter(spp!="NO")
df_na<-df_na %>% mutate(pred=predict(fit2,newdata = df_na))

####################################----------------####################################################################3

# MODELOS A EVALUAR roble rodal 1 y 2

no_train <- read_excel("C:/Users/carlo/Documents/github_projects/sanpablotregua_PL_N.A/datos/plantaciones_rauli.xlsx") %>% 
  filter(!is.na(ht), spp=="NO")

#lineal simple
fit1_no<-lm(ht~ dap,data = no_train )
fit2_no<-lm(ht~dap+I(dap^2),data=no_train )
fit3_no<-lm(ht~I(1/dap),data=no_train )
fit4_no<-lm(I(log(ht))~I(log(dap)),data=no_train )
fit5_no<-lm(I(log(ht))~-I(0.5*dap), data=no_train )
fit6_no<-lm(I(log(ht))~I(1/sqrt(dap)),data = no_train )

##loop para meter los 6 modelos en un "tibble" y selecciónar el mejor

DF_no<-tibble(Formula=rep(NA,6),Model=rep(NA,6),K=rep(NA,6),R_2_train=rep(NA,6), R_2_test=rep(NA,6), AICc=rep(NA,6))
DF_no$Formula<-c("ht~dap","ht~dap+I(dap^2)","ht~I(1/dap)","I(log(ht))~I(log(dap))","I(log(ht))~I(0.5*dap)","I(log(ht))~I(1/sqrt(dap))")

for(i in 1:nrow(DF_no)){
  DF_no$Model[i]<-lm(as.formula(DF_no$Formula[i]),data=no_train ) %>% list()
  DF_no$R_2_train[i]<-DF_no$Model[i][[1]] %>% glance() %>% pull(r.squared)
  DF_no$K[i]<-DF_no$Model[i][[1]] %>% glance() %>% pull(df)
  DF_no$AICc[i]<-DF_no$Model[i][[1]] %>% AICc
}

##ahora voy a filtar por rodal 1 y usar predict para predecir las altura
df_NO<-df %>% filter(spp=="NO")
df_NO<-df_NO %>% mutate(pred= predict(fit2_no,newdata = df_NO))


##UNIR 2 BASES

plantacion_con_alturaas<-rbind(df_na,df_NO)

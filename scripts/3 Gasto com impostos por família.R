library(tidyverse)
gasto_icms_familias <- readRDS("./outputs/gasto_icms_familias.rds")
gasto_ipva_familias <- readRDS("./outputs/gasto_ipva_familias.rds")


gasto_impostos_familias <- gasto_icms_familias %>% 
        full_join(gasto_ipva_familias, by = "FAMILIA") %>% 
  rowwise() %>% 
  mutate(gasto_impostos = sum(gasto_icms,gasto_veiculos_anualizado,na.rm = T))

dados_familias <- readRDS("./outputs/dados_familias.rds")

familias_impostos <- dados_familias %>% 
        left_join(gasto_impostos_familias, by = "FAMILIA") %>% 
        rowwise() %>% 
        mutate(gasto_icms = replace_na(gasto_icms,0),
               gasto_veiculos_anualizado = replace_na(gasto_veiculos_anualizado,0),
               gasto_impostos = replace_na(gasto_impostos,0),
               
              # Renda total dos pagantes dos imposto
               RENDA_TOTAL_ANUAL_IPVA = ifelse(is.na(gasto_veiculos_anualizado),NA,RENDA_TOTAL_ANUAL),
           
               renda_pos_ipva = sum(RENDA_TOTAL_ANUAL, -gasto_veiculos_anualizado,na.rm = T),
               renda_pos_icms = sum(RENDA_TOTAL_ANUAL, -gasto_icms,na.rm = T),
               renda_pos_ipva_icms = sum(renda_pos_ipva, - gasto_icms, na.rm = T )
               )

saveRDS(familias_impostos,"./outputs/dados-familias-c-impostos.rds")

#Removendo os objetos criados
lista = ls()
lista <- lista[lista != "files"]
rm(list = lista)
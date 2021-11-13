library(readxl)
library(dplyr)

tradutor <- read_excel("./Dados/tabelas_de_recursos_e_usos/tradutor_pof2017_scn.xlsx",
                       col_types = c("numeric","text","text","numeric","text"))

tru_consumo_icms <- readRDS("./outputs/aliquotas_icms_tru.rds")

tradutor_icms <- tradutor %>% 
  left_join(tru_consumo_icms %>% 
              select(`Código atividade`,icms_efetivo), by = c("cod_atv_scn"= "Código atividade"))

tradutor_icms <- tradutor_icms %>%
        filter(!is.na(icms_efetivo))

# 4184 produtos da pof dentre os 8629

rm(tradutor,tru_consumo_icms)

saveRDS(tradutor_icms,"./outputs/aliquotas_icms_pof.rds")

#Removendo os objetos criados
lista = ls()
lista <- lista[lista != "files"]
rm(list = lista)


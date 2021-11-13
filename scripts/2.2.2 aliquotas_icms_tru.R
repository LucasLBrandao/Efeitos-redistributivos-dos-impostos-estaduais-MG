library(dplyr)
library(readxl)

# referências https://www.scielo.br/j/rbe/a/RCsXtJNd5CG5s4F4NXSD7Mq/?lang=pt
# 2004 - Carga tributária indireta no brasil, pag 68,69,77, 107
# Agregrar as áreas das contas nacionais de modo a ter alíquotas médias que abarquem mais produtos
# utilizar o tradutor da pof

#Trazendo dados de arrecadação
tru_icms <-  read_xls("./Dados/tabelas_de_recursos_e_usos/68_tab1_2018 - arrecadado em icms.xls", skip = 3)
colnames(tru_icms)[1] <- "COD_PROD_TRU"
tru_icms <- tru_icms %>% select(COD_PROD_TRU,ICMS) %>% filter(COD_PROD_TRU != "Total",
                                                              !is.na(COD_PROD_TRU))

#trazendo dados do consumo das famílias
tru_consumo <-  read_xls("./Dados/tabelas_de_recursos_e_usos/68_tab2_2018 - consumo das famílias.xls", skip = 3,sheet = 2)
colnames(tru_consumo)[c(1,6)] <- c("COD_PROD_TRU","consumo_familias")


tru_icms_consumo <- tru_consumo %>% 
        left_join(tru_icms, by = "COD_PROD_TRU") %>% 
        filter(!COD_PROD_TRU %in% c("Total",NA)) %>% 
        mutate(COD_PROD_TRU = as.numeric(COD_PROD_TRU))

colnames(tru_icms_consumo) <- c("COD_PROD_TRU","produto_SCN","Exportação",
                                "Consumo_governo","Consumo_ISFLSF","consumo_familias",
                                "formacao_bruta_capital_fixo","variacao_de_estoque",
                                "demanda_final","demanda_total","ICMS")


#trazendo tradutor produtos e atividades
tru_atividades <- read_excel("./Dados/tabelas_de_recursos_e_usos/classificacao produtos SCN 2010.xlsx")


tru_atividades_icms <-  tru_icms_consumo %>% 
        left_join(tru_atividades, by = c("COD_PROD_TRU" = "Código")) %>% 
         group_by(`Código atividade`) %>% 
         summarize(atividade_scn = first(`Descrição atividade`),
                   exportação = sum(Exportação,na.rm = T),
                   consumo_governo = sum(Consumo_governo,na.rm = T),
                   consumo_ISFLSF = sum(Consumo_ISFLSF,na.rm = T),
                   consumo_familias = sum(consumo_familias,na.rm = T),
                   formacao_bruta_capital_fixo = sum(formacao_bruta_capital_fixo,na.rm = T),
                   variacao_de_estoque = sum(variacao_de_estoque,na.rm = T),
                   demanda_final = sum(demanda_final,na.rm = T),
                   demanda_total = sum(demanda_total,na.rm = T),
                   ICMS = sum(ICMS, na.rm = T)) %>% 
         ungroup() %>% 
         mutate(perc_familias = consumo_familias/(exportação + consumo_governo + 
                                                 consumo_ISFLSF + consumo_familias),
               perc_demand_total = consumo_familias/demanda_total,
               icms_efetivo = ICMS/consumo_familias) %>% 
                filter(ICMS > 0,
                       consumo_familias > 0,
                       perc_familias > 0.5 | `Código atividade` == 191, 
                        icms_efetivo < 1)


# Selecionado só os produtos em que as famílias representam mais de 40% da demanda final

saveRDS(tru_atividades_icms,"./outputs/aliquotas_icms_tru.rds")

#Removendo os objetos criados
lista = ls()
lista <- lista[lista != "files"]
rm(list = lista)

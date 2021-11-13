library(tidyverse)
library(readxl)

gasto_p_prod_p_familia <- readRDS("./outputs/gasto_por_produto_por_familia.rds")
aliq_icms_pof <- readRDS("./outputs/aliquotas_icms_pof.rds")
produtos_pof <- read_xls("D:/Users/lucas/Desktop/Meus Arquivos/Faculdade/Monografia/Dados/POF/Documentacao_20210423/Cadastro de Produtos.xls")

aliq_icms_pof <- aliq_icms_pof %>% distinct(codigo4, .keep_all = T)

produtos_pof <- produtos_pof %>% 
  mutate(produto_POF4 = trunc(as.numeric(`CÓDIGO DO PRODUTO`)/100)) %>% 
  group_by(produto_POF4) %>% 
  summarize(nome_produto = first(`DESCRIÇÃO DO PRODUTO`)) %>% 
  ungroup()


gasto_icms_p_produto <- gasto_p_prod_p_familia %>% 
  left_join(aliq_icms_pof %>% select(codigo4,desc_atv_scn,icms_efetivo), 
            by = c("produto_POF4" = "codigo4")) %>% 
  left_join(produtos_pof, by = "produto_POF4") %>% 
  group_by(quadro) %>% 
  mutate(icms_efetivo = ifelse(nome_produto == "AGREGADO" & is.na(icms_efetivo),
                                mean(icms_efetivo,na.rm = T),icms_efetivo )) %>%
  ungroup() %>% 
  mutate(gasto_icms = despesa*icms_efetivo)

#Verificando os produtos sem alíquotas
produtos_sem_aliquotas <- gasto_icms_p_produto %>% 
  group_by(produto_POF4) %>% 
  summarise(nome_produto = first(nome_produto),
            despesa = sum(despesa),
            icms_efetivo = first(icms_efetivo)) %>%
  ungroup() %>% 
  mutate(perc = despesa/sum(despesa)) %>% 
  arrange(desc(despesa)) %>% 
  filter(is.na(icms_efetivo)) %>% 
  distinct(produto_POF4, .keep_all = T)



saveRDS(gasto_icms_p_produto,"./outputs/gasto_p_prod_p_familia_icms.rds")

rm(gasto_p_prod_p_familia)

gasto_icms_familias <- gasto_icms_p_produto %>% 
        group_by(FAMILIA) %>% 
        summarise(despesa = sum(despesa,na.rm = T),
                  gasto_icms = sum(gasto_icms, na.rm = T)) %>% 
        ungroup()


saveRDS(gasto_icms_familias,"./outputs/gasto_icms_familias.rds")

#Removendo os objetos criados
lista = ls()
lista <- lista[lista != "files"]
rm(list = lista)


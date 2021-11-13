library(survey)
library(dplyr)
library(readxl)
library(visdat)

# Trazendo produtos do SCN
tradutor <- read_excel("./Dados/tabelas_de_recursos_e_usos/tradutor_pof2017_scn.xlsx",
                       col_types = c("numeric","text","text","numeric","text"))

#Trazendo dados das famílias
dados_familias <- readRDS("./outputs/dados_familias.rds")
gasto_produto_familias <- readRDS("./outputs/gasto_por_produto_por_familia.rds")


#trazendo dados dos produtos
produtos_pof <- read_xls("D:/Users/lucas/Desktop/Meus Arquivos/Faculdade/Monografia/Dados/POF/Documentacao_20210423/Cadastro de Produtos.xls")
produtos_pof <- produtos_pof %>% 
        mutate(`CÓDIGO DO PRODUTO` = as.numeric(`CÓDIGO DO PRODUTO`),
               produto_POF4 = trunc(`CÓDIGO DO PRODUTO`/100)) %>% 
        group_by(produto_POF4) %>% 
        summarize(descricao_pof = first(`DESCRIÇÃO DO PRODUTO`)) %>% 
        ungroup()


gasto_produto_familias <- gasto_produto_familias %>% 
        left_join(produtos_pof, by = "produto_POF4" ) %>% 
        left_join(dados_familias, by = "FAMILIA") %>% 
        left_join(tradutor, by = c("produto_POF4" = "codigo4"))


gasto_produto_familias %>% group_by(desc_atv_scn) %>% 
        summarize(total = sum(despesa*peso)) %>% 
        arrange(desc(total)) %>% 
        ungroup() %>% 
        mutate(perc = total/sum(total))
# Identifiquei que 24% das despesas está fora do Sistema de contas nacionais
# Dessa forma, abaixo filtro pelos produtos fora do SCN para fazer a compatibilização
# Manualmente

gasto_produto_familias %>% 
        ungroup() %>% 
        filter(is.na(desc_atv_scn)) %>% 
        group_by(descricao_pof) %>% 
        summarise(V9001 = first(V9001),
                  produto_POF4 = first(produto_POF4),
                  despesa_total = sum(despesa*peso)) %>% 
        arrange(produto_POF4,desc(despesa_total)) %>%
        select(V9001,descricao_pof) %>% 
        write.csv2("./outputs/produtos_pof_sem_atividade.csv",row.names = F)


# PRECISO ADICIONAR A CATEGORIA DOS PRODUTOS 

gasto_por_scn <- gasto_icms_familias %>% 
        mutate(desc_atv_scn = replace_na(desc_atv_scn,"Fora do SCN")) %>% 
        group_by(classes_renda,produto_scn) %>% 
        summarise(despesa = sum(despesa*peso)) %>% 
        ungroup(desc_atv_scn) %>% 
        mutate(rank = rank(-despesa)) %>% 
        ungroup()
        



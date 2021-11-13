library(survey)
library(srvyr)

# Referência: http://asdfree.com/pesquisa-de-orcamentos-familiares-pof.html

dados_familias_c_impostos <- readRDS("./outputs/dados-familias-c-impostos.rds")

pos_estrato <- read.csv("./Dados/Pos_estratos_totais.csv")


pof_df_mg <- left_join(dados_familias_c_impostos,
                    pos_estrato, by = c("UPA"="COD_UPA.UF.SEQ.DV.")) %>%
        group_by(pos_estrato) %>% 
        mutate(TOTAL_FAMILIAS_REFERENCIA = sum(peso) ) %>% 
        ungroup() %>% 
        filter(uf == 31)


pos_estrato_mg <- pos_estrato %>% 
        filter(uf == 31)

pre_stratified_design_mg <- 
    svydesign(
        id = ~UPA , 
        strata = ~ESTRATO ,
        weights = ~peso ,
        data = pof_df_mg ,
        nest = TRUE
    )


# População de cada pos estrato
population_totals_mg <- 
    data.frame(
        pos_estrato = unique( pof_df_mg$pos_estrato ) , 
        Freq = unique( pof_df_mg$TOTAL_FAMILIAS_REFERENCIA ) 
    )

pof_design_mg <-
    postStratify(
        pre_stratified_design_mg , 
        ~ (pos_estrato) , 
        population_totals_mg
    )


# Calculando o total de famílias
pof_design_mg <-
    transform(
        pof_design_mg ,
        one = 1)

svytotal( ~ one , pof_design_mg )


# Criando os quantis com base nos dados

quantis <- svyquantile(~RENDA_TOTAL_MENSAL_PC, pof_design_mg,seq(0.01,0.99,0.01))

k2 <- round(c(0,quantis$RENDA_TOTAL_MENSAL_PC[,"quantile"],Inf))


decis <- svyquantile(~RENDA_TOTAL_MENSAL_PC, pof_design_mg,seq(0.1,0.9,0.1))

decis_breaks <- round(c(0,decis$RENDA_TOTAL_MENSAL_PC[,"quantile"],Inf))
decis_nomes <- paste0(1:10,"º decil")

familias_impostos_design <- 
  transform(
    pof_design_mg,
    quantis = cut(RENDA_TOTAL_MENSAL_PC,
                  breaks = k2,
                  include.lowest = T,
                  ordered_result = T,
                  labels = 1:100),
    decis = cut(RENDA_TOTAL_MENSAL_PC,
                breaks = decis_breaks,
                include.lowest = T,
                ordered_result = T,
                labels = decis_nomes))

library(srvyr)
pof_impostos_srvyr_design <- as_survey(familias_impostos_design )


saveRDS(pof_impostos_srvyr_design, file = "./outputs/familias_impostos_mg_design.rds")

#Removendo os objetos criados
lista = ls()
lista <- lista[lista != "files"]
rm(list = lista)

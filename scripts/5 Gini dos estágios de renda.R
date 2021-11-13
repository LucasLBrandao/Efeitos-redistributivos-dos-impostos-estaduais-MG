library(survey)
library(tidyverse)
library(srvyr)
library(convey)


moradores <- readRDS("./Dados/Leitura Microdados POF/MORADOR.rds")
pof_impostos_srvyr_design <-readRDS( "./outputs/familias_impostos_mg_design.rds")

comparação_gini_mg <- moradores %>% 
        select(UF,ESTRATO_POF,TIPO_SITUACAO_REG,COD_UPA,NUM_DOM,NUM_UC,V0306,V0430,V0404,V0405,INSTRUCAO,ANOS_ESTUDO,PESO_FINAL) %>% 
        mutate(FAMILIA = paste(COD_UPA,NUM_DOM,NUM_UC),
        V0404  = ifelse(V0404 == 1, "Homem",ifelse(V0404 == 2,"Mulher","")),
        V0405 = case_when(V0405 == 1 ~ "Branca",
                          V0405 %in% c(2,4) ~ "Negra",
                          V0405 == 3 ~ "Amarela",
                          V0405 == 5 ~ "Indígena",
                          V0405 == 9 ~ "Não declarado",
                          TRUE ~ "Vazio"),
        INSTRUCAO_short = case_when(INSTRUCAO <= 2 ~ "Sem intrução",
                                    INSTRUCAO <= 4 ~ "Fundamental completo",
                                    INSTRUCAO <= 6 ~ "Médio completo",
                                    INSTRUCAO == 7 ~ "Ensino Superior completo",
                                    TRUE ~ "Não considerado"),
        INSTRUCAO_short = factor(INSTRUCAO_short, levels = c("Sem intrução",
                                                              "Fundamental completo",
                                                              "Médio completo",
                                                              "Ensino Superior completo")),
         sexo_raca = paste(V0404,V0405)) %>% 
         left_join(pof_impostos_srvyr_design$variables, by = "FAMILIA") %>% 
         filter(UF == 31) %>% 
         ungroup()

comparação_gini_mg <- comparação_gini_mg %>% mutate(RENDA_ANUAL_PC = RENDA_TOTAL_ANUAL/n_moradores,
                                                    renda_pos_ipva_pc = renda_pos_ipva/n_moradores,
                                                    renda_pos_icms_pc = renda_pos_icms/n_moradores,
                                                    renda_pos_ipva_icms_pc = renda_pos_ipva_icms/n_moradores,
                              ICMS_ANUAL_PC = gasto_icms/n_moradores,
                              IPVA_ANUAL_pc = gasto_veiculos_anualizado/n_moradores,
                              IMPOSTOS_ANUAL_PC = gasto_impostos/n_moradores)


pre_stratified_design_mg <- 
    svydesign(
        id = ~UPA , 
        strata = ~ESTRATO ,
        weights = ~peso ,
        data = comparação_gini_mg ,
        nest = TRUE
    )


population_totals <- 
    data.frame(
        pos_estrato = unique( comparação_gini_mg$pos_estrato ) , 
        Freq = unique( comparação_gini_mg$TOTAL_PESSOAS_REFERENCIA ) 
    )

pof_design_moradores_mg <-
    postStratify(
        pre_stratified_design_mg , 
        ~ pos_estrato, 
        population_totals
    )

pof_design_moradores_mg <- transform(pof_design_moradores_mg, one = 1)

saveRDS(pof_design_moradores_mg,"./outputs/moradores_impostos_mg_design.rds")

lista = ls()
lista <- lista[lista != "files"]
rm(list = lista)
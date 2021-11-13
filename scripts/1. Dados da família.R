library(dplyr)
library(ggplot2)


#Trazendo para dentro os microdados de moradores da POF
morador <- readRDS("./Dados/Leitura Microdados POF/MORADOR.rds")

morador <- morador %>%
        # Selecionando as colunas que me interessam na base de moradores
        select(COD_UPA,NUM_DOM,NUM_UC,ESTRATO_POF,PESO_FINAL,V0306,RENDA_TOTAL,
               PC_RENDA_DISP,V0404,V0405,PC_DEDUCAO,INSTRUCAO,PC_RENDA_MONET)

# Criando vetor com os valores salário mínimos utilizados nos intervalos para classificar as rendas
k <- c(rep(954,10)*c(0,2,3,5,6,8,10,15,20,Inf))

# Criando vetor com os números de salário mínimos utilizados nos intervalos para classificar as rendas
ksm <- c(0,2,3,5,6,8,10,15,20,Inf)

carac_familias <- morador %>% 
        # Criando um código da família a partir do código da Unidade primária de amostragem, do número do domicílio
        # e do número da unidade de consumo
        mutate(FAMILIA = paste(COD_UPA,NUM_DOM,NUM_UC)) %>%
        group_by(FAMILIA) %>% 
        summarise(
                
                #Caracterísitcas da amostra
                UPA = first(COD_UPA),
                ESTRATO = first(ESTRATO_POF),
                peso = first(PESO_FINAL),
                
                # Contando todos as linhas que não são empregado doméstico ou parente do empregado doméstico
                n_moradores = sum(!V0306 %in% c(18,19)),
        
                # Renda total
                RENDA_TOTAL_MENSAL_PC = mean(RENDA_TOTAL, na.rm = T)/n_moradores,
                RENDA_TOTAL_ANUAL = mean(RENDA_TOTAL, na.rm = T)*12,
                
                # Renda disponível
                RENDA_DISP_MENSAL_PC = mean(PC_RENDA_DISP, na.rm = T),
                RENDA_DISP_ANUAL = mean(PC_RENDA_DISP, na.rm = T)*n_moradores*12,
                
                # Características gerais
                sexo_ref = max(V0404[V0306 == 01]),
                raça_ref = max(V0405[V0306 == 01]),
                escola_ref = max(INSTRUCAO[V0306 == 01])
        ) %>% 
        
        #Classificando as pessoas a paritr da Renda total mensal
        mutate(classes_renda = cut(RENDA_TOTAL_MENSAL_PC,
                                   breaks = k,
                                   include.lowest = T,
                                   ordered_result = T,
                                   labels = c(paste("Até", ksm[2]),
                                              paste(paste(rep("entre",7),ksm[2:8]),
                                                    paste(rep("e",7),ksm[3:9])),
                                              paste("Mais que", ksm[9])
                                              )
                                   )) %>% 
        
        # Traduzindo o sexo da pessoa de referência
         mutate(sexo_ref = ifelse(sexo_ref == 2,"Mulher",ifelse(sexo_ref == 1,"Homem","")),
                
        # Traduzindo a raça das pessoas de referência
         raça_ref = case_when(raça_ref == 1 & sexo_ref == "Homem"~ "Branco",
                              raça_ref == 1 & sexo_ref == "Mulher"~ "Branca",
                              raça_ref %in% c(2,4) & sexo_ref == "Homem" ~ "Negro",
                              raça_ref %in% c(2,4) & sexo_ref == "Mulher" ~ "Negra",
                              raça_ref == 3 ~ "Amarela",
                              raça_ref == 5 ~ "Indígena",
                              raça_ref == 9 ~ "Sem declaração",
                              TRUE ~ as.character("Sem declaração")),
        
        #Criando variável com o sexo e raça da pessoa de referência
         sexo_raca_ref = paste(sexo_ref,raça_ref)) %>% 
        ungroup()



#Salvando o rds dos dados das famílias
saveRDS(carac_familias,"./outputs/dados_familias.rds")

#Removendo os objetos criados
lista = ls()
lista <- lista[lista != "files"]
rm(list = lista)

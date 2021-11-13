library(dplyr)
library(tidyr)

#Trazendo os microdados de despesa individual, onde estão os gastos com veículos
outros_rendimentos <- readRDS("./Dados/Leitura Microdados POF/OUTROS_RENDIMENTOS.rds")

#Selecionando as colunas de interesse
outros_rendimentos <- outros_rendimentos %>% select(COD_UPA,NUM_DOM,NUM_UC,
                                                    V9001,COD_INFORMANTE,FATOR_ANUALIZACAO,
                                                    V8500_DEFLA,QUADRO,V9011)



rendimentos_anualizados <- outros_rendimentos %>% 
        # Criando a variável de família
        mutate(FAMILIA = paste(COD_UPA,NUM_DOM,NUM_UC),
               
              # Substituindo o valor nulo do número de meses que a despesa foi realizada por 1
               V9011 = replace_na(V9011,1),
              
              # Fazendo o mesmo para o fator de anualização
               FATOR_ANUALIZACAO = replace_na(FATOR_ANUALIZACAO,1),
              
              # Multiplicando o valor deflacionados e os dois fatores de anualização
               rendimento_anualizado = V8500_DEFLA*FATOR_ANUALIZACAO*V9011)
        
        #Agrupando os cálculos a serem feitos por família
 herança_por_familia <-    rendimentos_anualizados %>%   
            #Filtrando por herança e doação
            filter(V9001 %in% c(5502001,5401301)) %>% 
         group_by(FAMILIA,COD_INFORMANTE) %>% 
         summarise(doacao_herança = sum(rendimento_anualizado,na.rm = T)) %>% 
         mutate(desconto_itcd = doacao_herança*0.05) %>% 
         ungroup(COD_INFORMANTE) %>% 
         summarize(doacao_herança = sum(doacao_herança,na.rm = T),
                   desconto_itcd = sum(desconto_itcd,na.rm = T)) %>% 
         ungroup()
         

# Salvando o RDS com os dados de gasto de veículos por famílias
saveRDS(herança_por_familia,"./outputs/gasto_itcd_familias.rds")

#Removendo os objetos criados
lista = ls()
lista <- lista[lista != "files"]
rm(list = lista)

library(dplyr)
library(tidyr)

#Trazendo os microdados de despesa individual, onde estão os gastos com veículos
despesa_individual <- readRDS("./Dados/Leitura Microdados POF/DESPESA_INDIVIDUAL.rds")

#Selecionando as colunas de interesse
despesa_individual <- despesa_individual %>% select(COD_UPA,NUM_DOM,NUM_UC,
                                                    V9011,FATOR_ANUALIZACAO,
                                                    V8000_DEFLA,QUADRO,V9001)



gasto_veiculos <- despesa_individual %>% 
        # Criando a variável de família
        mutate(FAMILIA = paste(COD_UPA,NUM_DOM,NUM_UC),
               
              # Substituindo o valor nulo do número de meses que a despesa foi realizada por 1
               V9011 = replace_na(V9011,1),
              
              # Fazendo o mesmo para o fator de anualização
               FATOR_ANUALIZACAO = replace_na(FATOR_ANUALIZACAO,1),
              
              # Multiplicando o valor deflacionados e os dois fatores de anualização
               gasto_anualizado = V8000_DEFLA*FATOR_ANUALIZACAO*V9011) %>%
        
        #Agrupando os cálculos a serem feitos por família
        group_by(FAMILIA) %>% 
        
        # Calculando o gasto com veículos, mantendo só as despesas do quadro 50 e
        # retirando as despesas que representam gastos que não são direcionados para o estado
       summarize(gasto_veiculos_anualizado = sum(gasto_anualizado[QUADRO == 50 &
                                                                    !V9001 %in% c(5000201,
                                                                                  5000301,
                                                                                  5001001,
                                                                                  5001101,
                                                                                  5001201,
                                                                                  5001202,
                                                                                  5001301,
                                                                                  5001401,
                                                                                  5001501)])) %>% 
        # Desagrupando os dados para futuros cálculos
        ungroup()

# Salvando o RDS com os dados de gasto de veículos por famílias
saveRDS(gasto_veiculos,"./outputs/gasto_ipva_familias.rds")

#Removendo os objetos criados
lista = ls()
lista <- lista[lista != "files"]
rm(list = lista)
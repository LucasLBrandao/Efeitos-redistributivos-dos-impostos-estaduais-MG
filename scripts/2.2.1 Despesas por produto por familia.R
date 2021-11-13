library(tidyverse)

# Lendo as três tabelas que trazem as despesas ----
caderneta_coletiva <- readRDS("./Dados/Leitura Microdados POF/CADERNETA_COLETIVA.rds")
despesa_coletiva <- readRDS("./Dados/Leitura Microdados POF/DESPESA_COLETIVA.rds")
despesa_individual <- readRDS("./Dados/Leitura Microdados POF/DESPESA_INDIVIDUAL.rds")

#Anualizando as três tabelas ----

caderneta_coletiva <- caderneta_coletiva %>% 
                              select(V9002,COD_UPA,NUM_UC,NUM_DOM,
                                     V8000_DEFLA,FATOR_ANUALIZACAO,
                                     V9001,QUADRO)

prod_UC_caderneta_coletiva <- caderneta_coletiva %>%
        # Filtro para despesas apenas referentes à gastos monetários
        filter(V9002 <= 6) %>% 
        # Criando variável das família e anualizando cada gasto
        mutate(FAMILIA = paste(COD_UPA,NUM_DOM,NUM_UC),
               gasto_anualizado = V8000_DEFLA*FATOR_ANUALIZACAO) %>% 
        # Agrupando pela família e pelo produto
        group_by(FAMILIA,V9001) %>% 
        # Calculando o gasto anualizado
        summarize(despesa = sum(gasto_anualizado, na.rm = T),
                  quadro = first(QUADRO)) %>% 
        ungroup()


despesa_coletiva <- despesa_coletiva %>% 
                              select(V9002,COD_UPA,NUM_DOM,NUM_UC,V9011,QUADRO,
                                     V8000_DEFLA,FATOR_ANUALIZACAO,
                                     V9001)

prod_UC_despesa_coletiva <- despesa_coletiva %>%
  # Filtro para despesas apenas referentes à gastos
        filter(V9002 <= 6) %>% 
  
  # Mantendo apenas o número de meses para os quadros especificados no dicionario
  # Caso não esteja presente consideramos como 1
        mutate(V9011 = ifelse(QUADRO  %in% c(10,19),V9011,1),
               
              # Substituindo os valores não disponíveis por 1
               V9011 = replace_na(V9011,1),
              
              # Criando variável com a família
               FAMILIA = paste(COD_UPA,NUM_DOM,NUM_UC),
              
              # Calculando o gasto anualizado com cada produto
               gasto_anualizado = V8000_DEFLA*FATOR_ANUALIZACAO*V9011) %>% 
        # Agrupando os cálculos por família
        group_by(FAMILIA,V9001) %>% 
        
        # Calculando o gasto anualizado por produto e família
        summarise(despesa = sum(gasto_anualizado),
                  quadro = first(QUADRO)) %>% 
        ungroup()

despesa_individual <- despesa_individual %>% 
                                select(V9002,COD_UPA,NUM_DOM,NUM_UC,V9011,QUADRO,
                                       V8000_DEFLA,FATOR_ANUALIZACAO,
                                       V9001)

prod_UC_despesa_individual <- despesa_individual %>%
        
        # Filtrando apenas pelas despesas monetária ou cartão de crédito
        filter(V9002 <= 6) %>% 
        
        # Considerando apenas o número de meses das despesas especificadas no dicionário
        mutate(V9011 = ifelse(QUADRO  %in% c( 44, 47, 48, 49, 50),V9011,1),
               V9011 = replace_na(V9011,1),
               FAMILIA = paste(COD_UPA,NUM_DOM,NUM_UC),
               gasto_anualizado = V8000_DEFLA*FATOR_ANUALIZACAO*V9011) %>% 
        group_by(FAMILIA,V9001) %>% 
        summarise(despesa = sum(gasto_anualizado),
                  quadro = first(QUADRO)) %>% 
        ungroup()



# Unindo as três tabelas de despesas, que agora constam com familia, código do produto e despesa anualizada
prod_uc_despesas <- rbind(prod_UC_caderneta_coletiva,
                          prod_UC_despesa_coletiva,
                          prod_UC_despesa_individual) %>% 
  mutate(`produto_POF4` = floor(V9001/100))




saveRDS(prod_uc_despesas,"./outputs/gasto_por_produto_por_familia.rds")

#Removendo os objetos criados
lista = ls()
lista <- lista[lista != "files"]
rm(list = lista)

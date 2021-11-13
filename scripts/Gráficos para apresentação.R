library(extrafont)
loadfonts(device = "win", quiet = TRUE)
library(dplyr)
library(ggplot2)
library(tidyr)
library(forcats)
library(magrittr)
library(knitr)
library(ggtext)
library(lemon)
library(viridisLite)
library(data.table)

df <- data.frame(renda = c(rep(c("Renda de Mercado","Renda de Mercado Líquida","Renda Disponível","Renda pós fical","Renda Final"),5)),
                 
                 pais = c(rep("Argentina",5),rep("Bolívia",5),
                          rep("Brasil",5),rep("Mexico",5),rep("Peru",5)),
                 gini = c(0.503,0.498,0.465,0.480,0.388,
                          0.477,0.477,0.468,0.475,0.425,
                          0.573,0.562,0.542,0.539,0.459,
                          0.549,0.539,0.532,0.524,0.482,
                          0.504,0.498,0.494,0.489,NA))


# Cores dos grÃ¡ficos -----
GRAY1 <- "#231F20"
GRAY2 <- "#414040"
GRAY3 <- "#555655"
GRAY4 <- "#646369"
GRAY5 <- "#76787B"
GRAY6 <- "#828282"
GRAY7 <- "#929497"
GRAY8 <- "#A6A6A5"
GRAY9 <- "#BFBEBE"
BLUE1 <- "#174A7E"
BLUE2 <- "#4A81BF"
BLUE3 <- "#94B2D7"
BLUE4 <- "#94AFC5"
BLUE5 <- "#22435e"
BLUE6 <- "#95B3D7"
RED1 <- "#C3514E"
RED2 <- "#E6BAB7"
RED3 <- "#800000"
GREEN1 <- "#0C8040"
GREEN2 <- "#9ABB59"
GREEN3 <- "#31859C"
GREEN4 <- "#4BACC6"
GREEN5 <- "#93CDDD"
ORANGE1 <- "#F79747"
ORANGE2 <- "#FAC090"

# Tema dos grÃ¡ficos

theme_swd <- function() {
theme_minimal(base_size = 11) +
    theme(
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      axis.line = element_line(size = .5, color = GRAY9),
      axis.text = element_text(color = GRAY7),
      axis.ticks.x = element_line(size = 0.5, color = GRAY9),
      axis.ticks.y = element_line(size = 0.5, color = GRAY9),
      axis.title = element_text(color = GRAY3),
      axis.title.y = element_text(hjust = 1, margin = margin(0, 6, 0, 15, "pt")),
      axis.title.x = element_text(hjust = 0, margin = margin(6, 0, 15, 0, "pt")),
      plot.subtitle = element_text(color = GRAY4, size= 8, margin = margin(0,0,20,0,"pt")),
      plot.title = element_text(color = GRAY4, size= 12, margin = margin(0,0,5,0,"pt")),
      plot.title.position = "plot", # This aligns the plot title to the very left edge
      plot.caption = element_text(hjust = 0, color = GRAY6),
      plot.caption.position = "plot",
      plot.margin = margin(.3,.3,.2,.2,"cm"),
      strip.text = element_text(color = GRAY7)#,
     # text = element_text(family = "Arial")
     ) 
}



# Gráfico do gini descendo -------------

df %>% mutate(renda_num = as.numeric(factor(renda, levels = c("Renda de Mercado","Renda de Mercado Líquida","Renda Disponível","Renda pós fical","Renda Final")))) %>%  
  ggplot(aes(x = renda_num, y = gini, color = pais))+
  geom_line(size = 1.2)+
  geom_text(aes(label = gini), vjust = -1,
            size = 3)+
  geom_point(size = 2)+
  theme_swd()+
  scale_x_continuous(labels = c("Renda de Mercado","Renda de Mercado Líquida","Renda Disponível","Renda pós fical","Renda Final"))+
  theme(axis.text.x = element_text(size = 7),
        axis.title.x = element_blank())+
  labs(y = "Gini",
       color = "País")+
  scale_color_manual(values = viridis(5))


# Gráficos ginis Brasil -------

gini_wdi <- read.csv("UNdata_Export_20210307_184702911.csv", fileEncoding = "UTF-8") %>% 
        mutate(Year = as.numeric(Year))
gini_wdi_last_year <- gini_wdi %>% 
        drop_na() %>% 
        group_by(Country.or.Area) %>% 
        filter(Year == max(Year,na.rm = T)) %>% 
        mutate(Country.or.Area = case_when(Country.or.Area == "SÃ£o TomÃ© and Principe" ~ "São Tomé and Principe",
                                           Country.or.Area == "CÃ´te d'Ivoire" ~ " Côte d'Ivoire",
                                           TRUE ~ Country.or.Area )) %>%
        mutate(country =   fct_reorder(Country.or.Area,Value)) %>% 
        arrange(desc(Value)) %>% ungroup()

index_wdi <- 1:nrow(gini_wdi_last_year)
pos_br_wdi <- gini_wdi_last_year %>% 
        mutate(index1 = index_wdi) %>% 
        filter(Country.or.Area == "Brazil") %>% select(index1) %>% as.numeric()

load("swiid9_0.rda")

gini_swiid <- swiid_summary
gini_swiid_last_year <- gini_swiid %>% group_by(country) %>% 
        filter(year == max(year, na.rm = T)) %>% 
        mutate(country = fct_reorder(country,gini_disp)) %>% 
        arrange(desc(gini_disp)) %>% 
        ungroup()

index_swiid <- 1:nrow(gini_swiid_last_year)
pos_br_swiid <- gini_swiid_last_year %>% 
        mutate(index1 = index_swiid) %>% 
        filter(country == "Brazil") %>% select(index1) %>% as.numeric()


kable(gini_swiid_last_year %>% 
        head(24) %>% 
        select(country, year, gini_disp), 
      col.names = c("País","Ano","Índice de Gini"),
      caption = "Países com maior Indíce de gini da renda disponível")


gini_swiid_last_year %>% filter(year == 2018) %>%
        mutate(country = fct_reorder(country, gini_disp)) %>% 
        arrange(desc(gini_disp)) %>% 
        head(20) %>% 
        ggplot(aes(x = country, y = gini_disp))+
        geom_col()+
        coord_flip()+
        geom_text(aes(label = gini_disp), hjust = 1.3, color = "white")+
        labs(x = "País",
             y = "Gini da renda disponível (2018)")+
        theme_swd()

gini_swiid %>% filter(country == "Brazil",
                      year >= 1980) %>% 
        ggplot(aes(x = year, y = gini_disp,
                   ymin = gini_disp - gini_disp_se, 
                   ymax = gini_disp+gini_disp_se))+
        geom_line(size = 1.5, colour = "royalblue4")+
        geom_ribbon(alpha = 0.3, fill = "royalblue1")+
        geom_text(aes(label = gini_disp),
                  nudge_y = 0.5)+
        labs(y ="Gini da renda disponível",
             x = "Ano")+
        theme_swd()+
        scale_x_continuous(breaks = seq(1980,2018,2))


# Gráficos estados brasileiros -----
giniestados <- read.csv("gini_estados_2012_2019.csv")

giniestados2019 <- giniestados %>% filter(ano == 2019) %>% 
  mutate(uf = fct_reorder(uf,gini),
         gini = round(gini*100,2))

mediagini2019 <- mean(giniestados2019$gini)
giniestados2019 %>% 
  ggplot(aes(x = uf,y = gini))+
  geom_col(fill = BLUE1)+
  coord_flip()+
  geom_text(aes(label = gini), nudge_y = 3, size = 3,color = BLUE1)+
  gghighlight::gghighlight(uf == "Minas Gerais")+
  theme_swd()+
  labs(x = "Unidade Federativa",y = "Índice de Gini (2019)")+
  geom_hline(yintercept = mediagini2019, size = 0.5)

# Gráfico evolução do gini estados brasileiros ------
giniestados %>% mutate(uf = fct_reorder(uf,regiao.id)) %>% 
        ggplot(aes(x = ano))+
        geom_line(aes(color = uf, y = gini), size = 0.8,alpha = 0.5)+
        #geom_ribbon(aes(ymin = gini - erro,ymax = gini + erro,fill = uf),alpha = 0.2)+
        geom_text(data = subset(giniestados, ano ==2019),
                  aes(x = ano, y = gini+0.005, label = sigla, color = uf),
                  size = 3, 
                  family = "Arial", hjust = 1)+
        facet_wrap(vars(regiao.nome),nrow = 1)+
        theme_swd()+
        theme(legend.position = "none")+
  labs(x = "Ano",
       y = "Índice de Gini")+
  geom_vline(xintercept = 2014, color = GRAY9)


giniestados %>% ungroup() %>% 
  mutate(uf = fct_reorder(uf,regiao.id)) %>% 
        ggplot(aes(x = ano))+
        geom_line(aes(color = uf, y = gini), size = 0.8,alpha = 0.5)+
        #geom_ribbon(aes(ymin = gini - erro,ymax = gini + erro,fill = uf),alpha = 0.2)+
        geom_text(data = subset(giniestados, ano ==2019),
                  aes(x = ano, y = gini+0.005, label = sigla, color = uf),
                  size = 3, hjust = 1)+
        #facet_wrap(vars(regiao.nome),nrow = 1)+
        theme_swd()+
        theme(legend.position = "none")+
  labs(x = "Ano",
       y = "Índice de Gini")+
  geom_vline(xintercept = 2014, color = GRAY9)+
  gghighlight::gghighlight(sigla %in% c("MG","SE","SC"))+
  ylim(0.35,0.61)

# Gráfico regressividade ------
df <- data.frame(tx = c(10:1,rep(5,10),1:10), renda = rep(1:10,3), tipo = c(rep("Regressivo",10),rep("Proporcional",10),rep("Progressivo",10)))

df %>% ggplot(aes(x = renda, y = tx))+
  geom_line(size = 1)+
  facet_wrap(vars(tipo))+
  xlim(1,12)+
  ylim(-3,12)+
  labs(x = "Renda", y = "Imposto pago/renda")+
   theme_swd()+
  theme(axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.background = element_rect(fill = "white", colour = "grey50"))

# Gráfico sistema tributário brasil ------
df <- fread("Evolução dos impostos no Brasil.csv")

grafico <- function(governo = "NES", ano = 2008, variavel = "TAXPER"){
        
df1 <- df %>% #select(Country,TAX,`Tax revenue`,VAR,Indicator,YEA,Value) %>% 
        mutate(TAX = as.numeric(TAX)) %>% filter(TAX %% 1000 == 0,
                                                 VAR == variavel,
                                                 GOV == governo,
                                                 YEA >= ano) 
labels_impostos <- unique(df1$`Tax revenue`)

df2 <- df1 %>% mutate(`Tax revenue` = fct_reorder(`Tax revenue`,Value),
               TAX = fct_reorder(as.character(TAX), Value),
               imposto = case_when(`Tax revenue` == labels_impostos[3] ~ "Imposto sobre folha de pagamento",
                                   `Tax revenue` == labels_impostos[6] ~ "Outros impostos",
                                   `Tax revenue` == labels_impostos[4] ~ "Impostos sobre propriedade",
                                   `Tax revenue` == labels_impostos[1] ~ "Impostos sobre renda, lucro e ganhos de capital",
                                   `Tax revenue` == labels_impostos[2] ~ "Contribuições Sociais",
                                   `Tax revenue` == labels_impostos[5] ~ "Impostos sobre bens e serviços"))
        

        
df2 %>% ggplot(aes(x = YEA,y = Value, fill = `Tax revenue`))+
        geom_col(color = "white")+
        geom_text(aes(label = ifelse(Value >= 5,round(Value,1),"")),
                  position = position_stack(vjust = 0.9),
                  size = 2.5,
                  colour = "white")+
        theme_swd()+
        scale_fill_manual(values = c(GRAY9,GRAY8,GRAY7,GRAY6,GRAY5,BLUE1),
                          label = c("Imposto sobre folha de pagamento",
                                    "Outros impostos",
                                    "Impostos sobre propriedade",
                                    "Impostos sobre renda, lucro e ganhos de capital",
                                    "Contribuições Sociais",
                                    "Impostos em bens e serviços"))+
        scale_color_manual(values = c(GRAY9,GRAY8,GRAY7,GRAY6,GRAY5,BLUE1))+
        labs(x = "Ano",
             y = "Porcentagem do total(%)")+
        scale_x_continuous(breaks = seq(2008,2018,2))+
        theme(legend.text = element_text(size = 5),
              legend.title = element_blank())+
        theme(legend.position = "none",
              plot.margin = unit(c(1,13,1,1),"lines"))+
        coord_cartesian(xlim = c(2008,2018),clip = 'off')+
        geom_text(data = df2 %>% filter(YEA == 2018),
                  aes(x = 2018.5,label = (imposto), color = `Tax revenue` ),
                  hjust = -0.01,
                  check_overlap = T,
                  size = 2.5,
                  position = position_stack(vjust = 0.5))

}

grafico(ano = 2008, governo = "NES",variavel = "TAXPER")

# Gráfico sistema tributário dos estados -----
# Trazendo a base pra dentro -----
dfcompleto <- read.csv("Receitas orçamentarias 2019.csv")


# Tratanto a base --------------
dfimposto <- dfcompleto %>% 
  filter(Coluna != "") %>% 
  mutate(contanum = paste(conta.1,conta.2,conta.3,conta.4,conta.5,conta.6, sep = "")) %>% 
  select(UF,Coluna,Conta,contanum,conta.7,Valor) %>% 
  filter(Conta == "RECEITAS (EXCETO INTRA-ORÇAMENTÁRIAS) (I)"|
          (contanum %in% c("111812","111813","111821")),
         Coluna == "Receitas Brutas Realizadas") %>% 
  group_by(UF) %>% 
  mutate(perc_total = Valor/Valor[Conta == "RECEITAS (EXCETO INTRA-ORÇAMENTÁRIAS) (I)"] ,
         inverso_perc_total = perc_total)
  
impostos <- unique(dfimposto$conta.7)
impostossigla <- c("IPVA",
                   "ITCD",
                   "ICMS")

dfimposto_plot <- dfimposto %>% 
  filter(Conta != "RECEITAS (EXCETO INTRA-ORÇAMENTÁRIAS) (I)") %>% 
  mutate(imposto = case_when(conta.7 == impostos[2] ~ impostossigla[1],
                             conta.7 == impostos[3] ~ impostossigla[2],
                             conta.7 == impostos[4] ~ impostossigla[3])) %>% 
  mutate(imposto =factor(imposto, levels = c("ITCD","IPVA","ICMS")))

dficms <- with(dfimposto_plot[dfimposto_plot$imposto == "ICMS", ], 
  reorder((UF),perc_total,mean))


# Criando o gráfico --------

color_scale <- c("ICMS" = BLUE5,
  "IPVA" = GRAY5,
  "ITCD" = GRAY8)


formatted_subtitle <- paste0(
  "<span style='color:", color_scale[1], "'>**", names(color_scale)[1], "**</span>",
  " | ",
  "<span style='color:", color_scale[2], "'>**", names(color_scale)[2], "**</span>",
  " | ",
  "<span style='color:", color_scale[3], "'>**", names(color_scale)[3], "**</span>",
  " | "
  )


dfimposto_plot %>% mutate(UF = factor(UF,levels = levels(dficms))) %>% 
  ggplot(aes(y = UF, x = perc_total, fill = imposto ))+
  geom_bar(stat  = "identity",color = "white")+
  scale_fill_manual(values = color_scale, guide = F)+
  labs(subtitle = formatted_subtitle,
       x = "Porcentagem da receita total")+
  geom_text(aes(label = ifelse(perc_total < 0.03 ,"",scales::percent(perc_total,0.1))), 
            position = position_stack(vjust = 0.5),
            size = 2.5,
            color = "white")+
  #coord_flip()+
  theme_swd()+
  theme(legend.position = "none",
        panel.border = element_blank(),
        plot.subtitle = element_markdown(hjust = 0.17, size = 10,margin = margin(0,0,5,0,"pt")),
        axis.title.x = element_text(hjust = 0.06, color = GRAY6, size = 9))+
  guides(fill = guide_legend(reverse = TRUE))+
  scale_x_continuous(position = "top",labels = scales::percent, limits = c(0,1))+
  coord_capped_cart(top = "both")

dfcompleto2018 <- read.csv("Receitas orçamentarias ufs 2018.csv")


# Tratanto a base --------------
dfimposto2018 <- dfcompleto2018 %>% 
  filter(Coluna != "") %>% 
  mutate(contanum = paste(conta.1,conta.2,conta.3,conta.4,conta.5,conta.6, sep = "")) %>% 
  select(UF,Coluna,Conta,contanum,conta.7,Valor) %>% 
  filter(Conta == "Total Receitas"|
          (contanum %in% c("111812","111813","111821")),
         Coluna == "Receitas Brutas Realizadas") %>% 
  group_by(UF) %>% 
  mutate(perc_total = Valor/Valor[Conta == "Total Receitas"])
  
impostos <- unique(dfimposto2018$conta.7)
impostossigla <- c("IPVA",
                   "ITCD",
                   "ICMS")

dfimposto_plot2018 <- dfimposto2018 %>% 
  filter(Conta != "Total Receitas") %>% 
  mutate(imposto = case_when(conta.7 == impostos[2] ~ impostossigla[1],
                             conta.7 == impostos[3] ~ impostossigla[2],
                             conta.7 == impostos[4] ~ impostossigla[3])) %>% 
  mutate(imposto =factor(imposto, levels = c("ITCD","IPVA","ICMS")))


pibuf <- read.csv("PIB estados 2002-2018.csv")

pibuf_2018 <- pibuf %>% filter(Ano == 2018) %>% select(sigla, PIB)

df_plot_2018_pib <- dfimposto_plot2018 %>%  
  left_join(pibuf_2018, by = c("UF" = "sigla")) %>% 
  mutate(perc_pib = Valor/PIB)

brasil_pib_icms <- data.frame(UF = "BR",df_plot_2018_pib %>% group_by(imposto) %>% summarise(Valor = sum(Valor)), PIB = rep(7004141*10^6,3)) %>% mutate(perc_pib = Valor/PIB)

df_plot_2018_pib_br <- df_plot_2018_pib %>% select(UF,imposto,Valor,PIB,perc_pib) %>% 
  rbind(brasil_pib_icms)

dficms2018_pib <- with(df_plot_2018_pib_br[df_plot_2018_pib_br$imposto == "ICMS", ], 
  reorder((UF),perc_pib,mean))

df_plot_2018_pib_br %>% mutate(UF = factor(UF,levels = levels(dficms2018_pib))) %>% 
  ggplot(aes(y = UF, x = perc_pib, fill = imposto ))+
  geom_bar(stat  = "identity",color = "white")+
  scale_fill_manual(values = color_scale, guide = F)+
  labs(subtitle = formatted_subtitle,
       x = "Porcentagem do PIB")+
  geom_text(aes(label = ifelse(perc_pib < 0.005 ,"",scales::percent(perc_pib,0.1))), 
            position = position_stack(vjust = 0.5),
            size = 2.5,
            color = "white")+
  #coord_flip()+
  theme_swd()+
  theme(legend.position = "none",
        panel.border = element_blank(),
        plot.subtitle = element_markdown(hjust = 0.17, size = 10,margin = margin(0,0,5,0,"pt")),
        axis.title.x = element_text(hjust = 0.06, color = GRAY6, size = 9))+
  guides(fill = guide_legend(reverse = TRUE))+
  scale_x_continuous(position = "top",labels = scales::percent, limits = c(0,0.2))+
  coord_capped_cart(top = "both")

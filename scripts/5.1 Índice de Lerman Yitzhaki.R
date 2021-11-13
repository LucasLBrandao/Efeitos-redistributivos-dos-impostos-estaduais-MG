library(IC2)
library("rineq")
library(convey)

pof_design_moradores_mg <- readRDS("./outputs/moradores_impostos_mg_design.rds")

pof_design_moradores_prep <- convey_prep(pof_design_moradores_mg)

gini_renda_total <- coef( svygini( ~ RENDA_ANUAL_PC, pof_design_moradores_prep ) ) * 100

gini_pos_ipva <- coef( svygini( ~ renda_pos_ipva_pc, pof_design_moradores_prep )) * 100

gini_pos_icms <- coef( svygini( ~ renda_pos_icms_pc, pof_design_moradores_prep )) *100

gasto_icms_pc <- pof_design_moradores_mg$variables$ICMS_ANUAL_PC
renda_pos_icms_pc <- pof_design_moradores_mg$variables$renda_pos_icms_pc
gasto_ipva_pc <- pof_design_moradores_mg$variables$IPVA_ANUAL_pc
renda_pos_ipva_pc <- pof_design_moradores_mg$variables$renda_pos_ipva_pc
renda_pc <- pof_design_moradores_mg$variables$RENDA_ANUAL_PC
pesos <- pof_design_moradores_mg$variables$peso

renda_pos_icms_pc <- ifelse(renda_pos_icms_pc < 0, 0,renda_pos_icms_pc)

curveConcent(y = renda_pos_icms_pc, x = gasto_icms_pc, w = pesos, col = "red",
             xlab = "Ranking pela renda pós ICMS")
curveConcent(y = renda_pos_ipva_pc, x = renda_pos_ipva_pc, w = pesos, col = "black",add = TRUE)
curveConcent(y = renda_pos_icms_pc, x = renda_pos_icms_pc, w = pesos, col = "grey",add = TRUE)
curveConcent(y = renda_pos_ipva_pc, x = gasto_ipva_pc, w = pesos, col = "green",add = TRUE)
legend("topleft", c("Renda pós ICMS","Renda pós IPVA","ICMS","IPVA"), pch=c(16), col=c("grey","black","red","green"),
       bty = "n")


con_index_icms <- ci(gasto_icms_pc, renda_pos_icms_pc, wt=pesos, type = c("CI"))
con_index_ipva <- ci(gasto_ipva_pc, renda_pos_ipva_pc, wt=pesos, type = c("CI"))

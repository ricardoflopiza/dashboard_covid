#saveRDS(datos_nacional, "objetos/datos_nacional.rds")
#saveRDS(fig_1_ind_gest_nacional, "objetos/fig_1_ind_gest_nacional.rds")


### TAB 1 ####
# 1
datos_nacional <- readRDS("objetos/datos_nacional.rds")
# 2
fig_1_ind_gest_nacional <- readRDS("objetos/fig_1_ind_gest_nacional.rds")
# 3
plot_completadas_dia <- readRDS("objetos/plot_completadas_dia.rds")
# 4
plot_iniciadas_dia <- readRDS("objetos/plot_iniciadas_dia.rds")
# 5
datos_region <- readRDS("objetos/datos_region.rds")
# 6
fig_2_ind_gest_regional <- readRDS("objetos/fig_2_ind_gest_regional.rds")
# 7
datos_comuna <- readRDS("objetos/datos_comuna.rds") 


### TAB 2 ####
# 8
tasas_nacional <- readRDS("objetos/tasas_nacional.rds")
# 9
fig_3_resultado_op_nacional <- readRDS("objetos/fig_3_resultado_op_nacional.rds")
# 10
fig_4_resultado_acum_nacional <- readRDS("objetos/fig_3_resultado_acum_nacional.rds")

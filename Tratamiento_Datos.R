library(foreign)
library(dplyr)
library(purrr)
library(ggplot2)
library(openxlsx)
enemdu <- read.spss("enemdu_persona_2021_04.sav", to.data.frame = TRUE)
# ver el tipo de variables
str(enemdu)

# seleccionar las variables mas relevantes
nmdu <- enemdu %>% select(c(area, p01, p02, p03, p04, p05a, p06, p07, p10a, p11, p12a, p15, 
                            p20, p21, p23, p231, p24, p36, p42, p43, p45, p50, p51a, p61b1,
                           p71b, p72b, p73b, p74b, p75, p76, p77, p78, ingrl, sd03, condact, 
                           empleo, desempleo, secemp, grupo1, rama1, nnivins)) 

## filtrar por los jefes de hogar
nmdujh <- nmdu %>%  filter(p01=="01")
str(nmdujh)

#### procesamiento-----
# transofrmar a numericas las variables que corresponden
unique(nmdujh$p51a)
table(nmdujh$p71b)
nmdujh$p24 <- as.numeric(nmdujh$p24)
nmdujh$p45 <- as.numeric(nmdujh$p45)
nmdujh$p51a <- as.numeric(nmdujh$p51a)

nmdujh$p71b[nmdujh$p71b == 999999] <- NA 
nmdujh$p72b[nmdujh$p72b == 999999] <- NA 
nmdujh$p73b[nmdujh$p73b == 999999] <- NA 
nmdujh$p76 <- as.character(nmdujh$p76)
nmdujh$p76 <- as.numeric(nmdujh$p76)
nmdujh$p78 <- as.character(nmdujh$p78)
nmdujh$p78 <- as.numeric(nmdujh$p78) 


### recategorizar la edad en grupos----
table(nmdujh$p03)
nmdujh$p03 <- as.character(nmdujh$p03)
nmdujh$p03[nmdujh$p03 == "98 y más"] <- NA 
nmdujh$p03 <- as.numeric(nmdujh$p03)
class(nmdujh$p03)
range(nmdujh$p03)

nmdujh$p03_rec <- cut(nmdujh$p03, breaks = c(15, 24, 39, 64, 97), labels = c("18-24", "25-39", "40-64", "65 y más"))

### ingreso laboral 
# transformarla a numerica y eliminar valores centinelas 999 , no informa, 
table(nmdujh$ingrl)
table(nmdujh$ingrl_rec)

nmdujh$ingrl_rec <- as.character(nmdujh$ingrl)
nmdujh$ingrl_rec[nmdujh$ingrl == "No informa"] <- NA 
nmdujh$ingrl_rec[nmdujh$ingrl == "999"] <- NA 
nmdujh$ingrl_rec[nmdujh$ingrl == "Gasta mas de lo que gana"] <- NA 
nmdujh$ingrl_rec <- as.numeric(nmdujh$ingrl_rec)

#### 
#### ingreso total sumar los distintos ingresos---- 
#nmdujh$ingrl_tot <- nmdujh$ingrl_rec + nmdujh$p71b + nmdujh$p72b + nmdujh$p73b + nmdujh$p74b + nmdujh$p76 + nmdujh$p78 
#nmdujh$ingrl_tot <-  colSums(nmdujh[ ,c(43, 25:28, 30, 32)], na.rm = F)

###  renombrar las variables


ene <- read.csv("enemdu_proc2.csv", sep=";", encoding = "UTF-8")
summary(ene$ingrl_tot)

ene$ingrl_tot[ene$ingrl_tot == 0] <- NA 

names(ene) <- c("area", "persona", "Sexo", "Edad", "RelacionParentezco", "Seguro_social",
               "Estado_civil", "Asiste_clases", "Nivelinstruccion", "Sabe_leer_escribir",
               "Obtuvo_titulo", "Autoidentificacion", "Trabja", "Actividad_hogar", "Razon_notrabajo", 
               "Razon_covid", "Horas_trabajo_semanal", "Condicion_inactividad", "Categoria_ocupacion",
               "Tipo_trabajo", "Años_trabajo", "Numero_trabajo", "Horas_trabajo_principal", 
               "Forma_Seguridad_social", "Ingreso_transacciones_capital", "Ingresos_jubilacion", 
               "Donaciones", "Remesas", "Recibe_Bono_DDHH", "Monto_Bono_DDHH", "Recibe_Bono_Discapacidad",
               "Monto_Bono_Discapacidad", "IngresoLaboral", "Principal_motivo_desempleo", "Condicion_actividad", 
               "Empleo", "Desempleo", "Sectores_empleados", "Grupo_ocupacion", "Rama_actividad", 
               "Nivel_instruccion", "Categorias_edad", "Ingreso_laboral", "Ingresos_totales")

# guardar los datos para usar en tableau

#write.csv(ene, "enemdu_procesada_n.csv")
#write.xlsx(ene, "enemdu_proc_n.xlsx")
#sacar variables no relevantes
ene2 <- ene %>% select(-c(2, 5, 15, 16 ))

### graficos basicos
#histogramas y #graficos de barras

grafica_por_tipo <- function(datos, var) {
  
  
  vector_var <- datos[ ,var]
  
  if (is.factor(vector_var)) {
    g <- datos %>% filter(!is.na(var)) %>% 
      ggplot(aes_string(x = var, fill= var)) + 
      geom_bar()+coord_flip()
    
  }
  
  else {
    
    g <-  datos %>%  filter(!is.na(var)) %>% 
      ggplot(aes_string(x = var, fill = var)) + 
      geom_histogram(color= "deeppink", fill = "cyan", bins = 30) 

    
  }
  
  g + labs( fill = "", title = var) + # etiquetas de ejes y titulo
    theme_bw() +   theme(plot.title = element_text(size = 9), 
          axis.text.y = element_text(size = 6), legend.position = "None")
}

## pasar a double algunas variables que estaban como integer
ene2[c(3, 13, 17, 19, 21:24, 26, 28, 39, 40)] <- map(ene2[c(3, 13, 17, 19, 21:24, 26, 28, 39, 40)], ~as.numeric(.x))

variables <- colnames(ene2) ## obtener los nombres de las variables

##aplicar la funcion de graficos para los graficos
lista_graficos <- map(variables, ~grafica_por_tipo(ene2, .x))
lista_graficos



library(ggplot2)
library(dplyr)

source('C:/Users/Public/Documents/R/CONEXIONES/CONEXION LocalHost.r')
source('C:/Users/Public/Documents/DESEMPLEO/SINIESTROS/R/Cargue siniestros con estado.R')

sin<-read.csv2('C:/Users/Public/Documents/INDEMNIZACIONES/Siniestros con estado.csv',sep = ',')
par(mfrow=c(2,3))

# PERMANENCIA
hist(subset(sin, permanencia>0 & ano == 2016 & numero_moviento == 1)$permanencia,breaks = c(0,200,400,600,800,1000,1200))
hist(subset(sin, permanencia>0 & ano == 2017 & numero_moviento == 1)$permanencia,breaks = c(0,200,400,600,800,1000,1200,1400))
hist(subset(sin, permanencia>0 & ano == 2018 & numero_moviento == 1)$permanencia,breaks = c(0,200,400,600,800,1000,1200,1400,1600,1800))

quantile(subset(sin, permanencia>0 & ano == 2016 & numero_moviento == 1)$permanencia)
quantile(subset(sin, permanencia>0 & ano == 2017 & numero_moviento == 1)$permanencia)
quantile(subset(sin, permanencia>0 & ano == 2018 & numero_moviento == 1)$permanencia)

ass<-subset(sin, permanencia>0 & ano >= 2016 & numero_moviento == 1)[,c('ano','permanencia')]
ggplot(ass, aes(x = as.character(ass$ano), y = permanencia, fill=ano)) + geom_boxplot()


# AVISO
hist(subset(sin, t_aviso>0 & ano == 2016 & numero_moviento == 1)$t_aviso)
hist(subset(sin, t_aviso>0 & ano == 2017 & numero_moviento == 1)$t_aviso)
hist(subset(sin, t_aviso>0 & ano == 2018 & numero_moviento == 1)$t_aviso)

quantile(subset(sin, t_aviso>0 & ano == 2016 & numero_moviento == 1)$t_aviso)
quantile(subset(sin, t_aviso>0 & ano == 2017 & numero_moviento == 1)$t_aviso)
quantile(subset(sin, t_aviso>0 & ano == 2018 & numero_moviento == 1)$t_aviso)

ass<-subset(sin, t_aviso>0 & ano >= 2016 & numero_moviento == 1 & t_aviso<=365)[,c('ano','t_aviso')]
ggplot(ass, aes(x = as.character(ass$ano), y = t_aviso, fill=ano)) + geom_boxplot()

# DEFINICION PAGO

hist(subset(sin, t_def>0 & ano == 2016 & ESTADO.RVA5=='PAGO' & numero_moviento == 1)$t_def)
hist(subset(sin, t_def>0 & ano == 2017 & ESTADO.RVA5=='PAGO' & numero_moviento == 1)$t_def)
hist(subset(sin, t_def>0 & ano == 2018 & ESTADO.RVA5=='PAGO' & numero_moviento == 1)$t_def)

quantile(subset(sin, t_def>0 & ano == 2016 & ESTADO.RVA5=='PAGO' & numero_moviento == 1)$t_def)
quantile(subset(sin, t_def>0 & ano == 2017 & ESTADO.RVA5=='PAGO' & numero_moviento == 1)$t_def)
quantile(subset(sin, t_def>0 & ano == 2018 & ESTADO.RVA5=='PAGO' & numero_moviento == 1)$t_def)

quantile(subset(sin, t_def>0 & ano == 2017 & ESTADO.RVA5=='PAGO' & numero_moviento == 1 & t_def<=120)$t_def, probs = c(0,.10,.25,.50,.75,.90,.95,.99))


boxplot(subset(sin, t_def>0 & ano == 2016 & ESTADO.RVA5=='PAGO' & numero_moviento == 1 & t_def<=730)$t_def)
boxplot(subset(sin, t_def>0 & ano == 2017 & ESTADO.RVA5=='PAGO' & numero_moviento == 1 & t_def<=730)$t_def)
boxplot(subset(sin, t_def>0 & ano == 2018 & ESTADO.RVA5=='PAGO' & numero_moviento == 1 & t_def<=120)$t_def)


ass<-subset(sin, t_def>0 & ano >= 2016 & ESTADO.RVA5=='PAGO' & numero_moviento == 1 & t_def<=180)[,c('ano','t_def')]
ggplot(ass, aes(x = as.character(ass$ano), y = t_def, fill=ano)) + geom_boxplot()



# DEFINICION OBJ

hist(subset(sin, t_def>0 & ano == 2016 & ESTADO.RVA5!='PAGO'& ESTADO.RVA5!='PENDIENTE' & numero_moviento == 1)$t_def)
hist(subset(sin, t_def>0 & ano == 2017 & ESTADO.RVA5!='PAGO'& ESTADO.RVA5!='PENDIENTE' & numero_moviento == 1)$t_def)
hist(subset(sin, t_def>0 & ano == 2018 & ESTADO.RVA5=='PAGO'& ESTADO.RVA5!='PENDIENTE' & numero_moviento == 1)$t_def)

quantile(subset(sin, t_def>0 & ano == 2016 & ESTADO.RVA5!='PAGO'& ESTADO.RVA5!='PENDIENTE' & numero_moviento == 1)$t_def)
quantile(subset(sin, t_def>0 & ano == 2017 & ESTADO.RVA5!='PAGO'& ESTADO.RVA5!='PENDIENTE' & numero_moviento == 1)$t_def)

hist(subset(sin, t_def>0 & t_def<=90 & ano == 2018 & ESTADO.RVA5!='PAGO'& ESTADO.RVA5!='PENDIENTE' & numero_moviento == 1)$t_def)
quantile(probs = c(0,.10,.20,.30,.40,.5,.6,.7,.8,.9,.99),subset(sin, t_def>0 & t_def<=900 & ano == 2018 & ESTADO.RVA5!='PAGO'& ESTADO.RVA5!='PENDIENTE' & numero_moviento == 1,)$t_def)

quantile(subset(sin, t_def>0 & t_def<=900 & ano == 2018 & ESTADO.RVA5!='PAGO'& ESTADO.RVA5!='PENDIENTE' & numero_moviento == 1,)$t_def)


ass<-subset(sin, t_def>0 & ano >= 2016 & ESTADO.RVA5!='PAGO' & ESTADO.RVA5!='PENDIENTE' & numero_moviento == 1)[,c('ano','t_def')]
ggplot(ass, aes(x = as.character(ass$ano), y = t_def, fill=ano)) + geom_boxplot()


#######################################################################################
#######################################################################################
#######################################################################################
#######################################################################################


plot(sin$permanencia,sin$aviso)
plot(sin$vr_reserva,sin$vr_aseg_inicial)


plot(subset(sin,numero_moviento==1)$vr_reserva/1e6,subset(sin,numero_moviento==1)$vr_aseg_inicial/1e6)
plot(subset(sin,numero_moviento==1)$t_aviso,subset(sin,numero_moviento==1)$t_def)
plot(subset(sin,numero_moviento==1 & ESTADO.RVA5=='PENDIENTE')$vr_reserva/1e6,subset(sin,numero_moviento==1& ESTADO.RVA5=='PENDIENTE')$edad_sini)

plot(subset(sin,numero_moviento==1 & ESTADO.RVA5=='PENDIENTE')$fecha_ingreso,subset(sin,numero_moviento==1& ESTADO.RVA5=='PENDIENTE')$fec_sini)
plot(subset(sin,numero_moviento==1 & ESTADO.RVA5=='PENDIENTE')$fecha_formalizac,subset(sin,numero_moviento==1& ESTADO.RVA5=='PENDIENTE')$ult_mvto)
plot(subset(sin,numero_moviento==1 & ESTADO.RVA5=='PENDIENTE')$fecha_formalizac,subset(sin,numero_moviento==1& ESTADO.RVA5=='PENDIENTE')$hoy)

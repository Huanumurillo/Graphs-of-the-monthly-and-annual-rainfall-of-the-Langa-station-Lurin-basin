#install.packages("plotly")
library(plotly)

data <- read.csv("1.PMonth_langa.csv",head=TRUE,check.names = TRUE, stringsAsFactors = F, sep = ",")
data

dim(data)
length(data)
str(data)
summary(data)

fseq          <- as.list.data.frame(seq.Date, c("from", "to"))
Fecha         <- fseq(from=as.Date('1981-01-01'), to=as.Date('2016-01-01'), by='year')
DATA1    <- data.frame(Fecha, data)  

rownames(DATA1)<-DATA1$Fecha
DATA1[1] <- NULL 

MIN <- apply(DATA1,2,min)
DATA1 <- rbind(DATA1,MIN) # Comando para agregar al dataframe (cbind, rbind)
MAX <- apply(DATA1,2,max)
DATA1 <- rbind(DATA1,MAX)
MEDIA <- colMeans(DATA1)
DATA1 <- rbind(DATA1,MEDIA)

FECHA1 <- c('A_ENE', 'B_FEB', 'C_MAR', 'D_ABRI','E_MAY', 'F_JUN', 'G_JUL', 'H_AGO','I_SEP', 'J_OCT', 'K_NOV', 'L_DIC')
A <-data.frame(FECHA1,MIN,MAX,MEDIA)
fig1 <-  plot_ly(A, x= FECHA1, y= MIN, name="PREC. MÍNIMO", type="bar")%>%
  add_trace(y=MAX, name= "PREC. MÁXIMA")%>%
  add_trace(y=MEDIA, name= "PREC. MEDIA")%>% 
  layout(title = 'DATOS DE PRCIPITACIÓN MENSUALES (mm) ESTACIÓN LANGA - CUENCA LURÍN (1981-2016)',
         plot_bgcolor='#e5ecf6', 
         xaxis = list( 
           zerolinecolor = '#ffff', 
           zerolinewidth = 2, 
           gridcolor = 'ffff'), 
         yaxis = list( 
           zerolinecolor = '#ffff', 
           zerolinewidth = 2, 
           gridcolor = 'ffff'))
fig1 <- fig1 %>% layout(xaxis = list(title = "MESES (ENERO - DICIEMBRE)"),
                        yaxis = list(title = "PRECIPITACIÓN MENSUAL (mm)"))
fig1


SUMA <- rowSums(DATA1[1:36, ])
fseq          <- as.list.data.frame(seq.Date, c("from", "to"))
Fecha         <- fseq(from=as.Date('1981-01-01'), to=as.Date('2016-01-01'), by='year')
FECHA3 = (1981:2016)
B <- data.frame(Fecha,FECHA3,SUMA)
B[1]= NULL


fig <-  plot_ly(B, x= FECHA3, y= SUMA, name="PREC. ANUAL", type="bar",
                text=SUMA, textposition = "auto", marker = list(color = 'rgb(158,202,225)',
                                                                line = list(color = 'rgb(8,48,107)', width = 1.5)))%>%
  layout(title = 'DATOS DE PRCIPITACIÓN ANUAL (mm) ESTACIÓN LANGA - CUENCA LURÍN (1981-2016)',
         plot_bgcolor='#e5ecf6', 
         xaxis = list( 
           zerolinecolor = '#ffff', 
           zerolinewidth = 2, 
           gridcolor = 'ffff'), 
         yaxis = list( 
           zerolinecolor = '#ffff', 
           zerolinewidth = 2, 
           gridcolor = 'ffff'))

fig <- fig %>% layout(xaxis = list(title = "AÑOS (1981-2016)"),
                      yaxis = list(title = "PRECIPITACIÓN ANUAL (mm)"))

fig



####################
install.packages("ggplot2")
library(ggplot2)
data  <- read.csv("1.PMonth_langa.csv",head=TRUE,check.names = TRUE, stringsAsFactors = F, sep = ",")

fseq  <- as.list.data.frame(seq.Date, c("from", "to"))
Fecha <- fseq(from=as.Date('1981-01-01'), to=as.Date('2016-01-01'), by='year')
Suma  <- rowSums(DATA1[1:36, ])
DATA2 <- data.frame(Fecha, data, Suma) 

ggplot(data=DATA2, aes(x=Fecha, y=Suma))+
       geom_line(color='blue', size=1, linetype='solid') +
       ggtitle('PRECIPITACIÓN TOTAL ANUAL LANGA 1986-2016') +
       ylab('PRECIPITACIÓN (mm)') +
       xlab('AÑOS (1981-2016)') +
       theme(plot.title = element_text(hjust = 0.5, color='darkblue'))
  ###################






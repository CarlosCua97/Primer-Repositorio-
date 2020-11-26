#55. Tensión vs. Ciclos
#Pruebas de vida acelerada
#Ejemplo: Telar (Picciotto 1970)
#VD: Resistencia del hilo en ciclos
#VI: Tensión en centímetros
#"Picciotto1970"
#Condiciones normales de operación: 20 cm
rm(list=ls())
Datos=read.table(file.choose(),header=T)
head(Datos)
Tensión=Datos[,"Length"]
Ciclos=Datos[,"Cycles"]

NivelesTensión=unique(Tensión);NivelesTensión
summary(Ciclos)

hist(Ciclos)
plot(Tensión,Ciclos,pch=19,yaxp=c(0,400,20),las=2)
FiltroT30=Tensión==30
FiltroT60=Tensión==60
FiltroT90=Tensión==90
points(Tensión[FiltroT30],Ciclos[FiltroT30],pch=19,col="firebrick")
points(Tensión[FiltroT60],Ciclos[FiltroT60],pch=19,col="navy")
points(Tensión[FiltroT90],Ciclos[FiltroT90],pch=19,col="orange")
lines(lowess(Tensión,Ciclos),lwd=3,col="chartreuse3")
#lowess un suavizador para una nube de puntos

hist(Ciclos[FiltroT30],main="Tensión de 30 cm")
hist(Ciclos[FiltroT60],main="Tensión de 60 cm")
hist(Ciclos[FiltroT90],main="Tensión de 90 cm")


Colores=c("firebrick","navy","orange")
Contador=1
for(N in NivelesTensión)
{
  hist(Ciclos[Tensión==N],main=paste("Tensión de",N,"cm"),xaxp=c(0,400,40),freq=F,
       xlab="Ciclos",col=Colores[Contador])
  Sys.sleep(3)
  Contador=Contador+1
}

#56. Logverosimilitud con Variables Explicativas
#Concentrado
hist(Ciclos,main="Niveles de tensión en",xaxp=c(0,400,40),
     xlab="Ciclos",col="black",breaks=seq(0,400,10),density=10,angle=0)
Colores=c("firebrick","navy","orange")
Contador=1
for(N in NivelesTensión)
{
  hist(Ciclos[Tensión==N],xaxp=c(0,400,40),col=Colores[Contador],
       breaks=seq(0,400,10),density=10,angle=45*Contador,add=T)
  Sys.sleep(3)
  Contador=Contador+1
}

#Propuestas de ajuste para todos los niveles
#Propuestas basadas en distribuciones individuales, pero para todos
hist(Ciclos,main="Niveles de tensión en",xaxp=c(0,400,40),freq=F,
     xlab="Ciclos",breaks=seq(0,400,10))
#Weibull
t=seq(0,400,1)
DWeibll=dweibull(t,shape=2,scale=170)
lines(t,DWeibll,lwd=2,col="navy")
#El ajuste debe abarcar todos los datos, no tanto que se ajuste al comportamiento 

#LogVerosmilitud con variable explicativa
LogVerosimilitud=function(Muestra,Densidad,Parámetros,Explicativa)
{
  LV=sum(log(Densidad(Muestra,Parámetros,Explicativa)))
  return(LV)
}

#Escala: LINEAL | Forma: CONSTANTE
DensidadWeibullVM1=function(t,theta,explicativa)
{
  B0Escala=theta[1]
  B1Escala=theta[2]
  B0Forma=theta[3]
  Escala=B0Escala+B1Escala*explicativa
  Forma=B0Forma
  d=dweibull(t,shape=Forma,scale=Escala)
  return(d)
}

LogVerosimilitud(Muestra=Ciclos,Densidad=DensidadWeibullVM1,
 Parámetros=c(B0Escala=170,B1Escala=0,B0Forma=2),Explicativa=Tensión)
#Se le da valor 0 para primero intentar ambos parámetros constantes

#Se iterar el optim para no encerrase en un local óptimo
OptWeibullM1=optim(par=c(B0Escala=170,B1Escala=0,B0Forma=2),fn=LogVerosimilitud,
  Muestra=Ciclos,Densidad=DensidadWeibullVM1,Explicativa=Tensión,control=list(fnscale=-1))
OptWeibullM1

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

#57. Modelos lineales
LogVerosimilitudActual=OptWeibullM1$value;LogVerosimilitudActual
Tolerancia=0.001
repeat
{
  OptWeibullM1=optim(par=OptWeibullM1$par,fn=LogVerosimilitud,
  Muestra=Ciclos,Densidad=DensidadWeibullVM1,Explicativa=Tensión,control=list(fnscale=-1)) 
  if(OptWeibullM1$value-LogVerosimilitudActual<Tolerancia)
  {
    break
  }else
    {
    LogVerosimilitudActual=OptWeibullM1$value
    }
}
EMVWeibullM1=OptWeibullM1

Colores=c("firebrick","navy","orange")
Contador=1
for(N in NivelesTensión)
{
  hist(Ciclos[Tensión==N],main=paste("Tensión de",N,"cm"),xaxp=c(0,400,40),freq=F,
       xlab="Ciclos",col=Colores[Contador])
  lines(density(Ciclos[Tensión==N]),col="deeppink",lwd=3)
  d=DensidadWeibullVM1(t,theta=EMVWeibullM1$par,explicativa=N)
  lines(t,d,lwd=3,col="cyan")
  Sys.sleep(2)
  Contador=Contador+1
}

#EMV: y=B0+B1X
EMVWeibullM1[1]   #En promedio, son 244 ciclos para la escala
#B0: valor que tendría la variable Y cuando X=0
#Si la tensión del telar fuera 0, la duración del hilo sería 244.93 cilcos
#Que para este caso no tiene sentido la interpretación.

#B1: por cada unidad extra de tensión (cm) el número de ciclos que puede 
#durar el hilo en ese telar, disminuye 2.13 ciclos.

#B3: 2.04 de forma constante

#####################################################################

#Escala: CONSTANTE | Forma: LINEAL
DensidadWeibullVM2=function(t,theta,explicativa)
{
  B0Escala=theta[1]
  B0Forma=theta[2]
  B1Forma=theta[3]
  Escala=B0Escala
  Forma=B0Forma+B1Forma*explicativa
  d=dweibull(t,shape=Forma,scale=Escala)
  return(d)
}

LogVerosimilitud(Muestra=Ciclos,Densidad=DensidadWeibullVM2,
                 Parámetros=c(B0Escala=170,B0Forma=2,B1Forma=0),Explicativa=Tensión)
#Se le da valor 0 para primero intentar ambos parámetros constantes

#Se iterar el optim para no encerrase en un local óptimo
OptWeibullM2=optim(par=c(B0Escala=170,B0Forma=2,B1Forma=0),fn=LogVerosimilitud,
                   Muestra=Ciclos,Densidad=DensidadWeibullVM2,Explicativa=Tensión,control=list(fnscale=-1))
OptWeibullM2

LogVerosimilitudActual=OptWeibullM2$value;LogVerosimilitudActual
Tolerancia=0.001
repeat
{
  OptWeibullM2=optim(par=OptWeibullM2$par,fn=LogVerosimilitud,
                     Muestra=Ciclos,Densidad=DensidadWeibullVM2,Explicativa=Tensión,control=list(fnscale=-1)) 
  if(OptWeibullM2$value-LogVerosimilitudActual<Tolerancia)
  {
    break
  }else
  {
    LogVerosimilitudActual=OptWeibullM2$value
  }
}
EMVWeibullM2=OptWeibullM2;EMVWeibullM2

Colores=c("firebrick","navy","orange")
Contador=1
for(N in NivelesTensión)
{
  hist(Ciclos[Tensión==N],main=paste("Tensión de",N,"cm"),xaxp=c(0,400,40),freq=F,
       xlab="Ciclos",col=Colores[Contador])
  lines(density(Ciclos[Tensión==N]),col="deeppink",lwd=3)
  d=DensidadWeibullVM2(t,theta=EMVWeibullM2$par,explicativa=N)
  lines(t,d,lwd=3,col="cyan")
  Sys.sleep(2)
  Contador=Contador+1
}

#####################################################################

#Escala: LINEAL | Forma: LINEAL
DensidadWeibullVM3=function(t,theta,explicativa)
{
  B0Escala=theta[1]
  B1Escala=theta[2]
  B0Forma=theta[3]
  B1Forma=theta[4]
  Escala=B0Escala+B1Escala*explicativa
  Forma=B0Forma+B1Forma*explicativa
  d=dweibull(t,shape=Forma,scale=Escala)
  return(d)
}

LogVerosimilitud(Muestra=Ciclos,Densidad=DensidadWeibullVM3,
                 Parámetros=c(B0Escala=170,B1Escala=0,B0Forma=2,B1Forma=0),Explicativa=Tensión)
#Se le da valor 0 para primero intentar ambos parámetros constantes

#Se iterar el optim para no encerrase en un local óptimo
OptWeibullM3=optim(par=c(B0Escala=170,B1Escala=0,B0Forma=2,B1Forma=0),fn=LogVerosimilitud,
                   Muestra=Ciclos,Densidad=DensidadWeibullVM3,Explicativa=Tensión,control=list(fnscale=-1))
OptWeibullM3

LogVerosimilitudActual=OptWeibullM3$value;LogVerosimilitudActual
Tolerancia=0.001
repeat
{
  OptWeibullM3=optim(par=OptWeibullM3$par,fn=LogVerosimilitud,
                     Muestra=Ciclos,Densidad=DensidadWeibullVM3,Explicativa=Tensión,control=list(fnscale=-1)) 
  if(OptWeibullM3$value-LogVerosimilitudActual<Tolerancia)
  {
    break
  }else
  {
    LogVerosimilitudActual=OptWeibullM3$value
  }
}
EMVWeibullM3=OptWeibullM3;EMVWeibullM3

Colores=c("firebrick","navy","orange")
Contador=1
for(N in NivelesTensión)
{
  hist(Ciclos[Tensión==N],main=paste("Tensión de",N,"cm"),xaxp=c(0,400,40),freq=F,
       xlab="Ciclos",col=Colores[Contador])
  lines(density(Ciclos[Tensión==N]),col="deeppink",lwd=3)
  d=DensidadWeibullVM3(t,theta=EMVWeibullM3$par,explicativa=N)
  lines(t,d,lwd=3,col="cyan")
  Sys.sleep(2)
  Contador=Contador+1
}



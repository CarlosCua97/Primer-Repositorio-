#55. Tensión vs. Ciclos
#Pruebas de vida acelerada
#Ejemplo: Telar (Picciotto 1970)
#VD: Resistencia del hilo en ciclos
#VI: Tensión en centímetros
#"Picciotto1970"
#Condiciones normales de operación: 20 cm

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
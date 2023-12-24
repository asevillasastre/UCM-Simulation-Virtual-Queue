#programa de COLA VIRTUAL

###programa principal 4 colas con cola virtual CON ACTIVIDADES EXTRA
#cremos vectores vacíos
vector_acabadas=numeric()
vector_acabadasc=numeric()
vector_acabadas1=numeric()
vector_acabadas2=numeric()
vector_acabadas3=numeric()
vector_acabadas4=numeric()
vector_libres=numeric()
vector_acabadasp=numeric()
vector_acabadase=numeric()
vector_acabadasd=numeric()
n=100
i=0
#HACER PSEUDOCÓDIGO DE ESTO PARA EXPLICAR LOS CAMBIOS MAS DESTACADOS
####subrutinas####
#parámtros de las variables aleatorias
LAMBDA1 = 1/12000
LAMBDA2 = 2/11000
LAMBDA3 = 3/15000
LAMBDA4 = 1/19000
LAMBDAC= 1/16000
LAMBDAD= 1/15000
LAMBDAP= 1/17000
LAMBDAE=1/20000
MU1 = 180
MU2 = 120
MU3 = 250
MU4 = 100

#subrutinas de llegadas y salidas de las 4 atracciones principales
Llegada1 = function(NMAX,N1,TS1,TM){
  N1 = N1 + 1
  if (N1 >= 10 & TS1 == Inf){
    TS1 = TM + rnorm(1,MU1,30)
  }
  TL1 = TM + rexp(1,LAMBDA1*NMAX)
  TL1 = TM + rexp(1,LAMBDA1*NMAX)
  if (N1 >= 750) {TL1 = Inf}
  return(c(NMAX,N1,TS1,TL1,TL2,TL3,TL4))}

Llegada2 = function(NMAX,N2,TS2,TM){
  N2 = N2 + 1
  if (N2 >= 15 & TS2 == Inf){
    TS2 = TM + rnorm(1,MU2,30)
  }
  TL2 = TM + rexp(1,LAMBDA2*NMAX)
  TL2 = TM + rexp(1,LAMBDA2*NMAX)
  if (N2 >= 750) {TL2 = Inf}
  return(c(NMAX,N2,TS2,TL1,TL2,TL3,TL4))}

Llegada3 = function(NMAX,N3,TS3,TM){
  N3 = N3 + 1
  if (N3 >= 20 & TS3 == Inf){
    TS3 = TM + rnorm(1,MU3,30)
  }
  TL3 = TM + rexp(1,LAMBDA3*NMAX)
  TL3 = TM + rexp(1,LAMBDA3*NMAX)
  if (N3 >= 750) {TL3 = Inf}
  return(c(NMAX,N3,TS3,TL1,TL2,TL3,TL4))}

Llegada4 = function(NMAX,N4,TS4,TM){
  N4 = N4 + 1
  if (N4 >= 5 & TS4 == Inf){
    TS4 = TM + rnorm(1,MU4,30)
  }
  TL4 = TM + rexp(1,LAMBDA4*NMAX)
  TL4 = TM + rexp(1,LAMBDA4*NMAX)
  if (N4 >= 750) {TL4 = Inf}
  return(c(NMAX,N4,TS4,TL1,TL2,TL3,TL4))}

Salida1 = function(NMAX,N1,TS1,TM,ACABADAS,ACABADAS1){
  N1 = N1 - 10
  ACABADAS = ACABADAS + 10
  ACABADAS1 = ACABADAS1 + 10
  if (N1 < 10){TS1 = Inf}
  if (N1 >= 10){TS1 = TM + rnorm(1,MU1,30)}
  TL2 = TM + rexp(1,LAMBDA2*NMAX); TL1 = TM + rexp(1,LAMBDA1*NMAX); TL3 = TM + rexp(1,LAMBDA3*NMAX); TL4 = TM + rexp(1,LAMBDA4*NMAX)
  return(c(NMAX,N1,TS1,ACABADAS,TL1,TL2,TL3,TL4,ACABADAS1))
}

Salida2 = function(NMAX,N2,TS2,TM,ACABADAS,ACABADAS2){
  N2 = N2 - 15
  ACABADAS = ACABADAS + 15
  ACABADAS2 = ACABADAS2 +15
  if (N2 < 15){TS2 = Inf}
  if (N2 >= 15){TS2 = TM + rnorm(1,MU2,30)}
  TL2 = TM + rexp(1,LAMBDA2*NMAX); TL1 = TM + rexp(1,LAMBDA1*NMAX); TL3 = TM + rexp(1,LAMBDA3*NMAX); TL4 = TM + rexp(1,LAMBDA4*NMAX)
  return(c(NMAX,N2,TS2,ACABADAS,TL1,TL2,TL3,TL4,ACABADAS2))
}

Salida3 = function(NMAX,N3,TS3,TM,ACABADAS,ACABADAS3){
  N3 = N3 - 20
  ACABADAS = ACABADAS + 20
  ACABADAS3 = ACABADAS3 + 20
  if (N3 < 20){TS3 = Inf}
  if (N3 >= 20){TS3 = TM + rnorm(1,MU3,30)}
  if (NMAX > 0) {TL2 = TM + rexp(1,LAMBDA2*NMAX); TL1 = TM + rexp(1,LAMBDA1*NMAX); TL3 = TM + rexp(1,LAMBDA3*NMAX); TL4 = TM + rexp(1,LAMBDA4*NMAX)}
  if (NMAX == 0) {
    TL1=Inf
    TL2=Inf
    TL3=Inf
    TL4=Inf}
  return(c(NMAX,N3,TS3,ACABADAS,TL1,TL2,TL3,TL4,ACABADAS3))
}

Salida4 = function(NMAX,N4,TS4,TM,ACABADAS,ACABADAS4){
  N4 = N4 - 5
  ACABADAS = ACABADAS + 5
  ACABADAS4 = ACABADAS4 + 5
  if (N4 < 5){TS4 = Inf}
  if (N4 >= 5){TS4 = TM + rnorm(1,MU4,30)}
  TL2 = TM + rexp(1,LAMBDA2*NMAX); TL1 = TM + rexp(1,LAMBDA1*NMAX); TL3 = TM + rexp(1,LAMBDA3*NMAX); TL4 = TM + rexp(1,LAMBDA4*NMAX)
  return(c(NMAX,N4,TS4,ACABADAS,TL1,TL2,TL3,TL4,ACABADAS4))
}


##SUBRUTINAS DE LAS ACTIVIDADES EXTRA
Comer = function(NMAX,TM,ACABADASC,ACABADAS){
  ACABADASC = ACABADASC + 1
  ACABADAS = ACABADAS + 1
  TC = TM + rexp(1,LAMBDAC*NMAX)
  return(c(TC, ACABADASC, ACABADAS))
}

Disparar = function(NMAX,TM,ACABADASD,ACABADAS){
  ACABADASD = ACABADASD + 1
  ACABADAS = ACABADAS + 1
  TD = TM + rexp(1,LAMBDAD*NMAX)
  return(c(TD, ACABADASD, ACABADAS))
}
Encestar = function(NMAX,TM,ACABADASE,ACABADAS){
  ACABADASE = ACABADASE + 1
  ACABADAS = ACABADAS + 1
  TE = TM + rexp(1,LAMBDAE*NMAX)
  return(c(TE, ACABADASE, ACABADAS))
}
Pescar = function(NMAX,TM,ACABADASP,ACABADAS){
  ACABADASP = ACABADASP + 1
  ACABADAS = ACABADAS + 1
  TP = TM + rexp(1,LAMBDAP*NMAX)
  return(c(TP, ACABADASP, ACABADAS))
}

#PROGRAMA PRINCIPAL
while (i<n) {
#inicializamos las variables de la cola virtual
NMAX = 3000 - 10*3 - 15*3 - 20*3 - 5*3
TMAX = 21600
TM = 0
TS1 = Inf
TL1 = rexp(1,LAMBDA1*NMAX)
TS2 = Inf
TL2 = rexp(1,LAMBDA2*NMAX)
TS3 = Inf
TL3 = rexp(1,LAMBDA3*NMAX)
TS4 = Inf
TL4 = rexp(1,LAMBDA4*NMAX)
TC = rexp(1,LAMBDAC*NMAX)
TD = rexp(1,LAMBDAD*NMAX)
TE = rexp(1,LAMBDAE*NMAX)
TP = rexp(1,LAMBDAP*NMAX)
#añadir variables de las subrutinas
N1 = 0
N2 = 0
N3 = 0
N4 = 0
ACABADAS = 0
ACABADAS1 = 0
ACABADAS2 = 0
ACABADAS3 = 0
ACABADAS4 = 0
ACABADASC = 0
ACABADASD = 0
ACABADASE = 0
ACABADASP = 0
LIBRES = 0
TANT = 0

while(TM < TMAX){
  #Actualizamos el reloj de simulación
  TANT = TM
  TM = min(TL1,TS1,TL2,TS2,TS3,TL3,TS4,TL4,TC,TD,TE,TP)
  LIBRES = LIBRES + (NMAX)*(TM-TANT)
  #distinguimos casos:
  if (TM == TL1) {
    K = Llegada1(NMAX,N1,TS1,TM)
    NMAX = K[1]
    N1 = K[2]
    TS1 = K[3]
    TL1 = K[4]
    TL2= K[5]
    TL3= K[6]
    TL4= K[7]
  }
  if (TM == TL2) {
    K = Llegada2(NMAX,N2,TS2,TM)
    NMAX = K[1]
    N2 = K[2]
    TS2 = K[3]
    TL1 = K[4]
    TL2= K[5]
    TL3= K[6]
    TL4= K[7]
  }
  if (TM == TL3) {
    K = Llegada3(NMAX,N3,TS3,TM)
    NMAX = K[1]
    N3 = K[2]
    TS3 = K[3]
    TL1 = K[4]
    TL2= K[5]
    TL3= K[6]
    TL4= K[7]
  }
  if (TM == TL4) {
    K = Llegada4(NMAX,N4,TS4,TM)
    NMAX = K[1]
    N4 = K[2]
    TS4 = K[3]
    TL1 = K[4]
    TL2= K[5]
    TL3= K[6]
    TL4= K[7]
  }
  if (TM == TS1) {
    K = Salida1(NMAX,N1,TS1,TM,ACABADAS,ACABADAS1)    
    NMAX = K[1]
    N1 = K[2]
    TS1 = K[3]
    ACABADAS = K[4]
    TL1 = K[5]
    TL2 = K[6]
    TL3 = K[7]
    TL4 = K[8]
    ACABADAS1 = K[9]
  }
  if (TM == TS2) {
    K = Salida2(NMAX,N2,TS2,TM,ACABADAS,ACABADAS2)    
    NMAX = K[1]
    N2 = K[2]
    TS2 = K[3]
    ACABADAS = K[4]
    TL1 = K[5]
    TL2 = K[6]
    TL3 = K[7]
    TL4 = K[8]
    ACABADAS2 = K[9]
  }
  
  if (TM == TS3) {
    K = Salida3(NMAX,N3,TS3,TM,ACABADAS,ACABADAS3)    
    NMAX = K[1]
    N3 = K[2]
    TS3 = K[3]
    ACABADAS = K[4]
    TL1 = K[5]
    TL2 = K[6]
    TL3 = K[7]
    TL4 = K[8]
    ACABADAS3 = K[9]
  }
  
  if (TM == TS4) {
    K = Salida4(NMAX,N4,TS4,TM,ACABADAS,ACABADAS4)    
    NMAX = K[1]
    N4 = K[2]
    TS4 = K[3]
    ACABADAS = K[4]
    TL1 = K[5]
    TL2 = K[6]
    TL3 = K[7]
    TL4 = K[8]
    ACABADAS4 = K[9]
  }
  if(TM == TC) {
    K = Comer(NMAX,TM,ACABADASC,ACABADAS)
    TC = K[1]
    ACABADASC = K[2]
    ACABADAS = K[3]
  }
  if(TM == TD) {
    K = Disparar(NMAX,TM,ACABADASD,ACABADAS)
    TD = K[1]
    ACABADASD= K[2]
    ACABADAS = K[3]
  }
  if(TM == TE) {
    K = Encestar(NMAX,TM,ACABADASE,ACABADAS)
    TE= K[1]
    ACABADASE= K[2]
    ACABADAS = K[3]
  }
  if(TM == TP) {
    K = Pescar(NMAX,TM,ACABADASP,ACABADAS)
    TP = K[1]
    ACABADASP= K[2]
    ACABADAS = K[3]
  }
}
i=i+1
#guardamos datos en vectores
vector_acabadas=c(vector_acabadas,ACABADAS)
vector_acabadasc=c(vector_acabadasc,ACABADASC)
vector_acabadas1=c(vector_acabadas1,ACABADAS1)
vector_acabadas2=c(vector_acabadas2,ACABADAS2)
vector_acabadas3=c(vector_acabadas3,ACABADAS3)
vector_acabadas4=c(vector_acabadas4,ACABADAS4)
vector_libres=c(vector_libres,LIBRES)
vector_acabadasp=c(vector_acabadasp,ACABADASP)
vector_acabadase=c(vector_acabadase,ACABADASE)
vector_acabadasd=c(vector_acabadasd,ACABADASD)
}#fin bucle repeticion de la simulacion

#analizamos los datos de la simulacion
mean(vector_acabadas)/3000
a=mean(vector_acabadas1)
b=mean(vector_acabadas2)
c=mean(vector_acabadas3)
d=mean(vector_acabadas4)
e=mean(vector_libres)/21600

#medimos la satisfacción, cuántas actividades que se han hecho, y el costo y beneficio
satisfaccion = a/3000*0.12 + b/3000*0.07 + c/3000*0.17 + d/3000*0.05 + e/14000
satisfaccion
comidas = 12*mean(vector_acabadasc)*300
comidas
disparar=mean(vector_acabadasd)*300
disparar
pescar=mean(vector_acabadasp)*300
pescar
encestar=mean(vector_acabadasp)*300
encestar
implementar= 4000000
mantener= 3000000
beneficio = (1 + satisfaccion) * 15*3000*300 + comidas + pescar + disparar+ encestar -implementar -mantener
beneficio
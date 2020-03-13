

## Parametros iniciales
N = 10^4 #cantidad de agentes
D = 0.9  #densidad de provacunas
PI = 1 #porcentaje de infectados
gama = 0.8 #Prob de que se vacune
lambda = 0.3 #prob de que se enferme si entra en contacto con uno infectado
alfa = 0.5 #prob de curarse
phi = 0.2 #prob de que se acabe el efecto de la vacuna

T = 500 #cantidad de iteraciones del experimento

##Inicio algoritmo

#T0
pob = 1:N #agentes

muestraProVacuna = sample(pob,D*N)

O = rep(-1,N) #Opinion de la poblacion respecto vacunacion
O[muestraProVacuna] = 1

f1.pos = (O==1) #poblacion provacuna
f1.neg = (O==-1)  #poblacion antivacuna

muestraInfec = sample(pob,PI*N/100)

E = rep('S',N) #estado "epidemiologico" de la poblacion: I,S, o V
E[muestraInfec] = 'I'

##Dinamica de la epidemia

for (t in 1:T) {
  O_aux = rep(0,N)
  E_aux = rep('',N)
  
  # Actualizacion de opiniones
  for (i in pob) {
    P1P2 = sample(pob[pob!=i],2) #Elige dos personas distintas de i
    if (sum(O[c(i,P1P2)])>=1) {
      O_aux[i] = 1
    } else {
      O_aux[i] = -1
    }
  }
  O = O_aux
  
  #Actualizacion estado epidemiologico
  for (i in pob) {
    if (E[i] == 'V') {
      if (phi>runif(1)) E_aux[i] = 'S' else E_aux[i] = E[i]
    } else if (E[i] == 'I') {
      if (alfa>runif(1)) E_aux[i] = 'S' else E_aux[i] = E[i]
    } else {
      if (O[i] == 1 & gama>runif(1)) {
        E_aux[i] = 'V' 
      } else {
        PA = sample(pob[pob!=i],1) #Elijo una persona al azar
        if (E[PA] == 'I' & lambda>runif(1)) E_aux[i] = 'I' else E_aux[i] = 'S' #me fijo si esa persona esta infectada y me infecto con probabilidad lambda
      }
    }
  }
  E = E_aux
}
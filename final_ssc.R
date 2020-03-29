

## Parametros iniciales
set.seed(1) #Semilla para reproducir los mismos numeros aleatorios

N = 10^4 #cantidad de agentes
D = 0.4  #densidad de provacunas
PI = 1 #porcentaje de infectados
gama = 0.2 #Prob de que se vacune
lambda = 0.3 #prob de que se enferme si entra en contacto con uno infectado
alfa = 0.2 #prob de curarse
phi = 0.1 #prob de que se acabe el efecto de la vacuna
T = 500 #cantidad de iteraciones del experimento

repeticiones = 10 #cantidad de repeticiones de un experimento
S = rep(0,T+1)
V = rep(0,T+1)
I = rep(0,T+1)

pob = 1:N #agentes

##Inicio algoritmo
for (r in 1:repeticiones) {
  #Inicializacion
  
  muestraProVacuna = sample(pob,D*N)
  
  O = rep(-1,N) #Opinion de la poblacion respecto vacunacion
  O[muestraProVacuna] = 1
  
  muestraInfec = sample(pob,PI*N/100)
  E = rep('S',N) #estado "epidemiologico" de la poblacion: I,S, o V
  E[muestraInfec] = 'I'
  #Notar que al inicio no hay agentes vacunados
  
  #Voy a ir contando la cantidad de I, S, V, y como voy a considerar varias
  #muestras para promediar, voy a ir sumando en cada lugar y luego promediarlo
  I[1] = I[1] + PI*N/100 #Cantidad de infectados
  S[1] = S[1] + N-PI*N/100 #Los que no estan infectados estan Susceptibles
  V[1] = V[1] + 0
  
  ##Dinamica 
  
  for (t in 2:(T+1)) {
    O_aux = rep(0,N) 
    E_aux = rep('S',N)
    
    # Actualizacion de opiniones
    for (i in pob) {
      P1P2 = sample(pob[pob!=i],2) #Elige dos agentes al azar distintos de i
      if (sum(O[c(i,P1P2)])>=1) {
        O_aux[i] = 1
      } else {
        O_aux[i] = -1
      }
    }
    O = O_aux #Actualizacion de opinion
    
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
    I[t] = I[t] + sum(E=='I')
    S[t] = S[t] + sum(E=='S')
    V[t] = V[t] + sum(E=='V')
  }
}
I = I/repeticiones
S = S/repeticiones
V = V/repeticiones
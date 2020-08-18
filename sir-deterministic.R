library(deSolve)

# Functions
sir_1 <- function(beta, gamma, S0, I0, R0, times) {
  sir_equations <- function(t, variables, parameters) {
  with(as.list(c(variables, parameters)), {
    dS <- -beta * I * S / N
    dI <-  beta * I * S / N - gamma * I
    dR <- gamma * I
    return(list(c(dS, dI, dR)))
  })
  }
  parameters_values <- c(beta=beta, gamma=gamma)
  initial_values <- c(S = S0, I = I0, R = R0)
  out <- ode(initial_values, times, sir_equations, parameters_values)
  as.data.frame(out)
}


N    = 10^4
I0   = 10
S0   = N-I0
R0   = 0
dt   = 0.1 
tmax = 100

tfuture = seq(1,tmax, dt) 

bet = 0.8
gam = 0.2

mat = sir_1(beta=bet,gamma=gam,S0=S0,I0=I0,R0=R0,times=tfuture) #nAP= 861773 #N  = nAP# 10^5
print(head(mat))

x = mat[,1]
y = mat[,3]

output  = data.frame(x,y)
nameOUT = sprintf('sir-deterministic.dat')
write.table(output, file=nameOUT,row.names=F,col.names=F)


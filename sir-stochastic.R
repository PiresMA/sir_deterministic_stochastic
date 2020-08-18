#!/usr/bin/env Rscript
args = commandArgs(TRUE)

samp = as.numeric(args[1])

sir  =  function(beta, gamma, N, S0, I0, R0, tf) {
    time =  0
    S  =  S0
    I  =  I0
    R  =  R0
    ta =  numeric(0)
    Sa =  numeric(0)
    Ia =  numeric(0)
    Ra =  numeric(0)
    while (time < tf) {
        ta  =  c(ta, time)
        Sa  =  c(Sa, S)
        Ia  =  c(Ia, I)
        Ra  =  c(Ra, R)
        pf1 =  beta * S * I/N
        pf2 =  gamma * I
        pf  =  pf1 + pf2
        dt  =  rexp(1, rate = pf)
        time =  time + dt
        if (time > tf) {
            break
        }
        ru  =  runif(1)
        if (ru < (pf1/pf)) {
            S  =  S - 1
            I  =  I + 1
        } else {
            I  =  I - 1
            R  =  R + 1
        }
        if (I == 0) {
            break
        }
    }
    results  =  data.frame(time = ta, I = Ia)
    return(results)
}

N    = 10^4
I0   = 10
S0   = N-I0
R0   = 0
tmax = 100

lis = sir(0.8,0.2,N,N-I0,I0,R0,tmax)

x = lis[[1]]
y = lis[[2]]

output  = data.frame(x,y)
nameOUT = sprintf('sir-gillespie-samp%02d.dat',samp)
write.table(output, file=nameOUT,row.names=F,col.names=F)


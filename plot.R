


png('sir-I-stochastic.png')
# SIR: Stochastic 

nameIN=dir(patt='sir-gillespie-samp')
print(nameIN)

for( i in 1:length(nameIN[1:100]) ){
 mat = data.matrix(read.table(nameIN[i]))
 if(i==1) plot(mat[,1],mat[,2],type='l',col=i,xlab='Time (day)', ylab='Infected',xlim=c(0,40),ylim=c(0,4200))
 else lines(mat[,1],mat[,2],type='l',col=i+1,lty=2) 
}
#mtext(side=3,line=0.1,cex=1,'Stochastic SIR model')

dev.off()


png('sir-I-deterministic.png')

# SIR: deterministic

mat = data.matrix(read.table('sir-deterministic.dat'))
#lines(mat[,1]-1,mat[,2],type='l',col='black',lwd=3,lty=1) 
plot(mat[,1],mat[,2],type='l',col='black',xlab='Time (day)', ylab='Infected',xlim=c(0,40),ylim=c(0,4200))
#mtext(side=3,line=0.1,cex=1,'Deterministic SIR model')
dev.off()


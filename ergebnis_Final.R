# Ergebnisauswertung 
# Finale Version

#ToDo

#	Turnover
#	Performance vor und nach Transaktionskosten (nehmen Sie an, dass jede Transaktion Kosten in Höhe von 0.2% des Transaktionsvolumens verursachen)

#Benötigte Pakete: Installation
install.packages("quadprog")
install.packages("corpcor")

#Benötigte Pakete: Laden
library(quadprog)
library(corpcor)


# Daten einlesen (Ergebnismatrix_buyable)
ergebnismatrix_full = read.csv("Daten/3_Ergebnismatrix_buyable5Jahre80.csv", sep =",", dec = ".")


kurseTaegl = read.csv("Daten/0_Kurse.csv", sep =",", dec = ".", stringsAsFactors=FALSE)

perfdaily = kurseTaegl
perfdaily[]=NA
perfdaily[,1]=kurseTaegl[,1]
for ( w in 2:228){
  for (i in 2:6333){
    if(!(is.na(kurseTaegl[i,w]))&!(is.na(kurseTaegl[i-1,w]))){
      perfdaily[i,w]=1+(kurseTaegl[i,w]-kurseTaegl[i-1,w])/kurseTaegl[i-1,w]
    }
  }
}
write.csv(perfdaily, file="Daten/1_Perfdaily.csv", row.names = F)


perfdaily = read.csv("Daten/1_PerfdailyDiv.csv", sep =",", dec = ".")

#marketcap = read.csv("Daten/0_Marktkapitalisierung.csv", sep =";", dec = ".",stringsAsFactors=FALSE, header=TRUE)

#for (i in 1:25){
#  marketcap[i,is.na(marketcap[i,1:228])] = mean(as.numeric(marketcap[i,2:228]), na.rm=T)
#}  

GewichtungGG = ergebnismatrix_full
GewichtungGG[] = NA
GewichtungGG[,1] = ergebnismatrix_full[,1]
#Keine MW Gewichtung mehr sondern nach Wahrscheinlichkeitswert
GewichtungMW = ergebnismatrix_full
GewichtungMW[] = NA
GewichtungMW[,1] = ergebnismatrix_full[,1]
GewichtungMinVar = ergebnismatrix_full
GewichtungMinVar[] = NA
GewichtungMinVar[,1] = ergebnismatrix_full[,1]
BenchGewichtungMinVar = ergebnismatrix_full
BenchGewichtungMinVar[] = NA
BenchGewichtungMinVar[,1] = ergebnismatrix_full[,1]


N = 20

#A = rbind(c(rep(1,20)),diag(20))
#b = c(1,rep(0,20))
A.Equality <- matrix(c(rep(1,20)), ncol=1)
A = cbind(A.Equality, diag(20),-diag(20)) 
b = c(1, rep(0.02,20), rep(-0.15,20))
#m für 5 Jahre
for (m in 108:nrow(ergebnismatrix_full)) {
  top20=order(ergebnismatrix_full[m,1:228], decreasing = T)[2:(N+1)]
  GewichtungGG[m,top20] = 1/N
  covarianz= cov.shrink(perfdaily[which(perfdaily[,1]==ergebnismatrix_full[m,1]):(which(perfdaily[,1]==ergebnismatrix_full[m,1])-250),top20])
  
  GewichtungMinVar[m,top20]=solve.QP(covarianz,dvec=rep(0,20),Amat=A,bvec=b,meq=1)$solution
  
  #GewichtungMW[m,top20] = marketcap[as.numeric(format(as.Date(ergebnismatrix_buyable[m,1],origin="1899-12-30"), "%Y"))-1==marketcap[,1],top20]/sum(marketcap[as.numeric(format(as.Date(ergebnismatrix_buyable[m,1],origin="1899-12-30"), "%Y"))-1==marketcap[,1],top20])
  GewichtungMW[m,top20] = ergebnismatrix_full[m,top20]/sum(ergebnismatrix_full[m,top20])
  
  #inIndex=227-sum(is.na(ergebnismatrix_full[m,]))
  #AT.Equality <- matrix(c(rep(1,inIndex)), ncol=1)
  #AT = cbind(AT.Equality, diag(inIndex),-diag(inIndex)) 
  #bT = c(1, rep(0.002,inIndex), rep(-0.1,inIndex))

  #covarianz= cov.shrink(perfdaily[which(perfdaily[,1]==ergebnismatrix_full[m,1]):(which(perfdaily[,1]==ergebnismatrix_full[m,1])-250),c(F,!is.na(ergebnismatrix_full[m,2:228]))])
  #BenchGewichtungMinVar[m,c(F,!is.na(ergebnismatrix_full[m,2:228]))]=solve.QP(covarianz,dvec=rep(0,inIndex),Amat=AT,bvec=bT,meq=0)$solution
  
}

rowSums(GewichtungGG[,2:228],na.rm=T)
rowSums(GewichtungMinVar[,2:228],na.rm=T)
rowSums(GewichtungMW[,2:228],na.rm=T)
#rowSums(BenchGewichtungMinVar[,2:228],na.rm=T)

GewichtungGG[is.na(GewichtungGG)] = 0
GewichtungMW[is.na(GewichtungMW)] = 0
GewichtungMinVar[is.na(GewichtungMinVar)] = 0
#BenchGewichtungMinVar[is.na(BenchGewichtungMinVar)] = 0

performanceGG=c()
performanceMW=c()
performanceMinVar=c()
#Bench_performanceGG=c()
#Bench_performanceMinVar =c()

for (i in 108:297){
  #perfdaily[perfdaily[,1]>=GewichtungGG[i,1]&&perfdaily[,1]<GewichtungGG[i+1,1],GewichtungGG[i,1:228]>0]
  portGG = perfdaily[(perfdaily[,1]>=GewichtungGG[i,1]&perfdaily[,1]<GewichtungGG[i+1,1]),GewichtungGG[i,1:228]>0]
  performanceGG = append(performanceGG,sum(((tail(cumprod(portGG[,1:21]), n=1)-1)*GewichtungGG[i,GewichtungGG[i,1:228]>0])[2:21])+1)

  portMW = perfdaily[(perfdaily[,1]>=GewichtungMW[i,1]&perfdaily[,1]<GewichtungMW[i+1,1]),GewichtungMW[i,1:228]>0]
  performanceMW = append(performanceMW,sum(((tail(cumprod(portMW[,1:ncol(portMW)]), n=1)-1)*GewichtungMW[i,GewichtungMW[i,1:228]>0])[2:ncol(portMW)])+1)
  
  portMinVar = perfdaily[(perfdaily[,1]>=GewichtungMinVar[i,1]&perfdaily[,1]<GewichtungMinVar[i+1,1]),GewichtungMinVar[i,1:228]>0]
  performanceMinVar = append(performanceMinVar,sum(((tail(cumprod(portMinVar[,1:ncol(portMinVar)]), n=1)-1)*GewichtungMinVar[i,GewichtungMinVar[i,1:228]>0])[2:ncol(portMinVar)])+1)
  #Benchmark

  #Bench_portGG = perfdaily[(perfdaily[,1]>=GewichtungGG[i,1]&perfdaily[,1]<GewichtungGG[i+1,1]),!is.na(ergebnismatrix_full[i,])]
  #Bench_performanceGG = append(Bench_performanceGG,mean(as.numeric(tail(cumprod(Bench_portGG[,2:ncol(Bench_portGG)]), n=1))))

  #Bench_portMinVar = perfdaily[(perfdaily[,1]>=GewichtungGG[i,1]&perfdaily[,1]<GewichtungGG[i+1,1]),!is.na(ergebnismatrix_full[i,])]
  #Bench_performanceMinVar = append(Bench_performanceMinVar,sum(((tail(cumprod(Bench_portMinVar[,1:ncol(Bench_portMinVar)]), n=1)-1)*BenchGewichtungMinVar[i,!is.na(ergebnismatrix_full[i,])])[2:ncol(Bench_portMinVar)])+1)
  
}

cumprod(performanceMinVar)
GGCum=cumprod(performanceGG[1:249])
MWCum=cumprod(performanceMW[1:249])
MVCum=cumprod(performanceMinVar[1:249])
BenchCum=cumprod(Bench_performanceGG)
cumprod(Bench_performanceMinVar)
cumprod(performanceGG[1:249])
GGCum[6:9]
MWCum[6:9]
MVCum[6:9]
trueee=(colSums(GewichtungMW[53:56,2:228])>0)
GewichtungMW[53:56,trueee]
?cols
min(performanceMW)
max(GewichtungMW[2:228])


plot(cumprod(performanceMinVar[1:226]),type="l")

plot(cumprod(performanceMinVar[1:190]),type="l")

lines(cumprod(Bench_performanceMinVar[1:226]))
lines(cumprod(performanceGG[1:226]))
lines(cumprod(performanceMW[1:226]))
lines(cumprod(performanceMinVar[1:226]))
lines(cumprod(Bench_performanceGG[1:226]))

Results = data.frame(performanceGG=double(226))
Results$performanceGG3k12=performanceGG[1:226]                    
Results$performanceMinVar3k12=performanceMinVar[1:226] 
Results$performanceMW3k12=performanceMW[1:226]                    

Results$performanceGG5Jahre=c(rep(0,36),performanceGG[1:190])                    
Results$performanceMinVar5Jahre=c(rep(0,36),performanceMinVar[1:190]) 
Results$performanceMW5Jahre=c(rep(0,36),performanceMW[1:190])                    


Results$performanceGG5Jahre80=c(rep(0,36),performanceGG[1:190])                    
Results$performanceMinVar5Jahre80=c(rep(0,36),performanceMinVar[1:190]) 
Results$performanceMW5Jahre80=c(rep(0,36),performanceMW[1:190])                    


Results$Bench_performanceGG=Bench_performanceGG[1:226]
Results$Bench_performanceMinVar=Bench_performanceMinVar[1:226]

Results$CumperformanceGG=cumprod(performanceGG[1:249])                    
Results$CumperformanceMinVar=cumprod(performanceMinVar[1:249]) 
Results$CumBench_performanceGG=cumprod(Bench_performanceGG[1:249])
Results$CumBench_performanceMinVar=cumprod(Bench_performanceMinVar[1:249])

write.table(Results, file="Daten/5_ResultsNormal5Jahre80.csv", row.names = F,dec=",",sep=";")

#Transaktionskosten
transkostenGG=c()
transkostenMV=c()
transkostenMW=c()

for (i in 72:297){
  transkostenGG=append(transkostenGG, abs(sum(abs(GewichtungGG[i,2:228]-GewichtungGG[i+1,2:228]))))
  transkostenMV=append(transkostenMV, abs(sum(abs(GewichtungMinVar[i,2:228]-GewichtungMinVar[i+1,2:228]))))
  transkostenMW=append(transkostenMW, abs(sum(abs(GewichtungMW[i,2:228]-GewichtungMW[i+1,2:228]))))
}

Results$transkostenGG = c(rep(0,36),transkostenGG[1:190])
Results$transkostenGMinVar = c(rep(0,36),transkostenMV[1:190])
Results$transkostenMW=c(rep(0,36),transkostenMW[1:190])



#Data Preprocessing

#Working Directory zuhause und Uni
setwd("C:/Users/shook/Google Drive/Bachelor Thesis/Empirisches Modell")
setwd("H:/Bachelor Thesis/Empirische Umsetzung")

#Restart ACHTUNG löscht alle Daten 
rm(list=ls(all=TRUE))

#Libraries

#Schritt 1: Tägliche Kurse zu täglichen & monatl. Performancewerten
kurseTaegl = read.csv("Daten/0_Kurse.csv", sep =",", dec = ".", stringsAsFactors=FALSE)
komposition = read.csv("Daten/0_Komposition.csv", sep =";", dec = ",", stringsAsFactors=FALSE)


#Dummy für Perf Array 
Perf = array(NA,dim=c(40000,61))

#Stellen der Monatswechsel bestimmen
monthchangepos=c()

for (c in 2:nrow(kurseTaegl)){
  if (as.numeric(format(as.Date(kurseTaegl[c,1],origin="1899-12-30"), "%m"))!=as.numeric(format(as.Date(kurseTaegl[c-1,1],origin="1899-12-30"), "%m"))){
    monthchangepos=append(monthchangepos,c)
  }
}
#Für später (neuralnet backtest ergebnisse)
ergebnismatrix = kurseTaegl[monthchangepos,]
ergebnismatrix[,2:228] = NA
write.csv(ergebnismatrix, file="Daten/0_ergebnismatrix_blank.csv",row.names = F)

#Performance bestimmen

count=1
for (c in 62:(length(monthchangepos)-1)){
#for (c in 14:(length(monthchangepos)-1)){
  for(i in 2:228){
    #if (sum(is.na(kurseTaegl[monthchangepos[c-13]:monthchangepos[c+1],i]))==0 & kurseTaegl[monthchangepos[c],i]>5){
    if (sum(is.na(kurseTaegl[monthchangepos[c-61]:monthchangepos[c+1],i]))==0 & kurseTaegl[monthchangepos[c],i]>5){
        
      Perf[count,1]=kurseTaegl[monthchangepos[c],1]
      Perf[count,2]=colnames(kurseTaegl)[i]
      Perf[count,35]=as.numeric(as.numeric(format(as.Date(kurseTaegl[monthchangepos[c],1],origin="1899-12-30"), "%m"))==1)
      Perf[count,36]=1+(kurseTaegl[monthchangepos[c+1],i]-kurseTaegl[monthchangepos[c],i])/kurseTaegl[monthchangepos[c],i]
      
      Perf[count,37]=as.numeric(komposition[komposition[,1]==as.numeric(format(as.Date(kurseTaegl[monthchangepos[c],1],origin="1899-12-30"), "%Y")),i]==1)
     
      Perf[count, 3]=as.numeric(1+(kurseTaegl[monthchangepos[c-12],i]-kurseTaegl[monthchangepos[c-13],i])/kurseTaegl[monthchangepos[c-13],i])
      
      for(tminus in 2:12){
        Perf[count, 2+tminus]=as.numeric(Perf[count, 1+tminus])*(1+(kurseTaegl[monthchangepos[c-13+tminus],i]-kurseTaegl[monthchangepos[c-14+tminus],i])/kurseTaegl[monthchangepos[c-14+tminus],i])
      }
      Perf[count, 15]=1+(kurseTaegl[monthchangepos[c]-19,i]-kurseTaegl[monthchangepos[c]-20,i])/kurseTaegl[monthchangepos[c]-20,i]
      for(tminus in 1:19){
        Perf[count, 15+tminus]=as.numeric(Perf[count, 14+tminus])*(1+(kurseTaegl[monthchangepos[c]-19+tminus,i]-kurseTaegl[monthchangepos[c]-20+tminus,i])/kurseTaegl[monthchangepos[c]-20+tminus,i])
      }
      
      Perf[count, 38]=as.numeric(1+(kurseTaegl[monthchangepos[c-60],i]-kurseTaegl[monthchangepos[c-61],i])/kurseTaegl[monthchangepos[c-61],i])
      
      for(tminus in 2:24){
        Perf[count, 37+tminus]=as.numeric(Perf[count, 36+tminus])*(1+(kurseTaegl[monthchangepos[c-61+tminus],i]-kurseTaegl[monthchangepos[c-62+tminus],i])/kurseTaegl[monthchangepos[c-62+tminus],i])
      }
      
      count=count+1
    }
  }
}

performanceDF = data.frame(monat=integer(count-1),stringsAsFactors = F)
                           
#Übertrag in ein Dataframe zur weiterverarbeitung

performanceDF$monat=as.numeric(Perf[1:count-1,1])
performanceDF$titel=Perf[1:count-1,2]
performanceDF$monatl1=as.numeric(Perf[1:count-1,3])
performanceDF$monatl2=as.numeric(Perf[1:count-1,4])
performanceDF$monatl3=as.numeric(Perf[1:count-1,5])
performanceDF$monatl4=as.numeric(Perf[1:count-1,6])
performanceDF$monatl5=as.numeric(Perf[1:count-1,7])
performanceDF$monatl6=as.numeric(Perf[1:count-1,8])
performanceDF$monatl7=as.numeric(Perf[1:count-1,9])
performanceDF$monatl8=as.numeric(Perf[1:count-1,10])
performanceDF$monatl9=as.numeric(Perf[1:count-1,11])
performanceDF$monatl10=as.numeric(Perf[1:count-1,12])
performanceDF$monatl11=as.numeric(Perf[1:count-1,13])
performanceDF$monatl12=as.numeric(Perf[1:count-1,14])
performanceDF$taegl1=as.numeric(Perf[1:count-1,15])
performanceDF$taegl2=as.numeric(Perf[1:count-1,16])
performanceDF$taegl3=as.numeric(Perf[1:count-1,17])
performanceDF$taegl4=as.numeric(Perf[1:count-1,18])
performanceDF$taegl5=as.numeric(Perf[1:count-1,19])
performanceDF$taegl6=as.numeric(Perf[1:count-1,20])
performanceDF$taegl7=as.numeric(Perf[1:count-1,21])
performanceDF$taegl8=as.numeric(Perf[1:count-1,22])
performanceDF$taegl9=as.numeric(Perf[1:count-1,23])
performanceDF$taegl10=as.numeric(Perf[1:count-1,24])
performanceDF$taegl11=as.numeric(Perf[1:count-1,25])
performanceDF$taegl12=as.numeric(Perf[1:count-1,26])
performanceDF$taegl13=as.numeric(Perf[1:count-1,27])
performanceDF$taegl14=as.numeric(Perf[1:count-1,28])
performanceDF$taegl15=as.numeric(Perf[1:count-1,29])
performanceDF$taegl16=as.numeric(Perf[1:count-1,30])
performanceDF$taegl17=as.numeric(Perf[1:count-1,31])
performanceDF$taegl18=as.numeric(Perf[1:count-1,32])
performanceDF$taegl19=as.numeric(Perf[1:count-1,33])
performanceDF$taegl20=as.numeric(Perf[1:count-1,34])
performanceDF$jan=as.numeric(Perf[1:count-1,35])
performanceDF$outp=as.numeric(Perf[1:count-1,36])
performanceDF$inIndex=as.numeric(Perf[1:count-1,37])
performanceDF$monatl61=as.numeric(Perf[1:count-1,61])
performanceDF$monatl60=as.numeric(Perf[1:count-1,60])
performanceDF$monatl59=as.numeric(Perf[1:count-1,59])
performanceDF$monatl58=as.numeric(Perf[1:count-1,58])
performanceDF$monatl57=as.numeric(Perf[1:count-1,57])
performanceDF$monatl56=as.numeric(Perf[1:count-1,56])
performanceDF$monatl55=as.numeric(Perf[1:count-1,55])
performanceDF$monatl54=as.numeric(Perf[1:count-1,54])
performanceDF$monatl53=as.numeric(Perf[1:count-1,53])
performanceDF$monatl52=as.numeric(Perf[1:count-1,52])
performanceDF$monatl51=as.numeric(Perf[1:count-1,51])
performanceDF$monatl50=as.numeric(Perf[1:count-1,50])
performanceDF$monatl49=as.numeric(Perf[1:count-1,49])
performanceDF$monatl48=as.numeric(Perf[1:count-1,48])
performanceDF$monatl47=as.numeric(Perf[1:count-1,47])
performanceDF$monatl46=as.numeric(Perf[1:count-1,46])
performanceDF$monatl45=as.numeric(Perf[1:count-1,45])
performanceDF$monatl44=as.numeric(Perf[1:count-1,44])
performanceDF$monatl43=as.numeric(Perf[1:count-1,43])
performanceDF$monatl42=as.numeric(Perf[1:count-1,42])
performanceDF$monatl41=as.numeric(Perf[1:count-1,41])
performanceDF$monatl40=as.numeric(Perf[1:count-1,40])
performanceDF$monatl39=as.numeric(Perf[1:count-1,39])
performanceDF$monatl38=as.numeric(Perf[1:count-1,38])

#Überprüfung is dürfen keine NAs auftreten
sum(is.na(performanceDF[1:count-1,]))
write.csv(performanceDF, file="Daten/1_Performance5Jahre.csv",row.names = F)

#Schritt 2: Z Score berechnung
#kurse = read.csv("Daten/1_Performance.csv", sep =",", dec = ".")
kurse = performanceDF
Zscores_Final = performanceDF
zscores = array(NA,dim=c(nrow(kurse),61))
normalize=function(x){(x-min(x))/(max(x)-min(x))}

for (b in c(3:34,38:61)){
  m1=mean(kurse[which(kurse$monat==kurse[1,1]), b])
  std=sd(kurse[which(kurse$monat==kurse[1,1]), b])
  #meanT1=mean(kurse[which(kurse$monat==kurse[1,1]), 36])
  for (a in 1:nrow(kurse)){
    zscores[a,b]=(kurse[a,b]-m1)/std
    if (b==3){
      zscores[a,35]=kurse[a,35]
      
      #zscores[a,36]=as.numeric(kurse[a,36]>median(kurse[which(kurse$monat==kurse[a,1]), 36]))
      
      #if(kurse[a,36]>quantile(kurse[which(kurse$monat==kurse[a,1]), 36],.8)){
      #  zscores[a,36]=1  
      #}else{
      #  zscores[a,36]=0
      #}
      
      if(kurse[a,36]>quantile(kurse[which(kurse$monat==kurse[a,1]), 36],.8)){
        zscores[a,36]=2  
      }else if(kurse[a,36]>quantile(kurse[which(kurse$monat==kurse[a,1]), 36],.2)){
        zscores[a,36]=1
      }else{
        zscores[a,36]=0
      }
      
    }
    if (a<nrow(kurse)){
      if (kurse[a+1,1]!=kurse[a,1]){
        #meanT1=mean(kurse[which(kurse$monat==kurse[a,1]), 36])
        m1=mean(kurse[which(kurse$monat==kurse[a+1,1]), b])
        std=sd(kurse[which(kurse$monat==kurse[a+1,1]), b])
        zscores[which(kurse$monat==kurse[a,1]),b] = normalize(zscores[which(kurse$monat==kurse[a,1]),b])
      }
    }
    if (a==nrow(kurse)){
        zscores[which(kurse$monat==kurse[a,1]),b] = normalize(zscores[which(kurse$monat==kurse[a,1]),b])
    }
  }
  Zscores_Final[,b]=zscores[,b]
}

Zscores_Final[,35]=zscores[,35]
Zscores_Final[,36]=zscores[,36]
Zscores_Final[,37]=kurse$inIndex

#NAs aufgrund von einem "Handelstag" ohne Aktivität (keine Kursänderung --> Keine Standardabw --> Div durch 0 )
sum(is.na(Zscores_Final[]))

#NAs werden als 0.5 (neutral) weitergeführt
#Zscores_Final[is.na(Zscores_Final[])] = 0.5

write.csv(Zscores_Final, file="Daten/2_ZScoresNorm5Jahre3Klassen.csv", row.names = F)

#Damit ist die Vorverarbeitung abgeschlossen. Die Zscore dienen als Input für das neurale Netz

#Speicher bereinigen, nur die KurseTaegl und ZScore werden weiter benötigt
rm(Perf, performanceDF, zscores, kurse, komposition,)

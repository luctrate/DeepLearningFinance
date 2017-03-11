# Backtest
# Finale Version mit Autoencoder
# Working Directory zuhause und Uni
setwd("C:/Users/shook/Google Drive/Bachelor Thesis/Empirisches Modell")
setwd("H:/Bachelor Thesis/Empirische Umsetzung")

#Restart ACHTUNG löscht alle Daten im Workspace
rm(list=ls(all=TRUE))
#Shutdown der h2o Server instanz
h2o.shutdown()
Y
#Initialisieren der h2o Instanz mit N-1 verfügbaren Threads
h2o.init(nthreads = -1, enable_assertions = FALSE)

#Verwendete Packete. Installation
#install.packages("h2o")
#install.packages("SDMTools")

#Verwendete Packete. Lden
library(h2o)
library(SDMTools)

#1. Dateninput
Zscores_Final = read.csv("Daten/2_ZScoresNorm5Jahre80Percentil.csv", sep =",", dec = ".",stringsAsFactors = F)
#Umwandeln der Ergebnisvariable als Faktor
Zscores_Final$outp = as.factor(Zscores_Final$outp)
Zscores_Final=Zscores_Final[c(1:34,38:61,35:37)]
#Initiales training bis 12 00
format(as.Date(Zscores_Final[4953,1],origin="1899-12-30"), "%m, %y")
format(as.Date(Zscores_Final[4954,1],origin="1899-12-30"), "%m, %y")

#Seed setzten um Ergebnisse replizieren zu können, Train und TEst set splitten
set.seed(12)
test_ind = sample(nrow(Zscores_Final[1:4953,]), size = 1000)

test_init = Zscores_Final[test_ind, ]
train_init = Zscores_Final[1:4953,][-test_ind,]

#in h2o Frames umwandeln
train_init_h2o = as.h2o(train_init)
test_init_h2o = as.h2o(test_init)

#2. Hyperparametersearch. UmSicherzustelen das passende Hyperparameter 
#verwendet werden wird zunächst diese Suche durchgeführt.
#Auchmit gesetzten Seed ist es nicht gewährleistet immer das selbe Ergebnis zu erhalten.
search_criteria = list(strategy = "RandomDiscrete", max_runtime_secs = 360, max_models = 100, seed=1234567, stopping_rounds=5, stopping_tolerance=1e-2)

hyper_params <- list(
  activation=c("Rectifier","Tanh","Maxout","RectifierWithDropout","TanhWithDropout","MaxoutWithDropout"),
  input_dropout_ratio=c(0,0.02,0.03,0.04,0.05),
  l1=seq(0,1e-4,1e-6),
  l2=seq(0,1e-4,1e-6),
  hidden=list(c(60,24,6,54),c(55,20,5,52),c(50,24,4,50)),
  stopping_metric = c("logloss","AUC","misclassification")
)

dl_random_grid <- h2o.grid(
  algorithm="deeplearning",
  grid_id = "dl_grid_random",
  training_frame=train_init_h2o,
  validation_frame=test_init_h2o, 
  x=colnames(train_init[,3:59]), 
  y="outp",
  epochs=3,
  stopping_tolerance=1e-3,        
  stopping_rounds=2,
  score_duty_cycle=0.025,         
  max_w2=10,                      
  balance_classes=TRUE,
  score_validation_sampling="Stratified",
  hyper_params = hyper_params,
  search_criteria = search_criteria
  #export_weights_and_biases = T
)                                
grid <- h2o.getGrid("dl_grid_random",sort_by="logloss",decreasing=FALSE)
grid

grid@summary_table[1,]
#Hyper-Parameter Search Summary: ordered by increasing logloss
#activation          hidden input_dropout_ratio     l1     l2 stopping_metric
#1       Tanh [55, 20, 5, 52]                0.04 6.9E-5 7.2E-5             AUC
#model_ids            logloss
#1 dl_grid_random_model_0 0.6896824224293736
#best_model <- h2o.getModel(grid@model_ids[[1]]) ## model with lowest logloss

#Der folgende Autoencoder wird mit den oben genannten Hyperparams erstellt.


write.csv(train_init[,3:60], file="Daten/2_Train5Jahre.csv",row.names=FALSE)
write.csv(test_init[,3:60], file="Daten/2_Test5Jahre.csv",row.names=FALSE)
#Bei dem Rückgabewert handelt es sich nur um die Gewichte des Autoencoders
stacked_Model_weights = check.deeplearning_stacked_autoencoder()

#Weiteres Trainieren mit Backprop
w1 = c(h2o.weights(stacked_Model_weights[[1]],matrix_id=1))
w2 = c(h2o.weights(stacked_Model_weights[[2]],matrix_id=1))
w3 = c(h2o.weights(stacked_Model_weights[[3]],matrix_id=1))
w4 = c(h2o.weights(stacked_Model_weights[[4]],matrix_id=1))
w5 = c(h2o.weights(stacked_Model_weights[[4]],matrix_id=2))
#w4 = c(h2o.weights(stacked_Model_weights[[3]],matrix_id=2))

b1 = c(h2o.biases(stacked_Model_weights[[1]],vector_id=1))
b2 = c(h2o.biases(stacked_Model_weights[[2]],vector_id=1))
b3 = c(h2o.biases(stacked_Model_weights[[3]],vector_id=1))
b4 = c(h2o.biases(stacked_Model_weights[[4]],vector_id=1))
b5 = c(h2o.biases(stacked_Model_weights[[4]],vector_id=2))
#b4 = c(h2o.biases(stacked_Model_weights[[3]],vector_id=2))

#nntrain = h2o.deeplearning(x=3:35,
nntrain = h2o.deeplearning(x=3:59,
                           y="outp",
                           training_frame=train_init_h2o,
                           validation_frame=test_init_h2o,
                           hidden=c(55,20,5,52),
                           activation="Tanh",
                           epochs = 2, 
                           l1=6.9e-05, 
                           l2=7.2e-05,
                           input_dropout_ratio=0.04,
                           initial_weights=c(w1[[1]],w2[[1]],w3[[1]],w4[[1]],w5[[1]]),
                           initial_biases=c(b1[[1]],b2[[1]],b3[[1]],b4[[1]],b5[[1]]),
                           export_weights_and_biases = T)



#Folgend beginnt das iterative trainieren und testen
#
#Stellen der Monatswechsel bestimmen
monthchangepos = c()

for (c in 2:nrow(Zscores_Final)){
  if (as.numeric(format(as.Date(Zscores_Final[c,1],origin="1899-12-30"), "%m"))!=as.numeric(format(as.Date(Zscores_Final[c-1,1],origin="1899-12-30"), "%m"))){
    monthchangepos = append(monthchangepos,c)
  }
}

ergebnismatrix = read.csv("Daten/0_ergebnismatrix_blank.csv", sep =",", dec = ".")
ergebnismatrix_buyable = ergebnismatrix

nntrain_iterativ = nntrain
#arrays für Kennzahlen
Accuracy = c()
Misclass_Rate = c()
TP_Rate = c()
FP_Rate = c()
Specificity = c()
Precision = c()
Prevalence = c()

#klassifikationgenauigkeit der Top 20
TPRate = c()
confGesamt = matrix(c(rep(0,4)),ncol=2)

for (m in 46:length(monthchangepos)) {
#for (m in 34:44) {
  print(paste0("M: ", m, " Max:",length(monthchangepos)))
  
  test = Zscores_Final[monthchangepos[m]:(monthchangepos[m+1]-1),]
  test$jan[1] = if (test$jan[1]==0) 0.01 else 0.99
  #test[1,c(F,F,apply(test[3:34], 2, var, na.rm=TRUE) == 0)] = 0.49
  test[1,c(F,F,apply(test[3:58], 2, var, na.rm=TRUE) == 0)] = 0.49
  
  test_h2o = as.h2o(test)
  test$predict = as.matrix(h2o.predict(nntrain_iterativ, test_h2o)[,3])
  #test$predict = as.matrix(h2o.predict(nntrain_iterativ, test_h2o)[,4]-h2o.predict(nntrain_iterativ, test_h2o)[,2])
  #tempvar=as.matrix(as.numeric(h2o.predict(nntrain_iterativ, test_h2o)))
  #tempPredict=c(rep(NA,nrow(tempvar)))
  #tempPredict[(tempvar[,4]-tempvar[,2])>quantile((tempvar[,4]-tempvar[,2]),.8)]=2
  #tempPredict[tempvar[,2]>quantile(tempvar[,2],.775)&is.na(tempPredict)]=0
  #tempPredict[is.na(tempPredict)]=1
  
  #füllen der Ergebnismatrix
  for (i in 1:nrow(test$predict)){
    ergebnismatrix[which(ergebnismatrix[,1]==test$monat[i]),which(colnames(ergebnismatrix)==test$titel[i])] = test$predict[i]
    ergebnismatrix_buyable[which(ergebnismatrix[,1]==test$monat[i]),which(colnames(ergebnismatrix)==test$titel[i])] = if (test$inIndex[i]==1) test$predict[i] else NA
  }
  
  #conf = confusion.matrix(test$outp,test$predict,threshold = median(test$predict))
  conf = confusion.matrix(test$outp,test$predict,threshold = quantile(test$predict,.8))
  #conf = table(tempPredict,test$outp)
  confGesamt[1] = confGesamt[1] + conf[1,1]
  confGesamt[2] = confGesamt[2] + conf[2,1]
  confGesamt[3] = confGesamt[3] + conf[1,2]
  confGesamt[4] = confGesamt[4] + conf[2,2]
  #confGesamt[5] = confGesamt[5] + conf[2,2]
  #confGesamt[6] = confGesamt[6] + conf[3,2]
  #confGesamt[7] = confGesamt[7] + conf[1,3]
  #confGesamt[8] = confGesamt[8] + conf[2,3]
  #confGesamt[9] = confGesamt[9] + conf[3,3]
  #Berechnung der conf kennzahlen
  Accuracy = append(Accuracy, ((conf[2,2]+conf[1,1])/sum(conf)))
  #Misclass_Rate = append(Misclass_Rate, ((conf[2,1]+conf[1,2])/sum(conf)))
  #TP_Rate = append(TP_Rate, (conf[2,2]/(conf[1,2]+conf[2,2])))
  #FP_Rate = append(FP_Rate, (conf[2,1]/(conf[1,2]+conf[2,2])))
  #Specificity = append(Specificity, (conf[1,1]/(conf[1,2]+conf[2,2])))
  
  Precision = append(Precision, (conf[2,2]/(conf[2,1]+conf[2,2])))
  #Prevalence = append(Prevalence, ((conf[1,2]+conf[2,2])/sum(conf)))
  print(paste0("precision: ", tail(Precision, n=1)))
  print(paste0("Accuracy: ", tail(Accuracy, n=1)))
  
  #top20
  #top20 = test[order(test$predict, decreasing = T)[1:(20)],c(1,2,36,60)]
  top20 = test[order(test$predict, decreasing = T)[1:(20)],c(1,2,36,60)]
  
  TPRate = append(TPRate,sum(as.numeric(top20$outp)-1)/20 )
  #TPRate = append(TPRate,sum(as.numeric(top20$outp==2))/20 )
  
  print(paste0("TPRate: ", tail(TPRate, n=1)))

  w1 = append(w1, h2o.weights(nntrain_iterativ,matrix_id=1))
  w2 = append(w2, h2o.weights(nntrain_iterativ,matrix_id=2))
  w3 = append(w3, h2o.weights(nntrain_iterativ,matrix_id=3))
  w4 = append(w4, h2o.weights(nntrain_iterativ,matrix_id=4))
  w5 = append(w5, h2o.weights(nntrain_iterativ,matrix_id=5))
  b1 = append(b1, h2o.biases(nntrain_iterativ,vector_id=1))
  b2 = append(b2, h2o.biases(nntrain_iterativ,vector_id=2))
  b3 = append(b3, h2o.biases(nntrain_iterativ,vector_id=3))
  b4 = append(b4, h2o.biases(nntrain_iterativ,vector_id=4))
  b5 = append(b5, h2o.biases(nntrain_iterativ,vector_id=5))
  nntrain_iterativ = h2o.deeplearning(x=3:59,
                                     y="outp",
                                     training_frame=test_h2o,
                                     #hidden=c(40,4,50),
                                     hidden=c(55,20,5,52), 
                                     activation="Tanh", 
                                     epochs=2, 
                                     l1=6.9e-5,
                                     input_dropout_ratio=0.04,
                                     l2=7.2e-05,
                                     initial_weights=c(tail(w1, n=1)[[1]],tail(w2, n=1)[[1]],tail(w3, n=1)[[1]],tail(w4, n=1)[[1]],tail(w5, n=1)[[1]]),
                                     initial_biases=c(tail(b1, n=1)[[1]],tail(b2, n=1)[[1]],tail(b3, n=1)[[1]],tail(b4, n=1)[[1]],tail(b5, n=1)[[1]]),
                                     export_weights_and_biases = T)
}

median(TPRate)
mean(TPRate)
confGesamt
#kennzahlen=data.frame(monat=integer(190))
kennzahlen$Tp5Jahre80=c(rep(0,36),TPRate)
kennzahlen$Acc5Jahre80=c(rep(0,36),Accuracy)
kennzahlen$Prec5Jahre80=c(rep(0,36),Precision)

#confmatrixall=c()
confmatrixall=append(confmatrixall, confGesamt)

write.csv(ergebnismatrix_buyable, file="Daten/3_Ergebnismatrix_buyable5Jahre80.csv",row.names=FALSE)
write.csv(ergebnismatrix, file="Daten/3_Ergebnismatrix5Jahre80.csv",row.names=FALSE)
write.table(kennzahlen, file="Daten/5_Kennzahlen5Jahre80.csv", row.names = F,dec=",",sep=";")
mean(Accuracy)
plot(Accuracy)
plot(Precision)
plot(TPRate)
kennzahlen=data.frame()

#Funktion für die Erstellung des Autoencoders
check.deeplearning_stacked_autoencoder <- function() {  
  # this function builds a vector of autoencoder models, one per layer
  get_stacked_ae_array <- function(training_data,layers,args){  
    vector <- c()
    index = 0
    for(i in 1:length(layers)){    
      index = index + 1
      ae_model <- do.call(h2o.deeplearning, 
                          modifyList(list(x=names(training_data),
                                          training_frame=training_data,
                                          autoencoder=T,
                                          hidden=layers[i]),
                                     args))
      training_data = h2o.deepfeatures(ae_model,training_data,layer=1)
      
      names(training_data) <- gsub("DF", paste0("L",index,sep=""), names(training_data)) 
      #print(h2o.weights(ae_model,matrix_id=1))
      vector <- c(vector, ae_model)    
    }
    vector
  }
  
  # this function returns final encoded contents
  apply_stacked_ae_array <- function(data,ae){
    index = 0
    for(i in 1:length(ae)){
      index = index + 1
      data = h2o.deepfeatures(ae[[i]],data,layer=1)
      names(data) <- gsub("DF", paste0("L",index,sep=""), names(data)) 
    }
    data
  }
  
  TRAIN <- "Daten/2_Train5Jahre.csv"
  TEST <- "Daten/2_Test5Jahre.csv"
  response <- 58
  
  #set to T for RUnit
  #set to F for stand-alone demo
  if (F) {
    train_hex <- h2o.importFile(locate(TRAIN))
    test_hex  <- h2o.importFile(locate(TEST ))
  } else {
    #library(h2o)
    #h2o.init(nthreads=-1)
    homedir <- "C:/Users/shook/Google Drive/Bachelor Thesis/Empirisches Modell/" #modify if needed
    train_hex <- h2o.importFile(path = paste0(homedir,TRAIN), header = T, sep = ',')
    test_hex  <- h2o.importFile(path = paste0(homedir,TEST), header = T, sep = ',')
  }
  
  train <- train_hex[,-response]
  test  <- test_hex [,-response]
  train_hex[,response] <- as.factor(train_hex[,response])
  test_hex [,response] <- as.factor(test_hex [,response])
  
  ## Build reference model on full dataset and evaluate it on the test set
  #model_ref <- h2o.deeplearning(training_frame=train_hex, x=1:(ncol(train_hex)-1), y=response, hidden=c(40,4,50), epochs=3)
  #p_ref <- h2o.performance(model_ref, test_hex)
  #print(h2o.logloss(p_ref))
  
  ## Now build a stacked autoencoder model with three stacked layer AE models
  ## First AE model will compress the 717 non-const predictors into 200
  ## Second AE model will compress 200 into 100
  ## Third AE model will compress 100 into 50
  layers <- c(55,20,5)
  args <- list(activation="Tanh", epochs=2, l1=6.9e-5,
               input_dropout_ratio=0.04,
               l2=7.2e-05,export_weights_and_biases = T
  )
  ae <- get_stacked_ae_array(train, layers, args)
  bc=ae
  ## Now compress the training/testing data with this 3-stage set of AE models
  train_compressed <- apply_stacked_ae_array(train, ae)
  test_compressed <- apply_stacked_ae_array(test, ae)
  
  ## Build a simple model using these new features (compressed training data) and evaluate it on the compressed test set.
  train_w_resp <- h2o.cbind(train_compressed, train_hex[,response])
  test_w_resp <- h2o.cbind(test_compressed, test_hex[,response])
  model_on_compressed_data <- h2o.deeplearning(training_frame=train_w_resp, 
                                               x=1:(ncol(train_w_resp)-1), 
                                               y=ncol(train_w_resp), 
                                               hidden=c(52), 
                                               activation="Tanh", 
                                               epochs=2, 
                                               l1=6.9e-5,
                                               input_dropout_ratio=0.04,
                                               l2=7.2e-05,
                                               export_weights_and_biases = T)
  p <- h2o.performance(model_on_compressed_data, test_w_resp)
  print(h2o.logloss(p))
  return(bc=c(bc, model_on_compressed_data))
}





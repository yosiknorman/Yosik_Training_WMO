# library(MASS)
library(MLmetrics)
library(verification)
Tugas = read.csv2("Tugas.csv")
Tugas2 = Tugas
Tugas[Tugas == 0] = 0.000001
# Tugas2[Tugas == 0] = 0.001
# ini0 = unique(which(Tugas <= 20, arr.ind = T)[, 1])
# Tugas = Tugas[-ini0, ]
MAPE1 = MAPE(y_pred = as.numeric(Tugas$Model1), y_true = as.numeric(Tugas$GSMaP))
MAPE2 = MAPE(Tugas$Model2, Tugas$GSMaP)
MAPE3 = MAPE(Tugas$Model3, Tugas$GSMaP)

MAE1 = MAE(y_pred = as.numeric(Tugas$Model1), y_true = as.numeric(Tugas$GSMaP))
MAE2 = MAE(Tugas$Model2, Tugas$GSMaP)
MAE3 = MAE(Tugas$Model3, Tugas$GSMaP)

MSE1 = MSE(y_pred = as.numeric(Tugas$Model1), y_true = as.numeric(Tugas$GSMaP))
MSE2 = MSE(Tugas$Model2, Tugas$GSMaP)
MSE3 = MSE(Tugas$Model3, Tugas$GSMaP)

Stats = data.frame(MAE = c(MAE1, MAE2, MAE3), MSE = c(MSE1, MSE2, MSE3), MAPE = c(MAPE1, MAPE2, MAPE3))

Stats =  round(Stats, 2)

# ?verification::verify()

obs <- round(runif(100, 1,20) )
pred <- round(runif(100, 1,20) )

# A<- verify(obs, pred, frcst.type = "binary", obs.type = "binary" , thresholds = 2)
# summary(A)

pdf(file = "PDENS.pdf", height = 6, width = 7)
par(mfrow = c(2, 2))
plot(density(Tugas$GSMaP), xlim=c(-40, 180), ylim=c(0, 0.3),main = "GSMAP Analysis", xlab = "Precipitation (mm)")
# lines(density(Tugas$Model1), col = "red")
# lines(density(Tugas$Model2), col = "Green")
# lines(density(Tugas$Model3), col = "Blue")


plot(density(Tugas$Model1), xlim=c(-40, 180), ylim=c(0, 0.3), main = "Model 1", xlab = "Precipitation (mm)")
plot(density(Tugas$Model2), xlim=c(-40, 180), ylim=c(0, 0.3), main = "Model 2", xlab = "Precipitation (mm)")
plot(density(Tugas$Model3), xlim=c(-40, 180), ylim=c(0, 0.3), main = "Model 3", xlab = "Precipitation (mm)")
dev.off()
# max(Tugas$GSMaP)
TP1 = length(which(Tugas$Model1 >= 20 &Tugas$GSMaP >= 20))
TP2 = length(which(Tugas$Model2 >= 20 &Tugas$GSMaP >= 20))
TP3 = length(which(Tugas$Model3 >= 20 &Tugas$GSMaP >= 20))

FN1 = length(which(Tugas$Model1 < 20 &Tugas$GSMaP >= 20))
FN2 = length(which(Tugas$Model2 < 20 &Tugas$GSMaP >= 20))
FN3 = length(which(Tugas$Model3 < 20 &Tugas$GSMaP >= 20))

TN1 = length(which(Tugas$Model1 < 20 &Tugas$GSMaP < 20))
TN2 = length(which(Tugas$Model2 < 20 &Tugas$GSMaP < 20))
TN3 = length(which(Tugas$Model3 < 20 &Tugas$GSMaP < 20))

FP1 = length(which(Tugas$Model1 >= 20 &Tugas$GSMaP < 20))
FP2 = length(which(Tugas$Model2 >= 20 &Tugas$GSMaP < 20))
FP3 = length(which(Tugas$Model3 >= 20 &Tugas$GSMaP < 20))

ACC1 = (TP1+TN1)/sum(TP1, TN1, FP1,FN1 )
ACC2 = (TP2+TN2)/sum(TP2, TN2, FP2,FN2 )
ACC3 = (TP3+TN3)/sum(TP3, TN3, FP3,FN3 )

POD1 = TP1/(TP1 + FN1)
POD2 = TP2/(TP2 + FN2)
POD3 = TP3/(TP3 + FN3)

FAR1 = FP1/(TP1+FP1)
FAR2 = FP2/(TP2+FP2)
FAR3 = FP3/(TP3+FP3)

Ver = data.frame(Accuracy = c(ACC1, ACC2, ACC3),  POD = c(POD1, POD2, POD3) ,FAR = c(FAR1, FAR2, FAR3))
row.names(Ver) = c("Model_1","Model_2",  "Model_3")

write.csv2(Ver, file = "Ver.csv")


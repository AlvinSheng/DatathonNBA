# ELastic net Regression
number = round(nrow(sal_season)/10)
sal_lm_en <- lm(season17_18 ~ TmBOS + TmBRK + TmCHI + TmCLE + TmDAL + TmGSW + TmHOU + TmIND + TmLAC + TmLAL + TmMEM + TmMIA + TmMIN + TmNOP + TmOKC + TmORL + TmPHI + TmPOR + TmSAC + TmSAS + TmTOR + TmTOR + TmUTA + TmWAS + PosPG + PosSF + PosSG + Age + G + GS + TS. + X3PAr + STL. + USG. + DWS + WS + X3P + X3P. + X2P. + FT. + AST + PF, data = sal_season[-c(1:number),])
pred = predict(sal_lm_en, newx = sal_season[c(1:number),])
mean = mean(sal_season$season17_18[1:number])
error = sum((pred - mean)^2)


#PCA
number = round(nrow(as.data.frame(pc_dat))/10)
kk = as.data.frame(pc_dat)
sal_lm_pc <- lm(sal[-c(1:number)] ~ ., data = kk[-c(1:number),])
pred = predict(sal_lm_pc,newx=kk[c(1:number),])
mean = mean(sal[1:number])
error = sum((pred - mean)^2)

#PCA regularized
number = round(nrow(pc_dat)/10)
cv_lm <- cv.glmnet(pc_dat[-c(1:number),], sal[-c(1:number)], alpha = 1)
pred = predict(cv_lm,newx=pc_dat[c(1:number),])
mean = mean(sal[1:number])
error = sum((pred - mean)^2)

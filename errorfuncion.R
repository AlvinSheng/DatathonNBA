# ELastic net Regression
number = round(nrow(sal_season)/10)
cv <- cv.glmnet(sal_season_mat[-c(1:number),], sal[-c(1:number)], family = "gaussian", alpha = alphas[2], keep = TRUE)
pred = predict(cv,newx=sal_season_mat[c(1:number),])
mean = mean(sal_season[c(1:number),1])
error = sum((pred - mean)^2)


#PCA
number = round(nrow(as.data.frame(pc_dat))/10)
sal_lm_pc <- lm(sal ~ ., data = as.data.frame(pc_dat[-c(1:number),]))
pred = predict(sal_lm_pc,newx=as.data.frame(pc_dat)[c(1:number),])
mean = mean(as.data.frame(pc_dat)$sal[1:number])
error = sum((pred - mean)^2)

#PCA regularized
number = round(nrow(pc_dat)/10)
cv_lm <- cv.glmnet(pc_dat[-c(1:number)], sal[-c(1:number)], alpha = 1)
pred = predict(cv_lm,newx=pc_dat[c(1:number),])
mean = mean(pc_dat[c(1:number),]$sal)
error = sum((pred - mean)^2)
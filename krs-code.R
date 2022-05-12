

k<-matrix(0,dim(x.test)[1],dim(x.train)[1])
for (i in 1:dim(x.test)[1]) {
  for (j in 1:dim(x.train)[1]) {
    k[i,j]<-sqrt(sum((x.test[i,]-x.train[j,])^2))
  }
}
kx.test<-exp(-1/2*(1/(mean(k)/sqrt(2))^2)*k^2)
kx.train<-exp(-1/2*(1/(mean(dist(x.train))/sqrt(2))^2)*as.matrix(dist(x.train))^2)


#######krs#######
fit.krs <- MTPS(xmat = kx.train, ymat = y.train, family = "gaussian",
                cv = FALSE, residual = TRUE,
                method.step1 = glmnet1,
                method.step2 = glmnet.lasso) 
pred.krs <- predict(fit.krs, kx.test)
mse.krs<-apply((pred.krs-y.test)^2,2, mean)
mse.krs
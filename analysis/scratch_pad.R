
set.seed(666)

tt <- 70

ee <- rep(NA, tt)

cc <- matrix(NA, 100, 2)

for(i in 1:100) {
  
  dd <- sample(seq(2,13), tt, replace = TRUE)
  aa <- sample(seq(27,190), tt, replace = TRUE)
  
  for(t in 1:tt) {
    ee[t] <- sum(sample(seq(dd[t]), aa[t], replace = TRUE))
  }
  
  cc[i,1] <- cor(dd, ee)
  cc[i,2] <- cor(aa, ee)

}

## median & 90% quantiles
quantile(cc[,1], c(5, 50, 95)/100)
quantile(cc[,2], c(5, 50, 95)/100)

plot.ts(cbind(dd, aa, ee))

corrplot::corrplot(cor(cbind(dd, aa, ee)))


plot.ts(cbind(raw_data$days, raw_data$members))


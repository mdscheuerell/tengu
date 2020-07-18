
set.seed(666)

tt <- 70

ee <- rep(NA, tt)

dd <- sample(seq(5,10), tt, replace = TRUE)
aa <- sample(seq(100,150), tt, replace = TRUE)

for(t in 1:tt) {
  ee[t] <- sum(sample(seq(dd[t]), aa[t], replace = TRUE))
}





mm <- aa * dd/2

plot.ts(cbind(dd, aa, ee, mm))

corrplot::corrplot(cor(cbind(dd, aa, ee)))





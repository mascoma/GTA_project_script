data <- read.csv("phageinfo.csv", header = T)
head(data)
plot(data[,1],data[,3], xlab = "T", ylab = "genome size", xlim = c(0, 15),
     col= "cadetblue4", pch = 19, cex = 0.7, lty = "solid", lwd = 2)
text(data[,1], data[,3], labels=data[, 4], cex= 0.6, pos =2)

T_genome.lm = lm( genome_size ~ T , data = data)
summary(T_genome.lm)

abline(T_genome.lm, lty = 3, lwd = 3, col = "brown4")

Call:
  lm(formula = genome_size ~ T, data = data)

Residuals:
  Min     1Q Median     3Q    Max 
-14818 -10072  -6185   9553  35645 

Coefficients:
  Estimate Std. Error t value Pr(>|t|)    
(Intercept)   -67768      10087  -6.718 2.68e-06 ***
  T              17163       1214  14.133 3.47e-11 ***
  ---
  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Residual standard error: 15510 on 18 degrees of freedom
Multiple R-squared:  0.9173,  Adjusted R-squared:  0.9127 
F-statistic: 199.8 on 1 and 18 DF,  p-value: 3.474e-11
 
T3 <-  data[1:2, ]
T7 <- data[3:16, ]
T13 <- data[17:20,]
 
for (i in 1:100){
  T3index <- floor(runif(1, 1, 3))
  T7index <- floor(runif(1, 1, 15))
  T13index <- floor(runif(1, 1, 5))
  subdata<- rbind(T3[T3index, ], T7[T7index, ], T13[T13index, ])
  T_genome.lm = lm( genome_size ~ T , data = subdata)
  abline(T_genome.lm, lty = 3, lwd = 1, col = "darkslateblue")
}
summary(T_genome.lm)




b = 14995
a = -41343

b1 = 14995 - 4515 

b2 = 14995 + 4515

a1 =  -41343 - 39274

a2 = -41343 + 39274


x = 1
x = 3
x = 4
x = 7
x = 9
x = 13


y = b*x + a


 
y1 = b1*x + a1
y2 = b2*x + a2

 

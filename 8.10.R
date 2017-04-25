#------------------------ 8.10 ---------------------------------------------------

#---- (a)------

# Load NYSE data of JP Morgan, Citibank, Wells Fargo, Royal Dutch Shell, Exxon Mobil

stock.data<- read.table("T8-4.DAT")
head(stock.data)
n <- nrow(stock.data)
p <- ncol(stock.data)

# Construct Sample covariance matrix S

S <-cov(stock.data)

summary(stock.data)
# Obtain eigenvalues and eigenvectors of S

eigen(S)$values
eigen(S)$vectors

plot(eigen(S$values))
# Principle Components are given by
fit <- princomp(stock.data, cor = FALSE)
summary(fit)
plot(fit,type="l")






#---- (b)------

#-- Proportion of sample variance explained by first three PC:
var_p <- c()
for (i in 1:p)
{
  var_p[i]<-sum(eigen(S)$values[1:i])/sum(eigen(S)$values)
}
var_p
#---- (c)------

# Standard error for Bonferroni C.I
m=3
alpha = 0.10

denom <- abs(qnorm(1-alpha/(2*m)))*sqrt(2/n)

# Bonferroni Intervals for m=3 lambdas

Bon.CI <- matrix(NA, nrow = m, ncol = 2)
for (i in 1:m)
{
  Bon.CI[i,]<- c(eigen(S)$values[i]/(1+denom), eigen(S)$values[i]/(1-denom))
}

Bon.CI

#---- (d)------








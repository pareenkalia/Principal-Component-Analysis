# --------- 8.23 --------------

#Constructing covariance matrix from the textbook:

S <- matrix(NA, nrow= 6, ncol = 6)
S[upper.tri(S)]= c(1343.97, 731.54, 324.25, 1175.50, 537.35, 281.17, 162.68, 80.17, 39.15, 63.73, 238.37, 117.73, 56.80, 94.85, 13.88)
S <- t(S)
S[upper.tri(S)]= c(1343.97, 731.54, 324.25, 1175.50, 537.35, 281.17, 162.68, 80.17, 39.15, 63.73, 238.37, 117.73, 56.80, 94.85, 13.88)
diag(S)<- c(3266.46,721.91,179.28,474.98, 9.95, 21.26)

# S is given by:
S

# setting p = number of columns of S
p <- ncol(S)

# Eigenvalues and Eigenvectors of S
evalues.S <- eigen(S)$values
evalues.S

evectors.S <- eigen(S)$vectors
evectors.S

# proportion of variance explained by each PC using Covariance matrix S

prop.S <- c()
for(i in 1:p)
{
  prop.S[i] <- sum(evalues.S[1:i])/sum(evalues.S)
}

# Thus proportion of variance explained by each PC is given by:
prop.S


# Obtaining Correlation matrix R from covariance matrix S
R <- cov2cor(S)

# Eigenvalues and Eigenvectors of R
evalues.R <- eigen(R)$values
evalues.R

evectors.R <- eigen(R)$vectors
evectors.R


# proportion of variance explained by each PC using Correlation matrix R
prop.R <- c()
for(i in 1:p)
{
  prop.R[i] <- sum(evalues.R[1:i])/sum(evalues.R)
}

prop.R

# Principle Components are given by
fit <- princomp(stock.data, cor = FALSE)
summary(fit)
plot(fit,type="l")
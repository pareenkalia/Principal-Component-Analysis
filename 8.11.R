#--- 8.11

# Original Data
census.data<- read.table("T8-5.DAT")
head(census.data)

# Multiplying 5th variable by 10 as instructed
census.data$V5 <- census.data$V5 * 10
head(census.data)

# Sample covariance matrix 
S <- cov(census.data)
p <- ncol(S)

# Eigenvalues and eigenvectors
eigen(S)
evalues <- eigen(S)$values
evectors <- eigen(S)$vectors

# Proportion of variance explained by first two components
var_p <- c()
for (i in 1:p)
{
var_p[i]<-sum(eigen(S)$values[1:i])/sum(eigen(S)$values)
}
var_p

# Correlation Coefficients
r <- matrix(NA, nrow = p , ncol = p )
for(i in 1:p)
{
  for( k in 1:p)
  {
    r[k,i] <- evectors[k,i]*sqrt(evalues[i])/sqrt(S[k,k]) 
  }
}

r

# Correlation Matrix
R<- cor(census.data)

# Eigenvalues and Eigenvectors of R
evalues.R <- eigen(R)$values
evalues.R

evectors.R <- eigen(R)$vectors
evectors.R


# Proportion explained by correlation matrix
prop.R <- c()
for(i in 1:p)
{
  prop.R[i] <- sum(evalues.R[1:i])/sum(evalues.R)
}

prop.R

# Principle Components are given by
fit <- princomp(census.data, cor = FALSE)
summary(fit)
plot(fit,type="l")
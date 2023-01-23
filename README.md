# STATISTICAL-COMPUTING

weibull <- function(alpha, lambda)
{
  u <- runif(1)
  weibull_rn <- 1-(exp(-(lambda*(u^alpha))))
  return(weibull_rn)
}
weibull(4,3)

su <- function()
{
weibull_vector <- numeric(100)
for(i in 1:5)
{
  weibull_vector[i] <- weibull(alpha = i,lambda = 3 ) 
}
suu <-sum(weibull_vector)
return(suu)
}

su_vector <- vector(length=100)
for(i in 1:100)
{
  su_vector[i] <- su()
}

ans <- var(su_vector) + (mean(su_vector)^2)
hist(weibull_vector)

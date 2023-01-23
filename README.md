# STATISTICAL-COMPUTING

weibull <- function(alpha, lambda)
{
  u <- runif(1)
  weibull_rn <- 1-(exp(-(lambda*(u^alpha))))
  return(weibull_rn)
}
weibull(4,3)
weibull_vector <- numeric(100)
for(i in 1:100)
{
  weibull_vector[i] <- weibull(alpha = 4,lambda =3 ) 
}
hist(weibull_vector)

# inverse transformation

# BERNOULLI

bern <- function(p)
{
  u <- runif(1)
  if(u < p){ return(1) }
  else{ return(0) }
}
bern(0.5)

bern_vec <- numeric(1000)
for(i in 1:1000)
{
  bern_vec[i] <- bern(0.5)
}
mean(bern_vec)


# BINOMIAL

# book

bino <- function(n,p)
{
  i = 0
  c = p/(1-p)
  pr = (1-p)^n
  f = pr
  u = runif(1)
  while(u > f)
  {
    pr = ((n-i)/(i+1)) * pr * c
    f = f + pr
    i = i+1
  }
  return(i)
}
bino(20,0.5)

bino_vec <- numeric(1000)
for(i in 1:1000)
{
  bino_vec[i] <- bino(20,0.5)
}
mean(bino_vec)


#GEOMETRIC

# inverse transformation

# tail type

# book
geo <- function(p)
{
  i = 0
  pr = p
  f = pr 
  u <- runif(1)
  while(u > f)
  {
    pr = pr * (1-p)
    f = f + pr
    i = i+1
  }
  return(i)
}
geo(0.5)

# simple
geom <- function(p)
{
  u <- runif(1)
  sum = p
  i = 0
  while (sum < u)
  {
    i = i + 1
    sum = sum + p*(1 - p)^i
  }
  return(i)
}
geom(0.5)

geo_vec <- numeric(1000)
for(i in 1:1000)
{
  geo_vec[i] <- geo(0.5)
}
mean(geo_vec)

# another one

geom <- function(p)
{
  count <- 0
  b <- 0
  while(b <= 1)
  {
    u <- runif(1,0,1)
    if( u < p ){ a <- 1 }
    else { a <- 0 }
    b <- a + b
    count <- count+1
  }
  return(count)
}
geom(0.5)

# trail type

trgeo <- function(p)
{
  i = 1
  pr = p
  f = pr 
  u <- runif(1)
  while(u > f)
  {
    pr = pr * (1-p)
    f = f + pr
    i = i+1
  }
  return(i)
}
trgeo(0.5)

trgeo_vec <- numeric(1000)
for(i in 1:1000)
{
  trgeo_vec[i] <- trgeo(0.5)
}
mean(trgeo_vec)


# POISSON

# inverse transformation

# book

poi <- function(p)
{
  i = 0
  pr = exp(-p)
  f = pr
  u = runif(1)
  while(u > f)
  {
    pr = (p/(i+1)) * pr 
    f = f + pr
    i = i+1
  }
  return(i)
}
poi(17)

poi_vec <- numeric(1000)
for(i in 1:1000)
{
  poi_vec[i] <- poi(17)
}
mean(poi_vec)


# NEGATIVE BINOMIAL

# tail type

negbino <- function(r,p)
{
  i = 0
  pr = p^r
  f = pr
  u = runif(1)
  while(u > f)
  {
    pr = ((i+r)/(i+1)) * (1-p) * pr
    f = f + pr
    i = i+1
  }
  return(i)
}
negbino(20,0.5)

negbino_vec <- numeric(1000)
for(i in 1:1000)
{
  negbino_vec[i] <- negbino(20,0.5)
}
mean(negbino_vec)


# trail type

trnegbino <- function(r,p)
{
  i = r
  pr = p^r
  f = pr
  u = runif(1)
  while(u > f)
  {
    pr =  (i/(i-r+1)) * (1-p) * pr
    f = f + pr
    i = i+1
  }
  return(i)
}
trnegbino(10,0.2)

trnegbino_vec <- numeric(1000)
for(i in 1:1000)
{
  trnegbino_vec[i] <- trnegbino(10,0.2)
}
mean(trnegbino_vec)




















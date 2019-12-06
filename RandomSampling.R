rm(list = ls())
library(ggplot2)
set.seed(42)
#Create the user base:
#let's say we are tracking the amount of money people spend on the app
#but let's say that the amount of money people spend depends on whether they
#live in a big city or a small city
res = 1e6
df = data.frame(id = 1:res )
cities = c(rep('big',times = 100), rep('small',times = 1))
df$cities = sample(cities,replace = TRUE,size = length(df$id))
df$bigcities = 0
df$towns = 0
df$bigcities[which(df$cities == 'big')] = 1
df$towns[which(df$cities == 'small')] = 1

expres = 100*2
unbalanced = sample(df$id,size = expres)
unbalanceddf = df[which(df$id %in% unbalanced),]
unbalanceddf['treatment'] = sample(c(0,1),replace = TRUE, size = expres)


intercept = 50
beta1 = 50
beta2 = -10
beta3 = 5
beta4 = -10
error = rnorm(expres,0,10)

unbalanceddf$response = intercept + unbalanceddf$bigcities*beta1 + unbalanceddf$towns*beta2 + unbalanceddf$treatment*beta3 + 
                        + beta4*unbalanceddf$treatment*unbalanceddf$towns + error

fit1 = lm(data = unbalanceddf,response~treatment)
fit2 = lm(data = unbalanceddf,response~treatment+cities)
fit3 = lm(data = unbalanceddf,response~treatment*cities)

townid = df$id[which(df$towns == 1)]
bigcityid = df$id[which(df$bigcities== 1)]
balanced = c(sample(townid,size = expres/2),sample(bigcityid, size = expres/2))
balanceddf = df[which(df$id %in% balanced),]
balanceddf['treatment'] = sample(c(0,1),replace = TRUE, size = expres)
balanceddf$response = intercept + balanceddf$bigcities*beta1 + balanceddf$towns*beta2 + balanceddf$treatment*beta3 + 
  + beta4*balanceddf$treatment*balanceddf$towns + error

balfit1 = lm(data = balanceddf, response~treatment)
balfit2 = lm(data = balanceddf, response~treatment+cities)
balfit3 = lm(data = balanceddf, response~treatment*cities)

q = ggplot(data = unbalanceddf, aes(x = response, fill = cities))
q + facet_wrap('treatment') +
  geom_histogram()

q = ggplot(data = balanceddf, aes(x = response, fill = cities))
q + facet_wrap('treatment') +
  geom_histogram()


#=====================================
#convert this into click through rates
res = 1000000
newdf = data.frame(id = 1:res )
cities = c(rep('big',times = 100), rep('small',times = 1))
newdf$cities = sample(cities,replace = TRUE,size = length(newdf$id))
newdf$bigcities = 0
newdf$towns = 0
newdf$bigcities[which(newdf$cities == 'big')] = 1
newdf$towns[which(newdf$cities == 'small')] = 1
newdf$response = 0


newexpres = 100*2
newunbalanced = sample(newdf$id,size = expres)
newunbalanceddf = newdf[which(newdf$id %in% unbalanced),]
newunbalanceddf['treatment'] = sample(c(0,1),replace = TRUE, size = expres)
newunbalanceddf['control'] = 0
newunbalanceddf['control'][which(newunbalanceddf['treatment']==0),] = 1

newunbalanceddf['response'] = 0
#when treatment = 0 p is 0.1
#when bigciteis = 0 
# y ~ binom(n,p)
# p = intercept + bigcities*beta1 + town*beta2 + treatment*beta3

calculatep = function(data) {
  beta1 = 0.2
  beta2 = 0.1
  treatment = 0.05
  return(c(beta1,beta2,treatment))
}
pvec = calculatep(newunbalanceddf)

newunbalanceddf['response'] = newunbalanceddf$bigcities*newunbalanceddf$control*rbinom(newexpres, size = 1, prob = pvec[1]) +
                              newunbalanceddf$towns*newunbalanceddf$control*rbinom(newexpres, size = 1, prob = pvec[2]) +
                            newunbalanceddf$bigcities*newunbalanceddf$treatment*rbinom(newexpres, size = 1, prob = pvec[1] + pvec[3]) +
                            newunbalanceddf$towns*newunbalanceddf$treatment*rbinom(newexpres, size = 1, prob = pvec[2] + pvec[3])

n = sum(newunbalanceddf$treatment == 1)
treatmentp = sum(newunbalanceddf[which(newunbalanceddf$treatment == 1),]$response == 1)/newexpres
treatmentsd = sqrt(treatmentp*(1-treatmentp)/n)
baselinep = sum(newunbalanceddf[which(newunbalanceddf$treatment == 0),]$response == 1)/newexpres
baselinesd = sqrt(baselinep*(1-baselinep)/(newexpres-n))

t.test2 <- function(m1,m2,s1,s2,n1,n2,m0=0,equal.variance=FALSE)
{
  if( equal.variance==FALSE ) 
  {
    se <- sqrt( (s1^2/n1) + (s2^2/n2) )
    # welch-satterthwaite df
    df <- ( (s1^2/n1 + s2^2/n2)^2 )/( (s1^2/n1)^2/(n1-1) + (s2^2/n2)^2/(n2-1) )
  } else
  {
    # pooled standard deviation, scaled by the sample sizes
    se <- sqrt( (1/n1 + 1/n2) * ((n1-1)*s1^2 + (n2-1)*s2^2)/(n1+n2-2) ) 
    df <- n1+n2-2
  }      
  t <- (m1-m2-m0)/se 
  dat <- c(m1-m2, se, t, 2*pt(-abs(t),df))    
  names(dat) <- c("Difference of means", "Std Error", "t", "p-value")
  return(dat) 
}

t.test2(baselinep, treatmentp, baselinesd, treatmentsd, newexpres-n, n)

#=====
newexpres = 100*2

townid = newdf$id[which(newdf$towns == 1)]
bigcityid = newdf$id[which(newdf$bigcities== 1)]
newbalanced = c(sample(townid,size = expres/2),sample(bigcityid, size = expres/2))
newbalanceddf = newdf[which(newdf$id %in% newbalanced),]
newbalanceddf['treatment'] = sample(c(0,1),replace = TRUE, size = expres)
newbalanceddf$control = 0
idx = which(newunbalanceddf['treatment']==0)
newbalanceddf$control[idx] = 1

newbalanceddf['response'] = 0
#when treatment = 0 p is 0.1
#when bigciteis = 0 
# y ~ binom(n,p)
# p = intercept + bigcities*beta1 + town*beta2 + treatment*beta3

pvec = calculatep(newbalanceddf)

newbalanceddf['response'] = newbalanceddf$bigcities*newbalanceddf$control*rbinom(newexpres, size = 1, prob = pvec[1]) +
                            newbalanceddf$towns*newbalanceddf$control*rbinom(newexpres, size = 1, prob = pvec[2]) +
                            newbalanceddf$bigcities*newbalanceddf$treatment*rbinom(newexpres, size = 1, prob = pvec[1] + pvec[3]) +
                            newbalanceddf$towns*newbalanceddf$treatment*rbinom(newexpres, size = 1, prob = pvec[2] + pvec[3])


glmfit = glm(data = newbalanceddf, response ~ cities*treatment, family = 'binomial')
glmfit2 = glm(data = newbalanceddf, response ~ cities + treatment, family = 'binomial')


#this is going to need some work to figure out
#I need to calculate the proportions for each subclass
#there's probably a built in method for this
treatmentp = sum(newbalanceddf[which(newbalanceddf$treatment == 1),]$response == 1)/newexpres
treatmentsd = sqrt(treatmentp*(1-treatmentp)/n)
baselinep = sum(newbalanceddf[which(newbalanceddf$treatment == 0),]$response == 1)/newexpres
baselinesd = sqrt(baselinep*(1-baselinep)/(newexpres-n))

treatmentp = sum(newbalanceddf[which(newbalanceddf$treatment == 1),]$response == 1)/newexpres
treatmentsd = sqrt(treatmentp*(1-treatmentp)/n)
baselinep = sum(newbalanceddf[which(newbalanceddf$treatment == 0),]$response == 1)/newexpres
baselinesd = sqrt(baselinep*(1-baselinep)/(newexpres-n))

t.test2(baselinep, treatmentp, baselinesd, treatmentsd, newexpres-n, n)

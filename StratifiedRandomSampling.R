rm(list = ls())
library(ggplot2)
set.seed(42)
#Create the user base:
#let's say we are tracking the number of clicks on the app
#live in a big city or a small city

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
newunbalanced = sample(newdf$id,size = newexpres)
newunbalanceddf = newdf[which(newdf$id %in% newunbalanced),]
newunbalanceddf['treatment'] = sample(c(0,1),replace = TRUE, size = newexpres)
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

fit1 = aov(data = newunbalanceddf, response ~ treatment)

#=====
newexpres = 100*2

townid = newdf$id[which(newdf$towns == 1)]
bigcityid = newdf$id[which(newdf$bigcities== 1)]
newbalanced = c(sample(townid,size = newexpres/2),sample(bigcityid, size = newexpres/2))
newbalanceddf = newdf[which(newdf$id %in% newbalanced),]
newbalanceddf['treatment'] = sample(c(0,1),replace = TRUE, size = newexpres)
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

fit2 = aov(data = newbalanceddf, response ~ treatment)
fit3 = aov(data = newbalanceddf, response ~ treatment + bigcities)

# Program to implement a Gibbs Sampler for sampling the model parameters for modeling the CO Sensor

library(HI)
library(invgamma)

### Do all data initializations/variable creation here
source('~/betaposteriorfunction.R')

d <- read.csv('~/CompiledData.csv') #imports the datafile
d$log.LascarCO = log(d$LascarCO+0.001) #avoid Na's
d$log.ReferenceCO = log(d$ReferenceCO + 0.001)
d$CummulativeExposure = d$CummulativeExposure/10000
d$indicator = 0
d$indicator[d$ReferenceCO>0] = 1
LascarOn = as.data.frame(d[which(d$LascarCO>0) ,])
nrow.d = nrow(d)
d$sno = 1:nrow.d

X = with(d,table(d$sno,d$LascarNumber))
X = X[,-1]
y = d$log.LascarCO
xcoeffs = matrix( c(rep(1,nrow.d),d$log.ReferenceCO, d$CummulativeExposure , X),nrow = nrow.d, ncol = 3+ncol(X)) # Have to include lascar Number as factor!

SimpleModel2 = lm(log.LascarCO ~ log.ReferenceCO + CummulativeExposure+ as.factor(LascarNumber)  , data = d) 
betavector= SimpleModel2$coefficients

### Initialize all parameters at once

burnIn = 100 # Not sure what value you're using here
maxIterations = 500 # Not sure how many are needed?
thin.value = 10 # 10 is usually a solid number for thinning
ctr = 1

sampledp = 0.5
sampledsigma = 3.275^2
alpha.p = 2
beta.p = 2
n.on = nrow(LascarOn)
n.off = nrow.d - n.on
c = 10
dd = 10
# This file will store the samples - maybe give the betas more meaningful names
write.table(matrix(c(sampledp, betavector, sigma), nrow = 1), 'parametersamples.txt', row.names = FALSE, col.names = FALSE)

#pb <- winProgressBar(title="Gibbs Sampling Progress bar", label="0% done", min=0, max=100, initial=0)

tempdata = c(sampledp, betavector, sampledsigma)
ctr = 1
while(ctr <= maxIterations){
    
    # Sample p
    
  
  sampledp = rbeta(1, n.on+alpha.p, n.off + beta.p)
    
   
  # Sample the Betas
  
    for(betactr in 1:length(betavector)){
      y_minus_xbetaother = y - xcoeffs[,-betactr]%*%matrix(betavector[-betactr], ncol = 1)
      sum.y_xbetaother_timesx = sum(y_minus_xbetaother*matrix(xcoeffs[,betactr], ncol = 1))
      sum.y_xbetaother.sq = sum(y_minus_xbetaother^2)
      sum.xisq = sum(xcoeffs[,betactr]^2)
    
      betavector[betactr] = .Call("arms", c(-10, 10),
                                f = function(x) betajposterior(x, sum.y_xbetaother.sq = sum.y_xbetaother.sq,
                                                               sum.y_xbetaother_timesx = sum.y_xbetaother_timesx, sum.xisq = sum.xisq, sigma = sampledsigma, sigmabeta = 10),
                                betavector[betactr], as.integer(1), new.env())
      
  }
  
  
    # Sample sigma
    sampledsigma =  rinvgamma(1,c+nrow.d/2, dd+0.5*sum((y - xcoeffs %*% betavector)^2))
    sampledc = .Call("arms", c(0, 100),
                f = function(x) c.posterior(x, d = dd, sigmasq = sampledsigma, lambda.c = 1/10), c, as.integer(1), new.env())
    sampledd = .Call("arms", c(0, 100),
                f = function(x) d.posterior(x, c = c, sigmasq = sampledsigma, lambda.d = 1/10), dd, as.integer(1), new.env())


    #   if(ctr >= burnIn & ctr %% thin.value == 0){
        write.table(matrix(c(sampledp, betavector, sampledsigma, sampledc, sampledd), nrow = 1), 'parametersamples.txt', row.names = FALSE, col.names = FALSE, append = TRUE)
        #     cat(ctr)
        #}
    
    #    info <- sprintf("%d%% done", round((ctr/maxIterations)*100))
    #    setWinProgressBar(pb, ctr/(maxIterations)*100, label=info)

if(ctr %% 20 == 0){ print(ctr)  }
tempdata = rbind(tempdata, c(sampledp, betavector, sampledsigma, sampledc, sampledd))
    ctr = ctr + 1

    
}

close(pb)


fitt =  (xcoeffs %*% betavector)
resids = y-fitt

# Verifying by computing the 95% confidence interval
colme = colMeans(sampled)
columnsd = NULL
for(i in 1:length(colme))
{
  columnsd = c(columnsd,sqrt(var(sampled[,i])))
}

z = 1.96
sqrtn = sqrt(nrow.d)
lowerlimit = NULL
upperlimit = NULL

for(i in 1:length(colme))
{
  lowerlimit = c(lowerlimit,colme[i] - z * columnsd[i]/sqrtn)
  upperlimit = c(upperlimit,colme[i] + z * columnsd[i]/sqrtn)
}


#PLOTTING!!!!

sampled = read.table("C:/Users/Prashant/Google Drive/Summer_stats/R_Code/NewCode/parametersamples.txt", 
                           header = FALSE)
sampled= sampled[-c(1:100),]

plot(1:nrow(sampled),sampled$X16,type = 'l',col = 'red',lwd = 3,xlab = 'Sample',
     ylab = 'Sampled dd',main = 'Gibbs Sampling Result')

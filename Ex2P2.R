#2.
  #2.1
    os = c(1.977866, 1.836622, 1.097168, 1.232889, 1.229526, 2.438342, 1.551389, 1.300618, 1.068584,
                1.183466, 2.179033, 1.535904, 1.323500, 1.458713, 1.013755, 3.602314, 1.087067, 1.014013,
                1.613929, 2.792161, 1.197081, 1.021430, 1.111531, 1.131036, 1.064926)
    
    n = length(os)
    
    #MLE
    MLE = function(x) {n/sum(log(x))}
    
    MLE.os = MLE(os)
    
    MLE.os
    
    #MME
    MME = function(x){mean(x)/(mean(x)-1)}
    
    MME.os = MME(os)
    
    MME.os
    
  #2.2
    In = n/(MLE.os^2)
    MLE.v = 1/In
    
    MLE.v
  #2.3
    #Likelihood function
    likh = function(theta){
      l = vector()
      for(t in theta)
        l = c(l, (t^n)*prod(1/(os^(t+1))))
      return(l)
    }
    
    #Log-Likelihood function
    loglikh = function(theta){
      ll = vector()
      for(t in theta)
        ll = c(ll, n*log(t)-((t + 1) * sum(log(os))))
      return(ll)
    }
    
    #Score function
    scr = function(theta) {
      s = vector()
      for(t in theta)
        s = c(s, (n/t) - sum(log(os)))
      return(s)
    }
    
    #Graphical Display
    
    par(mfrow=c(1,3))
    theta <- seq(0.1,4.0,0.05)
    
    plot(theta,likh(theta),ylab="likelihood",
         xlab=expression(theta),lwd=2,type="l",cex.lab=1.5)
    box(lwd=2)
    
    plot(theta,loglikh(theta),ylab="log-likelihood",
         xlab=expression(theta),lwd=2,type="l",cex.lab=1.5)
    box(lwd=2)
    
    plot(theta,scr(theta),ylab="score",
         xlab=expression(theta),lwd=2,type="l",cex.lab=1.5)
    abline(h=0,lty=3); box(lwd=2)
    
    #If we use interval estimation (2,4):
    
    #Likelihood function maximum
    optimize(likh,c(2,4),maximum=T)$maximum
    
    #Log-Likelihood function maximum
    optimize(loglikh,c(2,4),maximum=T)$maximum
    
    #Score function root
    uniroot(scr,c(2,4))$root
    
  #2.4
    #(install.packages("maxLik"))
    library(maxLik)
    maxLik(loglikh,start=2.81418)
    # Maximum Likelihood estimation
    # Newton-Raphson maximisation, 1 iterations
    # Return code 1: gradient close to zero (gradtol)
    # Log-Likelihood: -8.016816 (1 free parameter(s))
    # Estimate(s): 2.814179
    
    #The ML estimate of alpha by R function maxLik() was 2.814179.
    
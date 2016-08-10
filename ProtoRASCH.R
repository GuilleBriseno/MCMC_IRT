# putting the pieces together for a simple, MH alg, literally stealing from the other guy
rasch.mh <- function(startvalue, iterations, data){
  # initialize everything!!!
  y <- data # data is read
  chain <- array(dim = c(iterations+1,2)) # matrix for storing chain value is on
  # h.means <- array(dim = c(iterations+1, 2)) # matrix for hyperparameter means
  # h.vars <- array(dim = c(iterations+1, 2)) # matrix for hyperparameter vars
  
  chain[1,] <- startvalue # starting value is set for theta, b
  ###########################################################
  # not sure if we need this
  #b.mu.pars <- c(0, 100)  # value is set for hyperparameters for b
  #b.var.pars <- c(0.5, 0.5)
  #theta.mu.pars <- c(0, 100) # value is set for hyperparameters for theta
  #theta.var.pars <- c(0.5,0.5)
  ###########################################################
  t.proposal <- c()
  b.proposal <- c()
  th.p <- c()
  b.p <- c()
  
  
  
  # iterations
  for (i in 1:iterations){
    t.proposal[1] <- prop.theta(chain[i,1]) # sample a new value for theta
    t.proposal[2] <- chain[i,2]
    th.p[1] <- mu.t.prior(100) # large variance
    th.p[2] <- var.t.prior() # only produces an inverse gamma distributed value
    
    
      # acceptance criterion for theta # missing hyperparameters
    t.probab <- exp(data.like(t.proposal, y) + theta.prior(param = th.p , val = t.proposal[1])  
                  - data.like(chain[i,], y) - theta.prior(param = th.p , val = chain[i,1]))
    if (runif(1) < t.probab){
      chain[i+1,1] <- t.proposal[1]
    }else{
      chain[i+1,1] <- chain[i,1]
    }
    # second step - > b: sample b from proposal, then compare p(y)*p(b given upper values)
    b.p[1] <- mu.b.prior(100)
    b.p[2] <- var.t.prior()
    
    b.proposal[1] <- chain[i+1,1]
    b.proposal[2] <- prop.b(b.p[1]) # sample a new value for b
    
    b.probab <- exp(data.like(b.proposal, y) + b.prior(param = b.p, val = b.proposal[2])
                    - data.like(chain[i,], y) - b.prior(param = b.p, val = chain[i,2]))
    if (runif(1) < b.probab){
      chain[i+1,2] <- b.proposal[2]
    }else{
      chain[i+1,2] <- chain[i,2]
    }
    # third step - > hyperpars: sample hyperparameters: 
    # not actually necessary? 
    
  }

  
  # third step -> hyperparameters
  
  
  return(mcmc(chain))
}
################## sanity.R ##################
"sanity.check.prior" <- function(args){
  
  if(args$lambda0<=0 || args$lambda0>1){
    stop("\n\n\t-- Overall prior tightness (lambda0) must be between 0 and 1\n")
  }
  else if(args$lambda1<0){
    stop("\n\n\t--
        Prior tightness around AR(1) parameters (lambda1) must be greater than 0\n")
  }
  else if(args$lambda3<0){
    stop("\n\n\t-- Order of lag decay (lambda3) must be greater than 0\n")
  }
  else if(args$lambda4<0){
    stop("\n\n\t-- Prior tightness around intercept (lambda4) must be greater than 0\n")
  }
  else if(args$lambda5<0){
    stop("\n\n\t--
        Prior tightness around exogenous parameters (lambda5) must be greater than 0\n")
  }
  else if(args$mu5<0){
    stop("\n\n\t-- Prior weight of sum of coefficients must be non-negative\n")
  }
  else if(args$mu6<0){
    stop("\n\n\t-- Prior values of initial dummy observations or of cointegrating prior must be non-negative\n")
  }
  else{ return(1) }
  
}


"sanity.check.var" <- function(args){
  
  arg.names <- names(args)
  
  if(length(which(arg.names=="dat"))){
    Y <- args$dat
  } else if (length(which(arg.names=="Y"))){
    Y <- args$Y
  }
  
  # Check time series properties of input Y
  if(is.null(tsp(Y))==TRUE){
    stop("\n\n\t-- Input data [Y] must be of class ts() or mts()\n")
  } else {
    T <- nrow(Y)
    m <- ncol(Y)
  }
  
  ##     # Check time series properties of input z
  ##     if(is.null(args$z)==FALSE || is.ts(args$z)==FALSE){
  ##         stop("\n\n\t-- Input data [z] must be NULL, a ts() object, or an mts() object\n")
  ##     }
  
  # Check if num lags is possible given the amount of data supplied
  if(args$p<=0 || args$p>=T){
    stop("\n\n\t-- Number of lags (p): 0 < p < number of observations (T) \n")
  }
  
  return(1)
  
}

"sanity.check.bvar" <- function(args){
  
  arg.names <- names(args)
  
  if(length(which(arg.names=="dat"))){
    Y <- args$dat
  } else if (length(which(arg.names=="Y"))){
    Y <- args$Y
  }
  
  sanity.check.var(list(Y=Y, p=args$p, z=args$z))
  
  # Make sure prior specifications are at least within the bounds
  # outlined in the documentation
  
  prior.vals <- list(lambda0=args$lambda0, lambda1=args$lambda1,
                     lambda3=args$lambda3, lambda4=args$lambda4,
                     lambda5=args$lambda5, mu5=args$mu5, mu6=args$mu6)
  sanity.check.prior(prior.vals)
  
  if(args$qm!=4 && args$qm!=12){
    warning("\n\n\t-- qm should be set to 4 (quarterly) or 12 (monthly): qm=4 selected as default\n")
  }
  
  # BVAR-specific argument checks
  
  if(length(which(arg.names=="prior")) && (args$prior<0 || args$prior>2)){
    stop("\n\n\t-- Please specify valid prior form: (0) Normal-Wishart; (1) Normal-flat; or (2) Flat-flat\n")
  }
  
  if(length(which(arg.names=="posterior.fit")) && !(is.logical(args$posterior.fit))){
    stop("\n\n\t-- For BVAR models, posterior.fit argument must be logical (TRUE/FALSE)\n")
  }
  
  return(1)
}


"sanity.check.bsvar" <- function(args){
  
  arg.names <- names(args)
  
  sanity.check.bvar(args)
  
  m <- ncol(args[[1]])
  
  # Check identification MATRIX
  
  if(is.matrix(args$ident)==FALSE) {
    stop("\n\n\t-- Check Identification: ident argument should be of the class matrix, not list or dataframe\n")
  }
  
  if(length(which(args$ident==1))>(m*(m+1))/2){
    stop("\n\n\t-- Check Identification: no more than m*(m+1)/2 free parameters allowed\n")
  }
  
  if(length(which(args$ident==1))<m){
    stop("\n\n\t-- Check Identification: too few free parameters\n")
  }
  
  return(1)
}


"sanity.check.gibbs" <- function(args){
  
  if(args$N1<1000) warning("\n\n\t-- Burnin iteration count may be too low for sensible results (<1000)\n")
  if(args$N2<1000) warning("\n\n\t-- Gibbs sampler iteration count may be too low for sensible results (<1000)\n")
  if(args$thin<1) stop("\n\n\t-- Thinning parameter must be greater than or equal to 1\n")
  methodlist <- c("DistanceMLA", "DistanceMLAhat", "Euclidean", "PositiveDiagA", "PositiveDiagAinv")
  
  if(length(which(methodlist==args$normalization))==0){
    warning("\n\n\t-- Specified normalization method does not exist: default set to 'DistanceMLA'\n")
    return("DistanceMLA")
  }
  else { return(1) }
  
}


"sanity.check.irf" <- function(args){
  
  if(is.null(args$nsteps)){
    stop("\n\n\t-- 'nsteps' undefined: number of steps for IRF must be user defined\n")
  }
  else{ return(1) }
  
}


"sanity.check.mc.irf" <- function(args){
  
  arg.names <- names(args)
  
  if(args$nsteps<=0) stop("\n\n\t-- 'nsteps' must be greater than 0\n")
  
  if(attr(args$varobj,"class")=="VAR" && !(args$nsteps>0)){
    stop("\n\n\t--
        For VAR models, argument 'nsteps' must be defined and non-negative integer\n")
  }
  
  if(attr(args$varobj,"class")=="BVAR" && !(args$draws>0)){
    stop("\n\n\t--
        For BVAR models, argument 'draws' must be defined and non-negative integer\n")
  }
  
  if(attr(args$varobj,"class")=="BSVAR" && is.null(args$A0.posterior)){
    stop("\n\n\t-- mc.irf.BSVAR requires A0.posterior object from gibbs.A0()\n")
  }
  
  return(1)
}

###################### szbvar.R ####################
# szbvar estimator
"szbvar" <-
  function(Y, p, z=NULL, lambda0, lambda1, lambda3,
           lambda4, lambda5, mu5, mu6, nu=ncol(Y)+1, qm=4, prior=0,
           posterior.fit=FALSE)
  {
    
    sanity.check.bvar(list(Y=Y, p=p, z=z, lambda0=lambda0,
                           lambda1=lambda1, lambda3=lambda3,
                           lambda4=lambda4, lambda5=lambda5,
                           mu5=mu5, mu6=mu6, qm=qm, prior=prior,
                           posterior.fit=posterior.fit))
    
    n<-nrow(Y);	 	# # of observations in data set
    m<-ncol(Y);	 	# # of variables in data set
    
    # Test for the exogenous variables
    if (is.null(z))
    { num.exog <- 0 }
    else
    { num.exog <- ncol(z)
    z <- as.matrix(z)
    }
    
    # Compute the number of coefficients
    ncoef<-(m*p)+1;  # AR coefficients plus intercept in each RF equation
    ndum<-m+1;                # of dummy observations
    capT<-n-p+ndum;   	      # # of observations used in mixed estimation (Was T)
    Ts<-n-p;  		      # # of actual data observations @
    
    # Declare the endoegnous variables as a matrix.
    dat<-as.matrix(Y);
    
    # Create data matrix including dummy observations
    # X: Tx(m*p+1)
    # Y: Txm, ndum=m+1 prior dummy obs (sums of coeffs and coint).
    # mean of the first p data values = initial conditions
    if(p==1)
    { datint <- as.vector(dat[1,]) }
    else
    {   # wrap first p values in an as.matrix() in case someone
      # passes in a univariate series.
      datint<-as.vector(apply(as.matrix(dat[1:p,]),2,mean))
    }
    
    # Y and X matrices with m+1 initial dummy observations
    X<-matrix(0, nrow=capT, ncol=ncoef)
    Y<-matrix(0, nrow=capT, ncol=m)
    const<-matrix(1, nrow=capT)
    const[1:m,]<-0;	         # no constant for the first m periods
    X[,ncoef]<-const;
    
    # Build the dummy observations we need
    for(i in 1:m)
    {
      Y[ndum,i]<-datint[i];
      Y[i,i]<-datint[i];
      for(j in 1:p)
      { X[ndum,m*(j-1)+i]<-datint[i];
      X[i,m*(j-1)+i]<-datint[i];
      }
    }
    
    # Note that constant is put last here when the lags are made
    for(i in 1:p)
    { X[(ndum+1):capT,(m*(i-1)+1):(m*i)]<-matrix(dat[(p+1-i):(n-i),],ncol=m)
    }
    
    # Put on the exogenous regressors and make the constant the
    # first exog regressor after the AR coefs
    if(is.null(z)==F)
    {
      pad.z <- matrix(0,nrow=capT,ncol=ncol(z))
      pad.z[(ndum+1):capT,] <- matrix(z[(p+1):n,], ncol=ncol(z))
      X<-cbind(X,pad.z);
    }
    
    # Get the corresponding values of Y
    Y[(ndum+1):capT,]<-matrix(dat[(p+1):n,],ncol=m);
    
    # Weight dummy observations
    X[1:m,]<-mu5*X[1:m,];
    Y[1:m,]<-mu5*Y[1:m,];
    X[ndum,]<-mu6*X[ndum,];
    Y[ndum,]<-mu6*Y[ndum,];
    
    # END OF DATA SET UP FOR THE MIXED ESTIMATION
    
    # NOW CREATE THE PRIORS
    
    # Create monthly lag decay to match 1/k decay in quarterly
    # data where k = quarters
    ld<-seq(1:p)^-lambda3;			# regular lag decay (note ^-lambda3)
    if(qm==12)
    { j<-ceiling(p/3)^-lambda3;   	# last quarter (rounded up) eg. l2=13=>xx2=5
    b<-0
    if(p > 1)
    { b<-(log(1)-log(j))/(1-p) }
    a<-exp(-b);
    ld<-a*exp(b*seq(1:p));	# Tao Zha's lag decay to match 13th lag
    }
    
    # Scale factors from OLS
    s2<-matrix(0,nrow=m,ncol=1)
    for(i in 1:m)
    {
      s2[i,1] <- ar.ols(Y[,i], aic=FALSE, order.max=p,
                        intercept=TRUE, demean=FALSE)$var.pred
    }
    #  Prior scale matrix for Sigma:  Sigma ~ IW(H0,v) @
    S0 <- diag(m)
    diag(S0)<- s2/(lambda0^2);	 # see SZ p.961 (24)
    
    # prior for intercept
    prior.intercept <- 0;
    if( lambda4 > 0)
    { prior.intercept <- 1/(lambda0*lambda4)^2 }  # inverse prior variance for constant @
    
    # Now scale the priors for the exogneous variables
    prior.exog <- 0
    if( lambda5 > 0 )
    { prior.exog <- 1/(lambda0*lambda5)^2 }
    
    
    # Prior cov of B|Sigma is Sigma.*.inv(H0)  for now, this uses the
    # prior on the intercept for the exogenous variables
    
    if(num.exog==0)
    {  H0<-diag(c(kronecker((1/(ld*lambda0*lambda1))^2,s2),
                  prior.intercept), nrow=ncoef, ncol=ncoef)
    }
    else
    {  H0<-diag(c(kronecker((1/(ld*lambda0*lambda1))^2,s2),
                  prior.intercept, rep(prior.exog, num.exog)),
                nrow=(ncoef+num.exog), ncol=(ncoef+num.exog))
    }
    
    # Now, do the special cases of normal-flat and flat-flat
    if(prior == 1)
    { S0 <-0*S0 } # using normal-flat prior as special case
    
    if(prior == 2)         # using the flat-flat prior as a special case
    { H0<-0*H0
    S0<-0*S0
    nu <- 0
    }
    
    # Set up some matrices for later
    XX <- crossprod(X)    # Cross product of RHS variables
    hstar1 <- H0 + XX     # Prior + Cross product
    
    # Posterior mean of B: Bh  = inv(x'x + H0)*(x'y + H0[.,1:m]);
    # This is different from the original code because the intercept
    # is now the last coefficient in the matrix, rather than the
    # first, followed by the exogenous regressors.
    Bh<-solve((hstar1),(crossprod(X,Y) + H0[,1:m]))
    
    # Posterior mean of Sigma
    # This is different from the original code because the intercept
    # is now the last coefficient in the matrix, rather than the first.
    St <- (S0 + crossprod(Y) + H0[1:m,1:m] - t(Bh)%*%(hstar1)%*%(Bh))
    Sh<- St/(Ts+nu-m-1)
    
    # Posterior variance of B: VBh = diag(Sh.*.(inv((H0 + x'x)))
    hstarinv <- solve(hstar1)
    vcv.Bh <- kronecker(Sh,hstarinv)
    
    # Residuals and MLE estimators of the variance
    u<-(Y - X%*%(Bh));
    Sh1<-crossprod(u)/capT;  # u'u/n form of estimator
    
    # Format the output variables
    # Split the coefficient matrix (will make IRFs and forecasting easier)
    intercept<-Bh[ncoef,];                   # extract the intercept
    ar.coefs<-t(Bh[1:(ncoef-1),]);           # extract the
    # ar coefficients
    
    dim(ar.coefs)<-c(m,m,p)              # push ar coefs into M x M x P array
    
    ar.coefs<-aperm(ar.coefs,c(2,1,3))   # reorder array so columns are for eqn
    
    if(is.null(z))
    {exog.coefs <- NA}
    else
    {
      exog.coefs <- Bh[(ncoef+1):nrow(Bh),]
    }
    
    marg.llf <- NA
    marg.post <- NA
    coef.post <- NA
    
    pfit <- list(capT=capT, m=m, ncoef=ncoef, num.exog=num.exog,
                 nu=nu, H0=H0, S0=S0, Y=Y, X=X,
                 hstar1=hstar1, Sh=Sh, u=u, Bh=Bh, Sh1=Sh1)
    
    output <- list(intercept = intercept,
                   ar.coefs=ar.coefs,
                   exog.coefs=exog.coefs,
                   Bhat = Bh,
                   vcv=Sh1,
                   vcv.Bh=vcv.Bh,
                   mean.S=Sh,
                   St=St,
                   hstar=(H0 + XX),
                   hstarinv=hstarinv,
                   H0=H0,
                   S0=S0,
                   residuals = u,
                   X=X,
                   Y=Y,
                   y=dat,
                   z=z,
                   p=p,
                   num.exog=num.exog,
                   qm=qm,
                   prior.type=prior,
                   prior=c(lambda0,lambda1,lambda3,lambda4,lambda5,mu5,mu6,nu),
                   pfit=pfit,
                   marg.llf=marg.llf,
                   marg.post=marg.post,
                   coef.post=coef.post)
    class(output) <- c("BVAR")
    attr(output, "eqnames") <- colnames(dat) # Get variable names for
    # attr
    
    # Compute the posterior fit measures.
    if(posterior.fit==T)
    {
      tmp <- posterior.fit.BVAR(output)
      output$marg.llf <- tmp$data.marg.llf
      output$marg.post <- tmp$data.marg.post
      output$coef.post <- tmp$coef.post
    }
    
    # Here are the returns
    return(output)
  }


# Summary function for BVAR models
"summary.BVAR" <- function(object, ...)
{
  cat("------------------------------------------\n")
  cat("Sims-Zha Prior reduced form Bayesian VAR\n")
  cat("------------------------------------------\n")
  if(object$prior.type==0) prior.text <- "Normal-inverse Wishart"
  if(object$prior.type==1) prior.text <- "Normal-flat"
  if(object$prior.type==2) prior.text <- "Flat-flat"
  
  cat("Prior form : ", prior.text, "\n")
  cat("Prior hyperparameters : \n")
  cat("lambda0 =", object$prior[1], "\n")
  cat("lambda1 =", object$prior[2], "\n")
  cat("lambda3 =", object$prior[3], "\n")
  cat("lambda4 =", object$prior[4], "\n")
  cat("lambda5 =", object$prior[5], "\n")
  cat("mu5     =", object$prior[6], "\n")
  cat("mu6     =", object$prior[7], "\n")
  cat("nu      =", object$prior[8], "\n")
  
  cat("------------------------------------------\n")
  cat("Number of observations : ", nrow(object$Y), "\n")
  cat("Degrees of freedom per equation : ", nrow(object$Y)-nrow(object$Bhat), "\n")
  cat("------------------------------------------\n")
  
  cat("Posterior Regression Coefficients :\n")
  cat("------------------------------------------\n")
  cat("Autoregressive matrices: \n")
  for (i in 1:dim(object$ar.coefs)[3])
  {
    cat("B(", i, ")\n", sep="")
    prmatrix(round(object$ar.coefs[,,i], 6))
    cat("\n")
  }
  cat("------------------------------------------\n")
  cat("Constants\n")
  cat(round(object$intercept,6), "\n")
  cat("------------------------------------------\n")
  
  if(is.na(object$exog.coefs[1])==FALSE)
  {
    cat("------------------------------------------\n")
    cat("Exogenous variable posterior coefficients\n")
    prmatrix(object$exog.coefs)
    cat("\n")
    cat("------------------------------------------\n")
  }
  
  cat("------------------------------------------\n")
  cat("Posterior error covariance\n")
  prmatrix(object$mean.S)
  cat("\n")
  cat("------------------------------------------\n")
  
}

############# forecast.R ##############
# Generate forecast methods for the models in the MSBVAR package
#
# Patrick T. Brandt
#
# 20120621 : Updated to include MSBVAR forecast functions.

"forecast" <- function(varobj, nsteps, A0=t(chol(varobj$mean.S)),
                       shocks=matrix(0,nrow=nsteps,ncol=dim(varobj$ar.coefs)[1]),
                       exog.fut=matrix(0,nrow=nsteps,ncol=nrow(varobj$exog.coefs)),
                       N1, N2)
{
  if(inherits(varobj,"VAR")){
    return(forecast.VAR(varobj, nsteps, A0=A0,
                        shocks=shocks, exog.fut=exog.fut))
  }
  
  if(inherits(varobj, "BVAR")){
    return(forecast.VAR(varobj, nsteps, A0=A0,
                        shocks=shocks, exog.fut=exog.fut))
  }
  
  if(inherits(varobj, "BSVAR")){
    return(forecast.VAR(varobj, nsteps, A0=solve(varobj$A0.mode),
                        shocks=shocks, exog.fut=exog.fut))
  }
  
  if(inherits(varobj, "MSBVAR")){
    return(forecast.MSBVAR(x=varobj, k=nsteps, N1, N2))
  }
}

# This is the generic VAR forecasting function.  The other
"forecast.VAR" <-
  function(varobj, nsteps, A0, shocks, exog.fut)
  {
    # Set up the initial parameters for the VAR forecast function from
    #  VAR object
    y <- varobj$y
    intercept <- varobj$intercept
    ar.coefs <- varobj$ar.coefs
    exog.coefs <- varobj$exog.coefs
    m<-dim(ar.coefs)[1]
    p<-dim(ar.coefs)[3]
    capT<-nrow(y)
    yhat<-rbind(y,matrix(0,ncol=m,nrow=nsteps))
    
    # Compute the deterministic part of the forecasts (less the intercept!)
    if(is.na(sum(varobj$exog.coefs))==F)
    {
      deterministic.VAR <- as.matrix(exog.fut) %*% exog.coefs
    }
    else
    { deterministic.VAR <- matrix(0,nrow=nsteps,ncol=m)
    }
    
    # Now loop over the forecast horizon
    for(h in 1:nsteps)
    {  yhat[capT + h, ] <- (yhat[capT + h - 1,] %*% ar.coefs[,,1] +
                              intercept + deterministic.VAR[h,] + (shocks[h,]%*%A0))
    if (p>1) {for(i in 2:p)
    { yhat[capT + h, ] <- (yhat[capT + h, ] +
                             (yhat[capT + h - i, ] %*% ar.coefs[,,i]))
    
    }}
    }
    output <- ts(yhat, start = start(varobj$y), frequency = frequency(varobj$y), names = colnames(varobj$y))
    attr(output, "class") <- c("forecast.VAR", "mts", "ts")
    attr(output, "eqnames") <- attr(varobj, "eqnames")
    return(output)
  }

"forecast.BVAR" <- function(varobj, nsteps, A0, shocks, exog.fut)
{
  output <- forecast.VAR(varobj, nsteps, A0, shocks, exog.fut)
  attr(output, "class") <- c("forecast.BVAR", "mts", "ts")
  attr(output, "eqnames") <- attr(varobj, "eqnames")
  return(output)
}

"forecast.BSVAR" <- function(varobj, nsteps, A0=solve(varobj$A0.mode), shocks, exog.fut)
{
  output <- forecast.VAR(varobj, nsteps, A0, shocks, exog.fut)
  attr(output, "class") <- c("forecast.BSVAR", "mts", "ts")
  attr(output, "eqnames") <- attr(varobj, "eqnames")
  return(output)
}

"uc.forecast" <- function(varobj, nsteps, burnin, gibbs,
                          exog=NULL)
{
  if(inherits(varobj, "VAR"))
  {
    stop("Not implemented for VAR models!\nUse a BVAR with a flat-flat prior if you want this case.\n")
    ##         varobj$H0 <- matrix(0, nrow(varobj$Bhat), nrow(varobj$Bhat))
    ##         varobj$S0 <- matrix(0, ncol(varobj$Bhat), ncol(varobj$Bhat))
    ##         output <- uc.forecast.VAR(varobj, nsteps, burnin, gibbs, exog)
    ##         attr(output, "class") <- c("forecast.VAR")
    ##         return(output)
  }
  
  if(inherits(varobj, "BVAR"))
  {
    output <- uc.forecast.VAR(varobj, nsteps, burnin, gibbs, exog)
    attr(output, "class") <- c("forecast.VAR")
    return(output)
  }
  
  if(inherits(varobj, "BSVAR"))
  {
    stop("Not yet implemented for BSVAR models!\n")
    ##       output <- uc.forecast.VAR(varobj, nsteps, burnin, gibbs,exog)
    ##       attr(output, "class") <- c("uc.forecast.VAR", "mts", "ts")
    ##       return(output)
  }
}

"uc.forecast.VAR" <- function(varobj, nsteps, burnin, gibbs, exog)
{ # Extract all the elements from the VAR object
  y <- varobj$y
  ar.coefs <- varobj$ar.coefs
  intercept <- varobj$intercept
  A0 <- t(chol(varobj$mean.S))
  X <- varobj$X         # rhs variables for the model
  Y <- varobj$Y         # lhs variables for the model
  H0 <- varobj$H0
  S0 <- varobj$S0
  #    mu <- varobj$hyperp
  exog.coefs <- varobj$exog.coefs
  z <- varobj$z
  #    lambda0 <- varobj$prior[1]
  #    lambda1 <- varobj$prior[2]
  #    lambda3 <- varobj$prior[3]
  #    lambda4 <- varobj$prior[4]
  #    lambda5 <- varobj$prior[5]
  #    mu5 <- varobj$prior[6]
  #    mu6 <- varobj$prior[7]
  nu <- varobj$prior[8]
  prior <- varobj$prior.type
  num.exog <- varobj$num.exog
  qm <- varobj$qm
  ncoef <- nrow(varobj$Bhat)
  
  # Get some constants we are going to need
  starttime<-date()           # Starting time for simulation
  
  p<-dim(ar.coefs)[3]         # Capture the number of lags
  # from input ar coefficients
  
  m<-ncol(y);                 # Number of endogenous
  # variables in the VAR
  k<-m*nsteps;              # k = mh, the maximal number
  # of forecasts
  
  capT<-nrow(y)               # Number of observations we
  # are going to use.
  
  # Make arrays to hold the Gibbs sampler results
  yforc<-array(0,c(gibbs,nsteps,m))
  
  # Do the Gibbs draws.....
  for(i in 1:(burnin+gibbs))
  { # Step (a): Compute a draw of the conditional forecasts
    # COMPUTE INNOVATIONS: These are the structural innovations
    
    # First draw the innovations
    epsilon.i <- matrix(rnorm(nsteps*m),nrow=nsteps,ncol=m)
    
    # Then construct a forecast using the innovations
    ytmp <- forecast.VAR(varobj, nsteps, A0=A0, shocks=epsilon.i,
                         exog)
    
    # Store draws that are past the burnin in the array
    if(i>burnin)
    { for(j in 1:m)
    { yforc[(i-burnin),(1:nsteps),j]<-ytmp[((capT+1):(capT+nsteps)),j] }
    }
    
    
    # Step (b): Compute the mode of the posterior for the
    # forecast distribution.  This is the "extended"
    # dataset that includes the i'th Gibbs sample forecast
    
    # Set up the updated Y Matrix
    # this is just "ytmp" from above
    
    Y.update <- ytmp[(capT-p+1):nrow(ytmp),]
    
    # Set up the updated X -- this is hard because we need to get
    # the RHS lags correct.  We do this by padding the existing Y
    # and then building the lags.  This reuses the code for the lag
    # construction in the szbvar code
    
    # Now build rhs -- start with an empty matrix
    X.update <- matrix(0, nsteps, m*p+1)
    # Add in the constant
    X.update[, (m*p+1)] <- matrix(1, nsteps, 1)
    # Put in the lagged y's
    
    # Note that constant is put last here when the lags are made
    for(j in 1:p)
    {
      X.update[1:nsteps,(m*(j-1)+1):(m*j)] <- matrix(Y.update[(p+1-j):(nsteps+p-j),],ncol=m)
    }
    
    # Put in exogenous coefficients if there are any.
    if(is.null(exog)==F)
    {
      X.update<-cbind(X.update,exog);
    }
    
    # Now, stack the original Y data and the augmented data.
    Y.update <- rbind(Y, ytmp[(capT+1):nrow(ytmp),])
    
    # Set up crossproducts and inverses we need
    X.update <- rbind(X, X.update)
    XX.update <- crossprod(X.update)     # Cross product of RHS variables
    hstar.update <- H0 + XX.update       # Prior + Cross product
    
    # Updated Regression estimates, Beta | Sigma
    B.update<-solve((hstar.update),(crossprod(X.update,Y.update) + H0[,1:m]))
    
    # Posterior mean of Sigma | Beta
    S.update <- (S0 + crossprod(Y.update)
                 + H0[1:m,1:m] - t(B.update)%*%(hstar.update)%*%(B.update))/(capT+nsteps+nu-m-1)
    
    # Posterior variance of B: VBh = diag(Sh.*.(inv((H0 + x'x)))
    hstarinv <- solve(hstar.update)
    vcv.Bh <- kronecker(S.update,hstarinv)
    
    # Draw from the conditional posterior pdfs of the parameters
    
    # This is only valid for just-identified models.
    df <- capT - m*p - m - 1 + nsteps
    wisharts <- rwishart(1, df, diag(m))
    
    # Generate the draws from the Wishart and the Beta
    # Wishart draw
    Sigmat <- (chol(S.update))
    Sigma.Draw <- t(Sigmat)%*%(df*solve(matrix(wisharts,m,m)))%*%Sigmat
    sqrtwish <- t(chol(Sigma.Draw))
    # Covariance of beta
    bcoefs.covar <- t(chol(vcv.Bh))
    
    # Draw beta|Sigma
    aplus <- matrix(B.update, ncol=1) + bcoefs.covar%*%matrix(rnorm(nrow(bcoefs.covar)), ncol=1)
    aplus <- matrix(aplus, ncol=m)
    
    aplus.coefs<-t(aplus[1:(m*p),]);        # extract the ar coefficients
    dim(aplus.coefs)<-c(m,m,p)                    # push ar coefs into M x M x P array
    aplus.coefs<-aperm(aplus.coefs,c(2,1,3))      # reorder array
    
    
    intercept <- aplus[(m*p+1),]       # get drawn intercept....
    ar.coefs<-aplus.coefs            # AR coefs
    A0 <- sqrtwish
    
    if(num.exog!=0)
    {
      exog.coefs <- aplus[(m*p+2):nrow(aplus),]
    }
    
    
    # Print some intermediate results to capture progress....
    # and tell us that things are still running
    if (i%%500==0)
    { cat("Gibbs Iteration = ", i, "     \n");
      if(i<=burnin)
      { cat("(Still a burn-in draw.)\n");
      }
      
    }
    # Back to the top of the Gibbs loop....
  }
  endtime<-date()
  # Print time stamp so we know how long everything took.
  cat("Start time : ", starttime, "\n");
  cat("End time   : ", endtime, "\n");
  # Returns a list object
  output <- list(forecast=yforc)
  #    attr(output, "class") <- c("forecast.VAR")
  return(output)
}

"hc.forecast" <- function(varobj, yconst, nsteps, burnin, gibbs, exog=NULL)
{
  if(inherits(varobj, "VAR"))
  {
    stop("Not yet implemented for VAR models!\nUse a BVAR model.")
    ##         output <- hc.forecast.VAR(varobj, yconst, nsteps, burnin,
    ##                                   gibbs, exog)
    ##         attr(output, "class") <- c("forecast.VAR")
    ##         return(output)
  }
  
  if(inherits(varobj, "BVAR"))
  {
    output <- hc.forecast.VAR(varobj, yconst, nsteps, burnin,
                              gibbs, exog)
    attr(output, "class") <- c("forecast.VAR")
    return(output)
  }
  
  if(inherits(varobj, "BSVAR"))
  {
    stop("Not yet implemented for B-SVAR models!\n")
    ##         output <- hc.forecast.VAR(varobj, yconst, nsteps, burnin,
    ##                                   gibbs, exog)
    ##         attr(output, "class") <- c("hc.forecast.VAR", "mts", "ts")
    ##         return(output)
  }
}

"hc.forecast.VAR" <-
  function(varobj, yconst, nsteps, burnin, gibbs, exog=NULL)
  {
    # Extract all the elements from the VAR object that we will need
    y <- varobj$y
    ar.coefs <- varobj$ar.coefs
    intercept <- varobj$intercept
    exog.coefs <- varobj$exog.coefs
    A0 <- t(chol(varobj$mean.S))
    mu <- varobj$hyperp
    prior <- varobj$prior
    ncoef <- nrow(varobj$Bhat)
    X <- varobj$X         # rhs variables for the model
    Y <- varobj$Y         # lhs variables for the model
    H0 <- varobj$H0       # precision for the var coefs
    S0 <- varobj$S0       # precision for the Sigma
    nu <- varobj$prior[8] # df
    
    # Get some constants we are going to need from the inputs
    starttime<-date()           # Starting time for simulation
    # of forecasts
    
    q<-nrow(as.matrix(yconst));   # Number of restrictions
    
    p<-dim(ar.coefs)[3]         # Capture the number of lags
    # from input ar coefficients
    
    m<-ncol(y);                 # Number of endogenous
    # variables in the VAR
    
    capT<-nrow(y)               # Number of observations we
    # are going to use.
    
    k<-m*nsteps;                # k = mh, the maximal number
    
    q <- nrow(yconst)             # Number of constraints
    
    # Make arrays to hold the Gibbs sampler results
    yforc<-array(0,c(gibbs,nsteps,m))
    
    
    # Do the Gibbs draws.....
    for(i in 1:(burnin+gibbs))
    {
      # Step (a): Compute a draw of the conditional forecasts
      # COMPUTE INNOVATIONS: These are the structural innovations
      # Solve the constraint equation for the updated forecast errors
      
      # Generate the forecasts without shocks
      ytmp<-as.matrix(coef.forecast.VAR(y, intercept, ar.coefs, exog.coefs, m, p,
                                        capT, nsteps, A0=(A0)))
      
      # Get the impulse responses that correspond to the forecasted data
      M <- irf.VAR(varobj, nsteps, A0)$mhat
      
      # Construct the draw of the orthogonalized innovations that
      # satisfy the hard condition.
      
      # These are the q constrained innovations
      r <- (yconst - ytmp[(capT+1):(capT+nsteps),])
      r<-matrix(r[1:nsteps,1],ncol=1)
      
      # Build the matrix of the impulses that define the constraint
      R <- matrix(0, k, q)
      
      # Put the g'th column of the impulse into the constraint matrix,
      # such that R * epsilon = r
      for (g in 1:q)
      {
        if(g==1)
        { R[1:length(M[,1,1]), 1] <- M[,1,1] }
        else
        {
          R[,g] <- c(M[,1,g], R[,g-1])[1:k]
        }
      }
      
      # Solve the minimization problem for the mean and variance of
      # the constrained innovations.
      
      RRinv<-solve(crossprod(R))
      mean.epsilon <- R%*%RRinv%*%r;
      var.epsilon <- diag(1,nrow=k) - (R%*%RRinv%*%t(R));
      
      # Draw from the singular MVN pdf of the constrained innovations.
      
      epsilon.i <- matrix(rmultnorm(1, mean.epsilon, var.epsilon),
                          nrow=nsteps, ncol=m, byrow=T)
      
      # Add the innovations to the forecasts
      ytmp[(capT+1):(capT +nsteps),] <- ytmp[(capT+1):(capT +nsteps),]+epsilon.i%*%A0
      
      # Store forecasts that are past the burnin point
      if(i>burnin)
      { for(j in 1:m)
      { yforc[(i-burnin),(1:nsteps),j]<-ytmp[(capT+1):(capT+nsteps),j] }
      }
      
      
      # Step (b): Compute the mode of the posterior for the
      # conditional forecast distribution.  This is the "extended"
      # dataset that includes the i'th Gibbs sample forecast
      
      
      # Build the augmented LHS and RHS matrices
      # 1) Get the nsteps+p observations we need to build the lagged
      # endogenous variables for the augmented system.
      
      Y.update <- ytmp[(capT-p+1):nrow(ytmp),]
      
      # 2) Build the updated X -- this is hard because we need to get
      # the RHS lags correct.  We do this by padding the existing Y
      # and then building the lags.  This reuses the code for the lag
      # construction in the szbvar code
      
      X.update <- matrix(0, nsteps, ncoef)
      X.update[,ncoef] <- matrix(1, nsteps, 1)
      
      # Note that constant is put last here when the lags are made
      for(j in 1:p)
      {
        X.update[1:nsteps,(m*(j-1)+1):(m*j)] <- matrix(Y.update[(p+1-j):(nsteps+p-j),],ncol=m)
      }
      
      # Put on the exogenous regressors and make the constant the
      # first exog regressor after the AR coefs
      if(is.null(exog)==F)
      {
        X.update<-cbind(X.update,exog);
      }
      
      # Now, stack the original Y data and the augmented data.
      Y.update <- rbind(Y, ytmp[(capT+1):nrow(ytmp),])
      
      # Set up crossproducts and inverses we need
      X.update <- rbind(X, X.update)
      XX.update <- crossprod(X.update)     # Cross product of RHS variables
      hstar.update <- H0 + XX.update       # Prior + Cross product
      
      # Updated Regression estimates, Beta | Sigma
      B.update<-solve((hstar.update),(crossprod(X.update,Y.update) + H0[,1:m]))
      
      # Posterior mean of Sigma | Beta
      S.update <- (S0 + crossprod(Y.update)
                   + H0[1:m,1:m] - t(B.update)%*%(hstar.update)%*%(B.update))/(capT+nsteps+nu-m-1)
      
      # Posterior variance of B: VBh = diag(Sh.*.(inv((H0 + x'x)))
      hstarinv <- solve(hstar.update)
      vcv.Bh <- kronecker(S.update,hstarinv)
      
      # Draw from the conditional posterior pdfs of the parameters
      
      # This is only valid for just-identified models.
      df <- capT - m*p - m - 1 + nsteps
      wisharts <- rwishart(1, df, diag(m))
      
      # Generate the draws from the Wishart and the Beta
      # Wishart draw
      Sigmat <- (chol(S.update))
      Sigma.Draw <- t(Sigmat)%*%(df*solve(matrix(wisharts,m,m)))%*%Sigmat
      sqrtwish <- t(chol(Sigma.Draw))
      # Covariance of beta
      bcoefs.covar <- t(chol(vcv.Bh))
      
      # Draw of beta|Sigma ~ MVN(B.update, S.Update .*. Hstarinv)
      aplus <- matrix(B.update, ncol=1) +
        bcoefs.covar%*%matrix(rnorm(nrow(bcoefs.covar)), ncol=1)
      
      # Reshape and extract the coefs
      aplus <- matrix(aplus, ncol=m)
      aplus.coefs<-t(aplus[1:(m*p),]);          # extract the ar coefficients
      dim(aplus.coefs)<-c(m,m,p)                # push ar coefs into M x M x P array
      aplus.coefs<-aperm(aplus.coefs,c(2,1,3))  # reorder array
      
      intercept <- aplus[(m*p+1),]      # get drawn intercept....
      ar.coefs<-aplus.coefs             # AR coefs
      A0 <- sqrtwish
      
      #      exog.coefs <- aplus[(m*p+2):nrow(aplus),]
      
      # Need to add something here to deal with the exogenous
      # regressors!
      
      
      # Print some intermediate results to capture progress....
      # and tell us that things are still running
      if (i%%1000==0)
      { cat("Gibbs Iteration = ", i, "     \n");
        if(i<=burnin)
        { cat("(Still a burn-in draw.)\n");
        }
      }
      # Back to the top of the Gibbs loop....
    }
    endtime<-date()
    # Print time stamp so we know how long everything took.
    cat("Start time : ", starttime, "\n");
    cat("End time   : ", endtime, "\n");
    # Returns a list object
    output <- list(forecast=yforc, orig.y=y) #llf=ts(llf),hyperp=c(mu,prior)))
    attr(output, "class") <- c("forecast.VAR")
    return(output)
  }


# Forecasting function for MSBVAR models.
#
# 20110113 : Initial version
# 20110628 : Clean out remaining bugs on DF corrections for
#            observations
# 20120207 : Updated to work with revisions to MSBVAR

###### MSBVARfcast #####
# Forecast for one draw -- this is an internal function that only
# returns the forecasts averaged over the regimes.
#
# Inputs :
# y = data modeled + forecast steps
# k = number of forecast steps
# h = number of regimesq
# ar.coefs = array of c(m,m,p,h) for the MSBVAR
# intercepts = array of c(m,h) for the intercepts
# Sigma = array of c(m.m,h) for the variances
# shock = array of c(k,m,h) for the shocks
# ss = matrix of c(k,h) for the regimes

MSBVARfcast <- function(y, k, h, ar.coefs, intercepts, shocks, ss)
{
  # Get constants
  dims <- dim(ar.coefs)
  m <- dims[1]
  p <- dims[3]
  capT <- nrow(y)
  
  # Set up the object to hold the forecasts
  # Zero out forecast periods so we can cumulate sums over time,
  # lags, and regimes to get the correct weighted forecast.
  
  forcs <- rbind(y, matrix(0, k, m))
  
  for(j in 1:k)     # Loop over forecast steps
  {
    for(i in 1:h) # Now over the regime weights, multiplying by
      # the regime weights and aggregating
    {
      forcs[capT + j,] <- forcs[capT+j,] +
        ss[(capT+j),i]*(forcs[capT + j - 1,] %*%
                          ar.coefs[,,1,i] + intercepts[,i] + shocks[j,,i])
      # Now add in the other lags -- be sure they are from the
      # right state!
      if(p>1) { for(lg in 2:p)
      { forcs[capT + j, ] <- (forcs[capT + j, ] +
                                ss[(capT+j-lg),i]*(forcs[capT + j - lg, ] %*% ar.coefs[,,lg,i]))
      }
      }
    }
  }
  
  return(forcs[(capT+1):(capT+k),])
}

# Betai2coefs
# Converts the draw Betai into arrays of AR coefficents for each
# regime and the intercepts
Betai2coefs <- function(Betai, m, p, h)
{
  ar.coefsi <- array(0, c(m,m,p,h))
  for(i in 1:h)
  {
    tmp <- t(Betai[1:(m*p),,i])
    dim(tmp) <- c(m,m,p)
    ar.coefsi[,,,i] <- aperm(tmp, c(2,1,3))  # lags then regimes.
    #        ar.coefs[,,,i] <- tmp
  }
  intercepts <- Betai[(m*p+1),,]
  return(list(ar.coefsi=ar.coefsi, intercepts=intercepts))
}


updateinit <- function(Y, init.model)
{
  n<-nrow(Y);	 	# of observations in data set
  m<-ncol(Y)	# of variables in data set
  p <- init.model$p
  
  # Compute the number of coefficients
  ncoef<-(m*p)+1;  # AR coefficients plus intercept in each RF equation
  ndum<-m+1;                # of dummy observations
  capT<-n-p+ndum;   	      # # of observations used in mixed estimation (Was T)
  Ts<-n-p;  		      # # of actual data observations @
  
  # Declare the endoegnous variables as a matrix.
  dat<-as.matrix(Y);
  
  # Create data matrix including dummy observations
  # X: Tx(m*p+1)
  # Y: Txm, ndum=m+1 prior dummy obs (sums of coeffs and coint).
  # mean of the first p data values = initial conditions
  if(p==1)
  { datint <- as.vector(dat[1,]) }
  else
  { datint<-as.vector(apply(dat[1:p,],2,mean)) }
  
  # Y and X matrices with m+1 initial dummy observations
  X<-matrix(0, nrow=capT, ncol=ncoef)
  Y<-matrix(0, nrow=capT, ncol=m)
  const<-matrix(1, nrow=capT)
  const[1:m,]<-0;	         # no constant for the first m periods
  X[,ncoef]<-const;
  
  # Build the dummy observations we need
  for(i in 1:m)
  {
    Y[ndum,i]<-datint[i];
    Y[i,i]<-datint[i];
    for(j in 1:p)
    { X[ndum,m*(j-1)+i]<-datint[i];
    X[i,m*(j-1)+i]<-datint[i];
    }
  }
  
  # Note that constant is put last here when the lags are made
  for(i in 1:p)
  { X[(ndum+1):capT,(m*(i-1)+1):(m*i)]<-matrix(dat[(p+1-i):(n-i),],ncol=m)
  }
  
  # Put on the exogenous regressors and make the constant the
  # first exog regressor after the AR coefs
  ## if(is.null(z)==F)
  ##   {
  ##     pad.z <- matrix(0,nrow=capT,ncol=ncol(z))
  ##     pad.z[(ndum+1):capT,] <- matrix(z[(p+1):n,], ncol=ncol(z))
  ##     X<-cbind(X,pad.z);
  ##   }
  
  # Get the corresponding values of Y
  Y[(ndum+1):capT,]<-matrix(dat[(p+1):n,],ncol=m);
  
  # Weight dummy observations
  X[1:m,] <- init.model$prior[6]*X[1:m,];
  Y[1:m,] <- init.model$prior[6]*Y[1:m,];
  X[ndum,] <- init.model$prior[7]*X[ndum,];
  Y[ndum,] <- init.model$prior[7]*Y[ndum,];
  
  init.model$X <- X
  init.model$Y <- Y
  init.model$hstar <- (init.model$H0 + crossprod(X))
  
  return(init.model)
}


# This is an amalgam of uc.forecast and gibbs.msbvar.  In this
# version, the same steps as gibbs.msbvar are used, but with the
# updated data from the forecast step.

# x = posterior mode object -- can either be from gibbs.msbvar or
#     msbvar.  Need to handle identification steps or permute as
#     appropriate.  Should warn against permuting!
# k = number of forecast steps.
#
# -----------------------------------------------------------------#
################## Steps in MSBVAR forecasting #####################
# -----------------------------------------------------------------#
# A) Augment the dataset from the mode -- forecasting
# B) Update the input data
# C) For given parameters,
#    1) draw statespace
#    2) update / draw Q
#    3) update regression -- need to remake the input matrices as part
#       of this.
#    4) sample variances
#    5) sample regression
#    6) update errors
#    7) Impose identification on the regimes (if necessary)
# top of loop
# -----------------------------------------------------------------#
#
# Initial version assumes user is inputing an identified object from
# gibbs.msbvar().  Need to add a warning when they using an msbvar()
#     object and reconcile this.
#
forecast.MSBVAR <- function(x, k, N1=1000, N2=1000)
{
  # Get some constants / objects
  init.model <- x$init.model  # initial model with the prior
  # matrices.
  
  # Get other constants from the input object
  h <- x$h
  m <- x$m
  p <- x$p
  
  # Set the sampler method for Q based on inputs
  if(attr(x, "Qsampler")=="Gibbs") Qsampler <- Q.drawGibbs
  if(attr(x, "Qsampler")=="MH") Qsampler <- Q.drawMH
  
  # Input data
  y <- init.model$y
  
  # Get sample size of original dataset and the effective sample
  TT <- capT <- nrow(init.model$y)
  
  # Settle how we are going to handle input sample size versus the
  # other sample sizes for the inputs / outputs.  Do this the same
  # way as the earlier unconditional forecasting code.  In this
  # code, we treated capT as the full original sample size minus the
  # lags to make it match the estimator. We then
  # manipulate from that.  We do this by always forecasting off of
  # the original data from capT+1 to capT+k,  This is the principal
  # in uc.forecast() and forecast.VAR and coef.forecast.VAR() [in
  # hidden.R].  The difference here is that tbe value of capT has to
  # be adjusted compared to what is in the other code.
  #
  # Principal in this is just working with data augmentation off of
  # the original data setup.
  
  Tp <- TT-p # effective sample
  
  alpha.prior <- x$alpha.prior
  
  # Get the modes for the posterior
  Q <- matrix(apply(x$Q.sample, 2, mean), h, h)
  Betai <- array(apply(x$Beta.sample, 2, mean), c(m*p+1, m, h))
  tmp <- Betai2coefs(Betai, m, p, h)
  ar.coefsi <- tmp$ar.coefsi
  intercepts <- tmp$intercepts
  
  Sigmai <- array(apply(matrix(apply(x$Sigma.sample, 2, mean),
                               m*(m+1)/2, h),
                        2, xpnd), c(m,m,h))
  intercepts <- Betai[(m*p+1),,]
  
  # Extend the dataset / input data
  # Residuals and regression
  hreg <- hregime.reg2(h, m, p, mean.SS(x), init.model)
  e <- array(0, c(Tp+k, m, h))
  
  # Now take random draws from the error process for each regime to
  # pad out initial values for the forecast periods.
  Sigmachol <- aperm(array(apply(Sigmai, 3, chol), c(m,m,h)),
                     c(2,1,3))
  
  for(i in 1:h)
  {
    etmp <-  t(Sigmachol[,,i]%*%matrix(rnorm(k*m), m, k))
    e[,,i] <- rbind(hreg$e[,,i], etmp)
  }
  
  # State-space initialization -- flat convolution of the last
  # state!
  
  tmpSS <- mean.SS(x)
  sstmp <- rbind(mean.SS(x), matrix(rep(NA, k*h), k, h))
  
  for(i in 1:k)
  {
    tmp <- sstmp[Tp+i-1,]%*%Q
    sstmp[Tp+i,] <- tmp/sum(tmp)
    
  }
  
  transtmp <- count.transitions(sstmp)
  ss <- list(SS=sstmp, transitions=transtmp)
  
  # Make list objects for Beta and Sigma
  Betai <- list(Betai=Betai)
  Sigmai <- list(Sigmai=Sigmai)
  
  # Storage
  forecasts <- array(0, c(N2, k, m))
  states <- vector("list", N2)
  
  # Burnin loop + Final loop
  for (j in 1:(N1+N2))
  {
    
    # Update the residuals
    shocks <- array(rnorm(m*k*h), c(k, m, h))
    
    Sigmai.chol <- aperm(array(apply(Sigmai$Sigmai, 3, chol), c(m,m,h)),
                         c(2,1,3))
    
    for(i in 1:h)
    {
      #             shocks[,,i] <- Sigmai.chol[,,i]%*%shocks[,,i]
      shocks[,,i] <- t(tcrossprod(Sigmai.chol[,,i], shocks[,,i]))
      e[(Tp+1):(Tp+k),,i] <- (shocks[,,i])
    }
    
    
    # Forecast
    
    fcast <- MSBVARfcast(y[(p+1):nrow(y),], k, h, ar.coefsi,
                         intercepts, shocks, ss$SS)
    
    #        print(round(cbind(shocks[,,1], shocks[,,2], fcast), 2))
    
    # Update / Draw statespace
    
    oldtran <- matrix(0,h,h)
    
    while(sum(diag(oldtran)==0)>0)
    {
      ss <- SS.ffbs(e, (Tp+k), m, p, h, Sigmai$Sigmai, Q)
      oldtran <- ss$transitions
    }
    
    #print(ss$transitions)
    ## plot(ts(ss$SS), plot.type="s", col=1:h,
    ##      main=paste("Iteration :", j))
    
    #if(sum(diag(ss$transitions)==0)>0) stop("Oops.")
    
    # Draw Q
    Q <- Qsampler(Q, ss$transitions, prior=alpha.prior, h)
    
    # Update regression
    # Update the model object input with the new data -- this is
    # not very efficient right now, but it works
    
    # Need to re-initialize the init.model object with the latest
    # forecast data so it can be an input to the next sample calls
    #
    # This should be really done with some BVAR setup function
    # that is then used in all of the later models in the pkg --
    # something to do later for speed.
    
    init.model <- updateinit(Y=rbind(y,fcast), init.model)
    
    # Update regression steps
    hreg <- hregime.reg2(h, m, p, ss$SS, init.model)
    
    # Draw variances
    
    #        cat("Iteration :", j, "\n")
    #        print(hreg$Sigmak)
    
    Sigmai <- Sigma.draw(m, h, ss, hreg, Sigmai$Sigmai)
    
    #        print(Sigmai)
    
    # Draw VAR regression coefficients
    Betai <- Beta.draw(m, p, h, Sigmai$Sigmai, hreg, init.model,
                       Betai$Betai)
    
    # Split out AR coefs and intercepts
    tmp <- Betai2coefs(Betai$Betai, m, p, h)
    ar.coefsi <- tmp$ar.coefsi
    intercepts <- tmp$intercepts
    
    # Update error estimate
    e <- residual.update(m, h, init.model, Betai$Betai, e)
    
    # Permute regimes if necessary / order regimes
    
    # Save results if past burnin=N1.
    # Store forecasts and states
    if(j > N1)
    {
      for(i in 1:m)
      {
        forecasts[(j-N1),,i] <- t(fcast[,i])
        states[[(j-N1)]] <-
          as.bit.integer(as.integer(ss$SS[,1:(h-1)]))
      }
    }
    
    # Print out iteration information
    if(j%%1000==0) cat("Iteration : ", j, "\n")
  }
  
  # Define the output object and its attributes
  class(forecasts) <- c("forecast.MSBVAR")
  class(states) <- c("SS")
  
  output <- list(forecasts=forecasts, ss.sample=states, k=k, h=h)
  
  # Add classing and attributes here.  These will be use later for
  # the plotting and other summary functions.
  attr(output, "eqnames") <- attr(x, "eqnames")
  tmp <- tsp(x$init.model$y)
  attr(output, "start") <- tmp[1]
  attr(output, "end") <- tmp[2]
  attr(output, "freq") <- tmp[3]
  
  return(output)
}

#############initialize.msbvar.R ####################
# initialize.msbvar.R -- Sets up initial values for the block
#                        optimization method used to find the mode for
#                        the MSVAR models
#
# 20120110 : Initial version -- PTB
# 20120504 : Updated to get initial regime parameters from a kmeans()
#            of the data
# 20120515 : Updated to use kmeans of VAR residuals to make more
#            robust
#

initialize.msbvar <- function(y, p, z=NULL, lambda0, lambda1, lambda3,
                              lambda4, lambda5, mu5, mu6, nu=NULL,
                              qm, prior, h, Q=NULL)
  
{
  # Set up the constants in the data matrices and the prior using a
  # call to szbvar()
  m <- ncol(y)
  if(is.null(nu)) nu <- m
  
  tmp <- szbvar(y, p, z=z, lambda0, lambda1, lambda3,
                lambda4, lambda5, mu5, mu6, nu=m,
                qm=qm, prior=prior,
                posterior.fit=FALSE)
  
  # Define outputs for blkopt() and populate them
  thetahat.start <- array(NA, c(m, 1+m*p+m, h))
  
  # Use a kmeans of the data to separate across the regimes to
  # populate the regime-specific coefficients.
  
  #regimes <- kmeans(y, centers=h)$cluster
  regimes <- kmeans(tmp$residuals[(m+1):nrow(tmp$residuals),], centers=h)$cluster
  fptmp <- regimes[(p+1):length(regimes)]
  
  ################################################################
  # Now do regime-specific regressions for the data
  ################################################################
  
  # Parameters for regime-specific regressions
  df <- table(fptmp)
  #    Sigma.draw <- array(NA, c(m,m,h))
  
  for(i in 1:h)
  {
    s <- c(rep(i,m+1), fptmp)
    Y1 <- tmp$Y[s==i,]
    X1 <- tmp$X[s==i,]
    XX <- crossprod(X1) + tmp$H0
    reg <- qr.coef(qr(crossprod(X1)),
                   crossprod(X1,Y1) + tmp$H0[,1:m])
    e <- Y1 - X1%*%reg
    
    # Sample the regime coefs
    # a) Inv-Wishart for Sigma for each regime
    #        wisharts <- rwishart(h, df[i], diag(m))
    cSigma <- (tmp$S0 + tmp$H0[1:m,1:m] + crossprod(e))/(df[i]+nu)
    
    # Draw Sigma and tune it
    #        Sigma.draw[,,i] <-
    #            t(cSigma)%*%(df[i]*solve(wisharts[,,i]))%*%cSigma
    
    # Draw the regressors based on the tuned values
    ## bcoefs.se <- t(chol(chol2inv(chol(kronecker(cSigma, XX)))))
    
    
    ## bcoefs <- as.vector(reg) + crossprod(bcoefs.se,
    ##                                      rnorm((m^2)*p+m))
    
    ## # Reorder in MSBVAR format for VAR reg coefs
    ## Bhat <- matrix(bcoefs, ncol=m)
    
    # Populate the optimization object / array for blkopt
    
    # Intercepts
    thetahat.start[1:m,1,i] <- (reg[((m*p)+1),])
    
    # AR coefs
    thetahat.start[1:m, 2:((m*p)+1), i] <- t(reg[1:(m*p),])
    
    # Variances
    thetahat.start[1:m, (2+(m*p)):ncol(thetahat.start), i] <-
      t(cSigma)
    
    
  }
  
  # Now check the input Q or populate one.
  
  if(is.null(Q)==TRUE)
  {
    # Make Q from kmeans output
    lt <- length(fptmp)
    Qtmp <- table(fptmp[2:lt], fptmp[1:(lt-1)])
    Q <- Qtmp/rowSums(Qtmp)
  }
  
  # Now validate Q, user input
  
  if(is.null(Q)==FALSE)
  {
    
    if(sum(ifelse(rowSums(Q)==1, 1, 0))<h)
    {
      stop("initialize.msbvar(): Invalid user input: Improper initial Q, transition matrix, for the MS process.  Rows must sum to 1.\n")
      
    }
  }
  
  # Return the final object we need.
  return(list(init.model=tmp, thetahat.start=thetahat.start, Qhat.start=Q))
}

####### msvar.R #########
### msvar.R -- Estimates the MLE for MSVAR and MS univariate
### models.

# 20120109 : Initial version by Ryan Davis


msvar <- function(Y, p, h, niterblkopt=10)
{
  
  # Switching indicator: 'IAH' totally switching
  # fixed for now
  indms <- 'IAH'
  
  n <- nrow(Y)
  m <- ncol(Y)
  
  # check value of h
  if(h<2) stop("h should be an integer >=2")
  
  # Now do a baseline, non-regime model using szbvar() since this
  # gives us all of the inputs we need for later.
  # n.b. the below specification is equivalent to MLE
  #      (but if mu5 or mu6 does not equal zero, then problems)
  init.model <- szbvar(ts(Y), p,
                       lambda0=1, lambda1=1, lambda3=1, lambda4=1,
                       lambda5=1, mu5=0, mu6=0, prior=2)
  
  # set initial parameters for blockwise optimization
  # initial Q
  Qhat.start <- (1-(h*0.1/(h-1)))*diag(h) + matrix(0.1/(h-1), h, h)
  
  # array for storage
  thetahat.start <- array(NA, c(m, 1+m*p+m, h))
  
  # set intercept and AR coef initial values all to zero
  thetahat.start[,1:(1+m*p),] <- 0
  
  # set sigma initial values
  # first, get residuals from initial model
  # dummy obs are appended, so adjust for those
  res.im <- init.model$residuals[(m+2):n,]
  sig2.start <- (1/n)*crossprod(res.im, res.im)
  
  # the sig2 starting values need to be different though,
  # so adjust these by a small amount over regimes
  for (i in 1:h) { thetahat.start[,(1+m*p+1):(1+m*p+m),i] <-
    sig2.start}
  
  blkopt.est <- blkopt(Y=Y, p=p, thetahat.start=thetahat.start,
                       Qhat.start=Qhat.start, niter=niterblkopt,
                       indms)
  
  
  # now, setup hreg, adjusting for dummies
  hreg <- hregime.reg2.mle(h, m, p, TT=(n-p), fp=blkopt.est$fpH, init.model)
  
  
  output <- list(init.model=init.model,
                 hreg=hreg,
                 Q=blkopt.est$Qhat,
                 fp=blkopt.est$fpH,
                 m=m, p=p, h=h,
                 llfval=blkopt.est$llfval,
                 DirectBFGSLastSuccess=blkopt.est$DirectBFGSLastSuccess)
  class(output) <- "MSVAR"
  
  return(output)
  
} # end mlemsvar() function




# Ryan adjusted several items from the original hregime.reg2 function
hregime.reg2.mle <- function(h, m, p, TT, fp, init.model)
{
  
  # Storage
  tmp <- vector(mode="list", length=h)
  Bk <- array(0, c(m*p+1, m, h))
  Sigmak <- array(0, c(m,m,h))
  df <- apply(fp, 2, sum)
  e <- array(0, c(TT, m, h))
  Y <- init.model$Y[(m+1+1):nrow(init.model$Y),]
  X <- init.model$X[(m+1+1):nrow(init.model$X),]
  
  # Loops to compute
  # 1) sums of squares for X and Y
  # 2) B(k) matrices
  # 3) Residuals
  # 4) Sigma(k) matrices
  
  for(i in 1:h)
  {
    # Note how the dummy obs. are appended to the moment matrices
    Sxy <- crossprod(X, diag(fp[,i]))%*%Y + crossprod(init.model$X[1:(m+1),], init.model$Y[1:(m+1),])
    Sxx <- crossprod(X, diag(fp[,i]))%*%X + crossprod(init.model$X[1:(m+1),])
    
    # Compute the regression coefficients
    hstar <- Sxx# + init.model$H0
    #Bk[,,i] <- solve(hstar,
    #                  (Sxy + init.model$H0[,1:m]))
    Bk[,,i] <- solve(hstar,Sxy,tol=1e-100)
    
    # Compute residuals and Sigma (based on Krolzig)
    
    # Get the full residuals -- need these for filtering
    e[,,i] <- Y - X%*%Bk[,,i]
    
    #Sigmak[,,i] <- (init.model$S0 + crossprod(e[,,i],diag(fp[,i]))%*%e[,,i])/df[i]
    Sigmak[,,i] <- (crossprod(e[,,i],diag(fp[,i]))%*%e[,,i])/df[i]
    
    # Save the moments
    tmp[[i]] <- list(Sxy=Sxy, Sxx=Sxx) #, ytmp=ytmp, xtmp=xtmp)
  }
  
  return(list(Bk=Bk, Sigmak=Sigmak, df=df, e=e, moment=tmp))
}

######### forecast_msbvar.R ######
# Generate forecast methods for the models in the MSBVAR package
#
# Patrick T. Brandt
#
# 20120621 : Updated to include MSBVAR forecast functions.

"forecast.szbvar" <- function(varobj, nsteps, A0=t(chol(varobj$mean.S)),
                       shocks=matrix(0,nrow=nsteps,ncol=dim(varobj$ar.coefs)[1]),
                       exog.fut=matrix(0,nrow=nsteps,ncol=nrow(varobj$exog.coefs)),
                       N1, N2)
{
  if(inherits(varobj,"VAR")){
    return(forecast.VAR(varobj, nsteps, A0=A0,
                        shocks=shocks, exog.fut=exog.fut))
  }
  
  if(inherits(varobj, "BVAR")){
    return(forecast.VAR(varobj, nsteps, A0=A0,
                        shocks=shocks, exog.fut=exog.fut))
  }
  
  if(inherits(varobj, "BSVAR")){
    return(forecast.VAR(varobj, nsteps, A0=solve(varobj$A0.mode),
                        shocks=shocks, exog.fut=exog.fut))
  }
  
  if(inherits(varobj, "MSBVAR")){
    return(forecast.MSBVAR(x=varobj, k=nsteps, N1, N2))
  }
}

# This is the generic VAR forecasting function.  The other
"forecast.VAR" <-
  function(varobj, nsteps, A0, shocks, exog.fut)
  {
    # Set up the initial parameters for the VAR forecast function from
    #  VAR object
    y <- varobj$y
    intercept <- varobj$intercept
    ar.coefs <- varobj$ar.coefs
    exog.coefs <- varobj$exog.coefs
    m<-dim(ar.coefs)[1]
    p<-dim(ar.coefs)[3]
    capT<-nrow(y)
    yhat<-rbind(y,matrix(0,ncol=m,nrow=nsteps))
    
    # Compute the deterministic part of the forecasts (less the intercept!)
    if(is.na(sum(varobj$exog.coefs))==F)
    {
      deterministic.VAR <- as.matrix(exog.fut) %*% exog.coefs
    }
    else
    { deterministic.VAR <- matrix(0,nrow=nsteps,ncol=m)
    }
    
    # Now loop over the forecast horizon
    for(h in 1:nsteps)
    {  yhat[capT + h, ] <- (yhat[capT + h - 1,] %*% ar.coefs[,,1] +
                              intercept + deterministic.VAR[h,] + (shocks[h,]%*%A0))
    if (p>1) {for(i in 2:p)
    { yhat[capT + h, ] <- (yhat[capT + h, ] +
                             (yhat[capT + h - i, ] %*% ar.coefs[,,i]))
    
    }}
    }
    output <- ts(yhat, start = start(varobj$y), frequency = frequency(varobj$y), names = colnames(varobj$y))
    attr(output, "class") <- c("forecast.VAR", "mts", "ts")
    attr(output, "eqnames") <- attr(varobj, "eqnames")
    return(output)
  }

"forecast.BVAR" <- function(varobj, nsteps, A0, shocks, exog.fut)
{
  output <- forecast.VAR(varobj, nsteps, A0, shocks, exog.fut)
  attr(output, "class") <- c("forecast.BVAR", "mts", "ts")
  attr(output, "eqnames") <- attr(varobj, "eqnames")
  return(output)
}

"forecast.BSVAR" <- function(varobj, nsteps, A0=solve(varobj$A0.mode), shocks, exog.fut)
{
  output <- forecast.VAR(varobj, nsteps, A0, shocks, exog.fut)
  attr(output, "class") <- c("forecast.BSVAR", "mts", "ts")
  attr(output, "eqnames") <- attr(varobj, "eqnames")
  return(output)
}

"uc.forecast" <- function(varobj, nsteps, burnin, gibbs,
                          exog=NULL)
{
  if(inherits(varobj, "VAR"))
  {
    stop("Not implemented for VAR models!\nUse a BVAR with a flat-flat prior if you want this case.\n")
    ##         varobj$H0 <- matrix(0, nrow(varobj$Bhat), nrow(varobj$Bhat))
    ##         varobj$S0 <- matrix(0, ncol(varobj$Bhat), ncol(varobj$Bhat))
    ##         output <- uc.forecast.VAR(varobj, nsteps, burnin, gibbs, exog)
    ##         attr(output, "class") <- c("forecast.VAR")
    ##         return(output)
  }
  
  if(inherits(varobj, "BVAR"))
  {
    output <- uc.forecast.VAR(varobj, nsteps, burnin, gibbs, exog)
    attr(output, "class") <- c("forecast.VAR")
    return(output)
  }
  
  if(inherits(varobj, "BSVAR"))
  {
    stop("Not yet implemented for BSVAR models!\n")
    ##       output <- uc.forecast.VAR(varobj, nsteps, burnin, gibbs,exog)
    ##       attr(output, "class") <- c("uc.forecast.VAR", "mts", "ts")
    ##       return(output)
  }
}

"uc.forecast.VAR" <- function(varobj, nsteps, burnin, gibbs, exog)
{ # Extract all the elements from the VAR object
  y <- varobj$y
  ar.coefs <- varobj$ar.coefs
  intercept <- varobj$intercept
  A0 <- t(chol(varobj$mean.S))
  X <- varobj$X         # rhs variables for the model
  Y <- varobj$Y         # lhs variables for the model
  H0 <- varobj$H0
  S0 <- varobj$S0
  #    mu <- varobj$hyperp
  exog.coefs <- varobj$exog.coefs
  z <- varobj$z
  #    lambda0 <- varobj$prior[1]
  #    lambda1 <- varobj$prior[2]
  #    lambda3 <- varobj$prior[3]
  #    lambda4 <- varobj$prior[4]
  #    lambda5 <- varobj$prior[5]
  #    mu5 <- varobj$prior[6]
  #    mu6 <- varobj$prior[7]
  nu <- varobj$prior[8]
  prior <- varobj$prior.type
  num.exog <- varobj$num.exog
  qm <- varobj$qm
  ncoef <- nrow(varobj$Bhat)
  
  # Get some constants we are going to need
  starttime<-date()           # Starting time for simulation
  
  p<-dim(ar.coefs)[3]         # Capture the number of lags
  # from input ar coefficients
  
  m<-ncol(y);                 # Number of endogenous
  # variables in the VAR
  k<-m*nsteps;              # k = mh, the maximal number
  # of forecasts
  
  capT<-nrow(y)               # Number of observations we
  # are going to use.
  
  # Make arrays to hold the Gibbs sampler results
  yforc<-array(0,c(gibbs,nsteps,m))
  
  # Do the Gibbs draws.....
  for(i in 1:(burnin+gibbs))
  { # Step (a): Compute a draw of the conditional forecasts
    # COMPUTE INNOVATIONS: These are the structural innovations
    
    # First draw the innovations
    epsilon.i <- matrix(rnorm(nsteps*m),nrow=nsteps,ncol=m)
    
    # Then construct a forecast using the innovations
    ytmp <- forecast.VAR(varobj, nsteps, A0=A0, shocks=epsilon.i,
                         exog)
    
    # Store draws that are past the burnin in the array
    if(i>burnin)
    { for(j in 1:m)
    { yforc[(i-burnin),(1:nsteps),j]<-ytmp[((capT+1):(capT+nsteps)),j] }
    }
    
    
    # Step (b): Compute the mode of the posterior for the
    # forecast distribution.  This is the "extended"
    # dataset that includes the i'th Gibbs sample forecast
    
    # Set up the updated Y Matrix
    # this is just "ytmp" from above
    
    Y.update <- ytmp[(capT-p+1):nrow(ytmp),]
    
    # Set up the updated X -- this is hard because we need to get
    # the RHS lags correct.  We do this by padding the existing Y
    # and then building the lags.  This reuses the code for the lag
    # construction in the szbvar code
    
    # Now build rhs -- start with an empty matrix
    X.update <- matrix(0, nsteps, m*p+1)
    # Add in the constant
    X.update[, (m*p+1)] <- matrix(1, nsteps, 1)
    # Put in the lagged y's
    
    # Note that constant is put last here when the lags are made
    for(j in 1:p)
    {
      X.update[1:nsteps,(m*(j-1)+1):(m*j)] <- matrix(Y.update[(p+1-j):(nsteps+p-j),],ncol=m)
    }
    
    # Put in exogenous coefficients if there are any.
    if(is.null(exog)==F)
    {
      X.update<-cbind(X.update,exog);
    }
    
    # Now, stack the original Y data and the augmented data.
    Y.update <- rbind(Y, ytmp[(capT+1):nrow(ytmp),])
    
    # Set up crossproducts and inverses we need
    X.update <- rbind(X, X.update)
    XX.update <- crossprod(X.update)     # Cross product of RHS variables
    hstar.update <- H0 + XX.update       # Prior + Cross product
    
    # Updated Regression estimates, Beta | Sigma
    B.update<-solve((hstar.update),(crossprod(X.update,Y.update) + H0[,1:m]))
    
    # Posterior mean of Sigma | Beta
    S.update <- (S0 + crossprod(Y.update)
                 + H0[1:m,1:m] - t(B.update)%*%(hstar.update)%*%(B.update))/(capT+nsteps+nu-m-1)
    
    # Posterior variance of B: VBh = diag(Sh.*.(inv((H0 + x'x)))
    hstarinv <- solve(hstar.update)
    vcv.Bh <- kronecker(S.update,hstarinv)
    
    # Draw from the conditional posterior pdfs of the parameters
    
    # This is only valid for just-identified models.
    df <- capT - m*p - m - 1 + nsteps
    wisharts <- rwishart(1, df, diag(m))
    
    # Generate the draws from the Wishart and the Beta
    # Wishart draw
    Sigmat <- (chol(S.update))
    Sigma.Draw <- t(Sigmat)%*%(df*solve(matrix(wisharts,m,m)))%*%Sigmat
    sqrtwish <- t(chol(Sigma.Draw))
    # Covariance of beta
    bcoefs.covar <- t(chol(vcv.Bh))
    
    # Draw beta|Sigma
    aplus <- matrix(B.update, ncol=1) + bcoefs.covar%*%matrix(rnorm(nrow(bcoefs.covar)), ncol=1)
    aplus <- matrix(aplus, ncol=m)
    
    aplus.coefs<-t(aplus[1:(m*p),]);        # extract the ar coefficients
    dim(aplus.coefs)<-c(m,m,p)                    # push ar coefs into M x M x P array
    aplus.coefs<-aperm(aplus.coefs,c(2,1,3))      # reorder array
    
    
    intercept <- aplus[(m*p+1),]       # get drawn intercept....
    ar.coefs<-aplus.coefs            # AR coefs
    A0 <- sqrtwish
    
    if(num.exog!=0)
    {
      exog.coefs <- aplus[(m*p+2):nrow(aplus),]
    }
    
    
    # Print some intermediate results to capture progress....
    # and tell us that things are still running
    if (i%%500==0)
    { cat("Gibbs Iteration = ", i, "     \n");
      if(i<=burnin)
      { cat("(Still a burn-in draw.)\n");
      }
      
    }
    # Back to the top of the Gibbs loop....
  }
  endtime<-date()
  # Print time stamp so we know how long everything took.
  cat("Start time : ", starttime, "\n");
  cat("End time   : ", endtime, "\n");
  # Returns a list object
  output <- list(forecast=yforc)
  #    attr(output, "class") <- c("forecast.VAR")
  return(output)
}

"hc.forecast" <- function(varobj, yconst, nsteps, burnin, gibbs, exog=NULL)
{
  if(inherits(varobj, "VAR"))
  {
    stop("Not yet implemented for VAR models!\nUse a BVAR model.")
    ##         output <- hc.forecast.VAR(varobj, yconst, nsteps, burnin,
    ##                                   gibbs, exog)
    ##         attr(output, "class") <- c("forecast.VAR")
    ##         return(output)
  }
  
  if(inherits(varobj, "BVAR"))
  {
    output <- hc.forecast.VAR(varobj, yconst, nsteps, burnin,
                              gibbs, exog)
    attr(output, "class") <- c("forecast.VAR")
    return(output)
  }
  
  if(inherits(varobj, "BSVAR"))
  {
    stop("Not yet implemented for B-SVAR models!\n")
    ##         output <- hc.forecast.VAR(varobj, yconst, nsteps, burnin,
    ##                                   gibbs, exog)
    ##         attr(output, "class") <- c("hc.forecast.VAR", "mts", "ts")
    ##         return(output)
  }
}

"hc.forecast.VAR" <-
  function(varobj, yconst, nsteps, burnin, gibbs, exog=NULL)
  {
    # Extract all the elements from the VAR object that we will need
    y <- varobj$y
    ar.coefs <- varobj$ar.coefs
    intercept <- varobj$intercept
    exog.coefs <- varobj$exog.coefs
    A0 <- t(chol(varobj$mean.S))
    mu <- varobj$hyperp
    prior <- varobj$prior
    ncoef <- nrow(varobj$Bhat)
    X <- varobj$X         # rhs variables for the model
    Y <- varobj$Y         # lhs variables for the model
    H0 <- varobj$H0       # precision for the var coefs
    S0 <- varobj$S0       # precision for the Sigma
    nu <- varobj$prior[8] # df
    
    # Get some constants we are going to need from the inputs
    starttime<-date()           # Starting time for simulation
    # of forecasts
    
    q<-nrow(as.matrix(yconst));   # Number of restrictions
    
    p<-dim(ar.coefs)[3]         # Capture the number of lags
    # from input ar coefficients
    
    m<-ncol(y);                 # Number of endogenous
    # variables in the VAR
    
    capT<-nrow(y)               # Number of observations we
    # are going to use.
    
    k<-m*nsteps;                # k = mh, the maximal number
    
    q <- nrow(yconst)             # Number of constraints
    
    # Make arrays to hold the Gibbs sampler results
    yforc<-array(0,c(gibbs,nsteps,m))
    
    
    # Do the Gibbs draws.....
    for(i in 1:(burnin+gibbs))
    {
      # Step (a): Compute a draw of the conditional forecasts
      # COMPUTE INNOVATIONS: These are the structural innovations
      # Solve the constraint equation for the updated forecast errors
      
      # Generate the forecasts without shocks
      ytmp<-as.matrix(coef.forecast.VAR(y, intercept, ar.coefs, exog.coefs, m, p,
                                        capT, nsteps, A0=(A0)))
      
      # Get the impulse responses that correspond to the forecasted data
      M <- irf.VAR(varobj, nsteps, A0)$mhat
      
      # Construct the draw of the orthogonalized innovations that
      # satisfy the hard condition.
      
      # These are the q constrained innovations
      r <- (yconst - ytmp[(capT+1):(capT+nsteps),])
      r<-matrix(r[1:nsteps,1],ncol=1)
      
      # Build the matrix of the impulses that define the constraint
      R <- matrix(0, k, q)
      
      # Put the g'th column of the impulse into the constraint matrix,
      # such that R * epsilon = r
      for (g in 1:q)
      {
        if(g==1)
        { R[1:length(M[,1,1]), 1] <- M[,1,1] }
        else
        {
          R[,g] <- c(M[,1,g], R[,g-1])[1:k]
        }
      }
      
      # Solve the minimization problem for the mean and variance of
      # the constrained innovations.
      
      RRinv<-solve(crossprod(R))
      mean.epsilon <- R%*%RRinv%*%r;
      var.epsilon <- diag(1,nrow=k) - (R%*%RRinv%*%t(R));
      
      # Draw from the singular MVN pdf of the constrained innovations.
      
      epsilon.i <- matrix(rmultnorm(1, mean.epsilon, var.epsilon),
                          nrow=nsteps, ncol=m, byrow=T)
      
      # Add the innovations to the forecasts
      ytmp[(capT+1):(capT +nsteps),] <- ytmp[(capT+1):(capT +nsteps),]+epsilon.i%*%A0
      
      # Store forecasts that are past the burnin point
      if(i>burnin)
      { for(j in 1:m)
      { yforc[(i-burnin),(1:nsteps),j]<-ytmp[(capT+1):(capT+nsteps),j] }
      }
      
      
      # Step (b): Compute the mode of the posterior for the
      # conditional forecast distribution.  This is the "extended"
      # dataset that includes the i'th Gibbs sample forecast
      
      
      # Build the augmented LHS and RHS matrices
      # 1) Get the nsteps+p observations we need to build the lagged
      # endogenous variables for the augmented system.
      
      Y.update <- ytmp[(capT-p+1):nrow(ytmp),]
      
      # 2) Build the updated X -- this is hard because we need to get
      # the RHS lags correct.  We do this by padding the existing Y
      # and then building the lags.  This reuses the code for the lag
      # construction in the szbvar code
      
      X.update <- matrix(0, nsteps, ncoef)
      X.update[,ncoef] <- matrix(1, nsteps, 1)
      
      # Note that constant is put last here when the lags are made
      for(j in 1:p)
      {
        X.update[1:nsteps,(m*(j-1)+1):(m*j)] <- matrix(Y.update[(p+1-j):(nsteps+p-j),],ncol=m)
      }
      
      # Put on the exogenous regressors and make the constant the
      # first exog regressor after the AR coefs
      if(is.null(exog)==F)
      {
        X.update<-cbind(X.update,exog);
      }
      
      # Now, stack the original Y data and the augmented data.
      Y.update <- rbind(Y, ytmp[(capT+1):nrow(ytmp),])
      
      # Set up crossproducts and inverses we need
      X.update <- rbind(X, X.update)
      XX.update <- crossprod(X.update)     # Cross product of RHS variables
      hstar.update <- H0 + XX.update       # Prior + Cross product
      
      # Updated Regression estimates, Beta | Sigma
      B.update<-solve((hstar.update),(crossprod(X.update,Y.update) + H0[,1:m]))
      
      # Posterior mean of Sigma | Beta
      S.update <- (S0 + crossprod(Y.update)
                   + H0[1:m,1:m] - t(B.update)%*%(hstar.update)%*%(B.update))/(capT+nsteps+nu-m-1)
      
      # Posterior variance of B: VBh = diag(Sh.*.(inv((H0 + x'x)))
      hstarinv <- solve(hstar.update)
      vcv.Bh <- kronecker(S.update,hstarinv)
      
      # Draw from the conditional posterior pdfs of the parameters
      
      # This is only valid for just-identified models.
      df <- capT - m*p - m - 1 + nsteps
      wisharts <- rwishart(1, df, diag(m))
      
      # Generate the draws from the Wishart and the Beta
      # Wishart draw
      Sigmat <- (chol(S.update))
      Sigma.Draw <- t(Sigmat)%*%(df*solve(matrix(wisharts,m,m)))%*%Sigmat
      sqrtwish <- t(chol(Sigma.Draw))
      # Covariance of beta
      bcoefs.covar <- t(chol(vcv.Bh))
      
      # Draw of beta|Sigma ~ MVN(B.update, S.Update .*. Hstarinv)
      aplus <- matrix(B.update, ncol=1) +
        bcoefs.covar%*%matrix(rnorm(nrow(bcoefs.covar)), ncol=1)
      
      # Reshape and extract the coefs
      aplus <- matrix(aplus, ncol=m)
      aplus.coefs<-t(aplus[1:(m*p),]);          # extract the ar coefficients
      dim(aplus.coefs)<-c(m,m,p)                # push ar coefs into M x M x P array
      aplus.coefs<-aperm(aplus.coefs,c(2,1,3))  # reorder array
      
      intercept <- aplus[(m*p+1),]      # get drawn intercept....
      ar.coefs<-aplus.coefs             # AR coefs
      A0 <- sqrtwish
      
      #      exog.coefs <- aplus[(m*p+2):nrow(aplus),]
      
      # Need to add something here to deal with the exogenous
      # regressors!
      
      
      # Print some intermediate results to capture progress....
      # and tell us that things are still running
      if (i%%1000==0)
      { cat("Gibbs Iteration = ", i, "     \n");
        if(i<=burnin)
        { cat("(Still a burn-in draw.)\n");
        }
      }
      # Back to the top of the Gibbs loop....
    }
    endtime<-date()
    # Print time stamp so we know how long everything took.
    cat("Start time : ", starttime, "\n");
    cat("End time   : ", endtime, "\n");
    # Returns a list object
    output <- list(forecast=yforc, orig.y=y) #llf=ts(llf),hyperp=c(mu,prior)))
    attr(output, "class") <- c("forecast.VAR")
    return(output)
  }


# Forecasting function for MSBVAR models.
#
# 20110113 : Initial version
# 20110628 : Clean out remaining bugs on DF corrections for
#            observations
# 20120207 : Updated to work with revisions to MSBVAR

###### MSBVARfcast #####
# Forecast for one draw -- this is an internal function that only
# returns the forecasts averaged over the regimes.
#
# Inputs :
# y = data modeled + forecast steps
# k = number of forecast steps
# h = number of regimesq
# ar.coefs = array of c(m,m,p,h) for the MSBVAR
# intercepts = array of c(m,h) for the intercepts
# Sigma = array of c(m.m,h) for the variances
# shock = array of c(k,m,h) for the shocks
# ss = matrix of c(k,h) for the regimes

MSBVARfcast <- function(y, k, h, ar.coefs, intercepts, shocks, ss)
{
  # Get constants
  dims <- dim(ar.coefs)
  m <- dims[1]
  p <- dims[3]
  capT <- nrow(y)
  
  # Set up the object to hold the forecasts
  # Zero out forecast periods so we can cumulate sums over time,
  # lags, and regimes to get the correct weighted forecast.
  
  forcs <- rbind(y, matrix(0, k, m))
  
  for(j in 1:k)     # Loop over forecast steps
  {
    for(i in 1:h) # Now over the regime weights, multiplying by
      # the regime weights and aggregating
    {
      forcs[capT + j,] <- forcs[capT+j,] +
        ss[(capT+j),i]*(forcs[capT + j - 1,] %*%
                          ar.coefs[,,1,i] + intercepts[,i] + shocks[j,,i])
      # Now add in the other lags -- be sure they are from the
      # right state!
      if(p>1) { for(lg in 2:p)
      { forcs[capT + j, ] <- (forcs[capT + j, ] +
                                ss[(capT+j-lg),i]*(forcs[capT + j - lg, ] %*% ar.coefs[,,lg,i]))
      }
      }
    }
  }
  
  return(forcs[(capT+1):(capT+k),])
}

# Betai2coefs
# Converts the draw Betai into arrays of AR coefficents for each
# regime and the intercepts
Betai2coefs <- function(Betai, m, p, h)
{
  ar.coefsi <- array(0, c(m,m,p,h))
  for(i in 1:h)
  {
    tmp <- t(Betai[1:(m*p),,i])
    dim(tmp) <- c(m,m,p)
    ar.coefsi[,,,i] <- aperm(tmp, c(2,1,3))  # lags then regimes.
    #        ar.coefs[,,,i] <- tmp
  }
  intercepts <- Betai[(m*p+1),,]
  return(list(ar.coefsi=ar.coefsi, intercepts=intercepts))
}


updateinit <- function(Y, init.model)
{
  n<-nrow(Y);	 	# of observations in data set
  m<-ncol(Y)	# of variables in data set
  p <- init.model$p
  
  # Compute the number of coefficients
  ncoef<-(m*p)+1;  # AR coefficients plus intercept in each RF equation
  ndum<-m+1;                # of dummy observations
  capT<-n-p+ndum;   	      # # of observations used in mixed estimation (Was T)
  Ts<-n-p;  		      # # of actual data observations @
  
  # Declare the endoegnous variables as a matrix.
  dat<-as.matrix(Y);
  
  # Create data matrix including dummy observations
  # X: Tx(m*p+1)
  # Y: Txm, ndum=m+1 prior dummy obs (sums of coeffs and coint).
  # mean of the first p data values = initial conditions
  if(p==1)
  { datint <- as.vector(dat[1,]) }
  else
  { datint<-as.vector(apply(dat[1:p,],2,mean)) }
  
  # Y and X matrices with m+1 initial dummy observations
  X<-matrix(0, nrow=capT, ncol=ncoef)
  Y<-matrix(0, nrow=capT, ncol=m)
  const<-matrix(1, nrow=capT)
  const[1:m,]<-0;	         # no constant for the first m periods
  X[,ncoef]<-const;
  
  # Build the dummy observations we need
  for(i in 1:m)
  {
    Y[ndum,i]<-datint[i];
    Y[i,i]<-datint[i];
    for(j in 1:p)
    { X[ndum,m*(j-1)+i]<-datint[i];
    X[i,m*(j-1)+i]<-datint[i];
    }
  }
  
  # Note that constant is put last here when the lags are made
  for(i in 1:p)
  { X[(ndum+1):capT,(m*(i-1)+1):(m*i)]<-matrix(dat[(p+1-i):(n-i),],ncol=m)
  }
  
  # Put on the exogenous regressors and make the constant the
  # first exog regressor after the AR coefs
  ## if(is.null(z)==F)
  ##   {
  ##     pad.z <- matrix(0,nrow=capT,ncol=ncol(z))
  ##     pad.z[(ndum+1):capT,] <- matrix(z[(p+1):n,], ncol=ncol(z))
  ##     X<-cbind(X,pad.z);
  ##   }
  
  # Get the corresponding values of Y
  Y[(ndum+1):capT,]<-matrix(dat[(p+1):n,],ncol=m);
  
  # Weight dummy observations
  X[1:m,] <- init.model$prior[6]*X[1:m,];
  Y[1:m,] <- init.model$prior[6]*Y[1:m,];
  X[ndum,] <- init.model$prior[7]*X[ndum,];
  Y[ndum,] <- init.model$prior[7]*Y[ndum,];
  
  init.model$X <- X
  init.model$Y <- Y
  init.model$hstar <- (init.model$H0 + crossprod(X))
  
  return(init.model)
}


# This is an amalgam of uc.forecast and gibbs.msbvar.  In this
# version, the same steps as gibbs.msbvar are used, but with the
# updated data from the forecast step.

# x = posterior mode object -- can either be from gibbs.msbvar or
#     msbvar.  Need to handle identification steps or permute as
#     appropriate.  Should warn against permuting!
# k = number of forecast steps.
#
# -----------------------------------------------------------------#
################## Steps in MSBVAR forecasting #####################
# -----------------------------------------------------------------#
# A) Augment the dataset from the mode -- forecasting
# B) Update the input data
# C) For given parameters,
#    1) draw statespace
#    2) update / draw Q
#    3) update regression -- need to remake the input matrices as part
#       of this.
#    4) sample variances
#    5) sample regression
#    6) update errors
#    7) Impose identification on the regimes (if necessary)
# top of loop
# -----------------------------------------------------------------#
#
# Initial version assumes user is inputing an identified object from
# gibbs.msbvar().  Need to add a warning when they using an msbvar()
#     object and reconcile this.
#
forecast.MSBVAR <- function(x, k, N1=1000, N2=1000)
{
  # Get some constants / objects
  init.model <- x$init.model  # initial model with the prior
  # matrices.
  
  # Get other constants from the input object
  h <- x$h
  m <- x$m
  p <- x$p
  
  # Set the sampler method for Q based on inputs
  if(attr(x, "Qsampler")=="Gibbs") Qsampler <- Q.drawGibbs
  if(attr(x, "Qsampler")=="MH") Qsampler <- Q.drawMH
  
  # Input data
  y <- init.model$y
  
  # Get sample size of original dataset and the effective sample
  TT <- capT <- nrow(init.model$y)
  
  # Settle how we are going to handle input sample size versus the
  # other sample sizes for the inputs / outputs.  Do this the same
  # way as the earlier unconditional forecasting code.  In this
  # code, we treated capT as the full original sample size minus the
  # lags to make it match the estimator. We then
  # manipulate from that.  We do this by always forecasting off of
  # the original data from capT+1 to capT+k,  This is the principal
  # in uc.forecast() and forecast.VAR and coef.forecast.VAR() [in
  # hidden.R].  The difference here is that tbe value of capT has to
  # be adjusted compared to what is in the other code.
  #
  # Principal in this is just working with data augmentation off of
  # the original data setup.
  
  Tp <- TT-p # effective sample
  
  alpha.prior <- x$alpha.prior
  
  # Get the modes for the posterior
  Q <- matrix(apply(x$Q.sample, 2, mean), h, h)
  Betai <- array(apply(x$Beta.sample, 2, mean), c(m*p+1, m, h))
  tmp <- Betai2coefs(Betai, m, p, h)
  ar.coefsi <- tmp$ar.coefsi
  intercepts <- tmp$intercepts
  
  Sigmai <- array(apply(matrix(apply(x$Sigma.sample, 2, mean),
                               m*(m+1)/2, h),
                        2, xpnd), c(m,m,h))
  intercepts <- Betai[(m*p+1),,]
  
  # Extend the dataset / input data
  # Residuals and regression
  hreg <- hregime.reg2(h, m, p, mean.SS(x), init.model)
  e <- array(0, c(Tp+k, m, h))
  
  # Now take random draws from the error process for each regime to
  # pad out initial values for the forecast periods.
  Sigmachol <- aperm(array(apply(Sigmai, 3, chol), c(m,m,h)),
                     c(2,1,3))
  
  for(i in 1:h)
  {
    etmp <-  t(Sigmachol[,,i]%*%matrix(rnorm(k*m), m, k))
    e[,,i] <- rbind(hreg$e[,,i], etmp)
  }
  
  # State-space initialization -- flat convolution of the last
  # state!
  
  tmpSS <- mean.SS(x)
  sstmp <- rbind(mean.SS(x), matrix(rep(NA, k*h), k, h))
  
  for(i in 1:k)
  {
    tmp <- sstmp[Tp+i-1,]%*%Q
    sstmp[Tp+i,] <- tmp/sum(tmp)
    
  }
  
  transtmp <- count.transitions(sstmp)
  ss <- list(SS=sstmp, transitions=transtmp)
  
  # Make list objects for Beta and Sigma
  Betai <- list(Betai=Betai)
  Sigmai <- list(Sigmai=Sigmai)
  
  # Storage
  forecasts <- array(0, c(N2, k, m))
  states <- vector("list", N2)
  
  # Burnin loop + Final loop
  for (j in 1:(N1+N2))
  {
    
    # Update the residuals
    shocks <- array(rnorm(m*k*h), c(k, m, h))
    
    Sigmai.chol <- aperm(array(apply(Sigmai$Sigmai, 3, chol), c(m,m,h)),
                         c(2,1,3))
    
    for(i in 1:h)
    {
      #             shocks[,,i] <- Sigmai.chol[,,i]%*%shocks[,,i]
      shocks[,,i] <- t(tcrossprod(Sigmai.chol[,,i], shocks[,,i]))
      e[(Tp+1):(Tp+k),,i] <- (shocks[,,i])
    }
    
    
    # Forecast
    
    fcast <- MSBVARfcast(y[(p+1):nrow(y),], k, h, ar.coefsi,
                         intercepts, shocks, ss$SS)
    
    #        print(round(cbind(shocks[,,1], shocks[,,2], fcast), 2))
    
    # Update / Draw statespace
    
    oldtran <- matrix(0,h,h)
    
    while(sum(diag(oldtran)==0)>0)
    {
      ss <- SS.ffbs(e, (Tp+k), m, p, h, Sigmai$Sigmai, Q)
      oldtran <- ss$transitions
    }
    
    #print(ss$transitions)
    ## plot(ts(ss$SS), plot.type="s", col=1:h,
    ##      main=paste("Iteration :", j))
    
    #if(sum(diag(ss$transitions)==0)>0) stop("Oops.")
    
    # Draw Q
    Q <- Qsampler(Q, ss$transitions, prior=alpha.prior, h)
    
    # Update regression
    # Update the model object input with the new data -- this is
    # not very efficient right now, but it works
    
    # Need to re-initialize the init.model object with the latest
    # forecast data so it can be an input to the next sample calls
    #
    # This should be really done with some BVAR setup function
    # that is then used in all of the later models in the pkg --
    # something to do later for speed.
    
    init.model <- updateinit(Y=rbind(y,fcast), init.model)
    
    # Update regression steps
    hreg <- hregime.reg2(h, m, p, ss$SS, init.model)
    
    # Draw variances
    
    #        cat("Iteration :", j, "\n")
    #        print(hreg$Sigmak)
    
    Sigmai <- Sigma.draw(m, h, ss, hreg, Sigmai$Sigmai)
    
    #        print(Sigmai)
    
    # Draw VAR regression coefficients
    Betai <- Beta.draw(m, p, h, Sigmai$Sigmai, hreg, init.model,
                       Betai$Betai)
    
    # Split out AR coefs and intercepts
    tmp <- Betai2coefs(Betai$Betai, m, p, h)
    ar.coefsi <- tmp$ar.coefsi
    intercepts <- tmp$intercepts
    
    # Update error estimate
    e <- residual.update(m, h, init.model, Betai$Betai, e)
    
    # Permute regimes if necessary / order regimes
    
    # Save results if past burnin=N1.
    # Store forecasts and states
    if(j > N1)
    {
      for(i in 1:m)
      {
        forecasts[(j-N1),,i] <- t(fcast[,i])
        states[[(j-N1)]] <-
          as.bit.integer(as.integer(ss$SS[,1:(h-1)]))
      }
    }
    
    # Print out iteration information
    if(j%%1000==0) cat("Iteration : ", j, "\n")
  }
  
  # Define the output object and its attributes
  class(forecasts) <- c("forecast.MSBVAR")
  class(states) <- c("SS")
  
  output <- list(forecasts=forecasts, ss.sample=states, k=k, h=h)
  
  # Add classing and attributes here.  These will be use later for
  # the plotting and other summary functions.
  attr(output, "eqnames") <- attr(x, "eqnames")
  tmp <- tsp(x$init.model$y)
  attr(output, "start") <- tmp[1]
  attr(output, "end") <- tmp[2]
  attr(output, "freq") <- tmp[3]
  
  return(output)
}

######### szbsvar.R ########
# Link: https://github.com/lnsongxf/msbvar/blob/master/R/szbsvar.R
# Package detail: msbvar/R/szbsvar.R

"szbsvar" <-
  function(Y, p, z=NULL, lambda0, lambda1, lambda3, lambda4, lambda5,
           mu5, mu6, ident, qm=4){
    
    sanity.check.bsvar(list(Y=Y, p=p, z=z, lambda0=lambda0,
                            lambda1=lambda1, lambda3=lambda3,
                            lambda4=lambda4, lambda5=lambda5,
                            mu5=mu5, mu6=mu6, qm=qm, ident=ident))
    
    # Set up some constants we need
    m <- ncol(Y)                                  # number of endog variables
    nexog <- ifelse(is.null(z)==TRUE, 0, ncol(z)) # number of exog variables
    ncoef <- m*p + nexog + 1                      # plus 1 for the constant
    n<-nrow(Y);	 	                    # observations
    
    if(dim(ident)[1]!=m)
    {
      stop("Identification matrix 'ident' and dimension of 'Y' are nonconformable.")
    }
    # Compute the number of coefficients
    endog.ncoef<-(m*p)+1;  # AR coefficients plus intercept in each RF equation
    ndum<-m+1;             # of dummy observations
    capT<-n-p+ndum;        # degrees of freedom for the mixed estimation
    Ts<-n-p;  	     # # of actual data observations
    
    # Do some error checking of the identification scheme.
    # NEED to put this into the model.....
    
    # Define the linear restrictions from the identification matrix
    # for the free parameters
    
    # set up the linear restrictions for the parameters based on the A0
    # identification in ident
    Q <- array(0, c(m,m,m))
    for(i in 1:m)
    { Q[,,i] <- diag(ident[,i])
    }
    
    # Find the orthonormal bases for each of the Q matrices.  Need
    # thse because they define the null space for the squeezing the
    # parameters. This is just the null space for each matrix in Q.
    
    Ui <- sapply(1:m, function(i){null.space(Q[,,i])}, simplify=F)
    
    # Set up the prior
    
    # Prior mean of the regression parameters, Pi, for the ith equation
    Pi <- matrix(0, ncoef, m)
    diag(Pi) <- 1
    
    # Set up the prior for each equation using the resids from a
    # univariate AR model
    
    # Scale factors from univariate OLS
    s2i<-matrix(0,nrow=m,ncol=1)
    for(i in 1:m)
    {
      s2i[i,1] <- ar.ols(Y[,i], aic=FALSE,order.max=p,
                         intercept=TRUE,demean=FALSE)$var.pred
    }
    
    # Prior scale for A0 -- Si in the Waggoner and Zha notation
    S0 <- diag(m)
    diag(S0)<- 1/s2i
    
    # Prior for A+ or F coefficients -- same for all equations for now
    
    # Lag decays
    # create monthly lag decay to match 1/k decay in quarterly
    #  data where k = quarters
    if(qm==12)
    { j<-ceiling(p/3)^-lambda3;   	# last quarter (rounded up) eg. l2=13=>xx2=5
    b<-0
    if(p > 1)
    { b<-(log(1)-log(j))/(1-p) }
    a<-exp(-b);
    }
    
    # Find the lag decays for the variances over the p lags
    Aplus.prior.cov <- matrix(0, ncoef, 1)
    
    for(i in 1:p)
    { if(qm==12)
    { ld <- a*exp(b*i*lambda3) }
      for(j in 1:m)
      { if(qm==12)
      { Aplus.prior.cov[((i-1)*m+j),1] <- ld^2/s2i[j,1]
      } else {
        Aplus.prior.cov[((i-1)*m+j),1] <- (1/i^lambda3)^2/s2i[j,1]
      }
      }
    }
    
    # Find the prior for A0 conditional prior variances
    A0.prior <- lambda0^2/s2i           # sg0bida
    Aplus.prior <- lambda0^2*lambda1^2*Aplus.prior.cov #sgpbida
    Aplus.prior[(m*p+1),1] <- (lambda0*lambda4)^2  # prior for intercept
    
    if(nexog>0)
    { Aplus.prior[(m*p+2):ncoef,1] <- lambda0^2*lambda5^2  # prior for eexog
    }
    
    Aplus.prior1 <- Aplus.prior[(m+1):ncoef,1] # sgppbd
    
    # Now compute the H matrices
    Hptd <- diag(as.vector(Aplus.prior))
    Hptdi <- diag(as.vector(1/Aplus.prior))
    
    # Now find the final covariance matrices for each of the i=1,..,m
    # equations.
    H0multi <- array(0, c(m, m, m))
    H0invmulti <- H0multi
    Hpmulti <- array(0, c(ncoef,ncoef,m))
    Hpmultiinv <- Hpmulti
    
    # This can be modified if we want to implement an asymmetric prior
    # across the columns (equations).
    H0td <- matrix(0, m, m)
    H0tdi <- H0td
    for (i in 1:m)
    { # A0 parts
      A0i <- A0.prior
      A0i.inv <- 1/A0i
      diag(H0td) <- A0i
      diag(H0tdi) <- 1/A0i
      H0multi[,,i] <- H0td
      H0invmulti[,,i] <- H0tdi
      
      # A+ parts
      Hpmulti[,,i] <- Hptd
      Hpmultiinv[,,i] <- Hptdi
    }
    
    # Now combine the prior with the linear restrictions --- maps from
    # a(i) and f(i) vectors into the b(i) and g(i) vectors of the
    # restricted model.  This is where we map from q(a_i, f_i) to
    # q(a_i, f_i | Q a_i = 0 and R f_i = 0)
    
    # This is where we make the Ptilde, H0tilde and Hptilde matrices for
    # the restricted system.
    
    Hpinv.tilde <- Hpmultiinv
    
    # Use sapply, since we do not know the size of the null spaces, so the
    # result needs to be dynamically sized
    
    Pi.tilde <- sapply(1:m, function(i){Pi%*%Ui[[i]]}, simplify=F)
    H0inv.tilde <- sapply(1:m, function(i)
    {t(Ui[[i]])%*%H0invmulti[,,i]%*%Ui[[i]]}, simplify=F)
    
    # Set up the data
    # 1) Set up matrices of data and the moment matrices for the data
    
    # Test for the exogenous variables and check rank
    if (is.null(z))
    { num.exog <- 0 } else {
      num.exog <- ncol(z)
      z <- as.matrix(z)
      if(det(crossprod(cbind(rep(1,nrow(z)),z)))<=0)
      {
        stop("Matrix of exogenous variables, z has deficient rank.")
      }
    }
    
    # Create data matrix including dummy observations
    # X: Tx(m*p+1)
    # Y: Txm, ndum=m+1 prior dummy obs (sums of coeffs and coint).
    # mean of the first p data values = initial conditions
    if(p==1)
    { datint <- as.vector(Y[1,])
    } else {
      datint<-as.vector(apply(Y[1:p,],2,mean)) }
    
    # Y and X matrices with m+1 initial dummy observations
    X1<-matrix(0, nrow=capT, ncol=endog.ncoef)
    Y1<-matrix(0, nrow=capT, ncol=m)
    const<-matrix(1, nrow=capT)
    const[1:m,]<-0;	         # no constant for the first m periods
    X1[,endog.ncoef]<-const;
    
    # Build the dummy observations we need
    for(i in 1:m)
    {
      Y1[ndum,i]<-datint[i];
      Y1[i,i]<-datint[i];
      for(j in 1:p)
      { X1[ndum,m*(j-1)+i]<-datint[i];
      X1[i,m*(j-1)+i]<-datint[i];
      }
    }
    
    # Make the lags.  Note that constant is put last here when the lags are made
    for(i in 1:p)
    { X1[(ndum+1):capT,(m*(i-1)+1):(m*i)]<-matrix(Y[(p+1-i):(n-i),],ncol=m)
    }
    
    # Put on the exogenous regressors and make the constant the
    # first exog regressor after the AR coefs
    if(is.null(z)==F)
    {
      pad.z <- matrix(0,nrow=capT,ncol=ncol(z))
      pad.z[(ndum+1):capT,] <- matrix(z[(p+1):n,], ncol=ncol(z))
      X1<-cbind(X1,pad.z);
    }
    
    # 2) Dummy observations
    
    # Get the corresponding values of Y
    Y1[(ndum+1):capT,]<-matrix(Y[(p+1):n,],ncol=m);
    
    # Weight dummy observations
    X1[1:m,]<-mu5*X1[1:m,];
    Y1[1:m,]<-mu5*Y1[1:m,];
    X1[ndum,]<-mu6*X1[ndum,];
    Y1[ndum,]<-mu6*Y1[ndum,];
    
    # 3) Get moment matrices
    XX <- crossprod(X1)
    XY <- crossprod(X1, Y1)
    YY <- crossprod(Y1)
    
    # Compute the posterior moments -- combine the data and the
    # prior.  These are the posterior moments needed for the results
    # in Waggoner and Zha 2003, JEDC, eqns. 12 and 13 (the H_iu,
    # P_i, S_i below these equations
    
    # H_i^-1 matrices for equations 1...m
    Hpinv.posterior <- sapply(1:m, function(i) { XX + Hpinv.tilde[,,i] },
                              simplify=F)
    
    # Next two build the P_i, the "squeezed" matrices of the SVAR parameters
    P1.posterior <- sapply(1:m,
                           function(i) { XY%*%Ui[[i]] +
                               Hpinv.tilde[,,i]%*%Pi.tilde[[i]] }, simplify=F)
    
    P.posterior <- sapply(1:m, function(i)
    {solve(Hpinv.posterior[[i]])%*%P1.posterior[[i]] },
    simplify=F)
    
    # S_i matrices are next -- these are the SVAR covariances for
    # the columns of A0, a_i.  Note that we do NOT include the
    # scaling by 1/T because this is not necessary for the posterior
    # draws.
    
    H0inv.posterior <- sapply(1:m, function(i)
    { (t(Ui[[i]])%*%YY%*%Ui[[i]] + H0inv.tilde[[i]] +
         t(Pi.tilde[[i]])%*%Hpinv.tilde[,,i]%*%Pi.tilde[[i]] -
         t(P1.posterior[[i]])%*%P.posterior[[i]])
    }, simplify=F)
    
    # Optimize the likelihood to solve for A0 parameters (the b's in
    # the "squeezed" model.  Here we are numerically computing the
    # peak of the PDF -- makes for more efficient draws from the
    # posterior later.
    
    # Find # free parameters in each equation and a vector of the
    # cusum for indexing them
    
    n0 <- sapply(1:m, function(i) {ncol(as.matrix(Ui[[i]]))})
    n0cum <- c(0,cumsum(n0))
    # Generate some random starting values.
    b <- (1/max(s2i))*(rnorm(sum(n0)))
    
    # Optimize the log posterior wrt A0.
    
    # Start with some Nelder-Mead simplex steps to get things
    # started in the right direction.
    cat("Estimating starting values for the numerical optimization\nof the log posterior of A(0)\n")
    
    max.obj <- optim(b, A0.llf, method=c("Nelder-Mead"),
                     control=list(maxit=6000, fnscale=capT, trace=0),
                     Ui=Ui, df=capT, H0inv.posterior=H0inv.posterior)
    
    # Do the final optimization with BFGS.
    cat("Estimating the final values for the numerical optimization\nof the log posterior of A(0)\n")
    
    max.obj <- optim(max.obj$par, A0.llf, method=c("BFGS"), hessian=F,
                     control=list(maxit=5000, fnscale=capT, trace=1),
                     Ui=Ui, df=capT,
                     H0inv.posterior=H0inv.posterior)
    
    # Check for convergence
    if(max.obj$convergence!=0)
    {
      stop("Estiamtes of A(0) did not converge.  You should restart the function with a new seed.")
    }
    
    
    # Build back the A0 from the b's estimated at the peak of the
    # log posterior pdf.
    
    # Estimate of A0 at the peak of the log-posterior.
    A0.mode <- b2a(max.obj$par, Ui)
    
    # Estimates of the SVAR posterior coefs for the lagged and
    # exogenous variables -- the F matrix defined in equations 13
    # and on page 351.
    
    # Build back the A+ and F coefficients from the sub-space of the SVAR
    # back to the unrestricted parameter space
    
    F.posterior <- matrix(0, ncoef, m)
    
    for (i in 1:m)
    { bj <- max.obj$par[(n0cum[i]+1):(n0cum[(i+1)])]
    gj <- P.posterior[[i]]%*%bj
    F.posterior[,i] <- gj
    }
    
    # Now map it all back to reduced form coefficients
    B.posterior <- F.posterior%*%solve(A0.mode)
    
    # Pluck out the arrays of the AR coefficients
    
    AR.coefs.posterior <- t(B.posterior[1:(m*p),])
    dim(AR.coefs.posterior) <- c(m,m,p)
    AR.coefs.posterior <- aperm(AR.coefs.posterior, c(2,1,3))
    
    # compute the structural innovations
    structural.innovations <- Y1%*%A0.mode - X1%*%F.posterior
    
    # reduced form exogenous coefficients
    if(nexog==0){
      exog.coefs <- NA
    } else {
      exog.coefs <- B.posterior[((m*p)+2):nrow(B.posterior),]
    }
    
    # Now build an output list / object for the B-SVAR model
    output <- list(XX=XX,                               # data matrix moments with dummy obs
                   XY=XY,
                   YY=YY,
                   y=Y,
                   Y=Y1,
                   X=X1,
                   structural.innovations=structural.innovations,
                   Ui=Ui,                                         # restriction transformation
                   Hpinv.tilde=Hpinv.tilde,    # Prior moments
                   H0inv.tilde=H0inv.tilde,
                   Pi.tilde=Pi.tilde,
                   Hpinv.posterior=Hpinv.posterior,
                   P.posterior=P.posterior,
                   H0inv.posterior=H0inv.posterior,
                   A0.mode=A0.mode,
                   F.posterior=F.posterior,
                   B.posterior=B.posterior,
                   ar.coefs=AR.coefs.posterior,
                   intercept=B.posterior[(m*p+1),],
                   exog.coefs=exog.coefs,
                   prior=c(lambda0,lambda1,lambda3,lambda4,lambda5,mu5,mu6),
                   df=capT,
                   n0=n0,
                   ident=ident,
                   b=max.obj$par,
                   p=p
    )
    class(output) <- c("BSVAR")
    attr(output, "eqnames") <- colnames(Y) # Get variable names for
    # attr
    
    return(output)
  }


# Summary function for BSVAR models
"summary.BSVAR" <- function(object, ...)
{
  p <- object$p
  
  cat("------------------------------------------\n")
  cat("A0 restriction matrix\n")
  cat("------------------------------------------\n")
  prmatrix(object$ident)
  cat("\n")
  
  cat("------------------------------------------\n")
  cat("Sims-Zha Prior Bayesian Structural VAR\n")
  cat("------------------------------------------\n")
  ##     if(object$prior.type==0) prior.text <- "Normal-inverse Wishart"
  ##     if(object$prior.type==1) prior.text <- "Normal-flat"
  ##     if(object$prior.type==2) prior.text <- "Flat-flat"
  
  cat("Prior form : Sims-Zha\n")
  cat("Prior hyperparameters : \n")
  cat("lambda0 =", object$prior[1], "\n")
  cat("lambda1 =", object$prior[2], "\n")
  cat("lambda3 =", object$prior[3], "\n")
  cat("lambda4 =", object$prior[4], "\n")
  cat("lambda5 =", object$prior[5], "\n")
  cat("mu5     =", object$prior[6], "\n")
  cat("mu6     =", object$prior[7], "\n")
  cat("nu      =", dim(object$ar.coefs)[1]+1, "\n")
  
  cat("------------------------------------------\n")
  cat("Number of observations : ", nrow(object$Y), "\n")
  cat("Degrees of freedom per equation : ", nrow(object$Y)-nrow(object$Bhat), "\n")
  cat("------------------------------------------\n")
  
  cat("Posterior Regression Coefficients :\n")
  cat("------------------------------------------\n")
  cat("Reduced Form Autoregressive matrices: \n")
  for (i in 1:dim(object$ar.coefs)[3])
  {
    cat("B(", i, ")\n", sep="")
    prmatrix(round(object$ar.coefs[,,i], 6))
    cat("\n")
  }
  cat("------------------------------------------\n")
  cat("Reduced Form Constants\n")
  cat(round(object$intercept,6), "\n")
  cat("------------------------------------------\n")
  
  if(nrow(object$B.posterior)>m*p + 1)
  {
    cat("------------------------------------------\n")
    cat("Reduced Form Exogenous coefficients\n")
    prmatrix(object$B.posterior[(m*p+2):nrow(object$B.posterior),])
    cat("\n")
    cat("------------------------------------------\n")
  }
  
  # Now print the structural coefficients in the same way as the
  # RFs.
  cat("Structural Autoregressive matrices: \n")
  m <- dim(object$ar.coefs)[1]
  struct.ar <- object$F.posterior[1:(m*p),]
  dim(struct.ar) <- c(m, m, p)
  struct.ar <- aperm(struct.ar, c(2,1,3))
  for (i in 1:p)
  {
    cat("A(", i, ")\n", sep="")
    prmatrix(round(struct.ar[,,i], 6))
    cat("\n")
  }
  cat("------------------------------------------\n")
  cat("Structural Constants\n")
  cat(round(object$F.posterior[(m*p +1),],6), "\n")
  cat("------------------------------------------\n")
  
  if(nrow(object$B.posterior)>m*p + 1)
  {
    cat("------------------------------------------\n")
    cat("Structural Exogenous coefficients\n")
    prmatrix(object$F.posterior[(m*p+2):nrow(object$F.posterior),])
    cat("\n")
    cat("------------------------------------------\n")
  }
  
  
  cat("------------------------------------------\n")
  cat("Posterior mode of the A0 matrix\n")
  prmatrix(round(object$A0.mode,6))
  cat("\n")
  cat("------------------------------------------\n")
  
}


######### msbvar.R ########
# msbvar() and related functions
# Patrick T. Brandt
# 20081113 : Initial version
# 20120110 : Updated to use new block optimization function by Ryan
#            Davis.  This implements the SWZ block algorithm to get
#            the MLE of the MSVAR and then sets this up for Gibbs
#            sampler.
# 20120112 : Updated to setup some smart starting values and allow the
#            users to input them.  Some error checking is included as
#            well.  Also users multiple random and user input methods
#            to generate starting values for MCMC

# Workhorse msbvar() function with SZ prior to initialize the posterior
# of the Gibbs sampler for the MSBVAR models.

msbvar <- function(Y, z=NULL, p, h,
                   lambda0, lambda1, lambda3,
                   lambda4, lambda5, mu5, mu6, qm,
                   alpha.prior=100*diag(h) + matrix(2, h, h),
                   prior=0, max.iter=40, initialize.opt=NULL)
{
  # Get the number of equations
  m <- ncol(Y)
  
  # This should be part of a sanity.check.msbvar
  if(h==1)
  {
    stop("\n\n\t -- For MSBVAR models, h>1.  Otherwise, just for a BVAR or VAR!\n")
  }
  
  # Check the dimensions of alpha.prior
  chk <- dim(alpha.prior)
  if( chk[1]!=h) stop("Incorrect number of rows in alpha.prior.")
  if( chk[2]!=h) stop("Incorrect number of columns in alpha.prior.")
  
  ########################################################
  # Before the loop this is all initialization for the EM
  # implementation of the Bayesian model.
  ########################################################
  
  # As default, do a baseline, non-regime model using szbvar() since
  # this gives us all of the inputs we need for later.  User can
  # input their own function / object to do something different as
  # long as it conforms.  See docs for details.
  
  if(is.null(initialize.opt)==TRUE)  # User provided nothing
  {
    # Inherits all from inputs.
    setup <- initialize.msbvar(Y, p, z, lambda0, lambda1,
                               lambda3, lambda4, lambda5, mu5,
                               mu6, nu=m, qm, prior, h,
                               Q=NULL)
    
    init.model <- setup$init.model
    Qhat.start <- setup$Qhat.start
    thetahat.start <- setup$thetahat.start
  } else {
    # This is the user input one, so we need to validate it
    if(inherits(initialize.opt$init.model, "BVAR")==FALSE)
    {
      stop("msbvar() initialize.opt list must have an object named init.model of class BVAR.  Create this using szbvar()\n")
    }
    
    # Check dim of thetahat.start
    tmp <- dim(initialize.opt$thetahat.start)
    if(tmp[1]!=m)
    {
      stop("initialize.opt$thetahat.start has the wrong number of rows\n")
    }
    if(tmp[2]!=(1+m*p+m))
    {
      stop("initialize.opt$thetahat.start has the wrong number of columns\n")
    }
    if(tmp[3]!=h)
    {
      stop("initialize.opt$thetahat.start has the wrong array dimension\n")
    }
    
    # Validate an initial Q from the user
    
    if(sum(initialize.opt$Qhat.start)!=h)
    {
      stop("msbvar(): Improper initial Q, transition matrix, for the MS process.  Rows must sum to 1.\n")
    }
    
    # If we get to here, the input object is valid.  So assign for
    # use
    init.model <- initialize.opt$init.model
    Qhat.start <- initialize.opt$Qhat.start
    thetahat.start <- initialize.opt$thetahat.start
    
    # Check dim of alpha.prior
    tmp <- dim(alpha.prior)
    if(tmp[1]!=h)
    { stop("msbvar(): Incorrect number of rows in alpha.prior") }
    
    if(tmp[2]!=h)
    { stop("msbvar(): Incorrect number of columns in alpha.prior")
    }
    
  }  # End validations of user inputs
  
  #################################################################
  # EM-blkopt algorithm -- all handled in the Fortran code
  #################################################################
  
  indms <- 'IAH'   # Right now, user does not have choice over what
  # switches.  Allow I = Intercepts, A = AR, H =
  # Variances.
  
  mlemod <- blkopt(Y, p, thetahat.start, Qhat.start, niter=max.iter,
                   indms)
  
  # Do setup of the object we want to feed into the posterior
  # sampler, gibbs.msbvar().  This is basically a final pass at
  # setting up the moment matrices for the regressions with the
  # appropriate priors.
  
  hreg <- setupGibbs(mlemod, Y, init.model)
  
  # Now re-assign the objects and set up the output
  
  output <- list(init.model=init.model,
                 hreg=hreg,
                 Q=mlemod$Qhat,
                 fp=mlemod$fpH,
                 m=m, p=p, h=h,
                 alpha.prior=alpha.prior)
  
  class(output) <- c("MSVARsetup")
  attr(output, "eqnames") <- colnames(Y) # Get variable names for
  # attr
  return(output)
}

############################################################
# blkopt() -- the block optimizer for MSBVAR and MS models
############################################################

blkopt <- function(Y, p, thetahat.start, Qhat.start, niter=10, indms) {
  
  m <- ncol(Y)
  n <- nrow(Y) # later, definition of n will change
  h <- nrow(Qhat.start)
  
  ##################################################
  ### Setup Regression Matrices
  ##################################################
  
  # setup LHS Y as Yregmat (Y regression matrix)
  Yregmat <- matrix(Y[(p+1):n,], ncol=m)
  
  # setup lagged Y's on RHS for AR coefficients, which we define as
  # Xregmat = [Y_t-1, Y_t-2, ..., Y_t-p] (X regression matrix)
  # hence, we will have n-p observations
  Xregmat <- NULL
  if (p>0) { for (i in 1:p) { Xregmat <- cbind(Xregmat, Y[(p-i+1):(n-i),]) } }
  Xregmat <- cbind(rep(1,n-p), Xregmat)  # add intercept on front
  
  
  ##################################################
  ### Initialize
  ##################################################
  
  beta0.it <- array(NA, c(m,1,h))
  beta0.it[,,] <- thetahat.start[,1,]
  
  betap.it <- NULL
  if (p>0) {
    # rows correspond to equation
    # columns ordered by lag order, then each variable
    betap.it <- array(thetahat.start[,2:(1+p*m),], c(m,m*p,h))
  }
  
  sig2.it  <- array(thetahat.start[,(1+m*p+1):ncol(thetahat.start),], c(m,m,h))
  
  theta.it <- array(NA, c(m,1+m*p+m,h))
  theta.it[,1,] <- beta0.it
  if (p>0) theta.it[,2:(1+m*p),] <- betap.it
  theta.it[,(1+m*p+1):ncol(theta.it),] <- sig2.it
  
  Qhat.it  <- Qhat.start
  # Qhat is in last h columns
  
  # add one to store llfval for starting values
  llfval <- rep(0,niter+1)
  
  # run filter to get LLF value for starting values
  
  # First, obtain residuals
  # e[,,i] is an (n-p) x m matrix containing conditional means
  # for regime i, i=1,2,...,h (e's third dimension is regime)
  e <- array(NA, c(n-p, m, h))
  betahat <- array(NA, c(m,1+m*p,h))
  betahat[,1,] <- beta0.it
  if (p>0) betahat[,2:(1+m*p),] <- betap.it
  # adjust for univariate vs. multivariate case
  if (m>1) {
    for (i in 1:h) { e[,,i] <- Yregmat - Xregmat %*% t(betahat[,,i]) }
  } else {
    for (i in 1:h) { e[,,i] <- Yregmat - Xregmat %*% betahat[,,i] }
  }
  
  # Using residuals, get filtered regime probabilities
  # HamFilt <- filter.Hamresid(e, sig2.it, Qhat.it)  # R code
  firstHamFilt <- .Fortran("HamiltonFilter",
                           bigt=as.integer(n),
                           m = as.integer(m), p = as.integer(p),
                           h = as.integer(h),
                           e = e,
                           sig2 = sig2.it,
                           Qhat = Qhat.it,
                           f = double(1),
                           filtprSt =
                             matrix(0,as.integer(n-p),as.integer(h))
  )
  
  llfval[1] <- firstHamFilt$f
  
  cat('Initial Log Likelihood Value:', llfval[1] ,'\n', sep=" ")
  
  ##################################################
  ### Begin Blockwise Optimization
  ##################################################
  
  cat('Starting blockwise optimization over', niter, 'block optimizations...\n')
  
  # blocks: intercept(s), AR coefficient(s), variance/Sigma
  for (iter in 1:niter) {
    # intercepts
    beta0.it <- optim(par=c(beta0.it), fn=llf.msar, Y=Yregmat, X=Xregmat, p=p,
                      theta=theta.it, Q=Qhat.it, optstr='beta0',
                      ms.switch=indms, method="BFGS")$par
    
    if (length(grep('I', indms)) == 0) beta0.it <- array(beta0.it[1:m], c(m,1,h))
    
    theta.it[,1,] <- array(beta0.it, c(m,1,h))
    
    # AR coefficients
    if (p>0) {
      betap.it <- optim(par=c(betap.it), fn=llf.msar, Y=Yregmat,
                        X=Xregmat, p=p,
                        theta=theta.it, Q=Qhat.it, optstr='betap',
                        ms.switch=indms, method="BFGS")$par
      
      # need to check below line...
      if (length(grep('A', indms)) == 0) betap.it <- array(betap.it[1:(m*p)], c(m,m*p,h))
      theta.it[,2:(1+m*p),] <- array(betap.it, c(m,m*p,h))
    }
    
    # variance/Sigma
    if (m==1) {
      # AR (univariate)
      sig2.it <- optim(par=c(sig2.it), fn=llf.msar, Y=Yregmat, X=Xregmat, p=p,
                       theta=theta.it, Q=Qhat.it, optstr='sig2',
                       ms.switch=indms, method="BFGS")$par
      
      if (length(grep('H', indms)) == 0) { sig2.it <- array(sig2.it[1], c(m,m,h)) }  # adjust if variance does not switch
      sig2.it <- array(sig2.it, c(m,m,h))
    } else {
      # VAR, only optimize over distinct elements of Sigma for each regime
      sig2.lower <- sig2.it[,,][lower.tri(sig2.it[,,1], diag=TRUE)]
      sig2.lower <- optim(par=c(sig2.lower), fn=llf.msar, Y=Yregmat,
                          X=Xregmat, p=p,
                          theta=theta.it, Q=Qhat.it, optstr='sig2',
                          ms.switch=indms, method="BFGS")$par
      
      sig2.it  <- array(NA, c(m,m,h))
      # number of distinct: m*(m+1)/2
      nd <- (m*(m+1)/2)
      for (i in 1:h) {
        low <- sig2.lower[(1+(i-1)*nd):(nd+(i-1)*nd)]
        sig2.it[,,i] <- xpnd(low, nrow=m)  # user-defined function below
      }
      if (length(grep('H', indms)) == 0) { sig2.it <- array(sig2.it[,,1], c(m,m,h)) }  # only want first returned value
    }
    theta.it[,(1+m*p+1):ncol(theta.it),] <- sig2.it
    
    # transition matrix
    Qhat.it <- optim(par=c(Qhat.it[,1:(h-1)]), fn=llf.msar, Y=Yregmat,
                     X=Xregmat, p=p,
                     theta=theta.it, Q=Qhat.it, optstr='Qhat',
                     ms.switch=indms, method="BFGS")$par
    
    Qhat.it <- matrix(Qhat.it, nrow=h, ncol=h-1)
    Qhat.it <- cbind(Qhat.it, 1-rowSums(Qhat.it))
    
    # obtain value of likelihood function and store it
    llfval[iter+1] <- -llf.msar(param.opt=beta0.it, Y=Yregmat,
                                X=Xregmat, p=p,
                                theta=theta.it, Q=Qhat.it,
                                optstr='beta0', ms.switch='TOTAL')
    
    cat('Completed iteration ', iter, ' of ', niter,
        '. Log Likelihood Value: ',
        llfval[iter+1], '\n', sep="")
    
  }
  
  # run filter one last time using final parameter estimates
  
  # First, obtain residuals
  # e[,,i] is an (n-p) x m matrix containing conditional means
  # for regime i, i=1,2,...,h (e's third dimension is regime)
  e <- array(NA, c(n-p, m, h))
  betahat <- array(NA, c(m,1+m*p,h))
  betahat[,1,] <- beta0.it
  if (p>0) betahat[,2:(1+m*p),] <- betap.it
  # adjust for univariate vs. multivariate case
  if (m>1) {
    for (i in 1:h) { e[,,i] <- Yregmat - Xregmat %*% t(betahat[,,i]) }
  } else {
    for (i in 1:h) { e[,,i] <- Yregmat - Xregmat %*% betahat[,,i] }
  }
  
  # Using residuals, get filtered regime probabilities
  # HamFilt <- filter.Hamresid(e, sig2.it, Qhat.it)  # R code
  lastHamFilt <- .Fortran("HamiltonFilter",
                          bigt=as.integer(n),
                          m = as.integer(m), p = as.integer(p), h = as.integer(h),
                          e = e,
                          sig2 = sig2.it,
                          Qhat = Qhat.it,
                          f = double(1),
                          filtprSt = matrix(0,as.integer(n-p),as.integer(h))
  )
  
  fpH <- lastHamFilt$filtprSt
  
  output <- list(theta=theta.it,
                 Qhat=Qhat.it,
                 llfval=llfval,
                 fpH=fpH,
                 m=m, p=p, h=h)
  
  class(output) <- c('blkopt')
  
  return(output)
  
}



llf.msar <- function(param.opt, Y, X, p, theta, Q, optstr, ms.switch) {
  
  m <- ncol(Y)
  n <- nrow(Y) + p
  h <- nrow(Q)
  
  # initially assign values from theta
  beta0 <- array(theta[,1,], c(m,1,h))
  betap <- NULL
  if (p > 0) betap <- array(theta[,2:(1+m*p),], c(m,m*p,h))
  sig2  <- array(theta[,(1+m*p+1):ncol(theta),], c(m,m,h))
  Qhat  <- Q
  
  # now choose the parameter over which we are optimizing
  if (optstr=='beta0') {
    beta0 <- array(param.opt, c(m,1,h))
  } else if (optstr=='betap') {
    if (p > 0) betap <- array(param.opt, c(m,m*p,h))
  } else if (optstr=='sig2') {
    sig2  <- array(NA, c(m,m,h))
    # number of distinct: m*(m+1)/2
    nd <- (m*(m+1)/2)
    for (i in 1:h) {
      low <- param.opt[(1+(i-1)*nd):(nd+(i-1)*nd)]
      sig2[,,i] <- xpnd(low, nrow=m)  # user-defined function below
    }
  } else if (optstr=='Qhat') {
    # only passing in first h-1 columns, so add column
    Qhat <- matrix(param.opt, nrow=h, ncol=h-1)
    Qhat <- cbind(Qhat, 1-rowSums(Qhat))
  }
  
  # numerical checks on Q matrix
  # prevents elements in Q from going negative or greater than 1
  # if that occurs during optimization, then just set to previous Q
  if ( (min(Qhat) <= 0.0001) || (max(Qhat) >= 0.9999 )) Qhat <- Q
  
  # numerical checks on sig2
  # prevents elements in sig2 from going negative
  # if that occurs during optimization, then just set to previous sig2
  if ( (min(sig2) <= 0.0001) ) sig2 <- array(theta[,(1+m*p+1):ncol(theta),], c(m,m,h))
  # constrain max(off-diagonal) to be less than min(diagonal)
  blnUseOld <- FALSE
  if (m>1) {
    blnSetPast = 0
    for (i in 1:h) {
      if (max(sig2[,,i][lower.tri(sig2[,,i], diag=FALSE)]) > min(diag(sig2[,,i]))) blnSetPast = 1
    }
    if (blnSetPast == 1) blnUseOld <- TRUE
  }
  if (blnUseOld==TRUE) sig2 <- array(theta[,(1+m*p+1):ncol(theta),], c(m,m,h))
  
  # by default, everything switches, so now adjust by
  # assigning values that do not switch to first state
  # intercept only
  if (ms.switch=='I') {
    if (p > 0) betap <- array(betap[,,1], c(m,m*p,h))
    sig2  <- array(sig2[,,1], c(m,m,h))
  } else if (ms.switch=='H') { # heteroskedastic
    beta0 <- array(beta0[,,1], c(m,1,h))
    if (p > 0) betap <- array(betap[,,1], c(m,m*p,h))
  } else if (ms.switch=='A') {
    beta0 <- array(beta0[,,1], c(m,1,h))
    sig2  <- array(sig2[,,1], c(m,m,h))
  } else if (ms.switch=='IA') { # homoskedastic
    sig2  <- array(sig2[,,1], c(m,m,h))
  } else if (ms.switch=='IH') {
    if (p > 0) betap <- array(betap[,,1], c(m,m*p,h))
  } else if (ms.switch=='AH') {
    beta0 <- array(beta0[,,1], c(m,1,h))
  }
  
  
  #############################################
  # Filtering section
  # Note: there is both Fortran and native R
  #       code in the package (they parallel).
  # Use Fortran for speed, but R for
  # pedagogical purposes.
  #############################################
  
  # First, obtain residuals
  # e[,,i] is an (n-p) x m matrix containing conditional means
  # for regime i, i=1,2,...,h (e's third dimension is regime)
  e <- array(NA, c(n-p, m, h))
  betahat <- array(NA, c(m,1+m*p,h))
  betahat[,1,] <- beta0
  if (p>0) betahat[,2:(1+m*p),] <- betap
  # adjust for univariate vs. multivariate case
  if (m>1) {
    for (i in 1:h) { e[,,i] <- Y - X %*% t(betahat[,,i]) }
  } else {
    for (i in 1:h) { e[,,i] <- Y - X %*% betahat[,,i] }
  }
  
  # Using residuals, get filtered regime probabilities
  # HamFilt <- filter.Hamresid(e, sig2.it, Qhat.it)  # R code
  HamFilt <- .Fortran("HamiltonFilter",
                      bigt=as.integer(n),
                      m = as.integer(m), p = as.integer(p), h = as.integer(h),
                      e = e,
                      sig2 = sig2,
                      Qhat = Qhat,
                      f = double(1),
                      filtprSt = matrix(0,as.integer(n-p),as.integer(h))
  )
  
  f <- HamFilt$f
  
  return(-f) # optim() minimizes negative
  
}




# This needs to be implemented in C // C++ code later
BHLK.smoother <- function(fp, Q)
{
  TT <- nrow(fp)
  h <- ncol(fp)
  p.smooth <- matrix(0, TT, h)
  
  p.smooth[TT,] <- fp[TT,]
  
  for(tt in (TT-1):1)
  {
    p.predict <- Q%*%fp[tt,]
    p.smooth[tt,] <- crossprod(Q, (p.smooth[tt+1,]/p.predict))*fp[tt,]
  }
  
  return(p.smooth)
}

hregime.setup <- function(h, m, p, TT)
{
  
  Bk <- array(0, c(m*p+1, m, h))
  Sigmak <- array(0, c(m,m,h))
  df <- matrix(0, 1, h)
  e <- array(0, c(TT, m, h))
  return(list(Bk=Bk, Sigmak=Sigmak, df=df, e=e))
}


setupGibbs <- function(blkoptobj, Y, init.model)
{
  
  n <- nrow(Y)
  h <- blkoptobj$h
  m <- blkoptobj$m
  p <- blkoptobj$p
  
  # now, setup hreg, adjusting for dummies
  hreg <- hregime.reg2.mle(h, m, p, TT=(n-p), fp=blkoptobj$fpH, init.model)
  
  
  return(hreg)
}

# Ryan adjusted several items from the original hregime.reg2 function
hregime.reg2.mle <- function(h, m, p, TT, fp, init.model)
{
  
  # Storage
  tmp <- vector(mode="list", length=h)
  Bk <- array(0, c(m*p+1, m, h))
  Sigmak <- array(0, c(m,m,h))
  df <- apply(fp, 2, sum)
  e <- array(0, c(TT, m, h))
  Y <- init.model$Y[(m+1+1):nrow(init.model$Y),]
  X <- init.model$X[(m+1+1):nrow(init.model$X),]
  
  # Loops to compute
  # 1) sums of squares for X and Y
  # 2) B(k) matrices
  # 3) Residuals
  # 4) Sigma(k) matrices
  
  for(i in 1:h)
  {
    # Note how the dummy obs. are appended to the moment matrices
    Sxy <- crossprod(X, diag(fp[,i]))%*%Y + crossprod(init.model$X[1:(m+1),], init.model$Y[1:(m+1),])
    Sxx <- crossprod(X, diag(fp[,i]))%*%X + crossprod(init.model$X[1:(m+1),])
    
    # Compute the regression coefficients
    hstar <- Sxx
    Bk[,,i] <- solve(hstar,Sxy,tol=1e-100)
    
    # Compute residuals and Sigma (based on Krolzig)
    
    # Get the full residuals -- need these for filtering
    e[,,i] <- Y - X%*%Bk[,,i]
    
    Sigmak[,,i] <- (crossprod(e[,,i],diag(fp[,i]))%*%e[,,i])/df[i]
    
    # Save the moments
    tmp[[i]] <- list(Sxy=Sxy, Sxx=Sxx) #, ytmp=ytmp, xtmp=xtmp)
  }
  
  return(list(Bk=Bk, Sigmak=Sigmak, df=df, e=e, moment=tmp))
}


###########################################################################
# THIS SHOULD BE RENAMED / RE-DEFINED, SINCE EARLIER VERSIONS ARE IN
# THE SVN NOW
###########################################################################
# hregime.reg2
# h  = number of regimes
# m  = number of equations
# p  = number of lags in the VAR
# fp = filter probability matrix, T x h -- where T matches the sample!
# init.model = initial prior / model object produced by szbvar()
###########################################################################

hregime.reg2 <- function(h, m, p, fp, init.model)
{
  
  # Storage
  tmp <- vector(mode="list", length=h)
  Bk <- array(0, c(m*p+1, m, h))
  Sigmak <- array(0, c(m,m,h))
  df <- colSums(fp)
  e <- array(0, c(nrow(fp)+m+1, m, h))
  
  # New version -- just pad in to get the dummy obs.  Use the full
  # design matrix and require first m+1 dummy obs are in each
  # regime.
  Y <- init.model$Y; X <- init.model$X  # So the dummy obs. are
  # included here, as defined
  # by init.model <- szbvar().
  
  # Pad the filter probabilities so that each regime gets the dummy
  # observations.
  
  fp <- rbind(matrix(1, m+1, h), fp)
  
  # Loop to compute
  # 1) Sums of squares for X and Y for each regime
  # 2) B(k) matrices
  # 3) Residuals
  # 4) Sigma(k) matrices
  
  for(i in 1:h)
  {
    # Note how the dummy obs. are appended to the moment matrices above,
    # so we no longer need to handle these separately as sums
    # inside of each cross-product computation.  See earlier SVN
    # commits to see how this was done (badly) in earlier iterations.
    
    # Compute the cross products we need..
    # Last one here is so we can avoid ((X'X)^{-1} \ccross Sigma)
    # or the need  to do inverses on potentially sample degenerate
    # values in a regime with too few observations to compute a
    # p.d. version of X'X.
    
    # Subset the data for each regime
    X1 <- X[fp[,i]==1,]
    Y1 <- Y[fp[,i]==1,]
    
    Sxy <- crossprod(X1,Y1)
    Sxx <- crossprod(X1)
    
    ## Sxy <- crossprod(X, diag(fp[,i]))%*%Y
    ## Sxx <- crossprod(X, diag(fp[,i]))%*%X
    ## Syx <- crossprod(Y, diag(fp[,i]))%*%X
    
    hstar <- Sxx + init.model$H0  # Regression X'X + Prior
    
    # Compute regressions for a state using QR
    #
    # Note this includes the priors because of the padding in the
    # state-space matrices for fp.
    
    Bk[,,i] <- qr.coef(qr(hstar), Sxy + init.model$H0[,1:m])
    
    # Compute residuals and Sigma (based on Krolzig)
    
    # Get the full residuals -- need these for filtering -- note
    # we do not subset here!
    e[,,i] <- (Y - X%*%Bk[,,i])
    
    # Now compute the VCOV of the errors.  Note this can be done
    # in several ways for the posterior.  Want to use the
    # regime-specific, sample identified values. This depends on
    # having appropriate df and variation in the error
    # covariance.  Below assumes this is true.  Can do this also
    # with the classic Zellner formula of S0 + Y'Y + H0 - B'(X'X +
    # H0)B, adjusted by df, but this is numerically unstable in
    # some regime classifications.
    
    # Note this can also handle degenerate regimes (i.e., too few
    # obs.) since the prior will dominate the following
    # computation for the sample variance in a regime and be p.d.
    
    esub <- as.matrix(e[,,i])
    esub <- esub[fp[,i]==1,]
    Sigmak[,,i] <- (init.model$S0 + init.model$H0[1:m,1:m] +
                      crossprod(esub))/(df[i]+m+1)
    
    # Save the moments
    tmp[[i]] <- list(Sxy=Sxy, Sxx=Sxx)
  }
  
  # Only return the observation relevant residuals in "e"
  return(list(Bk=Bk, Sigmak=Sigmak, df=df, e=e[(m+2):nrow(e),,],
              moment=tmp))
}

# Count the number of state transitions for the n(ij) for the
# state-space updates.
count.transitions <- function(s)
{ M <- ncol(s)
TT <- nrow(s)
s <- crossprod(t(s), as.matrix(seq(1:M)))
sw <- matrix(0, M, M)
for (t in 2:TT)
{ st1 <- s[t-1]
st <- s[t]
sw[st1,st] <- sw[st1, st] + 1
}
return(sw)
}

######### gibbs.msbvar.R #########
# gibbs.msbvar.R Functions for Gibbs sampling an MSBVAR model based on
# a SZ prior.

# Patrick T. Brandt
# 20081113 : Initial version
# 20100325 : Added the computation for the log marginal data densities
# 20100615 : Cleaned up permutation / flipping code.  This now has its
#            own function that is called inside the Gibbs sampler.
#            Also reorganized the code in this file so that is easier
#            to read, document and follow.
# 20100617 : Wrote separate functions for the Beta, Sigma, and e block
#            updates.  Cleans up the code for the gibbs.msbvar into
#            managable chunks.
# 20120113 : Replaced filtering-sampler steps for the state space with
#            compiled Fortran code.
# 20140609 : Added gc() calls for some improved memory francege


####################################################################
# Utility functions for managing matrices in the Gibbs sampler
####################################################################
#
# vech function for efficiently sorting and subsetting the unique
# elements of symmetric matrices (like covariance Sigma)
#
vech <- function (x)
{
  x <- as.matrix(x)
  if (dim(x)[1] != dim(x)[2]) {
    stop("Non-square matrix passed to vech().\n")
  }
  output <- x[lower.tri(x, diag = TRUE)]
  dim(output) <- NULL
  return(output)
}

####################################################################
# xpnd function for reconstructing symmetric matrices from vech
####################################################################

xpnd <- function (x, nrow = NULL)
{
  dim(x) <- NULL
  if (is.null(nrow))
    nrow <- (-1 + sqrt(1 + 8 * length(x)))/2
  output <- matrix(0, nrow, nrow)
  output[lower.tri(output, diag = TRUE)] <- x
  hold <- output
  hold[upper.tri(hold, diag = TRUE)] <- 0
  output <- output + t(hold)
  return(output)
}

####################################################################
# tcholpivot function for computing Choleski's of known PD matrices
# with correction for underflow via pivoting.  Necessary to handle
# some numerically unstable possible draws that the Gibbs sampler can
# take with low probability.  Returns the draws after undoing the
# pivoting and transposing for correct francege in scaling draws from the
# relevant MVN densities.
####################################################################
tcholpivot <- function(x)
{
  tmp <- chol(x, pivot=TRUE)
  return(t(tmp))
}


####################################################################
# Sampler blocks for the MSBVAR Gibbs sampler
####################################################################
# Metropolis-Hastings sampler for the transition matrix Q
Q.drawMH <- function(Q, SStrans, prior, h)
{
  Q.old <- Q
  
  # compute the density ordinate based on the counts.
  alpha <- SStrans + prior - matrix(1, h, h)
  
  Q.new <- t(apply(alpha, 1, rdirichlet, n=1))
  
  eta.new <- eta.ergodic(Q.new,h)
  eta.old <- eta.ergodic(Q.old,h)
  
  # acceptance rate
  # assume initial state is state 1
  A <- eta.new[1] / eta.old[1]
  
  if (runif(1) < A) {
    # need to add acceptance ratio calculations here
    # e.g., accsum <- accsum + 1
    return(Q.new)
  } else {
    return(Q.old)
  }
}

# Calculate invariant probability distribution, eta,
# when assumed distribution for initial state is ergodic
# Fruhwirth-Schnatter 2006, page 306
eta.ergodic <- function(Q, h)
{
  A <- rbind(diag(h)-Q, 1)
  b <- solve(t(A) %*% A) %*% t(A)
  eta <- b[,h+1]  # h by 1 column vector
  return(eta)
}


# Gibbs sampler for the transition matrix Q
Q.drawGibbs <- function(Q, SStrans, prior, h)
{
  # compute the density ordinate based on the counts.
  alpha <- SStrans + prior - matrix(1, h, h)
  # Sample
  Q.new <- t(apply(alpha, 1, rdirichlet, n=1))
  return(Q.new)
}


####################################################################
# State-space sampling for the regimes -- this is the FFBS
# implementation.  Actual work is done by calling compiled Fortran
# code in the package
####################################################################
## oldSS.ffbs <- function(e, bigt, m, p, h, sig2, Q)
## {
##     TT <- bigt-p
##     SStmp <- .Fortran("FFBS",
##                       bigt=as.integer(bigt),
##                       m = as.integer(m), p = as.integer(p),
##                       h = as.integer(h), e = e, sig2 = sig2,
##                       Q = Q, f = double(1),
##                       filtprSt = matrix(0,as.integer(TT),as.integer(h)),
##                       SS = matrix(as.integer(0),
##                                   as.integer(TT),as.integer(h)),
##                       transmat = matrix(as.integer(0),
##                                         as.integer(h),as.integer(h))
##                       )

##     # Output
##     ss <- list(SS=SStmp$SS, transitions=SStmp$transmat)
##     return(ss)
## }

SS.ffbs <- function(e, bigt, m, p, h, sig2, Q)
{
  
  # Hamilton's forward filter
  
  ff <- .Fortran('ForwardFilter',
                 nvar=as.integer(m), e=e,
                 bigK=as.integer(h), bigT=as.integer(bigt),
                 nbeta=as.integer(1+m*p),
                 sig2, Q,
                 llh=double(1),
                 pfilt=matrix(0,bigt+1,h))
  
  # Backwards multi-move sampler
  
  # Draw random Unif(0,1) to feed into backwards sampler.
  # Doing this on the R side (rather than using RAND() in Fortran)
  # allows to control the seed.
  rvu <- runif(bigt+1)
  
  bs <- .Fortran('BackwardSampler',
                 bigK=as.integer(h),bigT=as.integer(bigt),
                 ff$pfilt,Q,rvu,
                 backsamp.bigS=integer(bigt+1),
                 transmat=matrix(as.integer(0),h,h))
  
  # Now, convert the draws in backsamp.bigS (1,2,3,...)
  # to matrix of zeros and ones.
  # Includes initial state, we remove in the below ([-1])
  matSS <- matrix(as.integer(matrix(seq(1,h), bigt, h,
                                    byrow=TRUE)==bs$backsamp.bigS[-1]),
                  bigt, h)
  
  # Output:
  # 1) SS = state-space
  # 2) transitions = transition matrix
  # 3) llf = log-likelihood computed by the filter
  ss <- list(SS=matSS, transitions=bs$transmat, llf=ff$llh)
  return(ss)
}




####################################################################
# Sample the error covariances for each state
####################################################################

Sigma.draw <- function(m, h, ss, hreg, Sigmai)
{
  # Draw the variances for the MSVAR from an inverse Wishart --
  # draws h matrices which are returned as an m^2 x h matrix.  These
  # then need to be unwound into the matrices we need.
  
  # Unscaled inverse Wishart draws
  df <- hreg$df
  tmp <- array(unlist(lapply(sapply(1:h,
                                    function(i) {rwishart(N=1,
                                                          df=df[i]+m+1,
                                                          Sigma=diag(m))},
                                    simplify=FALSE), solve)), c(m,m,h))
  
  wishscale <- hreg$Sigmak
  
  # Scale the draws for each regime
  for (i in 1:h)
  {
    #        dc <- tcholpivot(hreg$Sigmak[,,i])
    dc <- t(chol(hreg$Sigmak[,,i]))
    Sigmai[,,i] <- dc%*%(tmp[,,i]*(df[i]+m+1))%*%t(dc)
    wishscale[,,i] <- wishscale[,,i]*(df[i]+m+1)
  }
  
  
  return(list(Sigmai=Sigmai, wishscale=wishscale))
}

####################################################################
# Sample the regressors for each state
####################################################################

Beta.draw <- function(m, p, h, Sigmai, hreg, init.model, Betai)
{
  mmp1 <- m*(m*p+1)
  Beta.cpprec <- Beta.cpv <- array(NA, c(mmp1,mmp1,h))
  
  for(i in 1:h)
  {
    # Set up some the moments we need
    # 1) (X'X)^{-1} with the prior included -- see regression step
    # 2) Use the fact that the inverses and Kroneckers can be
    # switched around.
    # 3) Compute the posterior precision and variance on the fly
    # for later use
    
    # Robust version that uses stable choleskys to do the
    # computation
    Beta.cpprec[,,i] <- kronecker(Sigmai[,,i], hreg$moment[[i]]$Sxx
                                  + init.model$H0)
    Beta.cpv[,,i] <- chol2inv(chol(Beta.cpprec[,,i]))
    
    bcoefs.se <- t(chol(Beta.cpv[,,i]))
    
    # Sample the regressors
    Betai[,,i] <- hreg$Bk[,,i] +
      matrix(bcoefs.se%*%matrix(rnorm(m^2*p+m), ncol=1), ncol=m)
  }
  
  # Returns
  # 1) Betai = the MCMC draw
  # 2) Beta.cpm = conditional posterior mean
  # 3) Beta.cpprec = conditional posterior precision
  # 4) Beta.cpv = conditional posterior variance
  
  return(list(Betai=Betai, Beta.cpm=hreg$Bk, Beta.cpprec=Beta.cpprec,
              Beta.cpv=Beta.cpv))
}

####################################################################
# Update the residuals for each state
# Start with m+2 residual, since the others are part of the dummy
# observations for the prior.
####################################################################
residual.update <- function(m, h, init.model, Betai, e)
{
  for(i in 1:h)
  {
    e[,,i] <- init.model$Y[(m+2):nrow(init.model$Y),] -
      init.model$X[(m+2):nrow(init.model$X),]%*%Betai[,,i]
  }
  return(e)
}

######################################################################
# Function to do the random permuation / state labeling steps for the
# models.  This function handles both the random permutation steps and
# the identified models based on either tbe Beta.idx or the Sigma.idx
# input args.
######################################################################
PermuteFlip <- function(x, h, permute, Beta.idx, Sigma.idx)
{
  # Random permutation step
  if(permute==TRUE){
    rp <- sort(runif(h), index.return=TRUE)$ix
    
    Sigmaitmp <- x$Sigmai; Betaitmp <- x$Betai
    etmp <- x$e; sstmp <- x$ss; df <- x$df
    
    for(i in 1:h)
    {
      Sigmaitmp[,,i] <- x$Sigmai[,,rp[i]]
      Betaitmp[,,i] <- x$Betai[,,rp[i]]
      etmp[,,i] <- x$e[,,rp[i]]
      sstmp$SS[,i] <- x$ss$SS[,rp[i]]
    }
    
    Q <- x$Q[rp,rp]
    sstmp$transitions <- x$ss$transitions[rp, rp]
    df <- df[rp]
    return(list(Betai=Betaitmp, Sigmai=Sigmaitmp,
                ss=sstmp, e=etmp, Q=Q, df=df))
  }
  
  # Sort regimes based on values of regressors in Beta using the
  # Beta.idx indices
  
  if(permute==FALSE & is.null(Beta.idx)==FALSE){
    # Find the sort indices for the objects
    ix <- sort(x$Betai[Beta.idx[1], Beta.idx[2],], index.return=TRUE)$ix
    
    Sigmaitmp <- x$Sigmai
    Betaitmp <- x$Betai
    etmp <- x$e
    sstmp <- x$ss
    df <- x$df
    
    for(i in 1:h)
    {
      Sigmaitmp[,,i] <- x$Sigmai[,,ix[i]]
      Betaitmp[,,i] <- x$Betai[,,ix[i]]
      etmp[,,i] <- x$e[,,ix[i]]
      sstmp$SS[,i] <- x$ss$SS[,ix[i]]
    }
    
    Q <- x$Q[ix,ix]
    sstmp$transitions <- x$ss$transitions[ix, ix]
    df <- df[ix]
    return(list(Betai=Betaitmp, Sigmai=Sigmaitmp, ss=sstmp,
                e=etmp, Q=Q, df=df))
  }
  
  # Sort regimes based on values of the Sigma matrix element
  # selected.
  
  if(permute==FALSE & is.null(Sigma.idx)==FALSE){
    # Find the sort indices for the objects
    ix <- sort(x$Sigmai[Sigma.idx, Sigma.idx,], index.return=TRUE)$ix
    
    Sigmaitmp <- x$Sigmai
    Betaitmp <- x$Betai
    etmp <- x$e
    sstmp <- x$ss
    df <- x$df
    
    for(i in 1:h)
    {
      Sigmaitmp[,,i] <- x$Sigmai[,,ix[i]]
      Betaitmp[,,i] <- x$Betai[,,ix[i]]
      etmp[,,i] <- x$e[,,ix[i]]
      sstmp$SS[,i] <- x$ss$SS[,ix[i]]
    }
    
    Q <- x$Q[ix,ix]
    sstmp$transitions <- x$ss$transitions[ix, ix]
    df <- df[ix]
    return(list(Betai=Betaitmp, Sigmai=Sigmaitmp, ss=sstmp,
                e=etmp, Q=Q, df=df))
  }
}

######################################################################
# marginal.moments() Computes the marginal moments for the conditional
# posterior of Beta and Sigma for the given prior.  These computations
# are necessary for later evaluation of the marginal likelihood.  So
# this function is called conditionally after the regression draws are
# made in order to get the conditional moments.
######################################################################

## marginal.moments <- function(h, Betai, Sigmai, hreg, init.model)
## {
##     # Constants we need up front
##     tmp <- dim(Betai)
##     mp1 <- tmp[1]
##     m <- tmp[2]
##     h <- tmp[3]

##     prior.b0m <- as.vector(rbind(diag(m), matrix(0, m*(p-1)+1, m)))

##     # Storage

##     for(i in 1:h)
##     {
##         Sinv <- chol2inv(chol(Sigmai[,,i]))
##         SiginvXX <- kronecker(Sinv, hreg$moment[[i]]$Sxx)

##         # Compute the conditional posterior precision, B|S^{-1}
##         beta.prec <- kronecker(Sinv, init.model$H0) + SiginvXX

##         # Compute conditional posterior variance, B|S
##         beta.cpv <- chol2inv(chol(beta.prec))

##         # Compute the conditional posterior mean
##         bhat <- kronecker(Sinv, diag(mp1)) %*% as.vector(hreg$moment[[i]]$Sxy)
##         beta.cpm <- beta.cpv %*% kronecker(Sinv, init.model$H0) + bhat

##         # Flatten things out for later storage

##     }
##     return()
## }

####################################################################
# Full MCMC sampler for MSBVAR models with SZ prior
####################################################################

gibbs.msbvar <- function(x, N1=1000, N2=1000,
                         permute=TRUE,
                         Beta.idx=NULL, Sigma.idx=NULL,
                         Q.method="MH")
{
  # Do some sanity / error checking on the inputs so people know
  # what they are getting back
  
  if(permute==TRUE & is.null(Beta.idx)==FALSE){
    cat("You have set permute=TRUE which means you requested a random permutation sample.\n Beta.idx argument will be ignored")
  }
  
  if(permute==TRUE & is.null(Sigma.idx)==FALSE){
    cat("You have set permute=TRUE which means you requested a random permutation sample.\n Sigma.idx argument will be ignored")
  }
  if(permute==FALSE & (is.null(Beta.idx)==TRUE &
                       is.null(Sigma.idx)==TRUE)){
    cat("You have set permute=FALSE, but failed to provide an identification to label the regimes.\n Set Beta.idx or Sigma.idx != NULL")
  }
  
  # Set the sampler method for Q based on inputs
  if(Q.method=="Gibbs") Qsampler <- Q.drawGibbs
  if(Q.method=="MH") Qsampler <- Q.drawMH
  
  # Initializations
  init.model <- x$init.model
  hreg <- x$hreg
  Q <- x$Q
  fp <- x$fp
  m <- x$m
  p <- x$p
  h <- x$h
  alpha.prior <- x$alpha.prior
  TT <- nrow(fp)
  
  # Initialize the Gibbs sampler parameters
  e <- hreg$e
  Sigmai <- hreg$Sigma
  Betai <- hreg$Bk
  
  # Burnin loop
  for(j in 1:N1)
  {
    # Smooth / draw the state-space, with checks for the
    # degeneracy of the state-space on the R side.
    ##         oldtran <- matrix(0,h,h)
    
    ##         while(sum(diag(oldtran)==0)>0)
    ##         {
    ##             ss <- SS.ffbs(e, (TT+p), m, p, h, Sigmai, Q) # may need p=0
    ##             oldtran <- ss$transitions
    ## #            print(oldtran)
    ##         }
    
    ss <- SS.ffbs(e, TT, m, p, h, Sigmai, Q)
    
    #        print(ss$transitions)
    # Draw Q
    Q <- Qsampler(Q, ss$transitions, prior=alpha.prior, h)
    
    # Update the regression step
    hreg <- hregime.reg2(h, m, p, ss$SS, init.model)
    
    # Draw the variance matrices for each regime
    Sout <- Sigma.draw(m, h, ss, hreg, Sigmai)
    Sigmai <- Sout$Sigmai
    
    # Sample the regression coefficients
    Bout <- Beta.draw(m, p, h, Sigmai, hreg, init.model, Betai)
    Betai <- Bout$Betai
    
    # Update the residuals
    e <- residual.update(m, h, init.model, Betai, e)
    
    # Now do the random permutation of the labels with an MH step
    # for the permutation or sort the regimes based on the
    # Beta.idx or Sigma.idx conditions.
    pf.obj <- PermuteFlip(x=list(Betai=Betai, Sigmai=Sigmai,
                                 ss=ss, e=e, Q=Q, df=hreg$df),
                          h, permute, Beta.idx, Sigma.idx)
    
    # Replace sampler objects with permuted / flipped ones
    Betai <- pf.obj$Betai
    Sigmai <- pf.obj$Sigmai
    ss <- pf.obj$ss
    e <- pf.obj$e
    Q <- pf.obj$Q
    
    # Print out some iteration information
    if(j%%1000==0) cat("Burn-in iteration : ", j, "\n")
    
  }
  
  # End of burnin loop!
  
  gc(); gc()
  
  # Declare the storage for the return objects
  ss.storage <- vector("list", N2)
  transition.storage <- array(NA, c(h,h,N2))
  Beta.storage <- matrix(NA, N2, (m^2*p+m)*h)
  Sigma.storage <- matrix(NA, N2, m*(m+1)*0.5*h)
  Q.storage <- matrix(NA, N2, h^2)
  llf <- matrix(NA, N2, 1)
  
  # Storage for the conditional posterior moments when permute==TRUE
  if(permute==TRUE)
  {
    # Conditional posterior moments
    Beta.cpm <- matrix(NA, N2, (m^2*p+m)*h)
    Sigma.wishscale <- matrix(NA, N2, 0.5*m*(m+1)*h)
    tmpdim <- m*((m*p)+1)
    Beta.cpprec <- Beta.cpv <- matrix(NA, N2, 0.5*tmpdim*(tmpdim+1)*h)
    # Sigma / Wishart df moments
    df.storage <- matrix(NA, N2, h)
    # Q, conditional posterior
    Q.cp <- matrix(NA, N2, h^2)
  }
  
  # Main loop after the burnin -- these are the sweeps that are kept
  # for the final posterior sample.
  # Note, we need to do the storage AFTER the permutation steps so
  # we get the labeling correct.
  for(j in 1:N2)
  {
    # Draw the state-space
    # Smooth / draw the state-space, with checks for the
    # degeneracy of the state-space on the R side.
    ## oldtran <- matrix(0,h,h)
    
    ## while(sum(diag(oldtran)==0)>0)
    ## {
    ##     ss <- SS.ffbs(e, (TT+p), m, p, h, Sigmai, Q) # may need p=0
    ##     oldtran <- ss$transitions
    ## }
    
    ss <- SS.ffbs(e, TT, m, p, h, Sigmai, Q)
    
    # Draw Q
    Q <- Qsampler(Q, ss$transitions, prior=alpha.prior, h)
    
    # Update the regression step
    hreg <- hregime.reg2(h, m, p, ss$SS, init.model)
    
    # Draw the variance matrices for each regime
    Sout <- Sigma.draw(m, h, ss, hreg, Sigmai)
    Sigmai <- Sout$Sigmai
    
    # Sample the regression coefficients
    Bout <- Beta.draw(m, p, h, Sigmai, hreg, init.model, Betai)
    Betai <- Bout$Betai
    
    # Update the residuals
    e <- residual.update(m, h, init.model, Betai, e)
    
    # Now do the random permutation of the labels with an MH step
    # for the permutation or sort the regimes based on the
    # Beta.idx or Sigma.idx conditions.
    pf.obj <- PermuteFlip(x=list(Betai=Betai, Sigmai=Sigmai,
                                 ss=ss, e=e, Q=Q, df=hreg$df),
                          h, permute, Beta.idx, Sigma.idx)
    
    # Replace sampler objects with permuted / flipped ones
    Betai <- pf.obj$Betai
    Sigmai <- pf.obj$Sigmai
    ss <- pf.obj$ss
    e <- pf.obj$e
    Q <- pf.obj$Q
    df <- pf.obj$df
    
    # Store things -- after flip / identification
    Sigma.storage[j,] <- as.vector(apply(Sigmai, 3, vech))
    Beta.storage[j,] <- as.vector(Betai)
    ss.storage[[j]] <- as.integer(as.integer(ss$SS[,1:(h-1)]))
    transition.storage[,,j] <- ss$transitions
    Q.storage[j,] <- as.vector(Q)
    
    # Store the llf
    llf[j,1] <- ss$llf
    
    # Store conditional posterior moments
    if(permute==TRUE)
    {
      Beta.cpm[j,] <- as.vector(Bout$Beta.cpm)
      Beta.cpprec[j,] <- as.vector(apply(Bout$Beta.cpprec, 3, vech))
      Beta.cpv [j,] <- as.vector(apply(Bout$Beta.cpv, 3, vech))
      Sigma.wishscale[j,] <- as.vector(apply(Sout$wishscale, 3, vech))
      df.storage[j,] <- df
      Q.cp[j,] <- as.vector(ss$transitions + alpha.prior)
    }
    
    # Print out some iteration information
    if(j%%1000==0) cat("Final iteration : ", j, "\n")
  }
  
  gc(); gc()
  
  # Now make an output object and provide classing
  class(ss.storage) <- c("SS")
  
  if(permute==FALSE)
  {
    output <- list(Beta.sample=mcmc(Beta.storage),
                   Sigma.sample=mcmc(Sigma.storage),
                   Q.sample=mcmc(Q.storage),
                   transition.sample=transition.storage,
                   ss.sample=ss.storage,
                   llf=llf,
                   init.model=init.model,
                   alpha.prior=alpha.prior,
                   h=h,
                   p=p,
                   m=m)
  } else {
    output <- list(Beta.sample=mcmc(Beta.storage),
                   Sigma.sample=mcmc(Sigma.storage),
                   Q.sample=mcmc(Q.storage),
                   transition.sample=transition.storage,
                   ss.sample=ss.storage,
                   llf=llf,
                   init.model=init.model,
                   alpha.prior=alpha.prior,
                   h=h,
                   p=p,
                   m=m,
                   Beta.cpm=Beta.cpm,
                   Beta.cpprec=Beta.cpprec,
                   Beta.cpv=Beta.cpv,
                   Sigma.wishscale=Sigma.wishscale,
                   df=df.storage,
                   Q.cp=Q.cp)
  }
  
  # Class the equation name information
  class(output) <- c("MSBVAR")
  attr(output, "eqnames") <- attr(init.model, "eqnames")
  
  # Keep track of the regime identification properties from
  # estimation.
  attr(output, "permute") <- permute
  attr(output, "Beta.idx") <- Beta.idx
  attr(output, "Sigma.idx") <- Sigma.idx
  attr(output, "Qsampler") <- Q.method
  
  # ts attributes for inputs to later plotting and summary functions
  # for the states.
  tmp <- tsp(init.model$y)
  attr(output, "start") <- tmp[1]
  attr(output, "end") <- tmp[2]
  attr(output, "freq") <- tmp[3]
  
  return(output)
}

################################################################
# Deprecated code
################################################################

# R code for legacy purposes
#SS.ffbs(e, TT+p, m, p, h, Sigmai, Q)
#SS.ffbs <- function(e, bigt, m, p, h, sig2, Q)
#{
#  TT <- bigt-p
#
#  # Forward-filtering
#  fHam <- .Fortran("HamiltonFilter",
#                   bigt=as.integer(bigt),
#                   m = as.integer(m), p = as.integer(p), h = as.integer(h),
#                   e = e,
#                   sig2 = sig2,
#                   Q = Q,
#                   f = double(1),
#                   filtprSt = matrix(0,as.integer(TT),as.integer(h))
#                   )
#  fpH <- fHam$filtprSt
#
#  # Backwards-sampling
#  SS <- matrix(0, nrow=TT, ncol=h)
#  SS[TT,] <- bingen(fpH[TT,], Q, 1, h)
#  for (t in (TT-1):1)
#  {
#    SS[t,] <- bingen(fpH[t,], Q, which(SS[t+1,]==1), h)
#  }
#
#  # Construct STT (TTx1 vector of regimes)
#  STT <- rep(0,TT)
#  for (ist in 1:TT) {
#    STT[ist] <- which(SS[ist,]==1)
#  }
#
#  # Construct transition matrix
#  transmat <- matrix(0,h,h)
#
#  for (ist in 1:(TT-1)) {
#    transmat[STT[ist+1], STT[ist]] <- transmat[STT[ist+1], STT[ist]] + 1
#  }
#
#  ss <- list(SS=SS,
#             transitions=transmat)
#
#  return(ss)
#}
#
#bingen <- function(prob, Q, st1, h)
#{
#  i <- 1
#  while(i<h)
#  {
#    pr0 <- prob[i]*Q[st1,i]/sum(prob[i:h]*Q[st1,i:h])
#    if(runif(1)<=pr0)
#    { return(diag(h)[i,]) } else { st1 <- i <- i+1 }
#  }
#  return(diag(h)[h,])
#}

######### posterior_fit.R #######

"posterior.fit" <- function(varobj, A0.posterior.obj=NULL, maxiterbs=500)
{
  if(inherits(varobj, "VAR"))
  {
    stop("posterior.fit() not implemented for VAR class objects since they do not have a proper prior.\n")
    ##         output <- posterior.fit.VAR(varobj)
    ##         attr(output, "class") <- c("posterior.fit.VAR")
    ##         return(output)
  }
  if(inherits(varobj, "BVAR"))
  {
    output <- posterior.fit.BVAR(varobj)
    attr(output, "class") <- c("posterior.fit.BVAR")
    return(output)
  }
  if(inherits(varobj, "BSVAR"))
  {
    output <- posterior.fit.BSVAR(varobj, A0.posterior.obj)
    attr(output, "class") <- c("posterior.fit.BSVAR")
    return(output)
  }
  if(inherits(varobj, "MSBVAR"))
  {
    output <- posterior.fit.MSBVAR(x=varobj, maxiterbs=maxiterbs)
    attr(output, "class") <- c("posterior.fit.MSBVAR")
    return(output)
  }
}

"posterior.fit.BSVAR" <- function(varobj, A0.posterior.obj)
{  # Constants from the model object
  
  m <- ncol(varobj$A0.mode)
  gk <- varobj$F.posterior
  N2 <- A0.posterior.obj$N2
  
  # compute the pdf of the prior: p(A_0, A_+) and the exponential
  # term of pr(Y|A0, A+)
  
  yexpt <- 0.0              # initial value for the exp term of pr(Y|A0, A+)
  pa0ap <- matrix(0, m, 1)  # Vector of log priors.
  
  for (i in 1:m)
  {
    # Grab the parameters in each structural equation.
    a0k <- varobj$A0.mode[,i]
    apk <- varobj$F.posterior[,i]
    
    # covariances of the free parameters in each equation
    S0bar <- varobj$H0inv.tilde[[i]]
    Spbar <- varobj$Hpinv.tilde[,,i]
    
    # Next 2 lines are the free parameters in each equation
    bk <- t(varobj$Ui[[i]])%*%a0k
    gbark <- varobj$Pi.tilde[[i]]%*%bk
    
    # Compute the exponential term.
    yexpt <- yexpt + 0.5*(t(a0k)%*%varobj$YY%*%a0k -
                            2*t(apk) %*% varobj$XY %*% a0k +
                            crossprod(apk, varobj$XX)%*%apk)
    # log prior
    pa0ap[i,] <- -0.5*(ncol(matrix(varobj$Ui[[i]], m)) +
                         nrow(varobj$F.posterior))*log(2*pi) +
      0.5*log(abs(sum(diag(S0bar)))) +
      0.5*log(abs(sum(diag(Spbar)))) -
      0.5*(t(bk)%*%S0bar%*%bk + t(gk[,i] - gbark)%*%Spbar%*%(gk[,i]-gbark))
  }
  
  # Find log p(Y | A0, A+) --- this is the log likelihood at
  # the restricted parameters, so it is conditional on the model.  We
  # can use this later for computing the MDD and log posterior for the
  # models.  We could compute this without the loop, but then we would
  # not have the cumulative version for the CBF calculations later!
  
  U <- qr.R(qr(t(varobj$A0.mode)))
  ada <- sum(log(abs(diag(U))))
  log.llf <- -0.5*m*varobj$df*log(2*pi) + varobj$df*ada + yexpt
  
  # Now find Pr(A+ | Y, A0)
  totparam <- length(varobj$F.posterior)
  ld.covar.param <- sum(sapply(1:m, function(i) {
    determinant(varobj$Hpinv.posterior[[i]])$modulus }))
  
  bk <- a2b(varobj$A0.mode, varobj$Ui)
  Apexpt <- 0
  n0cum <- c(0,cumsum(varobj$n0))
  
  for (i in 1:m)
  {
    bj <- bk[(n0cum[i]+1):(n0cum[(i+1)])]
    tmp <- varobj$F.posterior[,i] - varobj$P.posterior[[i]]%*%bj
    Apexpt <- Apexpt - 0.5%*%t(tmp)%*%varobj$Hpinv.posterior[[i]]%*%tmp
  }
  
  pa.ya0 <- -0.5*totparam*log(2*pi) + ld.covar.param + Apexpt
  
  ################################################################################
  # Now find the pdf for each equation: p(a0_k |Y, a0_{not k})
  ################################################################################
  # Set up the gibbs sampler
  setup <- gibbs.setup.bsvar(varobj)
  Tinv <- setup$Tinv
  UT <- setup$UT
  
  logpa0.yao <- matrix(0, m, 1)  # storage for log Pr(A0_k | Y A0_k*, k*\ne k)
  
  vlog <- matrix(0, N2, 1)       # storage across draws.
  A0gbs <- varobj$A0.mode
  Wout <- vector("list", m)
  bk <- a2b(varobj$A0.mode, varobj$Ui)
  b.free <- vector("list", m)
  for(i in 1:m)
  {
    b.free[[i]] <- bk[(n0cum[i]+1):(n0cum[(i+1)])]
  }
  
  # Build up the constant terms we need for each of the A0{k) calculations
  lgamma.term <- lgamma(0.5*(varobj$df+1))
  ldf <- log(varobj$df)
  df1 <- 0.5*(varobj$df+1)*log(2)
  constant1 <- 0.5*(varobj$df+varobj$n0)*ldf
  constant2 <- 0.5*(varobj$n0-1)*log(2*pi)
  constantterm <- constant1 - constant2 - df1 - lgamma.term
  
  logpa0.yao <- .Call("log.marginal.A0k",
                      A0.posterior.obj$W.posterior, varobj$A0.mode,
                      as.integer(N2), constantterm, b.free, UT, Tinv,
                      as.integer(varobj$df), varobj$n0)
  
  # Now compute the output objects
  log.prior <- sum(pa0ap)
  lPy <- log.prior + log.llf - sum(logpa0.yao) - pa.ya0
  
  output <- list(log.prior=log.prior,
                 log.llf=log.llf,
                 log.posterior.Aplus=pa.ya0,
                 log.marginal.data.density=lPy,
                 log.marginal.A0k=logpa0.yao)
  class(output) <- c("posterior.fit.BSVAR")
  return(output)
}

"posterior.fit.BVAR" <- function(varobj)
{
  # Assign variables
  capT <- varobj$pfit$capT
  m <- varobj$pfit$m
  ncoef <- varobj$pfit$ncoef
  num.exog <- varobj$pfit$num.exog
  nu <- varobj$pfit$nu
  H0 <- varobj$pfit$H0
  S0 <- varobj$pfit$S0
  Y <- varobj$pfit$Y
  X <- varobj$pfit$X
  hstar1 <- varobj$pfit$hstar1
  Sh <- varobj$pfit$Sh
  u <- varobj$pfit$u
  Bh <- varobj$pfit$Bh
  Sh1 <- varobj$pfit$Sh1
  
  # Compute the marginal posterior LL value
  # For the derivation of the integrand see Zellner 1971, Section 8.2
  
  scalefactor <- (sum(lgamma(nu + 1 - seq(1:m))) -
                    sum(lgamma(nu + capT + 1 - seq(1:m))))
  
  # Find some log-dets to make the final computation easier.
  # This does depend on the prior chosen, since some of these
  # matrices will be zero for flat prior model, so the ldet of
  # the S0 mtx will be zero.
  # This is done with if-else statements.
  
  M0 <- (diag(capT) + X%*%solve(H0)%*%t(X))
  B0 <- matrix(0,nrow=(ncoef+num.exog),ncol=m)
  diag(B0) <- 1
  Bdiff <- Y-X%*%B0
  
  ld.S0 <- determinant(S0, logarithm=T)
  ld.S0 <- ld.S0$sign * ld.S0$modulus
  
  ld.M0 <- determinant(M0, logarithm=T)
  ld.M0 <- ld.M0$sign * ld.M0$modulus
  
  ld.tmp <- determinant((S0 + t(Bdiff)%*%solve(M0)%*%Bdiff), logarithm=T)
  ld.tmp <- ld.tmp$sign * ld.tmp$modulus
  
  data.marg.llf <-  (- 0.5*capT*m*log(2*pi)
                     - m*0.5*ld.M0
                     + capT*0.5*ld.S0
                     - scalefactor
                     - 0.5*(nu+capT)*ld.tmp)
  
  # Now find the predictive posterior density
  M1 <- (diag(capT) + X%*%solve(hstar1)%*%t(X))
  
  ld.S1 <- determinant(Sh, logarithm=T)
  ld.S1 <- ld.S1$sign * ld.S1$modulus
  
  ld.M1 <- determinant(M1, logarithm=T)
  ld.M1 <- ld.M1$sign * ld.M1$modulus
  
  ld.tmp <- determinant((Sh + t(u)%*%solve(M1)%*%u), logarithm=T)
  ld.tmp <- ld.tmp$sign * ld.tmp$modulus
  
  data.marg.post <- (- 0.5*capT*m*log(2*pi)
                     - m*0.5*ld.M1
                     + capT*0.5*ld.S1
                     - scalefactor
                     - 0.5*(nu+capT)*ld.tmp)
  
  # Now compute the marginal llf and the posterior for the
  # coefficients
  Bdiff <- B0 - Bh
  ld.S1 <- determinant(Sh1, logarithm=T)
  ld.S1 <- ld.S1$sign * ld.S1$modulus
  wdof <- capT - ncoef - num.exog - m - 1
  
  scalefactor1 <- (wdof*m*0.5)*log(2) + 0.25*m*(m-1) + (sum(lgamma(wdof + 1 - seq(1:m))))
  scalefactor2 <- -0.5*(ncoef*m)*log(2*pi)
  coef.post <- (scalefactor1 + scalefactor2 -0.5*(nu + capT + m +1)*ld.S1
                - 0.5*sum(diag(solve(Sh1)%*%Sh))
                - 0.5*(ncoef+num.exog)*ld.S1
                - 0.5*sum(diag(Sh1%*%t(Bdiff)%*%hstar1%*%Bdiff)))
  
  output <- list(data.marg.llf=data.marg.llf,
                 data.marg.post=data.marg.post,
                 coef.post=coef.post)
  class(output) <- c("posterior.fit.BVAR")
  return(output)
}

"print.posterior.fit" <- function(x, ...)
{
  ##     if(inherits(x, "posterior.fit.VAR"))
  ##     { print.posterior.fit.VAR(x, ...) }
  
  if(inherits(x, "posterior.fit.BVAR"))
  { print.posterior.fit.BVAR(x, ...) }
  
  if(inherits(x, "posterior.fit.BSVAR"))
  { print.posterior.fit.BSVAR(x, ...) }
  
  if(inherits(x, "posterior.fit.MSBVAR"))
  { print.posterior.fit.MSBVAR(x, ...) }
  
}

## "print.posterior.fit.VAR" <- function(x, ...)
## {
##     cat("Log marginal density, Pr(Y)         :  ", x$data.marg.llf, "\n")
##     cat("Predictive marginal density         :  ", x$data.marg.post, "\n")
##     cat("Coefficient marginal LLF/posterior  :  ", x$coef.post, "\n")
## }

"print.posterior.fit.BVAR" <- function(x, ...)
{
  cat("Log marginal density, Pr(Y)         :  ", x$data.marg.llf, "\n")
  cat("Predictive marginal density         :  ", x$data.marg.post, "\n")
  cat("Coefficient marginal LLF/posterior  :  ", x$coef.post, "\n")
}

"print.posterior.fit.BSVAR" <- function(x, ...)
{    cat("Log prior, Pr(A0, A+)            : ", x$log.prior, "\n")
  cat("Log LLF, Pr(Y | A0, A+)          : ", x$log.llf, "\n")
  cat("Log Pr(A+|Y, A0)                 : ", x$log.posterior.Aplus, "\n")
  cat("Log marginal density, Pr(Y)      : ", x$log.marginal.data.density, "\n")
  cat("Log marginal A0(k) | not A0(k)   : ", x$log.marginal.A0k, "\n")
}

"print.posterior.fit.MSBVAR" <- function(x, ...)
{
  cat("Marginal likelihood, importance sampler           :", x$marglik.IS, "\n")
  cat("Marginal likelihood std error, importance sampler :", x$marglik.IS.se, "\n")
  
  cat("Marginal likelihood, reciprocal sampler           :", x$marglik.RI, "\n")
  cat("Marginal likelihood std error, reciprocal sampler :", x$marglik.RI.se, "\n")
  
  cat("Marginal likelihood, bridge sampler               :", x$marglik.BS, "\n")
  cat("Marginal likelihood std error, bridge sampler     :", x$marglik.BS.se, "\n")
  
}


######## mc_irf.R #######
# Generate the impulse responses -- requires a posterior sample of the
# A0 to be drawn already.  Could probably put an option / overload in here that
# if there are no A0 supplied that it draws them.

"mc.irf" <- function(varobj, nsteps, draws=1000, A0.posterior=NULL, sign.list=rep(1,ncol(varobj$Y)))
{
  if(inherits(varobj, "VAR")){
    return(mc.irf.VAR(varobj=varobj, nsteps=nsteps, draws=draws))
  }
  if(inherits(varobj, "BVAR")){
    return(mc.irf.BVAR(varobj=varobj, nsteps=nsteps, draws=draws))
  }
  if(inherits(varobj, "BSVAR")){
    return(mc.irf.BSVAR(varobj=varobj, nsteps=nsteps,
                        A0.posterior=A0.posterior, sign.list=sign.list))
  }
  if(inherits(varobj, "MSBVAR")){
    return(mc.irf.MSBVAR(varobj=varobj, nsteps=nsteps,
                         draws=length(varobj$ss.sample)))
  }
}

"mc.irf.VAR" <- function(varobj, nsteps, draws)
{ output <- .Call("mc.irf.var.cpp", varobj, as.integer(nsteps),
                  as.integer(draws))
attr(output, "class") <- c("mc.irf", "mc.irf.VAR")
attr(output, "eqnames") <- attr(varobj, "eqnames")
return(output)
}


"mc.irf.BVAR" <- function(varobj, nsteps, draws)
{
  output <- mc.irf.VAR(varobj, nsteps, draws)
  attr(output, "class") <- c("mc.irf", "mc.irf.BVAR")
  attr(output, "eqnames") <- attr(varobj, "eqnames")
  return(output)
}


"mc.irf.BSVAR" <- function(varobj, nsteps, A0.posterior, sign.list)
{ m<-dim(varobj$ar.coefs)[1]  # Capture the number of variablesbcoefs <- varobj$Bhat

# Check length of sign.list
if(length(sign.list)!=m) {
  stop("sign.list has wrong number of elements.  Must be m")
}

p<-dim(varobj$ar.coefs)[3]    # Capture the number of lags
ncoef <- dim(varobj$B.posterior)[1]
n0 <- varobj$n0
n0cum <- c(0,cumsum(n0))
N2 <- A0.posterior$N2

# Get the covar for the coefficients
XXinv <- chol(solve(varobj$Hpinv.posterior[[1]]))

# storage for the impulses and the sampled coefficients.
impulse <- matrix(0,nrow=N2, ncol=(m^2*nsteps))

output <- .Call("mc.irf.bsvar.cpp", A0.posterior$A0.posterior,
                as.integer(nsteps), as.integer(N2), as.integer(m),
                as.integer(p), as.integer(ncoef), as.integer(n0),
                as.integer(n0cum), XXinv, varobj$Ui, varobj$P.posterior,
                sign.list)
attr(output, "class") <- c("mc.irf", "mc.irf.BSVAR")
attr(output, "eqnames") <- attr(varobj, "eqnames")
return(output)
}

######################################
# Helper functions for mc.irf.MSBVAR
######################################

# Get an A0 -- function to get the reduced form VAR A(0) to capture
# the contemp effects.

getA0 <- function(z, m, h)
{
  tmp <- apply(matrix(z, ncol=h), 2, xpnd)
  tmp <- array(tmp, c(m,m,h))
  tmp <- array(apply(tmp, 3, chol), c(m,m,h))
  aperm(tmp, c(2,1,3))
}




"irf.var.from.beta" <-
  function(A0,bvec,nsteps)
  {   m <- ncol(A0)
  p <- (length(bvec))/m^2;
  bmtx <- matrix(bvec,ncol=m)
  ar.coefs<-t(bmtx)     # extract the ar coefficients
  dim(ar.coefs)<-c(m,m,p)              # push ar coefs into M x M x P array
  ar.coefs<-aperm(ar.coefs,c(2,1,3))   # reorder array so columns
  # are for eqn
  impulses <- irf.VAR(list(ar.coefs=ar.coefs),nsteps,A0=A0)$mhat
  impulses <- aperm(impulses, c(3,1,2)) # flips around the responses
  # to stack them as series
  # for each response, so the
  # first nstep elements are
  # the responses to the first
  # shock, the second are the
  # responses of the first
  # variable to the next
  # shock, etc.
  
  dim(impulses)<-c((m^2)*nsteps,1)
  return(impulses)
  }


"mc.irf.MSBVAR" <- function(varobj, nsteps, draws)
{
  # Get constants
  m <- varobj$m
  p <- varobj$p
  h <- varobj$h
  N2 <- draws
  
  # Get the A0 or structural shocks
  Sigmavec <- varobj$Sigma.sample
  
  cat("Running setup tasks to sort regime parameters.\n")
  
  # Set them up in regime specific arrays
  A0s <- array(sapply(1:N2, function(i) {getA0(Sigmavec[i,], m, h)}),
               c(m, m, h, N2))
  
  # Now set up the AR coefficients vector.  Need to pluck out the
  # intercepts from the posterior and assemble the AR coefs in a vector
  # for each regime
  
  # Intercept indices
  mpplus1 <- m*p + 1
  nc <- m*mpplus1
  ii <- seq(mpplus1, by=mpplus1, length=m)  # Intercept indices
  
  # Split Beta by regimes
  
  Beta <- array(varobj$Beta.sample, c(N2, nc, h))
  
  # Remove the intercepts -- just the AR part
  AR <- Beta[,-ii,]
  rm(Beta)
  
  cat("Computing long run regime probabilities.\n")
  # Get summaries of the long run ergodic probabiities
  lrQ <- sapply(1:N2,
                function(i) {
                  steady.Q(matrix(varobj$Q.sample[i,],h,h))})
  
  # Storage for the impulse responses: draws, steps, response-shock
  # combinations, regimes array
  
  tmp <- array(0, c(N2, nsteps, m^2, h))
  outputavg <- array(0, c(N2, nsteps, m^2))
  
  # Loop over the draws and regimes to compute the IRF for each
  # regime and in equilibrium
  
  # Computing the IRF for each regime
  cat("Beginning to compute the IRFs for each regime, and averaged over the regime probabilities.\n")
  for(i in 1:N2)
  {
    for(j in 1:h)
    {
      tmp[i,,,j] <- irf.var.from.beta(A0s[,,j,i], AR[i,,j], nsteps)
      
      # Now take long run averages and store them
      outputavg[i,,] <- tmp[i,,,j] + tmp[i,,,j]*lrQ[j,i]
    }
    if(i%%1000==0) cat("Finished ", 100*i/N2, " percent\n", sep="")
  }
  
  output <- list(shortrun=tmp, longrun=outputavg)
  attr(output, "class") <- c("mc.irf", "mc.irf.MSBVAR")
  attr(output, "eqnames") <- attr(varobj, "eqnames")
  return(output)
}

######################################################################
# Main classes plotting function for impulse responses computed with
# mc.irf and it classes cousins.
######################################################################

"plot.mc.irf" <- function(x, method=c("Sims-Zha2"), component=1,
                          probs=c(0.16,0.84),
                          varnames=attr(x, "eqnames"),
                          regimelabels=NULL, ask=TRUE, ...)
{
  # VAR
  if(inherits(x, "mc.irf.VAR"))
  { tmp <- plot.mc.irf.VAR(x, method=method, component=component,
                           probs=probs, varnames=varnames,...) }
  # BVAR
  if(inherits(x, "mc.irf.BVAR"))
  { tmp <- plot.mc.irf.BVAR(x, method=method, component=component,
                            probs=probs, varnames=varnames,...) }
  # BSVAR
  if(inherits(x, "mc.irf.BSVAR"))
  { tmp <- plot.mc.irf.BSVAR(x, method=method, component=component,
                             probs=probs, varnames=varnames,...) }
  # MSBVAR
  if(inherits(x, "mc.irf.MSBVAR"))
  { tmp <- plot.mc.irf.MSBVAR(x, method=method,
                              component=component, probs=probs,
                              varnames=varnames,
                              regimelabels=regimelabels, ask,...) }
  # Get the eigen-fractions, if relevant and available.
  return(invisible(tmp))
}

# Use a single function to compute the IRF across each kind of VAR.
# Then the computation of the IRF can be separated from the plotting
# functions for more user control.  This is hidden from the user, but
# called inside of the various functions needed to draw the IRF plots
# with error bands.

"compute.plot.mc.irf" <- function(x, method, component, probs)
{
  
  mc.impulse <- x
  m <- sqrt(dim(mc.impulse)[3])
  nsteps <- dim(mc.impulse)[2]
  draws <- dim(mc.impulse)[1]
  
  # Storage
  irf.ci <- array(0,c(nsteps,length(probs)+1,m^2))
  
  # Compute the IRF confidence intervals for each element of MxM
  # array
  
  # There are multiple methods for doing this.
  
  # Monte Carlo / Bootstrap percentile method
  if (method=="Percentile")
  {
    eigen.sum <- 0
    for(i in 1:m^2)
    {
      irf.bands <- t(apply(mc.impulse[,,i], 2, quantile, probs))
      irf.mean <- apply(mc.impulse[,,i], 2, mean)
      irf.ci[,,i] <- cbind(irf.bands, irf.mean)
    }
  }
  if (method=="Normal Approximation")
  {
    eigen.sum <- 0
    for (i in 1:m^2)
    {
      irf.mean <- apply(mc.impulse[,,i], 2, mean)
      irf.var <- apply(mc.impulse[,,i], 2, var)
      irf.bands <- irf.mean + matrix(rep(qnorm(probs), each=nsteps), nrow=nsteps)*irf.var
      irf.ci[,,i] <- cbind(irf.bands, irf.mean)
    }
  }
  
  # Sims and Zha symmetric eigen decomposition (assumes normality
  # approximation) with no accounting for the correlation across the
  # responses.  Method does account for the correlation over time.
  
  if (method=="Sims-Zha1")
  {
    eigen.sum <- matrix(0, m^2, nsteps)
    for(i in 1:m^2)
    {
      decomp <- eigen(var(mc.impulse[,,i]), symmetric=T)
      W <- decomp$vectors
      lambda <- decomp$values
      irf.mean <- apply(mc.impulse[,,i], 2, mean)
      irf.bands <- irf.mean + W[,component]*matrix(rep(qnorm(probs), each=nsteps), nrow=nsteps)*sqrt(lambda[component])
      irf.ci[,,i] <- cbind(irf.bands, irf.mean)
      eigen.sum[i,] <- 100*lambda/sum(lambda)
    }
  }
  
  # Sims and Zha asymmetric eigen decomposition (no normality
  # assumption) with no accounting for the correlation across the
  # responses.  Method does account for correlation over time.
  if (method=="Sims-Zha2")
  {
    eigen.sum <- matrix(0, m^2, nsteps)
    for(i in 1:m^2)
    {
      decomp <- eigen(var(mc.impulse[,,i]), symmetric=T)
      W <- decomp$vectors
      lambda <- decomp$values
      gammak <- mc.impulse[,,i]*(W[component,])
      gammak.quantiles <- t(apply(gammak, 2, quantile, probs=probs))
      irf.mean <- apply(mc.impulse[,,i], 2, mean)
      irf.bands <- irf.mean + gammak.quantiles
      irf.ci[,,i] <- cbind(irf.bands, irf.mean)
      eigen.sum[i,] <- 100*lambda/sum(lambda)
    }
  }
  
  # Sims and Zha asymmetric eigen decomposition with no normality
  # assumption and an accounting of the temporal and cross response
  # correlations.
  
  if (method=="Sims-Zha3")
  {
    eigen.sum <- matrix(0, m^2, m^2*nsteps)
    
    # Stack all responses and compute one eigen decomposition.
    stacked.irf <- array(mc.impulse, c(draws, m^2*nsteps))
    decomp <- eigen(var(stacked.irf), symmetric=T)
    W <- decomp$vectors
    lambda <- decomp$values
    gammak <- stacked.irf*W[component,]
    gammak.quantiles <- apply(gammak, 2, quantile, probs)
    irf.mean <- matrix(apply(stacked.irf, 2, mean),
                       nrow=length(probs),
                       ncol=dim(stacked.irf)[2],
                       byrow=T)
    irf.bands <- irf.mean + gammak.quantiles
    
    # Reshape these....
    irf.ci <- array((rbind(irf.bands,irf.mean[1,])),
                    c(length(probs)+1, nsteps, m^2))
    irf.ci <- aperm(irf.ci, c(2, 1, 3))
    eigen.sum <- 100*lambda/sum(lambda)
  }
  return(list(responses=irf.ci, eigenvector.fractions=eigen.sum,
              m=m, nsteps=nsteps))
}

"plot.mc.irf.VAR" <- function(x, method=method, component=component,
                              probs=probs, varnames=varnames,...)
{
  tmp <- compute.plot.mc.irf(x, method, component, probs)
  
  # Get out the main components we need
  m <- tmp$m
  nsteps <- tmp$nsteps
  irf.ci <- tmp$responses
  eigen.sum <- tmp$eigenvector.fractions
  
  # Compute the bounds for the plots
  minmax <- matrix(0, nrow=m, ncol=2)
  within.plots <- apply(irf.ci, 3, range)
  
  tmp <- (c(1:m^2)%%m)
  tmp[tmp==0] <- m
  indices <- sort(tmp, index.return=T)$ix
  dim(indices) <- c(m, m)
  
  for(i in 1:m){minmax[i,] <- range(within.plots[,indices[,i]])}
  # Now loop over each m columns to find the minmax for each column
  # responses in the MAR plot.
  j <- 1
  # Plot the results
  par(mfcol=c(m,m), mai=c(0.25,0.25,0.15,0.25),
      omi=c(0.15,0.75,1,0.15))
  
  for(i in 1:m^2)
  {
    lims <- ifelse((i-m)%%m==0, m, (i-m)%%m)
    ts.plot(irf.ci[,,i],
            gpars=list(xlab="",ylab="",ylim=minmax[lims,]), ...)
    abline(h=0)
    
    if(i<=m){ mtext(varnames[i], side=2, line=3)}
    if((i-1)%%m==0){
      mtext(varnames[j], side=3, line=2)
      j <- j+1
    }
  }
  
  mtext("Response in", side=2, line=3, outer=T)
  mtext("Shock to", side=3, line=3, outer=T)
  
  # Put response names on the eigenvector fractions
  if(method == "Sims-Zha1" | method == "Sims-Zha2")
  {
    if(is.null(varnames)==T) varnames <- paste("V", seq(1:m), sep = "")
    shock.name <- rep(varnames, m)
    response.name <- rep(varnames, each=m)
    eigen.sum <- cbind(shock.name, response.name, as.data.frame(eigen.sum))
    colnames(eigen.sum) <- c("Shock","Response", paste("Component", seq(1:nsteps)))
  }
  if(method == "Sims-Zha3")
  { names(eigen.sum) <- c(paste("Component", seq(1:m^2*nsteps))) }
  
  # Return
  return(list(responses=irf.ci, eigenvector.fractions=eigen.sum))
}

"plot.mc.irf.BVAR" <- function(x, method=method, component=component,
                               probs=probs, varnames=varnames, ...)
{
  plot.mc.irf.VAR(x, method, component,
                  probs, varnames, ...)
}


# BSVAR model IRFs

"plot.mc.irf.BSVAR" <- function(x, method=method, component=component,
                                probs=probs, varnames=varnames, ...)
{
  m <- sqrt(dim(x)[3])
  nsteps <- dim(x)[2]
  draws <- dim(x)[1]
  irf.ci <- array(0, c(nsteps, length(probs) + 1, m^2))
  if (method == "Percentile") {
    eigen.sum <- 0
    for (i in 1:m^2) {
      irf.bands <- t(apply(x[, , i], 2, quantile,
                           probs))
      irf.mean <- apply(x[, , i], 2, mean)
      irf.ci[, , i] <- cbind(irf.bands, irf.mean)
    }
  }
  if (method == "Normal Approximation") {
    eigen.sum <- 0
    for (i in 1:m^2) {
      irf.mean <- apply(x[, , i], 2, mean)
      irf.var <- apply(x[, , i], 2, var)
      irf.bands <- irf.mean + matrix(rep(qnorm(probs),
                                         each = nsteps), nrow = nsteps) * irf.var
      irf.ci[, , i] <- cbind(irf.bands, irf.mean)
    }
  }
  if (method == "Sims-Zha1") {
    eigen.sum <- matrix(0, m^2, nsteps)
    for (i in 1:m^2) {
      decomp <- eigen(var(x[, , i]), symmetric = T)
      W <- decomp$vectors
      lambda <- decomp$values
      irf.mean <- apply(x[, , i], 2, mean)
      irf.bands <- irf.mean + W[, component] * matrix(rep(qnorm(probs),
                                                          each = nsteps), nrow = nsteps) * sqrt(lambda[component])
      irf.ci[, , i] <- cbind(irf.bands, irf.mean)
      eigen.sum[i, ] <- 100 * lambda/sum(lambda)
    }
  }
  if (method == "Sims-Zha2") {
    eigen.sum <- matrix(0, m^2, nsteps)
    for (i in 1:m^2) {
      decomp <- eigen(var(x[, , i]), symmetric = T)
      W <- decomp$vectors
      lambda <- decomp$values
      gammak <- x[, , i] * (W[component, ])
      gammak.quantiles <- t(apply(gammak, 2, quantile,
                                  probs = probs))
      irf.mean <- apply(x[, , i], 2, mean)
      irf.bands <- irf.mean + gammak.quantiles
      irf.ci[, , i] <- cbind(irf.bands, irf.mean)
      eigen.sum[i, ] <- 100 * lambda/sum(lambda)
    }
  }
  if (method == "Sims-Zha3") {
    eigen.sum <- matrix(0, m^2, m^2 * nsteps)
    stacked.irf <- array(x, c(draws, m^2 * nsteps))
    decomp <- eigen(var(stacked.irf), symmetric = T)
    W <- decomp$vectors
    lambda <- decomp$values
    gammak <- stacked.irf * W[component, ]
    gammak.quantiles <- apply(gammak, 2, quantile, probs)
    irf.mean <- matrix(apply(stacked.irf, 2, mean), nrow = length(probs),
                       ncol = dim(stacked.irf)[2], byrow = T)
    irf.bands <- irf.mean + gammak.quantiles
    irf.ci <- array((rbind(irf.bands, irf.mean[1, ])), c(length(probs) +
                                                           1, nsteps, m^2))
    irf.ci <- aperm(irf.ci, c(2, 1, 3))
    eigen.sum <- 100 * lambda/sum(lambda)
  }
  
  minmax <- matrix(0, nrow = m, ncol = 2)
  within.plots <- apply(irf.ci, 3, range)
  
  tmp <- (c(1:m^2)%%m)
  tmp[tmp==0] <- m
  indices <- sort(tmp, index.return=T)$ix
  dim(indices) <- c(m, m)
  
  for(i in 1:m)
  { minmax[i,] <- range(within.plots[,indices[,i]])
  }
  
  # Now loop over each m columns to find the minmax for each column
  # responses in the MAR plot.
  j <- 1
  # Plot the results
  par(mfcol=c(m,m),mai=c(0.15,0.2,0.15,0.15), omi=c(0.15,0.75,1,0.15))
  for(i in 1:m^2)
  {
    lims <- ifelse((i-m)%%m==0, m, (i-m)%%m)
    ts.plot(irf.ci[,,i],
            gpars=list(xlab="",ylab="",ylim=minmax[lims,]))
    
    abline(h=0)
    
    if(i<=m)
    { mtext(varnames[i], side=2, line=3) }
    if((i-1)%%m==0)
    { mtext(varnames[j], side=3, line=2)
      j <- j+1
    }
  }
  
  # Add row and column labels for graph
  
  mtext("Response in", side = 2, line = 3, outer = T)
  mtext("Shock to", side = 3, line = 3, outer = T)
  
  # Return eigendecomposition pieces if necessary
  
  if (method == "Sims-Zha1" | method == "Sims-Zha2") {
    if (is.null(varnames) == T)
      varnames <- paste("V", seq(1:m), sep = "")
    shock.name <- rep(varnames, m)
    response.name <- rep(varnames, each = m)
    eigen.sum <- cbind(shock.name, response.name, as.data.frame(eigen.sum))
    colnames(eigen.sum) <- c("Shock", "Response", paste("Component",
                                                        seq(1:nsteps)))
  }
  if (method == "Sims-Zha3") {
    names(eigen.sum) <- c(paste("Component", seq(1:m^2 *
                                                   nsteps)))
  }
  
  return(list(responses = irf.ci, eigenvector.fractions = eigen.sum))
}


# MSBVAR IRF plot
"plot.mc.irf.MSBVAR" <- function(x, method=method,
                                 component=component, probs=probs,
                                 varnames=varnames,
                                 regimelabels=regimelabels, ask=ask, ...)
{
  # Subset out the irfs by the number of regimes and
  # variables. Recall that the dimensions of the irf array in "x"
  # are N2 x nsteps x (shock*response) x no. regimes.
  
  # Start by plotting each of the shortrun plots
  d <- dim(x$shortrun)
  h <- d[4]
  
  # Storage / list for the IRF output summaries
  out <- vector(mode="list", length=(h+1))
  
  # Make regime labels if they have not been provided
  if(is.null(regimelabels)) regimelabels <- paste("Regime", 1:h)
  
  # Main loop for the plot.
  for (i in 1:h)
  {
    # plot each short run irf, per regime, and a label as
    # such.
    
    # Store the summary / CI / eigendecomposition if it is done.
    out[[i]] <- plot.mc.irf.VAR(x=x$shortrun[,,,i], method=method,
                                component=component,
                                probs=probs, varnames=varnames, ...)
    # Add regime label to plot
    mtext(regimelabels[i], side=3, line=4.5, outer=T)
    
    devAskNewPage(ask=ask)
  }
  
  # Now plot the long-run of regime probability averaged IRF, with
  # error bands
  
  tmp <- plot.mc.irf.VAR(x$longrun, method=method, component=component,
                         probs=probs, varnames=varnames)
  mtext("Regime averaged IRF", side=3, line=4.5, outer=TRUE)
  
  out[[h+1]] <- tmp
  
  # Add regime names to output
  names(out) <- c(regimelabels, "Regime averaged")
  
  return(out)
}

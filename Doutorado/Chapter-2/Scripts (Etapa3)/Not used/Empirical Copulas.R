empiricalCopulaValuesMetric=function(XwhereToEvaluteEmpiricalCopula,smoothingEmpiricalCopula=c("none"))
{
  library(copula)
  X1=P #Using all data, also when considering an ensem,bel only
  X2=W #Using all data, also when considering an ensem,bel only
  X=cbind(X1,X2)
  #pairs.panels(X)
  #U <- pobs(X)
  #pairs.panels(U)    XwhereToEvaluteEmpiricalCopula=XwhereToEvaluteEmpiricalCopula    U=X*NA
  for(iVar in 1:dim(U)[2])
  {
    U[,iVar]=ecdf(X[,iVar])(X[,iVar])
    #hist(ecdf(X[,1])(X[,1]))
  }    # Compute U values associated with the ecdf of all data defined above.
  if(is.null(dim(XwhereToEvaluteEmpiricalCopula)))
  {
    Uobs=XwhereToEvaluteEmpiricalCopula*NA
    for(iVar in 1:length(Uobs))
    {
      Uobs[iVar]=ecdf(X[,iVar])(XwhereToEvaluteEmpiricalCopula[iVar])
    }
  }
  else {
    Uobs=XwhereToEvaluteEmpiricalCopula*NA
    for(iVar in 1:dim(Uobs)[2])
    {
      Uobs[,iVar]=ecdf(X[,iVar])(XwhereToEvaluteEmpiricalCopula[,iVar])
    }
  }    ECvalues=F.n(Uobs,
                    U,#X=pobs(X), #empircal copula cunction fitted to these data
                    smoothing = smoothingEmpiricalCopula)#c("none"))#, "beta", "checkerboard"))
  #
  return(ECvalues)
}
ECvalues=empiricalCopulaValuesMetric(XwhereToEvaluteEmpiricalCopula=cbind(P,W))
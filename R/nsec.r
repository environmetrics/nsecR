
nsec<-function(fit,mods=NULL,control.conc=0,sig=0.05){
  options(warn=-1)
  if( !inherits(fit,"drc")) stop("supplied model fit is not from package drc")
  if(is.null(mods)){
      mods<-list(eval(fit$call$fct))
      message("no alternative models specified - computing NSEC for current model fit")
  } else {
      message("computing model-averaged NSEC (maNSEC) for current fit + alternative models")
  }
 y.lwr<-stats::predict(fit,newdata=data.frame(control.conc),level=1-2*sig,interval="confidence")
 n.sec<-invisible(drc::maED(fit,respLev = y.lwr[2],type="absolute",
                  interval="none",fctList =mods,display=TRUE ))
 attributes(n.sec)<-NULL
 return(n.sec)
 options(warn=0)
}




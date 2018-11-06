#' correlation de Pearson
#' @export r_pearson
#' @param V1 premier facteur
#' @param V2 deuxieme facteur
#' @param alpha risque alpha
#' @return Conclusion de test statistique

r_pearson=function(V1,V2,alpha=.05){
  res=cor(V1,V2)
  t=res*sqrt((length(V1)-2)/(1-res^2))
  p_val=(1-pt(abs(t),df=18))*2

  if (round(p_val)==0) {
    interpretation=paste("; p-val <",.001)
  } else if (round(p_val,3)==1) {
    interpretation=paste("; p-val >",.999)
  } else {interpretation=paste("; p-val =",round(p_val,3))}

  if(p_val<=alpha){
    decision="$-->RH_{0}$"
  } else {decision="$-->NRH_{0}$"}

  ccl=paste0("$r$ = ",round(res,3),interpretation,decision)

    return(ccl)
}

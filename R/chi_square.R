#' chi square test
#' @export chi_square
#' @param VI1 premier facteur
#' @param VI2 deuxieme facteur
#' @param nb_fact nombre de facteur
#' @param alpha risque alpha
#' @param pr probabilites theoriques
#' @keywords chisquare, APA
#' @import stats
#' @importFrom questionr cramer.v
#' @return Conclusion de test statistique

chi_square=function(VI1,VI2,nb_fact,pr,alpha){
  if (nb_fact==2){
    res=chisq.test(VI1,VI2)
    ampl=cramer.v(table(VI1,VI2))
  } else if (nb_fact==1){
    res=chisq.test(table(VI1),p=pr)
  }

  if (round(res$p.value)==0) {
    interpretation=paste("; p-val <",.001)
  } else if (round(res$p.value,3)==1) {
    interpretation=paste("; p-val >",.999)
  } else {interpretation=paste("; p-val =",round(res$p.value,3))}

  if(res$p.value<=alpha){
    decision="$-->RH_{0}$"
  } else {decision="$-->NRH_{0}$"}

  if (nb_fact==2){
  ccl=paste0("X²(",res$parameter,")=",round(res$statistic,3),interpretation,decision,", V de cramer = ",round(ampl,3))
  } else {ccl=paste0("X²(",res$parameter,")=",round(res$statistic,3),interpretation,decision)}
  return(ccl)
}




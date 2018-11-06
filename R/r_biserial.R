#' correlation biserielle de points
#' @export r_biserial
#' @param V_dicho premier facteur
#' @param V_scale deuxieme facteur
#' @param alpha risque alpha
#' @importFrom ltm biserial.cor
#' @return Conclusion de test statistique

r_biserial=function(V_dicho,V_scale,alpha=.05){
  res=abs(biserial.cor(V_scale,V_dicho))
  p_val=t.test(V_scale~V_dicho)$p.value

  if (round(p_val)==0) {
    interpretation=paste("; p-val <",.001)
  } else if (round(p_val,3)==1) {
    interpretation=paste("; p-val >",.999)
  } else {interpretation=paste("; p-val =",round(p_val,3))}

  if(p_val<=alpha){
    decision="$-->RH_{0}$"
  } else {decision="$-->NRH_{0}$"}

  ccl=paste0("$r_{pb}$ = ",round(res,3),interpretation,decision)

  M=tapply(V_scale,V_dicho,mean)

  comparaison="est plus grande que"
  if (M[1]==M[2]){
    comparaison="est égale à"
  } else if  (M[1]>M[2]){
    comparaison="est plus grande que"
  } else {comparaison="est plus petite que"}
  Remarque=paste0("Remarque: la moyenne du groupe '",names(M)[1],"' (= ",round(M[1],3),") ",comparaison," la moyenne du groupe '",names(M)[2]," ' (= ",round(M[2],3),")")

    return(c(ccl,Remarque))
}

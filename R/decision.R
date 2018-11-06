#' RH0 ou NRH0?
#' @export decision
#' @param p p_valeur
#' @param alpha risque alpha
#' @keywords chisquare, APA
#' @return Conclusion de test statistique

decision=function(p,alpha=.05){
  if(p<=alpha){
  dec="$-->RH_{0}$"
  } else {dec="$-->NRH_{0}$"}
  return(dec)
}


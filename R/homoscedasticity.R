#' Creer un dataframe qui exclut les outliers d'une base de donnees
#' @export homoscedasticity
#' @param VD la VD de la base de donnees
#' @param VI1 VI1 de la bdd
#' @param VI2 VI2 de la bdd
#' @param nb_fact nombre de facteurs: 0, 1 ou 2
#' @param alpha risque alpha
#' @keywords homoscedasticite, APA
#' @importFrom car leveneTest
#' @return resultat du test de Levene, comme dans SPSS, centre autour de la moyenne, aux normes APA

homoscedasticity=function(VD,VI1,VI2,nb_fact,alpha){
  if (nb_fact==1){
    test=leveneTest(VD~as.factor(VI1),center="mean")
  } else if (nb_fact==2){
    test=leveneTest(VD~as.factor(VI1)*as.factor(VI2),center="mean")
  }

  if (test$`Pr(>F)`[1]>=.200){
    interpretation=paste("; p-val >=",.200)
  } else if (round(test$`Pr(>F)`[1],3)==0) {
    interpretation=paste("; p-val <",.001)
  } else {interpretation=paste("; p-val =",round(test$`Pr(>F)`[1],3))}

  if(test$`Pr(>F)`[1]<=alpha){
    decision="$-->RH_{0}$"
  } else {decision="$-->NRH_{0}$"}

  APA=paste0("F(",test$Df[1],",",test$Df[2],")=",round(test$`F value`[1],3),interpretation,decision)
  return(APA)
}


#' Creer un dataframe qui exclut les outliers d'une base de donnees
#' @export normality
#' @param VD la VD de la base de donnees
#' @param VI1 VI1 de la bdd
#' @param VI2 VI2 de la bdd
#' @param nb_fact nombre de facteurs: 0, 1 ou 2
#' @param alpha risque alpha
#' @keywords Outliers Boxplot
#' @importFrom nortest lillie.test
#' @return resultat du test de K-S, comme dans SPSS, aux normes APA

normality=function(VD,VI1,VI2,nb_fact,pred,alpha){

  if (nb_fact==0){
    regr=summary(lm(VD~pred))
    test=lillie.test(regr$residuals)
    if (test[[2]]>=.200){
      interpretation=paste(">=",.200)
    } else if (round(test[[2]],3)==0) {
      interpretation=paste("<",.001)
    } else {interpretation=paste("=",round(test[[2]],3))}

    if(test[[2]]<=alpha){
      decision="$-->RH_{0}$"
    } else {decision="$-->NRH_{0}$"}
    ccl=paste0("D(",length(VD),")=",round(test$statistic,3),"; p-val",interpretation,decision)
  } else if (nb_fact==1){
    Splitted_data=split(VD, f=VI1)
    J=length(table(VI1))
    ccl=list()
    for (j in 1:J){
      test=lillie.test(Splitted_data[[j]])
      if (test[[2]]>=.200){
        interpretation=paste(">=",.200)
      } else if (round(test[[2]],3)==0) {
        interpretation=paste("<",.001)
      } else {interpretation=paste("=",round(test[[2]],3))}

      if(test[[2]]<=alpha){
        decision="$-->RH_{0}$"
      } else {decision="$-->NRH_{0}$"}
      ccl[[j]]=paste0("Pour le groupe '",names(table(VI1))[j],"': D(",length(Splitted_data[[j]]),")=",round(test$statistic,3),"; p-val",interpretation,decision)
    }

  } else if (nb_fact==2){
    Splitted_data=split(VD, f=list(VI1,VI2))
    J=length(table(VI1,VI2))
    ccl=list()
    for (j in 1:J){
      test=lillie.test(Splitted_data[[j]])
      if (test[[2]]>=.200){
        interpretation=paste(">=",.200)
      } else if (round(test[[2]],3)==0) {
        interpretation=paste("<",.001)
      } else {interpretation=paste("=",round(test[[2]],3))}

      if(test[[2]]<=alpha){
        decision="$-->RH_{0}$"
      } else {decision="$-->NRH_{0}$"}
      ccl[[j]]=paste0("Pour le groupe '",names(Splitted_data)[j],"': D(",length(Splitted_data[[j]]),")=",round(test$statistic,3),"; p-val",interpretation,decision)
    }

  }
  return(ccl)
}



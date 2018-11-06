#' Creer un dataframe qui exclut les outliers d'une base de donnees
#' @export ANOVA_posthoc
#' @param VD la VD de la base de donnees
#' @param VI1 VI1 de la bdd a nettoyer
#' @param VI2 VI2 de la bdd a nettoyer
#' @param data dataframe
#' @param nb_fact nombre de facteurs: 0, 1 ou 2
#' @param alpha risque alpha
#' @param effect VI1, VI2 or VI1*VI2
#' @keywords homoscedasticite, APA
#' @return resultats du test ANOVA, comme dans SPSS, aux normes APA

  ANOVA_posthoc=function(VD,VI1,VI2,data,nb_fact,alpha,effect){
  if (nb_fact==1){
  test=""
  } else if (nb_fact==2){
  ### Realisation du test (effet principal SS type III, comme dans SPSS)
  # d'abord changer le type de contrastes, de sorte a avoir les memes resultats que dans SPSS
  options(contrasts = c("contr.helmert", "contr.poly"))
  options("contrasts")
  test=Anova(lm(VD ~ VI1 * VI2, data=data),type=3)
  df2=test[5,]$Df
  if(round(test[2,]$`Pr(>F)`,3)<=alpha){
    decision="$-->RH_{0}$"
  } else {decision="$-->NRH_{0}$"}

   if(effect=="VI1"){
      F_stat=test[2,]$`F value`
      p_val=test[2,]$`Pr(>F)`
      facteur=VI1
      df1=test[2,]$Df
   }else if (effect=="VI2"){
     F_stat=test[3,]$`F value`
     p_val=test[3,]$`Pr(>F)`
     facteur=VI2
     df1=test[3,]$Df
   }else if(effect=="VI1*VI2"){
     F_stat=test[4,]$`F value`
     p_val=test[4,]$`Pr(>F)`
     facteur="Interaction"
     df1=test[4,]$Df
   }

  name=NULL
  for (j in 1:length(table(facteur))){
    if (j==length(table(facteur))){
      name=paste(name,names(table(facteur))[j])
    } else {name=paste(name,names(table(facteur))[j],"vs.")}
  }

  if (round(p_val,3)==0) {
    interpretation=paste("; p-val <",.001)
  } else if (round(p_val,3)==1){
    interpretation=paste("; p-val >",.999)
  } else {interpretation=paste("; p-val =",round(p_val,3))}

    if(p_val<=alpha){
      decision="$-->RH_{0}$"
    } else {decision="$-->NRH_{0}$"}

  ccl=paste0("Conclusion liee au facteur '",name,"': F(",df1,",",df2,")=",round(F_stat,3),interpretation,decision)

  }

  return(ccl)

  }


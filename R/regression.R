#' Creer un dataframe qui exclut les outliers d'une base de donnees
#' @export regression
#' @param VD la VD de la base de donnees
#' @param pred VI1 de la bdd a nettoyer
#' @param data dataframe
#' @param nb_fact nombre de facteurs: 0, 1 ou 2
#' @param alpha risque alpha
#' @param effect VI1, VI2 or VI1*VI2
#' @keywords homoscedasticite, APA
#' @importFrom multicon tContrast
#' @return resultats du test ANOVA, comme dans SPSS, aux normes APA

regression=function(VD,pred,alpha){

  # realiser le test du modèle global
  glob=Anova(lm(VD~pred))
  df1_glob=glob$Df[1]
  df2_glob=glob$Df[2]
  F_stat_glob=glob$`F value`[1]
  pval_glob=glob$`Pr(>F)`[1]

  if (round(pval_glob,3)==0) {
    interpretation=paste("; p-val <",.001)
  } else if (round(pval_glob,3)==1){
    interpretation=paste("; p-val >",.999)
  } else {interpretation=paste("; p-val =",round(pval_glob,3))}
  if(pval_glob<=alpha){
    decision="$-->RH_{0}$"
  } else {decision="$-->NRH_{0}$"}

  ccl_glob=paste0("F(",df1_glob,",",df2_glob,")=",round(F_stat_glob,3),interpretation,decision)

  # equation des MC et MA
  regr=summary(lm(VD~pred))
  MC=paste0("Modèle compact : $y_{i}$ = ",round(mean(VD),3)," + $erreur_{ci}$")
  b0=regr$coefficients[1,1]

  bj=round(regr$coefficients[-1,1],3)
  predname=rownames(regr$coefficients)[-1]
  coeff_bj=NULL
  for (j in 1:length(bj)){
    if(bj[j]>=0) {coeff_bj=paste0(coeff_bj," + ",bj[j]," X ",predname[j])
    } else {coeff_bj=paste0(coeff_bj," - ",abs(bj[j])," X ",predname[j])}}

  MA=paste0("Modèle augmenté : $y_{i}$ = ",round(b0,3),coeff_bj," + $erreur_{ai}$")

  # conclusion pour chaque prédicteur
  regr=summary(lm(VD~pred))
  APA=NULL
  predname=NULL

  for (j in 1:nrow(regr$coefficients)){
        if (round(regr$coefficients[j,4],3)==0) {
          interpretation=paste("; p-val <",.001)
        } else if (round(regr$coefficients[j,4],3)==1){
         interpretation=paste("; p-val >",.999)
       } else {interpretation=paste("; p-val =",round(regr$coefficients[j,4],3))}
       if(round(regr$coefficients[j,4],3)<=alpha){
          decision="$-->RH_{0}$"
       } else {decision="$-->NRH_{0}$"}

    if (j==1){
      predname="intercept"
    } else {predname=colnames(data)[j-1]}

    APA[j]=paste0("Pour b",j-1,"(",predname,"), t(",df2_glob,")=",round(regr$coefficients[j,3],3),interpretation,decision)
    names(APA[j])=paste0("Pour b",j-1)
      }

  return(list(ANOVA=ccl_glob,MC=MC,MA=MA,t_coeff=APA))
}

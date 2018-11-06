#' Creer un dataframe qui exclut les outliers d'une base de donnees
#' @export ANOVA_contrastes
#' @param VD la VD de la base de donnees
#' @param VI1 VI1 de la bdd a nettoyer
#' @param VI2 VI2 de la bdd a nettoyer
#' @param data dataframe
#' @param nb_fact nombre de facteurs: 0, 1 ou 2
#' @param alpha risque alpha
#' @param effect VI1, VI2 or VI1*VI2
#' @keywords homoscedasticite, APA
#' @importFrom multicon tContrast
#' @importFrom car leveneTest
#' @return resultats du test ANOVA, comme dans SPSS, aux normes APA

ANOVA_contrastes=function(VD,VI1,VI2,data,nb_fact,alpha,contrastes,lateralite_hyp){
  if (nb_fact==1){
    PL=leveneTest(VD~as.factor(VI1),center="mean")$`Pr(>F)`[1]
    k=length(table(VI1))
    CL=matrix(contrastes,nrow=k)
    res=NULL

    summ=round(tapply(VD,VI1,mean),3)
    summ=paste("Moyenne du groupe ",names(summ)," : ",summ[names(summ)])
    # Plot the mean of teeth length by dose groups
    lateralite=c(lateralite_hyp,rep("unequal",k-1)) # lat should be "less", "greater" or "unequal"
    if (PL >= alpha){
        ligne="Compte tenu du résultat au test de Levene (non significatif), on regarde la ligne 'Assume equal variances'"
        for (i in 1:(length(table(VI1))-1)){
        C=tContrast(VD~VI1, wgt=CL[,i],EQVAR=TRUE,alternative=lateralite[i])

        if (round(C[[2]][4],3)>=.200){
          interpretation=paste("; p-val >=",.200)
        } else if (round(C[[2]][4],3)==0) {
          interpretation=paste("; p-val <",.001)
        } else {interpretation=paste("; p-val =",round(C[[2]][4],3))}

        if(round(C[[2]][4],3)<=alpha){
          decision="$-->RH_{0}$"
        } else {decision="$-->NRH_{0}$"}

        res[i]=paste0("Pour le contraste ",i,", t(",C[[2]][2],") = ",round(C[[2]][1],3),interpretation,decision)}
        rem=paste0("Remarque : pour le C1, la p-valeur fournie par SPSS en bilatéral valait ",round(tContrast(VD~VI1, wgt=CL[,1],EQVAR=TRUE,alternative="unequal")[[2]][4],3))

    } else {
      ligne="Compte tenu du résultat au test de Levene (significatif), on regarde la ligne 'Does not assume equal variances'"
      for (i in 1:(length(table(VI1))-1)){
        C=tContrast(VD~VI1, wgt=CL[,i],EQVAR=FALSE)

        if (round(C[[2]][4],3)>=.200){
          interpretation=paste("; p-val >=",.200)
        } else if (round(C[[2]][4],3)==0) {
          interpretation=paste("; p-val <",.001)
        } else {interpretation=paste("; p-val =",round(C[[2]][4],3))}

        if(round(C[[2]][4],3)<=alpha){
          decision="$-->RH_{0}$"
        } else {decision="$-->NRH_{0}$"}

        res[i]=paste0("Pour le contraste ",i," , t(",round(C[[2]][2],3),") = ",round(C[[2]][1],3),interpretation,decision)}
        rem=paste0("Remarque : pour le C1, la p-valeur fournie par SPSS en bilatéral valait ",round(tContrast(VD~VI1, wgt=CL[,1],EQVAR=FALSE,alternative="unequal")[[2]][4],3))
      }
        # statistic = sum(m_j*Contraste_j)/sqrt(sum(var_j*Contraste_j^2/n_j))
        # where sqrt(sum(var_j*Contraste_j^2/n_j)) = standard error
  } else if (nb_fact==2){
    res="to do"
  }

  return(c(ligne,res,summ,rem))

}

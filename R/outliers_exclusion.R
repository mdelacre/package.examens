#' Creer un dataframe qui exclut les outliers d'une base de donnees
#' @export outliers_exclusion
#' @param data base de donnees que l'on veut nettoyer
#' @param VD la VD de la base de donnees contenant d'eventuels outliers
#' @param VI1 VI1 de la bdd a nettoyer
#' @param VI2 VI2 de la bdd a nettoyer
#' @param nb_fact nombre de facteurs catégoriels: 0, 1 ou 2
#' @param pred prédicteurs continus
#' @keywords Outliers Boxplot
#' @return outliers id, une bdd nettoyee ainsi qu'une conclusion en francais

outliers_exclusion=function(data,VD,VI1,VI2,pred,nb_fact=0){
  if(nb_fact==2){
    outliers=Boxplot(VD~VI1*VI2,data)
    if(length(outliers)==0){
      data_without_outliers = data
      ccl="Il n'y a pas d'outlier a supprimer"
    } else {
      data_without_outliers = data.frame(data[-as.numeric(outliers),])
      ccl=paste("Il y a",length(outliers),"outliers a supprimer, a savoir les observations portant les id",outliers)
    }
  } else if (nb_fact==1){
    outliers=Boxplot(VD~VI1,data=data)
    if(length(outliers)==0){
      data_without_outliers = data
      ccl="Il n'y a pas d'outlier a supprimer"
    } else {
      data_without_outliers = data[-as.numeric(outliers),]
      ccl=paste("Il y a",length(outliers),"outliers a supprimer, a savoir les observations portant les id",outliers)
    }
  }  else if (nb_fact==0){

    # realiser la regression et calculer les residus
    regr=summary(lm(VD~pred))$residuals
    outliers=Boxplot(regr,data=data)

    # Exclure les residus extremes

    if(length(outliers)==0){
      data_without_outliers = data
      ccl="Il n'y a pas d'outlier a supprimer"
    } else {
      data_without_outliers = data[-as.numeric(outliers),]
      ccl=paste("Il y a",length(outliers),"outliers a supprimer, a savoir les observations portant les id",outliers)
    }
  }
  return(list(outliers_id=outliers,clean_data=data_without_outliers,ccl=ccl))
}


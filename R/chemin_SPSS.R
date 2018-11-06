#' Chemin SPSS
#' @export chemin_SPSS
#' @param test le test pour lequel le chemin doit etre imprime:
#' Voici tous les tests encodes:
#' r_pearson,chi_indep,phi,v_cramer,rho_spearman,tau_kendall,biseriel_points,
#' t_indep,anova_posthoc,CL,t_indep,anova_facto,
#' chi_ajust,U_MW,KW,
#' regr_simple,regr_multiple,
#' normality_test,homoscedasticity_test
#' @keywords SPSS Chemin
#' @return retourne le chemin SPSS, en francais et en anglais


chemin_SPSS=function(test){
  chemin=nomdetest(test)
  chemin_agls=rep(NULL,length(test))
  chemin_frs=rep(NULL,length(test))

  for (i in 1:length(chemin)){
    if (test[i]=="r_pearson"|test[i]=="rho_spearman"|test[i]=="tau_kendall"|test[i]=="biseriel_points"){
      chemin_agls[i]="Analyze > Correlate > Bivariate..."
      chemin_frs[i]="Analyse > Correlation > Bivariee..."
    } else if (test[i]=="chi_indep"|test[i]=="phi"|test[i]=="v_cramer"){
      chemin_agls[i]="Analyze > Descriptive Statistics > Crosstabs..."
      chemin_frs[i]="Analyse > Statistiques descriptives > Tableaux croises"
    } else if (test[i]=="chi_ajust"){
      chemin_agls[i]="Analyze > Nonparametric Tests > Legacy Dialogs > Chi-square..."
      chemin_frs[i]="Analyse > Tests non parametriques > Boites de dialogue ancienne version > Khi-carré..."
    } else if (test[i]=="t_indep"){
      chemin_agls[i]="Analyze > Compare Means > Independent-Samples T Test... **OU** Analyze > Compare Means > One-Way ANOVA..."
      chemin_frs[i]="Analyse > Comparer les moyennes > Test T pour echantillons independants **OU** Analyse > Comparer les moyennes > ANOVA a 1 facteur..."
    } else if (test[i]=="anova_posthoc"|test[i]=="CL"){
      chemin_agls[i]="Analyze > Compare Means > One-Way ANOVA... **OU** Analyze > General Linear Model > Univariate... **OU** Analyze > Regression > Linear..."
      chemin_frs[i]="Analyse > Comparer les moyennes > ANOVA a 1 facteur... **OU** Analyse > Modele lineaire general > Univarie... **OU** Analyse > Regression > Lineaire..."
    } else if (test[i]=="anova_facto"){
      chemin_agls[i]="Analyze > General Linear Model > Univariate... **OU** Analyze > Regression > Linear..."
      chemin_frs[i]="Analyse > Modele lineaire general > Univarie... **OU** Analyse > Regression > Lineaire..."
    } else if (test[i]=="regr_simple"|test[i]=="regr_multiple"){
      chemin_agls[i]="Analyze > Regression > Linear..."
      chemin_frs[i]="Analyse > Regression > Lineaire..."
    } else if (test[i]=="U_MW"){
      chemin_agls[i]="Analyze > Nonparametric Tests > Legacy Dialogs > 2 Independent Samples... **OU** Analyze > Nonparametric Tests > Legacy Dialogs > K Independent Samples..."
      chemin_frs[i]="Analyse > Tests non parametriques > Boites de dialogue ancienne version > 2 echantillons independants **OU** Analyse > Tests non parametriques > Boites de dialogue ancienne version > K echantillons independants"
    } else if (test[i]=="KW"){
      chemin_agls[i]="Analyze > Nonparametric Tests > Legacy Dialogs > K Independent Samples..."
      chemin_frs[i]="Analyse > Tests non parametriques > Boites de dialogue ancienne version > K echantillons independants"}

    rep1=paste0("Chemin en anglais: ",chemin_agls)
    rep2=paste0("Chemin en francais: ",chemin_frs)
    return(c(rep1,rep2))
  }
}



#' Creer un dataframe qui exclut les outliers d'une base de donnees
#' @export nomdetest
#' @param test test
#' @keywords chisquare, APA
#' @return resultats du test chi-carre, comme dans SPSS, aux normes APA

nomdetest=function(test){
## Coefficient de correlation
  nom=NULL
  if(test=="r_pearson"){
    nom="r de Pearson"
  } else if (test=="chi_indep") {
    nom="Chi-carré d'indépendance"
  } else if (test=="phi") {
    nom="Phi"
  } else if (test=="v_cramer"){
    nom="V de Cramer"
  } else if (test=="rho_spearman"){
    nom="Rho de Spearman"
  } else if (test=="tau_kendall"){
    nom="Tau de Kendall"
  } else if (test=="r_biseriel") {
    nom="r bisériel de points"
  } else if (test=="anova_posthoc") {
    nom="ANOVA à un facteur avec post-hocs"
  } else if (test=="CL") {
    nom="Contrastes linéaires"
  } else if (test=="t_indep") {
    nom="Test t pour echantillons indépendants"
  } else if (test=="chi_ajust") {
    nom="Chi-carré d'ajustement"
  } else if (test=="regr_simple") {
    nom="Régression linéaire simple avec prédicteur continu"
  } else if (test=="regr_multiple") {
    nom="Régression linéaire multiple avec predicteurs continus"
  } else if (test=="U_MW") {
    nom="U de Mann-Whitney"
  } else if (test=="KW") {
    nom="Kruskal-Wallis"
  } else if (test=="anova_facto") {
    nom="ANOVA factorielle"
  } else if (test=="normality_test") {
    nom="Test de normalité"
  } else if (test=="homoscedasticity_test") {
    nom="Test de Levene"}
return(nom)

}


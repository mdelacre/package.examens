#' Test de Kruskal-Wallis
#' @export KW_test
#' @param VD la VD de la base de donnees
#' @param VI1 VI1 de la bdd
#' @param data dataframe
#' @param alpha risque alpha
#' @keywords Kruskall-Wallis test, APA
#' @importFrom stats kruskal.test
#' @return resultats du test de Kr-Wallis, comme dans SPSS, aux normes APA

KW_test<-function(VD,VI1,data,alpha){
      VI1=as.factor(VI1)
      test=kruskal.test(VD ~ VI1, data = data)

    if (round(test$p.value,3)==0) {
      interpretation_KW=paste("; p-val <",.001)
    } else if (round(test$p.value,3)==1){
      interpretation_KW=paste("; p-val >",.999)
    } else {interpretation_KW=paste("; p-val =",round(test$p.value,3))}

    if(test$p.value<=alpha){
      decision_KW="$-->RH_{0}$"
    } else {decision_KW="$-->NRH_{0}$"}

    ccl=paste0("X²(",test$parameter,")=",round(test$statistic,3),interpretation_KW,decision_KW)

    combi=combn(names(table(VI1)),m=2,simplify=FALSE)

    stat_U=NULL
    p_val_U=NULL
    stat_z=NULL
    p_val_z=NULL

    ccl_UMWa=NULL
    ccl_UMWb=NULL
    ccl_UMWc=NULL
    ccl_UMWd=NULL

    for (j in 1:length(combi)){
      level1=combi[[j]][1]
      level2=combi[[j]][2]
      gr1=data[data$VI1==level1,]
      gr2=data[data$VI1==level2,]
      n1 <- length(gr1$VD)
      n2 <- length(gr2$VD)
      n <- n1+n2
      vd=c(gr1$VD,gr2$VD)
      Vi=c(gr1$VI1,gr2$VI1)
      r <- rank(vd)
      U1 <- n1*n2+n1*(n1+1)/2-sum(r[1:n1])
      tie <- table(r)
      U <- min(U1, n1*n2-U1) # U
      W<- max(U1, n1*n2-U1)
      if (U%%1!=0){
      p_val_exact=min((1-pwilcox(q = W, m = n1, n = n2)) * 2,(pwilcox(q = W+1, m = n1, n = n2)) * 2)
      } else {p_val_exact=min((1-pwilcox(q = W-1, m = n1, n = n2)) * 2,(pwilcox(q = W, m = n1, n = n2)) * 2)}

      V <- n1*n2*(n^3-n-sum(tie^3-tie))/12/(n^2-n) # variance ties considered
      E <- n1*n2/2 # Expected
      z <- (U-E)/sqrt(V)  # z-value
      P <- pnorm(abs(z), lower.tail=FALSE)*2

      stat_U[j]=U
      p_val_U[j]=round(p_val_exact,3)

      stat_z[j]=round(z,3)
      p_val_z[j]=round(P,3)
    }

    k=length(table(VI1))

    p_val_U_corr=k*(k-1)*p_val_U/2
    p_val_z_corr=k*(k-1)*p_val_z/2
    alpha_corr=round((2*alpha)/(k*(k-1)),3)

    interpretation_UMWa=NULL
    decision_UMWa=NULL
    interpretation_UMWb=NULL
    decision_UMWb=NULL
    interpretation_UMWc=NULL
    decision_UMWc=NULL
    interpretation_UMWd=NULL
    decision_UMWd=NULL

        for (j in 1:length(combi)){

      # Pour la statistique U
      if (round(p_val_U[j],3)==0) {
        interpretation_UMWa[j]=paste("; p-val <",.001)
      } else if (round(p_val_U[j],3)>=1){
        interpretation_UMWa[j]=paste("; p-val >",.999)
      } else {interpretation_UMWa[j]=paste("; p-val =",round(p_val_U[j],3))}
      if(p_val_U[j]<=alpha_corr){
        decision_UMWa[j]=paste0(" < ", alpha_corr,"$-->RH_{0}$")
      } else {decision_UMWa[j]=paste0(" > ", alpha_corr,"$-->NRH_{0}$")}

      if (round(p_val_U_corr[j],3)==0) {
        interpretation_UMWb[j]=paste("; p-val <",.001)
      } else if (round(p_val_U_corr[j],3)>=1){
        interpretation_UMWb[j]=paste("; p-val >",.999)
      } else {interpretation_UMWb[j]=paste("; p-val =",round(p_val_U_corr[j],3))}
      if(p_val_U_corr[j]<=alpha){
        decision_UMWb[j]=paste0(" < ", alpha,"$-->RH_{0}$")
      } else {decision_UMWb[j]=paste0(" > ", alpha,"$-->NRH_{0}$")}

      # Pour la statistique z
      if (round(p_val_z[j],3)==0) {
        interpretation_UMWc[j]=paste("; p-val <",.001)
      } else if (round(p_val_z[j],3)>=1){
        interpretation_UMWc[j]=paste("; p-val >",.999)
      } else {interpretation_UMWc[j]=paste("; p-val =",round(p_val_z[j],3))}
      if(p_val_z[j]<=alpha_corr){
        decision_UMWc[j]=paste0(" < ", alpha_corr,"$-->RH_{0}$")
      } else {decision_UMWc[j]=paste0(" > ", alpha_corr,"$-->NRH_{0}$")}


      if (round(p_val_z_corr[j],3)==0) {
        interpretation_UMWd[j]=paste("; p-val <",.001)
      } else if (round(p_val_z_corr[j],3)>=1){
        interpretation_UMWd[j]=paste("; p-val >",.999)
      } else {interpretation_UMWd[j]=paste("; p-val =",round(p_val_z_corr[j],3))}
      if(p_val_z_corr[j]<=alpha){
        decision_UMWd[j]=paste0(" < ", alpha,"$-->RH_{0}$")
      } else {decision_UMWd[j]=paste0(" > ", alpha,"$-->NRH_{0}$")}

         }

    ccl_UMWa=paste0("U=",stat_U,interpretation_UMWa,decision_UMWa)
    ccl_UMWb=paste0("U=",stat_U,interpretation_UMWb,decision_UMWb)
    ccl_UMWc=paste0("z=",round(stat_z,3),interpretation_UMWc,decision_UMWc)
    ccl_UMWd=paste0("z=",round(stat_z,3),interpretation_UMWd,decision_UMWd)

    #invisible(list(U=stat_U,exact_p=p_val_U,Z=stat_z,asympt_p=p_val_z,p_val_U_corr=p_val_U_corr,p_val_z_corr=p_val_z_corr))
    return(list(K_W=ccl,U_alpha_corr=ccl_UMWa,U_p_corr=ccl_UMWb,z_alpha_corr=ccl_UMWc,z_p_corr=ccl_UMWd))
}



























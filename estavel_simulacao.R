library(stabledist)

estimador_beta<-function(n,m){
  
  n<-n
  m<-m
  
  alpha<-c(1:20)*0.1
  beta<-c(1:10)*0.1
  
  med<-numeric(length(alpha))
  vari<-numeric(length(alpha))
  
  s<-matrix(0,nrow=n,ncol=m)
  freq<-matrix(0,nrow=1,ncol=m)
  F0_emp<-matrix(0,nrow=1,ncol=m)
  estbeta<-matrix(0,nrow=1,ncol=m)
  
  result_med<-matrix(ncol = 10, nrow = 20)
  result_var<-matrix(ncol = 10, nrow = 20)
  set.seed(011) 
  
  for (k in 1:length(beta)) {
    for(i in 1:length(alpha)){
      set.seed(011) 
      for(j in 1:m){
        s[,j]<-rstable(n,alpha[i],beta[k],1,0,1)
        freq[,j]<-length(subset(s[,j],s[,j]<=0))
        F0_emp[,j]<-freq[,j]/n # f.d. empírica no ponto 0
        estbeta[,j]<-tan(alpha[i]*pi*(0.5- F0_emp[,j]))/tan(pi*alpha[i]/2)
      }
      med[i]<-format(round(mean(estbeta),4), nsmall = 4)
      
      var<-matrix(0,nrow=1,ncol=m)
      for(j in 1:m)
      {var[,j]<-((estbeta[,j]-mean(estbeta)))^2/(m-1)}
      vari[i]<-format(round(sqrt(sum(var)),4), nsmall = 4)
      
    }
    result_med[,k]<-med
    result_var[,k]<-vari
  }
  return(list("media"=result_med,"variancia"=result_var))
  
}

result_1<-estimador_beta(500,100)

result_med_1<-as.data.frame(result_1$media)
result_var_1<-as.data.frame(result_1$variancia)

result_2<-estimador_beta(1000,100)

result_med_2<-as.data.frame(result_2$media)
result_var_2<-as.data.frame(result_2$variancia)

result_3<-estimador_beta(10000,100)

result_med_3<-as.data.frame(result_3$media)
result_var_3<-as.data.frame(result_3$variancia)

result_4<-estimador_beta(100000,100)

result_med_4<-as.data.frame(result_4$media)
result_var_4<-as.data.frame(result_4$variancia)


# result_5<-estimador_beta(1000000,100)
# result_med_5<-result_5$media
# result_var_5<-result_5$variancia

erro_relativo<-function(x){
  dta<-x

beta<-c(1:10)*0.1
result_erro<-matrix(ncol = 10, nrow = 20)

for (i in 1:length(beta)) {
  for (j in 1:20) {
    result_erro[j,i]<-format(round((abs(beta[i]-as.numeric(as.character(dta[j,i])))/abs(beta[i]))*100,4), nsmall = 4)
  }
}

return(as.data.frame(result_erro))
}

erro_med_1<-erro_relativo(result_med_1)
erro_med_2<-erro_relativo(result_med_2)
erro_med_3<-erro_relativo(result_med_3)
erro_med_4<-erro_relativo(result_med_4)




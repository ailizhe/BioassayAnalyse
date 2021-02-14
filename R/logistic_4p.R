logistic_4p<-function(AT,dose,rep,ys,yt){
  #verification
  #1
  if(length(dose)*rep!=length(ys)|length(dose)*rep!=length(yt)) return("input error");
  #2
  data_matrix<-matrix(c(dose,ys,yt),nrow=length(dose));
  local_interim<-which(is.na(data_matrix));
  if(rep==1&sum(local_interim)!=0) return("input error");
  #3
  data_interim1<-data_matrix[,2:(rep+1)];
  data_interim2<-data_matrix[,(rep+2):(2*rep+1)];
  sum_na1<-NA;length(sum_na1)<-length(dose);
  sum_na2<-NA;length(sum_na2)<-length(dose);
  for(i in 1:length(dose)){
    sum_na1[i]<-sum(is.na(data_interim1[i,]));
    sum_na2[i]<-sum(is.na(data_interim2[i,]));
    i<-i+1;
  }
  if(rep>1&sum(which(sum_na1==rep))>0) return("input error");
  if(rep>1&sum(which(sum_na2==rep))>0) return("input error");

  #missing fill
  for(i in 1:length(which(sum_na1>0)))
  { j<-which(is.na(data_interim1[which(sum_na1>0)[i],]));
    data_interim1[which(sum_na1>0)[i],j]<-mean(data_interim1[which(sum_na1>0)[i],-j]);
    i<i+1;
  }
  for(i in 1:length(which(sum_na2>0)))
  { j<-which(is.na(data_interim2[which(sum_na2>0)[i],]));
  data_interim2[which(sum_na2>0)[i],j]<-mean(data_interim2[which(sum_na2>0)[i],-j]);
  i<i+1;
  }

  #summary as matrix
  data_matrix<-matrix(c(dose,data_interim1,data_interim2),nrow=length(dose));
  rownames(data_matrix)<-c(1:length(dose));
  name1<-c("Standard1","Standard2","Standard3","Standard4","Standard5");
  name2<-c("Trial1","Trial2","Trial3","Trial4","Trial5");
  colnames(data_matrix)<-c("Dose",name1[1:rep],name2[1:rep]);

  #statistics
  ys_interim<-rowMeans(data_matrix[,1:rep+1]);
  yt_interim<-rowMeans(data_matrix[,1:rep+1+rep]);
  y_interim<-c(ys_interim,yt_interim);
  RSDs<-NA;RSDt<-NA;
  length(RSDs)<-length(dose);length(RSDt)<-length(dose);
  for(i in 1:length(dose)){
    RSDs[i]<-sd(data_matrix[i,1:rep+1])/ys_interim[i]*100;
    RSDt[i]<-sd(data_matrix[i,1:rep+1+rep])/yt_interim[i]*100;
  }
  library(drc);
  Scoef<-coef(drc::drm(ys_interim~dose,fct=LL.4()))*c(-1,1,1,1);
  Tcoef<-coef(drc::drm(yt_interim~dose,fct=LL.4()))*c(-1,1,1,1);
  Y_s_fm<-Scoef[3]+(Scoef[2]-Scoef[3])/(1+(dose/Scoef[4])^Scoef[1]);
  Y_t_fm<-Tcoef[3]+(Tcoef[2]-Tcoef[3])/(1+(dose/Tcoef[4])^Tcoef[1]);
  RRs<-sum((Y_s_fm-mean(ys_interim))^2)/sum((ys_interim-mean(ys_interim))^2);
  RRt<-sum((Y_t_fm-mean(yt_interim))^2)/sum((yt_interim-mean(yt_interim))^2);

  dose_interim<-c(dose,dose);
  Model_cm<-drm(y_interim~dose_interim,c(rep("Standard",length(dose)),rep("Trial",length(dose))),pmodels=data.frame(1,1,1,c(rep("Standard",length(dose)),rep("Trial",length(dose)))),fct=LL.4());
  Scoef<-coef(drc::drm(y_interim~dose_interim,c(rep("Standard",length(dose)),rep("Trial",length(dose))),pmodels=data.frame(1,1,1,c(rep("Standard",length(dose)),rep("Trial",length(dose)))),fct=LL.4()))[-5]*c(-1,1,1,1);
  Tcoef<-coef(drc::drm(y_interim~dose_interim,c(rep("Standard",length(dose)),rep("Trial",length(dose))),pmodels=data.frame(1,1,1,c(rep("Standard",length(dose)),rep("Trial",length(dose)))),fct=LL.4()))[-4]*c(-1,1,1,1);
  Y_s_cm<-Scoef[3]+(Scoef[2]-Scoef[3])/(1+(dose/Scoef[4])^Scoef[1]);
  Y_t_cm<-Tcoef[3]+(Tcoef[2]-Tcoef[3])/(1+(dose/Tcoef[4])^Tcoef[1]);


  ss_total<-sum((data_interim1-mean(data_interim1))^2)+sum((data_interim2-mean(data_interim2))^2);
  f_total<-length(dose)*rep*2-1;
  ss_preparation<-length(dose)*rep*((mean(data_interim1)-mean(data_matrix[,-1]))^2+(mean(data_interim2)-mean(data_matrix[,-1]))^2);
  f_preparation<-1;
  ss_regression<-2*(sum((Y_s_cm-mean(data_interim1))^2)+sum((Y_t_cm-mean(data_interim2))^2));
  f_regression<-3;
  ss_line<-sum((data_interim1-c(Y_s_cm,Y_s_cm))^2)+sum((data_interim2-c(Y_t_cm,Y_t_cm))^2)-sum((data_interim1-c(Y_s_fm,Y_s_fm))^2)-sum((data_interim2-c(Y_t_fm,Y_t_fm))^2);
  f_line<-3;
  ss_residual<-sum((data_interim1-c(Y_s_fm,Y_s_fm))^2)+sum((data_interim2-c(Y_t_fm,Y_t_fm))^2);
  f_residual<-2*rep*length(dose)-8;
  ss_lackfit<-2*(sum((ys_interim-Y_s_fm)^2)+sum((yt_interim-Y_t_fm)^2));
  f_lackfit<-rep*length(dose)-8;
  ss_s_lackfit<-2*sum((ys_interim-Y_s_fm)^2);
  f_s_lackfit<-f_lackfit/2;
  ss_t_lackfit<-2*sum((yt_interim-Y_t_fm)^2);
  f_t_lackfit<-f_lackfit/2;
  ss_error<-ss_residual-ss_lackfit;
  f_error<-f_residual-f_lackfit;
  df<-c(f_preparation,f_regression,f_line,f_residual,f_lackfit,f_s_lackfit,f_t_lackfit,f_error,f_total);
  ss<-c(ss_preparation,ss_regression,ss_line,ss_residual,ss_lackfit,ss_s_lackfit,ss_t_lackfit,ss_error,ss_total);
  ms<-c(ss_preparation/f_preparation,ss_regression/f_regression,ss_line/f_line,ss_residual/f_residual,ss_lackfit/f_lackfit,ss_s_lackfit/f_s_lackfit,ss_t_lackfit/f_t_lackfit,ss_error/f_error,ss_total/f_total);
  F<-c(ms[1]/ms[4],ms[2]/ms[4],ms[3]/ms[4],NA,ms[5]/ms[8],ms[6]/ms[8],ms[7]/ms[8],NA,NA);
  p<-c(1-pf(F[1],f_preparation,f_residual),1-pf(F[2],f_regression,f_residual),1-pf(F[3],f_line,f_residual),NA,1-pf(F[5],f_lackfit,f_error),1-pf(F[6],f_s_lackfit,f_error),1-pf(F[7],f_t_lackfit,f_error),NA,NA);
  ANOVA_data<-data.frame(Variation=c("Preparation","Regression","Line","Residual","Lack of fit","Standard","Trial","Error","Total"),Df=df,SS=ss,MS=ms,F=F,P=p);
  summary_data<- data.frame(Variation=c(rep("Standard",length(dose)),rep("Trial",length(dose))),Dose=c(dose,dose),Y_mean=c(ys_interim,yt_interim),RSD=c(RSDs,RSDt),Y_predict_fm=c(Y_s_fm,Y_t_fm),Y_predict_cm=c(Y_s_cm,Y_t_cm))
  EDcomp(Model_cm,c(50,50),interval="delta");
  return(list(ANOVA=ANOVA_data,Data=summary_data));
}


parallel_line_stt<-function(AT,AU,sdose,tdose,udose,ns,nt,nu,ys,yt,yu,ratio,method=c("random","block"),select=c("2.2.2","3.3.3")){
  #verifaction
  if(sum(ns)!=length(ys)|sum(nt)!=length(yt)|length(sdose)!=length(ns)|length(tdose)!=length(nt)|(sum(nu)!=length(yu))|(length(udose)!=length(nu)))
    return("error input");
  if(min(ns)!=max(ns))
  {local_interim<-c(which((rep(max(ns),length(ns))-ns)>0));
  miss_interim<-max(ns)-ns[local_interim];
  m_interim<-max(ns);
  for(i in 1:length(local_interim))
  {
    if(i==length(local_interim))
    {y_interim<-c(ys[1:((local_interim[i]-1)*m_interim+ns[local_interim[i]])],rep(NA,miss_interim[i]));
    ys<-y_interim;}
    if(i!=length(local_interim))
    {y_interim<-c(ys[1:((local_interim[i]-1)*m_interim+ns[local_interim[i]])],rep(NA,miss_interim[i]),ys[((local_interim[i]-1)*m_interim+ns[local_interim[i]]+1):length(ys)]);
    ys<-y_interim;}
    i<-i+1;
  }
  ns<-rep(max(ns),length(ns));
  }
  if(min(nt)!=max(nt))
  {local_interim<-c(which((rep(max(nt),length(nt))-nt)>0));
  miss_interim<-max(nt)-nt[local_interim];
  m_interim<-max(nt);
  for(i in 1:length(local_interim))
  {
    if(i==length(local_interim))
    {y_interim<-c(yt[1:((local_interim[i]-1)*m_interim+nt[local_interim[i]])],rep(NA,miss_interim[i]));
    yt<-y_interim;}
    if(i!=length(local_interim))
    {y_interim<-c(yt[1:((local_interim[i]-1)*m_interim+nt[local_interim[i]])],rep(NA,miss_interim[i]),yt[((local_interim[i]-1)*m_interim+nt[local_interim[i]]+1):length(yt)]);
    yt<-y_interim;}
    i<-i+1;
  }
  nt<-rep(max(nt),length(nt));
  }
  if(min(nu)!=max(nu))
  {local_interim<-c(which((rep(max(nu),length(nu))-nu)>0));
  miss_interim<-max(nu)-nu[local_interim];
  m_interim<-max(nu);
  for(i in 1:length(local_interim))
  {
    if(i==length(local_interim))
    {y_interim<-c(yu[1:((local_interim[i]-1)*m_interim+nu[local_interim[i]])],rep(NA,miss_interim[i]));
    yu<-y_interim;}
    if(i!=length(local_interim))
    {y_interim<-c(yu[1:((local_interim[i]-1)*m_interim+nu[local_interim[i]])],rep(NA,miss_interim[i]),yu[((local_interim[i]-1)*m_interim+nu[local_interim[i]]+1):length(yu)]);
    yu<-y_interim;}
    i<-i+1;
  }
  nt<-rep(max(nt),length(nt));
  }
  if((mean(ns)!=mean(nt))|(mean(ns)!=mean(nu))|(mean(nt)!=mean(nu)))
  {
    if(mean(ns)>=mean(nt)&mean(ns)>=mean(nu))
    {if(mean(nt)<mean(ns))  {local_interim<-c(which((rep(max(ns),length(nt))-nt)>0));
      miss_interim<-max(ns)-nt[local_interim];
      m_interim<-max(ns);
      for(i in 1:length(local_interim))
      {
        if(i==length(local_interim))
        {y_interim<-c(yt[1:((local_interim[i]-1)*m_interim+nt[local_interim[i]])],rep(NA,miss_interim[i]));
        yt<-y_interim;}
        if(i!=length(local_interim))
        {y_interim<-c(yt[1:((local_interim[i]-1)*m_interim+nt[local_interim[i]])],rep(NA,miss_interim[i]),yt[((local_interim[i]-1)*m_interim+nt[local_interim[i]]+1):length(yt)]);
        yt<-y_interim;}
        i<-i+1;
      }
      nt<-rep(max(ns),length(nt));
      }
     if(mean(nu)<mean(ns))  {local_interim<-c(which((rep(max(ns),length(nu))-nu)>0));
      miss_interim<-max(ns)-nu[local_interim];
      m_interim<-max(ns);
      for(i in 1:length(local_interim))
      {
        if(i==length(local_interim))
        {y_interim<-c(yt[1:((local_interim[i]-1)*m_interim+nu[local_interim[i]])],rep(NA,miss_interim[i]));
        yt<-y_interim;}
        if(i!=length(local_interim))
        {y_interim<-c(yt[1:((local_interim[i]-1)*m_interim+nu[local_interim[i]])],rep(NA,miss_interim[i]),yt[((local_interim[i]-1)*m_interim+nu[local_interim[i]]+1):length(yt)]);
        yt<-y_interim;}
        i<-i+1;
      }
      nu<-rep(max(ns),length(nu));
     } }
     if(mean(nt)>=mean(ns)&mean(nt)>=mean(nu))
     {if(mean(ns)<mean(nt)) {local_interim<-c(which((rep(max(nt),length(ns))-ns)>0));
       miss_interim<-max(nt)-ns[local_interim];
       m_interim<-max(nt);
       for(i in 1:length(local_interim))
       {
         if(i==length(local_interim))
         {y_interim<-c(yt[1:((local_interim[i]-1)*m_interim+ns[local_interim[i]])],rep(NA,miss_interim[i]));
         yt<-y_interim;}
         if(i!=length(local_interim))
         {y_interim<-c(yt[1:((local_interim[i]-1)*m_interim+ns[local_interim[i]])],rep(NA,miss_interim[i]),yt[((local_interim[i]-1)*m_interim+ns[local_interim[i]]+1):length(yt)]);
         yt<-y_interim;}
         i<-i+1;
       }
       ns<-rep(max(nt),length(ns));
       }
      if(mean(nu)<mean(nt)) {local_interim<-c(which((rep(max(nt),length(nu))-nu)>0));
       miss_interim<-max(nt)-nu[local_interim];
       m_interim<-max(nt);
       for(i in 1:length(local_interim))
       {
         if(i==length(local_interim))
         {y_interim<-c(yt[1:((local_interim[i]-1)*m_interim+nu[local_interim[i]])],rep(NA,miss_interim[i]));
         yt<-y_interim;}
         if(i!=length(local_interim))
         {y_interim<-c(yt[1:((local_interim[i]-1)*m_interim+nu[local_interim[i]])],rep(NA,miss_interim[i]),yt[((local_interim[i]-1)*m_interim+nu[local_interim[i]]+1):length(yt)]);
         yt<-y_interim;}
         i<-i+1;
       }
       nu<-rep(max(nt),length(nu));
       } }
     if(mean(nu)>=mean(ns)&mean(nu)>=mean(nt))
     {if(mean(ns)<mean(nu)) {local_interim<-c(which((rep(max(nu),length(ns))-ns)>0));
       miss_interim<-max(nu)-ns[local_interim];
       m_interim<-max(nu);
       for(i in 1:length(local_interim))
       {
         if(i==length(local_interim))
         {y_interim<-c(yt[1:((local_interim[i]-1)*m_interim+ns[local_interim[i]])],rep(NA,miss_interim[i]));
         yt<-y_interim;}
         if(i!=length(local_interim))
         {y_interim<-c(yt[1:((local_interim[i]-1)*m_interim+ns[local_interim[i]])],rep(NA,miss_interim[i]),yt[((local_interim[i]-1)*m_interim+ns[local_interim[i]]+1):length(yt)]);
         yt<-y_interim;}
         i<-i+1;
       }
       ns<-rep(max(nu),length(ns));
       }
      if(mean(nt)<mean(nu)) {local_interim<-c(which((rep(max(nu),length(nt))-nt)>0));
       miss_interim<-max(nu)-nt[local_interim];
       m_interim<-max(nu);
       for(i in 1:length(local_interim))
       {
         if(i==length(local_interim))
         {y_interim<-c(yt[1:((local_interim[i]-1)*m_interim+nt[local_interim[i]])],rep(NA,miss_interim[i]));
         yt<-y_interim;}
         if(i!=length(local_interim))
         {y_interim<-c(yt[1:((local_interim[i]-1)*m_interim+nt[local_interim[i]])],rep(NA,miss_interim[i]),yt[((local_interim[i]-1)*m_interim+nt[local_interim[i]]+1):length(yt)]);
         yt<-y_interim;}
         i<-i+1;
       }
       nt<-rep(max(nu),length(nt));
       }}
  }

  #summary as matrix
  data_matrix<-matrix(c(ys,yt,yu),nrow=mean(ns));
  sname<-c("S1","S2","S3","S4");
  tname<-c("T1","T2","T3","T4");
  uname<-c("U1","U2","U3","U4");
  colnames(data_matrix)<-c(sname[1:length(ns)],tname[1:length(nt)],uname[1:length(nu)]);
  rownames(data_matrix)<-c(1:mean(ns));

  #outlier Dixon Grubbs
  m<-length(data_matrix[,1]);
  k<-length(sdose)+length(tdose)+length(udose);
  Jstandrd<-c(0,0,0.988,0.889,0.780,0.698,0.637,0.683,0.635,0.597,0.679,0.642,0.615);
  if(m>=3&m<=7){
    for(i in 1:length(data_matrix[1,])){
      y_interim<-sort(data_matrix[,i]);
      Jmin1<-(y_interim[2]-y_interim[1])/(y_interim[m]-y_interim[1]);
      Jmax1<-(y_interim[m]-y_interim[m-1])/(y_interim[m]-y_interim[1]);
      if(Jmin1>Jstandrd[m]){data_matrix[which.min(data_matrix[,i]),i]<-NA}
      if(Jmax1>Jstandrd[m]){data_matrix[which.max(data_matrix[,i]),i]<-NA}
      i<-i+1;
    }
  }
  if(m>=8&m<=10){
    for(i in 1:length(data_matrix[1,])){
      y_interim<-sort(data_matrix[,i]);
      Jmin2<-(y_interim[2]-y_interim[1])/(y_interim[m-1]-y_interim[1]);
      Jmax2<-(y_interim[m]-y_interim[m-1])/(y_interim[m]-y_interim[2]);
      if(Jmin2>Jstandrd[m]){data_matrix[which.min(data_matrix[,i]),i]<-NA}
      if(Jmax2>Jstandrd[m]){data_matrix[which.max(data_matrix[,i]),i]<-NA}
      i<-i+1;
    }
  }
  if(m>=11&m<=13){
    for(i in 1:length(data_matrix[1,])){
      y_interim<-sort(data_matrix[,i]);
      Jmin3<-(y_interim[3]-y_interim[1])/(y_interim[m-1]-y_interim[1]);
      Jmax3<-(y_interim[m]-y_interim[m-3])/(y_interim[m]-y_interim[2]);
      if(Jmin3>Jstandrd[m]){data_matrix[which.min(data_matrix[,i]),i]<-NA}
      if(Jmax3>Jstandrd[m]){data_matrix[which.max(data_matrix[,i]),i]<-NA}
      i<-i+1;
    }
  }
  if(m>13){
    p<-1-0.01/(2*m);
    G<-(m-1)*qt(p,m-2)/(m*(m-2+qt(p,m-2)^2))^0.5;
    for(i in 1:length(data_matrix[1,])){
      y_interim<-data_matrix[,i];
      data_matrix[((y_interim-mean(y_interim))/sd(y_interim)>G),i]<-NA;
      i<-i+1;
    }
  }

  #missing fill
  f_correct<-sum(is.na(data_matrix[is.na(data_matrix)]));
  if(method=="random"&f_correct>0){
    for(i in 1:length(data_matrix[1,])){
      y_interim<-data_matrix[,i][!is.na(data_matrix[,i])];
      data_matrix[is.na(data_matrix[,i]),i]<-mean(y_interim);
      i<i+1;
    }
  }
  if(method=="block"&f_correct>0){
    if(f_correct==1){
      if(which(is.na(data_matrix))%%m==0){nrow<-m; ncol<-which(is.na(data_matrix))%/%m;}
      if(which(is.na(data_matrix))%%m!=0){nrow<-which(is.na(data_matrix))%%m;ncol<-which(is.na(data_matrix))%/%m+1;}
      c<-sum(data_matrix[-nrow,ncol]);
      r<-sum(data_matrix[nrow,-ncol]);
      k<-length(sdose)+length(tdose)+length(udose);
      g<-sum(data_matrix[!is.na(data_matrix)]);
      data_matrix[is.na(data_matrix)]<-(k*c+m*r-g)/((k-1)*(m-1));
    }
    if(f_correct>1){
      n_interim<-which(is.na(data_matrix));
      data_matrix[n_interim]<-100;
      for(ii in 1:100){
        i<-1;
        for(i in 1:length(n_interim)){
          if(n_interim[i] %% m==0){nrow<-m;ncol<-n_interim[i]%/%m;}
          if(n_interim[i] %% m!=0){nrow<-n_interim[i]%%m;ncol<-n_interim[i]%/%m+1;}
          c<-sum(data_matrix[-nrow,ncol]);
          r<-sum(data_matrix[nrow,-ncol]);
          k<-length(sdose)+length(tdose)+length(udose);
          g<-sum(data_matrix)-data_matrix[nrow,ncol];
          data_matrix[n_interim[i]]<-(k*c+m*r-g)/((k-1)*(m-1));
          i<-i+1;
        }
        ii<-ii+1;
      }
    }
  }

  #ANOVA
  if(method=="random"){
    ss_total<-sum(data_matrix^2)-sum(data_matrix)^2/(m*k);f_total<-m*k-1;ms_total<-ss_total/f_total;
    ss_dose<-sum(colSums(data_matrix)^2)/m-sum(data_matrix)^2/(m*k);f_dose<-k-1;ms_dose<-ss_dose/f_dose;
    ss_error<-ss_total-ss_dose;f_error<-(m-1)*k-f_correct;ms_error<-ss_error/f_error;
    ss1<-c(ss_dose,ss_error,ss_total);
    df1<-c(f_dose,f_error,f_total);
    ms1<-c(ms_dose,ms_error,ms_total);
    F1<-c(ms_dose/ms_error,NA,NA);
    p1<-c(1-pf(F1[1],f_dose,f_error),NA,NA);
    ANOVA_data1<-data.frame(Variation=c("Dose","Error","Total"),SS=ss1,Df=df1,MS=ms1,F=F1,P=p1);
  }
  if(method=="block"){
    ss_total<-sum(data_matrix^2)-sum(data_matrix)^2/(m*k);f_total<-m*k-1;ms_total<-ss_total/f_total;
    ss_dose<-sum(colSums(data_matrix)^2)/m-sum(data_matrix)^2/(m*k);f_dose<-k-1;ms_dose<-ss_dose/f_dose;
    ss_block<-sum(rowSums(data_matrix)^2)/k-sum(data_matrix)^2/(m*k);f_block<-m-1;ms_block<-ss_block/f_block;
    ss_error<-ss_total-ss_dose-ss_block;f_error<-(k-1)*(m-1)-f_correct;ms_error<-ss_error/f_error;
    ss1<-c(ss_dose,ss_block,ss_error,ss_total);
    df1<-c(f_dose,f_block,f_error,f_total);
    ms1<-c(ms_dose,ms_block,ms_error,ms_total);
    F1<-c(ms_dose/ms_error,ms_block/ms_error,NA,NA);
    p1<-c(1-pf(F1[1],f_dose,f_error),1-pf(F1[2],f_block,f_error),NA,NA);
    ANOVA_data1<-data.frame(Variation=c("Dose","Block","Error","Total"),SS=ss1,Df=df1,MS=ms1,F=F1,P=p1);
  }

  #Reliability
  c_correct2.2.2<-matrix(c(-1,1,-1,1,-1,1,1,-1,-1,1,0,0,1,-1,0,0,-1,1,0,0,1,-1,-1,1),nrow=4,byrow=T);
  c_correct3.3.3<-matrix(c(-1,0,1,-1,0,1,-1,0,1,1,0,-1,-1,0,1,0,0,0,1,0,-1,0,0,0,-1,0,1,0,0,0,1,0,-1,-1,0,1,1,-2,1,1,-2,1,1,-2,1,-1,2,-1,1,-2,1,0,0,0,-1,2,-1,0,0,0,1,-2,1,0,0,0,-1,2,-1,1,-2,1),nrow=8,byrow=T);
  if(select=="2.2.2"){
    ss_preparation<-(sum(data_matrix[,1:2])^2+sum(data_matrix[,3:4])^2+sum(data_matrix[,5:6])^2)/(2*m)-sum(data_matrix)^2/(m*k); f_preparation<-2
    ss_regression<-sum(colSums(data_matrix)*c_correct2.2.2[1,])^2/(m*sum(c_correct2.2.2[1,]^2)); f_regression<-1;
    ss_line<-2*((colSums(data_matrix)*c_correct2.2.2[2,])^2+(colSums(data_matrix)*c_correct2.2.2[3,])^2+(colSums(data_matrix)*c_correct2.2.2[4,])^2)/(12*m); f_line<-2;
    ms_preparation<-ss_preparation/f_preparation;
    ms_regression<-ss_regression/f_regression;
    ms_line<-ss_line/f_line;
    F_preparation<-ms_preparation/ms_error;
    F_regression<-ms_regression/ms_error;
    F_line<-ms_line/ms_error;
    p_preparation<-1-pf(F_preparation,f_preparation,f_error);
    p_regression<-1-pf(F_regression,f_regression,f_error);
    p_line<-1-pf(F_line,f_line,f_error);
    ss2<-c(ss_preparation,ss_regression,ss_line);
    df2<-c(f_preparation,f_regression,f_line);
    ms2<-c(ms_preparation,ms_regression,ms_line);
    F2<-c(ms_preparation/ms_error,ms_regression/ms_error,ms_line/ms_error);
    p2<-c(p_preparation,p_regression,p_line);
    ANOVA_data2<-data.frame(Variation=c("Preparation","Regression","Line"),SS=ss2,Df=df2,MS=ms2,F=F2,P=p2);
  }
  if(select=="3.3.3"){
    ss_preparation<-(sum(data_matrix[,1:3])^2+sum(data_matrix[,4:6])^2+sum(data_matrix[,7:9])^2)/(3*m)-sum(data_matrix)^2/(m*k); f_preparation<-2
    ss_regression<-sum(colSums(data_matrix)*c_correct3.3.3[1,])^2/(m*sum(c_correct3.3.3[1,]^2)); f_regression<-1;
    ss_conic<-sum(colSums(data_matrix)*c_correct3.3.3[5,])^2/(m*sum(c_correct3.3.3[5,]^2)); f_conic<-1;
    ss_line<-2*((sum(colSums(data_matrix)*c_correct3.3.3[2,]))^2+(sum(colSums(data_matrix)*c_correct3.3.3[3,]))^2+(sum(colSums(data_matrix)*c_correct3.3.3[4,]))^2)/(12*m); f_line<-2;
    ss_inverseconic<-2*((sum(colSums(data_matrix)*c_correct3.3.3[6,]))^2+(sum(colSums(data_matrix)*c_correct3.3.3[7,]))^2+(sum(colSums(data_matrix)*c_correct3.3.3[8,]))^2)/(36*m); f_inverseconic<-2;

    ms_preparation<-ss_preparation/f_preparation;
    ms_regression<-ss_regression/f_regression;
    ms_line<-ss_line/f_line;
    ms_conic<-ss_conic/f_conic;
    ms_inverseconic<-ss_inverseconic/f_inverseconic;

    F_preparation<-ms_preparation/ms_error;
    F_regression<-ms_regression/ms_error;
    F_line<-ms_line/ms_error;
    F_conic<-ms_conic/ms_error;
    F_inverseconic<-ms_inverseconic/ms_error;

    p_preparation<-1-pf(F_preparation,f_preparation,f_error);
    p_regression<-1-pf(F_regression,f_regression,f_error);
    p_line<-1-pf(F_line,f_line,f_error);
    p_conic<-1-pf(F_conic,f_conic,f_error);
    p_inverseconic<-1-pf(F_inverseconic,f_inverseconic,f_error);

    ss2<-c(ss_preparation,ss_regression,ss_line,ss_conic,ss_inverseconic);
    df2<-c(f_preparation,f_regression,f_line,f_conic,f_inverseconic);
    ms2<-c(ms_preparation,ms_regression,ms_line,ms_conic,ms_inverseconic);
    F2<-c(F_preparation,F_regression,F_line,F_conic,F_inverseconic);
    p2<-c(p_preparation,p_regression,p_line,p_conic,p_inverseconic);
    ANOVA_data2<-data.frame(Variation=c("Preparation","Regression","Line","Conic","InverseConic"),SS=ss2,Df=df2,MS=ms2,F=F2,P=p2);
  }

  #potency
  sum_data<-colSums(data_matrix);
  names(sum_data)<-NULL;
  if(select=="2.2.2"){
    vt<-0.5*(sum_data[3]+sum_data[4]-sum_data[1]-sum_data[2]);
    vu<-0.5*(sum_data[5]+sum_data[6]-sum_data[1]-sum_data[2]);
    w<-1/3*(sum_data[4]-sum_data[3]+sum_data[6]-sum_data[5]+sum_data[2]-sum_data[1]);
    Dt<-sdose[2]/tdose[2];
    Du<-sdose[2]/udose[2];
    A<-1;B<-2/3;g<-4*(qt(0.975,f_error)^2)*ms_error*m/(3*w^2);
    I<-log10(ratio);
    smt<-I/(w^2*(1-g))*(m*ms_error*((1-g)*A*w^2+B*vt^2))^0.5;
    smu<-I/(w^2*(1-g))*(m*ms_error*((1-g)*A*w^2+B*vu^2))^0.5;
    potency_t<-AT*Dt*10^(I*vt/w);
    potency_u<-AU*Du*10^(I*vu/w);
    potency_t_low<-AT*10^(log10(Dt*10^(I*vt/w))/(1-g)-qt(0.975,f_error)*smt);
    potency_t_up<-AT*10^(log10(Dt*10^(I*vt/w))/(1-g)+qt(0.975,f_error)*smt);
    potency_u_low<-AU*10^(log10(Du*10^(I*vu/w))/(1-g)-qt(0.975,f_error)*smu);
    potency_u_up<-AU*10^(log10(Du*10^(I*vu/w))/(1-g)+qt(0.975,f_error)*smu);
  }

  if(select=="3.3.3"){
    vt<-1/3*(sum_data[4]+sum_data[5]+sum_data[6]-sum_data[1]-sum_data[2]-sum_data[3]);
    vu<-1/3*(sum_data[7]+sum_data[8]+sum_data[9]-sum_data[1]-sum_data[2]-sum_data[3]);
    w<-1/6*(sum_data[6]-sum_data[4]+sum_data[9]-sum_data[7]+sum_data[3]-sum_data[1]);
    Dt<-sdose[3]/tdose[3];
    Du<-sdose[3]/udose[3];
    A<-2/3;B<-1/6;g<-(qt(0.975,f_error)^2)*ms_error*m/(6*w^2);
    I<-log10(ratio);
    smt<-I/(w^2*(1-g))*(m*ms_error*((1-g)*A*w^2+B*vt^2))^0.5;
    smu<-I/(w^2*(1-g))*(m*ms_error*((1-g)*A*w^2+B*vu^2))^0.5;
    potency_t<-AT*Dt*10^(I*vt/w);
    potency_u<-AU*Du*10^(I*vu/w);
    potency_t_low<-AT*10^(log10(Dt*10^(I*vt/w))/(1-g)-qt(0.975,f_error)*smt);
    potency_t_up<-AT*10^(log10(Dt*10^(I*vt/w))/(1-g)+qt(0.975,f_error)*smt);
    potency_u_low<-AU*10^(log10(Du*10^(I*vu/w))/(1-g)-qt(0.975,f_error)*smu);
    potency_u_up<-AU*10^(log10(Du*10^(I*vu/w))/(1-g)+qt(0.975,f_error)*smu);
  }

  #return outcome
  outcome<-list(Data=data_matrix,ANOVA=ANOVA_data1,Reliability=ANOVA_data2,Potency_T=potency_t,Potency_T_95low=potency_t_low,Potency_T_95up=potency_t_up,Potency_U=potency_u,Potency_U_95low=potency_u_low,Potency_U_95up=potency_u_up);
  return(outcome);
}



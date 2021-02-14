ED50<-function(tdose,response){
  tm<-c(rep(100,length(tdose)));
  tr<-tm*response;
  tY<-0;
  tY[1:length(tdose)]<-0;
  for(i in 1:100){
    tZ<-exp(-tY^2/2)/sqrt(2*pi);
    ty<-tY+(response-pnorm(tY,0,1))/tZ;
    tw<-(tm*tZ^2)/(pnorm(tY,0,1)-pnorm(tY,0,1)^2);
    tx<-log(tdose);
    tsxx<-sum(tw*tx^2)-(sum(tw*tx))^2/sum(tw);
    tsxy<-sum(tw*tx*ty)-sum(tw*tx)*sum(tw*ty)/sum(tw);
    tsyy<-sum(tw*ty^2)-sum(tw*ty)^2/sum(tw);
    txbar<-sum(tw*tx)/sum(tw);
    tybar<-sum(tw*ty)/sum(tw);

    b<-tsxy/tsxx;
    tY<-(tybar-txbar*b)+tx*b;
    i<i+1;
  }

  reliability<-(tsyy-tsxy^2/tsxx);
  p_reliability<-(1-pchisq(reliability,(length(tdose)-2)));


  M<-(tybar-txbar*b)*(-1/b);
  ED50<-exp(M);
  H<-b^2*tsxx/(b^2*tsxx-1.96^2);
  L<-1/sum(tw);
  ED50_LCI<-exp((H*M-(H-1)*(txbar)-sqrt((H-1)*(L*(tsxx)+H*(M-txbar)^2))));
  ED50_UCI<-exp((H*M-(H-1)*(txbar)+sqrt((H-1)*(L*(tsxx)+H*(M-txbar)^2))));


  df1<-data.frame(sample=c(rep("trial",length(tdose))),dose=c(tdose),m=c(tm),r=c(tr),x=c(tx),Y=c(tY),
                  fi=c(pnorm(tY,0,1)),Z=c(tZ),y=c(ty),w=c(tw),wx=c(tw*tx),wy=c(tw*ty),wx2=c(tw*tx^2),
                  wy2=c(tw*ty^2),wxy=c(tw*tx*ty));
  df2<-data.frame(sample=c("trial"),Sum_w=c(sum(tw)),sum_wx=c(sum(tw*tx)),sum_wy=c(sum(tw*ty)),sum_wx2=c(sum(tw*tx^2)),sum_wy2=c(sum(tw*ty^2)),
                  sum_wxy=c(sum(tw*tx*ty)),Sxx=c(tsxx),Sxy=c(tsxy),Syy=c(tsyy),xbar=c(txbar),ybar=c(tybar),a=c((tybar-txbar*b)),b=b);

  out1<-matrix(c(reliability,p_reliability),nrow=1,ncol=2,dimnames=list(c("Line"),c("Statistic","P_value")));

  out2<-data.frame(ED50=ED50,Low_ED50=ED50_LCI,Up_ED50=ED50_UCI);
  result<-list(Convergence_worksheet1=df1,Convergence_worksheet2=df2,Outcome1=out1,Outcome2=out2);

  return(result);
}

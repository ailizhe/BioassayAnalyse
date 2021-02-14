bliss<-function(AT,sdose,sm,sr,tdose,tm,tr){
  sY<-0;tY<-0;
  sY[1:length(sdose)]<-0;tY[1:length(tdose)]<-0;
  for(i in 1:100){
    sZ<-exp(-sY^2/2)/sqrt(2*pi);
    sy<-sY+(sr/sm-pnorm(sY,0,1))/sZ;
    sw<-(sm*sZ^2)/(pnorm(sY,0,1)-pnorm(sY,0,1)*pnorm(sY,0,1));
    sx<-log(sdose);
    ssxx<-sum(sw*sx^2)-(sum(sw*sx))^2/sum(sw);
    ssxy<-sum(sw*sx*sy)-sum(sw*sx)*sum(sw*sy)/sum(sw);
    ssyy<-sum(sw*sy^2)-sum(sw*sy)^2/sum(sw);
    sxbar<-sum(sw*sx)/sum(sw);
    sybar<-sum(sw*sy)/sum(sw);

    tZ<-exp(-tY^2/2)/sqrt(2*pi);
    ty<-tY+(tr/tm-pnorm(tY,0,1))/tZ;
    tw<-(tm*tZ^2)/(pnorm(tY,0,1)-pnorm(tY,0,1)*pnorm(tY,0,1));
    tx<-log(tdose);
    tsxx<-sum(tw*tx^2)-(sum(tw*tx))^2/sum(tw);
    tsxy<-sum(tw*tx*ty)-sum(tw*tx)*sum(tw*ty)/sum(tw);
    tsyy<-sum(tw*ty^2)-sum(tw*ty)^2/sum(tw);
    txbar<-sum(tw*tx)/sum(tw);
    tybar<-sum(tw*ty)/sum(tw);

    b<-(ssxy+tsxy)/(ssxx+tsxx);
    sY<-(sybar-sxbar*b)+sx*b;
    tY<-(tybar-txbar*b)+tx*b;
    i<i+1;
  }

  reliability<-(ssyy-ssxy^2/ssxx)+(tsyy-tsxy^2/tsxx);
  p_reliability<-(1-pchisq(reliability,(length(sdose)+length(tdose)-4)));
  parallel<-(ssxy^2/ssxx+tsxy^2/tsxx)-((ssxy+tsxy)^2/(ssxx+tsxx));
  p_parallel<-(1-pchisq(parallel,1));

  M<-((tybar-txbar*b)-(sybar-sxbar*b))/b;
  PT<-AT*exp(M);
  H<-b^2*(ssxx+tsxx)/(b^2*(ssxx+tsxx)-1.96^2);
  L<-1/sum(sw)+1/sum(tw);
  PT_LCI<-AT*exp(H*M-(H-1)*(sxbar-txbar)-sqrt((H-1)*(L*(ssxx+tsxx)+H*(M-sxbar+txbar)^2)));
  PT_UCI<-AT*exp(H*M-(H-1)*(sxbar-txbar)+sqrt((H-1)*(L*(ssxx+tsxx)+H*(M-sxbar+txbar)^2)));


  df1<-data.frame(sample=c(rep("standard",length(sdose)),rep("trial",length(tdose))),dose=c(sdose,tdose),m=c(sm,tm),r=c(sr,tr),x=c(sx,tx),Y=c(sY,tY),
                 fi=c(pnorm(sY,0,1),pnorm(tY,0,1)),Z=c(sZ,tZ),y=c(sy,ty),w=c(sw,tw),wx=c(sw*sx,tw*tx),wy=c(sw*sy,tw*ty),wx2=c(sw*sx^2,tw*tx^2),
                           wy2=c(sw*sy^2,tw*ty^2),wxy=c(sw*sx*sy,tw*tx*ty));
  df2<-data.frame(sample=c("standard","trial"),Sum_w=c(sum(sw),sum(tw)),sum_wx=c(sum(sw*sx),sum(tw*tx)),sum_wy=c(sum(sw*sy),sum(tw*ty)),sum_wx2=c(sum(sw*sx^2),sum(tw*tx^2)),sum_wy2=c(sum(sw*sy^2),sum(tw*ty^2)),
                  sum_wxy=c(sum(sw*sx*sy),sum(tw*tx*ty)),Sxx=c(ssxx,tsxx),Sxy=c(ssxy,tsxy),Syy=c(ssyy,tsyy),xbar=c(sxbar,txbar),ybar=c(sybar,tybar),a=c((sybar-sxbar*b),(tybar-txbar*b)));
  out1<-matrix(c(reliability,parallel,p_reliability,p_parallel),nrow=2,ncol=2,dimnames=list(c("Line","Parallel"),c("Statistic","P_value")))

  out2<-data.frame(Potency=PT,Low_FL=PT_LCI,Up_FL=PT_UCI);
  result<-list(Convergence_worksheet1=df1,Convergence_worksheet2=df2,Outcome1=out1,Outcome2=out2);

  return(result);
}

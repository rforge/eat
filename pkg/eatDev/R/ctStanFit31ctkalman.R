ctStanFit<-function(datawide, ctmodelobj, continuoustime=TRUE, kalman=TRUE, 
  indvarying='all', dist=NULL, fit=TRUE, noncentered=TRUE,
  optimize=FALSE, vb=FALSE, iter=100, chains=1, parallel=TRUE, verbose=FALSE,...){
  require(rstan)
  
  if(!is.null(ctmodelobj$timeVarying)) stop('Time varying params not allowed at present! Correct ctModel spec')
  
  unlistCtModel<-function(ctmodelobj){
    out<-matrix(NA,nrow=0,ncol=5)
    out<-as.data.frame(out,stringsAsFactors =FALSE)
    objectlist<-c('T0MEANS','LAMBDA','DRIFT','DIFFUSION','MANIFESTVAR','MANIFESTMEANS', 'CINT', if(n.TDpred > 0) 'TDPREDEFFECT', 'T0VAR')
    for(obji in objectlist){
      for(rowi in 1:nrow(ctmodelobj[[obji]])){
        for(coli in 1:ncol(ctmodelobj[[obji]])){
          out<-rbind(out,data.frame(obji,rowi,coli,
            ifelse(is.na(suppressWarnings(as.numeric(ctmodelobj[[obji]][rowi,coli]))), #ifelse element is character string
              ctmodelobj[[obji]][rowi,coli],
              NA),
            ifelse(!is.na(suppressWarnings(as.numeric(ctmodelobj[[obji]][rowi,coli]))), #ifelse element is numeric
              as.numeric(ctmodelobj[[obji]][rowi,coli]),
              NA),
            stringsAsFactors =FALSE
          ))
        }
      }
    }
    colnames(out)<-c('matrix','row','col','param','value')
    return(out)
  }
  
  
  #read in ctmodel values
  n.latent<-ctmodelobj$n.latent
  n.manifest<-ctmodelobj$n.manifest
  Tpoints<-ctmodelobj$Tpoints
  n.TDpred<-ctmodelobj$n.TDpred
  n.TIpred<-ctmodelobj$n.TIpred
  
  manifestNames<-ctmodelobj$manifestNames
  latentNames<-ctmodelobj$latentNames
  TDpredNames<-ctmodelobj$TDpredNames
  TIpredNames<-ctmodelobj$TIpredNames

  ctspec<-unlistCtModel(ctmodelobj)
  if(kalman==FALSE) {
      message('ATTENTION: T0VAR matrix is ignored when kalman=FALSE \n')
      ctspec<-ctspec[-which(ctspec$matrix %in% 'T0VAR'),]
      if(any(!is.na(ctspec[ctspec$matrix %in% 'T0MEANS','value']))) message('ATTENTION: Some T0MEANS are fixed - may be problematic when kalman=FALSE \n')
    }
      
      freeparams<-is.na(ctspec[,'value'])
  message(paste0(1:sum(freeparams), ' ', ctspec[freeparams,'param'],'\n'))
  
  
  ctspec$lbound<-NA
  ctspec$ubound<-NA
  
  ######### STAN parameter limits
  ctspec$lbound[ctspec$matrix %in% c('T0MEANS','MANIFESTMEANS','TDPREDEFFECT','CINT') & freeparams] <- -50
  ctspec$ubound[ctspec$matrix %in% c('T0MEANS','MANIFESTMEANS','TDPREDEFFECT','CINT') & freeparams] <- 50
  
  ctspec$lbound[ctspec$matrix %in% c('DIFFUSION','MANIFESTVAR', 'T0VAR') & freeparams & ctspec$row != ctspec$col] <- -.99
  ctspec$ubound[ctspec$matrix %in% c('DIFFUSION','MANIFESTVAR', 'T0VAR') & freeparams & ctspec$row != ctspec$col] <- .99
  
  ctspec$lbound[ctspec$matrix %in% c('DIFFUSION','MANIFESTVAR', 'T0VAR') & freeparams & ctspec$row == ctspec$col] <- .001
  ctspec$ubound[ctspec$matrix %in% c('DIFFUSION','MANIFESTVAR', 'T0VAR') & freeparams & ctspec$row == ctspec$col] <- 50

  if(continuoustime==TRUE){
    ctspec$lbound[ctspec$matrix %in% c('DRIFT') & freeparams & ctspec$row == ctspec$col] <- -5
    ctspec$ubound[ctspec$matrix %in% c('DRIFT') & freeparams & ctspec$row == ctspec$col] <- -.00001
    
    ctspec$lbound[ctspec$matrix %in% c('DRIFT') & freeparams & ctspec$row != ctspec$col] <- -.5
    ctspec$ubound[ctspec$matrix %in% c('DRIFT') & freeparams & ctspec$row != ctspec$col] <- .5
  }
  if(continuoustime==FALSE){
    ctspec$lbound[ctspec$matrix %in% c('DRIFT') & freeparams & ctspec$row == ctspec$col] <- -.2
    ctspec$ubound[ctspec$matrix %in% c('DRIFT') & freeparams & ctspec$row == ctspec$col] <- 1.2
    
    ctspec$lbound[ctspec$matrix %in% c('DRIFT') & freeparams & ctspec$row != ctspec$col] <- -.5
    ctspec$ubound[ctspec$matrix %in% c('DRIFT') & freeparams & ctspec$row != ctspec$col] <- .5
  }
  
  nparams<-sum(freeparams)
  
  if(all(indvarying=='all'))  indvarying<-rep(TRUE,nparams)
  if(length(indvarying) != sum(freeparams)) stop('indvarying must be ', nparams,' long!')
  nindvarying <- sum(indvarying)
  
  if(is.null(dist)) {
    dist<-rep('uniform',nparams)
    if(continuoustime==TRUE) dist[which(ctspec$matrix[freeparams] %in% 'DRIFT' & ctspec$row[freeparams] == ctspec$col[freeparams]) ] <- 'gamma(1,1)'
  }
  print(ctspec)
  if(length(dist) != nparams) stop('dist must be ', sum(freeparams),' long!')
  
  ctspec$indvarying<-NA
  ctspec$indvarying[is.na(ctspec$value)]<-indvarying
  ctspec$indvarying[!is.na(ctspec$value)]<-FALSE
  ctspec$dist[is.na(ctspec$value)]<-dist
  
  datalong<-ctWideToLong(datawide,n.manifest=n.manifest,Tpoints=Tpoints, 
    manifestNames=ctmodelobj$manifestNames, n.TDpred=n.TDpred,
    TDpredNames = TDpredNames)
  
  T0check<-1
  for(i in 2:nrow(datalong)){
    T0check<-c(T0check, ifelse(datalong[,'id'][i] != datalong[,'id'][i-1], 1, 0))
  }
  
  if(any(is.na(datalong[,'dT']))) stop('Missing time intervals!')
  if(n.TDpred > 0) {
    datalong[,TDpredNames][is.na(datalong[,TDpredNames])] <-0 ## temporary fix for missingness
    if(any(is.na(datalong[,TDpredNames]))) stop('Missingness in TDpreds!')
  }
  if(n.TIpred > 0) {
    if(ctmodelobj$n.TIpred > 0) tipreds <- datawide[,ctmodelobj$TIpredNames,drop=FALSE]
    if(any(is.na(tipreds))) stop('Missingness in TIpreds!')
    
    tipredspec<-matrix(rep(c(TRUE,'uniform',-10,10),n.TIpred),ncol=n.TIpred*4,nrow=1)
    colnames(tipredspec)<-paste0(rep(TIpredNames,each=4),c('_effect','_dist','_lbound','_ubound'))
    ctspec<-cbind(ctspec,tipredspec,stringsAsFactors=FALSE)
    ctspec[!is.na(ctspec$value),paste0(TIpredNames,'_effect')]<-FALSE
  }
  
  datalong[is.na(datalong)]<-99999 #missing data
  
  intervalChange<-cbind(c(1,as.numeric((datalong[,'dT'][2:nrow(datalong)]-datalong[,'dT'][1:(nrow(datalong)-1)])!=0)))
  colnames(intervalChange)<-'intervalChange'
  datalong<-cbind(datalong,intervalChange)
  
  if(ctmodelobj$n.TDpred > 0) tdpreds <- datalong[,ctmodelobj$TDpredNames,drop=FALSE]
  
  nsubjects <- length(unique(datalong[, 'id'])) 
  
  
  
    
    stanmodelobj <- paste0('
      functions{
    
      matrix fillcholcordiag(matrix mat){
      matrix[rows(mat),cols(mat)] out;
      real tempsum;
      out[1,1]<-1;
      if(rows(mat) > 1){
      for(rowi in 2:rows(out)){
      tempsum<-0;
      for(coli in 1:(rowi-1)){
      out[rowi,coli]<-mat[rowi,coli];
      out[coli,rowi] <- 0;
      tempsum <- tempsum + mat[rowi,coli]^2;
      }
      out[rowi,rowi]<-sqrt(fabs(1-tempsum));
      }
      }
      return out;
      }
      
      
      
      matrix varmatrixtransform(matrix mat){
      int ndim;
      matrix[rows(mat),cols(mat)] mscale;
      matrix[rows(mat),cols(mat)] mcholcor;
      matrix[rows(mat),cols(mat)] mchol;
      matrix[rows(mat),cols(mat)] mtx;
      ndim <- cols(mat);
      for(coli in 1:ndim){
      for(rowi in 1:ndim){
      
      if(rowi==coli) {
      mscale[rowi,coli] <-  mat[rowi,coli];
      }
      
      if(rowi > coli){
      mscale[rowi,coli] <-0;
      mscale[coli,rowi] <-0;
      mcholcor[rowi,coli] <-  mat[rowi,coli]; 
      }
      }}
      mcholcor <- fillcholcordiag(mcholcor);
      mchol <- mscale * mcholcor;
      mtx<-multiply_lower_tri_self_transpose(mchol) ;
      return mtx;
      }

      
      real factorial(int m);
      real factorial(int m) {
      if (m == 0)
      return 1;
      else
      return (m * factorial(m-1));
      }
      
      
      matrix matrix_pow(matrix a, real n);
      matrix matrix_pow(matrix a, real n) {
      if (n == 0){
      return diag_matrix(rep_vector(1, rows(a)));
      }
      else
      return a * matrix_pow(a, n - 1);
      }
      
      
      matrix expmt(matrix mat, real t){
      matrix[rows(mat),rows(mat)] out;
      out<-diag_matrix(rep_vector(1.0, rows(mat)));
      for(i in 1:50){
      out<-out+ matrix_pow(mat,i) * t^i /factorial(i);
      }
      return out;
      }
      
      
      matrix expmp(matrix A, matrix padeC, vector padeCbig){
      int n;
      real nA;
      real colsum;
      int l;
      matrix[4,10] C;
      vector[4] t;
      matrix[rows(A),rows(A)] I;
      matrix[rows(A),rows(A)] P;
      matrix[rows(A),rows(A)] U;
      matrix[rows(A),rows(A)] V;
      matrix[rows(A),rows(A)] X;
      
      vector[14] Cbig;
      real s;
      real si;
      matrix[rows(A),rows(A)] B;
      matrix[rows(A),rows(A)] B2;
      matrix[rows(A),rows(A)] B4;
      matrix[rows(A),rows(A)] B6;
      matrix[rows(A),rows(A)] A2;
      
      si <- 0;
      C <- padeC;
      Cbig <- padeCbig;
      
      n <-rows(A);
      if(n != cols(A)) print("expmp: Matrix not square!")
      
      if (n <= 1) X <- exp(A);
      else{
      
      // nA <- Matrix::norm(A, "1")
      nA <- 0;
      for(coli in 1:n){
      colsum<-0;
      for(rowi in 1:n){
      colsum<-colsum+fabs(A[rowi,coli]);
      }
      if(colsum > nA) nA <- colsum;
      }
      
      I <- diag_matrix(rep_vector(1,n));
      if (nA <= 2.1) {
      t[1] <- 0.015; t[2]<- 0.25; t[3]<- 0.95; t[4]<- 2.1;
      
      //l <- which.max(nA <= t)
      for(ti in 1:4){
      if(l==0){
      if(nA <= t[ti]) l <- ti;
      }
      }
      
      A2 <- A * A;
      P <- I;
      U <- C[l, 2] * I;
      V <- C[l, 1] * I;
      for (k in 1:l) {
      P <- P * A2;
      U <- U + C[l, (2 * k) + 2] * P;
      V <- V + C[l, (2 * k) + 1] * P;
      }
      U <- A * U;
      X <- inverse(V - U) * (V + U);
      }
      
      else {
      s <- log2(nA/5.4);
      B <- A;
      if (s > 0) {
      s <- ceil(s);
      B <- B/(2^s);
      }
      
      B2 <- B * B;
      B4 <- B2 * B2;
      B6 <- B2*B4;
      U <- B*(B6*(Cbig[14] * B6 + Cbig[12] * B4 + Cbig[10] * B2) + Cbig[8] * B6 + Cbig[6] * B4 + Cbig[4] * B2 + Cbig[2] * I);
      V <- B6*(Cbig[13] * B6 + Cbig[11] * B4 + Cbig[9] * B2) + Cbig[7] * B6 + Cbig[5] * B4 + Cbig[3] * B2 + Cbig[1] * I;
      X <- inverse(V - U) * (V + U);
      
      if (s > 0) {
      while (si < s){ 
      si <- si + 1;
      X <- X * X;
      }
      }
      }
      
      }
      return X;
      }
      
      
      
      
      
      matrix kron_prod(matrix mata, matrix matb){
      int m;
      int p;
      int n;
      int q;
      matrix[rows(mata)*rows(matb),cols(mata)*cols(matb)] C;
      m<-rows(mata);
      p<-rows(matb);
      n<-cols(mata);
      q<-cols(matb);
      for (i in 1:m){
      for (j in 1:n){
      for (k in 1:p){
      for (l in 1:q){
      C[p*(i-1)+k,q*(j-1)+l] <- mata[i,j]*matb[k,l];
      }
      }
      }
      }
      return C;
      }
      
      }
      data {
      matrix[4,10] padeC;
      vector[14] padeCbig;
      int<lower=0> ndatapoints;
      int<lower=1> nmanifest;
      int<lower=1> nlatent;
      int<lower=1> nsubjects;
      
      ',if(n.TIpred > 0) paste0('int<lower=0> ntipred; \n 
        vector[ntipred] tipreds[nsubjects];'),'
      
      vector[nmanifest] Y[ndatapoints];
      int<lower=0> ntdpred;
      
      ',if(n.TDpred > 0) paste0('vector[ntdpred] tdpreds[ndatapoints];'),'
      
      vector[ndatapoints] dT;
      int intervalChange[ndatapoints];
      int subject[ndatapoints];
      int<lower=0> nparams;
      int<lower=0> nhypersdparams;
      vector[nparams] indvarying;
      int T0check[ndatapoints];
      int continuoustime;
      int nindvarying;
      
      // number of observed variables per observation
      int<lower = 0, upper = nmanifest> y_obs_n[ndatapoints];   
      
      // indices of the observed variables per observation
      // for row rowi, the code will only use the rowi entries
      int<lower = 0, upper = nmanifest> y_obs_i[ndatapoints, nmanifest];
      }
      
      transformed data{
      matrix[nlatent,nlatent] IIlatent;
      IIlatent <- diag_matrix(rep_vector(1,nlatent));
      }
      
      
      
parameters {
      
',paste0('real<lower=',ctspec[freeparams & (noncentered==TRUE | ctspec$indvarying==FALSE),'lbound'],
        ',upper=',ctspec[freeparams & (noncentered==TRUE | ctspec$indvarying==FALSE),'ubound'],
          '> ', ctspec[freeparams & (noncentered==TRUE | ctspec$indvarying==FALSE),'param'],'; \n',collapse=''),'
      
',if(any(ctspec$indvarying)) paste0('real<lower=0.01,upper=',
        ctspec[ctspec$indvarying,'ubound'],'-', 
        ctspec[ctspec$indvarying,'lbound'],'> hsd_', 
        ctspec[ctspec$indvarying,'param'],'; \n ',collapse=''),'


',if(noncentered==TRUE & nindvarying > 0) paste0(
  'vector<lower=', ctspec[ctspec$indvarying,'lbound'],' - ', ctspec[ctspec$indvarying,'param'],
  ',upper=',ctspec[ctspec$indvarying,'ubound'],' - ', ctspec[ctspec$indvarying,'param'],
  '>[nsubjects] sub_', ctspec[ctspec$indvarying,'param'],'; \n',collapse=''),'


',if(noncentered==FALSE & nindvarying > 0) paste0(
      'vector<lower=',ctspec[ctspec$indvarying,'lbound'],
      ',upper=',ctspec[ctspec$indvarying,'ubound'],
      '>[nsubjects] sub_', ctspec[ctspec$indvarying,'param'],'; \n',collapse=''),'

',if(nindvarying > 1) paste0( 'vector<lower= -1,upper=1>[(',nindvarying,' * ',nindvarying,' - ', nindvarying,')/2] hypercholcorrpars; \n '),'
      
',if(n.TIpred > 0) paste0(unlist(lapply(TIpredNames,function(tipredi) unlist(lapply(1:nrow(ctspec),function(rowi) if(
        ctspec[rowi,paste0(tipredi,'_effect')]==TRUE) paste0(
        'real<lower=',ctspec[rowi,paste0(tipredi,'_lbound')],
        ',upper=',ctspec[rowi,paste0(tipredi,'_ubound')],
        '> ', paste0(tipredi,'_effect_', ctspec[rowi,'param']),'; \n',collapse=''))))),collapse=''),'
      
',if(kalman==FALSE) paste0('
        vector[nlatent] eta[ndatapoints-nsubjects];
        //vector[(ndatapoints-nsubjects)*nlatent] innovationbase;
        //vector[nlatent] innovation[ndatapoints-nsubjects];
        '),'
}
      
      
transformed parameters{
',if(nindvarying > 0)  paste0(' matrix[nindvarying,nindvarying] hypercholcorr;
                                  vector[nindvarying] hypersd; \n'),'

',if(any(ctspec$indvarying) & noncentered==FALSE) paste0('real ', ctspec[ctspec$indvarying,'param'],';\n',collapse=''),'

',if(any(ctspec$indvarying) & noncentered==FALSE) paste0(ctspec[ctspec$indvarying,'param'],
      '<- mean(sub_',ctspec[ctspec$indvarying,'param'],');\n',collapse=''),'

',if(any(ctspec$indvarying)) paste0('hypersd[', 1:nindvarying,'] <- hsd_',ctspec[ctspec$indvarying==TRUE,'param'],'; \n ',collapse=''),'

',if(any(ctspec$indvarying)) paste0('hypercholcorr<-diag_matrix(rep_vector(0.000001,nindvarying));'),
  if(sum(ctspec$indvarying)>1) paste0(unlist(lapply(1,function(i) {
        parami<-0
        for(rowi in 1:nindvarying){
        for(coli in 1:nindvarying){
        if(rowi > coli) {
          parami<-parami+1
        out<-paste0('hypercholcorr[',rowi,',',coli,'] <- hypercholcorrpars[',parami,']; \n')
        }
        }
        }
    return(out)
  })),collapse=''),
  if(sum(ctspec$indvarying)>0) paste0('hypercholcorr<-fillcholcordiag(hypercholcorr); \n'),'
}

      
model{

',if(any(ctspec$indvarying)) paste0('matrix[nsubjects,nindvarying] indparams; 
      vector[nindvarying] hypermeans; 
      matrix[nindvarying,nindvarying] paramchol; \n'),'
    
      matrix[nlatent,nlatent] DIFFUSION[nsubjects];
      matrix[nlatent,nlatent] discreteDIFFUSION;
      matrix[nlatent,nlatent] T0VAR[nsubjects];
      matrix[nlatent,nlatent] DRIFT[nsubjects];
      matrix[nlatent*nlatent,nlatent*nlatent] DRIFTHATCH[nsubjects];
      matrix[nmanifest,nmanifest] MANIFESTVAR[nsubjects];
      vector[nmanifest] MANIFESTMEANS[nsubjects];
      vector[nlatent] T0MEANS[nsubjects];
      matrix[nmanifest,nlatent] LAMBDA[nsubjects];
      vector[nlatent] CINT[nsubjects];
      vector[nlatent] discreteCINT;
      matrix[nlatent,nlatent] discreteDRIFT;
      matrix[nlatent,nlatent] invDRIFT[nsubjects];
      matrix[nlatent,nlatent] asymDIFFUSION[nsubjects];
      vector[nlatent*nlatent] asymDIFFUSIONvec;

',if(n.TDpred > 0) paste0('matrix[nlatent,ntdpred] TDPREDEFFECT[nsubjects];'),'
      
',if(kalman==TRUE) paste0('
        //real<lower=0.0> Qmiss;
        // prior state: nlatent(theta_t | y_t, ..., y_{rowi-1})
        vector[nlatent] etaprior[ndatapoints];
        matrix[nlatent, nlatent] etapriorcov[ndatapoints];
        // marginal likelihood: nlatent(y_t | y_t, ..., y_t-1)
        vector[nmanifest] yprob[ndatapoints];
        matrix[nmanifest, nmanifest] yprobcov[ndatapoints];
        // state posterior: nlatent(theta_t | y_t, ..., y_t)
        vector[nlatent] etapost[ndatapoints + 1];
        matrix[nlatent, nlatent] etapostcov[ndatapoints + 1];
        '),'
      
',if(kalman==FALSE) paste0('
        matrix[nlatent,nlatent] discreteDIFFUSIONchol;
        //vector[nlatent] innovation[ndatapoints-nsubjects];
        '),'
      
      
',if(n.TIpred > 0) paste0(unlist(lapply(TIpredNames,function(tipredi) unlist(lapply(1:nrow(ctspec),function(rowi) if(
        ctspec[rowi,paste0(tipredi,'_effect')]==TRUE & 
        ctspec[rowi,paste0(tipredi,'_dist')]!='uniform') paste0(
            tipredi,'_effect_', ctspec[rowi,'param'],' ~ ',
        ctspec[rowi,paste0(tipredi,'_dist')],'; \n'))))),collapse=''),'

',if(any(dist != 'uniform')) paste0(if(ctspec[freeparams & ctspec$dist != 'uniform','ubound'] <= 0) '-', 
                              ctspec[freeparams & ctspec$dist != 'uniform','param'],' ~ ',
                              ctspec[freeparams & ctspec$dist != 'uniform','dist'],';',collapse=''),'

',if(any(ctspec$indvarying)) paste0('paramchol<- diag_pre_multiply(hypersd,hypercholcorr);'),'
',if(any(ctspec$indvarying)) paste0(unlist(lapply(1:nindvarying,function(parami) {
    out<-paste0('indparams[1:nsubjects,',parami,'] <- sub_',ctspec[ctspec$indvarying,'param'][parami],'; \n ')
    if(noncentered==FALSE) out<-c(out,paste0('hypermeans[',parami,'] <- mean(sub_',ctspec[ctspec$indvarying,'param'][parami],'); \n'))
   return(out)
})),collapse=''),'

',if(any(ctspec$indvarying)) paste0('for(subjecti in 1:nsubjects) indparams[subjecti] ~ multi_normal_cholesky(',
      if(noncentered==TRUE) paste0('rep_vector(0,nindvarying)'), 
      if(noncentered==FALSE) paste0('hypermeans'),
    ', paramchol); \n'),'

for(subjecti in 1:nsubjects){
',paste0(unlist(lapply(1:nrow(ctspec),function(rowi) paste0(
  ctspec[rowi,'matrix'], '[subjecti][', ctspec[rowi,'row'], 
  if(ctspec[rowi,'matrix'] %in% c('LAMBDA','DRIFT','DIFFUSION',
    'MANIFESTVAR', 'TDPREDEFFECT', 'T0VAR')) paste0(' , ', ctspec[rowi,'col']),
    '] <- ', 
    if(ctspec[rowi,'indvarying']) 'sub_',
    if(is.na(ctspec[rowi,'value'])) ctspec[rowi,'param'],
      if(!is.na(ctspec[rowi,'value'])) ctspec[rowi,'value'],
        if(ctspec[rowi,'indvarying']) '[subjecti]',
  if(noncentered==TRUE & ctspec[rowi,'indvarying']) paste0(' + ', ctspec[rowi,'param']),
  ';\n ',  collapse=''))),collapse=''),
'}
      
      
for(individual in 1:nsubjects){

      MANIFESTVAR[individual] <- varmatrixtransform(MANIFESTVAR[individual]);
      DIFFUSION[individual] <- varmatrixtransform(DIFFUSION[individual]);
      T0VAR[individual] <- varmatrixtransform(T0VAR[individual]);
      
      ',if(continuoustime==TRUE) paste0('
        invDRIFT[individual] <- inverse(DRIFT[individual]);
        DRIFTHATCH[individual] <- kron_prod(DRIFT[individual],diag_matrix(rep_vector(1, nlatent))) + kron_prod(diag_matrix(rep_vector(1, nlatent)),DRIFT[individual]);
        
        asymDIFFUSIONvec <- -inverse(DRIFTHATCH[individual]) * to_vector(DIFFUSION[individual]);
        for(drowi in 1:nlatent) {
        for(dcoli in 1:nlatent){
        asymDIFFUSION[individual][drowi,dcoli] <- asymDIFFUSIONvec[drowi+(dcoli-1)*nlatent];
        if(drowi > dcoli) asymDIFFUSION[individual][drowi,dcoli] <- asymDIFFUSION[individual][dcoli,drowi]; //symmetry enforcement
        }}
        '),
      if(continuoustime==FALSE) paste0('
          asymDIFFUSIONvec <- (diag_matrix(rep_vector(1, nlatent*nlatent)) - kron_prod(DRIFT[individual],DRIFT[individual])) * to_vector(DIFFUSION[individual]);
  for(drowi in 1:nlatent) {
  for(dcoli in 1:nlatent){
  asymDIFFUSION[individual][drowi,dcoli] <- asymDIFFUSIONvec[drowi+(dcoli-1)*nlatent];
  if(drowi > dcoli) asymDIFFUSION[individual][drowi,dcoli] <- asymDIFFUSION[individual][dcoli,drowi]; //symmetry enforcement
  }}
  '),'
      }
      
      
      
      
      
',if(kalman==FALSE) paste0('
        
        for(rowi in 1:ndatapoints){
        
        if(T0check[rowi]==1) {
        ',if(continuoustime==FALSE) paste0('
        discreteDRIFT<- DRIFT[subject[rowi]];
        discreteCINT<- CINT[subject[rowi]];
        discreteDIFFUSION <- DIFFUSION[subject[rowi]];
        '),'
        }
        
        if(T0check[rowi]==0 && continuoustime==1 && intervalChange[rowi]==1){
        discreteDRIFT<- expmp(DRIFT[subject[rowi]] * dT[rowi], padeC, padeCbig);
        discreteCINT<- invDRIFT[subject[rowi]] * (discreteDRIFT - IIlatent) * CINT[subject[rowi]];
        discreteDIFFUSION <- asymDIFFUSION[subject[rowi]] - quad_form_sym(asymDIFFUSION[subject[rowi]], discreteDRIFT\');
        }

        if(T0check[rowi] ==0 && T0check[rowi-1] ==1)  eta[rowi-subject[rowi]] ~ multi_normal(discreteDRIFT * T0MEANS[subject[rowi]] + discreteCINT, discreteDIFFUSION);
        if(T0check[rowi] ==0 && T0check[rowi-1] ==0)  eta[rowi-subject[rowi]] ~ multi_normal(discreteDRIFT * eta[rowi-1-subject[rowi]] + discreteCINT, discreteDIFFUSION);

        ',if(n.TDpred > 0) paste0('eta[rowi]<- eta[rowi] + TDPREDEFFECT[subject[rowi]] * tdpreds[rowi]; /n '),'
        
        if(T0check[rowi] ==0) Y[rowi]~ multi_normal(LAMBDA[subject[rowi]] * eta[rowi-subject[rowi]] + MANIFESTMEANS[subject[rowi]], MANIFESTVAR[subject[rowi]]);
        if(T0check[rowi] ==1) Y[rowi]~ multi_normal(LAMBDA[subject[rowi]] * T0MEANS[subject[rowi]] + MANIFESTMEANS[subject[rowi]], MANIFESTVAR[subject[rowi]]);
        }
        
        '),'
      
      
',if(kalman==TRUE) paste0('      
        
        // BEGIN KALMAN FILTER
        
        // set initial states
        //etapost[1] <- T0MEANS[1];
        //etapostcov[1] <- posdef(asymDIFFUSION[1]); //asymDIFFUSION[1];
        //Qmiss <- 0.5;
        
        for(rowi in 1:ndatapoints){
        
        vector[y_obs_n[rowi]] y_tmp;
        matrix[y_obs_n[rowi], nlatent] LAMBDA_tmp;
        vector[y_obs_n[rowi]] MANIFESTMEANS_tmp;
        matrix[y_obs_n[rowi], y_obs_n[rowi]] MANIFESTVAR_tmp;
        vector[y_obs_n[rowi]] ypred_tmp;      
        matrix[y_obs_n[rowi], y_obs_n[rowi]] ypredcov_tmp;
        
        vector[y_obs_n[rowi]] err;
        matrix[nlatent, y_obs_n[rowi]] K;
        matrix[y_obs_n[rowi], y_obs_n[rowi]] invypredcov;
        matrix[nlatent, nlatent] J;
        
        if(T0check[rowi] == 1) {

        ',if(continuoustime==TRUE) paste0('
        discreteDRIFT<- DRIFT[subject[rowi]];
        discreteCINT<- CINT[subject[rowi]];
        discreteDIFFUSION <- DIFFUSION[subject[rowi]];'),'

        etaprior[rowi] <- T0MEANS[subject[rowi]];
        ',if(n.TDpred > 0) paste0('//etaprior[rowi] <-TDPREDEFFECT[subject[rowi]] * tdpreds[rowi] + etaprior[rowi];'),'
        etapriorcov[rowi] <-  T0VAR[subject[rowi]];
        }
        
        if(T0check[rowi]==0){
        ',if(continuoustime==TRUE) paste0('
        if(intervalChange[rowi]==1){
        discreteDRIFT<- expmp(DRIFT[subject[rowi]] * dT[rowi], padeC, padeCbig);
        discreteCINT<- invDRIFT[subject[rowi]] * (discreteDRIFT - IIlatent) * CINT[subject[rowi]];
        discreteDIFFUSION <- asymDIFFUSION[subject[rowi]] - quad_form_sym(asymDIFFUSION[subject[rowi]], discreteDRIFT\');
        }'),'
        etaprior[rowi] <- discreteCINT  + discreteDRIFT * etapost[rowi];
        ',if(n.TDpred > 0) paste0('etaprior[rowi] <-TDPREDEFFECT[subject[rowi]] * tdpreds[rowi-1] + etaprior[rowi];'),'
        etapriorcov[rowi] <-  quad_form_sym(etapostcov[rowi], discreteDRIFT\')  + discreteDIFFUSION;
        }
        
        
        // what are these for? probably for missingness...
        //yprob[rowi] <-     rep_vector(0.0, nmanifest);
        //yprobcov[rowi] <-   diag_matrix(rep_vector(Qmiss, nmanifest));
        
        
        if (y_obs_n[rowi] == 0) {      // if all observations missing
        etapost[rowi + 1] <- etaprior[rowi];
        etapostcov[rowi + 1] <- etapriorcov[rowi];
        } else {
        
        if (y_obs_n[rowi] == nmanifest) {    // if no observations missing
        y_tmp <- Y[rowi];
        MANIFESTMEANS_tmp <- MANIFESTMEANS[subject[rowi]];
        LAMBDA_tmp <- LAMBDA[subject[rowi]];
        MANIFESTVAR_tmp <- MANIFESTVAR[subject[rowi]];
        
        } else {      // if at least one observation missing
        for (i in 1:y_obs_n[rowi]) {
        y_tmp[i] <- Y[rowi][y_obs_i[rowi][i]];
        MANIFESTMEANS_tmp[i] <- Y[rowi][y_obs_i[rowi][i]];
        for (j in 1:nlatent) {
        LAMBDA_tmp[i, j] <- LAMBDA[subject[rowi]][y_obs_i[rowi][i], j];
        }
        for (j in 1:y_obs_n[rowi]) {
        MANIFESTVAR_tmp[i, j] <- MANIFESTVAR[subject[rowi]][y_obs_i[rowi][i], y_obs_i[rowi][j]];
        }
        }
        }
        
        // update step 
        ypred_tmp <- MANIFESTMEANS_tmp + LAMBDA_tmp * etaprior[rowi];
        ypredcov_tmp <- quad_form_sym(etapriorcov[rowi], LAMBDA_tmp\') + MANIFESTVAR_tmp;
        
        // forecast error
        err <- y_tmp - ypred_tmp;
        
        invypredcov <- inverse_spd(ypredcov_tmp);

        // Kalman gain
          K <- etapriorcov[rowi] * LAMBDA_tmp\' * invypredcov; 
          J <- (IIlatent - K * LAMBDA_tmp);
        
        // posterior distribution 
        etapost[rowi + 1] <- etaprior[rowi] + K * err;
        
        //using simple form
        etapostcov[rowi + 1] <- J * etapriorcov[rowi];
        
        //using Joseph / stabilized form
        //etapostcov[rowi + 1] <- quad_form_sym(etapriorcov[rowi], J\') + quad_form_sym(MANIFESTVAR_tmp, K\');
        
        
        // -2 log likelihood
        increment_log_prob(-.5* (y_obs_n[rowi] * log(2 * pi())  + log_determinant(ypredcov_tmp)    + quad_form_sym(invypredcov, err))); // did use Qinv instead of ypredcov
        
        // fix this section for factor scores & missingness! currently doing nothing
        //      if (y_obs_n[rowi] == nmanifest) {
        //      yprob[rowi] <- ypred_tmp;
        //      yprobcov[rowi] <- ypredcov_tmp;
        //      
        //      } else {
        //      for (i in 1:y_obs_n[rowi]) {
        //      
        //      yprob[rowi][y_obs_i[rowi][i]] <- ypred_tmp[i];
        //      for (j in 1:y_obs_n[rowi]) {
        //      yprobcov[rowi][y_obs_i[rowi][i], y_obs_i[rowi][j]] <- ypredcov_tmp[i, j];
        //      }
        //      }
        //      }
        }
        }
        '),'
      
      
      
      
      
      
      
      
', if(optimize==TRUE && n.TIpred > 0) paste0('print("tipredeffect ",tipredeffect);'),'
', if(optimize==TRUE) paste0('
        print("hypersd",hypersd);
        print("DRIFT ", DRIFT);
        print("DIFFUSION ", DIFFUSION);
        print("T0VAR ", T0VAR);
        print("MANIFESTVAR ", MANIFESTVAR);
        print("T0MEANS[1] ", T0MEANS);
        print("CINT ", CINT);
        print("MANIFESTMEANS[1] ", MANIFESTMEANS);
        print("lp =",get_lp());
        '),'\n

      }
generated quantities{


}')
    
    
    out<-stanmodelobj
        
  
  #multiply_lower_tri_self_transpose
  
  
  datalong[,'dT'][datalong[,'dT']==0] <- .22
  
  if(fit==TRUE){
    standata<-list(
      Y=cbind(datalong[,ctmodelobj$manifestNames]),
      subject=datalong[,'id'],
      nsubjects=nsubjects,
      nmanifest=n.manifest,
      T0check=T0check,
      continuoustime=sum(continuoustime),
      nlatent=n.latent,
      ntipred=n.TIpred,
      ntdpred=n.TDpred,
      nparams=nparams,
      indvarying=as.numeric(indvarying),
      nindvarying=nindvarying,
      nhypersdparams=sum(indvarying[indvarying==TRUE]),
      IIparams = diag(nparams),
      ndatapoints=nrow(datalong),
      padeC=rbind(c(120, 60, 12, 1, 0, 0, 0, 0, 0, 0), c(30240, 
        15120, 3360, 420, 30, 1, 0, 0, 0, 0), c(17297280, 
          8648640, 1995840, 277200, 25200, 1512, 56, 1, 0, 
          0), c(17643225600, 8821612800, 2075673600, 302702400, 
            30270240, 2162160, 110880, 3960, 90, 1)),
      padeCbig= c(64764752532480000, 32382376266240000, 7771770303897600, 
        1187353796428800, 129060195264000, 10559470521600, 
        670442572800, 33522128640, 1323241920, 40840800, 
        960960, 16380, 182, 1),
      dT=array(datalong[,'dT'],dim=nrow(datalong)),
      intervalChange=array(datalong[,'intervalChange'],dim=nrow(datalong)),
      y_obs_n=array(apply(datalong[,ctmodelobj$manifestNames,drop=FALSE],1,function(x) length(x[x!=99999])),dim=nrow(datalong)),
      y_obs_i=matrix(t(apply(datalong[,ctmodelobj$manifestNames,drop=FALSE],1,function(x) {
        out<-as.numeric(which(x!=99999))
        if(length(out)==0) out<-rep(0,n.manifest)
        if(length(out)<n.manifest) out<-c(out,rep(0,n.manifest-length(out)))
        out
      }) ),nrow=c(nrow(datalong),ncol=n.manifest)))
    
    if(n.TIpred > 0) standata<-c(standata,list(tipreds=array(tipreds,dim=c(nrow(tipreds),ncol(tipreds)))))
    if(n.TDpred > 0) standata<-c(standata,list(tdpreds=array(tdpreds,dim=c(nrow(tdpreds),ncol(tdpreds)))))
    
    # browser()
    
    # ctmodelparams <- unlistCtModel(ctmodelobj=ctmodelobj)
    # ctmodelparams<-ctmodelparams[is.na(ctmodelparams[,'value']),]
    # 
    # for(rowi in 1:nrow(ctmodelparams)){
    #   if(ctmodelparams[rowi,'matrix'] %in% c('DIFFUSION','MANIFESTVAR', 'T0VAR')){
    #     if(ctmodelparams[rowi,'row'] != ctmodelparams[rowi,'col']) ctmodelparams[rowi,'value']<- 0
    #     if(ctmodelparams[rowi,'row'] == ctmodelparams[rowi,'col']) ctmodelparams[rowi,'value']<- 1
    #   }
    #   if(ctmodelparams[rowi,'matrix'] %in% c('T0MEANS','CINT','MANIFESTMEANS', 'TDPREDEFFECT')){
    #     ctmodelparams[rowi,'value']<- 0 #meanmin + meanrange*ctmodelparams[rowi,'value']
    #   }
    #   if(ctmodelparams[rowi,'matrix'] %in% c('DRIFT')){
    #     if(ctmodelparams[rowi,'row'] != ctmodelparams[rowi,'col']) ctmodelparams[rowi,'value']<- 0
    #     if(ctmodelparams[rowi,'row'] == ctmodelparams[rowi,'col'] & continuoustime==TRUE) ctmodelparams[rowi,'value']<- -.2
    #     if(ctmodelparams[rowi,'row'] == ctmodelparams[rowi,'col'] & continuoustime==FALSE) ctmodelparams[rowi,'value']<- .8
    #   }
    # }
    # 
    # inits<-list()
    # for(chaini in 1:chains){
    #   inits[[chaini]]<-list()
    #   inits[[chaini]]$hypermeans<- ctmodelparams[ match(paramsIndex[,2],ctmodelparams$param),'value'] * rnorm(nparams,1,.1)
    #   if(any(indvarying==TRUE)) {
    #     for(hypersdi in 1:length(paramsIndex[indvarying==TRUE,2])){
    #       inits[[chaini]]<-c(inits[[chaini]],rnorm(1,.4,.01))
    #       names(inits[[chaini]])[hypersdi+1]<-paste0('hsd_',paramsIndex[indvarying==TRUE,2][hypersdi])
    #     }
    #     nhypersdparams<-sum(indvarying==TRUE)
    #     inits[[chaini]]$hypercholcorrpars<- array(rnorm((nhypersdparams * nhypersdparams-nhypersdparams)/2,0, 0))
    #     
    #     if(noncentered==TRUE) inits[[chaini]]$indparamsnoisebase<-array(rnorm(nhypersdparams*nsubjects,0,.01))
    #   }
    #   if(noncentered==FALSE) inits[[chaini]]$indparams<-matrix(inits[[chaini]]$hypermeans,byrow=T,ncol=nparams,nrow=nsubjects)
    #   if(n.TIpred > 0) inits[[chaini]]$tipredeffect<-matrix(0,nrow=nparams,ncol=n.TIpred)
    # }
    # print(inits[[1]])
    # # 
    # control<-list(stepsize=.001, stepsize_jitter=0,metric='dense_e', algorithm='HMC', adapt_engaged=F)
    control<-list(adapt_delta=.8,
      # adapt_term_buffer=25, adapt_init_buffer=40,adapt_window=10,
      # metric="dense_e",
      max_treedepth=8,stepsize=1)
    
    inits<-list()
    inits[[1]]<-list()
    out <- stan(model_code = c(stanmodelobj), 
      # init=0,
      enable_random_init=T,
      refresh=10,
      data = standata, iter = iter, chains = ifelse(optimize==FALSE & vb==FALSE,chains,0), verbose = verbose, control=control,
      # open_progress = TRUE,
      cores=max(c(chains,detectCores()))) 
    
#     browser()
#     
#     
#     
#     
#     
#     
#     ctStanSummary<-function(){
#       params <- unlistCtModel(ctmodelobj=ctmodelobj)
#       params<-cbind(params,matrix(NA,ncol=6,dimnames=list(c(),c('hypermean', 'hypermean_lower', 'hypermean_upper','hypersd','hypersd_lower','hypersd_upper'))))
#       summ<-summary(out)$summary
#       hypermeans<-summ[,c('mean','2.5%','97.5%')][grepl('hypermeans',rownames(summ)),]
#       # hypermeans<-1/(1+exp(- hypermeans))
#       params[match(paramsIndex[,2], params$param),6:8] <- hypermeans
#       hypermeanindices<-6:8
#       
#       if(variableParams==TRUE){
#         hypersd<-summ[,c('mean','2.5%','97.5%')][grepl('hypersd',rownames(summ)),]
#         params[match(paramsIndex[,2], params$param),9:11] <- hypersd
#       }
#       
#       if(n.TIpred > 0){
#         tipreds<-summ[,c('mean','2.5%','97.5%')][grepl('tipredeffect',rownames(summ)),]
#         tipredeffect<-exp(-exp(log(-log(hypermeans[,1]))+matrix(tipreds,ncol=n.TIpred*3,byrow=T))) - hypermeans[,1]
#         colnames(tipredeffect)<-paste0(ctmodelobj$TIpredNames,'_effect_',
#           rep(colnames(tipreds),each=n.TIpred))
#         
#         tipredmat<-matrix(NA,nrow=nrow(params),ncol=n.TIpred*3)
#         colnames(tipredmat)<-colnames(tipredeffect)
#         params<-cbind(params,tipredmat)
#         params[match(paramsIndex[,2], params$param),(ncol(params)+1-n.TIpred*3):(ncol(params))] <- tipredeffect
#         # transformindices<-c(transformindices,(ncol(params)+1-n.TIpred*3):(ncol(params)))
#         
#         tipredeffect<-params[,c(1:5,(ncol(params)+1-n.TIpred*3):(ncol(params)))]
#         params<-params[,-(ncol(params)+1-n.TIpred*3):-(ncol(params))]
#         tipredeffectindices<-col(tipredeffect[1,])[,-1:-5]
#       }
#       
#       
#       for(rowi in 1:nrow(params)){
#         if(params[rowi,'matrix'] %in% c('DIFFUSION','MANIFESTVAR', 'T0VAR')){
#           if(params[rowi,'row'] != params[rowi,'col']) params[rowi,hypermeanindices]<- corlinkinv(params[rowi,hypermeanindices])
#           if(params[rowi,'row'] == params[rowi,'col']) params[rowi,hypermeanindices]<- varlinkinv(params[rowi,hypermeanindices])
#         }
#         if(params[rowi,'matrix'] %in% c('T0MEANS','CINT','MANIFESTMEANS', 'TDPREDEFFECT')){
#           params[rowi,hypermeanindices]<- meanlinkinv(params[rowi,hypermeanindices])
#         }
#         if(params[rowi,'matrix'] %in% c('DRIFT')){
#           if(params[rowi,'row'] != params[rowi,'col']) params[rowi,hypermeanindices]<- crlinkinv(params[rowi,hypermeanindices] )
#           if(params[rowi,'row'] == params[rowi,'col']) params[rowi,hypermeanindices]<- arlinkinv(params[rowi,hypermeanindices])
#         }
#       }
#       params[,hypermeanindices]<-round(params[,hypermeanindices],2)
#       ctsummary<-list(params)
#       
#       if(n.TIpred > 0){
#         for(rowi in 1:nrow(tipredeffect)){
#           if(tipredeffect[rowi,'matrix'] %in% c('DIFFUSION','MANIFESTVAR', 'T0VAR')){
#             if(tipredeffect[rowi,'row'] != tipredeffect[rowi,'col']) tipredeffect[rowi,tipredeffectindices]<- corrange*(tipredeffect[rowi,tipredeffectindices])
#             if(tipredeffect[rowi,'row'] == tipredeffect[rowi,'col']) tipredeffect[rowi,tipredeffectindices]<- varrange*tipredeffect[rowi,tipredeffectindices]
#           }
#           if(tipredeffect[rowi,'matrix'] %in% c('T0MEANS','CINT','MANIFESTMEANS','TDPREDEFFECT')){
#             tipredeffect[rowi,tipredeffectindices]<- meanrange*(tipredeffect[rowi,tipredeffectindices])
#           }
#           if(tipredeffect[rowi,'matrix'] %in% c('DRIFT')){
#             if(tipredeffect[rowi,'row'] != tipredeffect[rowi,'col']) tipredeffect[rowi,tipredeffectindices]<- driftcrossrange*(tipredeffect[rowi,tipredeffectindices] )
#             # if(tipredeffect[rowi,'row'] == tipredeffect[rowi,'col']) tipredeffect[rowi,tipredeffectindices]<- log(driftarmin+tipredeffect[rowi,tipredeffectindices])
#           }
#         }
#         tipredeffect[,tipredeffectindices]<-round(tipredeffect[,tipredeffectindices],2)
#         ctsummary<-list(params,tipredeffect)
#         
#       }
#       return(ctsummary)
#     }#END CTSTAN SUMMARY
#     
    
    
    
    
    
    
    
    if(optimize==FALSE & vb==FALSE){ #summarise
      # ctsummary<-ctStanSummary()
      out<-list(stanmodel=stanmodelobj,fit=out)
    }
    
    if(optimize==TRUE && fit==TRUE) {
      out <- optimizing(object = out@stanmodel, 
        init=0,
        # algorithm='BFGS',
        # init=inits[[1]],
        as_vector=F,
        history_size=10,
        # init_alpha=.001,
        tol_obj=1e-12, tol_grad=1e-12,tol_param=1e-12,tol_rel_grad=0, tol_rel_obj=0,
        data = standata, iter=120000,verbose = verbose)
      
      out<-list(stanmodel=stanmodelobj,optimized=out)
      
      
    }
    
    if(vb==TRUE && fit==TRUE) {
      # message('disabled inits')
      out <- vb(object = out@stanmodel, 
        iter=iter,
        # init=inits[[1]],
        data = standata,...)
      
      # ctsummary<-ctStanSummary()
      
      out<-list(stanmodel=stanmodelobj,vb=out)
      
    }
    
  } # end if fit==TRUE
  
  return(out)
      }

require(assertthat)
require(psych)
require(lme4) #for icc function

#View(scoredDfCTT(score(csv)))
scoredDfCTT<-function(x){
	require(CTT)
	scores<-score(x,output.scored=TRUE,rel=T)
	scoresdf<-data.frame(lapply(scores$reliability,function(x){cbind(x)}))#FOR VIEWING ONLY
	colnames(scoresdf)<-c("nItem","nPerson","alpha","scaleMean","scaleSD","alphaIfDeleted","pBis","Bis","itemMean")
	scoresdf
}

#https://www.wwnorton.com/college/psych/psychsci/media/rosenberg.htm
rsescore<-function(x,alpha=F){
  require(psych)
  assert_that(dim(x)[2]==10)
  x<-sapply(x,as.double) #convert to numbers
  assert_that(all(x<=5,na.rm=T))
  assert_that(all(x>=1,na.rm=T))
  my.keys <- make.keys(nvars=10,list(RSE10=c(1,2,-3,4,-5,6,7,-8,-9,-10)))
  my.scales <- scoreItems(my.keys,x,totals=T,missing=T) #missing=F so no imputation
  my.scores <- my.scales$scores
  colnames(my.scores)<-"rse"
  ifelse(alpha==F,return(my.scores),return(as.data.frame(t(my.scales$alpha))))
}

hsnsscore<-function(df,alpha=F){
  assert_that(dim(df)[2] == 10)
  #df<-sapply(df,as.double) #convert to numbers
  assert_that(all(df<7,na.rm=T))
  my.keys <- make.keys(nvars=10,list(
    HSNS=c(1,2,3,4,5,6,7,8,9,10)
  ))
  my.scales <- scoreItems(my.keys,df)
  #my.scales #alpha, correlation b/n attributes
  #t(my.scales$alpha)
  my.scores <- my.scales$scores
  colnames(my.scores)<-"hsns"
  ifelse(alpha==F,return(my.scores),return(as.data.frame(t(my.scales$alpha))))
}

ipip50score<-function(df,alpha=F){
  assert_that(dim(df)[2] == 50)
  assert_that(all(df<7,na.rm=T))
  my.keys <- make.keys(nvars=50,list(
  	ipip50.Openness=c(5,-10,15,-20,25,-30,35,40,45,50),
  	ipip50.Conscientiousness=c(3,-8,13,-18,23,-28,33,-38,43,48),
  	ipip50.Extraversion=c(1,-6,11,-16,21,-26,31,-36,41,-46),
  	ipip50.Agreeableness=c(-2,7,-12,17,-22,27,-32,37,42,47),
  	ipip50.Neuroticism=c(-4,9,-14,19,-24,-29,-34,-39,-44,-49)
  ))
  my.scales <- scoreItems(my.keys,df)
  #my.scales #alpha, correlation b/n attributes
  #t(my.scales$alpha)
  my.scores <- my.scales$scores
  colnames(my.scores)<-c("ipip50.O","ipip50.C","ipip50.E","ipip50.A","ipip50.N")
  ifelse(alpha==F,return(my.scores),return(as.data.frame(t(my.scales$alpha))))
}

npi40score<-function(npi,alpha=F){
  assert_that(dim(npi)[2] == 40)
  assert_that(all(npi<=3,na.rm=T))
  assert_that(all(npi>0,na.rm=T))
  require(psych)
  
npikey<-c(1, 1, 1, 2, 2, 1, 2, 1, 2, 2, 1, 1, 1, 1, 2, 1, 2, 2, 2, 2, 1, 2, 2, 1, 1, 2, 1, 2, 1, 1, 1, 2, 1, 1, 2, 1, 1, 1, 1, 2)
npiscores<-score.multiple.choice(npikey, npi, score=FALSE, total=TRUE, missing=FALSE, digits=2, short=FALSE) #evaluates whether matches key or not. returns 1 if so, otherwise 0

my.keys <- make.keys(nvars=40,list(npi=c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36,37,38,39,40))) #matches key if 1, anything else doesnt matter
my.scales <- scoreItems(my.keys,npiscores,totals=T)
#my.scales #alpha, correlation b/n attributes
my.scores <- my.scales$scores
colnames(my.scores)<-"npi40"
ifelse(alpha==F,return(my.scores),return(as.data.frame(t(my.scales$alpha))))
}

npi16score<-function(npi,alpha=F){
  assert_that(dim(npi)[2] == 16)
  assert_that(all(npi<3,na.rm=T))
  npikey<-c(1,2,1,2,2,1,2,1,1,2,1,2,2,1,2,1)
  my.keys <- make.keys(nvars=16,list(npi=c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16))) #matches key if 1, anything else doesnt matter
  my.scales <- scoreItems(my.keys,npi,totals=T)
  my.scores <- my.scales$scores
  colnames(my.scores)<-"npi16"
  ifelse(alpha==F,return(my.scores),return(as.data.frame(t(my.scales$alpha))))
  }

bfi44score<-function(x,alpha=F){
  assert_that(dim(x)[2] == 44)
  assert_that(all(x<6,na.rm=T))
  my.keys <- make.keys(nvars=44,list(
    bfi44.Openness=c(5,10,15,20,25,30,-35,40,-41,44),
    bfi44.Conscientiousness=c(3,-8,13,-18,-23,28,33,38,-43),
    bfi44.Extraversion=c(1,-6,11,16,-21,26,-31,36),
    bfi44.Agreeableness=c(-2,7,-12,-17,22,-27,32,-37,42),
    bfi44.Neuroticism=c(4,-9,14,19,-24,29,-34,39)
  ))
  my.scales <- scoreItems(my.keys,x)#BFI
  #my.scales #alpha, correlation b/n attributes
  #t(my.scales$alpha)
  my.scores <- my.scales$scores
  colnames(my.scores)<-c("bfi44.O","bfi44.C","bfi44.E","bfi44.A","bfi44.N")
  ifelse(alpha==F,return(my.scores),return(as.data.frame(t(my.scales$alpha))))
}

ffmrfscore<-function(csv,alpha=F){
  require(assertthat)
  require(psych)
  #assert_that(all(is.numeric(csv)==T,na.rm=T))
  assert_that(dim(csv)[2] == 30)
  assert_that(all(csv<6,na.rm=T))
  assert_that(all(csv>0,na.rm=T))
  my.keys <- make.keys(nvars=dim(csv)[2],list(
    Openness=c(13:18),
    Conscientiousness=c(25:30),
    Extraversion=c(7:12),
    Agreeableness=c(19:24),
    Neuroticism=c(1:6)
  ))
  my.scales <- scoreItems(my.keys,csv)
  #my.scales #alpha, correlation b/n attributes
  #t(my.scales$alpha)
  my.scores <- as.data.frame(my.scales$scores)
  colnames(my.scores)<-c("Openness","Conscientiousness","Extraversion","Agreeableness","Neuroticism")
  ifelse(alpha==F,return(my.scores),return(as.data.frame(t(my.scales$alpha))))
}

tipiscore<-function(x){
  # The reverse scored items are 2, 4, 6, 8, & 10.
  #x<-sapply(x,is.numeric)
  require(assertthat)
  assert_that(dim(x)[2] == 10)
  assert_that(all(is.numeric(x),na.rm=T))
  see_if(all(x<8,na.rm=T))
  assert_that(all(x<8,na.rm=T))
  ret<-data.frame(
    extraversion=c(rowSums(cbind(x[,1],(8-x[,6])),na.rm=FALSE)),
    agreeableness=c(rowSums(cbind(x[,7],(8-x[,2])),na.rm=FALSE)),
    conscientiousness=c(rowSums(cbind(x[,3],(8-x[,8])),na.rm=FALSE)),
    emo_stab=c(rowSums(cbind(x[,9],(8-x[,4])),na.rm=FALSE)),
    openness=c(rowSums(cbind(x[,5],(8-x[,10])),na.rm=FALSE))
  )
  return(ret)
}

#http://www.persoc.net/persoc/uploads/Toolbox/NARQ_English.pdf
narq<-function(x,alpha=F){
  require(assertthat)
  assert_that(dim(x)[2] == 18)
  #assert_that(all(is.numeric(x),na.rm=T))
  assert_that(all(x<=6,na.rm=T))
  #return(narq)
  my.keys <- make.keys(nvars=18,list(
    narc_score=c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18),
    admiration=c(1,2,3,5,7,8,15,16,18),
    rivalry=c(4,6,9,10,11,12,13,14,17)
  ))
  my.scales <- scoreItems(my.keys,x)
  #my.scales #alpha, correlation b/n attributes
  #t(my.scales$alpha)
  my.scores <- my.scales$scores
  ifelse(alpha==F,return(my.scores),return(as.data.frame(t(my.scales$alpha))))
}

sd3_score<-function(x){
  require(assertthat)
  require(psych)
  assert_that(dim(x)[2] == 27)
  assert_that(all(x<=5,na.rm=T))
  assert_that(all(x>=1,na.rm=T))

  sd3<-NULL
  sd3$triadmachiavelli<-rowSums(reverse.code(c(1,1,1,1,1,1,1,1,1),x[,1:9],mini=1,maxi=5),na.rm=FALSE)
  sd3$triadnarc<-rowSums(reverse.code(c(1,-1,1,1,1,-1,1,-1,1),x[,10:18],mini=1,maxi=5),na.rm=FALSE)
  sd3$triadpsychopathy<-rowSums(reverse.code(c(1,-1,1,1,1,1,-1,1,1),x[,19:27],mini=1,maxi=5),na.rm=FALSE)
  return(sd3)
}

###############################################################################
#Functions to View data
##############################################################################

#example: apaCorr2(data.frame(sample.int(20,10,replace=T),sample.int(20,10,replace=T)))
apaCorr2<-function(df,round_digits=2){
  require("psych")
  cors<-corr.test(df)
  res <- cors$p
  res[] <- paste0(round(cors$r,digits=round_digits),ifelse(res<=0.001,'***',ifelse(res<=.01,'**',ifelse(res<=.05,'*',''))))
  res[upper.tri(res, diag=TRUE)] <- "--"  ####THIS IS A FOR A REASON.... otherwise if NA dimensions are wrong because first row gets dropped
  pc<-res[!!rowSums(res!=''),!!colSums(res!='')]
  #return(pc)
  temp<-paste0(round(colMeans(df),2),"(",round(apply(df,2,sd),2),")")
  temp<-cbind(temp)
  colnames(temp)<-"mean(SD)"
  df<-data.frame(temp,pc)
  return(df)
}

#get p-value from linear model
lmp <- function (modelobject) {
  if (class(modelobject) != "lm") stop("Not an object of class 'lm' ")
  f <- summary(modelobject)$fstatistic
  p <- pf(f[1],f[2],f[3],lower.tail=F)
  attributes(p) <- NULL
  return(p)
}

#takes in linear model outputs: F(dfb,dfw)=fobt,p-val
#x<-lm(csv$bfat~csv$exer) #example x input
prettyF<-function(x){
  f<-summary(x)$fstatistic
  s<-summary(x)$coeff
  rn<-row.names(s)
  rr<<-0
  apply(s,1,function(x){
    dfb<-f[2]
    dfw<-f[3]
    fval<-round(x[3]^2,4)
    pval<-round(x[4],4)
    rr<<-rr+1
    return(cat(rn[rr]," F(",f[2],",",f[3],")=",fval,",",pval,"\n",sep=""))
  })
}

#######get semi-partial correlation
######## example call: semi_partial_df(res,"gender")

#partial- How much of the Y variance which is not estimated by the other IVs in the equation is estimated by this variable? 
#Partial correlation is the correlation of two variables while controlling for a third variable.
#(specific IV variance)/(100-(variance acocunted for in other IV's))
#semi-partial (aka part)- unique contribution of 1 IV.
#Semi-partial correlation is the correlation of two variables with variation from a third variable removed only from the second variable.
#(specific IV variance)/100
partial_df<-function(x,cont){
  library(ppcor)
  cont<<-cont
  x<<-x[complete.cases(x),] #semi-partial func below requires complete cases
  cols<-cbind(colnames(x))
  cols<-cols[-c(grep(cont,cols))]
  result_vect<-Vectorize(partial_corr)
  res<-outer(cols,cols,result_vect)
  colnames(res)<-cols
  row.names(res)<-cols
  ##REMOVE UPPER TRI
  res[upper.tri(res, diag=TRUE)] <- ''
  res<-res[-c(1),-c(length(cols))]
  ##
  return(res)
}

#helper funct. dont call directly. must set x[,cont]
partial_corr<-function(a,b){
  library(ppcor)
  tryCatch( #errors caused by matrix not singular in most cases
    {
      rr<-round(pcor.test(x[a], x[b], x[,cont])$estimate,2)
      #return(rr)
      pv<-pcor.test(x[a], x[b], x[,cont])$p.value
      pv<-round(pv,2)
      return(paste0(rr,"(",pv,")",ifelse(pv<=0.001,'***',ifelse(pv<=.01,'**',ifelse(pv<=.05,'*','')))))
    },
    error=function(cond){
      #message(paste('error:',cond,"cols: ",x[a], x[b],x[,cont],"\n"))
      return(NA)
    }
  )
}


#calculate icc1 from NLME model
my.nlme.icc1<-function(x){
  vals<-as.numeric(VarCorr(x)[,1])
  vals[1]/(vals[1]+vals[length(vals)])#intercept varaince/(intercept varaince+residual varaince)
}

#presents output of random effects (NOT FIXED) from an nlme linear model
view.nlme.model<-function(x){
  a<-summary(x)$coefficients$fixed[1] #Fixed Intercept
  b<-VarCorr(x)[nrow(VarCorr(x)),1]#Within-person variance :Rand residual ^2
  c<-VarCorr(x)[1,1]#between-person varaince: Intercept resid ^2
  d<-summary(x)$logLik
  e<-summary(x)$AIC #AIC
  f<-summary(x)$BIC #BIC
  res<-cbind(a,b,c,d,e,f)
  res<-as.numeric(res)
  res<-round(res,4)
  res<-rbind(res)
  colnames(res)<-c("Fixed Intercept","Within-person Random","Between-person Random","Log Likelihood","AIC","BIC")
  t(res)
}

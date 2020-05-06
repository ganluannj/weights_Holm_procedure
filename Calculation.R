
Calculation<-function(w1list,mu1, mu2=0, mu3, mu4=0, sigma1=1, sigma2=1,
                  sigma3=1, sigma4=1, N, alpha=0.05, Method='OF')
{ 
  library(gsDesign)
  library(mvtnorm)
  LEN=length(w1list)
  # generate a dataframe to store the result
  # value in Result represent the probability
  Result<-data.frame('w1'=w1list, 'w2'=1-w1list, 'Rej_both'=rep(0, LEN), 
                     'Rej_1_only'=rep(0, LEN), 'Rej_2_only'=rep(0, LEN),
                     'No_rej'=rep(0, LEN),
                     'Rej_atleast_one'=rep(0, LEN),'Rej_1'=rep(0,LEN), 
                     'Rej_2'=rep(0, LEN))
  
  Delta1=mu1-mu2
  Delta2=mu3-mu4
  sigmatilde1=sqrt(sigma1^2+sigma2^2)
  sigmatilde2=sqrt(sigma3^2+sigma4^2)
  Mean1=c(sqrt(N)*Delta1/(sqrt(2)*sigmatilde1), sqrt(N)*Delta1/sigmatilde1)
  Mean2=c(sqrt(N)*Delta2/(sqrt(2)*sigmatilde2), sqrt(N)*Delta2/sigmatilde2)
  V<-c(1, sqrt(1/2), sqrt(1/2),1)
  M<-matrix(V, nrow=2)
  B<-gsDesign(k=2, alpha=alpha,test.type=1,sfu=Method)$upper$bound
  
  for (i in 1:LEN){
    w1=w1list[i]
    w2=1-w1
    B1<-gsDesign(k=2, alpha=alpha*w1,test.type=1,sfu=Method)$upper$bound
    B2<-gsDesign(k=2, alpha=alpha*w2,test.type=1,sfu=Method)$upper$bound
    
    ###############################################################################
    ############### Reject at least one ##########################################
    ###############                     #########################################
    ###########################################################################
    
    ## probability of not rejecting hypothesis 1
    p1<-pmvnorm(lower=c(-Inf, -Inf), upper = B1, mean=Mean1, sigma=M)[[1]]
    ## probability of not rejecting hypothesis 2
    p2<-pmvnorm(lower=c(-Inf, -Inf), upper = B2, mean=Mean2, sigma=M)[[1]]
    ## probability of rejecting at least one
    patleast1<-1-p1*p2
    Result[i,'Rej_atleast_one']<-patleast1
    
    ###############################################################################
    ############### Reject only 1 (A)     ##########################################
    ###############                     #########################################
    ###########################################################################
    
    ## first the probabilty of rejecting hypothesis 1 at middle
    p1<-pmvnorm(lower=c(B1[1], -Inf), upper = c(Inf, Inf), mean=Mean1, sigma=M)[[1]]
    p2<-pmvnorm(lower=c(-Inf, -Inf), upper = B, mean=Mean2, sigma=M)[[1]]
    pmid<-p1*p2
    ## first the probabilty of rejecting hypothesis 1 at final
    p1<-pmvnorm(lower=c(-Inf, B1[2]), upper = c(B1[1], Inf), mean=Mean1, sigma=M)[[1]]
    p2<-pmvnorm(lower=c(-Inf, -Inf), upper = c(B2[1], B[2]), mean=Mean2, sigma=M)[[1]]
    pfinal<-p1*p2
    ponlyA<-pmid+pfinal
    Result[i,'Rej_1_only']<-ponlyA
    
    ###############################################################################
    ############### Reject only 2 (B)       ##########################################
    ###############                     #########################################
    ###########################################################################
    
    ## first the probabilty of rejecting hypothesis 2 at middle
    p1<-pmvnorm(lower=c(-Inf, -Inf), upper = B, mean=Mean1, sigma=M)[[1]]
    p2<-pmvnorm(lower=c(B2[1], -Inf), upper = c(Inf, Inf), mean=Mean2, sigma=M)[[1]]
    pmid<-p1*p2
    ## first the probabilty of rejecting hypothesis 2 at final
    p1<-pmvnorm(lower=c(-Inf, -Inf), upper = c(B1[1], B[2]), mean=Mean1, sigma=M)[[1]]
    p2<-pmvnorm(lower=c(-Inf, B2[2]), upper = c(B2[1], Inf), mean=Mean2, sigma=M)[[1]]
    pfinal<-p1*p2
    ponlyB<-pmid+pfinal
    Result[i,'Rej_2_only']<-ponlyB
    
    #################################
    Rejboth<-patleast1-ponlyA-ponlyB
    Rej1<-ponlyA+Rejboth
    Rej2<-ponlyB+Rejboth
    Result[i,'Rej_both']<-Rejboth
    Result[i,'Rej_1']<-Rej1
    Result[i,'Rej_2']<-Rej2
    Result[i, 'No_rej']<-1-patleast1
  }
  
  # reshape Result for plotting
  library("tidyverse")
  Result<-Result %>%
    gather(key = "variable", value = "probability", -w1, -w2)
  return(Result)
}


#w1list=c(0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9)
w1list=c(0.1,0.2)
Result<-Calculation(w1list=w1list, mu1=0.2, mu2=0, mu3=0.2, mu4=0, sigma1=1, sigma2=1,
                    sigma3=1, sigma4=1, N=150, alpha=0.05, Method='OF')

library(ggplot2)
# for example we want to plot probability of 
# reject both and probability of 
# rejecting at least one together

#df<-Result[Result$variable %in% c('Rej_1', 'Rej_2'),]
df<-Result[Result$variable %in% c('Rej_both', 'Rej_atleast_one'),]
p<-ggplot(df, aes(x=w1, y=probability))
p<-p+geom_line(aes(colour=variable), size=1.2)
p<-p+geom_point(aes(colour=variable), size=3)
p<-p+ggtitle("Probability vs weight A")
p<-p+theme(plot.title = element_text(hjust = 0.5),
           legend.text = element_text(size=12),
           legend.position=c(0.5, 0.1),
           legend.direction = "vertical",
           axis.text=element_text(size=14),
           axis.title=element_text(size=14))
p<-p+theme(legend.title = element_blank()) 
p<-p+theme(axis.title.x = element_text(size=14))
p<-p+xlab('Weight A')
p<-p+theme(axis.title.y = element_text(size=14))
p<-p+ylim(0,1)
p<-p+scale_color_discrete(labels = c("Reject at least one hypothesis", "Reject both hypotheses"))
# ggsave(filename = paste0('mu1_0.2_mu3_0.2_p1.png'), 
#        plot=p, width=4, height=6, units='in')
p




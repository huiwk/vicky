lib<-function(){
  library(readxl)
  library(data.table)
  library(tidyverse)
  library(Hmisc)
}

lib()

# 1. variable type -----------------------------------------------------------

# -> categorical variable ----------------------------------------------------

is.categorical.variable<-function(vec,max.categories=10){
  vec%>%{if (!is.data.frame(vec)) tibble(vec=.) else .}%>%
    mutate_all(as.factor)%>%map_lgl(~.x%>%levels%>%length%>%{.>0&.<=max.categories})
}

# empty variable ----------------------------------------------------------

is.empty.variable<-function(vec){
  vec%>%{if (!is.data.frame(vec)) tibble(vec=.) else .}%>%
    mutate_all(as.factor)%>%map_lgl(~.x%>%levels%>%length%>%{.==0})
}

# 2.Normality check ---------------------------------------------------------

# -> for continuous variable only --------------------------------------------
standardisation<-function(vec){
  df<-vec%>%{if (!is.data.frame(vec)) tibble(vec=.) else .}
  map(df,~{
    eachcolumn<-.x
    if(!is.categorical.variable(eachcolumn)&!is.empty.variable(eachcolumn)) {
      mean<-mean(eachcolumn,na.rm=TRUE)
      sd<-sd(eachcolumn,na.rm=TRUE)
      ((eachcolumn-mean)/sd)
    }
  })%>%reduce(c)
}

is.normal<-function(vec){
  df<-vec%>%{if (!is.data.frame(vec)) tibble(vec=.) else .}
  map(df,~{
    standardised.column<-standardisation(.x)
    if(!is.null(standardised.column)){
      ks.test(standardised.column,'pnorm')$p.value%>%{.>=0.05}
    } else NULL
  })
}

# weighted functions in Hmisc ---------------------------------------------


# weighted mean + sd ------------------------------------------------------

weighted.mean.sd<-function(vec,flag=is.normal(vec),weight=NULL,fieldwidth=5,precision=2,p1="±",p2=""){
  map2(vec,flag,~{
    eachvec<-{if(is.data.frame(.x)) .x%>%reduce(c) else .x}
    eachflag<-.y
    if(!length(eachflag)) return(NULL)
    if(eachflag){
        c(wtd.mean(eachvec,weight),sqrt(wtd.var(eachvec,weight)))%>%
          map_chr(~sprintf(paste0("%",fieldwidth,".",precision,"f"),.x)%>%trimws)%>%{paste0(.[1],p1,.[2],p2)}
    }
    
  })
}

# weighted iqr ------------------------------------------------------------

weighted.inter.quartile.range<-function(vec,flag=!is.normal(vec),weight=NULL,fieldwidth=5,precision=2,p1="[",p2=",",p3="]"){
  map2(vec,flag,~{
    eachvec<-{if(is.data.frame(.x)) .x%>%reduce(c) else .x}
    eachflag<-.y
    if(!length(eachflag)) return(NULL)
    if(eachflag){
      c(wtd.quantile(eachvec,weight,probs=c(.25,.5,.75)))%>%
        map_chr(~sprintf(paste0("%",fieldwidth,".",precision,"f"),.x)%>%trimws)%>%{paste0(.[1],p1,.[2],p2,.[3],p3)}
    }
    
  })
}

# weighted count + pct ---------------------------------------------------- (continue)

weighted.cnt.pct<-function(vec,flag=is.categorical.variable(vec),weight=NULL,fieldwidth=5,precision=2,p1="(",p2=")"){
  map2(vec,flag,~{
    eachvec<-{if(is.data.frame(.x)) .x%>%reduce(c) else .x}%>%{if(!is.factor(.)) as.factor(.) else .}
    eachflag<-.y
    eachlevel<-eachvec%>%levels()
    if(!length(eachflag)) return(NULL)
    if(eachflag){
      map(eachlevel,~{
        nestedlevel<-.x
        cnt<-if(!length(weight)) sum(weight[eachlevel%in%nestedlevel],na.rm=TRUE) else sum(eachlevel%in%nestedlevel,na.rm=TRUE)
        
      })
      c(wtd.quantile(eachvec,weight,probs=c(.25,.5,.75)))%>%
        map_chr(~sprintf(paste0("%",fieldwidth,".",precision,"f"),.x)%>%trimws)%>%{paste0(.[1],p1,.[2],p2,.[3],p3)}
      
    }
    
  })
}



# missing -----------------------------------------------------------------



# hypothesis testing ------------------------------------------------------


# test case ---------------------------------------------------------------

max.categories<-10
cohort<-fread("E:/matched_ch_adult_wv14_0518_rt2.csv")%>%select(contains(c("baseline","weights"))&!contains("date"))%>%tibble
cohort%>%mutate_all(as.factor)%>%map(~.x%>%levels)

weighted.mean.sd(cohort,weight=cohort$weights)
weighted.inter.quartile.range(cohort[,1:2],c(1,1),weight=cohort$weights)

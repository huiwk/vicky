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
  weight<-vec$weight
  vec<-vec%>%select(-weight)
  stats<-map2(vec,flag,~{
    eachvec<-{if(is.data.frame(.x)) .x%>%reduce(c) else .x}
    eachflag<-.y
    if(!length(eachflag)) return(NULL)
    if(eachflag){
      c(wtd.mean(eachvec,weight),sqrt(wtd.var(eachvec,weight)))%>%
        map_chr(~sprintf(paste0("%",fieldwidth,".",precision,"f"),.x)%>%trimws)%>%
        {paste0(.[1],p1,.[2],p2)}
    }
  }
  )
  tibble(variables=names(vec)[map_lgl(stats,~is.null(.x))%>%`!`%>%which],
         statistics=stats%>%reduce(c))
  }

# weighted iqr ------------------------------------------------------------

weighted.inter.quartile.range<-function(vec,flag=NULL,weight=NULL,fieldwidth=5,precision=2,p1="[",p2=",",p3="]"){
  weight<-vec$weight
  vec<-vec%>%select(-weight)
  flag<-{if(!length(flag)) {is.normal(vec)%>%map(~{if(is.null(.x)) FALSE else if (.==FALSE) TRUE else FALSE})} else flag}
  flag
  stats<-map2(vec,flag,~{
    eachvec<-{if(is.data.frame(.x)) .x%>%reduce(c) else .x}
    eachflag<-.y
    if(!length(eachflag)) return(NULL)
    if(eachflag){
      c(wtd.quantile(eachvec,weight,probs=c(.25,.5,.75)))%>%
        map_chr(~sprintf(paste0("%",fieldwidth,".",precision,"f"),.x)%>%trimws)%>%{paste0(.[1],p1,.[2],p2,.[3],p3)}
    }

  })
  tibble(variables=names(vec)[map_lgl(stats,~is.null(.x))%>%`!`%>%which],
         statistics=stats%>%reduce(c))
}

# weighted count + pct ---------------------------------------------------- (continue)

weighted.cnt.pct<-function(vec,flag=is.categorical.variable(vec),weight=NULL,fieldwidth=5,precision=2,p1="(",p2="%)"){
    weight<-vec$weight
    vec<-vec%>%select(-weight)
    stats<-map2(vec,flag,~{
    eachvec<-{if(is.data.frame(.x)) reduce(.x,c,na.rm=T) else .x}%>%{if(!is.factor(.)) as.factor(.) else .}
    eachflag<-.y
    eachlevel<-eachvec%>%levels()
    if(!length(eachflag)) return(NULL) 
    if(eachflag){
      map_df(eachlevel,~{
        nestedlevel<-.x
        cnt<-if(!is.null(weight)) sum(weight[eachvec%in%nestedlevel],na.rm=TRUE) else sum(eachvec%in%nestedlevel,na.rm=TRUE)
        denom<-if(!is.null(weight)) sum(weight[!eachvec%in%NA],na.rm=TRUE) else sum(!eachvec%in%NA)
        pct<-(cnt/denom)*100
        c(cnt,pct)%>%map_chr(~sprintf(paste0("%",fieldwidth,".",precision,"f"),.x)%>%trimws)%>%{tibble(levels=nestedlevel,statistic=paste0(.[1],p1,.[2],p2))}
      })
    }
  })
    stats%>%rbindlist(idcol="variables")%>%tibble
}

# missing -----------------------------------------------------------------
weighted.missing.pct<-function(vec,flag=ncol(vec),weight=NULL,fieldwidth=5,precision=2,p1="(",p2="%)"){
  weight<-vec$weight
  vec<-vec%>%select(-weight)
  stats<-map2(vec,flag,~{
    eachvec<-{if(is.data.frame(.x)) reduce(.x,c,na.rm=T) else .x}%>%{if(!is.factor(.)) as.factor(.) else .}
    eachflag<-.y
    eachlevel<-eachvec%>%levels()
    if(!length(eachflag)) return(NULL) 
    if(eachflag){
      cnt<-if(!is.null(weight)) sum(weight[eachvec%in%NA],na.rm=TRUE) else sum(eachvec%in%NA,na.rm=TRUE)
      denom<-if(!is.null(weight)) sum(weight,na.rm=TRUE) else length(eachvec)
      pct<-(cnt/denom)*100
      c(cnt,pct)%>%map_chr(~sprintf(paste0("%",fieldwidth,".",precision,"f"),.x)%>%trimws)%>%{tibble(levels=NA,statistic=paste0(.[1],p1,.[2],p2))}    }
  })
  stats%>%rbindlist(idcol="variables")%>%tibble
}


# hypothesis testing ------------------------------------------------------


# One-Way-ANOVA -----------------------------------------------------------

mean.sd.test<-function(vec,groupID,flag=is.normal(vec%>%select(-!!groupID)),fieldwidth=5,precision=3){
  vars<-names(vec)%>%.[!.%in%groupID]
  map2(vars,flag,~{
    eachvec<-.x
    eachflag<-.y
    if(!length(eachflag)) return(NULL)
    if(eachflag){
      formula<-paste0(eachvec,"~",groupID)%>%as.formula
      aov(formula,data=vec)%>%summary%>%.[[1]]%>%.$"Pr(>F)"%>%.[1]%>%{
        if(is.null(.)) "N/A" else {if(.<0.001) "<0.001" else sprintf(paste0("%",fieldwidth,".",precision,"f"),.)%>%trimws}}
      }
  })
}


# Kruskal-Wallis Test -----------------------------------------------------

inter.quartile.range.test<-function(vec,groupID,flag=is.categorical.variable(vec%>%select(-!!groupID)),
                                    fieldwidth=5,precision=3){
  vars<-names(vec)%>%.[!.%in%groupID]
  map2(vars,flag,~{
    eachvec<-.x
    eachflag<-.y
    if(!length(eachflag)) return(NULL)
    if(eachflag){
      formula<-paste0(eachvec,"~",groupID)%>%as.formula
      cond<-vec[[groupID]][complete.cases(vec[[eachvec]],vec[[groupID]])]%>%factor%>%nlevels%>%{.<2L}%>%`!`
      if(cond){
        try({
          kruskal.test(formula,data=vec)%>%.$p.value%>%{
            if(is.null(.)|is.na(.)) "N/A" else {if(.<0.001) "<0.001" else sprintf(paste0("%",fieldwidth,".",precision,"f"),.)%>%trimws}}
          
        })
      } else "N/A"
      }
  })
}


# Fisher Exact Test -------------------------------------------------------

cnt.pct.test.fisher<-function(vec,groupID,flag=is.categorical.variable(vec%>%select(-!!groupID)),
                       fieldwidth=5,precision=3,workspace=2e8){
  vars<-names(vec)%>%.[!.%in%groupID]
  map2(vars,flag,~{
    eachvec<-.x
    eachflag<-.y
    if(!length(eachflag)) return(NULL)
    if(eachflag){
      tab<-table(vec[[eachvec]],vec[[groupID]])
      if(nrow(tab)>1){
        try({fisher.test(tab,workspace=workspace)%>%.$p.value%>%{
          if(is.null(.)) "N/A" else {if(.<0.001) "<0.001" else sprintf(paste0("%",fieldwidth,".",precision,"f"),.)%>%trimws}}
        })
        } else "N/A"
         }
  })
}

# Chi-square Test ---------------------------------------------------------

cnt.pct.test.chi<-function(vec,groupID,flag=is.categorical.variable(vec%>%select(-!!groupID)),
                       fieldwidth=5,precision=3,simulate.p.value=TRUE){
  vars<-names(vec)%>%.[!.%in%groupID]
  map2(vars,flag,~{
    eachvec<-.x
    eachflag<-.y
    if(!length(eachflag)) return(NULL)
    if(eachflag){
      tab<-table(vec[[eachvec]],vec[[groupID]])
      if(nrow(tab)>1){
        try({chisq.test(tab,simulate.p.value=simulate.p.value)%>%.$p.value%>%{
          if(is.null(.)) "N/A" else {if(.<0.001) "<0.001" else sprintf(paste0("%",fieldwidth,".",precision,"f"),.)%>%trimws}}
        })
      } else "N/A"
    }
  })
}


# Balance statistics ------------------------------------------------------

ASMD<-function(vec,groupID,weight=NULL,fieldwidth=5,precision=4,method="ATT",twang=TRUE){
  paired.dat<-by(vec,vec[[groupID]],identity)%>%{
    subsets<-.
    map((combn(seq_len(vec[[groupID]]%>%as.factor%>%nlevels),2))%>%
          data.frame%>%as.list,
        ~{subsets[c(.x)]%>%rbindlist(fill=T)})
  }
  dnames<-map_chr(paired.dat,~{
    d<-.
    d[[groupID]]%>%as.factor()%>%levels%>%{paste(.,collapse=" vs ")}
  })
  map(paired.dat,~{
      pair.dat<-.x[complete.cases(.x),]
      treat.var<-pair.dat[[groupID]]%>%as.factor()
      s.d.denom<-case_when(
        is.null(weight)|twang==TRUE~"all",
        method=="ATT"~"treated",
        method=="ATE"~"pooled"
        )
      
      bal.tab.weight<-if(!is.null(weight)) pair.dat$weight else rep(1,times=nrow(pair.dat))

      bal.tab(pair.dat%>%select(-all_of(groupID)),treat=treat.var,bindary="std",
            weights=bal.tab.weight,s.d.denom=s.d.denom)%>%{
              asmd<-.$Balance%>%
                  .$Diff.Adj%>%map_chr(~{if(is.null(.)) "N/A" else {if(abs(.)<0.001) "<0.001" else sprintf(paste0("%",fieldwidth,".",precision,"f"),abs(.))%>%trimws}})
              conames<-.[[1]]%>%rownames
              tibble(variables=conames,ASMD=asmd)
            }
      })%>%set_names(dnames)
}


# Warp-up Unweighted ------------------------------------------------------

Unweighted.stats<-
# test case ---------------------------------------------------------------

max.categories<-10
cohort<-fread("C:/Users/User/Desktop/matched_ch_adult_wv14_0518_rt2.csv")%>%select(contains(c("baseline","weights"))&!contains("date"))%>%tibble
cohort%>%mutate_all(as.factor)%>%map(~.x%>%levels)
g<-sample(c(0:1),size=nrow(cohort),prob=c(0.4,0.6),replace=TRUE)
m<-sample(c(0:2),size=nrow(cohort),prob=c(0.3,0.4,0.3),replace=TRUE)
cohort$groupid<-g
cohort$mgroupid<-m
weighted.mean.sd(cohort,weight=cohort$weights)
weighted.inter.quartile.range(cohort,weight=cohort$weights)
weighted.cnt.pct(cohort,weight=cohort$weights)
weighted.missing.pct(cohort,weight=cohort$weights)
mean.sd.test(cohort,groupID="mgroupid")
inter.quartile.range.test(cohort,groupID="mgroupid")
cnt.pct.test.fisher(cohort,groupID="mgroupid")
cnt.pct.test.chi(cohort,groupID="mgroupid")

library(twang)
data("AOD")
cohort<-AOD
g<-sample(c(0:1),size=nrow(cohort),prob=c(0.4,0.6),replace=TRUE)
m<-sample(c(0:2),size=nrow(cohort),prob=c(0.3,0.4,0.3),replace=TRUE)
cohort$groupid<-g
cohort$mgroupid<-m
cohort$weight<-runif(nrow(cohort))
ASMD(cohort,groupID="groupid")

by(cohort,cohort[["groupid"]],identity)%>%{
  subsets<-.
  map((combn(seq_len(cohort[["groupid"]]%>%as.factor%>%nlevels),2))%>%
        data.frame%>%as.list,
      ~{subsets[c(.x)]%>%rbindlist(fill=T)})
}


bal.tab(covs, treat = lalonde$treat, weights = lalonde$att.weights)

bal.tab(cohort%>%select(-contains(c("group","weight"))),treat=cohort$groupid,weights=rep(1,times=nrow(cohort)))->a

bal.tab(cohort%>%select(-contains(c("group","weight"))),treat=cohort$groupid,weights=cohort$weights)->z



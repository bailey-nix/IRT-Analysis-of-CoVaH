---
  title: "Project-CLEAN"
author: "Bailey Nix"
date: "2023-05-15"
output: pdf_document
---
  
  ```{r libraries and data cleanup,message=FALSE}
library(tidyverse)
library(catR)
library(mirt)
library(haven)
library(psych)
library(lordif)
library(kableExtra)
setwd("~/Desktop")
dat<-read_sav("Kotta et al data.sav")

#dataset of only responses for 15 item questionnaire
responses<-dat%>%select(CV_risk9,CV_risk8,CV_risk11,CV_risk10,CV_risk12,CV_risk4,CV_risk5,CV_risk6,CV_risk2,CV_skept8,CV_skept9,CV_FEAR9,CV_FEAR10,CV_FEAR7,CV_FEAR8)

#polytomous responses separated by factors
responses_f1<-responses[,1:5]
responses_f2<-responses[,6:11]
responses_f3<-responses[,12:15]
```

```{r}
#check if factor 1 was reversed coded or not
cor(rowSums(responses_f1),rowSums(responses_f2))
car::scatterplot(rowSums(responses_f1),rowSums(responses_f2))
```

```{r}
#defining multidimensional model
mirt.model<-mirt.model("F1=1-5,
                       F2=6-11,
                       F3=12-15,
                       COV=F1*F2*F3")
```

```{r}
#CFA
cfa<-mirt(responses,mirt.model,"graded",method="MHRM",technical=list(NCYCLES=1000))
cfa_loadings<-summary(cfa, rotate = 'oblimin')

#table for factor loadings
cfa_loadings%>%
  kbl(caption="Factor Loadings of CoVaH Scale")%>%
  kable_classic(full_width=F,html_font = "Times New Roman")
```

```{r}
#fitting partial credit model
pcm<-mirt(responses,1,"Rasch",verbose=FALSE)
#graded response model- unidimensional
mirt_graded<-mirt(responses, model=1 , "graded", verbose=FALSE)
#graded response model-3 factor
mirt_graded_multi<-mirt(responses, model=mirt.model, "graded", method="MHRM",
                        verbose=FALSE,technical=list(NCYCLES=1000))
```

```{r}
#3 unidimensional models for each factor
gradedf1<-mirt(responses_f1,1,"graded",verbose=FALSE)

gradedf2<-mirt(responses_f2,1,"graded",verbose=FALSE)

gradedf3<-mirt(responses_f3,1,"graded",verbose=FALSE)
```

```{r}
#absolute fit
pcm_M2<-M2(pcm)
grm_M2<-M2(mirt_graded)
grm_M2_3<-M2(mirt_graded_multi)

#table of absolute fit stats
fit_measures<-list(pcm_M2,grm_M2,grm_M2_3)
fit_measures<-Reduce(function(x, y) merge(x, y, all=TRUE), fit_measures)
row.names(fit_measures) <- c("Graded Response Model (multidimensional)", "Graded Response Model (unidimensional)", "Partial Credit Model")
fit_measures<-round(fit_measures,2)

fit_measures%>%
  kbl(caption = "Absolute Model Fit Statistics") %>%
  kable_classic(full_width =F, html_font = "Times New Roman")
```

```{r}
#relative fit
rel_fit<-as.data.frame(anova(pcm,mirt_graded,mirt_graded_multi,bounded=TRUE))

#table of relative fit stats
row.names(rel_fit) <- c("Partial Credit Model", "Graded Response Model (unidimensional)", "Graded Response Model (multidimensional)")
rel_fit<-round(rel_fit,2)
rel_fit%>%
  kbl(caption="Relative Model Fit Statisitcs")%>%
  kable_classic(full_width=F,html_font="Times New Roman")
```

```{r}
#item fit
itemfit<-as.data.frame(itemfit(mirt_graded_multi,fit_stats=c("S_X2")))

#table for item fit
row.names(itemfit)<-c("Item 1","Item 2","Item 3","Item 4","Item 5","Item 6","Item 7","Item 8","Item 9","Item 10","Item 11","Item 12","Item 13","Item 14","Item 15")

itemfit<-round(itemfit[,2:5],2)
itemfit%>%
  kbl(caption="Item Fit Statistics")%>%
  kable_classic(full_width = F, html_font = "Times New Roman")
```

```{r}
#item parameters
ip_graded_multi<-as.data.frame(coef(mirt_graded_multi,simplify=TRUE,IRTpars=FALSE)$items)
ip_graded_multi_ab<-(ip_graded_multi)
ip_graded_multi_ab$b1<-c(-.634,-.729,-.410,-.962,-1.145,-.659,-.349,-1.266,-.505,-.452,-1.399,.389,.860,.615,1.1897)
ip_graded_multi_ab$b2<-c(-.107,-.212,.074,-.225,-.475,-.047,.120,-.420,.046,.096,-.713,1.17,1.17,.988,1.662)
ip_graded_multi_ab$b3<-c(.336,.249,.5798,.488,.195,.668,.658,.392,.800,.941,.22,1.43,1.42,1.30,2.04)
ip_graded_multi_ab$b4<-c(.666,.561,.908,.902,.619,1.17,1.11,1.04,1.323,1.679,.988,1.72376,1.71,1.70,2.478)
ip_graded_multi_ab<-ip_graded_multi_ab%>%select("a1","a2","a3","b1","b2","b3","b4")

#table of item Parameters
ip_graded_multi_ab<-round(ip_graded_multi_ab,2)
row.names(ip_graded_multi_ab)<-c("Item 1","Item 2","Item 3","Item 4","Item 5","Item 6","Item 7","Item 8","Item 9","Item 10","Item 11","Item 12","Item 13","Item 14","Item 15")
ip_graded_multi_ab %>%
  kbl(caption = "Graded Response Model Item Parameters") %>%
  kable_classic(full_width = F, html_font = "Times New Roman")
```

```{r}
#item plots grouped by factor
plot(gradedf1, type = 'trace', theta_lim = c(-4,4), lwd=2,main="Category Response Functions of Skepticism Factor")

plot(gradedf2,type='trace',theta_lim = c(-4,4), lwd=2,main="Category Response Functions of Risk Factor")

plot(gradedf3,type='trace',theta_lim = c(-4,4), lwd=2,main="Category Response Functions of Fear Factor")
```

```{r}
#theta estimation
wle_ests_multi<-apply(responses,1,thetaEst,
                      it=ip_graded_multi,
                      method="WL")

#theta distribution plot
theta_plot<-plot(density(wle_ests_multi),main="Distribution of Theta Estimates",xlab="Theta",xlim=c(-.3,.7))
theta_plot
```

```{r}
#TIF and SE plots by factor
plot(gradedf1,type="infoSE", main="Test Information and Standard Errors of Skepticism Factor")

plot(gradedf2,type="infoSE", main="Test Information and Standard Errors of Risk Factor")

plot(gradedf3,type="infoSE", main="Test Information and Standard Errors of Fear Factor")
```

```{r}
#DIF based on chronic illness status
responses<-as.data.frame(responses)
chronic_disease<-factor(dat$chronic_disease)
dif_chronic_disease<-lordif(resp=responses,group=chronic_disease,model="GRM")

plot(dif_chronic_disease, labels = c("No Chronic Illness", "Chronic Illness"))
```

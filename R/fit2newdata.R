#' Make a new data set for prediction
#'@param fit An object of class "lm", "glm" or "loess"
#'@param predictors Names of predictor variables in string
#'@param mode A numeric. Useful when the variables are numeric. If 1, c(-1,0,1)*sd + mean is used. If 2, the 16th, 50th, 84th percentile values used. If 3 sequence over a the range of a vector used
#'@param pred.values For which values of the predictors should be used? Default is NULL. If NULL, 20 seq_range is used.
#'@param modx.values For which values of the moderator should lines be plotted? Default is NULL. If NULL, then the customary +/- 1 standard deviation from the mean as well as the mean itself are used for continuous moderators. If the moderator is a factor variable and modx.values is NULL, each level of the factor is included.
#'@param mod2.values For which values of the second moderator should lines be plotted? Default is NULL. If NULL, then the customary +/- 1 standard deviation from the mean as well as the mean itself are used for continuous moderators. If the moderator is a factor variable and modx.values is NULL, each level of the factor is included.
#'@param colorn The number of regression lines when the modifier variable(s) are numeric.
#'@param maxylev An integer indicating the maximum number of levels of numeric variable be treated as a categorical variable
#'@param summarymode An integer indicating method of extracting typical value of variables. If 1, typical() is used.If 2, mean() is used.
#'@importFrom prediction seq_range
#'@importFrom magrittr "%>%"
#'@importFrom purrr reduce
#'@importFrom modelr typical
#'@importFrom stats sd na.omit
#'@importFrom dplyr ".data"
#'@importFrom stats quantile
#'@export
#'@examples
#'fit=lm(mpg~hp*wt*cyl+carb+am,data=mtcars)
#'fit2newdata(fit,predictors=c("hp","wt","am"))
#'fit2newdata(fit,predictors=c("hp","wt","cyl"))
#'fit2newdata(fit,predictors=c("hp"))
#'fit2newdata(fit,predictors=c("hp","wt"))
#'fit=loess(mpg~hp*wt*am,data=mtcars)
#'fit2newdata(fit,predictors=c("hp"))
#'\donttest{
#'mtcars$engine=ifelse(mtcars$vs==0,"V-shaped","straight")
#'fit=lm(mpg~wt*engine,data=mtcars)
#'fit2newdata(fit,predictors=c("wt","engine"))
#'fit=lm(mpg~wt*factor(vs),data=mtcars)
#'fit2newdata(fit,predictors=c("wt","vs"))
#'fit2newdata(lm(mpg~hp*wt,data=mtcars),predictors=c("hp","wt"),mode=3,colorn=30)
#'fit=lm(mpg~hp*log(wt),data=mtcars)
#'fit2newdata(fit,predictors=c("hp","log(wt)"))
#'fit=lm(mpg~hp*wt*factor(vs),data=mtcars)
#'fit2newdata(fit,predictors=c("hp"))
#'require(moonBook)
#'fit=lm(log(NTAV)~I(age^2)*sex,data=radial)
#'fit2newdata(fit,predictors=c("I(age^2)","sex"))
#'}
fit2newdata=function(fit,predictors,mode=1,pred.values=NULL,modx.values=NULL,mod2.values=NULL,colorn=3,maxylev=6,summarymode=1){

       #  fit=lm(100/mpg~wt*hp,data=mtcars)
       # predictors=c("wt","hp")
       # fit=lm(mpg~hp*wt*cyl+carb+am,data=mtcars)
       # predictors=c("hp")
       # mode=1;pred.values=NULL;modx.values=NULL;mod2.values=NULL;colorn=3;maxylev=6;summarymode=1

     predictors=restoreNames(predictors)
     predictors

     if("loess" %in% class(fit)){
          vars=rownames(attr(fit$terms,"factors"))
          yvar=vars[1]
          # xname=vars[2]
          # if(length(vars)>2) colorname=vars[3]
          # if(length(vars)>3) facetname=vars[4]

          # df=cbind(fit$y,data.frame(fit$x))
          # data.frame(fit$x)
          # colnames(df)[1]=yvar
          df=data.frame(fit$x)
          df

     } else{
    df=fit$model[-1]
    yvar=names(fit$model)[1]
     }

     df=restoreData(df)
     df=restoreData2(df)
     df


    df1<-df[predictors]
    select=setdiff(names(df),predictors)

    if(length(which(str_detect(select,"I\\(|factor\\(")))>0){
           select=select[-which(str_detect(select,"I\\(|factor\\("))]
    }
    if(length(which(str_detect(select,"^log|^sqrt|^exp")))>0){
        select=select[-which(str_detect(select,"^log|^sqrt|^exp"))]
    }
    select
    df2<-df[select]

    if(is.mynumeric(df1[[1]],maxylev=maxylev)) {
        newdf=seq_range(df1[[1]],30)
    } else{
        newdf=unique(df1[[1]])
    }

    if(!is.null(pred.values)) newdf=pred.values
    newdf=data.frame(newdf)
    newdf

        if(length(df1)>1){
        newdf2<-lapply(df1[2:length(df1)],function(x) {
            if(is.mynumeric(x,maxylev=maxylev)) {
                if(mode==1) mean(x,na.rm=TRUE)+c(-1,0,1)*sd(x,na.rm=TRUE)
                else if(mode==2) quantile(x,probs=c(0.16,0.50,0.84),type=6)
                else if(mode==3) seq_range(x,colorn)
            } else{
                unique(x)
            }
        })
        if(!is.null(modx.values)) newdf2[[1]]=modx.values
        if(!is.null(mod2.values)) newdf2[[2]]=mod2.values
        newdf2
        if(length(newdf2)>1) {
           newdf2<-newdf2 %>% reduce(expand.grid)
        }
        newdf=expand.grid2(newdf,newdf2)
    }
    colnames(newdf)=colnames(df1)

    caption<-NULL
    if(length(df2)>0){
        # lapply(df2,modelr::typical)
        if(summarymode==1){
           newdf3<-lapply(df2,modelr::typical)
        } else{
          newdf3<-lapply(df2,mean,na.rm=TRUE)
        }
        newdf3
        newdf3=data.frame(newdf3,stringsAsFactors = FALSE)
        if(nrow(newdf3)>1) newdf3<-newdf3[1,]
        colnames(newdf3)=names(df2)
        caption<-paste(names(newdf3),newdf3[1,],sep="=",collapse=",")
        newdf3
        newdf
        newdf<-expand.grid2(newdf,newdf3)
    }

    newdf
    result <- predict(fit, newdata = newdf, type = "response",se=TRUE)
    # result <- predict(fit, newdata = newdf, type = "response",se.fit=TRUE)
     result
     newdf
     if(any(str_detect(names(df),"I\\("))){
          select=which(str_detect(names(df),"I\\("))
          select
          for(i in seq_along(select)){
               temp=names(df)[select[i]]

               eq=str_replace_all(temp,"I\\(|\\)$","")
               for(j in seq_along(names(newdf))){
                    temp2=names(newdf)[j]
                    if(str_detect(eq,temp2)){
                         temp3=str_replace(eq,temp2,paste0("newdf$",temp2))
                         newdf[[temp]]=eval(parse(text=temp3))
                    }
               }
          }
     }
     if(any(str_detect(names(df),"^log|^sqrt|^exp|^factor\\("))){
         select=which(str_detect(names(df),"^log|^sqrt|^exp|^factor\\("))
         select

         for(i in seq_along(select)){
             temp=names(df)[select[i]]
             temp
             for(j in seq_along(names(newdf))){
                 temp2=names(newdf)[j]
                 if(str_detect(temp,temp2)){
                     temp3=str_replace(temp,temp2,paste0("newdf$",temp2))
                     temp3
                     newdf[[temp]]=eval(parse(text=temp3))
                 }
             }

         }
         newdf
     }
    newdf[[yvar]]<-result$fit
    newdf$se.fit<-result$se.fit
    newdf$ymax<-newdf[[yvar]]+result$se.fit
    newdf$ymin<-newdf[[yvar]]-result$se.fit
    newdf=restoreData2(newdf)
    newdf=restoreData3(newdf)
    if(!is.null(caption)) attr(newdf,"caption")=caption
    newdf
}

"alpha" <- 
    function(x,keys=NULL,cumulative=FALSE,title=NULL,max=10,na.rm=TRUE,check.keys=FALSE,n.iter=1,delete=TRUE,use="pairwise",warnings=TRUE,n.obs=NULL) {  #find coefficient alpha given a data frame or a matrix
    
 alpha.1 <- function(C,R) {
    n <- dim(C)[2]
    alpha.raw <- (1- tr(C)/sum(C))*(n/(n-1))
    sumR <- sum(R)
    alpha.std <-  (1- n/sumR)*(n/(n-1))  
    smc.R <- smc(R)
    G6 <- (1- (n-sum(smc.R))/sumR)
    av.r <- (sumR-n)/(n*(n-1))
    R.adj <- R
    diag(R.adj) <- NA
    var.r  <- var(as.vector(R.adj),na.rm=TRUE)
    mod1 <- matrix(av.r,n,n)
    Res1 <- R - mod1
    GF1 =  1- sum(Res1^2)/sum(R^2)
    Rd <- R - diag(R)
    diag(Res1) <- 0
    GF1.off <- 1 - sum(Res1^2)/sum(Rd^2)  
    sn <- n*av.r/(1-av.r)
#   Q = (2 * n^2/((n-1)^2*(sum(C)^3))) * (sum(C) * (tr(C^2) + (tr(C))^2) - 2*(tr(C) * sum(C^2))) #corrected 1/15/16 
    Q = (2 * n^2/((n - 1)^2 * (sum(C)^3))) * (sum(C) * (tr(C%*%C) +  (tr(C))^2) - 2 * (tr(C) * sum(C%*%C)))   #correction from Tamaki Hattori
    result <- list(raw=alpha.raw,std=alpha.std,G6=G6,av.r=av.r,sn=sn,Q=Q,GF1,GF1.off,var.r = var.r)
    return(result)
    }
    
    #begin main function
    cl <- match.call()
    if(!is.matrix(x) && !is.data.frame(x)) stop('Data must either be a data frame or a matrix')
    if(class(x)[1] != "data.frame") x <- fix.dplyr(x)    #to get around a problem created by dplyr
    if(!is.null(keys)){#two 3 cases  1 it is a list, 2 is a vector of character, 3 it is keys matrix  4 it is a list of items to  reverse
    if( is.list(keys)) { select <- sub("-","",unlist(keys))   #added 9/26/16 to speed up scoring one scale from many
      x <- x[select] 
      keys <- make.keys(x,keys)}}
      
    nvar <- dim(x)[2]
    nsub <- dim(x)[1]
    scores <- NULL
    response.freq <- NULL
   
    if (!isCorrelation(x))  { #find the correlations if we are given  raw data
       item.var <- apply(x,2,sd,na.rm=na.rm)
       bad <- which((item.var <= 0)|is.na(item.var))
       if((length(bad) > 0) && delete) {
            for (baddy in 1:length(bad)) {warning( "Item = ",colnames(x)[bad][baddy], " had no variance and was deleted")}
            x <- x[,-bad] 
            nvar <- nvar - length(bad)
             }
         response.freq <- response.frequencies(x,max=max)
         C <- cov(x,use=use)} else {C <- x}
        
        if(is.null(colnames(x)))  colnames(x) <- paste0("V",1:nvar)
       
         #if(check.keys && is.null(keys)) {
            p1 <- principal(x,scores=FALSE)
               if(any(p1$loadings < 0)) {if (check.keys) {if(warnings) warning("Some items were negatively correlated with total scale and were automatically reversed.\n This is indicated by a negative sign for the variable name.") 
                    keys <- 1- 2* (p1$loadings < 0 ) } else {
                       if(is.null(keys) && warnings ) {warning("Some items were negatively correlated with the total scale and probably \nshould be reversed.  \nTo do this, run the function again with the 'check.keys=TRUE' option")
                       if(warnings) cat("Some items (",rownames(p1$loadings)[(p1$loadings < 0)],") were negatively correlated with the total scale and \nprobably should be reversed.  \nTo do this, run the function again with the 'check.keys=TRUE' option")
                        }} 
            }  #keys is now a vector of 1s and -1s
           # }
            
         if (is.null(keys)) {
             keys <- rep(1,nvar)} 
         else {  
             keys<- as.vector(keys)
             if (length(keys) < nvar) { 
                 temp <- keys  #this is the option of keying just the reversals
                 keys <- rep(1,nvar)
                 names(keys) <- colnames(x)
                 keys[temp] <- -1}
             } 
             key.d <- diag(keys)
             C <- key.d %*% C %*% key.d
             signkey <- strtrim(keys,1)
             signkey[signkey=="1"] <- ""
             colnames(x) <- paste(colnames(x),signkey,sep="")
                     
         if (nsub !=nvar)  {   #raw data      
             if (any(keys < 0 )) { 
               min.item <- min(x,na.rm=na.rm)
              max.item <- max(x,na.rm=na.rm)
              adjust <- max.item + min.item
              flip.these <- which(keys < 0 )
               x[,flip.these]  <- adjust - x[,flip.these] 
         }

        if(cumulative) {total <- rowSums(x,na.rm=na.rm) } else {total <- rowMeans(x,na.rm=na.rm)}
                mean.t <- mean(total,na.rm=na.rm)
                sdev <- sd(total,na.rm=na.rm) 
                raw.r <- cor(total,x,use=use)

                       
        t.valid <- colSums(!is.na(x))} else {   #we are working with a correlation matrix
                 total <- NULL
                 totals <- TRUE
                 }
          
         R <- cov2cor(C)
         drop.item <- vector("list",nvar)
         alpha.total <- alpha.1(C,R)
         if(nvar > 2) {
         for (i in 1:nvar) {
         drop.item[[i]] <- alpha.1(C[-i,-i,drop=FALSE],R[-i,-i,drop=FALSE])
                            } 
         } else {drop.item[[1]] <- drop.item[[2]] <- c(rep(R[1,2],2),smc(R)[1],R[1,2],NA,NA,NA,NA)  #added the extra 2 NA June 18, 2017
       }
        by.item <- data.frame(matrix(unlist(drop.item),ncol=9,byrow=TRUE)) 
        
                  #allows us to specify the number of subjects for correlation matrices
        if(max(nsub,n.obs) > nvar) {by.item[6] <- sqrt(by.item[6]/(max(nsub,n.obs)) )
         by.item <- by.item[-c(7:8)]
         colnames(by.item) <- c("raw_alpha","std.alpha","G6(smc)","average_r","S/N","alpha se") } else {
         
             by.item <- by.item[-c(6:8)]
             colnames(by.item) <- c("raw_alpha","std.alpha","G6(smc)","average_r","S/N") }
        rownames(by.item) <- colnames(x)
        
        Vt <- sum(R)
        item.r <- colSums(R)/sqrt(Vt)  #this is standardized r
       
     #correct for item overlap by using  smc 
        RC <-R
        diag(RC) <-smc(R)
        Vtc <- sum(RC)
        item.rc <-colSums(RC)/sqrt(Vtc)
     #yet one more way to correct is to correlate item with rest of scale
      if(nvar > 1) {
      r.drop <- rep(0,nvar)
        for (i in 1:nvar) { v.drop <- sum(C[-i,-i,drop=FALSE])
          c.drop <- sum(C[,i]) - C[i,i]
          r.drop[i] <- c.drop/sqrt(C[i,i]*v.drop)
              }
      }
     
     #  
        item.means <- colMeans(x, na.rm=na.rm )
        item.sd <-  apply(x,2,sd,na.rm=na.rm)
        if(nsub > nvar) {
           Unidim <- alpha.total[7]
           var.r <- alpha.total[[9]]
	   Fit.off <- alpha.total[8]
           ase = sqrt(alpha.total$Q/nsub)
           alpha.total <- data.frame(alpha.total[1:5],ase=ase,mean=mean.t,sd=sdev)
           colnames(alpha.total) <- c("raw_alpha","std.alpha","G6(smc)","average_r","S/N","ase","mean","sd")
	  
        	 alpha.total <- data.frame(alpha.total[1:5],ase=ase,mean=mean.t,sd=sdev)
        	colnames(alpha.total) <- c("raw_alpha","std.alpha","G6(smc)","average_r","S/N","ase","mean","sd")
        	rownames(alpha.total) <- ""
        	stats <- data.frame(n=t.valid,raw.r=t(raw.r),std.r =item.r,r.cor = item.rc,r.drop = r.drop,mean=item.means,sd=item.sd)
        	} else {
        	if(is.null(n.obs)) {
        	       Unidim <- alpha.total[7]
        	       Fit.off <- alpha.total[8]
        	       var.r <- alpha.total[9] 
        	      alpha.total <- data.frame(alpha.total[1:5])  #fixed 27/7/14 
        	        colnames(alpha.total) <- c("raw_alpha","std.alpha","G6(smc)" ,"average_r","S/N") } else {
        	        Unidim <- alpha.total[7]
        	 		Fit.off <- alpha.total[8] 
        	        alpha.total <- data.frame(alpha.total[1:5],ase=sqrt(alpha.total$Q/n.obs))
        	        colnames(alpha.total) <- c("raw_alpha","std.alpha","G6(smc)" ,"average_r","S/N","ase")}
        	        rownames(alpha.total) <- "" 
        	        

                  stats <- data.frame(r =item.r,r.cor = item.rc,r.drop = r.drop) #added r.drop 10/12/13
        	}
       	rownames(stats) <- colnames(x)
       	
       	#added measures of unidimensionality  Feb 24, 2016
        #found in alpha.1
        # the basic idea is how big are the residuals given the model
        #we can compare them to the total R or the off diagonal R.
      
       	#end of unidimensionality statistics
       	if(n.iter > 1) {#do a bootstrap confidence interval for alpha
   #    	 if(!require(parallel)) {message("The parallel package needs to be installed to run mclapply")}
        if(nsub == nvar) {message("bootstrapped confidence intervals require raw data") 
                          boot <- NULL
                          boot.ci <- NULL } else {
       	 boot <- vector("list",n.iter)
       	 boot <- mclapply(1:n.iter,function(XX) {
       	 xi <- x[sample.int(nsub,replace=TRUE),]
       	   C <- cov(xi,use="pairwise")
       	           if(!is.null(keys)) {key.d <- diag(keys)
                                      xi <- key.d %*% C %*% key.d}
                
                                      
         R <- cov2cor(C)
       	 alpha.1(C,R)
       	 })  #end of mclapply 
       
       	  boot <- matrix(unlist(boot),ncol=8,byrow=TRUE)
       	  colnames(boot) <- c("raw_alpha","std.alpha","G6(smc)","average_r","s/n","ase","Unidim","Goodfit")
       	  boot.ci <- quantile(boot[,1],c(.025,.5,.975))
       	}} else {boot=NULL
       	         boot.ci <- NULL}
       	names(Unidim) <- "Unidim"
       	names(Fit.off) <- "Fit.off" 
        result <- list(total=alpha.total,alpha.drop=by.item,item.stats=stats,response.freq=response.freq,keys=keys,scores = total,nvar=nvar,boot.ci=boot.ci,boot=boot,Unidim=Unidim,var.r=var.r,Fit=Fit.off,call=cl,title=title)
        class(result) <- c("psych","alpha")
        return(result) 
  }
  #modified Sept 8, 2010 to add r.drop feature  
  #modified October 12, 2011 to add apply to the sd function
  #modified November 2, 2010 to use sd instead of SD
  #January 30, 2011  - added the max category parameter (max)
  #June 20, 2011 -- revised to add the check.keys option as suggested by Jeremy Miles
  #Oct 3, 2013 check for variables with no variance and drop them with a warning
  #November 22, 2013  Added the standard error as suggested by 
  #modified December 6, 2013 to add empirical confidence estimates
  #modified January 9, 2014 to add multicore capabilities to the bootstrap 
  #corrected December 18 to allow reverse keying for correlation matrices as well as raw data
  #modified 1/16/14 to add S/N to summary stats
  #added item.c  (raw correlation) 1/10/15
  #corrected 1/16/16 corrected the formula for Q following a suggestion by Tamaki Hattori
  #added the n.obs option to allow us to find standard errors even from correlation matrices
  
  
#a kludge to get around a problem introduced by dplyr which changes the class structure of data frames.
#created in response to a problem raised by Adam Liter   (February, 2017) 
"fix.dplyr" <- function (object) {
   if (is.data.frame(object)) {   
      cn <- class(object)
      df <- which(cn=="data.frame")
      cn.not <- cn[-df]
      cn <- c("data.frame",cn.not)
      class(object) <- cn
      } 
   invisible(object)
}

#apply the Duhacheck and Iacobucci estimates
#compare with Feldt's estimate
"alpha.ci" <- function(alpha,n.obs,n.var=NULL,p.val=.05,digits=2) {
#  Q = (2 * n^2/((n - 1)^2 * (sum(C)^3))) * (sum(C) * (tr(C%*%C) +  (tr(C))^2) - 2 * (tr(C) * sum(C%*%C)))   #correction from Tamaki Hattori
   CI.high <- 1- (1-alpha)* qf(p.val/2,n.obs-1,Inf)
   CI.low <- 1-  (1-alpha)* qf(1-p.val/2,n.obs-1,Inf)
   if(!is.null(n.var)) {r.bar <- alpha/(n.var - alpha*(n.var-1)) } else {r.bar=NA}
   result <- list(lower.ci =CI.low,alpha=alpha,upper.ci=CI.high,r.bar=r.bar)
   print(result,digits=digits)
   invisible(result)
}

#' Variance Inflation Factors
#' Calculates variance-inflation and generalized variance-inflation factors for linear, generalized linear, and other models.
#'
#' @param mod an object that responds to coef, vcov, and model.matrix, such as an lm or glm object.
#' @importFrom stats coef coefficients cov2cor model.matrix vcov
#' @return A vector of vifs, or a matrix containing one row for each term in the model, and columns for the GVIF, df.
vif=function (mod)
{
    if (any(is.na(coef(mod))))
        stop("there are aliased coefficients in the model")
    v <- vcov(mod)
    assign <- attr(model.matrix(mod), "assign")
    if (names(coefficients(mod)[1]) == "(Intercept)") {
        v <- v[-1, -1]
        assign <- assign[-1]
    }
    else warning("No intercept: vifs may not be sensible.")
    terms <- labels(terms(mod))
    n.terms <- length(terms)
    if (n.terms < 2)
        stop("model contains fewer than 2 terms")
    R <- cov2cor(v)
    detR <- det(R)
    result <- matrix(0, n.terms, 3)
    rownames(result) <- terms
    colnames(result) <- c("GVIF", "Df", "GVIF^(1/(2*Df))")
    for (term in 1:n.terms) {
        subs <- which(assign == term)
        result[term, 1] <- det(as.matrix(R[subs, subs])) * det(as.matrix(R[-subs,
                                                                           -subs]))/detR
        result[term, 2] <- length(subs)
    }
    if (all(result[, 2] == 1))
        result <- result[, 1]
    else result[, 3] <- result[, 1]^(1/(2 * result[, 2]))
    result
}


#' Correlation and Fitting linear model function for function "mycor"
#'
#' @param y numeric vectors of data values
#' @param x numeric vectors of data values
#' @param digits integer indicating the number of decimal places (round) or
#'     significant digits (signif) to be used.
#' @importFrom stats cor.test lm
#' @return mylm returns a list of following components
#'
#'     \describe{
#'        \item{out}{a list of class "htest" from \code{\link{cor.test}}
#'           between the last paired samples in a data.frame.}
#'        \item{result}{a numeric vector of length 4, consist of r and p values
#'              from \code{\link{cor.test}},slope and intercept values from
#'              \code{\link{lm}} between numeric vector y and x}
#'     }
mylm=function(y,x,digits=3){
    # performing cor.test
    out1=cor.test(y,x)
    my.r.value= round(out1$estimate,digits)
    my.p.value= round(out1$p.value,digits)
    # performing lm to get slope and intercept
    out=lm(y~x)
    result=c(my.r.value,my.p.value,
             round(out$coef[2],max(2,digits-1)),
             round(out$coef[1],max(2,digits-1)))

    # Return list consist of output of cor.test
    # as weel as r, p, slope, intercept
    list(out=out1,result=result)
}


#'Perform correlation and linear regression for a data.frame
#'@param x A data.frame
#'@param digits	integer indicating the number of decimal places
mycor=function (x, digits = 3) {
                select <- (lapply(x, function(x) is.numeric(x)) == TRUE)
                num_data = x[select]
                y <- names(num_data)
                ncol = length(num_data)
                r.value <- matrix(0, ncol, ncol)
                colnames(r.value) <- rownames(r.value) <- y
                p.value <- slope <- intercept <- r.value
                for (i in 1:length(y)) {
                    for (j in 1:length(y)) {
                        out = mylm(num_data[[j]], num_data[[i]], digits = digits)
                        r.value[i, j] = out$result[1]
                        p.value[i, j] = out$result[2]
                        slope[j, i] = out$result[3]
                        intercept[j, i] = out$result[4]
                    }
                }
                result <- list(df = x, select = select, out = out$out, r = r.value,
                               p = p.value, slope = slope, intercept = intercept)
                result
}

#'Draw a heatmap of correlation test
#'@param data A data.frame
#'@param what if 1, correlation, if 2, partial correlation, if 3, semi-partial correlation
#'@param label if 0, no label(default), if 1, use r value as label, if 2, use r value with significant mark as label
#'@param colors colors for low, mid and high correlation values
#'@param title if true, add title to the heatmap
#'@param mode 1 or 2
#'@param digits The number of decimal place'
#'@param yreverse If true, reverse y axis
#'@param xangle x-axis text angle
#'@param  yangle y-axis text angle
#'@param use.label Logical whether or not use label in case of labelled data
#'@importFrom stats na.omit
#'@importFrom ggplot2 coord_equal geom_tile ggtitle scale_fill_gradient2 scale_y_discrete "%+replace%"
ggCor=function (data, what = 1, label = 0, colors = NULL, title = TRUE,
          mode = 2, digits = 2,  yreverse = TRUE,
          xangle = 45, yangle = 0, use.label = FALSE)
{
    data = as.data.frame(data)
    select = sapply(data, is.numeric)
    data = data[select]
    if (what > 1) {
        data = na.omit(data)
    }
    data
    if (what == 1) {
        result = mycor(data, digits = digits)
        method = result$out$method
        Lab = paste("Correlation Coeffients by", method)
    }
    if (is.null(colors))
        colors = c("#6D9EC1", "white", "#E46726")
    cor_mat <- result$r
    p_mat <- result$p
    diag(cor_mat) <- NA
    diag(p_mat) <- NA
    if (mode == 2)
        cor_mat[upper.tri(cor_mat)] = NA
    var1 <- rep(row.names(cor_mat), ncol(cor_mat))
    var2 <- rep(colnames(cor_mat), each = nrow(cor_mat))
    cor <- as.numeric(cor_mat)
    cor_mat <- data.frame(var1 = var1, var2 = var2, cor = cor,
                          stringsAsFactors = FALSE)
    pval = as.numeric(p_mat)
    cor_mat$label = ifelse(is.na(cor_mat$cor), "", sprintf(paste0("%0.",
                                                                  digits, "f"), cor_mat$cor))
    if (label == 2)
        cor_mat$label = paste0(cor_mat$label, ifelse(is.na(pval),
                                                     "", ifelse(pval < 0.001, "***", ifelse(pval < 0.01,
                                                                                            "**", ifelse(pval < 0.05, "*", "")))))
    else if (label == 3)
        cor_mat$label = paste0(cor_mat$label, "\n", p2chr(pval))
    cor_mat$p = ifelse(is.na(pval), "", ifelse(pval < 0.001,
                                               "< 0.001", sprintf(" = %0.3f", pval)))
    cor_mat[["tooltip"]] <- sprintf("<i>%s</i> vs <i>%s</i>:</br><i>r</i> = %s</br><i>p</i> %s",
                                    var1, var2, cor_mat$label, cor_mat$p)
    if (mode == 2)
        cor_mat = na.omit(cor_mat)
    if (mode == 1)
        p <- ggplot(data = cor_mat, aes_string(x = "var1", y = "var2",
                                               tooltip = "tooltip"))
    else if (mode == 2)
        p <- ggplot(data = cor_mat, aes_string(x = "var2", y = "var1",
                                               tooltip = "tooltip"))
    p <- p + geom_tile(aes(fill = cor), colour = "grey50") +
        scale_fill_gradient2(low = colors[1], mid = colors[2],
                             high = colors[3], limits = c(-1, 1)) + coord_equal() +
        xlab("") + ylab("")
    if (title) {
        p <- p + ggtitle(Lab)
    }
    if (label > 0)
        p <- p + geom_text(aes(label = label))
    if (mode == 2) {
        mynames = rownames(result$r)
        p <- p + scale_x_discrete(limits = mynames[-length(mynames)])
        if (yreverse)
            p <- p + scale_y_discrete(limits = rev(mynames[-1]))
        else p <- p + scale_y_discrete(limits = mynames[-1])
        p <- p + theme_clean2(xangle = xangle, yangle = yangle)
        p <- p + theme(legend.position = c(0.8, 0.8)) + labs(fill = "r value")
    }
    p
}


#'Clean theme for ggCor
#'@param base_size base font size
#'@param xangle x-axis text angle
#'@param yangle y-axis text angle
#'@importFrom ggplot2 theme_grey
theme_clean2=function (base_size = 12, xangle = 45, yangle = 0)
{
    theme_grey(base_size) %+replace% theme(panel.background = element_blank(),
                                           panel.grid = element_blank(), axis.title = element_blank(),
                                           axis.text.x = element_text(angle = xangle), axis.text.y = element_text(angle = yangle),
                                           axis.ticks.length = unit(0, "cm"), complete = TRUE)
}

#'Convert p values to character
#'@param x	A vector
p2chr=function (x)
{
    ifelse(is.na(x), "", ifelse(x < 0.001, "(<.001)", paste0("(",
                                                             substr(sprintf("%.3f", x), 2, 5), ")")))
}

#'Add lines with labels to pre-existing ggplot
#'@param p An object of class ggplot
#'@param df A data.frame. Required columns are slope, intercept and label
#'@param xpos A numeric. Relative horizontal position
#'@param add.coord.fixed Logical. Whether or not add coord_fixed() function
#'@param lty line type
#'@param color line color
#'@param size line size
#'@param add_theme_bw2 logical Whether or not add theme_bw2()
#'@param ... Further arguments to be passed to geom_text
#'@importFrom ggplot2 ggplot stat_function
#'@export
#'@examples
#'require(ggplot2)
#'fit=lm(mpg~wt*hp,data=mtcars)
#'df=calEquation(fit)
#'p=ggplot(data=mtcars,aes(x=wt,y=mpg))
#'add_lines(p,df)
#'add_lines(p,df,lty=1:3,color=1:3,size=1)
#'fit=lm(mpg~wt*vs,data=mtcars)
#'df=calEquation(fit)
#'p=ggplot(data=mtcars)+geom_point(aes(x=wt,y=mpg))
#'add_lines(p,df)
#'add_lines(p,df,lty=1:2,color=1:2,size=1)+theme_bw()
add_lines=function(p,df,xpos=0.3,add.coord.fixed=TRUE,lty=NULL,color=NULL,size=0.5,add_theme_bw2=TRUE,...){
      # xpos=0.3;add.coord.fixed=TRUE;lty=1;color="black";size=0.50
     count=nrow(df)
     if(is.null(df$lty)) {
         if(is.null(lty)) {
             if(count<=6) {
                df$lty=1:count
             } else{
                df$lty=(1:count)%%6
             }
         } else {
             df$lty=lty
         }
     }
     if(is.null(df$color)) {
        if(is.null(color)) {
            df$color=gg_color_hue(count)
        } else{
           df$color=color
        }
     }
     if(is.null(df$size)) df$size=size
     df
     fun=list()
     statfun=list()
     for(i in 1:count){
          fun[[i]]=local({
               j<-i
               function(x){
                    df$slope[j]*x+df$intercept[j]
               }
          })
          statfun[[i]]=local({
               j<-i
               stat_function(fun=fun[[j]],lty=df$lty[j],color=df$color[j],size=size)
          })

     }
     for(i in 1:count){
          p<-p+statfun[[i]]
     }
     p
     info=getAspectRatio(p)
     ratio=info$ratio
     df$slope2=df$slope*ratio
     df$radian=atan(df$slope2)
     df$angle=df$radian*180/pi
     if(!is.null(df$xpos)) xpos=df$xpos
     x=info$xmin+(info$xmax-info$xmin)*xpos
     if(length(x)==1) x=rep(x,nrow(df))
     df$x=x
     df$y=df$x*df$slope+df$intercept
     if(is.null(df$vjust)) df$vjust=c(rep(-0.5,count-1),1.5)

     df

     p<-p+geom_text(data=df,aes_string(x="x",y="y",label="label",angle="angle",vjust="vjust"),color=df$color,...)
     if(add.coord.fixed) p<-p + coord_fixed(ratio=ratio)
     if(add_theme_bw2) p <- p + theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())
     p
}


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

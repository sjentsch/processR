#' Draw matrix plot
#' @param matrix A numeric vector
#' @param radx horizontal radius of the box.
#' @param rady vertical radius of the box.
#' @param xlim the x limits (min,max) of the plot
#' @param ylim the y limits (min,max) of the plot
#' @param labels Optional list of labels
#' @importFrom diagram openplotmat
#' @export
#' @examples
#' matrixPlot(c(1,1,1))
#' labels=list(X="X",M=c("M1","M2"),Y="Y")
#' bmatrix=c(1,1,1,0,0,1)
#' eq=multipleMediation(labels=labels,bmatrix=bmatrix,mode=1)
#' drawModel(equation=eq,labels=labels,nodemode=2)
#' matrixPlot(bmatrix)
#' bmatrix=c(1,1,0,1,0,0,1,1,1,1)
#' matrixPlot(c(1,1,0,1,0,0,1,1,1,1))
#' labels=list(X="X",M=c("M1","M2","M3"),Y="Y")
#' eq=multipleMediation(labels=labels,bmatrix=bmatrix,mode=1)
#' drawModel(equation=eq,labels=labels,parallel=TRUE,nodemode=2)
#' labels=list(X="indep",M=c("med1","med2"),Y="dep")
#' matrixPlot(c(1,1,1,0,0,1),labels=labels)
matrixPlot=function(matrix=c(1,1,1,0,0,1),radx=0.1,rady=0.05,xlim=NULL,ylim=NULL,
                    labels=NULL){


    no=matrix2no(matrix)

     result=matrix2df(matrix,labels=labels)
     if(is.null(xlim)) xlim=c(0,radx*(2*no+2))
     if(is.null(ylim)) ylim=c(1-rady*2*(no+2),1)
     openplotmat(xlim=xlim,ylim=ylim)
     for(i in 1:no){
         for(j in 1:no){
             if(result[i,j]=="") next
             mid=c(radx*(2*(j)),1-rady*(2*i+1))
             textrect(mid,radx=radx,rady=rady,lab=result[i,j])
         }
     }

     makeSubscript=function(x){
         res=c()
         for(i in seq_along(x)){
             if(nchar(x[i])==1){
                 temp=paste0("expression(italic(",x[i],"))")
             } else {
                 temp=paste0("expression(italic(",substr(x[i],1,1),"[",substr(x[i],2,nchar(x[i])),"]))")
             }
             res=c(res,temp)
         }
         res
     }

     if(is.null(labels)) {
         colnames(result)=makeSubscript(colnames(result))
         rownames(result)=makeSubscript(rownames(result))
     }

     for(i in 1:no){
         mid=c(radx*(2*(i)),1-rady)
         if(is.null(labels)) textplain(mid,lab=eval(parse(text=colnames(result)[i])))
         else textplain(mid,lab=colnames(result)[i])
     }
     for(i in 1:no){
         mid=c(rady,1-rady-rady*2*i)
         if(is.null(labels)) textplain(mid,lab=eval(parse(text=rownames(result)[i])))
         else textplain(mid,lab=rownames(result)[i])
     }
}

#' Calculate the dimension of matrix
#' @param matrix a numeric vector
#' @export
#' @examples
#' matrix2no(c(1,1,1,0,0,1))
matrix2no=function(matrix=c(1,1,1,0,0,1)){
    (count=length(matrix))
    i=1
    while(1){
        count=count-i
        if(count<=0) break
        i=i+1
    }
    i
}


#' Make data.frame with matrix
#' @param matrix a numeric vector
#' @param labels Optional list of labels
#' @export
#' @examples
#' labels=list(X="indep",M=c("med1","med2"),Y="dep")
#' matrix2df(c(1,1,1,0,0,1),labels=labels)
matrix2df=function(matrix=c(1,1,1,0,0,1),labels=NULL){
    no=matrix2no(matrix)
    result=matrix(rep("",no^2),ncol=no)
    result
    count=1
    for(i in 1:no){
        for(j in 1:no){
            if(i<j) next
            result[i,j]=matrix[count]
            count=count+1
        }
    }
    result=as.data.frame(result,stringsAsFactors = FALSE)
    if(is.null(labels)){
    if(no==2) {
        M=c("M")
    } else{
        M=paste0("M",1:(no-1))
    }
    colnames(result)=c("X",M)
    rownames(result)=c(M,"Y")
    } else{
      colnames(result)=c(labels$X,labels$M)
      rownames(result)=c(labels$M,labels$Y)
    }
    result
}

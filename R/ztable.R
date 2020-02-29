# ------------------------------------------------------------------------------
# from https://raw.githubusercontent.com/cardiomoon/ztable/master/R/ztable2flextable.R
#

#' Convert a named color into a hexadecimal color with rgb value
#' @param color A named color
#' @return a hexadecimal color
#' @importFrom stringr str_flatten str_detect
#' @importFrom grDevices col2rgb
#' @export
#' @examples
#' color2hex("green")
#' color2hex("red")
color2hex=function(color){
    result=color
    if(!str_detect(color,"#")){

        temp=ztable::zcolors$rgb[ztable::zcolors$name==tolower(color)]
        if(length(temp)==1) {
            result<-paste0("#",temp)
        } else{
            res=col2rgb(color)
            x=sprintf("%02s",as.hexmode(res))
            result=paste0("#",str_flatten(x))
        }
    }
    result
}

#' Extract hexadecimal colors from a color palette
#' @param name The name of color palette from RColorBrewer package
#' @param reverse Whether or not reverse the order of colors
#' @return hexadecimal colors
#' @importFrom RColorBrewer brewer.pal brewer.pal.info
#' @export
#' @examples
#' require(RColorBrewer)
#' require(magrittr)
#' palette2colors("Reds")
#' ztable(head(mtcars,10)) %>%
#'      addColColor(cols=1:12,bg=palette2colors("Set3"))
palette2colors=function (name, reverse = FALSE)
{
    colors = brewer.pal(RColorBrewer::brewer.pal.info[rownames(brewer.pal.info) ==
                                            name, "maxcolors"], name)
    if (reverse)
        colors = rev(colors)
    colors
}


#' Convert cgroup of ztable into data.frame
#' @param z An object of ztable
#' @return A data.frame
cgroup2df=function(z){
    cgroup=z$cgroup
    n.cgroup=z$n.cgroup

    layer=length(cgroup)
    name=list()
    for(i in 1:layer){
        temp=c()
        for(j in 1:length(cgroup[[i]])){
             temp=c(temp,rep(cgroup[[i]][j],n.cgroup[[i]][j]))
        }
        name[[i]]=temp
    }
    header=data.frame(
        col_keys=colnames(z$x),stringsAsFactors=FALSE
    )
    header
    for(i in 1:layer){
        temp=name[[i]]
        header=cbind(header,temp,stringsAsFactors=FALSE)
        colnames(header)[ncol(header)]=paste0("name",i)
    }
    header=cbind(header,colnames(z$x),stringsAsFactors=FALSE)
    header
}

#' Round the numbers of a data.frame
#' @param df A data.frame
#' @param digits A vector of integer indicating the number of decimal places
#' @return a rounded data.frame
#' @export
roundDf=function(df,digits=2){

    if(length(digits)==1){
        digits<-rep(digits,ncol(df))
    }
    else if(length(digits)<ncol(df)) {
        digits<-c(digits,rep(0,ncol(df)-length(digits)))
    }
    df[]<-lapply(1:ncol(df),function(i){
        if(is.integer(df[[i]])) {
            df[[i]]<-df[[i]]
        } else if(is.numeric(df[[i]])) {
            fmt=paste0("%0.",sprintf("%d",digits[i]),"f")
            df[[i]]=sprintf(fmt,df[[i]])
        } else{
            df[[i]]<-df[[i]]
        }

    })
    df
}

#' Convert an object of ztable into an object of flextable
#' @param z An object of class ztable
#' @return An object of class flextable
#' @importFrom flextable regulartable delete_part bg color border align merge_at
#' @importFrom flextable set_header_df merge_h padding hline_top hline border_remove
#' @importFrom officer fp_border
#' @export
#' @examples
#' require(magrittr)
#' z=ztable(head(mtcars)) %>%
#'    addRowColor(rows=1:7,palette2colors("Paired"))
#' z=ztable(head(mtcars))
#' z
#' ztable2flextable(z)
ztable2flextable=function(z){
    df=data2table(z)

    addcol=0
    if(z$include.rownames) {
        df=cbind(rowname=rownames(df),df)
        addcol=1
    }

    # if(addcol){
    #     digits=z$digits
    # }  else{
    #     digits=z$digits[-1]
    # }
    # df<-roundDf(df,digits)
    if(sum(colnames(df)=="")>0){
        colnames(df)[which(colnames(df)=="")]=rep(" ",which(colnames(df)==""))
    }

    big_border = fp_border(color="black", width = 2)
    std_border = fp_border(color="black", width = 1)
    ft<-regulartable(df) %>% border_remove() %>%
        hline_top(border=big_border,part="header") %>%
        hline(border=std_border,part="header") %>%
        hline(border=std_border,i=nrow(df),part="body")

    if(z$include.rownames) {
        ft<-ft %>% color(i=1,j=1,color="white",part="header")
    }
    if(z$include.colnames==FALSE)  ft<- ft %>% delete_part("header")

    for(i in 1:(nrow(df)+1)){
        for(j in 1:(ncol(df))){
            if(z$cellcolor[i,j]!="white") {
                if(i==1) {
                    ft<-ft %>% bg(i=i,j=j-ifelse(addcol,0,1),bg=color2hex(z$cellcolor[i,j]),part="header")
                } else{
                    ft<-ft %>% bg(i=i-1,j=j-ifelse(addcol,0,1),bg=color2hex(z$cellcolor[i,j]),part="body")
                }
            }
        }
    }
    for(i in 1:(nrow(df)+1)){
        for(j in 1:(ncol(df))){
            if(z$frontcolor[i,j]!="black") {
                #cat("i=",i,",j=",j,",color=",z$frontcolor[i,j],"\n")
                if(i==1) {
                    ft<-ft %>% color(i=i,j=j-ifelse(addcol,0,1),color=color2hex(z$frontcolor[i,j]),part="header")
                } else{
                    ft<-ft %>% color(i=i-1,j=j-ifelse(addcol,0,1),color=color2hex(z$frontcolor[i,j]),part="body")
                }

            }
        }
    }
    if(!is.null(attr(z$x,"footer"))){
        footer=attr(z$x,"footer")
        rowname=ft$header$col_keys[1]
        rowname
        ft<-eval(parse(text=paste0("add_footer(ft,",rowname,"=footer)"))) %>%
            merge_at(i=1,j=1:ncol(df),part="footer")
    }

    if(!is.null(z$caption)){
        header=z$caption
        rowname=ft$header$col_keys[1]
        ft<-eval(parse(text=paste0("add_header(ft,",rowname,"=header)"))) %>%
            merge_at(i=1,j=1:ncol(df),part="header") %>%
            color(i=1,j=1,color="black",part="header") %>%
            bg(i=1,j=1,bg="white",part="header") %>%
            border(border.top=fp_border(color="white"),part="header")

    }
    ft
    if(!is.null(z$cgroup)){
        header=cgroup2df(z)
        ft<-ft %>% set_header_df(mapping=header,key="col_keys") %>%
            merge_h(part="header") %>%
            border(border.top=fp_border(),border.bottom=fp_border(),part="header") %>%
            padding(padding.left=4,padding.right=4,part="header")
        for(i in 1:length(z$cgroupcolor)){
            for(j in 1:length(z$cgroupcolor[[i]])){
                if(z$cgroupcolor[[i]][j]!="black") {
                    if(addcol){
                        if(j <= 1+addcol) {
                            mycol=j
                        } else{
                            mycol=cumsum(z$n.cgroup[[i]])[j-1-addcol]+1+addcol
                        }
                    } else{
                        if(j<=2){
                            mycol=1
                        } else{
                            mycol=cumsum(z$n.cgroup[[i]])[j-2]+1
                        }
                    }
                    # cat("z$cgroupcolor[[",i,"]][",j,"]=",z$cgroupcolor[[i]][j],",mycol=",mycol,"\n")
                    ft<-ft %>% color(i=i,j=mycol,color=color2hex(z$cgroupcolor[[i]][j]),part="header")
                }
                if(z$cgroupbg[[i]][j]!="white") {
                    # cat("z$cgroupbg[[",i,"]][",j,"]=",z$cgroupbg[[i]][j],"\n")
                    if(addcol){
                        if(j <= 1+addcol) {
                            mycol=j
                        } else{
                            mycol=cumsum(z$n.cgroup[[i]])[j-1-addcol]+1+addcol
                        }
                    } else{
                        if(j<=2){
                            mycol=1
                        } else{
                            mycol=cumsum(z$n.cgroup[[i]])[j-2]+1
                        }
                    }
                    ft<-ft %>% bg(i=i,j=mycol,bg=color2hex(z$cgroupbg[[i]][j]),part="header")
                }
            }
        }

    }
    if(!is.null(z$spanCol)){

        for(i in 1 :nrow(z$spanCol)){
            if(addcol){
                 from=z$spanCol[i,2]
                 to=z$spanCol[i,3]
                 #cat("z$spanCol[i,]=",z$spanCol[i,],",from=",from,",to=",to,"\n")
                 ft=merge_at(ft,i=z$spanCol[i,1]-1,j=from:to,part="body")
            } else{
                from=z$spanCol[i,2]-1
                to=z$spanCol[i,3]-1
                myi=z$spanCol[i,1]-1
                #cat("z$spanCol[i,]=",z$spanCol[i,],",myi=",myi,",from=",from,",to=",to,"\n")
                ft=merge_at(ft,i=myi,j=from:to,part="body")
            }
        }
    }
    if(!is.null(z$spanRow)){
        for(i in 1 :nrow(z$spanRow)){
            ft=merge_at(ft,j=z$spanRow[i,1]-ifelse(addcol,0,1),i=(z$spanRow[i,2]-1):(z$spanRow[i,3]-1))
        }
    }
    ft<- ft %>% align(align="center",part="header")
    ft

}

# ------------------------------------------------------------------------------
# from https://raw.githubusercontent.com/cardiomoon/ztable/master/R/print.ztable.html.R
#

#' Find rgb value from color name
#'
#'@param name a valid color name
#'@return rgb value
name2rgb=function(name){
    if(substr(name,1,1)=="#") {
        result=name
    } else{
    number=grep(paste("^",name,sep=""),ztable::zcolors$name)
    if(length(number)<1) result="white"
    else{
        rgb=ztable::zcolors[number[1],2]
        result=paste("#",rgb,sep="")
    }
    }
    result
}

#' Delete first components of align
#'
#' @param align A character for define the align of column in Latex format
align2nd=function(align){
    if(substr(align,1,1)=="|") {
        result=substr(align,2,nchar(align))
        result=align2nd(result)
    } else result=substr(align,2,nchar(align))
    result
}

#' Count the number of align
#'
#' @param align A character for define the align of column in Latex format
#' @export
alignCount=function(align){
    result=unlist(strsplit(align,"|",fixed=TRUE))
    temp=c()
    for(i in 1:length(result)) temp=paste(temp,result[i],sep="")
    nchar(temp)
}


#' Check the validity of align
#'
#' @param align A character for define the align of column in Latex format
#' @param ncount An integer equals of ncol function
#' @param addrow An integer
#' @export
alignCheck=function(align,ncount,addrow){
    count=alignCount(align)
    #cat("align=",align,"count=",count,"\n")
    while(count != (ncount+addrow)){
        if(count< (ncount+addrow)) align=paste(align,"c",sep="")
        else if(count > (ncount+addrow)) align=align2nd(align)
        count=alignCount(align)
        #cat("align=",align,"count=",count,"\n")
    }
    result=align
    result
}


#' Convert the align in Latex format to html format
#'
#' @param align A character of align in Latex format
#' @export
align2html=function(align){
    result=c()
    for(i in 1:nchar(align)){
        temp=substr(align,i,i)
        if(temp=="|") next
        temp=ifelse(temp=="l","left",ifelse(temp=="r","right","center"))
        result=c(result,temp)
    }
    result
}


#' Add or delete vertical lines in a ztable
#'
#' @param z An object of ztable
#' @param type An integer or one of c("none","all")
#' @param add An integer vector indicating columns where the width of vertical lines added
#' @param del An integer vector indicating columns where the width of vertical lines subtracted
#' @importFrom stringr str_remove_all fixed
#' @export
vlines=function(z,type=NULL,add=NULL,del=NULL){

    if(is.null(type) & is.null(add) & is.null(del)) {
        cat("\nvlines : add or delete vertical lines to a ztable\n
Usage: type must be one of these or NULL: 0-1 or \"none\",\"all\"\n
       add and del: An integer vector indicating position to add or delete vertical line(s)\n")

        return(z)
    }
    align=str_remove_all(z$align,fixed("|"))
    vlines=align2lines(z$align)
    colcount=colGroupCount(z)
    addrow=ifelse(z$include.rownames,1,0)
    #align=alignCheck(align,ncol(z$x),addrow)
    count=nchar(align)

    if(!is.null(type)) {
        vltype=NULL
        if(!is.numeric(type)) {
            if(toupper(type) == "NONE") vltype=0
            else if(toupper(type) == "ALL") vltype=1
            else return(z)
        }
        if((type>=0) & (type<=1)) vltype=type
        if(vltype==0) vlines=rep(0,count+1)
        else vlines=rep(1,count+1) #vltype=1

    }
    if(!is.null(add)){
        if(is.numeric(add)){
            for(i in 1:length(add)) {
                if(add[i]<1 | add[i]>(count+1)) next
                vlines[add[i]]=vlines[add[i]]+1
            }
        }
    }
    if(!is.null(del)){
        if(is.numeric(del)){
            for(i in 1:length(del)){
                if(del[i]<1 | del[i]>(count+1)) next
                if(vlines[del[i]]>0) vlines[del[i]]=vlines[del[i]]-1
            }
        }
    }
    newalign=vline2align(align,vlines)
    z$align=newalign
    z
}


#' Add or delete horizontal lines in a ztable
#'
#' @param z An object of ztable
#' @param type An integer or one of c("none","all")
#' @param add An integer vector indicating rows where the horizontal lines added
#' @param del An integer vector indicating rows where the horizontal lines deleted
#' @export
hlines=function(z,type=NULL,add=NULL,del=NULL){

    if(is.null(type) & is.null(add) & is.null(del)) {
        cat("\nhlines : add or delete horizontal lines to a ztable\n
            Usage: type must be one of these or NULL: 0-1 or \"none\",\"all\"\n
            add and del: An integer vector indicating position to add or delete horizontal line(s)\n")

        return(z)
    }

    count=nrow(z$x)
    if(!is.null(z$hline.after)) result=z$hline.after
    else result=c(-1,0,count)

    if(!is.null(type)) {
        if(!is.numeric(type)) {
            if(toupper(type) == "NONE") hltype=0
            else if(toupper(type) == "ALL") hltype=1
            else return(z)
        }
        if((type>=0) & (type<=1)) hltype=type
        if(hltype==0) result=c(-1,0,count)
        else result=c(-1,0,1:count)

    }
    if(!is.null(add)){
        if(is.numeric(add)){
            for(i in 1:length(add)) {
                result=c(result,add)
            }
        }
    }
    if(!is.null(del)){
        if(is.numeric(del)){
            result1=c()
            for(i in 1:length(result)){
                if(!(result[i] %in% del)) result1=c(result1,result[i])
            }
            result=result1
        }
    }
    z$hline.after=result
    z
}

#' Make a latex "align" from a string and vertical line specifier
#'
#' @param align A character string indicating align of latex table
#' @param vlines An integer vector indicating vertical line position
#' @export
vline2align=function(align,vlines){
    newalign=c()
    for(i in 1:nchar(align)) {
        if(vlines[i]>0) for(j in 1:vlines[i]) newalign=c(newalign,"|")
        newalign=c(newalign,substr(align,i,i))
    }
    last=vlines[length(vlines)]
    if(last>0) for(j in 1:last) newalign=c(newalign,"|")
    temp=newalign[1]
    if(length(newalign)>1)
        for(i in 2:length(newalign)) {
            temp=paste(temp,newalign[i],sep="")
        }
    temp
}

#' count the vertical column lines from align of Latex format
#'
#' @param align A string of align Latex format
#' @return a numeric vector consists of vertical lines of each column
#' @export
align2lines=function(align){
    result=c()
    length=nchar(align)
    count=0
    number=alignCount(align)
    for(i in 1:length){
        temp=substr(align,1,1)
        if(temp=="|") {
            count=count+1
            if(i==length) result=c(result,count)
        }
        else{
            result=c(result,count)
            count=0
        }
        align=substr(align,2,nchar(align))
    }
    if(length(result)==number) result=c(result,0)
    result
}

#' Make a character string indicating the alignment of components of table.
#'
#' @param z An object of ztable
#' @export
getNewAlign=function(z){
    #cat("z$align=",z$align,"\n")
    if(is.null(z$cgroup)) return(z$align)
    lines=align2lines(z$align)
    lines
    exAlign=str_remove_all(z$align,fixed("|"))
    exAlign
    ncount=ncol(z$x)
    addrow=ifelse(z$include.rownames,1,0)
    addrow
    colCount=colGroupCount(z)
    colCount
    result=c()
    start=1+addrow
    # Add column group align "c" if lines
    for(i in 1:length(colCount)){
        #cat("start=",start,"stop=",colCount[i]+addrow,",")
        result=paste(result,substr(exAlign,start=start,stop=(colCount[i]+addrow)),sep="")
        #cat("i=",i,",start=",start,"stop=",(colCount[i]+addrow),",result=",result)
        start=colCount[i]+1+addrow
        #cat(",line[start]=",start,"\n")
        if(lines[start]==0) result=paste(result,"c",sep="")
        #cat("result=",result,"\n")
    }
    result
    if(colCount[length(colCount)]<ncount)
        result=paste(result,substr(exAlign,start=start,stop=nchar(z$align)),sep="")
    result
    newlines=c()
    for(i in 1:length(lines)){
        if(i==1) newlines=lines[1]
        else newlines=c(newlines,lines[i])
        if((i-1) %in% colCount[-length(colCount)])
            if(lines[i+1]==0) newlines=c(newlines,0)
    }
    temp=c()
    for(i in 1:length(newlines)){
        if(newlines[i]>0) for(j in 1:newlines[i]) temp=paste(temp,"|",sep="")
        if(i>nchar(result)) break
        temp=paste(temp,substr(result,start=i,stop=i),sep="")
    }
    #temp=paste(temp,"c",sep="")
    temp
}


#' print html style
#' @param z An object of ztable
#' @export
myhtmlStyle=function(z){
    if(is.null(z$family)) family="times"
    else family=z$family

    cat("<head>")
    cat("<style>
        table {
              font-family:",family,";\n")
    cat("color: ",z$color,";\n")
    #cat("border: ",z$color," 1px solid;\n")
    cat("text-align: right;}
        th {
              padding: 1px 1px 5px 5px;
	        }
        td {
             padding: 1px 1px 5px 5px; }
      </style>")
    cat("</head>")
}

#' Print HTML head if ztable object a has a colgroup
#'
#' @param z An object of ztable
#' @export
printHTMLHead=function(z){
    if(is.null(z$cgroup)) return
    if(is.null(z$n.cgroup)) return
    #colCount=colGroupCount(z)
    ncount=ncol(z$x)
    addrow=ifelse(z$include.rownames,1,0)
    cGroupSpan=cGroupSpan(z)
    cGroupSpan
    totalCol=totalCol(z)
    totalCol

    vlines=align2lines(z$align)

    for(i in 1:length(z$cgroup)){
        cat("<tr>\n")
        if(z$include.rownames) {
            cat("<td style=\"")
            if(i==1) cat("border-top: 2px solid gray; border-bottom: hidden;")
            cat(paste(" border-left: ",vlines[1],"px solid black;",sep=""))
            if(z$cgroupbg[[i]][1]!="white")
                cat(paste("background-color: ",name2rgb(z$cgroupbg[[i]][1]),sep=""))
            if(z$cgroupcolor[[i]][1]!=z$color)
                cat(paste("color: ",name2rgb(z$cgroupcolor[[i]][1]),";",sep=""))
            cat("\"> </td>\n")
        }
        colSum=1
        for(j in 1:length(z$cgroup[[i]])) {
            if(is.na(z$cgroup[[i]][j])) {
                cat("<td colspan=\"",cGroupSpan[[i]][j],"\" align=\"center\" ")
                cat("style=\"")
                if(i==1) cat("border-top: 2px solid gray;")
                cat("border-bottom: hidden;")
                cat(paste(" border-left: ",vlines[colSum+1],"px solid black;",sep=""))
                colSum=colSum+cGroupSpan[[i]][j]
                #if(colSum==ncol(z$x)+1)
                cat(paste("border-right:",vlines[colSum+1],"px solid black;",sep=""))
                if(z$cgroupbg[[i]][j+1]!="white")
                    cat(paste("background-color: ",name2rgb(z$cgroupbg[[i]][j+1]),";",sep=""))
                if(z$cgroupcolor[[i]][j+1]!=z$color)
                    cat(paste("color: ",name2rgb(z$cgroupcolor[[i]][j+1]),";",sep=""))
                cat(paste("\"></td>\n",sep=""))
            } else {
                cat("<td colspan=\"",cGroupSpan[[i]][j],"\" align=\"center\" ")
                if(z$colnames.bold) cat("style=\"font-weight: bold;")
                else cat("style=\"font-weight: normal;")
                if(i==1) cat("border-top: 2px solid gray;")
                if(z$cgroup[[i]][j]!="") cat(" border-bottom: 1px solid gray;")
                else cat(" border-bottom: hidden;")
                cat(paste(" border-left: ",vlines[colSum+1],"px solid black;",sep=""))
                colSum=colSum+cGroupSpan[[i]][j]
                if(colSum==ncol(z$x)+1)
                cat(paste("border-right:",vlines[colSum+1],"px solid black;",sep=""))
                if(z$cgroupbg[[i]][j+1]!="white")
                    cat(paste("background-color: ",name2rgb(z$cgroupbg[[i]][j+1]),";",sep=""))
                if(z$cgroupcolor[[i]][j+1]!=z$color)
                    cat(paste("color: ",name2rgb(z$cgroupcolor[[i]][j+1]),";",sep=""))
                cat(paste("\">",z$cgroup[[i]][j],"</td>\n",sep=""))
            }
            #if((j < ncol(z$cgroup)) & ((colSum+j-1)<totalCol)) {
            if(j < length(z$cgroup[[i]])) {
                result=colSum+1
                if(result<=length(vlines)) {
                    if(vlines[result]==0){
                        cat("<td style=\"")
                        if(i==1) cat("border-top: 2px solid gray;")
                        cat("border-bottom: hidden\">&nbsp;</td>\n")
                    }
                }
            }
        }
        cat("</tr>\n")
    }
}


#' Print an object of class "ztable" to html table
#'
#' @param z An object of class "ztable"
#' @param xdata A formatted data.frame
ztable2html=function(z,xdata){
    ncount=ncol(z$x)
    addrow=ifelse(z$include.rownames,1,0)
     # caption position
    if(z$caption.position=="r") cposition="right"
    else if(z$caption.position=="l") cposition="left"
    else cposition="center"
    fontsize=ifelse(z$size>=5,11+(z$size-5)*2,10-(4-z$size))
    headingsize=fontsize-2

    rgroupcount=0
    printrgroup=1
    if(!is.null(z$n.rgroup)){
        if(length(z$n.rgroup)>1) {
            for(i in 2:length(z$n.rgroup)) {
                printrgroup=c(printrgroup,printrgroup[length(printrgroup)]+z$n.rgroup[i-1])
            }
        }
        rgroupcount=1
    }

    NewAlign=getNewAlign(z)
    totalCol=totalCol(z)
    colCount=colGroupCount(z)

    # rgroupcount=0
    # printrgroup=1
    # if(!is.null(z$n.rgroup)){
    #     if(length(z$n.rgroup)>1) {
    #         for(i in 2:length(z$n.rgroup)) {
    #             printrgroup=c(printrgroup,printrgroup[length(printrgroup)]+z$n.rgroup[i-1])
    #         }
    #     }
    #     rgroupcount=1
    # }

    # table position
    if(z$position=="flushleft") tposition="left"
    else if(z$position=="flushright") tposition="right"
    else tposition="center"
    #cat("<table class='gmisc_table'")
    myhtmlStyle(z)
    cat("<table ")
    cat(paste("align=\"",tposition,"\" style=\"border-collapse: collapse; caption-side:",
              z$caption.placement,"; font-size:",as.integer(fontsize),"pt;\">",sep=""))
    cat(paste("<caption style=\"text-align:",cposition,";",sep=""))
    if(z$caption.bold) cat("font-weight: bold")
    cat(paste("\">",z$caption,"</caption>",sep=""))
    if((z$show.heading==TRUE) & (!is.null(attr(z$x,"heading")))) {
        head=attr(z$x,"heading")
        for(i in 1:length(head)) {
            if(nchar(head[i])<1) next
            cat(paste("<tr>\n<td style=\"border-top: hidden; font-size: ",
                      as.integer(headingsize),"pt; padding: 0px 0px;\" colspan=\"",ncount+addrow,
                      "\"  align=\"left\" >",head[i],sep=""))
            cat("</td>\n</tr>\n")

        }
    }
    vlines=align2lines(z$align)
    printtop=1
    if(!is.null(z$cgroup)) {
        printHTMLHead(z)
        printtop=0
    }
    if(z$include.colnames) {
        cat("<tr>\n")
        subcolnames=ifelse(is.null(z$subcolnames),0,1)
        if(z$include.rownames) {
            result=1
            if(!is.null(isspanCol(z,1,1)))
                cat(paste("<th colspan=\"",isspanCol(z,1,1),"\"",sep=""))
            else if(!is.null(isspanRow(z,1,1))){
                result=isspanRow(z,1,1)
                if(result>0) cat(paste("<th rowspan=\"",result,"\"",sep=""))
            } else cat("<th ")
            cat(paste("style=\"border-left: ",vlines[1],
                                  "px solid black;",
                                  "background-color: ",name2rgb(z$cellcolor[1,1]),";",sep=""))
            if(printtop) cat("border-top: 2px solid gray;")
            if(subcolnames==0) cat("border-bottom: 1px solid gray;")
            else cat("border-bottom: hidden;")
            cat(paste("\">&nbsp;</th>\n",sep=""))
        }
        colpos=align2html(z$align)
        for(i in 1:ncol(z$x)) {
            result=1
            if(!is.null(isspanCol(z,1,(i+1)))){
                result=isspanCol(z,1,(i+1))
                if(result>0) cat(paste("<th colspan=\"",result,"\"",sep=""))
                else if(result==0) next
            } else if(!is.null(isspanRow(z,1,(i+1)))){
                result=isspanRow(z,1,(i+1))
                if(result>0) cat(paste("<th rowspan=\"",isspanRow(z,1,(i+1)),"\"",sep=""))
                else cat("<th")
            } else cat("<th ")
            if(result!=0){
                 cat("<th ")
                 drawbottom=0
                 if((subcolnames==1)) {
                     if(is.na(z$subcolnames[i])){
                         cat("rowspan=\"2\" ")
                         drawbottom=1
                     }
                 }
                 cat(paste("align=\"center\" ",sep=""))
                 if(z$colnames.bold) cat("style=\"font-weight: bold;")
                 else cat("style=\"font-weight: normal;")
                 cat(paste("border-left: ",vlines[i+1],"px solid black;",sep=""))
                 if((i==ncol(z$x)) & (length(vlines)>ncol(z$x)+1))
                     cat(paste("border-right:",vlines[i+2],"px solid black;",sep=""))
                 if((subcolnames==0) | (subcolnames+drawbottom==2))
                     cat("border-bottom: 1px solid gray;")
                 else cat("border-bottom: hidden;")
                 if(printtop) cat("border-top: 2px solid gray;")
                 if(z$cellcolor[1,i+1]!="white")
                     cat(paste("background-color: ",name2rgb(z$cellcolor[1,i+1]),";",sep=""))
                 if(z$frontcolor[1,i+1]!=z$color)
                     cat(paste("color: ",name2rgb(z$frontcolor[1,i+1]),";",sep=""))
                                  cat(paste("\">",colnames(z$x)[i],"</th>\n",sep=""))
                 if(i %in% colCount[-length(colCount)]) {
                     if(vlines[i+2]==0){
                        if(subcolnames==0) cat("<th style=\"border-bottom: 1px solid gray;")
                        else cat("<th style=\"border-bottom: hidden;")
                        if(printtop) cat("border-top: 2px solid gray; ")
                        if((z$cellcolor[1,i+1]!="white") & (z$cellcolor[1,i+1]==z$cellcolor[1,i+2]))
                            cat("background-color: ",name2rgb(z$cellcolor[1,i+1]),";")
                        cat("\">&nbsp;</th>\n")
                     }
                 }
            }
        }
        cat("</tr>\n")
        printtop=0
        if(subcolnames){
            cat("<tr>\n")
            if(addrow) {
                cat(paste("<th style=\"border-left: ",vlines[1],
                          "px solid black;","border-bottom: 1px solid gray;",
                          "background-color: ",name2rgb(z$cellcolor[1,1]),";",sep=""))
                cat(paste("\">&nbsp;</th>\n",sep=""))
            }
            for(i in 1:length(z$subcolnames)){
                if(is.na(z$subcolnames[i])) {
                    if(vlines[i+2]==0){
                        if(i!=length(z$subcolnames)){
                            cat("<th style=\"border-bottom: 1px solid gray;")
                            #if(printtop) cat("border-top: 2px solid gray;")
                            if((z$cellcolor[1,i+1]!="white") & (z$cellcolor[1,i+1]==z$cellcolor[1,i+2]))
                                cat("background-color: ",name2rgb(z$cellcolor[1,i+1]),";")
                            cat("\">&nbsp;</th>\n")
                        }
                    }
                    next
                }
                cat("<th align=\"center\" ")
                if(z$colnames.bold) cat("style=\"font-weight: bold;")
                else cat("style=\"font-weight: normal;")
                cat(paste("border-left: ",vlines[i+1],"px solid black;",sep=""))
                if((i==ncol(z$x)) & (length(vlines)>ncol(z$x)+1))
                    cat(paste("border-right:",vlines[i+2],"px solid black;",sep=""))
                cat("border-bottom: 1px solid gray;")
                if(z$cellcolor[1,i+1]!="white")
                    cat(paste("background-color: ",name2rgb(z$cellcolor[1,i+1]),";",sep=""))
                if(z$frontcolor[1,i+1]!=z$color)
                    cat(paste("color: ",name2rgb(z$frontcolor[1,i+1]),";",sep=""))
                cat(paste("\">",z$subcolnames[i],"</th>\n",sep=""))
                if(i %in% colCount[-length(colCount)]) {
                    if(vlines[i+2]==0){
                        cat("<th style=\"border-bottom: 1px solid gray;")
                        #if(printtop) cat("border-top: 2px solid gray;")
                        if((z$cellcolor[1,i+1]!="white") & (z$cellcolor[1,i+1]==z$cellcolor[1,i+2]))
                            cat("background-color: ",name2rgb(z$cellcolor[1,i+1]),";")
                        cat("\">&nbsp;</th>\n")
                    }
                }
            }
            cat("</tr>\n")
        }
    }
    colpos=align2html(z$align)
    addrow=ifelse(z$include.rownames,1,0)
    addrow
    rgroupprinted=0
    for(i in 1:nrow(z$x)){
        if(rgroupcount>0) {

            if(i %in% printrgroup) {
                rgroupprinted=1
                if(is.null(z$cspan.rgroup)){
                    temp=paste("<tr>\n<td colspan=\"",totalCol,
                               "\"  align=\"left\""," style=\"font-weight: bold;",sep="")
                    if(z$rgroupbg[rgroupcount]!="white")
                        temp=paste(temp,"background-color:",name2rgb(z$rgroupbg[rgroupcount]),";",sep="")
                    if(z$rgroupcolor[rgroupcount]!="black")
                        temp=paste(temp,"color:",name2rgb(z$rgroupcolor[rgroupcount]),";",sep="")
                    temp=paste(temp," border-left: ",vlines[1],"px solid black; ",sep="")
                    temp=paste(temp,"border-right:",vlines[ncol(z$x)+2],"px solid black;",sep="")
                    temp=paste(temp,"border-bottom: 1px solid black;",sep="")
                    temp=paste(temp,"border-top: 1px solid black;",sep="")
                    temp=paste(temp,"\">",z$rgroup[rgroupcount],"</td>\n",sep="")
                }
                else {
                    if(z$cspan.rgroup==1) {
                        temp=paste("<tr>\n<td align=\"left\""," style=\"font-weight: bold;",sep="")
                        # if(z$colcolor[1]!="white")
                        #     temp=paste(temp,"background-color:",name2rgb(z$colcolor[1]),";",sep="")
                        if(z$rgroupbg[rgroupcount]!="white")
                            temp=paste(temp,"background-color:",name2rgb(z$rgroupbg[rgroupcount]),";",sep="")
                        if(z$rgroupcolor[rgroupcount]!="black")
                            temp=paste(temp,"color:",name2rgb(z$rgroupcolor[rgroupcount]),";",sep="")
                        temp=paste(temp," border-left: ",vlines[1],"px solid black; ",sep="")
                        #temp=paste(temp,"border-bottom: 1px solid black;",sep="")
                        if(i!=1) temp=paste(temp,"border-top: hidden; ",sep="")
                        if(!is.null(z$hline.after)){
                            if((i-1) %in% z$hline.after)
                                temp=paste(temp,"border-top: 1px solid black;")
                        }
                        temp=paste(temp,"\">",z$rgroup[rgroupcount],"</td>\n",sep="")
                        for(j in 1:(ncount+addrow-1)){
                            temp1=paste("<td style=\"border-left: ",
                                        vlines[j+1],"px solid black; ",sep="")
                            if(!is.null(z$hline.after)){
                                if((i-1) %in% z$hline.after)
                                    temp1=paste(temp1,"border-top: 1px solid black;")
                            }
                            else if(i!=1) temp1=paste(temp1,"border-top: hidden; ",sep="")
                            if((j==ncol(z$x)) & (length(vlines)>ncol(z$x)+1))
                                temp1=paste(temp1,"border-right:",vlines[j+2],"px solid black;",sep="")
                            if(!is.null(z$colcolor)) {
                                if(z$colcolor[j+1]!="white")
                                    temp1=paste(temp1,"background-color:",
                                            name2rgb(z$colcolor[j+1])," ",sep="")
                            }
                            temp1=paste(temp1,"\"></td>\n",sep="")
                            if(is.null(isspanRow(z,i+1,j+1))) temp=paste(temp,temp1,sep="")
                            else if(isspanRow(z,i+1,j+1)>0) temp=paste(temp,temp1,sep="")

                            if(!is.null(colCount)){
                                if(j %in% colCount[-length(colCount)]) {
                                    if(vlines[j+2]==0){
                                        #if((z$cellcolor[i+1,j+1]!="white")&(z$cellcolor[i+1,j+1]==z$cellcolor[i+1,j+2]))
                                        #    temp=paste(temp,"<td style=\"background-color: ",
                                        #           name2rgb(z$cellcolor[i+1,j+1]),"\"></td>\n",
                                        #           sep="")
                                        #else temp=paste(temp,"<td></td>\n",sep="")

                                        temp=paste(temp,"<td",sep="")
                                        if(i!=1) temp=paste(temp,"style=\"border-top: hidden;\"")
                                        temp=paste(temp,"></td>\n",sep="")

                                    }
                                }
                            }
                        }
                    } else {
                        if(z$cspan.rgroup<1 | z$cspan.rgroup>(ncount+addrow))
                            z$cspan.rgroup=ncount+addrow

                        temp=paste("<tr>\n<td colspan=\"",z$cspan.rgroup,
                                   "\"  align=\"left\""," style=\"font-weight: bold;",sep="")
                        # if(z$colcolor[1]!="white")
                        #     temp=paste(temp,"background-color:",name2rgb(z$colcolor[1]),";",sep="")

                        if(z$rgroupbg[rgroupcount]!="white")
                            temp=paste(temp,"background-color:",name2rgb(z$rgroupbg[rgroupcount]),";",sep="")
                        if(z$rgroupcolor[rgroupcount]!="black")
                            temp=paste(temp,"color:",name2rgb(z$rgroupcolor[rgroupcount]),";",sep="")

                        temp=paste(temp," border-left: ",vlines[1],"px solid black; ",sep="")
                        temp=paste(temp,"border-bottom: 1px solid black;",sep="")
                        temp=paste(temp,"border-top: 1px solid black;",sep="")
                        if(!is.null(z$hline.after)){
                            if((i-1) %in% z$hline.after)
                                temp=paste(temp,"border-top: 1px solid black;")
                        }
                        temp=paste(temp,"\">",z$rgroup[rgroupcount],"</td>\n",sep="")

                        if(z$cspan.rgroup<(ncount+addrow)) {
                            for(j in (z$cspan.rgroup):(ncount+addrow-1)) {
                                temp1=paste("<td style=\"border-left: ",
                                            vlines[j+1],"px solid black; ",sep="")
                                if((j==ncol(z$x)) & (length(vlines)>ncol(z$x)+1))
                                    temp1=paste(temp1,"border-right:",vlines[j+2],"px solid black;",sep="")
                                #temp1=paste(temp1,"border-bottom: 1px solid black;",sep="")
                                #temp1=paste(temp1,"border-top: 1px solid black;",sep="")
                                if(!is.null(z$hline.after)){
                                    if((i-1) %in% z$hline.after)
                                        temp1=paste(temp1,"border-top: 1px solid black;")
                                }
                                else if(i!=1) temp1=paste(temp1,"border-top: hidden; ",sep="")
                                if(!is.null(z$colcolor)) {
                                    if(z$colcolor[j+1]!="white")
                                        temp1=paste(temp1,"background-color:",
                                                name2rgb(z$colcolor[j+1])," ",sep="")
                                }
                                temp1=paste(temp1,"\"></td>\n",sep="")
                                if(is.null(isspanRow(z,i+1,j+1))) temp=paste(temp,temp1,sep="")
                                else if(isspanRow(z,i+1,j+1)>0) temp=paste(temp,temp1,sep="")

                                if(!is.null(colCount)){
                                    if(j %in% colCount[-length(colCount)]) {
                                        if(vlines[j+2]==0) {
                                            #if((z$cellcolor[i+1,j+1]!="white")&(z$cellcolor[i+1,j+1]==z$cellcolor[i+1,j+2]))
                                            #    temp=paste(temp,"<td style=\"background-color: ",
                                            #           name2rgb(z$cellcolor[i+1,j+1]),"\"></td>\n",
                                            #           sep="")
                                            #else temp=paste(temp,"<td></td>\n",sep="")
                                            if(i!=1) temp=paste(temp,"<td style=\"border-top: hidden;\"",sep="")
                                            else temp=paste(temp,"<td",sep="")
                                            temp=paste(temp,"></td>\n")
                                        }
                                    }
                                }
                            }
                        }
                    }
                }
                cat(temp,"</tr>\n")
                rgroupcount=rgroupcount+1
            }
        }
        bcolor="white"
        #if(i %in% z$prefix.rows)
        #    if(is.numeric(z$zebra)) bcolor=z$zebra.color[i]
        #        cat("<tr style=\"background-color:",name2rgb(bcolor),"\">")
        cat("<tr>\n")
        if(z$include.rownames) {
            result=1
            if(!is.null(isspanCol(z,(i+1),1)))
                cat(paste("<td colspan=\"",isspanCol(z,i+1,1),"\"",sep=""))
            else if(!is.null(isspanRow(z,(i+1),1))){
                result=isspanRow(z,(i+1),1)
                if(result>0) cat(paste("<td rowspan=\"",result,"\"",sep=""))

            } else cat("<td ")
            if(result>0){
                #cat("result=",result,"\n")
                cat(paste(" style=\"border-left: ",vlines[1],"px solid black; ",sep=""))
                if(i==1 & printtop) cat("border-top: 2px solid gray;")
                else if(i!=1 | rgroupprinted) cat("border-top: hidden;")
                if(!is.null(z$hline.after)){
                    if((i-1) %in% z$hline.after)
                        if(!(i %in% printrgroup)) cat("border-top: 1px solid black;")
                }
                if(z$cellcolor[i+1,1]!="white")
                    cat(paste("background-color: ",name2rgb(z$cellcolor[i+1,1]),"; ",sep=""))
                if(z$frontcolor[i+1,1]!=z$color)
                    cat(paste("color: ",name2rgb(z$frontcolor[i+1,1]),"; ",sep=""))
                cat(paste("\">",rownames(z$x)[i],"</td>\n",sep=""))
            }

        }
        for(j in 1:ncount) {
            if(is.null(isspanCol(z,(i+1),(j+1)))){
                if(is.null(isspanRow(z,(i+1),(j+1)))){
                    result=-1
                    cat("<td ")
                } else {
                    result=isspanRow(z,(i+1),(j+1))
                    if(result > 0) {
                        cat("<td rowspan=\"",result,"\" ")
                    }
                }
                if((result==-1)|(result>1)){
                    cat(paste("align=\"",colpos[j+addrow],"\" style=\"border-left: ",
                              vlines[j+1],"px solid black;",sep=""))
                    if((j==ncol(z$x)) & (length(vlines)>ncol(z$x)+1))
                        cat(paste("border-right:",vlines[j+2],"px solid black;",sep=""))
                    if(i==1 & printtop) cat("border-top: 2px solid gray;")
                    else if(i!=1 | rgroupprinted) cat("border-top: hidden;")
                    if(!is.null(z$hline.after)){
                        if((i-1) %in% z$hline.after)
                            if(!(i %in% printrgroup)) cat("border-top: 1px solid black;")
                    }
                    if(z$cellcolor[i+1,j+1]!="white")
                        cat(paste("background-color: ",name2rgb(z$cellcolor[i+1,j+1]),";",sep=""))
                    if(z$frontcolor[i+1,j+1]!=z$color)
                        cat(paste("color: ",name2rgb(z$frontcolor[i+1,j+1]),";",sep=""))
                    cat("\">")
                    cat(paste(xdata[i,j],"</td>\n",sep=""))
                }
                if(j %in% colCount[-length(colCount)]) {
                    if(vlines[j+2]==0) {
                        backcolor=NULL
                        if(!is.null(z$rowcolor)){
                            if(z$rowcolor[i+1]!="white") backcolor=z$rowcolor[i+1]
                        }
                        if(is.null(backcolor)){
                            if((z$cellcolor[i+1,j+1]!="white")&(z$cellcolor[i+1,j+1]==z$cellcolor[i+1,j+2]))
                                backcolor=z$cellcolor[i+1,j+1]
                        }
                        cat("<td style=\"")
                        if(i==1 & printtop) cat("border-top: 2px solid gray;")
                        else if(i!=1 | rgroupprinted) cat("border-top: hidden;")

                        if(!is.null(backcolor)) cat(" background-color: ",name2rgb(backcolor),";")
                        cat("\"></td>\n")

                    }
                }
            } else {
                result=isspanCol(z,(i+1),(j+1))
                if(result>0) {
                    width=spanColWidth(z,(i+1),(j+1))
                    cat(paste("<td colspan=\"",result,"\" align=\"",colpos[j+addrow],"\" style=\"border-left: ",
                              vlines[j+1],"px solid black;",sep=""))
                    #if((j==ncol(z$x)) & (length(vlines)>ncol(z$x)+1))
                    cat(paste("border-right:",vlines[j+width+1],"px solid black;",sep=""))
                    if(i==1 & printtop) cat("border-top: 2px solid gray;")
                    else if(i!=1 | rgroupprinted) cat("border-top: hidden;")
                    if(!is.null(z$hline.after)){
                        if((i-1) %in% z$hline.after)
                            if(!(i %in% printrgroup)) cat("border-top: 1px solid black;")
                    }
                    if(z$cellcolor[i+1,j+1]!="white")
                        cat(paste("background-color: ",name2rgb(z$cellcolor[i+1,j+1]),";",sep=""))
                    if(z$frontcolor[i+1,j+1]!=z$color)
                        cat(paste("color: ",name2rgb(z$frontcolor[i+1,j+1]),";",sep=""))
                    cat("\">")
                    cat(paste(xdata[i,j],"</td>\n",sep=""))
                    if(isGroupCol(j,result,colCount)) {
                        if(vlines[j+width+1]==0) {

                            cat("<td style=\"")
                            if(i==1 & printtop) cat("border-top: 2px solid gray;")
                            else if(i!=1 | rgroupprinted) cat("border-top: hidden;")

                            if(!is.null(backcolor)) cat(" background-color: ",name2rgb(backcolor),";")
                            cat("\"></td>\n")
                        }
                    }
                }
            }

        }
        cat("</tr>\n")
    }
    if((z$show.footer!=TRUE) | (is.null(attr(z$x,"footer")))) footer=""
    else footer=attr(z$x,"footer")
    cat("<tr>\n")
    cat(paste("<td colspan=\"",totalCol,
              "\" align=\"left\" style=\"font-size:",as.integer(headingsize),
              "pt ;border-top: 1px solid black; border-bottom: hidden;\">",footer,"</td>\n",sep=""))
    cat("</tr>\n")
    cat("</table>\n")
}

#' Print an object of ztable via rstudio::viewer
#'
#' @param z An object of ztable
ztable2viewer=function(z){
    temp.f=tempfile(fileext=".html")
    sink(temp.f)
    cat(paste("<html>",
              "<head>",
              "<meta http-equiv=\"Content-type\" content=\"text/html;charset=UTF-8\">",
              "</head>",
              "<body>",
              "<div style=\"margin: 0 auto; display: table; margin-top: 1em;\">",
              sep="\n"))
    print(z,type="html")
    cat(paste("</div>","</body>","</html>",sep="\n"))
    sink()

    viewer <- getOption("viewer")
    if (!is.null(viewer) &&
            is.function(viewer)){
        # (code to write some content to the file)
        viewer(temp.f)
    }else{
        utils::browseURL(temp.f)
    }
}
# ------------------------------------------------------------------------------
# from https://raw.githubusercontent.com/cardiomoon/ztable/master/R/ztable2.R
#

#' Add row colors of an object of ztable
#'
#' @param z An object of ztable
#' @param rows An integer vector indicating specific rows
#' @param bg A character vector indicating background color
#' @param color A character vector indicating color
#' @param condition Logical expression to select rows
#' @importFrom magrittr "%>%"
#' @export
#' @examples
#' z=ztable(head(iris))
#' z=addRowColor(z,c(1,3),color="platinum")
#' z
addRowColor=function(z,rows=NULL,bg=NULL,color=NULL,condition=NULL){
    if(!is.null(bg)){
    for(i in 1:length(bg)) bg[i]=validColor(bg[i])
    selected=NULL
    selected <- if (!missing(condition)) {
        e <- substitute(condition)
        r <- eval(e, z$x, parent.frame())
        if (!is.logical(r))
            stop("'subset' must be logical")
        selected=which(r & !is.na(r) )+1
    }
    rows=c(rows,selected)
    if(is.null(rows)) rows=1:(nrow(z$x)+1)
    if(length(rows)>length(bg)) bg=rep(bg,1+length(rows)/length(bg))
    for(i in 1:length(rows))
        for(j in 1:ncol(z$cellcolor))
            z$cellcolor[rows[i],j]=bg[i]
        z$zebra.type=3
        z$zebra=3
        if(is.null(z$rowcolor)) z$rowcolor=rep("white",nrow(z$cellcolor))
        for(j in 1:length(rows)) z$rowcolor[rows[j]]=bg[j]

    }
    if(!is.null(color)){
    for(i in 1:length(color)) color[i]=validColor(color[i])
    if(length(rows)>length(color)) color=rep(color,1+length(rows)/length(color))
    for(i in 1:length(rows))
        for(j in 1:ncol(z$frontcolor))
              z$frontcolor[rows[i],j]=color[i]
    }
    z
}

#' Add column colors of an object of ztable
#'
#' @param z An object of ztable
#' @param cols An integer vector indicating specific columns
#' @param bg A character vector indicating background color
#' @param color A character vector indicating color
#'@export
#' @examples
#' z=ztable(head(iris))
#' z=addColColor(z,c(1,3),color="platinum")
#' z
addColColor=function(z,cols=NULL,bg=NULL,color=NULL){
    cols <- if (missing(cols))
        1:(ncol(z$x)+1)
    else {
        nl <- as.list(seq_along(z$x))
        names(nl) <- names(z$x)
        result=tryCatch(class(cols),error=function(e) "error")
        add=0
        if(result=="error") add=1
        eval(substitute(cols), nl, parent.frame())+add
    }
    if(!is.null(bg)){
    for(i in 1:length(bg)) bg[i]=validColor(bg[i])
    if(length(cols)>length(bg)) bg=rep(bg,1+length(cols)/length(bg))
    for(j in 1:length(cols))
        for(i in 1:nrow(z$cellcolor))
            z$cellcolor[i,cols[j]]=bg[j]
    z$zebra.type=3
    z$zebra=3
    if(is.null(z$colcolor)) z$colcolor=rep("white",ncol(z$cellcolor))
    for(j in 1:length(cols)) z$colcolor[cols[j]]=bg[j]
    }
    if(!is.null(color)){
    for(i in 1:length(color)) color[i]=validColor(color[i])
    if(length(cols)>length(color)) color=rep(color,1+length(cols)/length(color))
    for(j in 1:length(cols))
        for(i in 1:nrow(z$frontcolor))
            z$frontcolor[i,cols[j]]=color[j]
    }
    z
}

#' Add column colors of an object of ztable
#'
#' @param z An object of ztable
#' @param rows An integer vector indicating specific rows
#' @param cols An integer vector indicating specific columns
#' @param bg A character vector indicating background color
#' @param color A character vector indicating color
#' @param condition Logical expression to select rows
#' @export
#' @examples
#' z=ztable(head(iris))
#' z=addRowColor(z,c(1,3),color="platinum")
#' z=addColColor(z,2,color="syan")
#' z=addCellColor(z,cols=c(5,4),rows=5,color="red")
#' z
addCellColor=function(z,rows=NULL,cols=NULL,bg=NULL,color=NULL,condition=NULL){
    selected=NULL
    selected <- if (!missing(condition)) {
        e <- substitute(condition)
        r <- eval(e, z$x, parent.frame())
        if (!is.logical(r))
            stop("'subset' must be logical")
        selected=which(r & !is.na(r) )+1
    }
    rows=c(rows,selected)
    if(is.null(rows)) rows=1:(nrow(z$x)+1)
    cols <- if (missing(cols))
        1:(ncol(z$x)+1)
    else {
        nl <- as.list(seq_along(z$x))
        names(nl) <- names(z$x)
        result=tryCatch(class(cols),error=function(e) "error")
        add=0
        if(result=="error") add=1
        eval(substitute(cols), nl, parent.frame())+add
    }
    # while(length(rows)!=length(cols)){
    #     if(length(rows)<length(cols)){
    #         rows=c(rows,rows)
    #         if(length(rows)>length(cols)) rows=rows[1:length(cols)]
    #     }
    #     if(length(rows)>length(cols)){
    #         cols=c(cols,cols)
    #         if(length(cols)>length(rows)) cols=cols[1:length(rows)]
    #     }
    # }
    if(!is.null(bg)){
    for(i in 1:length(bg)) bg[i]=validColor(bg[i])
    if(length(cols)>length(bg)) bg=rep(bg,1+length(cols)/length(bg))
    for(i in 1:length(rows)) {
        for(j in 1:length(cols)){
           z$cellcolor[rows[i],cols[j]]=bg[j]
        }
    }
    }
    if(!is.null(color)){
    for(i in 1:length(color)) color[i]=validColor(color[i])
    if(length(cols)>length(color)) color=rep(color,1+length(cols)/length(color))
    for(i in 1:length(rows)) {
        for(j in 1:length(cols)){
            z$frontcolor[rows[i],cols[j]]=color[j]
        }
    }
    # for(i in 1:length(cols)) {
    #     z$frontcolor[rows[i],cols[i]]=color[i]
    #     result=getspanRowLength(z,rows[i],cols[i])
    #
    #     if(!is.null(result)){
    #         if(result>1){
    #             for(j in 1:(result-1)) z$frontcolor[(rows[i]+j),cols[i]]=color[i]
    #         }
    #     }
    # }
    }
    z$zebra.type=3
    z$zebra=3
    z
}

#' Add column colors of an object of ztable
#'
#' @param z An object of ztable
#' @param rows An integer vector indicating specific rows
#' @param cols An integer vector indicating specific columns
#' @param color A character vector indicating color
#' @export
#' @examples
#' z=ztable(head(iris))
#' z=addFrontColor(z,rows=2:4,cols=c(2,4,6),color=c("red","green","blue"))
#' z
addFrontColor=function(z,rows,cols,color){
    for(i in 1:length(color)) color[i]=validColor(color[i])
    if(length(cols)>length(color)) color=rep(color,1+length(cols)/length(color))

    for(i in 1:length(rows)) {
        for(j in 1:length(cols)){
            z$frontcolor[rows[i],cols[j]]=color[j]
            result=getspanRowLength(z,rows[i],cols[j])
            if(!is.null(result)){
                if(result>1){
                    for(k in 1:(result-1)) z$frontcolor[(rows[i]+k),cols[j]]=color[j]
                }
             }
        }
    }
    z
}

#' Gets spanRow length
#'
#'@param z An object of ztable
#'@param i An integer indicating the row of specific cell
#'@param j An integer indicating the column of specific cell
#'@export
#'@return row count when spanRow starts, 0 when column spans.
getspanRowLength=function(z,i,j){
    if(is.null(z$spanRow)) return(NULL)
    newspan=z$spanRow
    for(k in 1:nrow(newspan)) {
        if(newspan[k,1]!=j) next
        if(newspan[k,2]>i) next
        if(newspan[k,2]==i) return(newspan[k,3]-newspan[k,2]+1)
        else if((newspan[k,2]<j) & (newspan[k,3]>=j)) return(0)
        else next
    }
    return(NULL)
}

#' Add column groups of an object of ztable
#'
#'@param z An object of ztable
#'@param cgroup A character vector or matrix indicating names of column group. Default value is NULL
#'@param n.cgroup A integer vector or matrix indicating the numbers of columns included in each cgroup
#'       Default value is NULL
#'@param color A character vector indicating the font color of each cells.
#'@param bg A character vector indicating the background color of each cells.
#'@param top Logical. Whether or not cgroup be placed at top.
#'@export
addcgroup=function(z,cgroup,n.cgroup,color="black",bg="white",top=FALSE){

    if(length(color)==1){
        color=rep(color,length(cgroup)+1)
    } else{
        color=c("black",color)
    }
    if(length(bg)==1){
        bg=rep(bg,length(cgroup)+1)
    } else{
        bg=c("white",bg)
    }

    if(length(z$cgroup)==0) {
        z$cgroup=list()
        z$cgroup[[1]]=cgroup
        z$cgroupcolor=list()
        z$cgroupcolor[[1]]=color
        z$cgroupbg=list()
        z$cgroupbg[[1]]=bg
        z$n.cgroup=list()
        z$n.cgroup[[1]]=n.cgroup
    } else{
        if(top){
            no=length(z$cgroup)
            for(i in no:1){
                z$cgroup[[no+1]]=z$cgroup[[no]]
                z$cgroupcolor[[no+1]]=z$cgroupcolor[[no]]
                z$cgroupbg[[no+1]]=z$cgroupbg[[no]]
                z$n.cgroup[[no+1]]=z$n.cgroup[[no]]
            }
            z$cgroup[[1]]=cgroup
            z$cgroupcolor[[1]]=color
            z$cgroupbg[[1]]=bg
            z$n.cgroup[[1]]=n.cgroup
        } else{
            no=length(z$cgroup)+1
            z$cgroup[[no]]=cgroup
            z$cgroupcolor[[no]]=color
            z$cgroupbg[[no]]=bg
            z$n.cgroup[[no]]=n.cgroup
        }
    }

    z
}

#' Add row groups of an object of ztable
#'
#'@param z An object of ztable
#'@param rgroup A character vector indicating names of row group. Default value is NULL
#'@param n.rgroup A integer vector indicating the numbers of rows included in each rgroup
#'       Default value is NULL
#'@param cspan.rgroup An integer indicating the column span of rgroup
#'@param color A character vector indicating the font color of rgroup.
#'@param bg A character vector indicating the background color of rgroup.
#'@export
addrgroup=function(z,rgroup,n.rgroup,cspan.rgroup=NULL,color="black",bg="white"){
    if(is.null(rgroup)) return(z)
    for(i in 1:length(rgroup)) {
        if(is.na(rgroup[i])) rgroup[i]=""
    }
    z$rgroup=rgroup
    z$n.rgroup=n.rgroup
    z$cspan.rgroup=cspan.rgroup
    if(length(bg)==1) bg=rep(bg,length(rgroup))
    if(length(color)==1) color=rep(color,length(rgroup))
    z$colcolor=rep(bg,ncol(z$cellcolor))
    z$rgroupcolor=color
    z$rgroupbg=bg
    z
}

#' Count the colgroup of an object of ztable
#'
#' @param z An object of class ztable
#' @return A vector indicating the position of colgroup
#'@export
colGroupCount=function(z){
    if(is.null(z$cgroup)) return(NULL)
    if(is.null(z$n.cgroup)) return(NULL)
    result=c()
    for(i in 1:length(z$n.cgroup)){
        count=0
        for(j in 1:length(z$n.cgroup[[i]])) {
            if(is.na(z$n.cgroup[[i]][j])) break
            count=count+z$n.cgroup[[i]][j]
            result=c(result,count)
        }
    }
    a=unique(result)
    a[order(a)]
}

#' Count the colspan of each colgroup
#'
#' @param z An object of ztable
#' @return A matrix indicating the column span occupied by each colgroup
#' @export
cGroupSpan=function(z){
    (vlines=align2lines(z$align))
    (colCount=colGroupCount(z))

    newCount=c()
    addrow=ifelse(z$include.rownames,1,0)
    for(i in 1:length(colCount)) {
        if(vlines[colCount[i]+1+addrow]==0) newCount=c(newCount,colCount[i])
    }
    newCount
    if(is.null(newCount)) return(z$n.cgroup)
    result=z$n.cgroup
    for(i in 1:length(z$n.cgroup)){
        start=0
        for(j in 1:length(z$n.cgroup[[i]])) {
            if(is.na(z$n.cgroup[[i]][j])) break
            end=start+z$n.cgroup[[i]][j]
            count=0
            for(k in 1:length(newCount)){
                if(newCount[k]>start & newCount[k]<end) count=count+1
            }
            result[[i]][j]=result[[i]][j]+count
            #cat("start=",start,",end=",end,",result[",i,",",j,"]=",result[i,j],"\n")
            start=end
        }
    }
    result
}

#' Print the head of latex table if the object of ztable has a colgroup
#'
#' @param z An object of ztable
#' @export
printLatexHead=function(z){
    if(is.null(z$cgroup)) return
    if(is.null(z$n.cgroup)) return
    #colCount=colGroupCount(z)
    ncount=ncol(z$x)
    addrow=ifelse(z$include.rownames,1,0)
    cGroupSpan=cGroupSpan(z)
    totalCol=totalCol(z)
    vlines=align2lines(z$align)
    #vlines=align2lines(getNewAlign(z))
    #vlines

    for(i in 1:length(z$cgroup)){
            colSum=0
            linecount=1
            if(z$include.rownames) {
                firstrow=cat(paste("\\cellcolor{",z$cgroupbg[[i]][1],"} &",sep=""))
                colSum=1
                linecount=1
            }
            for(j in 1:length(z$cgroup[[i]])) {
                if(is.na(z$cgroup[[i]][j])) break
                mcalign="c"
                if((j==1) & (addrow==0) & (vlines[linecount+1]>0))
                    for(k in 1:vlines[linecount+1]) mcalign=paste("|",mcalign,sep="")
                end=colSum+cGroupSpan[[i]][j]+1
                linecount=linecount+z$n.cgroup[[i]][j]
                if(vlines[linecount+1]>0)
                    for(k in 1:vlines[linecount+1]) mcalign=paste(mcalign,"|",sep="")
                second=paste("\\multicolumn{",cGroupSpan[[i]][j],"}{",mcalign,"}{",sep="")
                colSum=colSum+cGroupSpan[[i]][j]
                if(z$cgroupbg[[i]][j+1]!="white")
                    second=paste(second,"\\cellcolor{",z$cgroupbg[[i]][j+1],"}",sep="")
                if(z$cgroupcolor[[i]][j+1]!=z$color) {
                    second=paste(second,"\\color{",z$cgroupcolor[[i]][j+1],"}",sep="")
                }
                if(z$colnames.bold)
                    second=paste(second,"\\textbf{",z$cgroup[[i]][j],"}}",sep="")
                else second=paste(second,z$cgroup[[i]][j],"}",sep="")

                if(j!=1) second=paste("&",second,sep="")
                cat(second)
                if(linecount<(ncol(z$x)+1)) if(vlines[linecount+1]==0) cat("&")
            }
            cat("\\\\ \n")
            colSum=addrow+1
            start=1
            for(j in 1:length(z$cgroup[[i]])) {
                if(is.na(z$cgroup[[i]][j])) break
                if(z$cgroup[[i]][j]!="")
                    cat(paste("\\cline{",colSum,"-",colSum+cGroupSpan[[i]][j]-1,"}",sep=""))
                colSum=colSum+cGroupSpan[[i]][j]
                start=start+z$n.cgroup[[i]][j]
                if(j < length(z$cgroup[[i]])) if(vlines[start+1]==0) colSum=colSum+1

            }
            cat("\n")
    }
}


#' Calculating total columns of ztable
#'
#' @param z An object of ztable
#' @export
totalCol=function(z){
    ncount=ncol(z$x)
    addrow=ifelse(z$include.rownames,1,0)
    result=ncount+addrow
    vlines=align2lines(z$align)
    if(!is.null(z$cgroup)) {
        colCount=colGroupCount(z)
        if(length(colCount)>1){
        for(i in 1:(length(colCount)-1)) {
            if(vlines[colCount[i]+2]==0) result=result+1
        }
        }
    }
    result
}

#' Merging data cells of ztable object in columns
#'
#' @param z An object of ztable
#' @param row An integer indicating the row of merging data cell
#' @param from An integer indicating start column of merging data cell
#' @param to An integer indicating end column of merging data cell
#' @param bg An optional character indicating the background color of merging cell
#' @param color An optional character indicating the font color of merging cell
#' @export
spanCol=function(z,row,from,to,bg=NULL,color=NULL){
    if(length(row)!=1) {
        warning("Only one row is permitted")
        return(z)
    }
    if(row<0 | (row > (nrow(z$x)+1))) {
        warning("Out of range : row")
        return(z)
    }
    if(from>to){
        warning("\"to\" must be equal to or greater than \"from\"")
        return(z)
    }
    if(is.null(z$spanCol)) z$spanCol=matrix(c(row,from,to),nrow=1)
    else z$spanCol=rbind(z$spanCol,c(row,from,to))
    #colnames(z$spanCol)=c("row","from","to")
    z=addCellColor(z,cols=from,rows=row,bg=bg,color=color)
    z
}


#' Merging data cells of ztable object in rows
#'
#' @param z An object of ztable
#' @param col An integer indicating the column of merging data cell
#' @param from An integer indicating start row of merging data cell
#' @param to An integer indicating end row of merging data cell
#' @param bg An optional character indicating the background color of merging cell
#' @param color An optional character indicating the font color of merging cell
#' @export
spanRow=function(z,col,from,to,bg=NULL,color=NULL){
    if(length(row)!=1) {
        warning("Only one row is permitted")
        return(z)
    }
    if(col<0 | col > (ncol(z$x)+1)) {
        warning("Out of range : col")
        return(z)
    }
    if(from>to){
        warning("\"to\" must be equal to or greater than \"from\"")
        return(z)
    }
    if(is.null(z$spanRow)) z$spanRow=matrix(c(col,from,to),nrow=1)
    else z$spanRow=rbind(z$spanRow,c(col,from,to))
    #colnames(z$spanRow)=c("col","from","to")
    #if(!is.null(color)) z=addCellColor(z,cols=col,rows=from,color=color)
    z=addCellColor(z,cols=col,rows=from,bg=bg,color=color)
    z
}

#' Identify the spanCol status of a cell
#'
#'@param z An object of ztable
#'@param i An integer indicating the row of specific cell
#'@param j An integer indicating the column of specific cell
#'@return column plus space count when spanCol starts, 0 when column spans,
#'        minus value when spanCol ends, NULL when no span.
isspanCol=function(z,i,j){
    if(is.null(z$spanCol)) return(NULL)
    newspan=getNewSpanCol(z)
    for(k in 1:nrow(newspan)) {
        if(newspan[k,1]!=i) next
        if(newspan[k,2]>j) next
        if(newspan[k,2]==j) return(newspan[k,3]-newspan[k,2]+1)
        else if((newspan[k,2]<j) & (z$spanCol[k,3]>=j)) return(0)
        else next
    }
    return(NULL)
}

#' Calculate the spanColWidth when spanCol start
#'
#'@param z An object of ztable
#'@param i An integer indicating the row of specific cell
#'@param j An integer indicating the column of specific cell
#
#'@return column count when spanCol start
spanColWidth=function(z,i,j){
    if(is.null(z$spanCol)) return(NULL)
    newspan=z$spanCol
    for(k in 1:nrow(newspan)) {
        if(newspan[k,1]!=i) next
        if(newspan[k,2]>j) next
        if(newspan[k,2]==j) return(newspan[k,3]-newspan[k,2]+1)
        else next
    }
    return(NULL)
}

#' Calculating new spanCol with spanCol plus space made by column group
#'
#'@param z An object of ztable
#'@export
getNewSpanCol=function(z){
    result=z$spanCol
    result1=result
    result1
    if(is.null(z$cgroup)) return(result)
    if(is.null(colGroupCount(z))) return(result)
    vlines=align2lines(z$align)
    vlines
    addcol=ifelse(z$include.rownames,1,0)

    #colCount=colGroupCount(z)+1
    colCount=colGroupCount(z)+addcol
    newCount=c()
    colCount
    for(i in 1:length(colCount)) {
        if(vlines[colCount[i]+1]==0) newCount=c(newCount,colCount[i])
    }
    if(is.null(newCount)) return(result)
    for(i in 1:nrow(result)) {
        for(j in 1:(length(newCount))) {
            if((result[i,2]<=newCount[j]) & (result[i,3]>newCount[j])) {
                result1[i,3]=result1[i,3]+1
            }
        }
    }
    return(result1)
}

#' Identify the spanRow status of a cell
#'
#'@param z An object of ztable
#'@param i An integer indicating the row of specific cell
#'@param j An integer indicating the column of specific cell
#'@return columns count plus spaces by rgroup when spanRow starts, 0 when row spans,
#'        minus value when spanRow ends, NULL when no span.
isspanRow=function(z,i,j){
    if(is.null(z$spanRow)) return(NULL)
    newspanRow=getNewSpanRow(z)
    for(k in 1:nrow(z$spanRow)) {
        if(z$spanRow[k,1]!=j) next
        if(z$spanRow[k,2]>i) next
        if(z$spanRow[k,2]==i) return(newspanRow[k,3]-newspanRow[k,2]+1)
        else if(z$spanRow[k,3]==i) return(-(newspanRow[k,3]-newspanRow[k,2]+1))
        else if((z$spanRow[k,2]<i) & (z$spanRow[k,3]>i)) return(0)
        else next
    }
    return(NULL)
}

#'Gets the spanRow start column
#'
#'@param z An object of ztable
#'@param i An integer indicating the row of specific cell
#'@param j An integer indicating the column of specific cell
#'
#'@return An integer indicating column where spanRow start. This function is for latex
#'        multirow
getspanRowData=function(z,i,j){
    for(k in 1:nrow(z$spanRow)) {
        if(z$spanRow[k,1]!=j) next
        if(z$spanRow[k,2]>=i) next
        if(z$spanRow[k,3]==i) return(z$spanRow[k,2])
    }
    return(NULL)
}

#' Calculating new spanRow with spanRow plus space made by row group
#'
#'@param z An object of ztable
getNewSpanRow=function(z){
    result=z$spanRow
    result1=result
    if(is.null(z$rgroup)) return(result)
    if(is.null(z$n.rgroup)) return(result)
    #colCount=colGroupCount(z)+1
    printrgroup=1
    if(!is.null(z$n.rgroup)){
        if(length(z$n.rgroup)>1) {
            for(i in 2:length(z$n.rgroup)) {
                printrgroup=c(printrgroup,printrgroup[length(printrgroup)]+z$n.rgroup[i-1])
            }
        }
    }
    for(i in 1:nrow(result)) {
        for(j in 2:(length(printrgroup))) {
            if((result[i,2]<=printrgroup[j]) & (result[i,3]>printrgroup[j])) {
                result1[i,3]=result1[i,3]+1
            }
        }
    }
    result1
}

#' Returns whether or not column with position start plus length is group column
#'
#' @param start An integer indicating start column position
#' @param length An integer indicating spanCol length
#' @param colCount An integer vector calculating from colGroupCount()
#' @export
isGroupCol=function(start,length,colCount){

    if(is.null(colCount)) return(0)
    newstart=start
    for(i in 1:length(colCount)){
        if(colCount[i]<start) newstart=start+1
    }
    result=colCount
    for(i in 1:length(colCount)){
        result[i]=colCount[i]+(i-1)+1
    }
    if((newstart+length) %in% result[-length(result)]) return(1)
    else return(0)
}

#' Add a adjunctive name below column name in a ztable
#'
#'@param z An object of ztable
#'@param subcolnames A charactor vector
#'@export
addSubColNames=function(z,subcolnames){
    if(length(subcolnames)!=length(z$x))
        warning("length of subconames is different from length of z$x")
    else z$subcolnames=subcolnames
    z
}

#' Add row color or cellcolor for rows or cells of p-value less than sigp in a ztable
#'
#'@param z An object of ztable
#'@param level A p-value
#'@param bg A character indicating background color
#'@param color A character indicating color
#'@export
addSigColor=function(z,level=0.05,bg="lightcyan",color="black"){

    if("ztable.mytable" %in% class(z))  {
        if(is.null(z$cgroup)){
            temp=z$x[[ncol(z$x)]]
            temp[temp=="< 0.001"]=0
            below05=which(as.numeric(temp)<level)+1
            if(length(below05)>0) {
                z1=addRowColor(z,rows=below05,bg=bg,color=color)
            } else{
                z1=z
            }
        } else{
            count=length(z$cgroup[[1]])-1
            count
            colpergroup=(ncol(z$x)-1)/count
            colpergroup
            z1<-z
            for(i in 2:(count+1)){
                pcol=1+colpergroup*(i-1)
                temp=z$x[[pcol]]
                temp[temp=="< 0.001"]=0
                below05=which(as.numeric(temp)<level)+1
                if(length(below05)>0) for(j in 1:length(below05))
                    z1=addCellColor(z1,rows=below05[j],
                                   cols=(pcol+1-(colpergroup-1)):(pcol+1),bg=bg,color=color)

            }
        }
    } else {
        if(!is.null(z$pcol)){
            temp=z$x[[z$pcol]]
            below05=which(as.numeric(temp)<level)+1
            if(length(below05)>0){
                z1=addRowColor(z,rows=below05,bg=bg,color=color)
            }  else{
                z1<-z
            }
        } else{
            z1=z
        }
    }
    z1
}


# ------------------------------------------------------------------------------
# Original content
# from https://raw.githubusercontent.com/cardiomoon/ztable/master/R/ztable.R
#

#' Exporting a R object to an object of class "ztable"
#'
#' Exporting a R object to an object of class "ztable"
#' @param x An R object, mainly data.frame
#'@param digits Numeric vector of length equal to one (in which case it will be
#'       replicated as necessary) or to the number of columns of the resulting table
#' @param ... arguments to be passed to \code{\link{ztable_sub}}
#' @export
ztable=function(x,digits=NULL,...)  UseMethod("ztable")


#'@describeIn ztable Default method of ztable
#'@export
ztable.default=function(x,digits=NULL,...){
  cat(paste("\n Sorry, Currently function ztable() cannot handle",
      " the object of class ",class(x),"!\n",sep=""))
  invisible()
}


#'@describeIn ztable  Makes a ztable for class 'data.frame'
#'@export
ztable.data.frame=function(x,digits=NULL,...){
    z=ztable_sub(x,digits=digits,...)
    class(z) <-c("ztable")
    z
}

#' Exporting "data.frame" to an object of class "ztable"
#'
#' Exporting "data.frame" to an object of class "ztable"
#'@param x A data.frame
#'@param family Font family. Default value is NULL. Possible value is one of the c("serif","times").
#'@param size An integer from 1 to 10 indicating font size= c("tiny","scriptsize",
#'       "footnotesize","small","normalsize","large","Large","LARGE","huge","Huge")
#'       respectively. Defaulting is 5(= "normalsize").
#'@param color A character indicating color of ztable
#'@param tablewidth A numeric value indicating desired table width as a ratio to linewidth.
#'       This value is only useful when caption is longer than table length.
#'       Default value is 0.3.
#'@param type character indicating formats of ztable, either "html" or "latex".
#'       Default value is "latex"
#'@param include.rownames A logical value whether or not include rownames in the table
#'       Default value is TRUE.
#'@param placement The table will have placement given by placement where placement
#'       must be NULL or contain only elements of {"h","t","b","p","!","H"}.
#'       Default value is "!hbtp".
#'@param position The table will be have placed at the center of the paper
#'        if position is "center" or "c", and at the left side of the paper
#'        if it equals "left" or "l", and at the right side of the paper
#'        if it equals "right" or "r". The position is translated to specific
#'        latex environments such as "flushright" or "flushleft" or "center"
#'        (provided as a character vector) will enclose the tabular environment.
#'        Default value is "center".
#'@param show.heading A logical value whether or not include headings in the table.
#'        Default value is TRUE.
#'@param show.footer A logical value whether or not include headings in the table.
#'        Default value is TRUE.
#'@param caption A character
#'@param caption.placement The caption will be have placed at the top of the table
#'        if caption.placement is "top" and at the bottom of the table
#'        if it equals "bottom". Default value is "top".
#'@param caption.position The caption will be have placed at the center of the table
#'        if caption.position is "center" or "c", and at the left side of the table
#'        if it equals "left" or "l", and at the right side of the table
#'        if it equals "right" or "r". Default value is "center".
#'@param caption.bold whether or not use bold font for caption
#'@param align Character vector : nchar equal to the number of columns of the
#'       resulting table indicating the alignment of the corresponding columns.
#'@param digits Numeric vector of length equal to one (in which case it will be
#'       replicated as necessary) or to the number of columns of the resulting table
#'@param display Character vector of length equal to the number of columns of the
#'       resulting table indicating the format for the corresponding columns.
#'       Since the row names are printed in the first column, the length of display
#'       is one greater than ncol(x) if x is a data.frame. These values are passed
#'       to the formatC function. Use "d" (for integers), "f", "e", "E", "g", "G",
#'       "fg" (for reals), or "s" (for strings). "f" gives numbers in the usual
#'       xxx.xxx format; "e" and "E" give n.ddde+nn or n.dddE+nn (scientific format);
#'       "g" and "G" put x[i] into scientific format only if it saves space to do so.
#'       "fg" uses fixed format as "f", but digits as number of significant digits.
#'       Note that this can lead to quite long result strings. Default value is NULL.
#'       the class of x.
#'@param sidewaystable Logical value whether or not set the tabular environment=
#'       "sidewaystable". Requires Latex "rotating" package in preamble.
#'       Default value is FALSE.
#'@param longtable Logical value whether or not set the tabular environment=
#'       "longtable". Requires Latex "longtable" package in preamble.
#'       Default value is FALSE.
#'@param wraptable Logical value whether or not set the tabular environment=
#'       "wraptable". Requires Latex "wrapfig" package in preamble.
#'       Default value is FALSE.
#'@param rotate Logical value whether or not set the tabular environment=
#'       "rotate". No special arrangement is made to find space for the result.
#'       Requires Latex "rotating" package in preamble.
#'       If TRUE, requires the rotate angle(counterclockwise).
#'       Default value is FALSE.
#'@param turn Logical value whether or not set the tabular environment=
#'       "turn". In this environment, Latex leaves space for the rotated table.
#'       Requires Latex "rotating" package in preamble.
#'       If TRUE, requires the rotate angle.
#'       Default value is FALSE.
#'@param angle An integer indicate the angle to rotate(degree); range -180 to 180.
#'       Default value is 0.
#'@param wraptablewidth A integer indicate wraptable width in centimeter. Default=12.
#'@param tabular Logical value whether or not set the tabular environment.
#'       If TRUE, no tabular environment is set. Default value is FALSE.
#'@param label Character vector of length 1 containing the LaTeX label or HTML anchor.
#'       Set to NULL to suppress the label. Default value is NULL.
#'@param hline.after A vector of numbers between -1 and "nrow(x)", inclusive,
#'       indicating the rows after which a horizontal line should appear.
#'       If NULL is used no lines are produced. Default value is c(-1,0,nrow(x))
#'       which means draw a line before and after the columns names and at the
#'       end of the table. Repeated values are allowed.
#'@param booktabs Logical value. If TRUE, the toprule, midrule and bottomrule tags
#'       from the LaTex "booktabs" package are used rather than hline for the
#'       horizontal line tags. Requires Latex "booktabs" package in preamble.
#'       Default value is TRUE.
#'@param prefix.rows A numeric vector contains the position of rows on which
#'       extra Latex commands should be added as a prefix.
#'@param commands A character vector of the length 1 or same length of the nrow of
#'       data.frame which contains the command that should be added as a prefix at
#'       the specified rows. Default value is NULL, i.e. do not add commands.
#'@param top.command A character vector of the length 1 which contains the command
#'       that should be added as a prefix at the colnames row.
#'@param zebra Null or an integer of 0 or 1 or 2 or 3. The arguments zebra and zebra.color are
#'       used to make a Zebra striping table(table with alternating background colors)
#'       easily. A value of 1 sets background color of all odd rows/columns with specified with
#'       zebra.color. A value of 2 sets all even rows/columns. A value of 0 sets
#'       background colors of all rows/columns with colors specified with zebra.color.
#'       When zebra is 1 or 2, the parameters of prefix.rows and commands ignored.
#'       When zebra=3, the background colors can be defined by addRowColor, addColColor
#'       and addCellColor functions.
#'       Default is NULL.
#'@param zebra.color A color name or a numeric value indicating pre-defined color.
#'       When parameter zebra is 0 or 1 or 2 and zebra.color is NULL, then zebra.color
#'       is set to "platinum". Numeric values between 1 to 13 is converted to
#'       predefined color names. The predefined color names are c("peach","peach-orange",
#'       "peachpuff","peach-yellow","pear","pearl","peridot","periwinkle","pastelred",
#'       "pastelgray"). Default is NULL.
#'@param zebra.colnames whether or not use background colors in column names row,
#'       Default value is FALSE
#'@param zebra.rownames whether or not use background colors in row names column,
#'       Default value is TRUE
#'@param zebra.type An integer of 0 or 1 or 2 or 3. A value of 1 sets background colors by row.
#'       A value of 2 sets background colors by column. A value of 0 sets background colors of all cells.
#'       A value of 3 sets background colors of cells specified with zebra.list.
#'       Default value is 1.
#'@param zebra.list A list consists of y,x,color. zebra.list is used only when zebra.type=3.
#'       zebra.list sets the cells specified with rows of vector "y" and columns of vector "x" with "color".
#'       The y and x are integer vector indicating rows and columns. NA value of y or x indicating all columns or rows.
#'       The color is an character vector consists of names of color.
#'@param colnames.bold whether or not use bold font for column names, Default value is FALSE
#'@param include.colnames Logical. If TRUE the column names is printed. Default value is TRUE.
#'@param cgroup A character vector or matrix indicating names of column group. Default value is NULL
#'@param n.cgroup A integer vector or matrix indicating the numbers of columns included in each cgroup
#'       Default value is NULL
#'@param rgroup A character vector indicating names of row group. Default value is NULL
#'@param n.rgroup A integer vector indicating the numbers of rows included in each rgroup
#'       Default value is NULL
#'@param cspan.rgroup The number of columns that an rgroup should span. It spans by default all
#'       columns but you may want to limit this if you have column colors that you want to retain.
#'@param pcol number of column displaying p value
#'@export
#'@examples
#' require(ztable)
#' x=head(iris)
#' ztable(x)
#' ztable(x,size=3,caption="Table 1. ztable Test")
#' ztable(x,size=7,caption="Table 1. ztable Test",caption.position="l")
#' ztable(x,size=7,caption="Table 1. ztable Test",caption.placement="bottom",
#'       caption.position="l")
#' fit=lm(mpg~.,data=mtcars)
#' ztable(fit)
#' data(USArrests)
#' pr1 <- prcomp(USArrests)
#' ztable(pr1)
#' ztable(summary(pr1))
#' require(survival)
#' data(colon)
#' attach(colon)
#' out <- glm(status ~ rx+obstruct+adhere+nodes+extent, data=colon, family=binomial)
#' ztable(out)
#' colon$TS = Surv(time,status==1)
#' out1=coxph(TS~rx+obstruct+adhere+differ+extent+surg+node4,data=colon)
#' ztable(out1)
#' ztable(head(mtcars),zebra=1)
#' ztable(head(mtcars),zebra=1,zebra.type=2)
ztable_sub=function(x,
                    family=NULL,
                    size=5, # normal size, range 1-10
                    color=getOption("ztable.color","black"),
                    tablewidth=0.3,
                    type=getOption("ztable.type","latex"),
                    include.rownames=getOption("ztable.include.rownames",TRUE),
                    placement="!hbtp",position="c",
                    show.heading=getOption("ztable.show.heading",TRUE),
                    show.footer=getOption("ztable.show.footer",TRUE),
                    caption=NULL,
                    caption.placement=getOption("ztable.caption.placement","top"),
                    caption.position=getOption("ztable.caption.position","c"),
                    caption.bold=getOption("ztable.caption.bold",FALSE),
                    align=NULL,digits=NULL,display=NULL,
                    sidewaystable=FALSE,
                    longtable=FALSE,
                    rotate=FALSE,
                    turn=FALSE,
                    angle=0,
                    wraptable=FALSE,wraptablewidth=12,
                    tabular=FALSE,
                    label=NULL,hline.after=NULL,
                    booktabs=getOption("ztable.booktabs",TRUE),
                    prefix.rows=NULL,commands=NULL,top.command=NULL,
                    zebra=getOption("ztable.zebra",NULL),
                    zebra.color=getOption("ztable.zebra.color",NULL),
                    zebra.type=getOption("ztable.zebra.type",1),
                    zebra.colnames=getOption("ztable.zebra.colnames",FALSE),
                    zebra.rownames=getOption("ztable.zebra.rownames",TRUE),
                    zebra.list=NULL,
                    colnames.bold=getOption("ztable.colnames.bold",FALSE),
                    include.colnames=getOption("ztable.include.colnames",TRUE),
                    cgroup=NULL,n.cgroup=NULL,
                    rgroup=NULL,n.rgroup=NULL,cspan.rgroup=NULL,
                    pcol=NULL){

    ncount=ncol(x)
    nrow=nrow(x)
    cn=colnames(x)
    if(identical(caption.placement,"bottom") | identical(caption.placement,"b"))
        caption.placement="bottom"
    else caption.placement="top"
    if(identical(caption.position,"left")|identical(caption.position,"l"))
        caption.position="l"
    else if(identical(caption.position,"right")|identical(caption.position,"r"))
        caption.position="r"
    else caption.position="c"

    if(identical(position,"left")|identical(position,"l"))
        position="flushleft"
    else if(identical(position,"right")|identical(position,"r"))
        position="flushright"
    else position="center"

    addrow=ifelse(include.rownames,1,0)
    logicals <- sapply(x, is.logical)

    x[logicals] <- lapply(x[logicals], as.character)

    characters <- sapply(x, is.character)
    factors <- sapply(x, is.factor)
    ints <- sapply(x, is.integer)

    if(is.null(align)){
        y <- c("r", c("r","l")[(characters | factors) + 1])
        if(include.rownames) for(i in 1:length(y)) align=paste(align,y[i],sep="")
        else for(i in 2:length(y)) align=paste(align,y[i],sep="")
    }
    if(is.null(digits)) digits=c(0,rep(2,ncol(x)))
    if(length(digits)==1) digits=rep(digits,ncount+1)
    if (is.null(display)) {
        display <- rep("f", ncol(x))
        display[ints] <- "d"
        display[characters | factors] <- "s"
        display <- c("s", display)
    }
    if(!is.null(zebra)) {
        if(zebra==0){
            prefix.rows=1:nrow(x)
            if(is.null(zebra.color)) zebra.color=1:10
        } else if(zebra==1) {
            prefix.rows=seq(2,nrow(x),by=2)
            if(is.null(zebra.color)) zebra.color=2 #peach-orange
        } else {
            zebra=2
            prefix.rows=seq(1,nrow(x),by=2)
            if(is.null(zebra.color)) zebra.color=2 #peach-orange
        }
        mycolor=c("peach","peach-orange","peachpuff","peach-yellow","pear",
                  "pearl","peridot","periwinkle","pastelred","pastelgray")
        if(zebra!=0) {
            zebra.color[1]=validColor(zebra.color[1],mycolor)
            zebra.color=rep(zebra.color[1],nrow)
        }
        else {     # zebra==0; all rows
            result=c()
            for(i in 1:length(zebra.color)){
                result=c(result,validColor(zebra.color[i],mycolor))
            }
            zebra.color=result
            if(length(zebra.color)<nrow)
                zebra.color=rep(zebra.color,1+(nrow/length(zebra.color)))
        }
    }
    cellcolor=make.cell.color(x,zebra,zebra.color,zebra.type,zebra.list,
                              zebra.colnames,zebra.rownames)
    frontcolor=make.frontcolor(x,color)
    if(!is.null(prefix.rows) & (length(commands)==1))
        commands=rep(commands,nrow)
    if((0 %in% prefix.rows) & is.null(top.command) &(length(commands)>0))
        top.command=commands[1]
    if(!is.numeric(size)) size=5
    else if(size<0 | size>10) size=5

    result=list(x=x,
                family=family,
                cellcolor=cellcolor,
                frontcolor=frontcolor,
                size=size,
                color=color,
                tablewidth=tablewidth,
                type=type,
                include.rownames=include.rownames,
                placement=placement,
                position=position,
                show.heading=show.heading,
                show.footer=show.footer,
                caption=caption,
                caption.placement=caption.placement,
                caption.position=caption.position,
                caption.bold=caption.bold,
                align=align,
                digits=digits,
                display=display,
                sidewaystable=sidewaystable,
                longtable=longtable,
                wraptable=wraptable,
                wraptablewidth=wraptablewidth,
                tabular=tabular,
                rotate=rotate,
                turn=turn,
                angle=angle,
                label=label,
                hline.after=hline.after,
                booktabs=booktabs,
                prefix.rows=prefix.rows,
                commands=commands,
                top.command=top.command,
                zebra=zebra,
                zebra.color=zebra.color,
                zebra.type=zebra.type,
                zebra.list=zebra.list,
                zebra.colnames=zebra.colnames,
                zebra.rownames=zebra.rownames,
                include.colnames=include.colnames,
                colnames.bold=colnames.bold,
                cgroup=cgroup,
                n.cgroup=n.cgroup,
                rgroup=rgroup,
                n.rgroup=n.rgroup,
                cspan.rgroup=cspan.rgroup,
                pcol=pcol
    )
    class(result) <-c("ztable")
    result
}

#' Make a data.frame named "cellcolor" from ztable call
#'
#'@param x a data.frame
#'@param zebra Null or an integer of 0 or 1 or 2. The arguments zebra and zebra.color are
#'       used to make a Zebra striping table(table with alternating background colors)
#'       easily. A value of 1 sets background color of all odd rows/columns with specified with
#'       zebra.color. A value of 2 sets all even rows/columns. A value of 0 sets
#'       background colors of all rows/columns with colors specified with zebra.color.
#'       When zebra is 1 or 2, the parameters of prefix.rows and commands ignored.
#'       Default is NULL.
#'@param zebra.color A color name or a numeric value indicating pre-defined color.
#'       When parameter zebra is 0 or 1 or 2 and zebra.color is NULL, then zebra.color
#'       is set to "platinum". Numeric values between 1 to 13 is converted to
#'       predefined color names. The predefined color names are c("peach","peach-orange",
#'       "peachpuff","peach-yellow","pear","pearl","peridot","periwinkle","pastelred",
#'       "pastelgray"). Default is NULL.
#'@param zebra.type An integer of 0 or 1 or 2 or 3. A value of 1 sets background colors by row.
#'       A value of 2 sets background colors by column. A value of 0 sets background colors of all cells.
#'       A value of 3 sets background colors of cells specified with zebra.list.
#'       Default value is 1.
#'@param zebra.list A list consists of y,x,color. zebra.list is used only when zebra.type=3.
#'       zebra.list sets the cells specified with rows of vector "y" and columns of vector "x" with "color".
#'       The y and x are integer vector indicating rows and columns. NA value of y or x indicating all columns or rows.
#'       The color is an character vector consists of names of color.
#'@param zebra.colnames whether or not use background colors in column names row,
#'       Default value is FALSE
#'@param zebra.rownames whether or not use background colors in row names column,
#'       Default value is TRUE
make.cell.color=function(x,zebra,zebra.color,zebra.type,zebra.list,
                         zebra.colnames,zebra.rownames){

    temp=rep("white",nrow(x)+1)
    cellcolor=c()
    for(i in 1:(ncol(x)+1)) cellcolor=cbind(cellcolor,temp)
    colnames(cellcolor)=c(" ",colnames(x))
    rownames(cellcolor)=c(" ",rownames(x))
    if(!is.null(zebra)){

        if(is.null(zebra.color)) {
            if(zebra==0) color=1:10
            else color=2
        }
        else color=zebra.color

        mycolor=c("peach","peach-orange","peachpuff","peach-yellow","pear",
                  "pearl","peridot","periwinkle","pastelred","pastelgray")
        result=c()
        for(i in 1:length(color)){
                result=c(result,validColor(color[i],mycolor))
        }
        color=result
        if(zebra==0){
            if(zebra.type==1){
               cellcolor=apply(cellcolor,2,repColor,color=color)
            }
            else if(zebra.type==2){
                cellcolor=apply(cellcolor,1,repColor,color=color)
                cellcolor=t(cellcolor)
            }
            else if(zebra.type==0){
                temp=rep(color,1+length(cellcolor)/length(color))
                for(i in 1:nrow(cellcolor))
                    for(j in 1:ncol(cellcolor))
                        cellcolor[i,j]=temp[(i-1)*ncol(cellcolor)+j]
            }
        }
        else if(zebra==1){
            if(zebra.type==1){
                select=seq(2,nrow(cellcolor),by=2)
                for(i in select)
                    for(j in 1:ncol(cellcolor)) cellcolor[i,j]=color[1]
            }
            else if(zebra.type==2){
                select=seq(1,ncol(cellcolor),by=2)
                for(i in 1:nrow(cellcolor))
                    for(j in select)
                    cellcolor[i,j]=color[1]
            }
        }
        else if(zebra==2){
            if(zebra.type==1){
                select=seq(1,nrow(cellcolor),by=2)
                for(i in select)
                    for(j in 1:ncol(cellcolor)) cellcolor[i,j]=color[1]
            }
            else if(zebra.type==2){
                select=seq(2,ncol(cellcolor),by=2)
                for(i in 1:nrow(cellcolor))
                    for(j in select)
                        cellcolor[i,j]=color[1]
            }
        }
    }
    if(zebra.colnames==FALSE) {
        cellcolor[1,1:ncol(cellcolor)]="white"
    }
    if(zebra.rownames==FALSE) {
        cellcolor[1:nrow(cellcolor),1]="white"
    }
    if(zebra.type==3){
        if(!is.null(zebra.list)){
           ylength=length(zebra.list$y)
           if(length(zebra.list$color)<ylength){
                zebra.list$color=rep(zebra.list$color,1+ylength/length(zebra.list$color))
            }
            while(length(zebra.list$x)<ylength){
                zebra.list$x=c(zebra.list$x,NA)
            }
            for(i in 1:ylength){
                if(is.na(zebra.list$y[i])) {
                    if(is.na(zebra.list$x[i])) next
                    for(j in 1:nrow(cellcolor)) cellcolor[j,zebra.list$x[i]]=zebra.list$color[i]
                }
                else if(is.na(zebra.list$x[i])){
                    for(j in 1:ncol(cellcolor)) cellcolor[zebra.list$y[i],j]=zebra.list$color[i]
                }
                else cellcolor[zebra.list$y[i],zebra.list$x[i]]=zebra.list$color[i]
            }
        }

    }
    cellcolor
}

#' Make a data.frame named "cellcolor" from ztable call
#'
#'@param x A data.frame
#'@param color A character string
make.frontcolor=function(x,color="black"){
    temp=rep(color,nrow(x)+1)
    frontcolor=c()
    for(i in 1:(ncol(x)+1)) frontcolor=cbind(frontcolor,temp)
    colnames(frontcolor)=c(" ",colnames(x))
    rownames(frontcolor)=c(" ",rownames(x))
    frontcolor
}

#' Make vector x from vector color
#'
#' Internal function of make.cell.color
#' @param x A destination vector
#' @param color A charactor vector consists of color names
repColor=function(x,color){
    #cat("color=",color,"\n")
    temp=rep(color,1+length(x)/length(color))
    for(i in 1:length(x)) x[i]=temp[i]
    x
}

#'Update ztable before print
#'
#'Update options of ztable before print
#'@param z An object of class "ztable"
#'@param family Font family. Default value is NULL. Possible value is one of the c("serif","times").
#'@param size An integer from 1 to 10 indicating font size= c("tiny","scriptsize",
#'       "footnotesize","small","normalsize","large","Large","LARGE","huge","Huge")
#'       respectively.
#'@param color A character indicating color of ztable
#'@param tablewidth A numeric indicating desired table width as a ratio to linewidth.
#'       Default value is 0.3.
#'@param type character indicating formats of ztable, either "html" or "latex".
#'@param include.rownames A logical value whether or not include rownames in the table
#'@param placement The table will have placement given by placement where placement
#'       must be NULL or contain only elements of {"h","t","b","p","!","H"}.
#'@param position The table will be have placed at the center of the paper
#'        if position is "center" or "c", and at the left side of the paper
#'        if it equals "left" or "l", and at the right side of the paper
#'        if it equals "right" or "r". The position is translated to specific
#'        latex environments such as "flushright" or "flushleft" or "center"
#'        (provided as a character vector) will enclose the tabular environment.
#'@param show.heading A logical value whether or not include headings in the table.
#'@param show.footer A logical value whether or not include headings in the table.
#'@param caption A character
#'@param caption.placement The caption will be have placed at the top of the table
#'        if caption.placement is "top" and at the bottom of the table
#'        if it equals "bottom".
#'@param caption.position The caption will be have placed at the center of the table
#'        if caption.position is "center" or "c", and at the left side of the table
#'        if it equals "left" or "l", and at the right side of the table
#'        if it equals "right" or "r".
#'@param caption.bold whether or not use bold font for caption
#'@param align Character vector : nchar equal to the number of columns of the
#'       resulting table indicating the alignment of the corresponding columns.
#'@param digits Numeric vector of length equal to one (in which case it will be
#'       replicated as necessary) or to the number of columns of the resulting table
#'@param display Character vector of length equal to the number of columns of the
#'       resulting table indicating the format for the corresponding columns.
#'       Since the row names are printed in the first column, the length of display
#'       is one greater than ncol(x) if x is a data.frame. These values are passed
#'       to the formatC function. Use "d" (for integers), "f", "e", "E", "g", "G",
#'       "fg" (for reals), or "s" (for strings). "f" gives numbers in the usual
#'       xxx.xxx format; "e" and "E" give n.ddde+nn or n.dddE+nn (scientific format);
#'       "g" and "G" put x[i] into scientific format only if it saves space to do so.
#'       "fg" uses fixed format as "f", but digits as number of significant digits.
#'       Note that this can lead to quite long result strings.
#'@param sidewaystable Logical value whether or not set the tabular environment=
#'       "sidewaystable". Requires Latex "rotating" package in preamble.
#'@param longtable Logical value whether or not set the tabular environment=
#'       "longtable". Requires Latex "longtable" package in preamble.
#'@param wraptable Logical value whether or not set the tabular environment=
#'       "wraptable". Requires Latex "wrapfig" package in preamble.
#'@param rotate Logical value whether or not set the tabular environment=
#'       "rotate". No special arrangement is made to find space for the result.
#'       Requires Latex "rotating" package in preamble.
#'       If TRUE, requires the rotate angle(counterclockwise).
#'@param turn Logical value whether or not set the tabular environment=
#'       "turn". In this environment, Latex leaves space for the rotated table.
#'       Requires Latex "rotating" package in preamble.
#'       If TRUE, requires the rotate angle.
#'@param angle An integer indicate the angle to rotate(degree); range -180 to 180.
#'@param wraptablewidth A integer indicate wraptable width in centimeter.
#'@param tabular Logical value whether or not set the tabular environment.
#'       If TRUE, no tabular environment is set.
#'@param label Character vector of length 1 containing the LaTeX label or HTML anchor.
#'       Set to NULL to suppress the label.
#'@param hline.after A vector of numbers between -1 and "nrow(x)", inclusive,
#'       indicating the rows after which a horizontal line should appear.
#'       If NULL is used no lines are produced. Default value is c(-1,0,nrow(x))
#'       which means draw a line before and after the columns names and at the
#'       end of the table. Repeated values are allowed.
#'@param booktabs Logical value. If TRUE, the toprule, midrule and bottomrule tags
#'       from the LaTex "booktabs" package are used rather than hline for the
#'       horizontal line tags. Requires Latex "booktabs" package in preamble.
#'@param prefix.rows A numeric vector contains the position of rows on which
#'       extra Latex commands should be added as a prefix.
#'@param commands A character vector of the length 1 or same length of the nrow of
#'       data.frame which contains the command that should be added as a prefix at
#'       the specified rows.
#'@param top.command A character vector of the length 1 which contains the command
#'       that should be added as a prefix at the colnames row.
#'@param zebra Null or a integer of 1 or 2. The arguments zebra and zebra.color are
#'       used to make a Zebra striping table(table with alternating background colors)
#'       easily. A value of 1 sets background color of all odd rows with specified with
#'       zebra.color. A value of 2 sets all even rows. when zebra is 1 or 2,
#'       the parameters of prefix.rows and commands ignored.
#'@param zebra.color A color name or a numeric value indicating pre-defined color.
#'       When parameter zebra is 0 or 1 or 2 and zebra.color is NULL, then zebra.color
#'       is set to "platinum". Numeric values between 1 to 13 is converted to
#'       predefined color names. The predefined color names are c("peach","peach-orange",
#'       "peachpuff","peach-yellow","pear","pearl","peridot","periwinkle","pastelred",
#'       "pastelgray").
#'@param zebra.type An integer of 0 or 1 or 2 or 3. A value of 1 sets background colors by row.
#'       A value of 2 sets background colors by column. A value of 0 sets background colors of all cells.
#'       A value of 3 sets background colors of cells specified with zebra.list.
#'       Default value is 1.
#'@param zebra.list A list consists of y,x,color. zebra.list is used only when zebra.type=3.
#'       zebra.list sets the cells specified with cells[y,x] with "color". The y and x are
#'       integer indicating rows and columns. NA value of y or x indicating all columns or rows.
#'@param zebra.colnames whether or not use background colors in column names row,
#'       Default value is FALSE
#'@param zebra.rownames whether or not use background colors in row names column,
#'       Default value is TRUE
#'@param colnames.bold whether or not use bold font for column names.
#'@param include.colnames Logical. If TRUE the column names is printed.
#'@param cgroup A character vector or matrix indicating names of column group. Default value is NULL
#'@param n.cgroup A integer vector or matrix indicating the numbers of columns included in each cgroup
#'       Default value is NULL
#'@param rgroup A character vector indicating names of row group. Default value is NULL
#'@param n.rgroup A integer vector indicating the numbers of rows included in each rgroup
#'       Default value is NULL
#'@param cspan.rgroup The number of columns that an rgroup should span. It spans by default all
#'       columns but you may want to limit this if you have column colors that you want to retain.
#'@param pcol number of column displaying p value
#'@export
update_ztable=function(z,
                       family=NULL,
                       size=NULL, # normal size, range 1-10
                       color=NULL,
                       tablewidth=NULL,
                       type=NULL,
                       include.rownames=NULL,
                       placement=NULL,position=NULL,
                       show.heading=NULL,
                       show.footer=NULL,
                       caption=NULL,
                       caption.placement=NULL,
                       caption.position=NULL,
                       caption.bold=NULL,
                       align=NULL,digits=NULL,display=NULL,
                       sidewaystable=NULL,
                       longtable=NULL,
                       rotate=NULL,
                       turn=NULL,
                       angle=NULL,
                       wraptable=NULL,wraptablewidth=NULL,
                       tabular=NULL,
                       label=NULL,hline.after=NULL,
                       booktabs=NULL,
                       prefix.rows=NULL,commands=NULL,top.command=NULL,
                       zebra=NULL,
                       zebra.color=NULL,
                       zebra.type=NULL,
                       zebra.list=NULL,
                       zebra.colnames=NULL,
                       zebra.rownames=NULL,
                       colnames.bold=NULL,
                       include.colnames=NULL,
                       cgroup=NULL,
                       n.cgroup=NULL,
                       rgroup=NULL,
                       n.rgroup=NULL,
                       cspan.rgroup=NULL,
                       pcol=NULL){

     if(!is.null(family)) z$family=family
     if(!is.null(size)) z$size=size
     if(!is.null(color)) z$color=color
     if(!is.null(tablewidth)) z$tablewidth=tablewidth
     if(!is.null(type)) z$type=type
     if(!is.null(include.rownames)) z$include.rownames=include.rownames
     if(!is.null(placement)) z$placement=placement
     if(!is.null(position)) z$position=position
     if(!is.null(show.heading)) z$show.heading=show.heading
     if(!is.null(show.footer)) z$show.footer=show.footer
     if(!is.null(caption)) z$caption=caption
     if(!is.null(caption.placement)) z$caption.placement=caption.placement
     if(!is.null(caption.position)) z$caption.position=caption.position
     if(!is.null(caption.bold)) z$caption.bold=caption.bold
     if(!is.null(align)) z$align=align
     if(!is.null(digits)) {
         if(is.null(digits)) digits=c(0,rep(2,ncol(z$x)))
         if(length(digits)==1) digits=rep(digits,ncol(z$x)+1)
         z$digits=digits
     }
     if(!is.null(display)) z$display=display
     if(!is.null(sidewaystable)) z$sidewaystable=sidewaystable
     if(!is.null(longtable)) z$longtable=longtable
     if(!is.null(rotate)) z$rotate=rotate
     if(!is.null(turn)) z$turn=turn
     if(!is.null(angle)) z$angle=angle
     if(!is.null(wraptable)) z$wraptable=wraptable
     if(!is.null(wraptablewidth)) z$wraptablewidth=wraptablewidth
     if(!is.null(tabular)) z$tabular=tabular
     if(!is.null(label)) z$label=label
     if(!is.null(hline.after)) z$hline.after=hline.after
     if(!is.null(booktabs)) z$booktabs=booktabs
     if(!is.null(prefix.rows)) z$prefix.rows=prefix.rows
     if(!is.null(commands)) z$commands=commands
     if(!is.null(top.command)) z$top.command=top.command
     if(!is.null(zebra)) z$zebra=zebra
     if(!is.null(zebra.color)) z$zebra.color=zebra.color
     if(!is.null(zebra.type)) z$zebra.type=zebra.type
     if(!is.null(zebra.list)) z$zebra.list=zebra.list
     if(!is.null(zebra.colnames)) z$zebra.colnames=zebra.colnames
     if(!is.null(zebra.rownames)) z$zebra.rownames=zebra.rownames
     if(!is.null(colnames.bold)) z$colnames.bold=colnames.bold
     if(!is.null(include.colnames)) z$include.colnames=include.colnames
     if(!is.null(cgroup)) z$cgroup=cgroup
     if(!is.null(n.cgroup)) z$n.cgroup=n.cgroup
     if(!is.null(rgroup)) z$rgroup=rgroup
     if(!is.null(n.rgroup)) z$n.rgroup=n.rgroup
     if(!is.null(cspan.rgroup)) z$cspan.rgroup=cspan.rgroup
     if(!is.null(pcol)) z$pcol=pcol
     if(!is.null(z$zebra)) { if(z$zebra!=3) z$cellcolor=make.cell.color(x=z$x,zebra=z$zebra,zebra.color=z$zebra.color,
                                 zebra.type=z$zebra.type,
                                 zebra.list=z$zebra.list,
                                 zebra.colnames=z$zebra.colnames,
                                 zebra.rownames=z$zebra.rownames)
     }
     z
}

#' Print an object of class "ztable"
#'
#' @param x An object of class "ztable"
#' @param ... further argument passed to other function
#' @export
print.ztable=function(x,...){
    z=update_ztable(z=x,...)
    print_ztable(z)
}


#' Print an object of class "ztable"
#'
#' @param z An object of class "ztable"
print_ztable=function(z){
    xdata=data2table(z)
    if(z$type=="latex") ztable2latex(z,xdata)
    else if(z$type=="viewer") ztable2viewer(z)
    else ztable2html(z,xdata)
}

#'Subfunction used in ztable2latex
#'
#' @param string a character vector
tr=function(string) {
    string=gsub("%","\\%",string,fixed=TRUE)
    string=gsub(" -","\\hspace{0.5cm}",string,fixed=TRUE)
    string
}


#'Subfunction used in ztable2html
#'
#' @param string a character vector
tr2=function(string) {
    string=gsub(" -","&nbsp;&nbsp;&nbsp;",string,fixed=TRUE)
    string=gsub("     ","",string,fixed=TRUE)
    string
}


#' Convert data to formatted data for table
#'
#' @param z An object of class "ztable"
tableLength=function(z){
    xdata=data2table(z)
    a=apply(xdata,2,function(x) max(nchar(x)))
    if(z$include.colnames){
        b=nchar(colnames(xdata))
        l=c()
        for(i in 1:ncol(xdata)){
            l=c(l,max(a[i],b[i]))
        }
        length=sum(l)
    }
    else length=sum(a)
    result=length+ncol(xdata)-1
    if(z$include.rownames) result=result+max(nchar(rownames(xdata)))+1
    result
}

#' Convert data to formatted data for table
#'
#' @param z An object of class "ztable"
#' @export
data2table=function(z){
    data<-z$x
    ncount=ncol(data)
    nrow=nrow(data)

    select=sapply(data,is.factor)
    data[select]=lapply(data[select],as.character)
    #data
    for(i in 1:nrow){
        for(j in 1:ncount) {
            if(z$display[j+1]=="s"){
                temp=data[i,j]
                if(z$type=="latex") temp<-tr(temp)
                if(z$type=="html") temp<-tr2(temp)
            }
            else{
                if(is.na(z$x[i,j])) {
                    temp<-""
                } else{
                    temp<-formatC(z$x[i,j],digits=z$digits[j+1],
                                 format=z$display[j+1])
                }

            }
            data[i,j]<-temp
        }
    }
    pcol=z$pcol
    pcol
    if(!is.null(pcol)) {
        temp=data[[pcol]]
        temp
        pos=which((as.numeric(temp)==0) & (temp!=""))
        if(length(pos)>0){
            tempvalue=temp[pos][1]
            temp[pos]<- paste0("< ",substr(tempvalue,1,nchar(tempvalue)-1),"1")
        }
        data[[pcol]]=temp
    }
    data
}

#' Convert long caption to minipage
#'
#' @param z An object of ztable
#' @param caption A character vector to convert
caption2minipage=function(z,caption){
    tlength=tableLength(z)
    if(nchar(caption)>tlength){
        tablewidth=max(z$tablewidth,tlength/85)
        mycaption=paste("\\begin{minipage}[c]{",tablewidth,"\\linewidth}",
                    caption,"\\end{minipage}",sep="")
    }
    else mycaption=caption
    mycaption
}

#' Print an object of class "ztable" to Latex table
#'
#' @param z An object of class "ztable"
#' @param xdata A formatted data.frame
ztable2latex=function(z,xdata){
    ncount=ncol(z$x)
    nrow=nrow(z$x)
    cn=colnames(z$x)
    addrow=ifelse(z$include.rownames,1,0)

    NewAlign=getNewAlign(z)
    #NewAlign=z$align
    totalCol=totalCol(z)
    colCount=colGroupCount(z)

    vlines=align2lines(z$align)

    rgroupcount=0
    printrgroup=1
    if(!is.null(z$n.rgroup)){
        if(length(z$n.rgroup)>1) {
           for(i in 2:length(z$n.rgroup)) {
               printrgroup=c(printrgroup,printrgroup[length(printrgroup)]+z$n.rgroup[i-1])
           }
        }
        rgroupcount=1
    }
    Fontsize=c("tiny","scriptsize","footnotesize","small","normalsize",
           "large","Large","LARGE","huge","Huge")


    if(z$tabular) sort="tabular"
    else if(z$sidewaystable) sort="sidewaystable"
    else if(z$wraptable) sort="wraptable"
    else if(z$rotate) sort="rotate"
    else if(z$turn) sort="turn"
    else sort="table"
    headingsize=ifelse(z$size>3,z$size-2,1)
    z$cellcolor=define_colors(z$cellcolor)
    start=attr(z$cellcolor,"no")
    z$frontcolor=define_colors(z$frontcolor,no=start)
    start=attr(z$frontcolor,"no")
    if(!is.null(z$cgroupcolor)) {
        for(i in 1:length(z$cgroupcolor)){
            z$cgroupcolor[[i]]=define_colors(z$cgroupcolor[[i]],no=start)
            start=attr(z$cgroupcolor[[i]],"no")
        }
    }
    if(!is.null(z$cgroupbg)) {
        for(i in 1:length(z$cgroupbg)){
            z$cgroupbg[[i]]=define_colors(z$cgroupbg[[i]],no=start)
            start=attr(z$cgroupbg[[i]],"no")
        }
    }

    if(!is.null(z$rgroupcolor)) z$rgroupcolor=define_colors(z$rgroupcolor,no=start)
    start=attr(z$rgroupcolor,"no")
    if(!is.null(z$rgroupbg)) z$rgroupbg=define_colors(z$rgroupbg,no=start)
    start=attr(z$rgroupbg,"no")

    align=alignCheck(z$align,ncount,addrow)
    if(z$longtable){
        cat(paste("\\color{",z$color,"}\n",sep=""))
        cat(paste("\\begin{",Fontsize[z$size],"}\n",sep=""))
        cat(paste("\\begin{longtable}{",NewAlign,"}\n",sep=""))

    } else {
        if(z$wraptable) {
            if(z$position=="flushright") wrapposition<-"r"
            else wrapposition<-"l"
            cat(paste("\\begin{wraptable}{",wrapposition,"}[10pt]{",
                      z$wraptablewidth,"cm}\n",sep=""))

        } else if((sort=="rotate") | (sort=="turn")){
            cat(paste("\\begin{",sort,"}{",z$angle,"}\n",sep=""))
        } else if(sort!="tabular"){      # sidewaystable or table
            cat(paste("\\begin{",sort,"}[",z$placement,"]\n",sep=""))
            cat(paste("\\begin{",z$position,"}\n",sep=""))
        }
        if(!is.null(z$family)){
            if(z$family=="serif") cat("\\sffamily\n")
            else if(z$family=="times") cat("\\rmfamily\n")
            else if(z$family=="tt") cat("\\ttfamily\n")
            else {
                temp=paste0("\\",z$family,"\n")
                cat(temp)
            }
        }
        cat(paste("\\begin{",Fontsize[z$size],"}\n",sep=""))
        cat(paste("\\color{",z$color,"}\n",sep=""))
        cat(paste("\\begin{tabular}{",NewAlign,"}\n",sep=""))
    }

    if(!is.null(z$caption) & z$caption.placement=="top"){
        mycaption=caption2minipage(z,z$caption)
        cat(paste("\\multicolumn{",totalCol,"}{",
                  z$caption.position,"}{",sep=""))
        if(z$caption.bold) cat(paste("\\textbf{",mycaption,"}",sep=""))
        else cat(mycaption)
        cat("}\\\\ \n")
    }
    if((z$show.heading==TRUE) & (!is.null(attr(z$x,"heading")))) {
        head=attr(z$x,"heading")
        for(i in 1:length(head)) {
            h1=gsub("~","$\\sim$",head[i],fixed=TRUE)
            if(nchar(head[i])<1) next
            cat(paste("\\multicolumn{",totalCol,"}{l}{\\",Fontsize[headingsize],
                      "{",h1,"}}\\\\ \n",sep=""))
        }
    }
    if(is.null(z$hline.after)) cat(ifelse(z$booktabs,"\\toprule[1.2pt]\n","\\hline\n"))
    else if(-1 %in% z$hline.after) cat(ifelse(z$booktabs,"\\toprule[1.2pt]\n","\\hline\n"))
    if(!is.null(z$cgroup)) printLatexHead(z)
    subcolnames=ifelse(is.null(z$subcolnames),0,1)

        if(subcolnames) {
            if(is.na(z$subcolnames[1])) firstcn=paste("\\multirow{2}{*}{}",sep="")
            else firstcn=cn[1]
        }
        else firstcn=cn[1]
        if(z$colnames.bold) firstcn=paste("\\textbf{",firstcn,"}",sep="")

        if(z$frontcolor[1,2]!=z$color) firstcn=paste("\\color{",z$frontcolor[1,2],"}",firstcn,sep="")
        if(z$cellcolor[1,2]!="white") firstcn=paste("\\cellcolor{",z$cellcolor[1,2],"}",firstcn,sep="")
        if(z$include.rownames) {

            result=1
            if(!is.null(isspanCol(z,1,1)))
                first=paste("\\multicolumn{",isspanCol(z,1,1),"}{c}{}",sep="")
            else if(!is.null(isspanRow(z,1,1))){
                 result=isspanRow(z,1,1)
                 if(result>0) first=paste("\\multirow{",result,"}{*}{}",sep="")
            } else first=""
            if(z$cellcolor[1,1]!="white")
                first=paste("\\cellcolor{",z$cellcolor[1,1],first,"}",sep="")

            firstrow=paste(first,"&",firstcn,sep="")

        }
        else firstrow=firstcn

        if(ncount>1) {
            for(i in 2:ncount) {
                firstrow=paste(firstrow,"&",sep="")
                if((i==2)&(!is.null(colCount))){
                    if(1 %in% colCount[-length(colCount)]) {
                        if(vlines[1+2]==0) firstrow=paste(firstrow,"&",sep="")
                    }
                }
                if(z$cellcolor[1,i+1]!="white")
                    firstrow=paste(firstrow,"\\cellcolor{",z$cellcolor[1,i+1],"}",sep="")
                if(z$frontcolor[1,i+1]!=z$color)
                    firstrow=paste(firstrow,"\\color{",z$frontcolor[1,i+1],"}",sep="")
                if(z$colnames.bold) boldcn=paste("\\textbf{",cn[i],"}",sep="")
                else boldcn=cn[i]
                result=1
                if(!is.null(isspanCol(z,1,(i+1)))){
                    result=isspanCol(z,1,(i+1))
                    if(result>0) boldcn=paste("\\multicolumn{",result,"}{c}{",boldcn,"}",sep="")
                    else if(result==0) next
                } else if(!is.null(isspanRow(z,1,(i+1)))){
                    boldcn=paste("\\multirow{",isspanRow(z,1,(i+1)),"}{*}{",boldcn,"}",sep="")
                }
                if((subcolnames==1)) {
                    if(is.na(z$subcolnames[i])){
                       # boldcn=paste("\\multirow{2}{*}{",boldcn,"}",sep="")
                        boldcn=""
                    }
                }
                firstrow=paste(firstrow,boldcn,sep="")
                if(!is.null(colCount)){
                    if(i %in% colCount[-length(colCount)]) {
                        if(vlines[i+2]==0) {
                            #if(z$cellcolor[1,i+1]!="white")
                            #    firstrow=paste(firstrow,"&\\cellcolor{",z$cellcolor[1,i+1],"}",sep="")
                            #else firstrow=paste(firstrow,"&",sep="")
                            firstrow=paste(firstrow,"&",sep="")
                        }
                    }
                }
            }
        }

    if((0 %in% z$prefix.rows) & !is.null(z$top.command)) cat(z$top.command)
    if(z$include.colnames) {
        cat(paste(firstrow,"\\\\ \n",sep=""))
        if(subcolnames){
            if(z$include.rownames) {
                if(z$cellcolor[1,1]!="white")
                    cat(paste("\\cellcolor{",z$cellcolor[1,1],"} &",sep=""))
                else cat("&")

            }
            for(i in 1:length(z$subcolnames)){
                if(is.na(z$subcolnames[i])) {
                    temp=paste("\\multirow{-2}{*}{",colnames(z$x)[i],"}",sep="")
                    if(!is.null(z$colcolor)){
                        if(z$frontcolor[1,i+1]!=z$color)
                            temp=paste("\\color{",z$frontcolor[1,i+1],"}",temp,sep="")
                        if(z$cellcolor[1,i+1]!="white")
                            temp=paste("\\cellcolor{",z$cellcolor[1,i+1],"}",temp,sep="")
                    }
                    cat(temp)
                    if(i!=length(z$subcolnames)) cat("&")
                    if(i %in% colCount[-length(colCount)]) {
                        if(vlines[i+2]==0){
                            if((z$cellcolor[1,i+1]!="white") & (z$cellcolor[1,i+1]==z$cellcolor[1,i+2]))
                                cat(paste("\\cellcolor{",z$cellcolor[1,i+1],"}&",sep=""))
                            else cat("&")
                        }
                    }
                    next
                }
                if(z$colnames.bold) boldcn=paste("\\textbf{",z$subcolnames[i],"}",sep="")
                else boldcn=z$subcolnames[i]

                if(z$cellcolor[1,i+1]!="white")
                    cat(paste("\\cellcolor{",z$cellcolor[1,i+1],"}",boldcn,"&",sep=""))
                else cat(paste(boldcn,"&",sep=""))
                if(i %in% colCount[-length(colCount)]) {
                    if(vlines[i+2]==0){
                    if((z$cellcolor[1,i+1]!="white") & (z$cellcolor[1,i+1]==z$cellcolor[1,i+2]))
                        cat(paste("\\cellcolor{",z$cellcolor[1,i+1],"}&",sep=""))
                    else cat("&")
                    }
                }
            }
            cat("\\\\ \n")
        }
        if(is.null(z$hline.after)) cat(ifelse(z$booktabs,"\\midrule\n","\\hline\n"))
        else if(0 %in% z$hline.after) cat(ifelse(z$booktabs,"\\midrule\n","\\hline\n"))
    }

    for(i in 1:nrow){
        printcline=0
        if(rgroupcount>0) {
            if(i %in% printrgroup) {
                for(k in 1:length(printrgroup)){
                    if(i == printrgroup[k]){
                        if(is.na(z$rgroup[k])) break
                        if(z$rgroup[k]=="") break
                        printRowGroup(z,i)
                        break
                    }
                }

            }
        }
        if(i %in% z$prefix.rows) {
            #if(is.numeric(z$zebra))
            #   cat(paste("\\rowcolor{",z$zebra.color[i],"}",sep=""))
            if(!is.null(z$commands[i])) cat(z$commands[i])
        }
        tempo=NULL
        if(z$include.rownames) {
            tempo=rownames(z$x)[i]
            if(z$frontcolor[i+1,1]!=z$color) {
                tempo=paste("\\color{",z$frontcolor[i+1,1],"}",
                            tempo,sep="")
            }
            if(z$cellcolor[i+1,1]!="white") {
                tempo=paste("\\cellcolor{",z$cellcolor[i+1,1],"}",
                            tempo,sep="")
            }
            if(!is.null(isspanCol(z,(i+1),1)))
                tempo=paste("\\multicolumn{",isspanCol(z,i+1,1),"}{c}{",tempo,"}",sep="")
            else if(!is.null(isspanRow(z,(i+1),1))){
                result=isspanRow(z,(i+1),1)
                if(result<0) tempo=paste("\\multirow{",result,"}{*}{",tempo,"}",sep="")
            }
            cat(tempo)
        }

        for(j in 1:ncount) {
            skip=0
            if(z$frontcolor[i+1,j+1]==z$color) temp1=xdata[i,j]
            else temp1=paste("\\color{",z$frontcolor[i+1,j+1],"}",
                             xdata[i,j],sep="")
            if(z$cellcolor[i+1,j+1]!="white") {
                temp1=paste("\\cellcolor{",z$cellcolor[i+1,j+1],"}",
                             temp1,sep="")
            }
            if(is.null(isspanCol(z,(i+1),(j+1)))){
                if(is.null(isspanRow(z,(i+1),(j+1)))){
                    result=1

                } else {
                    result=isspanRow(z,(i+1),(j+1))
                    if(result < 0) {
                        k=getspanRowData(z,i+1,j+1)
                        if(z$cellcolor[i+1,j+1]=="white") temp2=xdata[k+1,j]
                        else temp2=paste("\\cellcolor{",z$cellcolor[i+1,j+1],"}",
                                         xdata[k-1,j],sep="")
                        temp1=paste("\\multirow{",result,"}{*}{",temp2,"}",sep="")
                    }
                    else {
                        skip=1
                        result=0 #
                        if(z$cellcolor[i+1,j+1]=="white") skipcolor=""
                        else skipcolor=paste("\\cellcolor{",z$cellcolor[i+1,j+1],"}",sep="")
                    }

                }

                if(j %in% colCount[-length(colCount)]) {
                    if(vlines[j+2]==0) {
                        backcolor=NULL
                        if(!is.null(z$rowcolor)){
                            if(z$rowcolor[i+1]!="white") backcolor=z$rowcolor[i+1]
                        }
                        if(is.null(backcolor)){
                            if((z$cellcolor[i+1,j+1]!="white")&(z$cellcolor[i+1,j+1]==z$cellcolor[i+1,j+2]))
                               backcolor=z$cellcolor[i+1,j+1]
                        }
                        if(is.null(backcolor)) temp1=paste(temp1,"&",sep="")
                        else temp1=paste(temp1,"&\\cellcolor{",backcolor,"}",sep="")
                        #temp1=paste(temp1,"&",sep="")
                    }
                }

            } else {
                result=isspanCol(z,(i+1),(j+1))
                if(result>0) {
                    width=spanColWidth(z,(i+1),(j+1))
                    mcalign="c"
                    mclinecount=vlines[j+width+1]
                    if(mclinecount > 0) {
                        for(k in 1:mclinecount)
                            mcalign=paste(mcalign,"|",sep="")
                    }
                    temp1=paste("\\multicolumn{",result,"}{",mcalign,"}{",temp1,"}",sep="")
                    if(isGroupCol(j,result,colCount))
                        if(vlines[j+width+1]==0)
                            #if((j+result)<ncol(z$x))
                                temp1=paste(temp1,"&",sep="")
                    #if((j+result-1) %in% colCount[-length(colCount)])
                    #    if(vlines[j+result+1]==0) temp1=paste(temp1,"&",sep="")
                }
                else next
            }
            #browser()
            if(is.null(tempo)) {
                cat(temp1)
                tempo=temp1
            }
            else {
                if(result!=0) cat(paste("&",temp1,sep=""))
                else if(skip) cat(paste("&",skipcolor,sep=""))
            }

            if(!is.null(colCount)){
                count=j
                if(!is.null(isspanCol(z,i+1,j+1))){
                    result=isspanCol(z,(i+1),(j+1))
                    if(result>0) count=count+result
                }
                #if(count %in% colCount[-length(colCount)]) {
                    #if(vlines[count+2]==0) cat("&")
                    #if(z$cellcolor[i+1,j+1]=="white") cat("&")
                    #else cat(paste("&\\cellcolor{",z$cellcolor[i+1,j+1],"}",sep=""))
                #}
            }
        }
        cat(paste("\\\\ \n",sep=""))
        if(i %in% z$hline.after)
            cat(ifelse(z$booktabs,ifelse(i==nrow,"\\bottomrule[1.2pt]\n","\\midrule"),"\\hline\n"))
    }
    if(is.null(z$hline.after)) cat(ifelse(z$booktabs,"\\bottomrule[1.2pt]\n","\\hline\n"))

    footer=attr(z$x,"footer")
    if(!is.null(footer) & (z$show.footer)){
        myfooter=caption2minipage(z,footer)
        myfooter=gsub("~","$\\sim$",myfooter,fixed=TRUE)
        cat(paste("\\multicolumn{",totalCol,"}{l}{\\",Fontsize[headingsize],
                  "{",myfooter,"}}\\\\ \n",sep=""))
    }
    if(!is.null(z$caption) & z$caption.placement=="bottom"){
        mycaption=caption2minipage(z,z$caption)
        if(z$caption.bold) cat(paste("\\multicolumn{",totalCol,"}{",
                                     z$caption.position,"}{\\textbf{",mycaption,"}}\\\\ \n",sep=""))
        else cat(paste("\\multicolumn{",totalCol,"}{",
                       z$caption.position,"}{",mycaption,"}\\\\ \n",sep=""))
    }

    if(z$longtable) {
        if(!is.null(z$label)) cat(paste("\\label{",z$label,"}\n",sep=""))
        cat("\\end{longtable}\n")
        cat(paste("\\end{",Fontsize[z$size],"}\n",sep=""))
    } else {
        cat("\\end{tabular}\n")
        cat(paste("\\end{",Fontsize[z$size],"}\n",sep=""))
        if(!is.null(z$label)) cat(paste("\\label{",z$label,"}\n",sep=""))
        if(sort!="tabular") {
            if((sort=="table") | (sort=="sidewaystable"))
               cat(paste("\\end{",z$position,"}\n",sep=""))
            cat(paste("\\end{",sort,"}\n",sep=""))
        }
    }
    cat("\\color{black}\n")
}


#' Print Row Groups in a latex table
#'
#' @param z An object of class ztable
#' @param i An integer indicating row
printRowGroup=function(z,i){

    ncount=ncol(z$x)
    nrow=nrow(z$x)
    cn=colnames(z$x)
    addrow=ifelse(z$include.rownames,1,0)

    NewAlign=getNewAlign(z)
    totalCol=totalCol(z)
    colCount=colGroupCount(z)

    vlines=align2lines(z$align)

    printrgroup=1
    if(!is.null(z$n.rgroup)){
        if(length(z$n.rgroup)>1) {
            for(j in 2:length(z$n.rgroup)) {
                printrgroup=c(printrgroup,printrgroup[length(printrgroup)]+z$n.rgroup[j-1])
            }
        }
    }
    printrgroup
    printcline=0
    rgroupcount=0

    for(k in 1:length(printrgroup)){
        if(i == printrgroup[k]){
            rgroupcount=k
            break
        }
    }

if(i %in% printrgroup) {
    if(is.null(z$cspan.rgroup)){
        if(i>1) cat(paste("\\cline{1-",totalCol,"}\n",sep=""))
        vlines=align2lines(NewAlign)
        #mcalign=substr(extractAlign(NewAlign),start=1,stop=1)
        mcalign="l"
        if(vlines[1]>0)
            for(k in 1:vlines[1]) mcalign=paste("|",mcalign,sep="")
        if(vlines[totalCol+1]>0)
            for(k in 1:vlines[totalCol+1]) mcalign=paste(mcalign,"|",sep="")
        temp=paste("\\multicolumn{",totalCol,"}{",mcalign,"}{",sep="")
        # if(z$colcolor[1]!="white")
        #     temp=paste(temp,"\\cellcolor{",z$colcolor[1],"}",sep="")
        if(z$rgroupbg[rgroupcount]!="white")
            temp=paste(temp,"\\cellcolor{",z$rgroupbg[rgroupcount],"}",sep="")
        if(z$rgroupcolor[rgroupcount]!="black")
            temp=paste(temp,"\\color{",z$rgroupcolor[rgroupcount],"}",sep="")
        temp=paste(temp,"\\textbf{",z$rgroup[rgroupcount],"}}",sep="")
        printcline=totalCol
    }
    else {
        if(z$cspan.rgroup==1) {
            # if(z$colcolor[1]!="white")
            #     temp=paste("\\cellcolor{",z$colcolor[1],"}",sep="")
            # else temp=""
            temp=""
            if(z$rgroupbg[rgroupcount]!="white")
                temp=paste(temp,"\\cellcolor{",z$rgroupbg[rgroupcount],"}",sep="")
            if(z$rgroupcolor[rgroupcount]!="black")
                temp=paste(temp,"\\color{",z$rgroupcolor[rgroupcount],"}",sep="")
            temp=paste(temp,"\\textbf{",z$rgroup[rgroupcount],"}",sep="")
            for(j in 1:(ncount+addrow-1)){
                temp1=""
                if(z$colcolor[j+1]!="white")
                    temp1=paste("\\cellcolor{",z$colcolor[j+1],"}",sep="")
                else {
                    if(!is.null(isspanRow(z,i+1,j+1))){
                        #cat("i=",i,",j=",j,"isspanRow(z,i,j+1)=",isspanRow(z,i+1,j+1),"\n")
                        if(isspanRow(z,i+1,j+1)<=0) {
                            #for(k in 1:nrow(z$spanRow)) {
                            #    if(z$spanRow[k,1]!=j+1) next
                            #    if(z$spanRow[k,2]>=i+1) next
                            #    if(z$spanRow[k,3]==i+1) break
                            #}
                            temp1=paste("\\cellcolor{",z$cellcolor[i+1,j+1],"}",sep="")
                        }
                    }
                    else temp1=""
                }
                temp=paste(temp,temp1,sep="&")
                if(!is.null(colCount)){
                    if(j %in% colCount[-length(colCount)]) {
                        if(vlines[j+2]==0) {
                            #if(z$colcolor[j+1]!="white")
                            #    temp=paste(temp,"&\\cellcolor{",z$colcolor[j+1],"}",sep="")
                            #else temp=paste(temp,"&",sep="")
                            temp=paste(temp,"&",sep="")
                        }
                    }
                }
            }
        } else {
            if(z$cspan.rgroup<1 | z$cspan.rgroup>(ncount+addrow))
                z$cspan.rgroup=ncount+addrow
            printcline=z$cspan.rgroup
            nvlines=align2lines(NewAlign)
            #mcalign=substr(extractAlign(NewAlign),start=1,stop=1)
            mcalign="l"
            if(nvlines[1]>0)
                for(k in 1:vlines[1]) mcalign=paste("|",mcalign,sep="")
            if(nvlines[printcline+1]>0)
                for(k in 1:nvlines[printcline+1]) mcalign=paste(mcalign,"|",sep="")
            temp=paste("\\multicolumn{",z$cspan.rgroup,"}{",mcalign,"}{\\textbf{",
                       "\\cellcolor{",z$rgroupbg[rgroupcount],"}",
                       "\\color{",z$rgroupcolor[rgroupcount],"}",
                       z$rgroup[rgroupcount],"}}",sep="")
            #temp=paste("\\cellcolor{",z$colcolor[1],"}",temp,sep="")
            if(z$cspan.rgroup<(ncount+addrow)) {
                for(j in (z$cspan.rgroup):(ncount+addrow-1)) {
                    if(z$colcolor[j+1]!="white")
                        temp=paste(temp,"&\\cellcolor{",z$colcolor[j+1],"}",sep="")
                    else {
                        if(!is.null(isspanRow(z,i+1,j+1))){
                            if(isspanRow(z,i+1,j+1)<=0) {
                                #for(k in 1:nrow(z$spanRow)) {
                                #    if(z$spanRow[k,1]!=j+1) next
                                #    if(z$spanRow[k,2]>=i+1) next
                                #    if(z$spanRow[k,3]==i+1) break
                                #}
                                temp=paste(temp,"&\\cellcolor{",z$cellcolor[i+1,j+1],"}",sep="")
                            }
                            else temp=paste(temp,"&",sep="")
                        }
                        else temp=paste(temp,"&",sep="")
                    }
                    if(!is.null(colCount)){
                        if(j %in% colCount[-length(colCount)]) {
                            if(vlines[j+2]==0){
                                #if(z$colcolor[z$cspan.rgroup+j]!="white")
                                #    temp=paste(temp,"&\\cellcolor{",z$colcolor[z$cspan.rgroup+j],"}",sep="")
                                #else temp=paste(temp,"&",sep="")
                                temp=paste(temp,"&",sep="")

                            }
                        }
                    }
                }
            }
        }
    }

    cat(paste(temp,"\\\\ \n",sep=""))
    if(printcline>0) cat(paste("\\cline{1-",printcline,"}\n",sep=""))
    rgroupcount=rgroupcount+1
}
}


#' Find valid color name
#'
#' @param a An integer or a character
#' @param mycolor predefined color names
#' @return a valid Latex color name
#' @export
validColor=function(a,mycolor){
    if(is.numeric(a)) {
        if(a>0 && a <11)
            a=mycolor[a]
        else a="peach"
    } else {
        a=validColor2(a)
    }
    a
}

#' Find valid color name
#'
#' @param a An integer or a character
#' @return a valid Latex color name
#' @importFrom grDevices colors
#' @export
validColor2=function(a){

    if(!is.character(a)) a="peach"
    else if(substr(a,1,1)=="#"){
        a=a
    } else {
        if(tolower(a) %in% colors()){
            a=tolower(a)
        } else {
            result=grep(paste("^",a,sep=""),ztable::zcolors$name,ignore.case=TRUE)
            if(length(result)>0) {
                a=ztable::zcolors$name[result][which.min(nchar(ztable::zcolors$name[result]))]
            } else a="peach"
        }

    }
    a
}

#' Define colors
#'
#' Define colors of mycolors
#' @param mycolors characters vectors of color names
#' @param no An integer indicating start number
#' @export
define_colors=function(mycolors,no=1) {
    if(is.null(mycolors)) return
    uniquecolors=unique(as.vector(unique(mycolors)))
    uniquecolors
    count=no
    for(i in 1:length(uniquecolors)) {
        if(uniquecolors[i]=="white") next
        if(substr(uniquecolors[i],1,1)=="#") {
                definition=hex2rgbDef(uniquecolors[i],no=count)
                cat(definition,"\n")
                mycolors[mycolors==uniquecolors[i]]=paste0("tempcolor",count)
                count=count+1
        } else{
        number=grep(paste("^",uniquecolors[i],sep=""),ztable::zcolors$name)
        if(length(number)<1) next
        else{
            definition=ztable::zcolors[number[1],3]
            cat(definition,"\n")
        }
        }
    }
    attr(mycolors,"no")=count
    mycolors
}


hex2rgbDef=function(hex="#C90000",no=1){
    r=hex2decimal(substr(hex,2,3))
    g=hex2decimal(substr(hex,4,5))
    b=hex2decimal(substr(hex,6,7))
    paste0("\\definecolor{tempcolor",no,"}{rgb}{",r,",",g,",",b,"}")
}

hex2decimal=function(hex="C9"){
    temp=paste0("0x",hex)
    round(strtoi(temp)/256,2)
}


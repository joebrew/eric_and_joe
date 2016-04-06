################################################################################
################################################################################
# Load data
ff <- read.csv("ff-master.csv",stringsAsFactors=FALSE)

bsm <- read.csv("bsms-wiw.csv",stringsAsFactors=FALSE)
bsm$file <- NULL
bsm$leg <- substr(bsm$EID,9,9)
bsm$yr <- substr(bsm$EID,1,4)
################################################################################
################################################################################
# Compile and format
sp <- bhv <- conf <- rep(NA,times=nrow(bsm))
for(i in 1:nrow(bsm)){
  EIDi <- bsm$EID[i]
  EIDff <- which(ff$EID==EIDi)[1]
  if(length(EIDff)>0){
    sp[i] <- as.character(ff$sp[EIDff])
    bhv[i] <- as.character(ff$bhv[EIDff])
    conf[i] <- as.character(ff$conf[EIDff])
  }
}
bsm$Species <- sp
bsm$bhv <- bhv
bsm$conf <- conf
bsm$feed <- paste0(substr(bsm$bhv,1,2),conf)
bsm$feed[bsm$feed%in%c("FE66","FE95")] <- "FEED"
bsm$feed[!bsm$feed%in%c("FEED")] <- "OTHER"
bsm$feed <- as.factor(bsm$feed)
bsm <- bsm[!is.na(bsm$Species) & !is.na(bsm$bhv),]
################################################################################
################################################################################
# Basic violin plot function
fvh.violin <- function(bsm,varname,ytit,ylog=TRUE){
  library(ggplot2)
  bsm$VAR <- bsm[,which(names(bsm)==varname)]
  if(ylog){bsm$VAR <- log(bsm$VAR)}
  
  dodge <- position_dodge(width=0.5)
  
  ggplot(data = bsm, aes(x = "ALL", y = VAR, fill = Species)) +
    geom_violin(trim=FALSE,position = dodge) +
    geom_boxplot(width=.2,position=dodge, show.legend = FALSE) +
    geom_violin(data=bsm,aes(x=feed,y=VAR,fill=Species),trim=FALSE,
                position = position_dodge(width = 0.4)) +
    geom_boxplot(aes(x=feed,y=VAR,fill=Species), width=.2,position=dodge,
                 show.legend = FALSE) +
    scale_fill_manual(values=adjustcolor(c("black","white"),alpha=.5)) +
    labs(x=" ", y = ytit) +
    theme_classic() +
    theme(axis.title.y=element_text(size=20,vjust=1.5),
          axis.title.x=element_blank(),
          axis.text.x=element_text(size=20),
          axis.text.y=element_text(size=15),
          legend.text=element_text(size=15),
          legend.title=element_text(size=15),
          legend.position="top",
          strip.text.x = element_text(size=15,face="bold") 
    )
}
################################################################################
################################################################################
# Apply to each environmental variable
fvh.violin(bsm=bsm,varname="V.hi",ytit="log 200 kHz Volume",ylog=TRUE)
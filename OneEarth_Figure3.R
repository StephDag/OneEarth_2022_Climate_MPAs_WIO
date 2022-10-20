# Aim: Figure 3

rm(list=ls(all.names=T))

# load libraries
library(dplyr)
library(tibble)
library(tidyr)
library(data.table) # use fread() for big data set
library(reshape2)
library(memisc)
library(here)
library(FD) 
library(doBy) # for sorting
library(papeR)
library(cluster) # for the distance
library(ape) # for the PCOA
library(geometry)
library(ape)
library(ggpubr)

# load traits database for WIO (only 6 traits)
#rm(WIO.traits)
WIO.traits %>% rm()
WIO.traits <- read.csv(here("_data","WIO_traits_Corals_367sp.csv"),header=T,sep=",")
colnames(WIO.traits)[1] <- "species"
WIO.traits %>% head()
dim(WIO.traits)

# traits as factors
WIO.traits$cat_growthrate <- factor(WIO.traits$cat_growthrate,ordered=T) # growth rate
WIO.traits$cat_skeletaldensity  <- factor(WIO.traits$cat_skeletaldensity,ordered=T) # skeletal density
WIO.traits$cat_corallitesize  <- factor(WIO.traits$cat_corallitesize,ordered=T) # corallite size
WIO.traits$cat_colonyheight  <- factor(WIO.traits$cat_colonyheight,ordered=T) # colony height
WIO.traits$cat_colonydiameter  <- factor(WIO.traits$cat_colonydiameter,ordered=T) # max colony size
WIO.traits$cat_SA_vol  <- factor(WIO.traits$cat_SA_vol,ordered=T) #surface to area volume ratio
WIO.traits$cat_spacesize <- factor(WIO.traits$cat_spacesize,ordered=T) # space size

# load traits with convexity data
OE.WIO.traits.conv <- readRDS(here("_data","OneEarth_WIO.coral_traits.rds"))

    # check
summary(WIO.traits)
summary(OE.WIO.traits.conv)

### PCOA
    #  2. FD 6 TRAITS
    # 2.1. PCOA with Gower distance with 6 traits
    colnames(WIO.traits)[1] <- "Species"
    rownames(WIO.traits) <- WIO.traits$Species
    WIO.traits$Species <- NULL
    head(WIO.traits)

  # gower distance
    set.seed(123)
    rm(d.traits)
    d.traits <- daisy(WIO.traits,metric="gower") # compute the distance matrix with GOWER distance with the 6 traits
    rm(pcoa.traits)
    pcoa.traits <- pcoa(d.traits,correction="cailliez")
    head(pcoa.traits$vectors.cor[,1:4])
    
    # compute FRIC pool
    rm(FRic_pool)
    FRic_pool<-convhulln(pcoa.traits$vectors.cor[,1:4],"FA")$vol
    FRic_pool

# compute random FRIc
    n = dim(WIO.traits)[1] - 8
    rm(MAT.FRIC)
    MAT.FRIC <- matrix(nrow=n,ncol=999,NA) # random functional richness
    rm(MAT.CONV)
    MAT.CONV <- matrix(nrow=n,ncol=999,NA) # random convexity
    rm(MAT.CONV.W)
    MAT.CONV.W<- matrix(nrow=n,ncol=999,NA) # random weighted convexity
    
    for (i in 1:n) {
      for (j in 1:999){
        
    print(paste("i=",i,"j=",j))
        # select randomly species
    rm(n.sampl) 
    n.sampl <- sort(sample(c(1:367), size=i, replace =F))
    n.sampl
        # compute new fric
    rm(FRic_pool_1)
    FRic_pool_1<-convhulln(pcoa.traits$vectors.cor[-n.sampl,1:4],"FA")$vol
    MAT.FRIC[i,j] <- FRic_pool_1
        # compute convexity of the new species pool
    rm(sp)
    sp <- rownames(pcoa.traits$vectors.cor[-n.sampl,1:4])
    sp.row <- which(OE.WIO.traits.conv$Species %in% sp)

    # mean convexity for the remaining pool 
    MAT.CONV[i,j] <- round(mean(OE.WIO.traits.conv[sp.row,"Mean_convexity"],na.rm=T),2)
    
    # mean weighted convexity for the remaining pool
    MAT.CONV.W[i,j] <- round(sum(as.numeric(OE.WIO.traits.conv[sp.row,"cat_growthrate"])*OE.WIO.traits.conv[sp.row,"Mean_convexity"],na.rm=T)/sum(as.numeric(OE.WIO.traits.conv[sp.row,"cat_growthrate"])),2)
    
      }
    }
    
    #write.csv(MAT.FRIC,here("_data","MAT.FRIC.coral.rand.csv"))
    #write.csv(MAT.CONV,here("_data","MAT.CONV.coral.rand.csv"))
    #write.csv(MAT.CONV.W,here("_data","MAT.CONV.W.coral.rand.csv"))
    
    # compute CI around the mean and prediction intervals around the mean
    rm(CI.FRIC)
    CI.FRIC <- matrix(nrow=n,ncol=6,NA)
    colnames(CI.FRIC) <- c("CI.l","CI.u","mean","median","PI.l","PI.h")
    for (i in 1:359){
      ci <- t.test(MAT.FRIC[i,]/FRic_pool)$conf.int
      pi <- predict(lm((MAT.FRIC[i,]/FRic_pool)~ 1), interval="predict") 
      CI.FRIC[i,1] <- ci[1] # confidence interval low
      CI.FRIC[i,2] <- ci[2] # confidence interval high
      CI.FRIC[i,3] <- mean(MAT.FRIC[i,]/FRic_pool) # mean random FRic
      CI.FRIC[i,4] <- median(MAT.FRIC[i,]/FRic_pool) # median random FRic
      CI.FRIC[i,5] <-  pi[1,2] # predictive interval low
      CI.FRIC[i,6] <- pi[1,3]  # predictive interval high
    }
    head(CI.FRIC)
    #write.csv(CI.FRIC,here("_data","FRIC.CI.random.csv"))
    
    # compute CI conv weighted around the mean and prediction intervales around the mean
    
    rm(CI.CONV.W)
    CI.CONV.W <- matrix(nrow=n,ncol=6,NA)
    colnames(CI.CONV.W) <- c("CI.l","CI.u","mean","median","PI.l","PI.h")
    for (i in 1:359){
      ci <- predict(lm(MAT.CONV.W[i,]~ 1), interval="confidence") 
      pi <- predict(lm((MAT.CONV.W[i,])~ 1), interval="predict") 
      CI.CONV.W[i,1] <- ci[1]
      CI.CONV.W[i,2] <- ci[2]
      CI.CONV.W[i,3] <- mean(MAT.CONV.W[i,])
      CI.CONV.W[i,4] <- median(MAT.CONV.W[i,])
      CI.CONV.W[i,5] <-  pi[1,2]
      CI.CONV.W[i,6] <- pi[1,3]
      
    }
    head(CI.CONV.W)
    dim(CI.CONV.W)
    summary(CI.CONV.W)
    
    perc <- round(100*(seq(1,359,1)/359),1)
    CI.CONV.W <- cbind(CI.CONV.W,perc)
    #write.csv(CI.FRIC,here("outputs","CONVEX.CI.coral.random.csv"))
    
    # plot median + predictions intervals around the median
    plot(CI.FRIC[,"median"],type="l",lwd=4)
    lines(CI.FRIC[,5],col="grey",lwd=1)
    lines(CI.FRIC[,6],col="grey",lwd=1)

########################
#    SOURCE scenario   #
########################   

    # order coral species for source scenario
    srce.order <- c("Competitive","Generalist","Weedy","Stress-tolerant")

    # order traits for source
    OE.WIO.traits.conv$cat.rank.srce <- ifelse(OE.WIO.traits.conv$cat2pred == "Competitive",1,ifelse(
      OE.WIO.traits.conv$cat2pred == "Generalist",2,ifelse(
        OE.WIO.traits.conv$cat2pred == "Weedy",3,4)))
    OE.WIO.traits.conv %>% head()
    
    # source data
    rm(newdata.srce)
    newdata.srce <- OE.WIO.traits.conv[order(OE.WIO.traits.conv$cat.rank.srce),]
    summary(newdata.srce)
    head(newdata.srce)
    
    # create new matrix for source estimates of FRic and convexity
    rm(MAT.FRIC.CONV.srce)
    MAT.FRIC.CONV.srce <- matrix(nrow=n,ncol=3,NA)
    colnames(MAT.FRIC.CONV.srce) <- c("FRic","CONV","W.CONV")
    head(MAT.FRIC.CONV.srce)
    # V2: remove by order of group one species after another
    
    for (i in 1:n){
      print(paste("i=",i))
      sp <- which(rownames(pcoa.traits$vectors.cor) %in% newdata.srce$Species[-c(1:i)])
      
      rm(FRic_pool_1)
      FRic_pool_1<-convhulln(pcoa.traits$vectors.cor[sp,1:4],"FA")$vol
      
      MAT.FRIC.CONV.srce[i,1] <- FRic_pool_1/FRic_pool
      # mean convexity for the remaining pool
      rm(sp.1)
      sp.1 <- rownames(pcoa.traits$vectors.cor[sp,])
      sp.row <- which(OE.WIO.traits.conv$Species %in% sp.1)
      
      MAT.FRIC.CONV.srce[i,2] <- round(mean(OE.WIO.traits.conv[sp.row,"Mean_convexity"],na.rm=T),2)
      
      # mean weighted convexity for the remaining pool
      MAT.FRIC.CONV.srce[i,3] <- round(sum(as.numeric(OE.WIO.traits.conv[sp.row,"cat_growthrate"])*OE.WIO.traits.conv[sp.row,"Mean_convexity"],na.rm=T)/sum(as.numeric(OE.WIO.traits.conv[sp.row,"cat_growthrate"])),2)
      
    }
    
    #write.csv(MAT.FRIC.CONV.srce,here("outputs","FRIC.CONVEX.coral.source.csv"))
    
    lines(MAT.FRIC.CONV.srce[,1])
    
    # bind all
    rm(all.srce)
    all.srce <- cbind(CI.FRIC,CI.CONV.W,MAT.FRIC.CONV.srce,newdata.srce$cat.rank.srce[1:359])
    colnames(all.srce)[17] <- "rank"
    colnames(all.srce)[1:6] <- paste(colnames(all.srce)[1:6],"FRIC.random",sep="_")
    colnames(all.srce)[7:12] <- paste(colnames(all.srce)[7:12],"Convex.random",sep="_")
    colnames(all.srce)[13:16] <- paste(colnames(all.srce)[13:16],"Source",sep="_")
    
    all.srce <- as.data.frame(all.srce)
    all.srce$rank_Source <- as.factor( all.srce$rank_Source)
    all.srce$perc <- round(100*(seq(1,359,1)/359),1)
    summary(all.srce)
    head(all.srce)
    str(all.srce)
   
#############################
#    CONVERGENCE scenario   #
############################# 
    
    # order traits for convergence
    OE.WIO.traits.conv$cat.rank.conv <- ifelse(OE.WIO.traits.conv$cat2pred == "Competitive",4,ifelse(
      OE.WIO.traits.conv$cat2pred == "Generalist",3,ifelse(
        OE.WIO.traits.conv$cat2pred == "Weedy",2,1)))

    # summary number of species
    OE.WIO.traits.conv %>% dplyr::select(cat2pred,Species) %>%
      dplyr::group_by(cat2pred) %>%
      dplyr::summarize(n = n())

    # convergence data
    rm(newdata.conv)
    newdata.conv <- OE.WIO.traits.conv[order(OE.WIO.traits.conv$cat.rank.conv),]
    summary(newdata.conv)
    head(newdata.conv)
    
    # create new matrix for convergence estimates
rm(MAT.FRIC.CONV.conv)
MAT.FRIC.CONV.conv <- matrix(nrow=359,ncol=3,NA)
colnames(MAT.FRIC.CONV.conv) <- c("FRic","CONV","W.CONV")
head(MAT.FRIC.CONV.conv)
# V2: remove by order of group one species after another

for (i in 1:359){
  
  sp <- which(rownames(pcoa.traits$vectors.cor) %in% newdata.conv$Species[-c(1:i)])
  
  rm(FRic_pool_1)
  FRic_pool_1<-convhulln(pcoa.traits$vectors.cor[sp,1:4],"FA")$vol
  
  MAT.FRIC.CONV.conv[i,1] <- FRic_pool_1/FRic_pool
  # mean convexity for the remaining pool
  rm(sp.1)
  sp.1 <- rownames(pcoa.traits$vectors.cor[sp,])
  sp.row <- which(OE.WIO.traits.conv$Species %in% sp.1)
  
  MAT.FRIC.CONV.conv[i,2] <- round(mean(OE.WIO.traits.conv[sp.row,"Mean_convexity"],na.rm=T),2)
  
  # mean weighted convexity for the remaining pool
  MAT.FRIC.CONV.conv[i,3] <- round(sum(as.numeric(OE.WIO.traits.conv[sp.row,"cat_growthrate"])*OE.WIO.traits.conv[sp.row,"Mean_convexity"],na.rm=T)/sum(as.numeric(OE.WIO.traits.conv[sp.row,"cat_growthrate"])),2)
  
}

#write.csv(MAT.FRIC.CONV.conv,here("outputs","FRIC.CONV.coral.convergence.csv"))

# bind all
rm(all.conv)
all.conv <- cbind(CI.FRIC,CI.CONV.W,MAT.FRIC.CONV.conv,newdata.conv$cat.rank.conv[1:359])
colnames(all.conv)[17] <- "rank"
colnames(all.conv)[1:6] <- paste(colnames(all.conv)[1:6],"FRIC.random",sep="_")
colnames(all.conv)[7:12] <- paste(colnames(all.conv)[7:12],"Convex.random",sep="_")
colnames(all.conv)[13:16] <- paste(colnames(all.conv)[13:16],"Convergence",sep="_")
all.conv<- as.data.frame(all.conv)
all.conv$rank_Convergence <- as.factor(all.conv$rank_Convergence)
all.conv$perc <- round(100*(seq(1,359,1)/359),1)
summary(all.conv)
head(all.conv)

#######################
#       FIGURE 3      #
#######################
 
#### nice plot srce + convergence
# FD loss
cols <- c("Source"="#CC79A7","Convergence"="#56B4E9","Random"="lightgrey")
library(ggplot2)
FD.coral %>% rm()
FD.coral <- ggplot(data=as.data.frame(all.srce),aes(y=median_FRIC.random,x=perc,ymin=PI.l_FRIC.random, ymax=PI.h_FRIC.random))+#,color=rank)) +
  geom_line(color="black") +
  geom_line(data=as.data.frame(all.srce),aes(y=PI.l_FRIC.random,x=perc,col="lightgrey")) +
  geom_line(data=as.data.frame(all.srce),aes(y=PI.h_FRIC.random,x=perc,col="lightgrey")) +
  geom_ribbon(alpha=0.5,color="lightgrey",fill="lightgrey") +
  geom_line(data=as.data.frame(all.srce),aes(y=FRic_Source,x=perc,col="#56B4E9"),size=2) +
  geom_line(data=as.data.frame(all.conv),aes(y=FRic_Convergence,x=perc,col="#CC79A7"),size=2) +
  labs(x="Species loss (%)",y="Functional Richness (FRic)")+
  scale_x_continuous(expand = c(0, 2)) +
  scale_color_manual(values=c("#CC79A7","#56B4E9","lightgrey"),name = "Scenarios", labels = c("Source", "Convergence","Random")) +
  #scale_colour_manual(name="Scenarios",values=cols) + #scale_fill_manual(name="Random",values=cols) +
  theme_bw() +
  theme(axis.text.x = element_text(size=19),axis.text.y = element_text(size=19),
        axis.title.x = element_text(size = 20,face="bold"),axis.title.y = element_text(size = 20,face="bold"),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank()) +
  theme(plot.margin = unit(c(0,0.5,0,0.5), "cm")) +
  theme(legend.title = element_text(size=20,face="bold"),legend.text = element_text(size=18))

FD.coral 

ggsave(here("_Figures","Fig3a_FRic_loss_coral.pdf"),FD.coral,width=11.5,height=8,units="in",dpi=300)

# Convexity changes
# plot median + predictions intervals around the median
CONV.coral %>% rm()
CONV.coral <- ggplot(data=as.data.frame(all.srce),aes(y=mean_Convex.random,x=perc,ymin=PI.l_Convex.random, ymax=PI.h_Convex.random)) +
  geom_line(color="black") +
  geom_line(data=as.data.frame(all.srce),aes(y=PI.l_Convex.random,x=perc,col="lightgrey")) +
  geom_line(data=as.data.frame(all.srce),aes(y=PI.h_Convex.random,x=perc,col="lightgrey")) +
  geom_ribbon(alpha=0.5,color="lightgrey",fill="lightgrey") +
  ylim(c(0,1)) +
  geom_line(data=as.data.frame(all.srce),aes(y=W.CONV_Source,x=perc,col="#56B4E9"),size=2) +
  geom_line(data=as.data.frame(all.conv),aes(y=W.CONV_Convergence,x=perc,col="#CC79A7"),size=2) +
  labs(x="Species loss (%)",y="Coral Assemblage conxevity") + 
  scale_x_continuous(expand = c(0, 2)) +
  scale_color_manual(values=c("#CC79A7","#56B4E9","lightgrey"),name = "Scenarios", labels = c("Source", "Convergence","Random")) +
  scale_y_reverse()+
  theme_bw() +
  theme(axis.text.x = element_text(size=19),axis.text.y = element_text(size=19),
        axis.title.x = element_text(size = 20,face="bold"),axis.title.y = element_text(size = 20,face="bold"),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank()) +
  theme(plot.margin = unit(c(1,0.5,0,0.5), "cm"))  +
  theme(legend.title = element_text(size=20,face="bold"),legend.text = element_text(size=18))
CONV.coral

ggsave(here("_Figures","Fig3b_Convexity_loss.pdf"),CONV.coral,width=11.5,height=8,units="in",dpi=300)

###
dev.off()
p <- ggarrange(FD.coral,CONV.coral,
               ncol = 1, labels = c("A","B"),nrow = 2,align="hv",common.legend = T,font.label = list(size = 20),legend="bottom") 
ggsave(here("_Figures","Figure3_FRIC_Convexity_loss.pdf"),p,width=8,height=11,units="in",dpi=300)
ggsave(here("_Figures","Figure3_FRIC_Convexity_loss.tiff"),p,width=8,height=11,units="in",dpi=300)

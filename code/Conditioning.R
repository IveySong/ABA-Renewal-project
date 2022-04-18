rm(list = ls())
library(readxl)
library(openxlsx)
library(ggplot2)
library(lme4)
library(lmerTest)
library(afex)
library(dplyr)
library(tidyr)
library(stringr)
library(Rmisc)

##Read data##
# dat_pt <- c()
# dat_en<-c()
session<-c(2)
for (s in session) {
  path<-"D:/Ivey/Academic/ABA/data/phase3_test_ABA"
  subdir <- paste0(path,"/session",s,"/", sep="")
  files <- list.files(path = subdir)
  for (f in files) {
    file <- paste(subdir, f, sep="")
    readdata<-function(varname,sheetname){
      varname<-read_xlsx(file,sheet = sheetname,col_names = c("startt","duration"),skip = 2)
      varname<-as.matrix(varname)
    }

    poketime<-readdata(poketime,"IN3")  #poketime
    soundB<-readdata(soundB,"OUT1")   # clicker
    soundR<-readdata(soundR,"OUT7")   #phaser
    df_all<-matrix(NA,nrow=8000000,ncol=8)
    colnames(df_all) <- c("subject", "session","poke","poketrials","reward","trials","soundtype","withbaseline")
    df_all<-as.data.frame(df_all)
    
    #the sound duration
    pfs=10000
    #baseline pre trials
    t_baseline=10000
    
    #combine data
    #poke time & duration
    for (i in 1:length(poketime[,1])) {
      n=poketime[i,1]
      m=poketime[i,2]
      df_all$poketrials[n:(n+m)]=i
      df_all$poke[n:(n+m)]=1
    }
    
    #sound B  Clicker    10s
    for(i in 1:length(soundB[,1])){
      n=soundB[i,1]
      m=soundB[i,2]
      df_all$trials[(n-t_baseline):(n+m)]=i
      df_all$soundtype[(n-t_baseline):(n+m)]<-"B"
      df_all$reward[(n-t_baseline):(n+m)]<-"reward"
      df_all$withbaseline[(n-t_baseline):n]="poke_baseline"
      df_all$withbaseline[n:(n+m)]="poke_sound"
    }
    #sound R phaser  10s
    for(i in 1:length(soundR[,1])){
      n=soundR[i,1]
      m=soundR[i,2]
      df_all$trials[(n-t_baseline):(n+m)]=i
      df_all$soundtype[(n-t_baseline):(n+m)]<-"R"
      df_all$reward[(n-t_baseline):(n+m)]<-"unreward"
      df_all$withbaseline[(n-t_baseline):n]="poke_baseline"
      df_all$withbaseline[n:(n+m)]="poke_sound"
    }
    
    # df is in 10s sound period + 10s baseline before the sound 
    df<-df_all[complete.cases(df_all$withbaseline),]

    #alternate the NA to 0
    df[is.na(df[,3]),3] <- 0  #poke
    df[is.na(df[,4]),4] <- 0  #poke trials
    
    #add sub & session information
    df$subject=as.numeric(substring(f,4,5))##get the fourth and fifth number
    df$session=s    
    
    
    df_poketime<-aggregate(poke~subject*session*trials*soundtype*poketrials*withbaseline,data = df,FUN = sum) #calculate trials*poke trials
    colnames(df_poketime)<-c("subject", "session","trials","soundtype","poketrials","withbaseline","poketime")
    df_poketime2<-df_poketime
    df_poketime2$pokeentries=1
    df_poketime2[df_poketime2$poketime==0,8]<-0
    #df_poke number
    df_entriesnum<-aggregate(pokeentries~subject*session*trials*soundtype*withbaseline,data = df_poketime2,FUN = sum)
    #df_poketime: poke time 
    df_poketime<-aggregate(poke~subject*session*trials*soundtype*withbaseline,data = df,FUN = sum)
    colnames(df_poketime)<-c("subject", "session","trials","soundtype","withbaseline","poketime")
  
    #combine data
    dat_pt<-rbind(dat_pt,df_poketime)
    dat_en<-rbind(dat_en,df_entriesnum)
  }
  
  setwd(path)
  # if(file.exists("dat_pt.RData")){
  #   file.remove("dat_pt.RData")
  # }
  # if(file.exists("dat_en.RData")){
  #   file.remove("dat_en.RData")
  # }
  save(dat_pt, file = paste0(s,"_dat_pt.RData"))
  save(dat_en, file =  paste0(s,"_dat_en.RData"))
}

#


#Do statistics
#split the data into two parts
# dat_pt[dat_pt$withbaseline==0,5]="poke_baseline"
# dat_pt[dat_pt$withbaseline==1,5]="poke_sound"
# dat_en[dat_en$withbaseline==0,5]="poke_baseline"
# dat_en[dat_en$withbaseline==1,5]="poke_sound"
#s means spread
percent=10000/100
spt<-spread(dat_pt,key = "withbaseline",value = "poketime")
spt$poke_baseline=spt$poke_baseline/percent
spt$poke_sound=spt$poke_sound/percent
spt$dif<-spt$poke_sound-spt$poke_baseline

sen<-spread(dat_en,key = "withbaseline",value = "pokeentries")
sen$dif<-sen$poke_sound-sen$poke_baseline


#normalize DV
spt$nor_poke_sound<-scale(spt$poke_sound,center=TRUE, scale=TRUE)
spt$nor_poke_dif<-scale(spt$dif,center=TRUE, scale=TRUE)
sen$nor_poke_sound<-scale(sen$poke_sound,center=TRUE, scale=TRUE)
sen$nor_poke_dif<-scale(sen$dif,center=TRUE, scale=TRUE)

#sound duration
p_pt<-aov(nor_poke_sound~session*subject*soundtype,spt)
anova(p_pt)
p_en<-aov(nor_poke_sound~session*subject*soundtype,sen)
anova(p_en)
#sound duration-baseline
p_pt<-aov(nor_poke_dif~session*subject*soundtype,spt)
anova(p_pt)
p_en<-aov(nor_poke_dif~session*subject*soundtype,sen)
anova(p_en)



#calculate mean sound period
s_spt<-aggregate(poke_sound~soundtype,data = spt,FUN = mean)
s_sen<-aggregate(poke_sound~soundtype,data = sen,FUN = mean)
#calculate mean sound period-baseline
dif_pt<-aggregate(dif~soundtype,data = spt,FUN = mean)
dif_en<-aggregate(dif~soundtype,data = sen,FUN = mean)



###########################################################################
##################################PLOT#####################################
###########################################################################
spt$session<-as.factor(spt$session)
sen$session<-as.factor(sen$session)
spt$subject<-as.factor(spt$subject)
sen$subject<-as.factor(sen$subject)

labels<-c("1"="session1","2"="session2","3"="session3","4"="session4","5"="session5","6"="session6","7"="session7","8"="session8","9"="session9","10"="session10","11"="Ct","12"="session12","13"="session13","14"="session14")
#,"11"="session11","12"="session12","13"="session13","14"="session14","15"="session15","16"="session16"
labels2<-c("1"="sub1","2"="sub2","3"="sub3","4"="sub4","5"="sub5","6"="sub6","7"="sub7","8"="sub8","9"="sub9","10"="sub10","11"="sub11","12"="sub12","13"="sub13","14"="sub14","15"="sub15","16"="sub16")

#plot type1  with each trials  total
trailsnum=16


#TYPE 1  sound period   poketime
spt1=spt
p <- ggplot(data =spt1, aes(x=trials, y=poke_sound,color=soundtype)) +
  geom_line(aes(group = soundtype))+
  geom_point(size=.7)  +
  facet_grid(rows = vars(subject),cols = vars(session),labeller = labeller(session=labels,subject=labels2)) +
  xlim(0,12)+
  xlab("Trials")+
  ylab("Poke Time Percent")+
  scale_colour_discrete(name=("Sound"),breaks = c("B","R"), labels = c("Reward","Unreward"))+
  guides(fill = guide_legend(title = NULL))+
  scale_x_continuous(breaks = seq(0,trailsnum,1))
#+ stat_summary(fun = "mean", geom = "point", shape = 4, size = 2)
p

#type 1  sound period   pokeentries
sen1=sen
p <- ggplot(data = sen1, aes(x=trials, y=poke_sound,color=soundtype)) +
  geom_line(aes(group = soundtype))+
  geom_point(size=.7)  +
  facet_grid(rows = vars(subject),cols = vars(session),labeller = labeller(session=labels,subject=labels2)) +
  xlim(0,12)+
  xlab("Trials")+
  ylab("Number of Entries")+ 
  scale_colour_discrete(name=("Sound"),breaks = c("B","R"), labels = c("Reward","Unreward"))+
  guides(fill = guide_legend(title = NULL))+
  scale_x_continuous(breaks = seq(0,trailsnum,1))
p


##type 1    -baseline   poketime
p <- ggplot(data =spt1, aes(x=trials, y=dif,color=soundtype)) +
  geom_line(aes(group = soundtype))+
  geom_point(size=.7)  +
  facet_grid(rows = vars(subject),cols = vars(session),labeller = labeller(session=labels,subject=labels2)) +
  xlim(0,12)+
  xlab("Trials")+
  ylab("Poke Time Percent - Baseline")+
  scale_colour_discrete(name=("Sound"),breaks = c("B","R"), labels = c("Reward","Unreward"))+
  guides(fill = guide_legend(title = NULL))+
  scale_x_continuous(breaks = seq(0,trailsnum,1))
#+ stat_summary(fun = "mean", geom = "point", shape = 4, size = 2)
p

##type 1     -baseline   poke entries
p <- ggplot(data = sen1, aes(x=trials, y=dif,color=soundtype)) +
  geom_line(aes(group = soundtype))+
  geom_point(size=.7)  +
  facet_grid(rows = vars(subject),cols = vars(session),labeller = labeller(session=labels,subject=labels2)) +
  xlim(0,12)+
  xlab("Trials")+
  ylab("Number of Entries - Baseline")+ 
  scale_colour_discrete(name=("Sound"),breaks = c("B","R"), labels = c("Reward","Unreward"))+
  guides(fill = guide_legend(title = NULL))+
  scale_x_continuous(breaks = seq(0,trailsnum,1))
p




##plot TYPE 2 with session total
##type 2     sound period   poketime
spt2<-aggregate(poke_sound~subject*session*soundtype,data = spt,FUN = mean)
p <- ggplot(data =spt2, aes(x=session,y=poke_sound,color=soundtype)) +
  geom_line(aes(group = soundtype))+
  geom_point(size=.7) +
  facet_grid(subject ~ .,labeller = labeller(subject=labels2))+
  # xlim(0,12)+
  xlab("Sessions")+
  ylab("Poke Time Percent")+
  scale_colour_discrete(name=("Sound"),breaks = c("B","R"), labels = c("Reward","Unreward"))+
  guides(fill = guide_legend(title = NULL))
  # scale_x_continuous(breaks = seq(0,trailsnum,1))
#+ stat_summary(fun = "mean", geom = "point", shape = 4, size = 2)
p

##type 2     sound period   pokeentries
sen2<-aggregate(poke_sound~subject*session*soundtype,data = sen,FUN = mean)
p <- ggplot(data =sen2, aes(x=session,y=poke_sound,color=soundtype)) +
  geom_line(aes(group = soundtype))+
  geom_point(size=.7) +
  facet_grid(subject ~ .,labeller = labeller(subject=labels2))+
  # xlim(0,12)+
  xlab("Sessions")+
  ylab("Number of Entries")+
  scale_colour_discrete(name=("Sound"),breaks = c("B","R"), labels = c("Reward","Unreward"))+
  guides(fill = guide_legend(title = NULL))
# scale_x_continuous(breaks = seq(0,trailsnum,1))
#+ stat_summary(fun = "mean", geom = "point", shape = 4, size = 2)
p

##type 2    -baseline   poketime
spt2_dif<-aggregate(dif~subject*session*soundtype,data = spt,FUN = mean)
p <- ggplot(data =spt2_dif, aes(x=session,y=dif,color=soundtype)) +
  geom_line(aes(group = soundtype))+
  geom_point(size=.7) +
  facet_grid(subject ~ .,labeller = labeller(subject=labels2))+
  # xlim(0,12)+
  xlab("Sessions")+
  ylab("Poke Time Percent - Baseline")+
  scale_colour_discrete(name=("Sound"),breaks = c("B","R"), labels = c("Reward","Unreward"))+
  guides(fill = guide_legend(title = NULL))
# scale_x_continuous(breaks = seq(0,trailsnum,1))
#+ stat_summary(fun = "mean", geom = "point", shape = 4, size = 2)
p

##type 2    -baseline   poke entries
sen2_dif<-aggregate(dif~subject*session*soundtype,data = sen,FUN = mean)
p <- ggplot(data = sen2_dif, aes(x=session, y=dif,color=soundtype)) +
  geom_line(aes(group = soundtype))+
  geom_point(size=.7)  +
  facet_grid(subject ~ .,labeller = labeller(subject=labels2))+
  xlab("Sessions")+
  ylab("Number of Entries - Baseline")+ 
  scale_colour_discrete(name=("Sound"),breaks = c("B","R"), labels = c("Reward","Unreward"))+
  guides(fill = guide_legend(title = NULL))
p



##TYPE 3 Total subject in each session
##type 3     sound period   poketime
spt3<-aggregate(poke_sound~subject*session*soundtype,data = spt,FUN = mean)
sptm<-spt3
spt3<-summarySE(spt3,"poke_sound",groupvars = c("soundtype","session"))
p <- ggplot(data =spt3, aes(x=session,y=poke_sound,color=soundtype)) +
  geom_line(aes(group = soundtype))+
  geom_point(size=2) +
  geom_errorbar(aes(ymin=poke_sound-se,ymax=poke_sound+se),width=.2,position = position_dodge(0))+
  geom_point(data=sptm,aes(x=session,y=poke_sound,color=soundtype),size=1,alpha = 1/2) +
  # facet_grid(subject ~ .,labeller = labeller(subject=labels2))+
  # xlim(0,12)+
  xlab("Sessions")+
  ylab("Poke Time Percent")+
  coord_cartesian(ylim = c(0, 90))+
  scale_y_continuous(breaks = seq(0,90,10))+
  scale_colour_discrete(name=("Sound"),breaks = c("B","R"), labels = c("Reward","Unreward"))+
  guides(fill = guide_legend(title = NULL))+
  ggtitle("Average poketime percent for each session")
  # theme_bw()
# scale_x_continuous(breaks = seq(0,trailsnum,1))
#+ stat_summary(fun = "mean", geom = "point", shape = 4, size = 2)
p


##type 3    sound period   pokeentries
sen3<-aggregate(poke_sound~subject*session*soundtype,data = sen,FUN = mean)
senm<-sen3
sen3<-summarySE(sen3,"poke_sound",groupvars = c("soundtype","session"))
p <- ggplot(data =sen3, aes(x=session,y=poke_sound,color=soundtype)) +
  geom_line(aes(group = soundtype))+
  geom_point(size=2) +
  geom_errorbar(aes(ymin=poke_sound-se,ymax=poke_sound+se),width=.2,position = position_dodge(0))+
  geom_point(data=senm,aes(x=session,y=poke_sound,color=soundtype),size=1,alpha = 1/2) +
  # facet_grid(subject ~ .,labeller = labeller(subject=labels2))+
  # xlim(0,12)+
  xlab("Sessions")+
  ylab("Number of Entries")+
  coord_cartesian(ylim = c(0,15))+
  scale_y_continuous(breaks = seq(0,15,3))+
  scale_colour_discrete(name=("Sound"),breaks = c("B","R"), labels = c("Reward","Unreward"))+
  guides(fill = guide_legend(title = NULL))+
  ggtitle("Average entries number for each session")
# theme_bw()
# scale_x_continuous(breaks = seq(0,trailsnum,1))
#+ stat_summary(fun = "mean", geom = "point", shape = 4, size = 2)
p


##type 3   -baseline   poketime
spt3_dif<-aggregate(dif~subject*session*soundtype,data = spt,FUN = mean)
sptm_dif<-spt3_dif
spt3_dif<-summarySE(spt3_dif,"dif",groupvars = c("soundtype","session"))
p <- ggplot(data =spt3_dif, aes(x=session,y=dif,color=soundtype)) +
  geom_line(aes(group = soundtype))+
  geom_point(size=2) +
  geom_errorbar(aes(ymin=dif-se,ymax=dif+se),width=.2,position = position_dodge(0))+
  geom_point(data=sptm_dif,aes(x=session,y=dif,color=soundtype),size=1,alpha = 1/2) +
  # facet_grid(subject ~ .,labeller = labeller(subject=labels2))+
  # xlim(0,12)+
  xlab("Sessions")+
  ylab("Poke Time Percent - Baseline")+
  coord_cartesian(ylim = c(0, 90))+
  scale_y_continuous(breaks = seq(0,90,10))+
  scale_colour_discrete(name=("Sound"),breaks = c("B","R"), labels = c("Reward","Unreward"))+
  guides(fill = guide_legend(title = NULL))+
  ggtitle("Average poketime percent for each session - Baseline")
# theme_bw()
# scale_x_continuous(breaks = seq(0,trailsnum,1))
#+ stat_summary(fun = "mean", geom = "point", shape = 4, size = 2)
p


##type 3    -baseline   poke entries
sen3_dif<-aggregate(dif~subject*session*soundtype,data = sen,FUN = mean)
senm_dif<-sen3_dif
sen3_dif<-summarySE(sen3_dif,"dif",groupvars = c("soundtype","session"))
p <- ggplot(data =sen3_dif, aes(x=session,y=dif,color=soundtype)) +
  geom_line(aes(group = soundtype))+
  geom_point(size=2) +
  geom_errorbar(aes(ymin=dif-se,ymax=dif+se),width=.2,position = position_dodge(0))+
  geom_point(data=senm_dif,aes(x=session,y=dif,color=soundtype),size=1,alpha = 1/2) +
  # facet_grid(subject ~ .,labeller = labeller(subject=labels2))+
  # xlim(0,12)+
  xlab("Sessions")+
  ylab("Number of Entries - Baseline")+
  coord_cartesian(ylim = c(0,15))+
  scale_y_continuous(breaks = seq(0,15,3))+
  scale_colour_discrete(name=("Sound"),breaks = c("B","R"), labels = c("Reward","Unreward"))+
  guides(fill = guide_legend(title = NULL))+
  ggtitle("Average entries number for each session -- Baseline")
# theme_bw()
# scale_x_continuous(breaks = seq(0,trailsnum,1))
#+ stat_summary(fun = "mean", geom = "point", shape = 4, size = 2)
p









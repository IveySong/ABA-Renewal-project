##PLOT
#CONDITIONING PHASE
load("D:/Ivey/Academic/ABA/data/phase1_conditioning/15_dat_en.RData")
load("D:/Ivey/Academic/ABA/data/phase1_conditioning/15_dat_pt.RData")
dat_en<-dat_en[dat_en$session==15,]
dat_pt<-dat_pt[dat_pt$session==15,]


#EXTINCT PHASE
load("D:/Ivey/Academic/ABA/data/phase2_extiction/6_dat_en.RData")
load("D:/Ivey/Academic/ABA/data/phase2_extiction/6_dat_pt.RData")
dat_en<-dat_en[dat_en$session==6,]
dat_pt<-dat_pt[dat_pt$session==6,]



#s means spread
percent=10000/100
spt<-spread(dat_pt,key = "withbaseline",value = "poketime")
spt$poke_baseline=spt$poke_baseline/percent
spt$poke_sound=spt$poke_sound/percent
spt$dif<-spt$poke_sound-spt$poke_baseline

sen<-spread(dat_en,key = "withbaseline",value = "pokeentries")
sen$dif<-sen$poke_sound-sen$poke_baseline

spt[spt$soundtype=='B',4]<-'Reward'
spt[spt$soundtype=='R',4]<-'Unreward'
sen[sen$soundtype=='B',4]<-'Reward'
sen[sen$soundtype=='R',4]<-'Unreward'


#normalize DV
spt$nor_poke_sound<-scale(spt$poke_sound,center=TRUE, scale=TRUE)
spt$nor_poke_dif<-scale(spt$dif,center=TRUE, scale=TRUE)
sen$nor_poke_sound<-scale(sen$poke_sound,center=TRUE, scale=TRUE)
sen$nor_poke_dif<-scale(sen$dif,center=TRUE, scale=TRUE)

spt$session<-as.factor(spt$session)
sen$session<-as.factor(sen$session)
spt$subject<-as.factor(spt$subject)
sen$subject<-as.factor(sen$subject)

trailsnum=16


## Plot the bar
test_spt<-aggregate(poke_sound~subject*soundtype,data = spt,FUN = mean)
sptm<-test_spt
test_spt<-summarySE(test_spt,"poke_sound",groupvars = c("soundtype"))
p <- ggplot(data =test_spt, aes(x=soundtype,y=poke_sound,fill=soundtype)) +
  geom_bar(stat = "identity", position = position_dodge())+
  # geom_point(size=2,position=position_dodge(.9)) +
  geom_point(data=sptm,aes(x=soundtype,y=poke_sound,color=soundtype),size=1,alpha = 1/2,position=position_dodge(.9)) +
  geom_errorbar(aes(ymin=poke_sound-se,ymax=poke_sound+se),width=.2,position=position_dodge(.9))+
  # facet_grid(subject ~ .,labeller = labeller(subject=labels2))+
  # xlim(0,12)+
  # xlab("Conditioning")+
  xlab("Extinction")+
  ylab("Poke Time Percent")+
  # scale_colour_discrete(name=("Sound"),breaks = c("B","R"), labels = c("Reward","Unreward"))+
  # guides(fill = guide_legend(title = NULL))+
  ggtitle("Average poketime percent for each session")

p


##sound period   pokeentries
test_sen<-aggregate(poke_sound~subject*soundtype,data = sen,FUN = mean)
senm<-test_sen
test_sen<-summarySE(test_sen,"poke_sound",groupvars = c("soundtype"))
p <- ggplot(data =test_sen, aes(x=soundtype,y=poke_sound,fill=soundtype)) +
  geom_bar(stat = "identity", position = position_dodge())+
  # geom_point(size=2) +
  geom_errorbar(aes(ymin=poke_sound-se,ymax=poke_sound+se),width=.2,position = position_dodge(0.9))+
  geom_point(data=senm,aes(x=soundtype,y=poke_sound,color=soundtype),size=1,alpha = 1/2,position=position_dodge(.9)) +
  # facet_grid(subject ~ .,labeller = labeller(subject=labels2))+
  # xlim(0,12)+
  # xlab("Conditioning")+
  xlab("Extinction")+
  ylab("Number of Entries")+
  # scale_colour_discrete(name=("Sound"),breaks = c("B","R"), labels = c("Reward","Unreward"))+
  # guides(fill = guide_legend(title = NULL))+
  ggtitle("Average entries number for each session")
# theme_bw()
# scale_x_continuous(breaks = seq(0,trailsnum,1))
#+ stat_summary(fun = "mean", geom = "point", shape = 4, size = 2)
p


## -baseline   poketime
test_spt_dif<-aggregate(dif~subject*soundtype,data = spt,FUN = mean)
sptm_dif<-test_spt_dif
test_spt_dif<-summarySE(test_spt_dif,"dif",groupvars = c("soundtype"))
p <- ggplot(data =test_spt_dif, aes(x=soundtype,y=dif,fill=soundtype)) +
  geom_bar(stat = "identity", position = position_dodge())+
  # geom_point(size=2) +
  geom_errorbar(aes(ymin=dif-se,ymax=dif+se),width=.2,position = position_dodge(0.9))+
  geom_point(data=sptm_dif,aes(x=soundtype,y=dif,color=soundtype),size=1,alpha = 1/2,position = position_dodge(0.9)) +
  # facet_grid(subject ~ .,labeller = labeller(subject=labels2))+
  # xlim(0,12)+
  # xlab("Conditioning")+
  xlab("Extinction")+
  ylab("Poke Time Percent - Baseline")+
  # scale_colour_discrete(name=("Sound"),breaks = c("B","R"), labels = c("Reward","Unreward"))+
  # guides(fill = guide_legend(title = NULL))+
  ggtitle("Average poketime percent for each session - Baseline")
# theme_bw()
# scale_x_continuous(breaks = seq(0,trailsnum,1))
#+ stat_summary(fun = "mean", geom = "point", shape = 4, size = 2)
p


## -baseline   poke entries
test_sen_dif<-aggregate(dif~subject*soundtype,data = sen,FUN = mean)
senm_dif<-test_sen_dif
test_sen_dif<-summarySE(test_sen_dif,"dif",groupvars = c("soundtype"))
p <- ggplot(data =test_sen_dif, aes(x=soundtype,y=dif,fill=soundtype)) +
  geom_bar(stat = "identity", position = position_dodge())+
  # geom_point(size=2) +
  geom_errorbar(aes(ymin=dif-se,ymax=dif+se),width=.2,position = position_dodge(0.9))+
  geom_point(data=senm_dif,aes(x=soundtype,y=dif,color=soundtype),size=1,alpha = 1/2,position = position_dodge(0.9)) +
  # facet_grid(subject ~ .,labeller = labeller(subject=labels2))+
  # xlim(0,12)+
  # xlab("Conditioning")+
  xlab("Extinction")+
  ylab("Number of Entries - Baseline")+
  # scale_colour_discrete(name=("Sound"),breaks = c("B","R"), labels = c("Reward","Unreward"))+
  # guides(fill = guide_legend(title = NULL))+
  ggtitle("Average entries number for each session -- Baseline")
# theme_bw()
# scale_x_continuous(breaks = seq(0,trailsnum,1))
#+ stat_summary(fun = "mean", geom = "point", shape = 4, size = 2)
p




























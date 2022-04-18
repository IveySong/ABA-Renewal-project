#combine data
#import ABB data
load("D:/Ivey/Academic/ABA/data/phase3_test_ABB/2_dat_en.RData")
load("D:/Ivey/Academic/ABA/data/phase3_test_ABB/2_dat_pt.RData")
ABB_en<-dat_en
ABB_pt<-dat_pt

ABB_en$Test<-'Context B'
ABB_pt$Test<-'Context B'

rm(dat_en)
rm(dat_pt)

#import ABA data
load("D:/Ivey/Academic/ABA/data/phase3_test_ABA/2_dat_en.RData")
load("D:/Ivey/Academic/ABA/data/phase3_test_ABA/2_dat_pt.RData")
ABA_en<-dat_en
ABA_pt<-dat_pt

ABA_en$Test<-'Context A'
ABA_pt$Test<-'Context A'

df_en<-rbind(ABA_en,ABB_en)
df_pt<-rbind(ABA_pt,ABB_pt)

#s means spread
percent=10000/100
spt<-spread(df_pt,key = "withbaseline",value = "poketime")
spt$poke_baseline=spt$poke_baseline/percent
spt$poke_sound=spt$poke_sound/percent
spt$dif<-spt$poke_sound-spt$poke_baseline

sen<-spread(df_en,key = "withbaseline",value = "pokeentries")
sen$dif<-sen$poke_sound-sen$poke_baseline


#normalize DV
spt$nor_poke_sound<-scale(spt$poke_sound,center=TRUE, scale=TRUE)
spt$nor_poke_dif<-scale(spt$dif,center=TRUE, scale=TRUE)
sen$nor_poke_sound<-scale(sen$poke_sound,center=TRUE, scale=TRUE)
sen$nor_poke_dif<-scale(sen$dif,center=TRUE, scale=TRUE)

#sound duration
p_pt<-aov(nor_poke_sound~Test*soundtype,spt)
anova(p_pt)
p_en<-aov(nor_poke_sound~Test*soundtype,sen)
anova(p_en)
#sound duration-baseline
p_pt<-aov(nor_poke_dif~Test*soundtype,spt)
anova(p_pt)
p_en<-aov(nor_poke_dif~Test*soundtype,sen)
anova(p_en)


#calculate mean sound period
s_spt<-aggregate(poke_sound~Test,data = spt,FUN = mean)
s_sen<-aggregate(poke_sound~Test,data = sen,FUN = mean)
#calculate mean sound period-baseline
dif_pt<-aggregate(dif~Test,data = spt,FUN = mean)
dif_en<-aggregate(dif~Test,data = sen,FUN = mean)


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

trailsnum=16

## Plot the bar
test_spt<-aggregate(poke_sound~subject*soundtype*Test,data = spt,FUN = mean)
sptm<-test_spt
test_spt<-summarySE(test_spt,"poke_sound",groupvars = c("soundtype","Test"))
p <- ggplot(data =test_spt, aes(x=Test,y=poke_sound,fill=soundtype)) +
  geom_bar(stat = "identity", position = position_dodge())+
  # geom_point(size=2,position=position_dodge(.9)) +
  geom_errorbar(aes(ymin=poke_sound-se,ymax=poke_sound+se),width=.2,position=position_dodge(.9))+
  geom_point(data=sptm,aes(x=Test,y=poke_sound,color=soundtype),size=1,alpha = 1/2,position=position_dodge(.9)) +
  # facet_grid(subject ~ .,labeller = labeller(subject=labels2))+
  # xlim(0,12)+
  xlab("Test")+
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


##sound period   pokeentries
test_sen<-aggregate(poke_sound~subject*soundtype*Test,data = sen,FUN = mean)
senm<-test_sen
test_sen<-summarySE(test_sen,"poke_sound",groupvars = c("soundtype","Test"))
p <- ggplot(data =test_sen, aes(x=Test,y=poke_sound,fill=soundtype)) +
  geom_bar(stat = "identity", position = position_dodge())+
  # geom_point(size=2) +
  geom_errorbar(aes(ymin=poke_sound-se,ymax=poke_sound+se),width=.2,position = position_dodge(0.9))+
  geom_point(data=senm,aes(x=Test,y=poke_sound,color=soundtype),size=1,alpha = 1/2,position=position_dodge(.9)) +
  # facet_grid(subject ~ .,labeller = labeller(subject=labels2))+
  # xlim(0,12)+
  xlab("Test")+
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


## -baseline   poketime
test_spt_dif<-aggregate(dif~subject*soundtype*Test,data = spt,FUN = mean)
sptm_dif<-test_spt_dif
test_spt_dif<-summarySE(test_spt_dif,"dif",groupvars = c("soundtype","Test"))
p <- ggplot(data =test_spt_dif, aes(x=Test,y=dif,fill=soundtype)) +
  geom_bar(stat = "identity", position = position_dodge())+
  # geom_point(size=2) +
  geom_errorbar(aes(ymin=dif-se,ymax=dif+se),width=.2,position = position_dodge(0.9))+
  geom_point(data=sptm_dif,aes(x=Test,y=dif,color=soundtype),size=1,alpha = 1/2,position = position_dodge(0.9)) +
  # facet_grid(subject ~ .,labeller = labeller(subject=labels2))+
  # xlim(0,12)+
  xlab("Test")+
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


## -baseline   poke entries
test_sen_dif<-aggregate(dif~subject*soundtype*Test,data = sen,FUN = mean)
senm_dif<-test_sen_dif
test_sen_dif<-summarySE(test_sen_dif,"dif",groupvars = c("soundtype","Test"))
p <- ggplot(data =test_sen_dif, aes(x=Test,y=dif,fill=soundtype)) +
  geom_bar(stat = "identity", position = position_dodge())+
  # geom_point(size=2) +
  geom_errorbar(aes(ymin=dif-se,ymax=dif+se),width=.2,position = position_dodge(0.9))+
  geom_point(data=senm_dif,aes(x=Test,y=dif,color=soundtype),size=1,alpha = 1/2,position = position_dodge(0.9)) +
  # facet_grid(subject ~ .,labeller = labeller(subject=labels2))+
  # xlim(0,12)+
  xlab("Test")+
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






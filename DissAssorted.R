#Assorted materials for the prospectus
#El√≠as Chavarr√≠a-Mora

library(rio) #importing
library(dplyr)
library(tidyr) #both good for tidying
library (ggplot2)
library(car) #for recoding
library (wnominate)

###
#Get and do some changes to the tweets database for the 2018 CR elections dataset
###
setwd("C:/Elias/1 Serious/Academia/University of Pittsburgh/1 Dissertation/Data/CR2018")
CR2018<- read.csv("tweets_CR2018.csv", encoding = "UTF-8" )
CR2018$user_name<-CR2018$author_id
CR2018$user_name<-car::recode(CR2018$user_name, "'19989379'='accionciudadana'")
CR2018$user_name<-car::recode(CR2018$user_name, "'823245705439547392'='CarlosAlvQ'") #***
CR2018$user_name<-car::recode(CR2018$user_name, "'2587844821'='AlianzaDC'")
CR2018$user_name<-car::recode(CR2018$user_name, "'49201423'='marioredondo'")
CR2018$user_name<-car::recode(CR2018$user_name, "'2194443991'='PTrabajadoresCR'")
CR2018$user_name<-car::recode(CR2018$user_name, "'2999633619'='jvega_cr'")
CR2018$user_name<-car::recode(CR2018$user_name, "'29837045'='FrenteAmplio'")
CR2018$user_name<-car::recode(CR2018$user_name, "'78932537'='EdgardoArayaS'")
CR2018$user_name<-car::recode(CR2018$user_name, "'968159915360620544'='SoyPINCR'")
CR2018$user_name<-car::recode(CR2018$user_name, "'252174687'='JDiegoCastroCR'")
CR2018$user_name<-car::recode(CR2018$user_name, "'282256674'='plncr'")
CR2018$user_name<-car::recode(CR2018$user_name, "'144583223'='alvarez_desanti'")
CR2018$user_name<-car::recode(CR2018$user_name, "'2176923912'='SergioMenaPNG'")
CR2018$user_name<-car::recode(CR2018$user_name, "'2358603307'='OttoGuevaraG'")
CR2018$user_name<-car::recode(CR2018$user_name, "'1569320167'='renovacionprc'")
CR2018$user_name<-car::recode(CR2018$user_name, "'369143606'='stephcamposofic'")
CR2018$user_name<-car::recode(CR2018$user_name, "'1037422095821664257'='restauracion_na'")
CR2018$user_name<-car::recode(CR2018$user_name, "'1589859326'='FabriAlvarado7'")
CR2018$user_name<-car::recode(CR2018$user_name, "'1090371633766903809'='SocialUnidad'")
CR2018$user_name<-car::recode(CR2018$user_name, "'721238259'='PizaRodolfo'")
CR2018$user_name<-car::recode(CR2018$user_name, "'828649770659041280'='RepublicanoCR'") #***
CR2018$user_name<-car::recode(CR2018$user_name, "'917867422669066240'='drhernandezcr'")
write.csv(CR2018,"C:/Elias/1 Serious/Academia/University of Pittsburgh/1 Dissertation/Data/CR2018Clean.csv", row.names = FALSE)


###
#Descriptives on tweets
###
setwd("C:/Elias/1 Serious/Academia/University of Pittsburgh/1 Dissertation/Data")
CR2018<- read.csv("CR2018Clean.csv", encoding = "UTF-8" )

#Number of tweets by user
tablenumbertweets<-table(CR2018$user_name)
tablenumbertweets

#number of likes
aggregate(CR2018$likes, by=list(Category=CR2018$user_name), FUN=sum)
#quotes
aggregate(CR2018$quotes, by=list(Category=CR2018$user_name), FUN=sum)
#replies
aggregate(CR2018$replies, by=list(Category=CR2018$user_name), FUN=sum)
#retweets
aggregate(CR2018$retweets, by=list(Category=CR2018$user_name), FUN=sum)


###
#Dataset of followers
###
#ojo que tuve que cambiar el encoding de todos los txt, no era utf-8 sino utf-16LE, pero esos no los lee
setwd("C:/Elias/1 Serious/Academia/University of Pittsburgh/1 Dissertation/Data/CR2018")
FsPAC<- read.delim("followers_accionciudadana.txt", header = FALSE, sep = "\t", dec = ".", encoding = "UTF-8" )
FsPAC$user_name<-'accionciudadana'
FsADC<- read.delim("followers_AlianzaDC.txt", header = FALSE, sep = "\t", dec = ".", encoding = "UTF-8" )
FsADC$user_name<-'AlianzaDC'
FsAAD<- read.delim("followers_alvarez_desanti.txt", header = FALSE, sep = "\t", dec = ".", encoding = "UTF-8" )
FsAAD$user_name<-'alvarez_desanti'
FsCAQ<- read.delim("followers_CarlosAlvQ.txt", header = FALSE, sep = "\t", dec = ".", encoding = "UTF-8" )
FsCAQ$user_name<-'CarlosAlvQ'
FsRH<- read.delim("followers_drhernandezcr.txt", header = FALSE, sep = "\t", dec = ".", encoding = "UTF-8" )
FsRH$user_name<-'drhernandezcr'
FsEA<- read.delim("followers_EdgardoArayaS.txt", header = FALSE, sep = "\t", dec = ".", encoding = "UTF-8" )
FsEA$user_name<-'EdgardoArayaS'
FsFaA<- read.delim("followers_FabriAlvarado7.txt", header = FALSE, sep = "\t", dec = ".", encoding = "UTF-8" )
FsFaA$user_name<-'FabriAlvarado7'
FsFA<- read.delim("followers_FrenteAmplio.txt", header = FALSE, sep = "\t", dec = ".", encoding = "UTF-8" )
FsFA$user_name<-'FrenteAmplio'
FsJDC<- read.delim("followers_JDiegoCastroCR.txt", header = FALSE, sep = "\t", dec = ".", encoding = "UTF-8" )
FsJDC$user_name<-'JDiegoCastroCR'
FsJV<- read.delim("followers_jvega_cr.txt", header = FALSE, sep = "\t", dec = ".", encoding = "UTF-8" )
FsJV$user_name<-'jvega_cr'
FsMR<- read.delim("followers_marioredondo.txt", header = FALSE, sep = "\t", dec = ".", encoding = "UTF-8" )
FsMR$user_name<-'marioredondo'
FsOG<- read.delim("followers_OttoGuevaraG.txt", header = FALSE, sep = "\t", dec = ".", encoding = "UTF-8" )
FsOG$user_name<-'OttoGuevaraG'
FsRP<- read.delim("followers_PizaRodolfo.txt", header = FALSE, sep = "\t", dec = ".", encoding = "UTF-8" )
FsRP$user_name<-'PizaRodolfo'
FsPLN<- read.delim("followers_plncr.txt", header = FALSE, sep = "\t", dec = ".", encoding = "UTF-8" )
FsPLN$user_name<-'plncr'
FsPT<- read.delim("followers_PTrabajadoresCR.txt", header = FALSE, sep = "\t", dec = ".", encoding = "UTF-8" )
FsPT$user_name<-'PTrabajadoresCR'
FsPRC<- read.delim("followers_renovacionprc.txt", header = FALSE, sep = "\t", dec = ".", encoding = "UTF-8" )
FsPRC$user_name<-'renovacionprc'
FsPRSC<- read.delim("followers_RepublicanoCR.txt", header = FALSE, sep = "\t", dec = ".", encoding = "UTF-8" )
FsPRSC$user_name<-'RepublicanoCR'
FsPRN<- read.delim("followers_restauracion_na.txt", header = FALSE, sep = "\t", dec = ".", encoding = "UTF-8" )
FsPRN$user_name<-'restauracion_na'
FsSM<- read.delim("followers_SergioMenaPNG.txt", header = FALSE, sep = "\t", dec = ".", encoding = "UTF-8" )
FsSM$user_name<-'SergioMenaPNG'
FsPUSC<- read.delim("followers_SocialUnidad.txt", header = FALSE, sep = "\t", dec = ".", encoding = "UTF-8" )
FsPUSC$user_name<-'SocialUnidad'
FsPIN<- read.delim("followers_SoyPINCR.txt", header = FALSE, sep = "\t", dec = ".", encoding = "UTF-8" )
FsPIN$user_name<-'SoyPINCR'
FsSC<- read.delim("followers_stephcamposofic.txt", header = FALSE, sep = "\t", dec = ".", encoding = "UTF-8" )
FsSC$user_name<-'stephcamposofic'
Followers<-rbind(FsPAC, FsADC, FsAAD, FsCAQ, FsRH, FsEA, FsFaA, FsFA, FsJDC, FsJV, FsMR, FsOG, FsRP, FsPLN, FsPT, FsPRC, FsPRSC, FsPRN, FsSM, FsPUSC, FsPIN, FsSC)
CRFollowers$author_id<-CRFollowers$user_name
CRFollowers$author_id<-car::recode(CRFollowers$author_id, "'accionciudadana'='19989379'")
CRFollowers$author_id<-car::recode(CRFollowers$author_id, "'CarlosAlvQ'='823245705439547392'") #***
CRFollowers$author_id<-car::recode(CRFollowers$author_id, "'AlianzaDC'='2587844821'")
CRFollowers$author_id<-car::recode(CRFollowers$author_id, "'marioredondo'='49201423'")
CRFollowers$author_id<-car::recode(CRFollowers$author_id, "'PTrabajadoresCR'='2194443991'")
CRFollowers$author_id<-car::recode(CRFollowers$author_id, "'jvega_cr'='2999633619'")
CRFollowers$author_id<-car::recode(CRFollowers$author_id, "'FrenteAmplio'='29837045'")
CRFollowers$author_id<-car::recode(CRFollowers$author_id, "'EdgardoArayaS'='78932537'")
CRFollowers$author_id<-car::recode(CRFollowers$author_id, "'SoyPINCR'='968159915360620544'")
CRFollowers$author_id<-car::recode(CRFollowers$author_id, "'JDiegoCastroCR'='252174687'")
CRFollowers$author_id<-car::recode(CRFollowers$author_id, "'plncr'='282256674'")
CRFollowers$author_id<-car::recode(CRFollowers$author_id, "'alvarez_desanti'='144583223'")
CRFollowers$author_id<-car::recode(CRFollowers$author_id, "'SergioMenaPNG'='2176923912'")
CRFollowers$author_id<-car::recode(CRFollowers$author_id, "'OttoGuevaraG'='2358603307'")
CRFollowers$author_id<-car::recode(CRFollowers$author_id, "'renovacionprc'='1569320167'")
CRFollowers$author_id<-car::recode(CRFollowers$author_id, "'stephcamposofic'='369143606'")
CRFollowers$author_id<-car::recode(CRFollowers$author_id, "'restauracion_na'='1037422095821664257'")
CRFollowers$author_id<-car::recode(CRFollowers$author_id, "'FabriAlvarado7'='1589859326'")
CRFollowers$author_id<-car::recode(CRFollowers$author_id, "'SocialUnidad'='1090371633766903809'")
CRFollowers$author_id<-car::recode(CRFollowers$author_id, "'PizaRodolfo'='721238259'")
CRFollowers$author_id<-car::recode(CRFollowers$author_id, "'RepublicanoCR'='828649770659041280'") #***
CRFollowers$author_id<-car::recode(CRFollowers$author_id, "'drhernandezcr'='917867422669066240'")
write.csv(CRFollowers,"C:/Elias/1 Serious/Academia/University of Pittsburgh/1 Dissertation/Data/CRFollowers.csv", row.names = FALSE)



###
#Get LAPOP information on internet usage
###
setwd("C:/Elias/1 Serious/Academia/Datasets/LAPOP")
LAPOP <- import ("2004-2018 LAPOP.dta")  #find out difference between 2004-2018 and 2009-2018
LAPOPdf<- data.frame(LAPOP)


#www1 and www1n: internet usage 
#smedia1 has facebook account
#smedia2 frequency viewing fb content
#smedia3 frequency viewing political content on fb
#smedia4 has twtter account
#smedia5 frequency viewing twitter content
#smedia6 frquency viewing political content on twitter
#smedia7 whatsapp accoutn
#smedia8 frquency viewing wa content
#smedia9 frequency viweing political content in wa
#as expected, insane ammounts of NA for the social media questions, it was only asked in 2018

#Explore the twitter questions
#they were only asked in 2018
LAPOP2018<-LAPOPdf %>%
  filter (year==2018 |  year==2019)
table_Twithas<- table (LAPOP2018$pais, LAPOP2018$smedia4, exclude = NULL)
table_Twithas
table_TwitPol<- table (LAPOP2018$pais, LAPOP2018$smedia6, exclude=NULL)
table_TwitPol

LAPOP2018$smedia4<-car::recode (LAPOP2018$smedia4, "2=0") #recode so no account is 0
LAPOP2018$smedia4<- as.factor(LAPOP2018$smedia4) #also needs to be factor

LAPOP2018$pais<-as.factor(LAPOP2018$pais) #pais has to be factor
#####
#Get all the country names
LAPOP2018$pais<-car::recode (LAPOP2018$pais, "'1'= 'MEX'")
LAPOP2018$pais<-car::recode (LAPOP2018$pais, "'2'= 'GUA'")
LAPOP2018$pais<-car::recode (LAPOP2018$pais, "'3'= 'SAL'")
LAPOP2018$pais<-car::recode (LAPOP2018$pais, "'4'= 'HON'")
LAPOP2018$pais<-car::recode (LAPOP2018$pais, "'5'= 'NIC'")
LAPOP2018$pais<-car::recode (LAPOP2018$pais, "'6'= 'CR'")
LAPOP2018$pais<-car::recode (LAPOP2018$pais, "'7'= 'PAN'")
LAPOP2018$pais<-car::recode (LAPOP2018$pais, "'8'= 'COL'")
LAPOP2018$pais<-car::recode (LAPOP2018$pais, "'9'= 'ECU'")
LAPOP2018$pais<-car::recode (LAPOP2018$pais, "'10'= 'BOL'")
LAPOP2018$pais<-car::recode (LAPOP2018$pais, "'11'= 'PER'")
LAPOP2018$pais<-car::recode (LAPOP2018$pais, "'12'= 'PAR'")
LAPOP2018$pais<-car::recode (LAPOP2018$pais, "'13'= 'CHI'")
LAPOP2018$pais<-car::recode (LAPOP2018$pais, "'14'= 'URU'")
LAPOP2018$pais<-car::recode (LAPOP2018$pais, "'15'= 'BRA'")
LAPOP2018$pais<-car::recode (LAPOP2018$pais, "'16'= 'VEN'")
LAPOP2018$pais<-car::recode (LAPOP2018$pais, "'17'= 'ARG'")
LAPOP2018$pais<-car::recode (LAPOP2018$pais, "'21'= 'DOM'")
LAPOP2018$pais<-car::recode (LAPOP2018$pais, "'23'= 'JAM'")
LAPOP2018$pais<-car::recode (LAPOP2018$pais, "'40'= 'USA'")
LAPOP2018$pais<-car::recode (LAPOP2018$pais, "'41'= 'CAN'")
#####
table (LAPOP2018$pais)

#drop all cuntries but CA, exclude Nicaragua. Keep in mind I named it LA but it's actually about central america
LAPOP2018LA<-LAPOP2018%>%
  filter (pais=='GUA'| pais=='SAL'| pais=='HON'| pais =='CR')

#build easier to understand variables for crosstabs
#age grouos
LAPOP2018LA$agegroup<-LAPOP2018LA$q2
LAPOP2018LA$agegroup<-car::recode (LAPOP2018LA$agegroup, "c('0','1','2','3','4','5','6','7','8','9','10')='0-10'")
LAPOP2018LA$agegroup<-car::recode (LAPOP2018LA$agegroup, "c('11','12','13','14','15','16','17','18','19','20')='11-20'")
LAPOP2018LA$agegroup<-car::recode (LAPOP2018LA$agegroup, "c('21','22','23','24','25','26','27','28','29','30')='21-30'")
LAPOP2018LA$agegroup<-car::recode (LAPOP2018LA$agegroup, "c('31','32','33','34','35','36','37','38','39','40')='31-40'")
LAPOP2018LA$agegroup<-car::recode (LAPOP2018LA$agegroup, "c('41','42','43','44','45','46','47','48','49','50')='41-50'")
LAPOP2018LA$agegroup<-car::recode (LAPOP2018LA$agegroup, "c('51','52','53','54','55','56','57','58','59','60')='51-60'")
LAPOP2018LA$agegroup<-car::recode (LAPOP2018LA$agegroup, "c('61','62','63','64','65','66','67','68','69','70')='61-70'")
LAPOP2018LA$agegroup<-car::recode (LAPOP2018LA$agegroup, "c('71','72','73','74','75','76','77','78','79','80')='71-80'")
LAPOP2018LA$agegroup<-car::recode (LAPOP2018LA$agegroup, "c('81','82','83','84','85','86','87','88','89','90')='81-90'")
LAPOP2018LA$agegroup<-car::recode (LAPOP2018LA$agegroup, "c('91','92','93','94','95','96','97','98','99','100')='91-100'")
table (LAPOP2018LA$agegroup, LAPOP2018LA$q2) #to make sure the recoding worked


#per country
prop.table (table(LAPOP2018LA$pais, LAPOP2018LA$smedia4), 1) 
#age
prop.table(table (LAPOP2018LA$agegroup, LAPOP2018LA$smedia4),2)
#this commands allows for a table of proportion
#prop.table (,1) divides for the value of each row, 2 fo cell; no extra parameter is by total value

#educational level ed
LAPOP2018LA$educlvl<-LAPOP2018LA$ed
LAPOP2018LA$educlvl<-car::recode (LAPOP2018LA$educlvl, "'0'='None'")
LAPOP2018LA$educlvl<-car::recode (LAPOP2018LA$educlvl, "c('1','2','3','4','5','6')='Primary'")
LAPOP2018LA$educlvl<-car::recode (LAPOP2018LA$educlvl, "c('7','8','9','10', '11', '12')='Secondary'")
LAPOP2018LA$educlvl<-car::recode (LAPOP2018LA$educlvl, "c('13','14','15','16','17','18')='Post-secondary'")
table (LAPOP2018LA$educlvl, LAPOP2018LA$ed)
#to make sure the recoding worked
prop.table(table (LAPOP2018LA$educlvl, LAPOP2018LA$smedia4),2)

#partisanship, 1 is has a partisan ID, 2 is no
prop.table(table(LAPOP2018LA$vb10, LAPOP2018LA$smedia4),2)

#trust in institutions
prop.table(table(LAPOP2018LA$aut1, LAPOP2018LA$smedia4),2)

#rural-urban. 1=urban, 2=rural
LAPOP2018LA$ur<- as.factor(LAPOP2018LA$ur) #also needs to be factor
prop.table(table(LAPOP2018LA$ur, LAPOP2018LA$smedia4),2)

#men-women
LAPOP2018LA$q1<- as.factor(LAPOP2018LA$q1) #also needs to be factor
prop.table(table(LAPOP2018LA$q1, LAPOP2018LA$smedia4),2)


#generalized trust, not that intersting
LAPOP2018LA$it1<- as.factor(LAPOP2018LA$it1) #also needs to be factor
prop.table(table(LAPOP2018LA$it1, LAPOP2018LA$smedia4),2)

#ideology l1
LAPOP2018LA$l1<- as.factor(LAPOP2018LA$l1) #also needs to be factor
prop.table(table(LAPOP2018LA$l1, LAPOP2018LA$smedia4),2)



#Percentage of ppl with twitter account
#to do a percentage bars graphic I need to mutate the df as follows
LAPOP2018LA1<-LAPOP2018LA%>%
  group_by(pais)%>%
  count(smedia4)%>%
  mutate(percentage=(n/sum(n))*100)

ggplot(LAPOP2018LA1, aes(x=pais, y=percentage))+
  geom_col (aes(fill=smedia4), position="fill", show.legend=FALSE)+
  labs (title="Figure 1. Percentage of Central Americans with twitter accounts, 2018-2019",
        x="Country")+
  scale_y_continuous(labels = scales::percent) +
  theme_classic()
#if you look at the df LAPOP2018LA1, you can see the percentages


table (LAPOP2018LA$smedia4, LAPOP2018LA$b2) #trust in institutions
table (LAPOP2018LA$smedia4, LAPOP2018LA$it1) #it1 generalized trust
table (LAPOP2018LA$smedia4, LAPOP2018LA$) #q1 ??
table (LAPOP2018LA$smedia4, LAPOP2018LA$) #e5 
table (LAPOP2018LA$smedia4, LAPOP2018LA$) #e15
table (LAPOP2018LA$smedia4, LAPOP2018LA$q10) #q10new income

#of those with a twitter account, how often do they see content
#####
LAPOP2018LA$smedia5<-as.factor(LAPOP2018LA$smedia5)
#recoding for ease of interpretetion
LAPOP2018LA$smedia5<-car::recode (LAPOP2018LA$smedia5, "'1'= '1. Daily'")
LAPOP2018LA$smedia5<-car::recode (LAPOP2018LA$smedia5, "'2'= '2. Weekly'")
LAPOP2018LA$smedia5<-car::recode (LAPOP2018LA$smedia5, "'3'= '3. Monthly'")
LAPOP2018LA$smedia5<-car::recode (LAPOP2018LA$smedia5, "'4'= '4. Yearly'")
LAPOP2018LA$smedia5<-car::recode (LAPOP2018LA$smedia5, "'5'= '5. Never'")
#####

LAPOP2018LA2<-LAPOP2018LA%>%
  filter(!is.na(smedia5))%>%
  group_by(pais)%>%
  count(smedia5)%>%
  mutate(percentage=(n/sum(n))*100)

ggplot(LAPOP2018LA2, aes(x=pais, y=percentage))+
  geom_col (aes(fill=forcats::fct_rev(smedia5)), position="fill", show.legend=TRUE)+
  labs (#title="Figure 2. How often Central Americans with an account see content on twitter, 2018-2019",
        x="Country",
        fill="Frequency" )+
  scale_y_continuous(labels = scales::percent) +
  theme_classic()

#Of those with a twitter account, how often they see political information
#####
LAPOP2018LA$smedia6<-as.factor(LAPOP2018LA$smedia6)
#recoding for ease of interpretetion
LAPOP2018LA$smedia6<-car::recode (LAPOP2018LA$smedia6, "'1'= '1. Daily'")
LAPOP2018LA$smedia6<-car::recode (LAPOP2018LA$smedia6, "'2'= '2. Weekly'")
LAPOP2018LA$smedia6<-car::recode (LAPOP2018LA$smedia6, "'3'= '3. Monthly'")
LAPOP2018LA$smedia6<-car::recode (LAPOP2018LA$smedia6, "'4'= '4. Yearly'")
LAPOP2018LA$smedia6<-car::recode (LAPOP2018LA$smedia6, "'5'= '5. Never'")
#####

LAPOP2018LA3<-LAPOP2018LA%>%
  filter(!is.na(smedia6))%>%
  group_by(pais)%>%
  count(smedia6)%>%
  mutate(percentage=(n/sum(n))*100)

ggplot(LAPOP2018LA3, aes(x=pais, y=percentage))+
  geom_col (aes(fill=forcats::fct_rev(smedia6)), position="fill", show.legend=TRUE)+
  labs (#title="Figure 3. How often Central Americans with an account see political information on twitter, 2018-2019",
        x="Country",
        fill="Frequency")+
  scale_y_continuous(labels = scales::percent) +
  theme_classic()


#Once done, also get FB and WA


###
#Database of tweets, 2018 parties and candidates, tweets taken on april 4
###

setwd("C:/Users/elias")
df_2018<-read.csv("1377358976896106496.csv", encoding = "UTF-8")

table (df_2018$user)

###
#Party positions based on manifestos and pela positions
###

#RILE from PEN manifesto data
setwd("C:/Elias/REVISAR")

Programas_Gobierno_Read <- read.csv("Completa_Programas2018BBDD_v23-7-2018.csv", sep = ";")
Programas_Gobierno_Data <- data.frame(Programas_Gobierno_Read)

Subset_Cartesiano <- select(Programas_Gobierno_Data, c(GEN4, MAT, POST_MAT))

#Atemcions, MAT es 1=capitalista, 0=estadista
#POST_MAT es 1=progre, 0=Conservador
Subset_Cartesiano$RILE <-(Subset_Cartesiano$MAT+(1-Subset_Cartesiano$POST_MAT))/2
#Ahora, RILE es 0=izquiersa, 1=derecha

RILE<-Subset_Cartesiano%>%
  filter(RILE!=0.5)%>%
  group_by(GEN4)%>%
  summarize(meanRILe=mean(RILE,na.rm=T))

#Ok, now PELA
setwd("C:/Elias/1 Serious/Academia/Datasets/PELA/CR data")
CR2018<-read.csv("BASEDATOS_COSTARICA_108.csv")
#partido para el partido, los codigos son
# 1=PLN, 2=pusc, 3=pac, 4=rn, 5=otros
CR2018$partido<-recode(CR2018$partido, "'1'= 'PLN'")
CR2018$partido<-recode(CR2018$partido, "'2'= 'PUSC'")
CR2018$partido<-recode(CR2018$partido, "'3'= 'PAC'")
CR2018$partido<-recode(CR2018$partido, "'4'= 'RN'")
CR2018$partido<-recode(CR2018$partido, "'5'= 'Otros'")
#ID2 es l-r para el partido, 1 es izquieda, 10 es derecha

lr<-CR2018%>%
  group_by(partido)%>%
  summarize(partyid=mean(ID2, na.rm=T))


#nada que ver con esto, revision r·pida para Scott

DataCr<-LAPOPdf %>%
  filter (pais==6)

tablevb10<-table(DataCr$year, DataCr$vb10, exclude=NULL)
prop.table(tablevb10, 1)


tablevb11_06<-table(DataCr$vb11_06)
prop.table(tablevb11_06)

tablevb11_08<-table(DataCr$vb11_08)
prop.table(tablevb11_08)

tablevb11_10<-table(DataCr$vb11_10)
prop.table(tablevb11_10)

tablevb11_12<-table(DataCr$vb11_12)
prop.table(tablevb11_12)

tablevb11_14<-table(DataCr$vb11_14)
prop.table(tablevb11_14)

tablevb11_16<-table(DataCr$vb11_16)
prop.table(tablevb11_16)

tablevb11_18<-table(DataCr$vb11_18)
prop.table(tablevb11_18)


####
#UNITY scores, roll call
####
setwd("C:/Elias/1 Serious/Arbeit/PEN/BBDDNOMINATE")
ROLLCALL<- read.csv("BBDD Voto nominal PEN Legislatura 2018-2019.csv", encoding = "UTF-8" )

#wUNITYi = SUM(%aye-%nay)ij*(1-%missing)j*(1-%aye-%nay)j)/SUM((1-%missing)j*(1-%aye-%nay)j)  


ROLLCALL2<-ROLLCALL%>%      
  mutate(ExpDeb = paste(Expediente, TIPO_DEB, NUM_SES, sep="_")) %>% 
  group_by(ExpDeb)%>%                 #group by project j
  count(VOTO)%>%                          #count number of ayes, nays and missed votes
  mutate(percentage=(n/sum(n))*100)       #get % for each of the prior  

ROLLCALL3<-spread(ROLLCALL2, VOTO, percentage) #spread to get a dataframe with project, %aye, %nays, %missed votes
ROLLCALL3[is.na(ROLLCALL3)] <- 0    #replace na's with zeros

ROLLCALL4<-aggregate(cbind(ROLLCALL3$No, ROLLCALL3$`No votÛ`, ROLLCALL3$SÌ), by=list(Expediente=ROLLCALL3$ExpDeb), FUN=sum)
#the last one creates a single row for wach project
#Queda V1 es No, V2 es no voto, V3 es sÌ

ROLLCALL5<-ROLLCALL4%>%      #This df has values for the final formula
  mutate(ATTEND=1-V2,
         CLOSE=1-V3-V1)

ROLLCALL6<-ROLLCALL%>%      
  mutate(ExpDeb = paste(Expediente, TIPO_DEB, NUM_SES, sep="_")) %>% 
  group_by(ExpDeb, FRACCI”N)%>%
  count(VOTO)%>%                         
  mutate(percentage=(n/sum(n))*100) 

ROLLCALL7<-spread(ROLLCALL6, VOTO, percentage) 
ROLLCALL7[is.na(ROLLCALL7)] <- 0   

ROLLCALL8<-aggregate(cbind(ROLLCALL7$No, ROLLCALL7$`No votÛ`, ROLLCALL7$SÌ), by=list(Expediente=ROLLCALL7$ExpDeb, FRACCI”N=ROLLCALL7$FRACCI”N), FUN=sum)

ROLLCALL9<-ROLLCALL8%>%  #This df has values for the formula. It's the important one, need to add the other
  mutate(UNITY=V3-V1)


UNITYdf<-merge(ROLLCALL9, ROLLCALL5,by="ExpDeb")

UNITYdf2<-UNITYdf%>%
  mutate(NUMERADOR= UNITY*ATTEND*CLOSE,
         DENOMINADOR=ATTEND*CLOSE)

WUNITYdf<-UNITYdf2%>%
  group_by(FRACCI”N)%>%
  mutate(WUNITY=(sum(NUMERADOR)/sum(DENOMINADOR))/100)

table(WUNITYdf$FRACCI”N, WUNITYdf$WUNITY)

#solo unity
WUNITYdf3<-UNITYdf2%>%
  group_by(FRACCI”N)%>%
  mutate(WUNITY=(sum(UNITY))/100)
table(WUNITYdf3$FRACCI”N, WUNITYdf3$WUNITY)

###
#el grafico
###
setwd("C:/Elias/1 Serious/Arbeit/PEN/BBDDNOMINATE")
UNITYsc<- read.csv("unitySCORES.csv", encoding = "UTF-8" )

UNITYplot<-ggplot (data=UNITYsc, aes(y=reorder(factor(X.U.FEFF.Partido), W.UNITY)))+
  geom_point(aes(x=W.UNITY, size=1.5, shape=15))+
  scale_shape_identity()+
  theme(axis.text.x = element_text(angle=0, hjust = .5)) +
  theme(panel.grid.major.y = element_line(colour = "grey95")) +
  theme(legend.key = element_rect(fill = "white")) +
  theme(panel.background = element_rect(fill = "white")) +
  theme(axis.line.x = element_line(colour = "black")) +
  theme(axis.line.y = element_line(colour = "black")) +
  theme(axis.title = element_text(size=10)) + #es para disminuir el tamaÒo del label. ojo que hace x & y simultaneamente
  guides(fill = FALSE) +
  coord_cartesian(xlim =c(0, 1)) +     #change the limits of the x axis
  theme(legend.position = "none")+ #take out the legend
  ylab("FracciÛn") #cambia el label de eje y

  
  #quitar leyenda, quitar escala de y


jpeg("UNITYCR2018.jpeg", units="px", width=3000, height=2000, res=400)
UNITYplot
dev.off()

#version svg
svg("UNITYCR2018.svg", width=15, height=10)
UNITYplot
dev.off()



###
##No es diss, pero bueno, aca va NOMINATE
###
setwd("C:/Elias/1 Serious/Arbeit/PEN/BBDDNOMINATE")
ROLLCALL<- read.csv("BBDD Voto nominal PEN Legislatura 2018-2019.csv", encoding = "UTF-8" )

ROLLCALL2<-ROLLCALL%>%
  mutate(ExpDeb = paste(Expediente, TIPO_DEB, NUM_SES, sep="_"))  #asi, btengo cuantos expedientes se votaron, tnato en primer como en segundo debate
expedientes=table(ROLLCALL2$ExpDeb)
expedientes

ROLLCALL2$VOTO<-recode(ROLLCALL2$VOTO, "'SÌ'= '1'")
ROLLCALL2$VOTO<-recode(ROLLCALL2$VOTO, "'No'= '2'")
ROLLCALL2$VOTO<-recode(ROLLCALL2$VOTO, "'No votÛ'= '9'")
table

ROLLCALL2$FRACCI”N2<-ROLLCALL2$FRACCI”N #FRACCION2 va a ser usada para hacer nominate, voy a usarla para unir a los de NR con RN
ROLLCALL2$FRACCI”N2 <-recode (ROLLCALL2$FRACCI”N2, "'PNR' = 'PRN'")

#OJO!!! los siguientes diputados salen repetidos pq tienen un espacio al final del nombre
ROLLCALL2$DIPUTADO<-recode(ROLLCALL2$DIPUTADO, "'Franggi Nicol·s Solano '='Franggi Nicol·s Solano'")
ROLLCALL2$DIPUTADO<-recode(ROLLCALL2$DIPUTADO, "'MarÌa InÈs SolÌs QuirÛs '='MarÌa InÈs SolÌs QuirÛs'")



#necesito un "rollcall object". usualmente ocupa sacarlo pero dado que aca tengo en df, solo voy a transformarlo
#para el rollcall object, la matriz debe de manipularse tal que sea legislators x vote, en este caso, DIPUTADO x ExpDeb
ROLLCALL3 <- ROLLCALL2 %>% 
  select (DIPUTADO, FRACCI”N2, VOTO, ExpDeb) %>%  #solo estas variables
  spread (ExpDeb, VOTO) %>% #hace la matriz una de DIPUTADO x votacion
  group_by (DIPUTADO)


#Aqui ya esta convertida en una matriz de legisladores x votacion, 1 es aye, 2 es nay, 9 es ausente
#now i need to create a rollcall object via the package pscl

#first, I need to extract vectors of legislators and party membership, then delete from the original matrix so it is only a matrix of votes. The party variable must be rolled into a matrix for inclusion in the rollcall object
ROLLCALL3<-as.matrix(ROLLCALL3) #OJO, ES FUNDAMENTAL tenerlo como matriz, como DF no sirve

#ROLLCALL3<-ROLLCALL3 %>%
#  rename(legis.names=DIPUTADO) #TIENE que hacer este rename, sino rc no lo lee para el rc object 
DiputadosNombres<-ROLLCALL3[,1] #hago vector de nombre de diputados, se ocupa aparte
DipData<-matrix(ROLLCALL3[,2], length (ROLLCALL3[,2]),1) #hago una MATRIZ de nombre de partidos, se ocupa aparte 
colnames(DipData)<-"party" #nombre para la columna de la matriz
ROLLCALL4<-ROLLCALL3[,-c(1,2)] #aqui saco diputados y partidos, que queden solo votos


#Now, to create the rollcall object
rc<-rollcall(ROLLCALL4, yea=1, nay=2, missing=9, notInLegis = 0, legis.names = DiputadosNombres, legis.data = DipData)


#Problema en este momento: legis.names y legis.data no los logra empatar aunque tienen igual numero de filas

#that's the rollcall object, yea y nay are self-explanatory, missing means ABSTENTION, notInLegis are NAs, legis.names and legis.data 
#we creted in the prior step
results<-wnominate(rc, minvotes = 1, lop=0, polarity = c(14, 14), verbose=T) #14 es steller, 23 es jon prendas. 25 es villalta
#It drops too many votes, why?????
#polarity se usa para orientar el grafico, basicamente ocupa que el 1er elemento es un legisldor conservador en la primera dimension, y el 2do, un legislador conservador en la 2da

summary(results)
warnings()
plot(results)

#dentro de ploot, coords es el grafico estandar de NOMINATE de espacio ideologico, skree muesta la cantidad de eigenvalues que ademas indican
#l a cantidad de dimensiones relevante, 

help(plot.coords)
help(symbols)
options(OutDec= ",") #cambia los puntos decimales por comas decimales

jpeg("NOMINATECR2018.jpeg", units="px", width=3000, height=2000, res=400)
plot.coords(results, main.title="", d1.title="Primera dimensiÛn", d2.title="Segunda dimensiÛn", Leyend=T, legend.x = 1.3, legend.y = 1, cex.lab=1)
dev.off()
#solo ojo, en opciones de nominate, d1.title etc es para cual va a ser el label de los eje, leyend indica que haya leyenda, legend.x y legen.y dinde va en el grafico, cex.lab es el tmaaÒo de eletra de titulo de ejes. 

svg("NOMINATECR2018.svg", width=15, height=10)
plot.coords(results, main.title="", d1.title="Primera dimensiÛn", d2.title="Segunda dimensiÛn", Leyend=T, legend.x = 1.3, legend.y = 1, cex.lab=1)
dev.off()


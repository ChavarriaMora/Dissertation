#Elias Chavarria-Mora
#Dissertation
#This is code for joining the individual country-election databases, as well as creating new variables based on the 
#LIWC analysis


library (tidyverse) #includes dplyr, tidyr, ggplot2, stringr
library(rio) #for import
library (gridExtra)
library(car) #for recode
library (tidytext) #text as data manipulation with tidyverse philosophy

#####
#Joining all the individual datasets, BEFORE LIWC. Important because LIWC can screw up the encoding pretty bad
setwd("C:/Elias/1 Serious/Academia/University of Pittsburgh/1 Dissertation/Data/ElectionsTweets")

CR2010 <-import('df_CR2010.csv') 
CR2010<- CR2010 %>%
  mutate (Country = "Costa Rica",
          .after=V1)
CR2010<- CR2010 %>%
  mutate (Election = "2010",
          .after=Country)

CR2014 <-import('df_CR2014.csv') 
CR2014<- CR2014 %>%
  mutate (Country = "Costa Rica",
          .after=V1)
CR2014<- CR2014 %>%
  mutate (Election = "2014",
          .after=Country)

CR2018 <-import('df_CR2018.csv') 
CR2018<- CR2018 %>%
  mutate (Country = "Costa Rica",
          .after=V1)
CR2018<- CR2018 %>%
  mutate (Election = "2018",
          .after=Country)

CR2022 <-import('df_CR2022.csv') 
CR2022<- CR2022 %>%
  mutate (Country = "Costa Rica",
          .after=V1)
CR2022<- CR2022 %>%
  mutate (Election = "2022",
          .after=Country)

ES2014 <-import('df_ES2014.csv') 
ES2014<- ES2014 %>%
  mutate (Country = "El Salvador",
          .after=V1)
ES2014<- ES2014 %>%
  mutate (Election = "2014",
          .after=Country)

ES2019 <-import('df_ES2019.csv') 
ES2019<- ES2019 %>%
  mutate (Country = "El Salvador",
          .after=V1)
ES2019<- ES2019 %>%
  mutate (Election = "2019",
          .after=Country)

GUA2011 <-import('df_Guatemala2011.csv') 
GUA2011<- GUA2011 %>%
  mutate (Country = "Guatemala",
          .after=V1)
GUA2011<- GUA2011 %>%
  mutate (Election = "2011",
          .after=Country)

GUA2015 <-import('df_Guatemala2015.csv') 
GUA2015<- GUA2015 %>%
  mutate (Country = "Guatemala",
          .after=V1)
GUA2015<- GUA2015 %>%
  mutate (Election = "2015",
          .after=Country)

GUA2019 <-import('df_Guatemala2019.csv') 
GUA2019<- GUA2019 %>%
  mutate (Country = "Guatemala",
          .after=V1)
GUA2019<- GUA2019 %>%
  mutate (Election = "2019",
          .after=Country)

HO2013 <-import('df_HO2013.csv') 
HO2013<- HO2013 %>%
  mutate (Country = "Honduras",
          .after=V1)
HO2013<- HO2013 %>%
  mutate (Election = "2013",
          .after=Country)

HO2017 <-import('df_HO2017.csv') 
HO2017<- HO2017 %>%
  mutate (Country = "Honduras",
          .after=V1)
HO2017<- HO2017 %>%
  mutate (Election = "2017",
          .after=Country)

HO2021 <-import('df_HO2021.csv') 
HO2021<- HO2021 %>%
  mutate (Country = "Honduras",
          .after=V1)
HO2021<- HO2021 %>%
  mutate (Election = "2021",
          .after=Country)

df<-rbind(CR2010, CR2014, CR2018, CR2022, ES2014, ES2019, GUA2011, GUA2015, GUA2019, HO2013, HO2017, HO2021)

#some extra prep
#create a country-election variable
df$CountryElection<-paste(df$Country, df$Election)
#and a country-election-username
df$CountryElectionUsername<-paste(df$Country, df$Election, df$user_username)
#I need to transform the "created at" variable from string to a data
df$created_at <- as.Date(df$created_at)

##### 
#party or candidate variable
df$type_of_account<-df$user_username
table(df$type_of_account)
df$type_of_account<-car::recode(df$type_of_account, "'_FredyCabrera'='candidate'")
df$type_of_account<-car::recode(df$type_of_account, "'accionciudadana'='party'")
df$type_of_account<-car::recode(df$type_of_account, "'AdelaTorrebiart'='candidate'")
df$type_of_account<-car::recode(df$type_of_account, "'AlianzaDC'='party'")
df$type_of_account<-car::recode(df$type_of_account, "'alvarez_desanti'='candidate'")
df$type_of_account<-car::recode(df$type_of_account, "'AnibalGarciaGT'='candidate'")
df$type_of_account<-car::recode(df$type_of_account, "'ARENAOFICIAL'='party'")
df$type_of_account<-car::recode(df$type_of_account, "'CarlosAlvQ'='candidate'")
df$type_of_account<-car::recode(df$type_of_account, "'CetoPablo'='candidate'")
df$type_of_account<-car::recode(df$type_of_account, "'creoguate'='party'")
df$type_of_account<-car::recode(df$type_of_account, "'criverapaniagua'='candidate'")
df$type_of_account<-car::recode(df$type_of_account, "'Cruickshankedu'='candidate'")
df$type_of_account<-car::recode(df$type_of_account, "'democraciasal'='party'")
df$type_of_account<-car::recode(df$type_of_account, "'diputado_dr'='candidate'")
df$type_of_account<-car::recode(df$type_of_account, "'DiputadosPac'='party'")
df$type_of_account<-car::recode(df$type_of_account, "'Dr_MDGarcia'='candidate'")
df$type_of_account<-car::recode(df$type_of_account, "'DrGiammattei'='candidate'")
df$type_of_account<-car::recode(df$type_of_account, "'DrHCaballeros'='candidate'")
df$type_of_account<-car::recode(df$type_of_account, "'drhernandezcr'='candidate'")
df$type_of_account<-car::recode(df$type_of_account, "'DrRoulan'='candidate'")
df$type_of_account<-car::recode(df$type_of_account, "'EdgardoArayaS'='candidate'")
df$type_of_account<-car::recode(df$type_of_account, "'Edmondmulet'='candidate'")
df$type_of_account<-car::recode(df$type_of_account, "'elifeinzaig'='candidate'")
df$type_of_account<-car::recode(df$type_of_account, "'elmenosmalo'='candidate'")
df$type_of_account<-car::recode(df$type_of_account, "'EncuentroGuate'='party'")
df$type_of_account<-car::recode(df$type_of_account, "'escoto_marlon'='candidate'")
df$type_of_account<-car::recode(df$type_of_account, "'EsdrasALopez'='candidate'")
df$type_of_account<-car::recode(df$type_of_account, "'eugeniotrejos'='candidate'")
df$type_of_account<-car::recode(df$type_of_account, "'FabriAlvarado7'='candidate'")
df$type_of_account<-car::recode(df$type_of_account, "'FedeMalavassiC'='candidate'")
df$type_of_account<-car::recode(df$type_of_account, "'figuerescr'='candidate'")
df$type_of_account<-car::recode(df$type_of_account, "'Fishman2010'='candidate'")
df$type_of_account<-car::recode(df$type_of_account, "'FMLNDeptalSS'='party'")
df$type_of_account<-car::recode(df$type_of_account, "'FMLNoficial'='party'")
df$type_of_account<-car::recode(df$type_of_account, "'FrenteAmplio'='party'")
df$type_of_account<-car::recode(df$type_of_account, "'FUERZAGUATEMALA'='party'")
df$type_of_account<-car::recode(df$type_of_account, "'Galdamez_Kaibil'='candidate'")
df$type_of_account<-car::recode(df$type_of_account, "'GANAOFICIAL'='party'")
df$type_of_account<-car::recode(df$type_of_account, "'gt_podemos'='party'")
df$type_of_account<-car::recode(df$type_of_account, "'GtMlp'='party'")
df$type_of_account<-car::recode(df$type_of_account, "'GuatemalaAVANZA'='party'")
df$type_of_account<-car::recode(df$type_of_account, "'HondurasHumana'='party'")
df$type_of_account<-car::recode(df$type_of_account, "'HugoMartinezSV'='candidate'")
df$type_of_account<-car::recode(df$type_of_account, "'Humanista_guate'='party'")
df$type_of_account<-car::recode(df$type_of_account, "'IsaacFarchi'='candidate'")
df$type_of_account<-car::recode(df$type_of_account, "'jccalleja'='candidate'")
df$type_of_account<-car::recode(df$type_of_account, "'JDiegoCastroCR'='candidate'")
df$type_of_account<-car::recode(df$type_of_account, "'jimmymoralesgt'='candidate'")
df$type_of_account<-car::recode(df$type_of_account, "'jmCorrales2014'='candidate'")
df$type_of_account<-car::recode(df$type_of_account, "'jmechandi'='candidate'")
df$type_of_account<-car::recode(df$type_of_account, "'Johnny_Araya'='candidate'")
df$type_of_account<-car::recode(df$type_of_account, "'josemvillalta'='candidate'")
df$type_of_account<-car::recode(df$type_of_account, "'josuealvaradosv'='candidate'")
df$type_of_account<-car::recode(df$type_of_account, "'JuanOrlandoH'='candidate'")
df$type_of_account<-car::recode(df$type_of_account, "'JulioHectorE'='candidate'")
df$type_of_account<-car::recode(df$type_of_account, "'jvega_cr'='candidate'")
df$type_of_account<-car::recode(df$type_of_account, "'Laura_Ch'='candidate'")
df$type_of_account<-car::recode(df$type_of_account, "'liberalcr'='party'")
df$type_of_account<-car::recode(df$type_of_account, "'LIDEHR1'='party'")
df$type_of_account<-car::recode(df$type_of_account, "'LinethSaborio'='candidate'")
df$type_of_account<-car::recode(df$type_of_account, "'lizarsosa'='candidate'")
df$type_of_account<-car::recode(df$type_of_account, "'luisguillermosr'='candidate'")
df$type_of_account<-car::recode(df$type_of_account, "'LuisVelasquezQ'='candidate'")
df$type_of_account<-car::recode(df$type_of_account, "'luiszelaya_hn'='candidate'")
df$type_of_account<-car::recode(df$type_of_account, "'ManfredoGuate'='candidate'")
df$type_of_account<-car::recode(df$type_of_account, "'ManuelBaldizon'='candidate'")
df$type_of_account<-car::recode(df$type_of_account, "'marioredondo'='candidate'")
df$type_of_account<-car::recode(df$type_of_account, "'MarleneAlv_'='candidate'")
df$type_of_account<-car::recode(df$type_of_account, "'MarlonEscotoTSH'='candidate'")
df$type_of_account<-car::recode(df$type_of_account, "'MartinChCastro'='candidate'")
df$type_of_account<-car::recode(df$type_of_account, "'MiltonBeniteztv'='candidate'")
df$type_of_account<-car::recode(df$type_of_account, "'MNRguatemala'='party'")
df$type_of_account<-car::recode(df$type_of_account, "'MovimientoWinaq'='party'")
df$type_of_account<-car::recode(df$type_of_account, "'MVillacortaOf'='candidate'")
df$type_of_account<-car::recode(df$type_of_account, "'Natdiaquin'='candidate'")
df$type_of_account<-car::recode(df$type_of_account, "'nayibbukele'='candidate'")
df$type_of_account<-car::recode(df$type_of_account, "'norman_quijano'='candidate'")
df$type_of_account<-car::recode(df$type_of_account, "'OrleSols'='candidate'")
df$type_of_account<-car::recode(df$type_of_account, "'oscardelcampo'='candidate'")
df$type_of_account<-car::recode(df$type_of_account, "'oscarlopez_cr'='candidate'")
df$type_of_account<-car::recode(df$type_of_account, "'OttoGuevaraG'='candidate'")
df$type_of_account<-car::recode(df$type_of_account, "'ottoperezmolina'='candidate'")
df$type_of_account<-car::recode(df$type_of_account, "'p_alianza'='party'")
df$type_of_account<-car::recode(df$type_of_account, "'p_unionista'='party'")
df$type_of_account<-car::recode(df$type_of_account, "'PANResponde'='party'")
df$type_of_account<-car::recode(df$type_of_account, "'Partido_VIVA'='party'")
df$type_of_account<-car::recode(df$type_of_account, "'PartidoCRJ'='party'")
df$type_of_account<-car::recode(df$type_of_account, "'PartidoFuerzaN2'='party'")
df$type_of_account<-car::recode(df$type_of_account, "'PartidoLibre'='party'")
df$type_of_account<-car::recode(df$type_of_account, "'PARTIDOPASE'='party'")
df$type_of_account<-car::recode(df$type_of_account, "'PartidoPuebloU1'='party'")
df$type_of_account<-car::recode(df$type_of_account, "'PartidoSalvador'='party'")
df$type_of_account<-car::recode(df$type_of_account, "'partidounidosgt'='party'")
df$type_of_account<-car::recode(df$type_of_account, "'PCN_OFICIAL'='party'")
df$type_of_account<-car::recode(df$type_of_account, "'Pinusd_HN'='party'")
df$type_of_account<-car::recode(df$type_of_account, "'Piza2022'='candidate'")
df$type_of_account<-car::recode(df$type_of_account, "'PLHonduras'='party'")
df$type_of_account<-car::recode(df$type_of_account, "'plncr'='party'")
df$type_of_account<-car::recode(df$type_of_account, "'PNH_oficial'='party'")
df$type_of_account<-car::recode(df$type_of_account, "'PNRH_Oficial'='party'")
df$type_of_account<-car::recode(df$type_of_account, "'ppatriota'='party'")
df$type_of_account<-car::recode(df$type_of_account, "'PRI_Guatemala'='party'")
df$type_of_account<-car::recode(df$type_of_account, "'PSDProgreso'='party'")
df$type_of_account<-car::recode(df$type_of_account, "'PSPOFICIAL_SV'='party'")
df$type_of_account<-car::recode(df$type_of_account, "'PTrabajadoresCR'='party'")
df$type_of_account<-car::recode(df$type_of_account, "'renovacionprc'='party'")
df$type_of_account<-car::recode(df$type_of_account, "'RepublicanoCR'='party'")
df$type_of_account<-car::recode(df$type_of_account, "'RigobertMenchu'='candidate'")
df$type_of_account<-car::recode(df$type_of_account, "'robertoarzugg'='candidate'")
df$type_of_account<-car::recode(df$type_of_account, "'RodolfoPiza'='candidate'")
df$type_of_account<-car::recode(df$type_of_account, "'RodrigoChavesR'='candidate'")
df$type_of_account<-car::recode(df$type_of_account, "'RolandoArayaCRJ'='candidate'")
df$type_of_account<-car::recode(df$type_of_account, "'SalvaPresidente'='candidate'")
df$type_of_account<-car::recode(df$type_of_account, "'sanchezceren'='candidate'")
df$type_of_account<-car::recode(df$type_of_account, "'SandraTorresGUA'='candidate'")
df$type_of_account<-car::recode(df$type_of_account, "'SergioMenaPNG'='candidate'")
df$type_of_account<-car::recode(df$type_of_account, "'SocialUnidad'='party'")
df$type_of_account<-car::recode(df$type_of_account, "'SoyPINCR'='party'")
df$type_of_account<-car::recode(df$type_of_account, "'stephcamposofic'='candidate'")
df$type_of_account<-car::recode(df$type_of_account, "'TODOSXGT'='party'")
df$type_of_account<-car::recode(df$type_of_account, "'tonysacaoficial'='candidate'")
df$type_of_account<-car::recode(df$type_of_account, "'unidospodem0s'='party'")
df$type_of_account<-car::recode(df$type_of_account, "'unionliberalcr'='party'")
df$type_of_account<-car::recode(df$type_of_account, "'URNG_GT'='party'")
df$type_of_account<-car::recode(df$type_of_account, "'vamoselsalvador'='party'")
df$type_of_account<-car::recode(df$type_of_account, "'VictoriaPartido'='party'")
df$type_of_account<-car::recode(df$type_of_account, "'VilledaMauricio'='candidate'")
df$type_of_account<-car::recode(df$type_of_account, "'WelmerRamos'='candidate'")
df$type_of_account<-car::recode(df$type_of_account, "'XiomaraCastroZ'='candidate'")
df$type_of_account<-car::recode(df$type_of_account, "'yanirosenthal'='candidate'")
df$type_of_account<-car::recode(df$type_of_account, "'ZuryxGuate'='candidate'")

#table(df$type_of_account)
#####
#I will clean up the text column
#1st, I create a new text column called clean_text
df$clean_text<-df$text

df$clean_text<-str_to_lower(df$clean_text) #get rid of upper case
df$clean_text<-str_replace_all(df$clean_text, "[\r\n]", "") #remove new line indicators 
df$clean_text<-str_replace_all(df$clean_text, "https", "")  #remove https, etc
df$clean_text<-str_replace_all(df$clean_text, "rt", "")
df$clean_text<-str_replace_all(df$clean_text, "t.co", "")
df$clean_text<-str_replace_all(df$clean_text, "amp", "")


#Usually, pre-processing would include removing punctuation, but no point, not counted in LIWC and could include emojis
#also no point in doing stemming, getting rid of stop words, etc

#ALL of the following problem was cause by LIWC not saving in utf-8. So, I keep the code but don't run it
#the main problem is that ï¿½ is replacing all special characters (á, é, í, ñ, etc)
#I need a list of all instances of such sequence of characters
#I will create one more column, for extra prep to extract the words
#first of all, get rid of all '¿½', since they will be eliminated later
#df$clean_text2<-df$clean_text
#df$clean_text2<-str_replace_all(df$clean_text2, "¿½", "")
#df$clean_text2<-str_replace_all(df$clean_text2, "@[^ ]*", "") #this regular expresion is @, and then any possible symbol until a space. So, usernames

#txt<-df%>% #to do this, you have to indicate first just the df
#  select(V1, clean_text2) %>% #this way, the db only has 2 variables and it is managable
#  unnest_tokens(words, clean_text2) #then here, name output column and indicate the input column
#basically, the prior step creates a list of ALL the words used in the db
#also, weirdly it splits all the "ï¿½" and leaves only the "ï", makes sense because the others are not letters
#words_to_correct<-str_extract_all(txt$words,'[a-zA-Z]*ï[a-zA-Z]*') #therefore, I need a regular expression for all words with 'ï' 


#words_to_correct<-unlist(words_to_correct) #turns the list into a vector, easier to extract the words
#words_to_correct<-unique(words_to_correct) #gets rid of duplicates
#words_to_correct #lista de como 23 000 palabras

#str_extract_all(df$clean_text,'[a-zA-Z]*ï¿½[a-zA-Z]*') #regular expression for all words with the 'ï¿½' sequence 


setwd("C:/Elias/1 Serious/Academia/University of Pittsburgh/1 Dissertation/Data")
write.csv(df, 'df_FullDiss_V1.csv', fileEncoding="UTF-8")
#this is automatically save with encoding as ANSI, and that is what LIWC cannot read 


#####
#At this point, I just run the LIWC analysis with the df_FullDiss_V1.csv
#OJO que hay que correrlo en clean_text!!!!

#####
#Uploading the full dataset, some extra preping
setwd("C:/Elias/1 Serious/Academia/University of Pittsburgh/1 Dissertation/Data")
df <-read.csv('df_FullDiss_V2.csv', fileEncoding="UTF-8") #this is the version after wunning LIWC
#ojo que tuve que usar read.csv, NO import, eso jodia el encoding.

#Basic prep, each time
#I need to eliminate a column with duplicate identifier elimiante  the 2nd, the first is the correct identifier identifier
df =select(df, -2)
df<-df %>%
  rename ("V1"="X") 

#I need to transform the "created at" variable from string to a date, every time I upload
df$created_at <- as.Date(df$created_at)

colnames(df)  #Gives names of all variables

#Variables based on LIWC
#Analytic
df<-df%>%
  mutate(Analytic=30+Articulo+Prepos-PronPer-PronImp-VerbAux-Conjunc-Adverb-Negacio)
#Authenticity
df<-df%>%
  mutate(Authenticity=Yo+Insight+Excl+Relativ-Discrep-ElElla)

#i+insight+differ+relativ-discrep-shehe
#Lo unico raro en traducciones aca fue que compare el manueal de 2022 con el paper del diccionario Esp y lo raro fue
#differ me sale como que lo tradujeron como inhib, y dentro de procesos cognitivos son los unicos que tienen sentido
#y son de differentiation a inhibiciones. Excl, exclusiones, una subcategoria de espacio tendia mas sentido, uso ese,
#los ejemplos son palabras como pero, sin excepto


write.csv(df, 'df_FullDiss_V3.csv', fileEncoding="UTF-8")


#Ok, here I start analysis
setwd("C:/Elias/1 Serious/Academia/University of Pittsburgh/1 Dissertation/Data")
df <-read.csv('df_FullDiss_V3.csv', fileEncoding="UTF-8")

table (df$retweet_count)
#Tables with names of candidates and party per year and election

gua11<-df %>%
  filter (Country=="Guatemala") %>% 
  filter (Election=="2011")
table (gua11$user_name)

gua15<-df %>%
  filter (Country=="Guatemala") %>% 
  filter (Election=="2015")
table (gua15$user_name)

gua19<-df %>%
  filter (Country=="Guatemala") %>% 
  filter (Election=="2019")
table (gua19$user_name)

ho13<-df %>%
  filter (Country=="Honduras") %>% 
  filter (Election=="2013")
table (ho13$user_name)

ho17<-df %>%
  filter (Country=="Honduras") %>% 
  filter (Election=="2017")
table (ho17$user_name)

ho21<-df %>%
  filter (Country=="Honduras") %>% 
  filter (Election=="2021")
table (ho21$user_name)

es14<-df %>%
  filter (Country=="El Salvador") %>% 
  filter (Election=="2014")
table (es14$user_name)

es18<-df %>%
  filter (Country=="El Salvador") %>% 
  filter (Election=="2019")
table (es18$user_name)

cr10<-df %>%
  filter (Country=="Costa Rica") %>% 
  filter (Election=="2010")
table (cr10$user_name)

cr14<-df %>%
  filter (Country=="Costa Rica") %>% 
  filter (Election=="2014")
table (cr14$user_name)

cr18<-df %>%
  filter (Country=="Costa Rica") %>% 
  filter (Election=="2018")
table (cr18$user_name)

cr22<-df %>%
  filter (Country=="Costa Rica") %>% 
  filter (Election=="2022")
table (cr22$user_name)

#get maximim and minumin values for Analytic and Authenticity
max(df$Authenticity) #100
min(df$Authenticity) #-100
max(df$Analytic) #96.66
min(df$Analytic) #-70


#Extract sample text from high and low ranking Analytic and Authenticity tweets
df$text[df$Authenticity==50]
df$text[df$Authenticity==0]
df$text[df$Authenticity==-40]

df$text[df$Analytic==50]
df$text[df$Analytic==0]
df$text[df$Analytic==-50]


#get the mean values of authenticity and analytic per account, see the max and min
meanAuth<-df %>%
  group_by(CountryElectionUsername) %>%
  summarise_at(vars(Authenticity), list(name=mean))
max(meanAuth$name) #17.411
min(meanAuth$name) #0

meanAnaly<-df %>%
  group_by(CountryElectionUsername) %>%
  summarise_at(vars(Analytic), list(name=mean))
max(meanAnaly$name) #45.933
min(meanAnaly$name) #21.160
  

#sd too
sdAuth<-df %>%
  group_by(CountryElectionUsername) %>%
  summarise_at(vars(Authenticity), list(name=sd))
max(sdAuth$name) #16.223
min(sdAuth$name) #0

sdAnaly<-df %>%
  group_by(CountryElectionUsername) %>%
  summarise_at(vars(Analytic), list(name=sd))
max(sdAnaly$name) #26.285
min(sdAnaly$name) #7.404

#Graph, cr 2022 of lineth and walter
CR2022<-df %>%
  filter (CountryElection=="Costa Rica 2022") %>%
  filter (user_username=="LinethSaborio" | user_username=="FedeMalavassiC")

CompAuth<-ggplot(data=CR2022, (aes(x=created_at, y=Authenticity, col=user_username)))+
  geom_line(size=.5)+
  theme_minimal() +
  theme(axis.text.x=
        element_text(angle=60, hjust=1, size=13))+
  theme(plot.title=
          element_text(hjust=0.5, size=18))+
  ylab("Authenticity")+
  xlab("")+
  ggtitle("")+
  theme(aspect.ratio = 1/4) +
  theme(legend.position="bottom") +
  ylim (-100,100)
CompAuth
#use a scatter instead
CompAuth2<-ggplot(data=CR2022, (aes(x=created_at, y=Authenticity, col=user_username)))+
  geom_point()+
  geom_smooth(method=lm)+
  theme_minimal() +
  theme(axis.text.x=
          element_text(angle=60, hjust=1, size=13))+
  theme(plot.title=
          element_text(hjust=0.5, size=18))+
  ylab("Authenticity")+
  xlab("")+
  ggtitle("")+
  theme(aspect.ratio = 1/4) +
  theme(legend.position="bottom") +
  ylim (-100,100)
CompAuth2

#Graph, gua 2019 of winaq and unionista
GUA2019<-df %>%
  filter (CountryElection=="Guatemala 2019") %>%
  filter (user_username=="MovimientoWinaq" | user_username=="p_unionista")

CompAnaly<-ggplot(data=GUA2019, (aes(x=created_at, y=Analytic, col=user_username)))+
  geom_line(size=.5)+
  theme_minimal() +
  theme(axis.text.x=
          element_text(angle=60, hjust=1, size=13))+
  theme(plot.title=
          element_text(hjust=0.5, size=18))+
  ylab("Analytic")+
  xlab("")+
  ggtitle("")+
  theme(aspect.ratio = 1/4) +
  theme(legend.position="bottom") +
  ylim (-100,100)
CompAnaly

#as a scatter
CompAnaly2<-ggplot(data=GUA2019, (aes(x=created_at, y=Analytic, col=user_username)))+
  geom_point()+
  geom_smooth(method=lm)+
  theme_minimal() +
  theme(axis.text.x=
          element_text(angle=60, hjust=1, size=13))+
  theme(plot.title=
          element_text(hjust=0.5, size=18))+
  ylab("Analytic")+
  xlab("")+
  ggtitle("")+
  theme(aspect.ratio = 1/4) +
  theme(legend.position="bottom") +
  ylim (-100,100)
CompAnaly2


#Graph, calculate a mean value of both analytic and authentocity per country-election-username, scatter on two dimensions

meanAuthAnaly<-df %>%
  group_by(CountryElectionUsername, type_of_account) %>% #ojo,necesuto agruparlo por coutry-election-username, pero solo incluyendo aca tambien type of accoint puedo mantener la variable
  summarise_at(vars(Authenticity, Analytic), list(name=mean))

rownames(meanAuthAnaly)<-meanAuthAnaly$CountryElectionUsername #gives each row the same name as the first var

Scatter_Auth_Analy<-ggplot(meanAuthAnaly, aes(x=Authenticity_name, y=Analytic_name, colour =type_of_account)) + 
  geom_point() +
  theme_minimal()+
  xlab("Authenticity")+
  ylab("Analytic")+
  labs(col="Type of account")
  #geom_text_repel(label=rownames(meanAuthAnaly)) #very hard to read
Scatter_Auth_Analy
cor(meanAuthAnaly$Authenticity_name, meanAuthAnaly$Analytic_name)

#////
#run a graphic of time series of Analytic and Authenticity to see variation
#This are just Bukele tweets for the 2019 Es election
BukeleES2019<-df %>%
  filter (Country=="El Salvador") %>% 
  filter (Election=="2019") %>%
  filter (user_username=="nayibbukele")

#authenticity bukele  
AuthenticityBukeleES2019<-ggplot(data=BukeleES2019,(aes(x=created_at, y=Authenticity)))+ 
  geom_line(color="blue", size=.5)+
  theme_minimal()+
  theme(axis.text.x=
          element_text(angle=60, hjust=1, size=13))+
  theme(plot.title=
          element_text(hjust=0.5, size=18))+
  ylab("Authenticity")+
  xlab("")+
  ggtitle("")+
  theme(aspect.ratio = 1/4) +
  theme(legend.position="none") +
  ylim (-100,100)
AuthenticityBukeleES2019

#analytic bukele
AnalyticBukeleES2019<-ggplot(data=BukeleES2019,(aes(x=created_at, y=Analytic)))+ 
  geom_line(color="blue", size=.5)+
  theme_minimal()+
  theme(axis.text.x=
          element_text(angle=60, hjust=1, size=13))+
  theme(plot.title=
          element_text(hjust=0.5, size=18))+
  ylab("Analytic")+
  xlab("")+
  ggtitle("")+
  theme(aspect.ratio = 1/4) +
  theme(legend.position="none") +
  ylim(-100, 100)
AnalyticBukeleES2019

#Que tal el pln? es partido, deberia de estar institucionalizado, etc, y lauchi?
#Vilallta 2022 election
VillaltaCR2022<-df %>%
  filter (Country=="Costa Rica") %>% 
  filter (Election=="2022") %>%
  filter (user_username=="josemvillalta")

#authenticity villalta
AuthenticityVillaltaCR2022<-ggplot(data=VillaltaCR2022 ,(aes(x=created_at, y=Authenticity)))+ 
  geom_line(color="red", size=.5)+
  theme_minimal()+
  theme(axis.text.x=
          element_text(angle=60, hjust=1, size=13))+
  theme(plot.title=
          element_text(hjust=0.5, size=18))+
  ylab("Authenticity")+
  xlab("")+
  ggtitle("")+
  theme(aspect.ratio = 1/4) +
  theme(legend.position="none") +
  ylim (-100,100)
AuthenticityVillaltaCR2022

#analytic villalta
AnalyticVillaltaCR2022<-ggplot(data=VillaltaCR2022 ,(aes(x=created_at, y=Analytic)))+ 
  geom_line(color="red", size=.5)+
  theme_minimal()+
  theme(axis.text.x=
          element_text(angle=60, hjust=1, size=13))+
  theme(plot.title=
          element_text(hjust=0.5, size=18))+
  ylab("Analytic")+
  xlab("")+
  ggtitle("")+
  theme(aspect.ratio = 1/4) +
  theme(legend.position="none") +
  ylim(-100, 100)
AnalyticVillaltaCR2022

#The two plots combined
grid.arrange(AuthenticityBukeleES2019, AuthenticityVillaltaCR2022, ncol=2)
grid.arrange(AnalyticBukeleES2019, AnalyticVillaltaCR2022, ncol=2)

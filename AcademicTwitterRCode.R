#Elías Chavarría-Mora
#Code for obtainign tweets from presidentail elections in CA, R version

#general code


library(academictwitteR) #For pulling the tweets
library(tidyverse) #clean up
#library(tm) #might not need
library(tidytext) #creating the corpus
#library(reshape2) 
#library (ggplot2) #plotting
#library(SnowballC)
#data("stop_words") #data for stop words
setwd("C:/Elias/1 Serious/Academia/University of Pittsburgh/1 Dissertation/Data/ElectionsTweets")
bearer_token <- "AAAAAAAAAAAAAAAAAAAAAKzNPAEAAAAA4i2sMirmlXNPBryoOr6SpMFhY9Q%3DyVCxU4eK9NuRwZQeRTb5oFlFoAGg9Xrh4kKwQqPvlBmOOBmF5s"


#Elections:
#Costa Rica
#Costa Rica 2010
CR2010<-get_all_tweets(query = 'from:plncr OR from:Laura_Ch OR from:accionciudadana OR from:ottonsolispac OR from:OttoGuevaraG OR from:SocialUnidad OR from:elmenosmalo OR from:Fishman2010 OR from:PARTIDOPASE OR from:oscarlopez_cr OR from:renovacionprc OR from:FrenteAmplio OR from:eugeniotrejos OR from:somosap OR from:RolandoArayaCRJ OR from:SoyPINCR OR from:diputado_dr', 
                              start_tweets = "2009-10-01T00:00:00Z", 
                              end_tweets = "2010-02-08T00:00:00Z",
                              bearer_token = bearer_token,
                              data_path = "CR2010",
                              bind_tweets = F,
                              n= Inf)
df_CR2010 <- bind_tweets(data_path = "CR2010", output_format = "tidy")
write.csv(df_CR2010, 'df_CR2010.csv')

#Costa Rica 2014
CR2014<-get_all_tweets(query = 'from:accionciudadana OR from:luisguillermosr OR from:plncr OR from:Johnny_Araya OR from:FrenteAmplio OR from:josemvillalta OR from:OttoGuevaraG OR from:SocialUnidad OR from:RodolfoPiza OR from:drhernandezcr OR from:jmCorrales2014 OR from:restauracion_na OR from:c_avendanocr OR from:renovacionprc OR from:justo_orozco OR from:PARTIDOPASE OR from:oscarlopez_cr OR from:SergioMenaPNG OR from:jmechandi OR from:SoyPINCR OR from:diputado_dr', 
                       start_tweets = "2013-10-02T00:00:00Z", 
                       end_tweets = "2014-04-07T00:00:00Z",
                       bearer_token = bearer_token,
                       data_path = "CR2014",
                       bind_tweets = F,
                       n= Inf)
df_CR2014 <- bind_tweets(data_path = "CR2014", output_format = "tidy")
write.csv(df_CR2014, 'df_CR2014.csv')

#Costa Rica 2018
CR2018<-get_all_tweets(query = 'from:accionciudadana OR from:CarlosAlvQ OR from:restauracion_na OR from:FabriAlvarado7 OR from:plncr OR from:alvarez_desanti OR from:SocialUnidad OR from:RodolfoPiza OR from:SoyPINCR OR from:JDiegoCastroCR OR from:RepublicanoCR OR from:drhernandezcr OR from:OttoGuevaraG OR from:FrenteAmplio OR from:EdgardoArayaS OR from:SergioMenaPNG OR from:AlianzaDC OR from:marioredondo OR from:renovacionprc OR from:stephcamposofic OR from:PARTIDOPASE OR from:oscarlopez_cr OR from:PTrabajadoresCR OR from:jvega_cr', 
                       start_tweets = "2017-10-03T00:00:00Z", 
                       end_tweets = "2018-04-02T00:00:00Z",
                       bearer_token = bearer_token,
                       data_path = "CR2018",
                       bind_tweets = F,
                       n= Inf)
df_CR2018 <- bind_tweets(data_path = "CR2018", output_format = "tidy")
write.csv(df_CR2018, 'df_CR2018.csv')

#Costa Rica 2022
CR2022<-get_all_tweets(query = 'from:nuevarepublica7 OR from:FabriAlvarado7 OR from:PartidoCRJ OR from:RolandoArayaCRJ OR from:PSDProgreso OR from:RodrigoChavesR OR from:plncr OR from:figuerescr OR from:liberalcr OR from:elifeinzaig OR from:RepublicanoCR OR from:drhernandezcr OR from:PARTIDOPASE OR from:oscarlopez_cr OR from:accionciudadana OR from:WelmerRamos OR from:AlianzaDC OR from:criverapaniagua OR from:oscardelcampo OR from:FrenteAmplio OR from:josemvillalta OR from:PartidoFuerzaN2 OR from:SoyPINCR OR from:diputado_dr OR from:PJusticiaSocial OR from:CarmenQuesadaS5 OR from:FatBobBeto OR from:DrRoulan OR from:RodolfoPiza OR from:Piza2022 OR from:SergioMenaPNG OR from:PartidoPuebloU1 OR from:MartinChCastro OR from:restauracion_na OR from:Cruickshankedu OR from:PTrabajadoresCR OR from:jvega_cr OR from:SocialUnidad OR from:LinethSaborio OR from:unidospodem0s OR from:Natdiaquin OR from:unionliberalcr OR from:FedeMalavassiC', 
                       start_tweets = "2021-10-05T00:00:00Z", 
                       end_tweets = "2022-04-04T00:00:00Z",
                       bearer_token = bearer_token,
                       data_path = "CR2022",
                       bind_tweets = F,
                       n= Inf)
df_CR2022 <- bind_tweets(data_path = "CR2022", output_format = "tidy")
write.csv(df_CR2022, 'df_CR2022.csv')



#El Salvador
#ES 2014
ES2014<-get_all_tweets(query = 'from:FMLNDeptalSS OR from:FMLNoficial OR from:sanchezceren OR from:ARENAOFICIAL OR from:norman_quijano OR from:GANAOFICIAL OR from:ElsalvadorPdc OR from:PCN_OFICIAL  OR from:tonysacaoficial OR from:PSPOFICIAL_SV', 
                       start_tweets = "2013-09-23T00:00:00Z", 
                       end_tweets = "2014-03-10T00:00:00Z",
                       bearer_token = bearer_token,
                       data_path = "ES2014",
                       bind_tweets = F,
                       n= Inf)
df_ES2014 <- bind_tweets(data_path = "ES2014", output_format = "tidy")
write.csv(df_ES2014, 'df_ES2014.csv')

#ES 2019
ES2019<-get_all_tweets(query = 'from:GANAOFICIAL OR from:nayibbukele OR from:ARENAOFICIAL OR from:PCN_OFICIAL OR from:ElsalvadorPdc OR from:democraciasal OR from:jccalleja OR from:FMLNoficial OR from:FMLNDeptalSS OR from:HugoMartinezSV OR from:vamoselsalvador OR from:josuealvaradosv', 
                       start_tweets = "2018-10-02T00:00:00Z", 
                       end_tweets = "2019-02-04T00:00:00Z",
                       bearer_token = bearer_token,
                       data_path = "ES2019",
                       bind_tweets = F,
                       n= Inf)
df_ES2019 <- bind_tweets(data_path = "ES2019", output_format = "tidy")
write.csv(df_ES2019, 'df_ES2019.csv')


#Honduras 
#HO 2013
HO2013<-get_all_tweets(query = 'from:PNH_oficial OR from:JuanOrlandoH OR from:PartidoLibre OR from:XiomaraCastroZ OR from:PLHonduras OR from:VilledaMauricio OR from:DiputadosPac OR from:SalvaPresidente OR from:p_alianza OR from:RomeoVasquezAPH OR from:OrleSols OR from:Pinusd_HN', 
                       start_tweets = "2013-05-23T00:00:00Z", 
                       end_tweets = "2013-11-25T00:00:00Z",
                       bearer_token = bearer_token,
                       data_path = "HO2013",
                       bind_tweets = F,
                       n= Inf)
df_HO2013 <- bind_tweets(data_path = "HO2013", output_format = "tidy")
write.csv(df_HO2013, 'df_HO2013.csv')

#HO 2017
HO2017<-get_all_tweets(query = 'from:PNH_oficial OR from:JuanOrlandoH OR from:PartidoLibre OR from:Pinusd_HN OR from:SalvaPresidente OR from:PLHonduras OR from:luiszelaya_hn OR from:p_alianza OR from:DiputadosPac OR from:MarleneAlv_ OR from:eliseovallecil3', 
                       start_tweets = "2017-05-25T00:00:00Z", 
                       end_tweets = "2017-11-26T00:00:00Z",
                       bearer_token = bearer_token,
                       data_path = "HO2017",
                       bind_tweets = F,
                       n= Inf)
df_HO2017 <- bind_tweets(data_path = "HO2017", output_format = "tidy")
write.csv(df_HO2017, 'df_HO2017.csv')

#HO 2021
HO2021<-get_all_tweets(query = 'from:PartidoLibre OR from:XiomaraCastroZ OR from:PNH_oficial OR from:Tito_alcalde OR from:PLHonduras OR from:yanirosenthal OR from:HondurasHumana OR from:MiltonBeniteztv OR from:p_alianza OR from:RomeoVasquezVe OR from:PNRH_Oficial OR from:EsdrasALopez OR from:PartidoSalvador OR from:Pinusd_HN OR from:SalvaPresidente OR from:TodossomosHND OR from:escoto_marlon OR from:MarlonEscotoTSH OR from:Abogado_Coto OR from:LIDEHR1 OR from:Vianamora OR from:SantosROrellana', 
                       start_tweets = "2021-05-27T00:00:00Z", 
                       end_tweets = "2021-11-28T00:00:00Z",
                       bearer_token = bearer_token,
                       data_path = "HO2021",
                       bind_tweets = F,
                       n= Inf)
df_HO2021 <- bind_tweets(data_path = "HO2021", output_format = "tidy")
write.csv(df_HO2021, 'df_HO2021.csv')


#Guatemala
#Guatemala 2011
Guatemala2011<-get_all_tweets(query = 'from:ppatriota OR from:ottoperezmolina OR from:ManuelBaldizon OR from:creoguate OR from:DrEduardoSuger OR from:Partido_VIVA OR from:DrHCaballeros OR from:RigobertMenchu OR from:PANResponde OR from:DrGiammattei OR from:AdelaTorrebiart', 
                                   start_tweets = "2011-05-02T00:00:00Z", 
                                   end_tweets = "2011-11-07T00:00:00Z",
                                   bearer_token = bearer_token,
                                   data_path = "Guatemala2011",
                                   bind_tweets = F,
                                   n= Inf)
df_Guatemala2011 <- bind_tweets(data_path = "Guatemala2011", output_format = "tidy")
write.csv(df_Guatemala2011, 'df_Guatemala2011.csv')

#Guatemala 2015
Guatemala2015<-get_all_tweets(query = 'from:jimmymoralesgt OR from:SandraTorresGUA OR from:ManuelBaldizon OR from:FUERZAGUATEMALA OR from:DrGiammattei OR from:Partido_VIVA OR from:ZuryxGuate OR from:TODOSXGT OR from:lizarsosa OR from:ppatriota OR from:Dr_MDGarcia OR from:creoguate OR from:CanelaCREO OR from:PANResponde OR from:MovimientoWinaq OR from:URNG_GT OR from:EncuentroGuate OR from:PRI_Guatemala OR from:MNRguatemala OR from:AnibalGarciaGT', 
                              start_tweets = "2015-05-02T00:00:00Z", 
                              end_tweets = "2015-10-26T00:00:00Z",
                              bearer_token = bearer_token,
                              data_path = "Guatemala2015",
                              bind_tweets = F,
                              n= Inf)
df_Guatemala2015 <- bind_tweets(data_path = "Guatemala2015", output_format = "tidy")
write.csv(df_Guatemala2015, 'df_Guatemala2015.csv')

#Guatemala 2019
Guatemala2019<-get_all_tweets(query = 'from:Partido_Vamos OR from:DrGiammattei OR from:SandraTorresGUA OR from:Humanista_guate OR from:Edmondmulet OR from:GtMlp OR from:ThelmaCabreraP2 OR from:PANResponde OR from:GuatePodemos OR from:gt_podemos OR from:robertoarzugg OR from:IsaacFarchi OR from:MovimientoWinaq OR from:MVillacortaOf OR from:Galdamez_Kaibil OR from:creoguate OR from:JulioHectorE OR from:TODOSXGT OR from:_FredyCabrera OR from:VictoriaPartido OR from:URNG_GT OR from:CetoPablo OR from:p_unionista  OR from:PabloDuarteGT OR from:EncuentroGuate OR from:ManfredoGuate OR from:AnibalGarciaGT OR from:partidounidosgt OR from:LuisVelasquezQ OR from:GuatemalaAVANZA', 
                              start_tweets = "2019-01-18T00:00:00Z", 
                              end_tweets = "2019-08-12T00:00:00Z",
                              bearer_token = bearer_token,
                              data_path = "Guatemala2019",
                              bind_tweets = F,
                              n= Inf)
df_Guatemala2019 <- bind_tweets(data_path = "Guatemala2019", output_format = "tidy")
write.csv(df_Guatemala2019, 'df_Guatemala2019.csv')


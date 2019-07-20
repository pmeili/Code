#************************
#Response variables table
#************************
accOAnimals<- cbind(unfaelle$ID.Testsrecke,as.data.frame(unfaelle$Wildart))
colnames(accOAnimals)<-c('ID','Art')
accOAnimals<-accOAnimals[1:1984,1:2]

totalAccidents<-cbind(1:151,matrix(NA,151,3))

#Aggregate in to four groups
for ( i in 1:151){
  jk<-accOAnimals[which(accOAnimals$ID==i),]
  totalAccidents[i,2]<-sum(jk$Art=='Rehwild',jk$Art=='Damwild',
                           jk$Art=='Rotwild',     na.rm=T)
  totalAccidents[i,3]<-sum(jk$Art=='Schwarzwild')
  totalAccidents[i,4]<-sum(jk$Art=='Dachs' ,jk$Art=='Fuchs' ,
                           jk$Art=='Hase/Kaninchen' ,jk$Art=='Katze' ,
                           jk$Art=='unbekannt' ,jk$Art =='Waschbär')
}

totalAccidents<-cbind(totalAccidents, apply(totalAccidents[,2:4],1,sum))
colnames(totalAccidents)<-c('id','roeRedFallowDeer', 'wildBoar', 'others','sumWVC')


#***************************
#Explanatory variables table
#***************************
strecken<-strecken[1:151,]

#Some of the variables needed to be found by the column index since the name of the
#variables did not work properly.
routes<-cbind.data.frame(strecken$ID.new,strecken[,5],strecken[,30],
                          strecken$F.F..1.,strecken$Feld.Wald..2.,strecken$W.W..3.,
                          strecken$Waldbestandsdichte..........AE3.0.5.AD3..D3..100.,
                          strecken$Fahrradweg,strecken$Pfad,strecken[,40], 
                          strecken[,41],strecken$Tunnel,strecken[,43], strecken$Bach,
                          strecken$Fluss,strecken$Kanal,strecken$Sumpfgebiet, strecken$Wasser,
                          strecken$Ablauf..drain. , strecken[,50] , 
                          strecken$Wald..m2.,strecken$Acker..m2.,strecken$Wiese..m2.,
                          strecken[,55],strecken$Unterholz..m2., strecken$Friedhof..m2.,
                          strecken$Siedlung..m2., strecken$Schrebergarten..m2., 
                          strecken$Heide..m2.,strecken$Steinbruch..m2., strecken$Obstgarten..m2., 
                          strecken$Industriegebiet..m2.,strecken$Park..m2., strecken$Naturreservat..m2., 
                          strecken[,66],strecken$Kurvigkeit, strecken$Mittelere.Geschwindigkeit, 
                          strecken$Regelquerschnitt.Kategorien,strecken$Anzahl.Fahrbahnen, 
                          strecken$Leitplanke..m., strecken$Wildwechselwarnschilder,
                          strecken$DTV.Gesamtverkehr,strecken$DTV.Schwerverkehr, strecken[,85],
                          strecken$Shannon.Diversity.Index, strecken$Winterweizen,
                          strecken$Sommerweizen, strecken$Winterroggen, strecken$Wintergerste,
                          strecken$Sommergerste, strecken$Sommerhafer, strecken$Mais, 
                          strecken[,123],strecken$Winterraps,strecken[,130], strecken[,131],
                          strecken[,132],strecken[,138],strecken[,141],strecken[,142], 
                          strecken[,143],strecken[,144],strecken[,148],strecken[149]
)

#To see the specifications of the different variables consult VariableList.xlsx.
colnames(routes)<-c('id','length','carPark', 'fieldField','fieldForest',
                    'forestForest','forestShare', 'bicycleLane', 'trail',
                    'footpath', 'bridge','tunnel','railing', 'brook','river',
                    'canal', 'swamp', 'water','drain', 'gutter','forest',
                    'field','meadow','grassland', 'underwood','graveyard',
                    'residentialArea', 'allotment','moorland','quarry',
                    'orchard','industrialArea','park', 'naturalPreserve', 
                    'sinuosity','curvature', 'meanSpeed', 'widthCat','lanes',
                    'guardrail', 'warningSings', 'totalTraffic','heavyTraffic',
                    'bicycles','diversity', 'wWheat','sWheat','wRye','wBarley
                    ','sBarley','sOat','corn','protein','wCanola','fieldFeed',
                    'alwaysGreen','fallowLand', 'nonWateredField','decidiousForest',
                    'coniferousForest','mixedForest','forestPassage',
                    'mixedSites','leisureSites'
)


#Replace all in percentage denominated variables with new values due to
#inaccuracies in the old data.
routes[,which(colnames(routes)=='wWheat')] <-landNew$Winterweizen
routes[,which(colnames(routes)=='sWheat')] <-landNew$Sommerweizen
routes[,which(colnames(routes)=='wRye')] <-landNew$Winterroggen
routes[,which(colnames(routes)=='wBarley')] <-landNew$Wintergerste
routes[,which(colnames(routes)=='sBArley')] <-landNew$Sommergerste
routes[,which(colnames(routes)=='sOat')] <-landNew$Sommerhafer
routes[,which(colnames(routes)=='corn')] <-landNew$Mais
routes[,which(colnames(routes)=='protein')] <-landNew[,10]
routes[,which(colnames(routes)=='wCanola')] <-landNew$Winterraps
routes[,which(colnames(routes)=='fieldFeed')] <-landNew[,17]
routes[,which(colnames(routes)=='alwaysGreen')] <-landNew[,18]
routes[,which(colnames(routes)=='fallowLand')] <-landNew[,19]
routes[,which(colnames(routes)=='nonWateredField')] <-landNew[,25]
routes[,which(colnames(routes)=='decidiousForest')] <-landNew[,28]
routes[,which(colnames(routes)=='coniferousForest')] <-landNew[,29]
routes[,which(colnames(routes)=='mixedForest')] <-landNew[,30]
routes[,which(colnames(routes)=='forestPassage')] <-landNew[,31]
routes[,which(colnames(routes)=='mixedSites')] <-landNew[,35]
routes[,which(colnames(routes)=='leisureSites')] <-landNew[,36]

routes<-cbind.data.frame(routes, landNew[,37])
colnames(routes)[65]<-'totalArea'


#Remove explanatory variables, which do not satisfy: Number of observations > 5.
routes<-routes[,-which(colnames(routes)=='park')]    #Number of entries = 4
routes<-routes[,-which(colnames(routes)=='tunnel')]  #All NA's
routes<-routes[,-which(colnames(routes)=='canal')]   #Number of entries = 5
routes<-routes[,-which(colnames(routes)=='swamp')]   #Number of entries = 4


#Change to per meter values
routes[,c(which(colnames(routes)=='fieldField'),
          which(colnames(routes)=='fieldForest'),
          which(colnames(routes)=='forestForest'),
          which(colnames(routes)=='forest'),
          which(colnames(routes)=='field'),
          which(colnames(routes)=='meadow'),
          which(colnames(routes)=='grassland'),
          which(colnames(routes)=='underwood'),
          which(colnames(routes)=='graveyard'),
          which(colnames(routes)=='residentialArea'),
          which(colnames(routes)=='allotment'),
          which(colnames(routes)=='moorland'),
          which(colnames(routes)=='quarry'),
          which(colnames(routes)=='orchard'),
          which(colnames(routes)=='industrialArea'),
          which(colnames(routes)=='naturalPreserve'),
          which(colnames(routes)=='guardrail'),
          which(colnames(routes)=='totalArea')
)]<- routes[,c(which(colnames(routes)=='fieldField'),
               which(colnames(routes)=='fieldForest'),
               which(colnames(routes)=='forestForest'),
               which(colnames(routes)=='forest'),
               which(colnames(routes)=='field'),
               which(colnames(routes)=='meadow'),
               which(colnames(routes)=='grassland'),
               which(colnames(routes)=='underwood'),
               which(colnames(routes)=='graveyard'),
               which(colnames(routes)=='residentialArea'),
               which(colnames(routes)=='allotment'),
               which(colnames(routes)=='moorland'),
               which(colnames(routes)=='quarry'),
               which(colnames(routes)=='orchard'),
               which(colnames(routes)=='industrialArea'),
               which(colnames(routes)=='naturalPreserve'),
               which(colnames(routes)=='guardrail'),
               which(colnames(routes)=='totalArea'))]/routes$length

#Change four values in the dataset
routes$heavyTraffic<-as.numeric(as.character(routes$heavyTraffic))
routes$heavyTraffic[62]<-mean(c(137,263))   #Was before '137-263'
routes$heavyTraffic[137]<-mean(c(829,349))  #Was before '829-349'

routes$lanes[135]<-'2-3'  #Was before '2-3 (gt 3)'
routes$lanes[77]<-'1-2'   #Was before '1-2 z.T. kein Mittelstreifen'

routes$lanes<-as.factor(as.character(routes$lanes))

#Standardize all factors with two levels to 0 and 1
routes[,c(which(colnames(routes)=='carPark'))]<-
  as.factor(ifelse(routes[,c(which(colnames(routes)=='carPark'))]=='ja',1,0))

for ( i in 8:17){
  routes[,i]<-as.numeric(routes[,i])
}

routes[is.na(routes)]<-0
routes$river<-replace(routes$river==2,1,0)
routes$gutter<-replace(routes$gutter==2,1,0)

for ( i in 8:17){
  routes[,i]<-as.factor(routes[,i])
}

routes$widthCat<-as.factor(routes$widthCat)
routes$warningSings<-as.factor(routes$warningSings)


#****************************
#Relevant variable list names
#****************************
#Remove the not used variables out of the variableList VL into the variableListUnit VLU
VLU<-VL[,which(colnames(VL)=='Unit')]
VLU<-as.matrix(VLU)
VLU<-VLU[-which(VL$Name.in.R=='park  **')]
VLU<-VLU[-which(VL$Name.in.R=='canal  **')]
VLU<-VLU[-which(VL$Name.in.R=='swamp  **')]
VLU<-VLU[-which(VL$Name.in.R=='tunnel  **')]
VLU<-VLU[2:61] #Remove the id column

#************************************************************
#Data preperation into four datasets for statistical analysis
#************************************************************
#Not needed for the analysis
routesWithoutID<- routes[,-which(colnames(routes)== 'id')]

#Create the 4 datasets
routesWVC<-cbind(routesWithoutID,
                 totalAccidents[,which(colnames(totalAccidents)=='sumWVC')])
colnames(routesWVC)[ncol(routesWVC)]<-'sumWVC'

routesRRFD<-cbind(routesWithoutID,
                  totalAccidents[,which(colnames(totalAccidents)=='roeRedFallowDeer')])
colnames(routesRRFD)[ncol(routesRRFD)]<-'roeRedFallowDeer'

routesWB<-cbind(routesWithoutID,
                totalAccidents[,which(colnames(totalAccidents)=='wildBoar')])
colnames(routesWB)[ncol(routesWB)]<-'wildBoar'

routesO<-cbind(routesWithoutID,
               totalAccidents[,which(colnames(totalAccidents)=='others')])
colnames(routesO)[ncol(routesO)]<-'others'

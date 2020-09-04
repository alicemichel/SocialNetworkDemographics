#######################################
##### Social Network Demographics #####
#######################################
library(RPostgreSQL)
library(network)
library(chron)
library(stringr)
library(EloRating)

setwd('G:/My Drive/Graduate School/Research/Projects/NetworkDemographics')

## Source functions
source('G:/My Drive/Graduate School/Research/Projects/TemporalNets/SeasonalNetworkAnalyses/createNetworkFunction.R') #Edge weights are either counts or duration
source('G:/My Drive/Graduate School/Research/Projects/TemporalNets/SeasonalNetworkAnalyses/createObsMatrix.R')
source('G:/My Drive/Graduate School/Research/AO/CleanAOData/CleanAOData/CleanSocialDataFunctions.R')

demo		<- read.csv('G:/My Drive/Graduate School/Research/FieldSeason2019MF/Demographic Data/demographicData.csv')
demo		<- demo[order(demo$ID),]

## Connect to database
drv	<- dbDriver('PostgreSQL') ##Be sure to use real database name
con	<- dbConnect(drv, dbname = 'diadema_fulvus', host = 'localhost', port = 5432,
								 user = 'postgres', password = 'Animalbehavior1#')

allData	<- dbGetQuery(con, 'select group_id, pin_code_name, focal_start_time, 
	focal_end_time, focal_individual_id, behavior_time, actor, subject, category, behavior, start_stop,
	initiator, mutual, latitude, longitude from main_tables.all_focal_data_view;')
focalList	<- dbGetQuery(con, 'select main_tables.list_focals.session_start_time, 
				main_tables.list_focals.focal_start_time, main_tables.list_focals.focal_end_time,
				main_tables.list_focals.focal_individual_id, main_tables.list_sessions.group_id,
				main_tables.list_sessions.pin_code_name from main_tables.list_focals
				left join main_tables.list_sessions
				on main_tables.list_focals.session_start_time = main_tables.list_sessions.session_start_time;')

###########################
### Match up start ends ###
###########################
dyadIDs	<- apply(as.matrix(allData[, c('actor', 'subject')]), 1, createID)
allDataID	<- cbind(allData, dyadIDs)
allDataIDNoNA	<- allDataID[is.na(allDataID$actor) == FALSE,]

focal_start_str	<- data.frame(str_split_fixed(as.character(allDataIDNoNA$focal_start_time), ' ', n = 2))
colnames(focal_start_str)	<- c('focal_date', 'focal_time')
focal_start_chron	<- chron(dates. = as.character(focal_start_str$focal_date), times. = as.character(focal_start_str$focal_time),
	format = c(dates = 'y-m-d', times = 'h:m:s'))
behavior_time_str	<- data.frame(str_split_fixed(as.character(allDataIDNoNA$behavior_time), ' ', n = 2))
colnames(behavior_time_str)	<- c('behav_date', 'behav_time')
behavior_time_chron	<- chron(dates. = as.character(behavior_time_str$behav_date), times. = as.character(behavior_time_str$behav_time),
	format = c(dates = 'y-m-d', times = 'h:m:s'))
allDataIDNoNA$focal_start_chron	<- focal_start_chron
allDataIDNoNA$behavior_time_chron	<- behavior_time_chron

social	<- allDataIDNoNA[is.na(allDataIDNoNA$category) == FALSE,]
affil		<- social[social$category == 'Affilative' & social$pin_code_name != 'Odilon',]
agg		<- social[social$category == 'Aggressive' & social$pin_code_name != 'Odilon' & social$pin_code_name != 'Sonne' & social$pin_code_name != 'Mamy',]
sub		<- social[social$category == 'Submissive' & social$pin_code_name != 'Odilon' & social$pin_code_name != 'Sahoby' & social$pin_code_name != 'Sonne' & social$pin_code_name != 'Mamy',]
info		<- social[social$category == 'Information' & social$pin_code_name == 'Meredith',]
grt		<- social[social$behavior == 'Greet' & (social$pin_code_name == 'Onja' | social$pin_code_name == 'Hasina' | social$pin_code_name == 'Diary'),]
socialSub	<- rbind(affil, agg, sub, info, grt)

cleanedData	<- data.frame()
errorFile	<- data.frame()

cleaned	<- cleanAllFocalData(socialSub, cleanedData, errorFile)

socialData	<- cleaned[[1]]

subWinLoss	<- sub[,c('group_id', 'pin_code_name', 'behavior_time', 'subject', 'actor', 'behavior')]
colnames(subWinLoss)	<- c('group_id', 'pin_code_name', 'behavior_time', 'winner', 'loser', 'behavior')
aggWinLoss	<- agg[,c('group_id', 'pin_code_name', 'behavior_time', 'actor', 'subject', 'behavior')]
colnames(aggWinLoss)	<- c('group_id', 'pin_code_name', 'behavior_time', 'winner', 'loser', 'behavior')

dominanceData	<- rbind(aggWinLoss, subWinLoss)
dominanceData	<- dominanceData[order(dominanceData$behavior_time),]
d2domData	<- dominanceData[dominanceData$group_id == 'Diadema 2',]
d3domData	<- dominanceData[dominanceData$group_id == 'Diadema 3',]

#####################################
### Getting focal list sorted out ###
#####################################
focalListNoMl				<- focalList[focalList$focal_individual_id != 'Ml',]
focalListNoVo				<- focalListNoMl[!(focalListNoMl$focal_individual_id == 'Vo' & focalListNoMl$focal_start_time >= '2019-10-07'),]

focalListNoOdilon				<- focalListNoVo[focalListNoVo$pin_code_name != 'Odilon',]
list_focal_start_str			<- data.frame(str_split_fixed(as.character(focalListNoOdilon$focal_start_time), ' ', n = 2))
colnames(list_focal_start_str)	<- c('focal_date', 'focal_time')
list_focal_start_chron			<- chron(dates. = as.character(list_focal_start_str$focal_date), times. = as.character(list_focal_start_str$focal_time),
							format = c(dates = 'y-m-d', times = 'h:m:s'))
focal_stop_str				<- data.frame(str_split_fixed(as.character(focalListNoOdilon$focal_end_time), ' ', n = 2))
colnames(focal_stop_str)		<- c('behav_date', 'behav_time')
focal_stop_chron				<- chron(dates. = as.character(focal_stop_str$behav_date), times. = as.character(focal_stop_str$behav_time),
							format = c(dates = 'y-m-d', times = 'h:m:s'))
focalListNoOdilon$focal_start_chron		<- list_focal_start_chron
focalListNoOdilon$focal_stop_chron		<- focal_stop_chron
focalListNoOdilon$focalDuration		<- as.numeric(focalListNoOdilon$focal_stop_chron - focalListNoOdilon$focal_start_chron)*24 #puts it in hours

#No Odilon is ok for affiliative behaviors, for other behaviors need to subset out various other observers' data

###########################################
### Generating monthly overlap matrices ###
###########################################

#First assume that all animals in the group are able to overlap with everyone, then remove the appropriate amounts
diadema2	<- sort(c('On', 'Ta', 'Zr', 'Kr', 'Gg', 'Jb', 'Tk', 'Kt', 'Vo', 'Gr', 'Bl'))
diadema3	<- sort(c('Pk', 'Mk', 'An', 'Bk', 'Sm', 'Or'))
fulvus2	<- sort(c('Af', 'Kl', 'Sf', 'Fz', 'Ps', 'Pr', 'Mn', 'Ld'))
fulvus3	<- sort(c('Zk', 'Rk', 'Vt', 'So', 'Kd', 'Pm', 'Gy'))

listAnimalsByGroup	<- list(diadema2, diadema3, fulvus2, fulvus3)
listGroups		<- c('Diadema 2', 'Diadema 3', 'Fulvus 2', 'Fulvus 3')
listFocalByGroup	<- list(focalListNoOdilon[focalListNoOdilon$group_id == listGroups[1],], focalListNoOdilon[focalListNoOdilon$group_id == listGroups[2],],
				focalListNoOdilon[focalListNoOdilon$group_id == listGroups[3],], focalListNoOdilon[focalListNoOdilon$group_id == listGroups[4],])

#Still need to deal with Raketa, Voa, and Akofa
allFocalDurations	<- list()
for(i in 1:4){ #for groups
	groupsFocals	<- listFocalByGroup[[i]]
	focalTable		<- data.frame(focal_individual_id = listAnimalsByGroup[[i]], duration = tapply(groupsFocals$focalDuration, groupsFocals$focal_individual_id, FUN = sum))
	allFocalDurations[[i]]	<- focalTable
}

allObsMatList	<- lapply(allFocalDurations, createObsMatrix) #Need to adjust for Vo, Rk, and Af

################################
### Removing problem animals ###
################################
socialData1	<- socialData[socialData$actor != 'Ml' & socialData$subject != 'Ml' & socialData$actor != 'UNK' & socialData$subject != 'UNK',]
socialData2	<- socialData1[!((socialData1$actor == 'Vo' | socialData1$subject == 'Vo') & socialData1$startTime >= '2019-09-28'),]

##########################################
### Organizing demographic information ###
##########################################
d2demo	<- demo[demo$group == 'Diadema 2',]
d3demo	<- demo[demo$group == 'Diadema 3',]
f2demo	<- demo[demo$group == 'Fulvus 2',]
f3demo	<- demo[demo$group == 'Fulvus 3',]

###########################################
### Calculate rank & covariate matrices ###
###########################################
d2res 	<- elo.seq(winner = d2domData$winner, loser = d2domData$loser, Date = d2domData$behavior_time, runcheck = TRUE)
d3res 	<- elo.seq(winner = d3domData$winner, loser = d3domData$loser, Date = d3domData$behavior_time, runcheck = TRUE)

d2Elo		<- extract_elo(d2res)
d2EloAlpha	<- d2Elo[c(10, 3, 11, 7, 6, 4, 2, 1, 9, 5, 8)]
d2demo$rank	<- d2EloAlpha

d3Elo		<- extract_elo(d3res)
d3EloAlpha	<- d3Elo[c(3, 6, 4, 1, 5, 2)]
d3demo$rank	<- d3EloAlpha

d2RankDiff	<- 0*table(diadema2)%*%t(table(diadema2))
d3RankDiff	<- 0*table(diadema3)%*%t(table(diadema3))
d2AgeDiff	<- 0*table(diadema2)%*%t(table(diadema2))
d3AgeDiff	<- 0*table(diadema3)%*%t(table(diadema3))

for(i in diadema2){
	for(j in diadema2){
		actorAge		<- d2demo[d2demo$ID == i,]$ageCat
		actorRank		<- d2demo[d2demo$ID == i,]$rank
		recipAge		<- d2demo[d2demo$ID == j,]$ageCat
		recipRank		<- d2demo[d2demo$ID == j,]$rank
		d2AgeDiff[i, j]	<- abs(actorAge - recipAge)
		d2RankDiff[i, j]	<- abs(actorRank - recipRank)
	}
}

for(i in diadema3){
	for(j in diadema3){
		actorAge		<- d3demo[d3demo$ID == i,]$ageCat
		actorRank		<- d3demo[d3demo$ID == i,]$rank
		recipAge		<- d3demo[d3demo$ID == j,]$ageCat
		recipRank		<- d3demo[d3demo$ID == j,]$rank
		d3AgeDiff[i, j]	<- abs(actorAge - recipAge)
		d3RankDiff[i, j]	<- abs(actorRank - recipRank)
	}
}

######################
### Create network ###
######################
d2plymat	<- createNet(socialData2$actor, socialData2$subject, socialData2$behavior, 'Play', subjects = diadema2, type = 'duration', durs = socialData2$duration)
d3plymat	<- createNet(socialData2$actor, socialData2$subject, socialData2$behavior, 'Play', subjects = diadema3, type = 'duration', durs = socialData2$duration)
f2plymat	<- createNet(socialData2$actor, socialData2$subject, socialData2$behavior, 'Play', subjects = fulvus2, type = 'duration', durs = socialData2$duration)
f3plymat	<- createNet(socialData2$actor, socialData2$subject, socialData2$behavior, 'Play', subjects = fulvus3, type = 'duration', durs = socialData2$duration)

d2grmmat	<- createNet(socialData2$actor, socialData2$subject, socialData2$behavior, 'Groom', subjects = diadema2, type = 'duration', durs = socialData2$duration)
d3grmmat	<- createNet(socialData2$actor, socialData2$subject, socialData2$behavior, 'Groom', subjects = diadema3, type = 'duration', durs = socialData2$duration)
f2grmmat	<- createNet(socialData2$actor, socialData2$subject, socialData2$behavior, 'Groom', subjects = fulvus2, type = 'duration', durs = socialData2$duration)
f3grmmat	<- createNet(socialData2$actor, socialData2$subject, socialData2$behavior, 'Groom', subjects = fulvus3, type = 'duration', durs = socialData2$duration)

d2plynet	<- as.network.matrix(d2plymat, directed = TRUE, names.eval = 'numply', ignore.eval = FALSE)
d3plynet	<- as.network.matrix(d3plymat, directed = TRUE, names.eval = 'numply', ignore.eval = FALSE) 
f2plynet	<- as.network.matrix(f2plymat, directed = TRUE, names.eval = 'numply', ignore.eval = FALSE)
f3plynet	<- as.network.matrix(f3plymat, directed = TRUE, names.eval = 'numply', ignore.eval = FALSE)

d2grmnet	<- as.network.matrix(d2plymat, directed = TRUE, names.eval = 'numply', ignore.eval = FALSE)
d3grmnet	<- as.network.matrix(d3plymat, directed = TRUE, names.eval = 'numply', ignore.eval = FALSE) 
f2grmnet	<- as.network.matrix(f2plymat, directed = TRUE, names.eval = 'numply', ignore.eval = FALSE)
f3grmnet	<- as.network.matrix(f3plymat, directed = TRUE, names.eval = 'numply', ignore.eval = FALSE)

set.vertex.attribute(d2plynet, 'elo', d2demo$rank)
set.vertex.attribute(d2plynet, 'sex', as.character(d2demo$sex))
set.vertex.attribute(d2plynet, 'obstime', allFocalDurations[[1]]$duration)
set.vertex.attribute(d2plynet, 'age', d2demo$age)
set.edge.value(d2plynet, 'grm', d2grmmat) 
set.edge.value(d2plynet, 'rankdiff', d2RankDiff)
set.edge.value(d2plynet, 'agediff', d2AgeDiff)
set.edge.value(d2plynet, 'dyadObs', allObsMatList[[1]])
set.edge.value(d2plynet, 'logObs', log(60*allObsMatList[[1]]))

set.vertex.attribute(d3plynet, 'elo', d3demo$rank)
set.vertex.attribute(d3plynet, 'sex', as.character(d3demo$sex))
set.vertex.attribute(d3plynet, 'obstime', allFocalDurations[[2]]$duration)
set.vertex.attribute(d3plynet, 'age', d3demo$age)
set.edge.value(d3plynet, 'grm', d3grmmat) 
set.edge.value(d3plynet, 'rankdiff', d3RankDiff)
set.edge.value(d3plynet, 'agediff', d3AgeDiff)
set.edge.value(d3plynet, 'dyadObs', allObsMatList[[2]])
set.edge.value(d3plynet, 'logObs', log(60*allObsMatList[[2]]))

##############
### ERGM's ###
##############

model1	<- ergm(d2plynet ~ sum + mutual(form = 'min') + nonzero + transitiveweights('min', 'max', 'min') + 
	nodematch('sex', form = 'sum') + edgecov(d2plynet, 'agediff', form = 'sum') + nodecov('age', form = 'sum') + 
	edgecov(d2plynet, 'rankdiff', form = 'sum') + nodecov('elo', form = 'sum') + edgecov(d2plynet, 'grm', form = 'sum') + edgecov(d2plynet, 'logObs', form = 'sum'), response = 'numply', reference = ~Poisson,
	control = control.ergm(MCMC.interval = 1000, MCMLE.maxit = 200, init.method = 'CD', MCMC.samplesize = 1000, seed = 32164))

summary(model1)
mcmc.diagnostics(model1)


model2	<- ergm(d3plynet ~ sum + mutual(form = 'min') + nonzero + transitiveweights('min', 'max', 'min') + 
	nodematch('sex', form = 'sum') + edgecov(d3plynet, 'agediff', form = 'sum') + nodecov('age', form = 'sum') + 
	edgecov(d3plynet, 'rankdiff', form = 'sum') + nodecov('elo', form = 'sum') + edgecov(d3plynet, 'grm', form = 'sum') + edgecov(d3plynet, 'logObs', form = 'sum'), response = 'numply', reference = ~Poisson,
	control = control.ergm(MCMC.interval = 1000, MCMLE.maxit = 200, init.method = 'CD', MCMC.samplesize = 1000, seed = 32164))

summary(model2)
mcmc.diagnostics(model2)
### Gather the SFB Eliminator Projections
#Source1
source1.sfb8 <- gsheet2tbl(
  'https://docs.google.com/spreadsheets/d/1F-Uyplgbad08BaVVFnrzr3cCeiC8I6qCtpn9vqRkVSc/edit#gid=1142013131'
)
source1.sfb8$Projector <- "source1"
names(source1.sfb8)[3]<-"Proj"
#Source2
source2.sfb8 <- gsheet2tbl(
  'https://docs.google.com/spreadsheets/d/1u-oKhnDp2ZLW6Q9ZomA2iRArYrif3ee3haj2-_TBI2k/edit#gid=1854830971'
)
source2.sfb8$Projector <- "source2"
names(source2.sfb8)[3]<-"Proj"


#Source3
source3.sfb8 <- gsheet2tbl(
  'https://docs.google.com/spreadsheets/d/1me8J0aq8V6Sa0YNIh7h63ThZ1pwucJJHjkxx1R5cvVE/edit#gid=265455049'
)
source3.sfb8$Projector <- "source3"
names(source3.sfb8)[3]<-"Proj"


#Source4
source4.sfb8 <- gsheet2tbl(
  'https://docs.google.com/spreadsheets/d/10CSVxs3wlou51hM6vdI-k6b74QP7wKKI2CL9cyB3dyY/edit#gid=896714279'
)
source4.sfb8$Projector <- "source4"
names(source4.sfb8)[3]<-"Proj"



#Source5
source5.sfb8 <- gsheet2tbl(
  'https://docs.google.com/spreadsheets/d/1wNBhCG1eKAUQ2oX3JkcgyUz9km-3DnDwTke8f5ZaNsk/edit#gid=1178427244'
)
source5.sfb8$Projector <- "source5"
names(source5.sfb8)[3]<-"Proj"


#Combine
sfb8.proj <- rbind(source1.sfb8,source2.sfb8,source3.sfb8, source4.sfb8, source5.sfb8)

#Adjust Julian Edelman (using Totals)
sfb8.proj$Proj<- 
  ifelse(sfb8.proj$Player == "Julian Edelman",
         (sfb8.proj$Proj/16)*12,
         sfb8.proj$Proj)

sfb8.averages <- sfb8.proj %>% select(-Projector) %>% 
  group_by(Player, Pos) %>%
  summarise_all(mean,na.rm=T)
names(sfb8.averages)[3]<-"avg.Proj"
sfb8.proj <- merge(sfb8.proj, sfb8.averages, by =c("Player","Pos"))

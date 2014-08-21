rm(list = ls())

library(data.table)

system.time(project <- fread(input="Z:/consulting/raw_data/campaigns.csv", 
            stringsAsFactors=FALSE, na.strings="\\N", sep=",", header=FALSE)) #hier funktioniert tatsächlich nur "<-" und nicht "="
setnames(project, c('id', 'projectId', 'name', 'parentId', 'projectType'))

proLevel1 = project[project$parentId==1,]

idxDown = which(proLevel1$name %in% c("SEM", "Kooperationen", "Affiliate"))
for(i in idxDown){
  if(proLevel1$name[i] == "SEM"){
    proLevel2 = project[proLevel1$projectId[i]==project$parentId,]
    proLevel3 = data.table()
    for(j in 1 : nrow(proLevel2)){
      proLevel3 = rbind(proLevel3, project[proLevel2$projectId[j]==project$parentId,])
    }
    for(j in 1 : nrow(proLevel3)){
      if(substr(proLevel3$name[j], 1, 3) == "SB_"){
        proLevel3$name[j] = "SEM - Brand"
      }else if(substr(proLevel3$name[j], 1, 3) == "DB_"){
        proLevel3$name[j] = "SEM - Remarketing"
      }else{
        proLevel3$name[j] = "SEM - Generisch"
      }
    }
    proLevel1[i, 1] = -1 #id auf -1 setzen um spaeter zu loeschen
    toAdd1 = proLevel3
  }
  if(proLevel1$name[i] == "Kooperationen"){
    proLevel2 = project[proLevel1$projectId[i]==project$parentId,]
    proLevel3 = project[proLevel2$projectId[1]==project$parentId,]
    for(j in 1 : nrow(proLevel3)){
      if(proLevel3$name[j] %in% c("focus", "Focus", "FOCUS", "focus.de", "focus_display")){
        proLevel3$name[j] = "Kooperationen - Focus"
      }else if(proLevel3$name[j] %in% c("imimmonet+monet", "immonet", "Immonet")){
        proLevel3$name[j] = "Kooperationen - Immonet"
      }else if(proLevel3$name[j] %in% c(" immoscout24", "immoscout", "immoscout_extra", "immoscout_extra_b", "immoscout24", "immoscout24?_s_peprm", "immoscout24?adKeyword", "immoscout24_af", "immoscout24_kaufplaner", "immoscout2gntm", "immobilienscout")){
        proLevel3$name[j] = "Kooperationen - Immoscout24"
      }else if(proLevel3$name[j] %in% c("immowelt", "Immowelt", "immowelt_flash_kind", "ImmoWeltRegio0601")){
        proLevel3$name[j] = "Kooperationen - Immowelt"
      }else{
        proLevel3$name[j] = "Kooperationen - Rest"
      }
    }
    proLevel1[i, 1] = -1 #id auf -1 setzen um spaeter zu loeschen
    toAdd2 = proLevel3
  }
  if(proLevel1$name[i] == "Affiliate"){
    proLevel2 = project[proLevel1$projectId[i]==project$parentId,]
    for(j in 1 : nrow(proLevel2)){
      if(proLevel2$name[j] %in% c("Affiliate Financeads", "Affiliate Zanox")){
        proLevel2$name[j] = "Affiliate - Rest"
      }else if(proLevel2$name[j] == "Affiliate Partnerprogramm"){
        proLevel2$name[j] = "Affiliate - Partnerprogramm"
      }
    }
    proLevel1[i, 1] = -1 #id auf -1 setzen um spaeter zu loeschen
    toAdd3 = proLevel2
  }
}
proLevel1 = proLevel1[-which(proLevel1$id == -1), ] #id mit -1 loeschen
proLevel1 = rbind(proLevel1, toAdd1, toAdd2, toAdd3)
levelName = proLevel1$name
rm(proLevel2, proLevel3, toAdd1, toAdd2, toAdd3, idxDown)

sets = list()
parID = project$parentId
for(i in 1 : nrow(proLevel1)){
  proLevel2 = project[proLevel1$projectId[i]==project$parentId,]
  oldSet = c()
  idx = c()
  set = proLevel2$projectId
  check = FALSE
  while(check==FALSE){ #den Baum runterhangeln
    set = c(oldSet,set)
    oldSum = sum(idx)
    idx = is.element(el = parID, set = set)
    newSum = sum(idx)
    check = newSum==oldSum
    oldSet = project$projectId[idx]
  }
  sets[[i]] <- oldSet
}
sets = list(sets, levelName)
save(sets, file = "Z:/consulting/r_data/sets.RData")

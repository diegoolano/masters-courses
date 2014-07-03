#describe pre-process steps

#load file
dd <-read.csv("finaldata-withblueislands.csv", na.strings = '', stringsAsFactors = FALSE)
dd[which(dd$blueisland == 1),c(2:5,32:33,96)]

colnames(dd)
# [1] "X"                         "fips"                      "dem"                       "rep"                      
# [5] "isdem"                     "lesshs"                    "hs"                        "leastbach"                
# [9] "graduate"                  "enrollp"                   "earnings"                  "educationi"               
# [13] "incomei"                   "white"                     "afric"                     "nativ"                    
# [17] "asian"                     "other"                     "latin"                     "povo65"                   
# [21] "popul"                     "preschl"                   "belowpov"                  "gini"                     
# [25] "childpov.x"                "wmanage"                   "wsales"                    "wfarm"                    
# [29] "wconstruct"                "wtransport"                "state.x"                   "state.y"                  
# [33] "county"                    "fairpoorhealth"            "fphq"                      "sickdays"                 
# [37] "sickdaysq"                 "mentaldays"                "mentaldaysq"               "lowbirthrate"             
# [41] "lowbirthrateq"             "smokers"                   "smokersq"                  "adultobese"               
# [45] "adultobeseq"               "phyinactive"               "phyinactiveq"              "excsdrinking"             
# [49] "excsdrinkingq"             "motordeath"                "motordeathq"               "stdsper100"               
# [53] "stdsper100q"               "teenbirthrate"             "teenbirthrateq"            "uninsured.x"              
# [57] "uninsuredq"                "physicianratio"            "ambulatorycare"            "ambulatorycarq"           
# [61] "diabeteshba1c"             "diabeteshba1cq"            "mammography"               "mammographyq"             
# [65] "posthighq"                 "unemployed"                "unemployedq"               "childpovq"                
# [69] "noemotionalsupport"        "noemotionalsupportq"       "singleparent"              "singleparentq"            
# [73] "violentcrime"              "violentcrimeq"             "particulatedaysq"          "airpollutionq"            
# [77] "recfac"                    "recfacq"                   "limitedaccesshealthyfood"  "limitedaccesshealthyfoodq"
# [81] "fastfoodresp"              "fastfoodresq"              "under18"                   "over65"                   
# [85] "noenglish"                 "female"                    "rural"                     "diabectic"                
# [89] "mphratio"                  "healthcosts"               "dentistratio"              "freelunch"                
# [93] "illiteracy"                "drivealone"                "healthyfood"               "blueisland" 

#get rid of state, county, etc columns and make fips the row.names
dd <- dd[,c(2:30,34:96)]
row.names(dd) <- dd$fips

#get rid of fips column
dd <- dd[,2:92]

#get rid of q columns 
qcols = c(32,34,36,38,40,42,44,46,48,50,52,55,57,59,60,62,63,65,67,69,70,71,73,75,77)
dd <- dd[,-qcols]

#change ratio columns to zero or to 1 / number.   for mphratio (mental health provider), dentist and physicianratio
dd.3 <- dd
dd <- dd.3

colnames(dd)[ratiocols]
#[1] "physicianratio" "mphratio"    "dentistratio

ratiocols <- c(42,59,61)
for(i in 1:nrow(dd)){
  currenti <- i
  for(ri in ratiocols){    
    pieces <- strsplit(dd[currenti,ri],":")[[1]]
    #only for those formatted as ratios ( not including few missing NAs)
    if(length(pieces) > 1)
    {  
      if( pieces[2] == "1"){ 
        dd[currenti,ri] = 1 / as.numeric(pieces[1]) 
      }
      else
      {
        if( pieces[2] == "0")
        {
          dd[currenti,ri] = 0
        }
        else
        {
          print("hmmm, ratio not ending in 0 or 1 so leave as is ")
          print(as.numeric(pieces[1]))
        }
      }
    }
  }
}

#make these columns numeric
dd[,42] <- as.numeric(dd[,42])
dd[,59] <- as.numeric(dd[,59])
dd[,61] <- as.numeric(dd[,61])

#at end, make one regression set ( target = dem / dem + rep ) and classification one one ( target = dem/rep)
dd.4 <- dd
dim(dd)

#for regression
dd[,1] <- dd[,1] / ( dd[,1] + dd[,2] )
dd <- dd[,c(1,4:66)]
write.csv(dd,"finaldata-regressionset.csv",row.names=TRUE)

#for classification
dim(dd.4)
summary(dd.4)
#get rid of dem and rep voting columns
dd.4 <- dd.4[,c(3:66)]
#colnames(dd.4)[1] <- "winner"
dd.4$winner <- dd.4$isdem
dd.4 <- dd.4[,2:65]

dd.4[which(dd.4$winner == "no"),64] <- "rep"
dd.4[which(dd.4$winner == "yes"),64] <- "dem"
dd.4[,64] <- as.factor(dd.4[,64])

#get rid of bogey q
dd.4 <- dd.4[,-27]
write.csv(dd.4,"finaldata-classificationset.csv",row.names=TRUE)

#don't do this.. scale data for both and save.... do it in the analysis part



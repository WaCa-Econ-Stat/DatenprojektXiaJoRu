###1
df1 <- data.frame(Betrieb = data$ID_betnr,HO = data$bhomeoff)
head(df1,1)
df2 <- data.frame(Betrieb = 1, HO = 0) #initialisieren mit Erstem Betrieb
for(i in 1:nrow(df1)){
  if( ! (  df1[i,1] %in% df2[,1] ) ){
    df2 <- rbind(df2, df1[i,])
  }
}
#Wenn die Betriebnummer noch nicht in der bisherigen Spalte 1 des neuen
#Datenrahmens aufgenommen wurde, dann wird die Zeile hinzugefügt 

nrow(df2) == length(unique(df1[,1])) #TRUE heißt alle Betirb-Duplikate eliminert
df2 <- df2[-nrow(df2), ] #Die NA-Betriebe kommen auch 1 mal ganz am Ende vor
df2$HO <- factor(df2$HO, levels=c(0,1), labels = c("Nein","Ja"))
Frequency_Table <- prop.table(table(df2$HO))
Share_HO <- as.numeric(Frequency_Table[2])
Share_HO
barplot(Frequency_Table, main="Erlauben Betriebe Home Office?",
        col=c("darkred","darkgreen"), horiz = T)
###2
df3 <- cbind(df1, Nutzen = data$mheim)[df1$HO==1,]
head(df3,1)
df4 <- data.frame(Betrieb = 6, HO = 1, Nutzen = 1)
for(i in 2:nrow(df3)){
  enthält <- F
  for(j in 1:3){
    if( is.na(df3[i,j]) ){enthält <- T}
  }
  if( !enthält){
    df4 <- rbind(df4, df3[i,])
  }
}
df4$Nutzen <- factor(df4$Nutzen, levels=c(0,1), labels= c(
  "Nutzt Möglichkeit nicht", "Nutzt Möglichkeit"
))
Frequency_Use <- prop.table(table(df4$Nutzen))
Frequency_Use #Knapp 1/4 genau 23.63% nutzen die HO Möglichkeit
barplot(Frequency_Use, col=c("darkblue", "lightblue"),
        main= "Nutzt der Mitarbeiter die Möglichkeit zum Home Office?")

###3
Relevante_Variablen <- character()
for(i in 1:length(colnames(data))){
  if( grepl("mheimnein_", colnames(data)[i] )   ){
    Relevante_Variablen <- c( Relevante_Variablen, colnames(data)[i])
  }
} #alle Variablen, die mheimnein_ enthalten, also hier zweckdienlich sind
Index <- which(colnames(data)  %in% Relevante_Variablen )
df5 <- na.omit( cbind(df1, data$mheim, data[,Index]) )
nj <- numeric()
for(j in 4:ncol(df5)){
  nj <- c(nj, sum(df5[,j]) )
}
Relevante_Variablen2 <- c("Erl.", "Tech.", "unmö.", "vorg.", "trenn.", "team", "karr.")
Gründe <- data.frame(Relevante_Variablen2, nj)[order(nj), ]
Gründe
barplot(Gründe$nj, names.arg= Gründe$Relevante_Variablen2,
        col=c(rep("darkblue",2),rep("blue",3),rep("lightblue",2)) ,
        main="Grund für kein Home Office")
#Die meisten geben einfach nur an, dass es Ihnen nicht möglich ist,
#am zweit häufigsten ist der Grund, dass Vorgesetzten die Anwesenheit "sehr wichtig" ist

###4







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
df6 <- factor(na.omit(subset( data.frame( 
                         wunsch3 = data$mheimwunsch, 
                         HO = data$bhomeoff),
              HO == 0))[,1],levels=1:3, labels = c(rep("Wunsch",2),"Kein Wunsch") )
#df6 ist ein Vektor der alle mheimwunsch für HO = 0 herausfindet, und diese dann in
#Wunscht besteht: Werte 1,2 und Wunsch besteht nicht: Wert 3 aufteilt
barplot(table(df6), main = "wünschen sich Arbeitnehmer \n in Betrieben, die kein HO anbieten
        von, zu Hause aus zu arbeiten?",
        col=c("lightblue","darkblue"))
###5
Wichtige_Variablen <- character()
for(i in 1:ncol(data)){
  if( grepl("mheimwunsch_", colnames(data)[i])){
    Wichtige_Variablen <- c(Wichtige_Variablen, colnames(data)[i])
  }
}
Index2 <- which(colnames(data) %in% Wichtige_Variablen)
df7 <- na.omit(data[, Index2])
Gründe2 <- c("fahren","freiz.","fam","qual.","h_erhöhen")
Häufigkeiten <- apply(df7, 2, sum)
barplot(Häufigkeiten, names.arg=Gründe2,
        col="lightblue")


###2###
###1
Branchen <- factor(as.vector(data$branche), levels= 1:5, labels = c(
  "Verarbeitendes Gewerbe", "Metall, Elektro, Fahrzeugbau ", "Handel, Verkehr, Nachrichten ",
  "Unternehmensnahe DL, Finanz. ", "Information/Kommunikation "
)) #enthält noch NA
head(Branchen)
Modell_Konstante <- lm(na.omit((data.frame(HO = as.vector(data$bhomeoff),B = Branchen))), formula=
                         HO~B)
summary(Modell_Konstante)   #Referenzkategorie ist hier das Verarbeitende Gewerbe

###Modell ohne Konstante
###2
Durchschnitte <- sapply(as.vector( unique(Branchen)[1:5] ), function(z) mean(df8$data.bhomeoff[df8$Branchen == z]) )
print(Durchschnitte)

###3
Modell <- lm(data=na.omit(data.frame(Größe = as.vector(data$size),
                                     HO = as.vector(data$bhomeoff), Branchen)),
             formula= HO ~ Größe+Branchen)
stargazer(Modell, type="text") #Dummy wieder Verarbeitendes Gewerbe

# Bei Infor sowie Handel bleibt es in etwas gleich, aber 
#Metall erhöht es sich und bei DL verkleinert es sich, das liegt daran, dass
#Metall: Davir (ohne Größe) war der Koeff ca. 0.05, dieser wurde aber zu hoch einge-
#schätzt, weil Größe positiv mit Metall korrelliert und einen positiven Einfluss auf
#Home Office hat (+ und +), damit wurde Metall mit 0.05 überschätzt. -> OmittedVariableBias
Größe <- as.vector(data$size)
Metall <- Branchen
df8 <- na.omit( data.frame(Größe,Metall) )

Br <- as.vector(unique(Branchen))[-length(unique(Branchen))]
Br
df8$Metall <- factor(df8$Metall, levels = Br, labels = c(rep(0,2),1,rep(0,2)))
mean(df8$Größe[df8$Metall==0]) #Durchschnittsgröße nicht Metall 
mean(df8$Größe[df8$Metall==1]) #Durchschnittsgröße Metall > als von nicht-Metall
#damit haben wir die positive Korrelation gezeigt


###DL-Sektor: Hier ist es genau anders herum, Größe hat positiven Einfluss (+), aber 
#DL und Größe korrelieren negativ miteinander (-) damit gilt:
#Beta_DL wurde zu niedrig eingeschätzt 

###1





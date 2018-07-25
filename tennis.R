zip.file <- 'TennisData_ATP.zip'
a<-atp_matches_1968
#download.file('https://github.com/JeffSackmann/tennis_atp/archive/master.zip', destfile = zip.file)}
#combining the files that I downloaded to make a main data frame 
temp<-list.files(pattern="*.csv")
temp1<-read.csv(temp[1], row.names = NULL, header=TRUE)
for (i in 2:51){
  temp2<-read.csv(temp[i], row.names = NULL, header=TRUE)
  temp1<-rbind(temp1,temp2)
}
#getting rid of unecessary columns 
tennismachine<-temp1[,-c(1,8,18)]
#making a correlation function
corr<-function(a,b){
  temp<-cor(a,b, use = "complete.obs")
  return(temp)
}
#looking at the unique surfaces to subset
unique(tennismachine$surface)
#subsetting by surfaces 
grass<-subset(tennismachine, tennismachine$surface=="Grass")
clay<-subset(tennismachine, tennismachine$surface=="Clay")
hard<-subset(tennismachine, tennismachine$surface=="Hard")
carpet<-subset(tennismachine, tennismachine$surface=="Carpet")
None<-subset(tennismachine, tennismachine$surface=="None")
#looking at draw sizes in the hard courts 
unique(hard$draw_size)
#I got rid of any draw sizes wich are less than 48
hard<-subset(hard, hard$draw_size>48)
#getting rid of columns
unique(hard$draw_size)
hard<-hard[,-(1:2)]
hard<-hard[,-c(2,3,10)]
hard<-hard[,-c(20:22)]
hard<-hard[,-c(4,19,20)]
#checking a few correlations 
corr(hard$loser_seed, hard$winner_seed)
corr(hard$loser_ht, hard$loser_age)
#plotting variables to see if there is a correlation
#I wanted to compare plots side by side
par(mfrow = c(1,2))
plot(hard$l_bpFaced, hard$l_bpSaved)
plot(hard$w_bpFaced, hard$w_bpSaved)
#LSRL line 
abline(lm(hard$l_bpSaved~hard$l_bpFaced),col="blue")
abline(lm(hard$w_bpSaved~hard$w_bpFaced),col="blue")
lm(hard$l_bpSaved~hard$l_bpFaced)
lm(hard$w_bpSaved~hard$w_bpFaced)
#finding correlation 
cor(hard$l_1stWon, hard$w_1stWon)


#

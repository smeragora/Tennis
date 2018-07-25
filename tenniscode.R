#downloading the files
zip.file <- 'TennisData_ATP.zip'
a<-atp_matches_1968
  #download.file('https://github.com/JeffSackmann/tennis_atp/archive/master.zip', destfile = zip.file)}
#combining the files to make a main data frame 
temp<-list.files(pattern="*.csv")
temp1<-read.csv(temp[1], row.names = NULL, header=TRUE)
for (i in 2:51){
temp2<-read.csv(temp[i], row.names = NULL, header=TRUE)
temp1<-rbind(temp1,temp2)
}
#subrating of columns that I will not use 
main<-temp1[,-c(1,8,18)]
#changing the type of for the files 
as.data.frame(files)
#making a pie chart to see which courts are played on the most 
library(ggplot)
library(plyr)
#subsetting by surface
hard<-subset(main, main$surface=="Hard")
clay<-subset(main, main$surface=="Clay")
grass<-subset(main, main$surface=="Grass")
carpet<-subset(main, main$surface=="Carpet")
none<-subset(main, main$surface=="None")
#putting the graphs next to eachother 
par(mfrow = c(2,3))
#making a=sure the histogram is normal 
hist(main$winner_ht, main="winner height dist")
hist(temp1$loser_ht, main="loser height dist")
hist(temp1$winner_age, main="winner age dist")
hist(temp1$loser_age, main="loser age dist")
#I wanted to see if height is a variable that gives an adv to the players 
plot(temp1$loser_ht,temp1$winner_ht, main="loser ht vs winner ht", xlab="loserht", ylab="winnerht")
#by plotting them I could not tell so I decided to run a T-Test
#Here I am just checking the mean heights of winners and losers 
mean(temp1$loser_ht, na.rm=TRUE)
mean(temp1$winner_ht, na.rm=TRUE)
#running the two sample t-test 
t.test(temp1$loser_ht,temp1$winner_ht)
corr<-cor((temp1$loser_ht),(temp1$winner_ht), use="complete.obs")
#checking the unique courts 
unique(main$surface)
#checking frequency of the courts
dist=count(main$surface)
#getting rid of the blank cell and its frequency 
dist<-dist[-1,]
#putting the frequency and labels in a vector, so I can call it when I plot 
x=c(19826,61004,18514,65888,80)
labels= c("Carpet", "Clay", "Grass", "Hard", "None")
#making the pie chart 
pie(dist$freq,labels, main="Surfaces")
#does rank matter 
#running another t test for 2 sample
#skewed right
hist(main$loser_rank)
#also skwewed right, but I'm going to run the test 
hist(main$winner_rank)
t.test(main$loser_rank,main$winner_rank)


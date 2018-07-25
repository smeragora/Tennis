#row binding 
recentplays<-rbind(atp_matches_2016,atp_matches_2017)
#plotting data
plot(recentplays$winner_seed,recentplays$match_num)
#There is very little correlation
corr<-cor(as.numeric(recentplays$winner_seed), as.numeric(recentplays$match_num), use="complete.obs")
method = c("pearson")
#plotting the ranks to see if there is a correlation
plot(recentplays$winner_rank,recentplays$loser_rank)
#There is a pretty low correlation of 0.454
corr1<-(cor(as.numeric(recentplays$winner_rank), 
         as.numeric(recentplays$loser_rank), use="complete.obs"))
#I wanted to play around with Federer's data because he's my favorite :D
#so I substetted out his wins 
Rogercourts<-subset(recentplays, recentplays$winner_name=="Roger Federer")
#I ploted the his rank vs his opponents 
plot(Rogercourts$winner_rank,Rogercourts$loser_rank)
#then I plotted his seed vs his opponents 
plot(Rogercourts$winner_seed,Rogercourts$loser_seed)
#then I wanted to see if there was association in the break points he faced vs the losers rank 
plot(Rogercourts$i_bpfaced,Rogercourts$loser_rank)

#Later I decided to play around with ggplot 
library(ggplot2)
#I decided to play around with ggplot 
#box plot 
#making a density curuve. Roger's seed was always really low!
ggplot(data = Rogercourts)+geom_density(aes(x=winner_seed), fill="blue")
#a butterfly plot
ggplot(Rogercourts, aes(y=winner_seed, x=match_num))+ geom_point()+geom_violin()
#as the number of matches he plays increased his seed gets lower
#the fluctuation of his seed is also less as he plays more matches 
ggplot(Rogercourts, aes(y=winner_seed, x=match_num))+ geom_line()
#finding out the unique surfacces he plays on 
unique(Rogercourts$surface)
#checking frequency of the courts
counting=count(Rogercourts$surface)
#getting rid of the blank cell and its frequency 
counting<-counting[-1,]
#putting the frequency and labels in a vector, so I can call it when I plot 
x=c(3,22,49)
labels= c("Hard","Clay", "None")
#making the pie chart 
pie(counting$freq,labels)


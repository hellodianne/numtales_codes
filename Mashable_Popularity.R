vars = c("url", "timedelta", "n_tokens_title", "n_tokens_content", "n_unique_tokens", 
"n_non_stop_words", "n_non_stop_unique_tokens", "num_imgs", "num_videos", "average_token_length",
"num_keywords", "data_channel_is_lifestyle", "data_channel_is_entertainment", "data_channel_is_bus",
"data_channel_is_socmed", "data_channel_is_tech", "data_channel_is_world",
"self_reference_min_shares", "self_reference_max_shares", "self_reference_avg_sharess",
"weekday_is_monday", "weekday_is_tuesday", "weekday_is_wednesday", "weekday_is_thursday",
"weekday_is_friday", "weekday_is_saturday", "weekday_is_sunday", "global_subjectivity",           
"global_sentiment_polarity", "global_rate_positive_words", "global_rate_negative_words",
"shares")

data = read.csv("OnlineNewsPopularity.csv")
data = data[,(names(data)%in%vars)]

#add category
data$category = NA

for (i in 1:length(data$data_channel_is_world)){
	if (data$data_channel_is_world[i] == 1){
		data$category[i] = "world"
	}
}

for (i in 1:length(data$data_channel_is_tech)){
	if (data$data_channel_is_tech[i] == 1){
		data$category[i] = "tech"
	}
}

for (i in 1:length(data$data_channel_is_socmed)){
	if (data$data_channel_is_socmed[i] == 1){
		data$category[i] = "socmed"
	}
}
for (i in 1:length(data$data_channel_is_bus)){
	if (data$data_channel_is_bus[i] == 1){
		data$category[i] = "bus"
	}
}
for (i in 1:length(data$data_channel_is_lifestyle)){
	if (data$data_channel_is_lifestyle[i] == 1){
		data$category[i] = "lifestyle"
	}
}
for (i in 1:length(data$data_channel_is_entertainment)){
	if (data$data_channel_is_entertainment[i] == 1){
		data$category[i] = "entertainment"
	}
}

data$dayposted = NA 

for (i in 1:length(data$weekday_is_monday)){
	if (data$weekday_is_monday[i] == 1){
		data$dayposted[i] = "monday"
	}
}

for (i in 1:length(data$weekday_is_sunday)){
	if (data$weekday_is_sunday[i] == 1){
		data$dayposted[i] = "sunday"
	}
}

for (i in 1:length(data$weekday_is_saturday)){
	if (data$weekday_is_saturday[i] == 1){
		data$dayposted[i] = "saturday"
	}
}

for (i in 1:length(data$weekday_is_friday)){
	if (data$weekday_is_friday[i] == 1){
		data$dayposted[i] = "friday"
	}
}

for (i in 1:length(data$weekday_is_thursday)){
	if (data$weekday_is_thursday[i] == 1){
		data$dayposted[i] = "thursday"
	}
}

for (i in 1:length(data$weekday_is_wednesday)){
	if (data$weekday_is_wednesday[i] == 1){
		data$dayposted[i] = "wednesday"
	}
}

for (i in 1:length(data$weekday_is_tuesday)){
	if (data$weekday_is_tuesday[i] == 1){
		data$dayposted[i] = "tuesday"
	}
}

#outlieres
outliersTop = data[which(data$shares >= 13000),]
outliersBottom = data[which(data$shares <= 500),]
#remove outliers?
data = data[which(data$shares > 500 & data$shares < 13000),]

#histogram
world = data[which(data$category=="world"),]
tech = data[which(data$category=="tech"),]
socmed = data[which(data$category=="socmed"),]
bus = data[which(data$category=="bus"),]
lifestyle = data[which(data$category=="lifestyle"),]
entertainment = data[which(data$category=="entertainment"),]


par(mfrow=c(2,3))
hist(world$shares, breaks=10000, main="World", xlab="")
hist(tech$shares,  breaks=10000, main="Tech", xlab="Number of shares")
hist(socmed$shares,  breaks=10000, main="SocialMedia", xlab="")
hist(bus$shares,  breaks=10000, main="Business", xlab="")
hist(lifestyle$shares,  breaks=10000, main="Lifestyle", xlab="Number of shares")
hist(entertainment$shares,  breaks=10000, main="Entertainment", xlab="")


#kruskal.test
kruskal.test(shares ~ category, data=mashdata)


#barplot

meanperC = aggregate(data$shares, by=list(Category=data$category), FUN=mean)
barMean = barplot(meanperC$x, names.arg=c("Bus", "Ent", "Lif", "SM", "Tech", "World"), col=c("lightblue4"))


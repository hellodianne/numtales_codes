#exploring data
oscar = read.csv("oscars.csv")
#1631 observations, 62 variables

nonvars = c("PP", "DD","FF", "Movie", "Ams", "Afs", "Cos", "Sco", "Sou", "For", "Anf", "Gdr", "Gmc", "Gd","Gf1", "Gf2","PGA", "DGA", "SAF","Gdr", "Gmc", "Gd", "Gf1", "Gf2", "PGA", "DGA", "SAF", "PN", "PD", "DN", "DP", "DPrN", "DPrW", "MN", "MP", "MPrN", "MPrW", "FN", "FP", "FPrN", "FPrW")

#did not include other nominations such as costume, score, song, editing etc.
#also removed golden globe awards(such as for females etc) except for the male actor

oscar = oscar[, !(names(oscar)%in%nonvars)]

#get the movies where lead actor is the nominee
oscar = subset(oscar, oscar$MM == 1)

#change values of Ch to binary
for (i in 1: length(oscar$Ch)){
	if (oscar$Ch[i] == 2){
	oscar$Ch[i] = 0
	}
}

#split by release years of movie

train = subset(oscar, oscar$Year < 2005)
test = subset(oscar, oscar$Year == 2005)

#test2 added because in this dataset, movies release from 2006 doesn't have any oscar awards yet, probably because this dataset is trying to predict the 79th awards'

test2 = subset(oscar, oscar$Year == 2006)



#BASELINE MODEL

#im going to use a baseline model, which if number of nominations(excluding best actor nominee) is higher than 5, 
#it is more likely that the nominee will win

#subtract the lead actor nomination from the total number of nominees
train$Nom = train$Nom - 1

#get median of the number of nomination (since histogram is skewed)
median(train$Nom)

#now we guess
baselineTrain = subset(train, train$Nom >= 5)
table(baselineTrain$Ch)


#building the model
model1 = glm(Ch ~. - Comp - Name - MM - Year - No, data=train, family=binomial)
#examine model1
summary(model1)

#model doesn't look good lets find multicollinear variables

#find for CORRELATION by eye examination
cor(oscar[c("Nom", "Pic", "Dir", "Aml", "Afl", "Ams", "Afs", "Scr", "Cin", "Art", "Cos", "Sco", "Son", "Edi", "Sou", "For", "Eff", "Mak", "Dan", "AD", "PrNl", "PrWl", "PrNs", "PrN", "PrW", "Gm1", "Gm2", "SAM")])


#PrNl and PrN are collinear; PrW and PrWl are collinear, which made sense because of double counting; Pic and Nom are 0.75 collinear

#lets try to remove them and build model1 again, im gonna remove Aml due to double counting the nominations

model1 = glm(Ch ~. - Comp - Name - MM - Year -No - Nom - PrN - PrW -Aml, data=train, family=binomial)
summary(model1)

#build with significant variables
model2 = glm(Ch~ Art + PrWl + Gm1 + SAM, data=train, family=binomial)
summary(model2)

#now everything looks significant, Art, PrWl, Gm1 and SAM

#now lets try to use predict function to our test set

predictTest = predict(model2, newdata=test, type="response")


table(test$Ch, predictTest>0.45) using .45 threshold, 
#I got a very accurate model yay!

#HoffmanPS is predicted as Best Actor for 2005, wherein He is the actual best actor. 
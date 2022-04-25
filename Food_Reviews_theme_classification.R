##setwd("~/NLP")
#Rawdata = read.csv("Microsoft Cloud_CC_Raw data.csv", stringsAsFactors = TRUE)
Rawdata = read.csv("amazonreviews_fornlp.csv", stringsAsFactors = TRUE) ##has to be customized
data_dim = dim(Rawdata)
num_rows = data_dim[1]
##converting the text to lower case

########## Tagged themes info 
# Out of 32000 reviews, 415 tweets are tagged as one of the five themes 
# 315 tagged reviews are used for deriving theme representation 
# 100 tagged reviews are used for testing predicting themes 


# Rawdata[1:170,'Theme']   "Customer Service"
# Rawdata[171:223,'Theme'] "Offers/Deals"
# Rawdata[224:254,'Theme']  "Payment Related"
# Rawdata[255:281,'Theme']  "Queries"
# Rawdata[282:314,'Theme']   "Refund"
# Rawdata[315:32031,'Theme'] "To be predicted" 
# (Here, 315 to 414 are tagged and will be used for testing the appraoch )

theme_rows = c(25, 36, 166, 224, 250, 279, 299, 1739) ## has to be changed accordingly
theme_names = c( "After Effect", "Cooking time",  "Flavor/Taste",
                 "Nutrition", "Packaging", "Price", "Texture", "zz") ## has to be chnaged accordingly
          
length(theme_rows)
length(theme_names)

########### Text processing and cleaning 
install.packages("stringr")
require(stringr)
colnames(Rawdata)


text_data_unmodified = Rawdata$Text ## has to be customized


#remove punctuations 
text_data = gsub("[[:punct:]]", " ",text_data_unmodified)

#Replace all words containing non english letters 
text_data = gsub("[^A-Za-z0-9]", " ", text_data)

install.packages("tm")
library(tm)

mydata = Corpus(VectorSource(text_data))
# remove standard stopwords
mydata <- tm_map(mydata, removeWords, stopwords("english"))

# remove other stop words 
myStopwords <- c("http", "https","www","com","org")
mydata <- tm_map(mydata, removeWords, myStopwords)
# remove extra whitespace
mydata <- tm_map(mydata, stripWhitespace)

# Remove numbers
mydata <- tm_map(mydata, removeNumbers)
# Stemming
install.packages("SnowballC")

library(SnowballC)
mydata <- tm_map(mydata, stemDocument)



## To see the lines in corpus ... 10th line
writeLines(head(strwrap(mydata[[10]])))

# tweet = text_data_unmodified[10]
# tweet
# stemmed_cleaned_tweet = strwrap(mydata[[10]])
# stemmed_cleaned_tweet

#create a term matrix and store it as tdm... takes time

control_list <- list(removePunctuation = TRUE, stopwords = TRUE,
                     tolower = TRUE,wordLengths=c(2,20),
                     weighting = weightTfIdf)
tdm <- TermDocumentMatrix(mydata, control = control_list)
dim(tdm)

new_tdm <- removeSparseTerms(tdm,sparse =0.995) ##0.0005 sparsity in the columns
dim(new_tdm)

######### Derving Theme representation vectors for above mentioned themes
### https://stackoverflow.com/questions/1484817

theme_rep = list()

start =1
for(i in 1:(length(theme_rows)-1)){
  end = theme_rows[i]
  theme_rep[[length(theme_rep)+1]] = rowSums(as.matrix(new_tdm[,start:end]))/(end-start)
  start = end + 1
}

theme_rep[[1]]


######### Theme categroization #############

### Creating empty columns to assign the values for each theme 

for (i in 1:(length(theme_rows)-1)){
  theme_name = theme_names[i]
  Rawdata[,theme_name] = NA
}

install.packages("lsa")
library(lsa)

for (i in 1:num_rows){
  print(i)
  tweet_vector = as.vector(new_tdm[,i])
  for (j in 1:(length(theme_names)-1)){
    theme_vector = theme_rep[[j]]
    theme_name = theme_names[j]
    theme_score = cosine(tweet_vector,theme_vector)
    Rawdata[i,theme_name] = theme_score
  }
}

#Rawdata1 = Rawdata[,-4]
write.csv(Rawdata, "AmazonReviews_NLPTagged1.csv")

taggedconversations <- read.csv("nlptaggeddata.csv")
View(taggedconversations)
str(taggedconversations)
taggedconversations$Sentiment <- as.factor(taggedconversations$Sentiment)
taggedconversations$sub.theme <- as.factor(taggedconversations$sub.theme)
taggedconversations$Predicted.Theme <- 
  as.factor(taggedconversations$Predicted.Theme)
reorderedtheme <- taggedconversations %>%  group_by(Predicted.Theme)  %>% summarise(count = n())
ordered_theme <- arrange(reorderedtheme, desc(count))
head(ordered_theme)
ggplot(ordered_theme, aes(fct_reorder(Predicted.Theme, count), count)) + 
  geom_col(aes(fill = count)) + coord_flip() + coord_polar() +
  labs(x = "Review theme", y = "count of reviews", title = "Volume of reviews by theme." ) 
taggeddata <- taggedconversations %>% filter(sub.theme != "zz")
dim(taggeddata)

library(vtreat)
set.seed(34245)  # set the seed for reproducibility
splitPlan <- kWayCrossValidation(nrow(taggeddata), 3, NULL, NULL)
split1 <- splitPlan[[1]]
split2 <- splitPlan[[2]]
split3 <- splitPlan[[3]]
unlist(split1)
split1$app
tagged1 <- taggeddata[split1$app,]
tagged2 <- taggeddata[split2$app,]

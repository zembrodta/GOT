
CharactersSoFar <- read_excel("~/Documents/GameOfThrones/CharactersSoFar.xlsx")

#Get rid of any columns that would be problematic 
CharactersTrunc <- CharactersSoFar %>% select(-c(CharacterFirst, CharacterLast, FullName, 
                                  Ranker, `2018Blog`, `Goodness score- only top 40, watchers on the wall`, seasonDeath, CauseOfDeath,
                                  `Major Death Event`, `Killer First`, KillerLast, `Season Present`, `dead spouse`, ScreenTime, Episodes, Season1,
                                  Season2, Season3, Season4, Season5, Season6, Season7))

#Correlation matrix

#Make categorical columns numeric

CharactersTrunc= CharactersTrunc %>% mutate_if(is.character, as.factor)
CharactersTrunc= CharactersTrunc %>% mutate_if(is.factor, as.numeric)

CharactersTrunc$FullName = CharactersSoFar$FullName
sum( CharactersTrunc$dead)
nrow(CharactersTrunc)
CharactersTrunc = CharactersTrunc[complete.cases(CharactersTrunc),]
res <- cor(CharactersTrunc) 

res
#Create training and testing
smp_size <- floor(0.85 * nrow(CharactersTrunc))

## set the seed to make your partition reproducible
train_ind <- sample(seq_len(nrow(CharactersTrunc)), size = smp_size)

train <- CharactersTrunc[train_ind, ]

test <- CharactersTrunc[-train_ind, ]

#Create logisitc regression model

mylogit <-glm(dead~Occupation+Hometown+timeperEp+children+Married, data = train)

summary(mylogit)

#Predict on test data 
test$rankP <- predict(mylogit, newdata = test)
test %>% select(FullName, dead, rankP)



#Very interesting. We did fairly well if we give it a greater than .5 means dead 
training = CharactersTrunc
training= training %>%
  select( -FullName)
training_x <- as.matrix(as.matrix(training[-2]))
training_x <- apply(training_x, 2 , as.numeric)
training_y <- as.matrix(training[2])
one_hot_train = to_categorical(training_y)[,-1]

normalizer <- function(x){
   max <- max(x)
   min <- min(x)

  for(i in 1:length(x)){
    x[i] <- (x[i]-min)/(max-min)
  }
  return(x)
}

training_x <- apply(training_x, 2,normalizer)

model <- keras_model_sequential() 
model %>% 
  layer_dense(units = 500,input_shape = 16) %>% 
  layer_activation_leaky_relu() %>% 
  layer_dropout(rate = 0.4) %>% 
  layer_dense(units = 250) %>%
  layer_activation_leaky_relu() %>% 
  layer_dropout(rate = 0.3) %>%
  layer_batch_normalization() %>% 
  layer_dense(units = 200) %>%
  layer_activation_leaky_relu() %>% 
  layer_dropout(rate = 0.3) %>%
  layer_dense(units = 150) %>%
  layer_activation_leaky_relu() %>%
  layer_dropout(rate = 0.4) %>%
  layer_dense(units = 200) %>%
  layer_activation_leaky_relu() %>%
  layer_dropout(rate = 0.4) %>%
  layer_dense(units = 1, activation = 'sigmoid') %>% 
  
  compile(
    loss = 'binary_crossentropy',
    optimizer = optimizer_adam(),
    metrics = c('accuracy')
  )
                                                    
history <- model %>% fit(
  training_x, one_hot_train,
  epochs = 100, batch_size = 6,
  validation_split = .2
)

library(keras)


#This is interesting... It kind of learns? 

c = CharactersTrunc$FullName
df = data.frame()
for (i in CharactersTrunc$FullName)
{
  train = CharactersTrunc %>%
    filter(FullName != i)
  test = CharactersTrunc %>%
    filter(FullName == i)
  mylogit <-glm(dead ~Occupation+Hometown+timeperEp+children+Married, data = train)
  test$Prediction <- predict(mylogit, newdata = test)
  df = rbind( df, test)
}

dfAll = df %>%
  mutate(roundedPred = round(Prediction,0)) %>%
  mutate( correct = ifelse( dead == roundedPred, TRUE, FALSE))

sum( dfAll$correct)/ nrow(dfAll)

sum(dfAll$dead)/ nrow(dfAll)

dfDead = df %>%
  select( FullName,dead, Prediction) %>% 
  filter( dead ==0)

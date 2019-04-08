CharactersSoFar <- read_excel("~/Documents/GameOfThrones/CharactersSoFar.xlsx")

#Get rid of any columns that would be problematic 
CharactersTrunc <- CharactersSoFar %>% select(-c(CharacterFirst, CharacterLast, FullName, 
                                  Ranker, `2018Blog`, `Goodness score- only top 40, watchers on the wall`, seasonDeath, CauseOfDeath,
                                  `Major Death Event`, `Killer First`, KillerLast, `Season Present`, `dead spouse`, ScreenTime, Episodes, Season1,
                                  Season2, Season3, Season4, Season5, Season6, Season7, house))

#Correlation matrix

#Make categorical columns numeric

CharactersTrunc= CharactersTrunc %>% mutate_if(is.character, as.factor)
CharactersTrunc= CharactersTrunc %>% mutate_if(is.factor, as.numeric)


sum( CharactersTrunc$dead)
nrow(CharactersTrunc)
CharactersTrunc = CharactersTrunc[complete.cases(CharactersTrunc),]
res <- cor(CharactersTrunc)
res
#Create training and testing
smp_size <- floor(0.7 * nrow(CharactersTrunc))

## set the seed to make your partition reproducible
set.seed(18)
train_ind <- sample(seq_len(nrow(CharactersTrunc)), size = smp_size)

train <- CharactersTrunc[train_ind, ]
test <- CharactersTrunc[-train_ind, ]

#Create logisitc regression model

mylogit <-glm(dead ~ SlaveProstitute+Hometown+timeperEp+children+Married+NotPresentButNotDead, data = CharactersTrunc)
summary(mylogit)
#Predict on test data 
test$rankP <- predict(mylogit, newdata = test)
test %>% select(dead, rankP)



#Very interesting. We did fairly well if we give it a greater than .5 means dead 
training = CharactersTrunc

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

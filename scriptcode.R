library(datasets) # Contains the dhfr data set
library(caret) # Package for machine learning algorithms / CARET stands for Classification And Regression Training

# Importing the dhfr data set (dihydrofolate reductase molecule)
data(dhfr)

# Check to see if there are missing data?
sum(is.na(dhfr))

# view dhfr
View(dhfr)

head(dhfr,5)
tail(dhfr,5)
dhfr$Y

#summary
summary(dhfr)
summary(dhfr$Y)

library(skimr)
skim(dhfr)

plot(dhfr$moe2D_zagreb,dhfr$moe2D_weinerPol, col="magenta")

hist(dhfr$moe2D_zagreb, col="lightgreen")

featurePlot(x=dhfr[,2:7],y=dhfr$Y,plot="box",
            strip=strip.custom(par.strip.text=list(cex=.7)),
            scales = list(x=list(reltion="free"),y=list(relation="free")))

# SVM model (polynomial kernel)

# Build Training model
Model <- train(Y ~ ., data = TrainingSet,
               method = "svmPoly",
               na.action = na.omit,
               preProcess=c("scale","center"),
               trControl= trainControl(method="none"),
               tuneGrid = data.frame(degree=1,scale=1,C=1))

# Build CV model
Model.cv <- train(Y ~ ., data = TrainingSet,
                  method = "svmPoly",
                  na.action = na.omit,
                  preProcess=c("scale","center"),
                  trControl= trainControl(method="cv", number=10),
                  tuneGrid = data.frame(degree=1,scale=1,C=1))

# Apply model for prediction
Model.training <-predict(Model, TrainingSet) # Apply model to make prediction on Training set
Model.testing <-predict(Model, TestingSet) # Apply model to make prediction on Testing set
Model.cv <-predict(Model.cv, TrainingSet) # Perform cross-validation

# Model performance (Displays confusion matrix and statistics)
Model.training.confusion <-confusionMatrix(Model.training, TrainingSet$Y)
Model.testing.confusion <-confusionMatrix(Model.testing, TestingSet$Y)
Model.cv.confusion <-confusionMatrix(Model.cv, TrainingSet$Y)

print(Model.training.confusion)
print(Model.testing.confusion)
print(Model.cv.confusion)

# Feature importance
Importance <- varImp(Model)
plot(Importance,top=25)
plot(Importance, col = "red")

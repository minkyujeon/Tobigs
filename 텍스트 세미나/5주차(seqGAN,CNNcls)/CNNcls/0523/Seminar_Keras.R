# Reference

# https://ratsgo.github.io/natural%20language%20processing/2017/03/19/CNN/
# https://www.youtube.com/watch?v=mPxi1YgU9Zw
# https://richliao.github.io/supervised/classification/2016/11/26/textclassifier-convolutional/
# https://github.com/kkb2849/cnn-sentence-classification/blob/423a71c459aacfb3d0a951d2bc98069a3f921508/cnn_sentence_classification.ipynb
# https://mxnet.incubator.apache.org/tutorials/nlp/cnn.html
# https://gist.github.com/ratsgo/7ff405f582437dbf96216dd940917427#file-cnn_sentence_classification-py-L96
# https://tykimos.github.io/2017/01/27/CNN_Layer_Talk/

install.packages('keras') ; library(keras) # Using Keras
install_keras() # Install Keras in Python
library(keras)
mnist <- dataset_mnist() # MNIST Data
imdb <- dataset_imdb(num_words = 10000) # imdb Data 
word_index <- dataset_imdb_word_index() # imdb Data_index

# Using UCI Data
red.url = 'https://archive.ics.uci.edu/ml/machine-learning-databases/wine-quality/winequality-red.csv'
white.url = 'https://archive.ics.uci.edu/ml/machine-learning-databases/wine-quality/winequality-white.csv'

red.wine = read.csv(url(red.url), sep = ';')
white.wine = read.csv(url(white.url), sep = ';')

red.wine$color = 'red'
white.wine$color = 'white'

wine = rbind(red.wine, white.wine)
head(wine)
set.seed(1234)
library(caret)

# Train ~ Test Partition
idx <- createDataPartition(wine$color, p = 0.8, list = F)
data.train <- wine[idx,]
data.test <- wine[-idx,]

# Train Data
x.train <- as.matrix(data.train[names(wine) != 'color']) # Color 제외 행
y.train <- data.train$color == 'red' # Red = True, White = False 

NX = ncol(x.train) # 변수 개수

# Single Layer
model <- keras_model_sequential()
model %>%
  layer_dense(units = 1, activation ='sigmoid', input_shape = c(NX))
summary(model)

# Environment
model %>% compile(
  loss = 'binary_crossentropy',
  optimizer = optimizer_adam(),
  metrics = c('accuracy')
)

# Training
history <- model %>% fit(
  x.train, y.train,
  epochs = 30, batch_size = 64,
  validation_split = 0.2)

history$metrics$val_loss # 낮아지는 Loss
history$metrics$val_acc # 높아지는 Acc

# Test
x.test <- as.matrix(data.test[names(wine) != 'color'])
y.test <- data.test$color == 'red'

model %>% evaluate(x.test, y.test) # 똑같은 모델에 test

# Multi_Layer
mlp <- keras_model_sequential()
mlp %>%
  layer_dense(units = 4, activation = 'sigmoid', input_shape = c(NX)) %>% # Hidden Layer, 4개의 Units
  layer_dense(units = 1, activation = 'sigmoid')

summary(mlp)

mlp %>% compile(
  loss = 'binary_crossentropy',
  optimizer = optimizer_adam(),
  metrics = c('accuracy')
)

history = mlp %>% fit(
  x.train, y.train,
  epochs = 30, batch_size = 128,
  validation_split = 0.2)

mlp %>% evaluate(x.test, y.test)

# Regression (Predict Quality)
reg <- keras_model_sequential()
reg %>%
  layer_dense(units = 4, activation = 'sigmoid', input_shape = c(NX-1)) %>%
  layer_dense(units = 1, activation = 'linear')

reg %>% compile(
  loss = 'mse',
  optimizer = optimizer_adam()
)

reg %>% fit(
  x.train[,-12],
  x.train[,12],
  epochs = 30, batch_size = 128,
  validation_split = 0.2
)

reg %>% evaluate(
  x.test[, -12],
  x.test[, 12]
)

# CNN
# mnist <- dataset_mnist()
x_train <- mnist$train$x
y_train <- mnist$train$y
x_test <- mnist$test$x
y_test <- mnist$test$y

dim(x_train)
dim(x_train) <- c(60000, 28, 28, 1)
dim(x_test) <- c(10000, 28, 28, 1)

x_train <- x_train / 255
x_test <- x_test / 255

dim(y_train)
y_train[1:10]

# One-Hot Encoding (6 -> 0, 0, 0, 0, 0, 1, 0, 0, 0, 0)
y_train <- to_categorical(y_train, 10)
y_test <- to_categorical(y_test, 10)

dim(y_train) # 60000 : 10

# CNN
cnn <- keras_model_sequential()
cnn %>%
  layer_conv_2d(32, 3, activation = 'relu', input_shape = c(28, 28, 1)) %>%
  layer_max_pooling_2d(pool_size = c(2,2)) %>%
  layer_flatten() %>%
  layer_dense(units = 32, activation = 'relu') %>%
  layer_dense(units = 10, activation = 'softmax') #클래스 10개줌

summary(cnn)

cnn %>% compile(
  loss = 'categorical_crossentropy',
  optimizer = optimizer_rmsprop(),
  metrics = c('accuracy')
)

history = cnn %>% fit(
  x_train, y_train,
  epochs = 30, batch_size = 128,
  validation_split = 0.2,
  callbacks = c(callback_early_stopping(monitor = "val_loss"))#validation loss값이 더이상 낮아지지않 안는다면 학습 멈춤
)

history$metrics$val_loss
history$metric$val_acc

# RNN
NUM.WORDS = 10000
# word_index <- dataset_imdb_word_index()
# imdb <- dataset_imdb(num_words = NUM.WORDS)
x_train <- imdb$train$x
y_train <- imdb$train$y
x_test <- imdb$test$x
y_test <- imdb$test$y

y_train[1:10]
x_train[1]

MAXLEN = 20 # 최대길이 (문장마다 길이기 다르기 때문에)
x_train <- pad_sequences(x_train, MAXLEN) # 짧으면 0으로 채운다
x_test <- pad_sequences(x_test, MAXLEN)

# RNN
rnn <- keras_model_sequential()
rnn %>%
  layer_embedding(input_dim = NUM.WORDS, output_dim = 8, input_length = MAXLEN) %>%
  layer_lstm(1, activation = 'sigmoid', return_sequences = F)#10000개(input_dim)에 해당하는 단어 corpus불러오기
summary(rnn)

rnn %>% compile(optimizer = optimizer_rmsprop(),
                loss = 'binary_crossentropy',
                metrics = 'accuracy')
history <- rnn %>% fit(
  x_train, y_train,
  epochs = 30, batch_size = 128,
  validation_split = 0.2,
  callbacks = c(callback_early_stopping(monitor = "val_loss"))
)
history

y_rnn <- predict_classes(rnn, x_test)
confusionMatrix(as.factor(y_rnn), as.factor(y_test))

# LSTM
lstm <- keras_model_sequential()
lstm %>% layer_embedding(input_dim = NUM.WORDS, output_dim = 8, input_length = MAXLEN) %>%
  layer_lstm(32, activation = 'tanh', return_sequences = F) %>%
  layer_dense(1, activation = 'sigmoid')
summary(lstm)

lstm %>% compile(optimizer = optimizer_rmsprop(),
                 loss = 'binary_crossentropy',
                 metrics = 'accuracy')

model_lstm <- lstm %>% fit(
  x_train, y_train,
  epochs = 30, batch_size = 128,
  validation_split = 0.2,
  callbacks = c(callback_early_stopping(monitor = "val_loss"))
)
model_lstm

y_lstm <- predict_classes(lstm, x_test)
confusionMatrix(as.factor(y_lstm), as.factor(y_test))

# return_sequences = T
sequence <- keras_model_sequential()
sequence %>%
  layer_embedding(input_dim = NUM.WORDS, output_dim = 8, input_length = MAXLEN) %>%
  layer_lstm(4, activation='tanh', return_sequences = T) %>%
  layer_flatten() %>%
  layer_dense(units=1, activation='sigmoid')
summary(sequence)

sequence %>% compile(optimizer = optimizer_rmsprop(),
                     loss = 'binary_crossentropy',
                     metrics = 'accuracy')
history <- sequence %>% fit(
  x_train, y_train,
  epochs = 30, batch_size = 128,
  validation_split = 0.2,
  callbacks = c(callback_early_stopping(monitor = "val_loss"))
)
history

y_sequence <- predict_classes(sequence, x_test)
confusionMatrix(as.factor(y_sequence), as.factor(y_test))

# CNN To imdb
# Parameter
max_features = 20000
maxlen = 100
embedding_size = 128
kernel_size = 5
filters = 64
pool_size = 4
lstm_output_size = 70
batch_size = 16
epochs = 5

# Data Preparation --------------------------------------------------------

imdb <- dataset_imdb(num_words = max_features)
str(imdb)

x_train <- imdb$train$x %>% pad_sequences(maxlen = maxlen)
x_test <- imdb$test$x %>% pad_sequences(maxlen = maxlen)

# Defining Model ------------------------------------------------------

model <- keras_model_sequential()

model %>%
  layer_embedding(max_features, embedding_size, input_length = maxlen) %>%
  layer_dropout(0.25) %>%
  layer_conv_1d(filters, kernel_size, padding = "valid",activation = "relu", strides = 1) %>%
  layer_max_pooling_1d(pool_size) %>%
  layer_lstm(lstm_output_size) %>%
  layer_dense(1) %>%
  layer_activation("sigmoid")

model %>% compile(
  loss = "binary_crossentropy",
  optimizer = "adam",
  metrics = "accuracy"
)

# Training ----------------------------------------------------------------

history <- model %>% fit(
  x_train, imdb$train$y,
  batch_size = batch_size,
  epochs = epochs,
  validation_data = list(x_test, imdb$test$y)
)

y_cnn = predict_classes(model, x_test)
head(y_cnn)
confusionMatrix(as.factor(y_cnn), as.factor(y_test))

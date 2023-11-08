import random
import json
import pickle
import numpy as np
import nltk
from nltk.stem import WordNetLemmatizer
from tensorflow.keras.models import Sequential
from tensorflow.keras.layers import Dense, Activation, Dropout
import tensorflow as tf

nltk.download('punkt')  # Ensure that NLTK data is downloaded

lemmatizer = WordNetLemmatizer()

# Load the intents from a JSON file
intents = json.loads(open('intents.json').read())

words = []  # Store individual words
classes = []  # Store intent tags
documents = []  # Store pairs of words and their corresponding intents

ignore_letters = ['?', '!', '.', ',']

# Preprocess the data and build vocabulary
for intent in intents['intents']:
    for pattern in intent['patterns']:
        # Tokenize the patterns into individual words
        word_list = nltk.word_tokenize(pattern)
        words.extend(word_list)
        documents.append((word_list, intent['tag']))
        if intent['tag'] not in classes:
            classes.append(intent['tag'])

# Lemmatize and sort the words
words = [lemmatizer.lemmatize(word.lower()) for word in words if word not in ignore_letters]
words = sorted(set(words))
print(words)
classes = sorted(set(classes))

# Save the words and classes
pickle.dump(words, open('words.pkl', 'wb'))
pickle.dump(classes, open('classes.pkl', 'wb'))

training = []
output_empty = [0] * len(classes)

# Create the training dataset
for document in documents:
    bag = []
    word_patterns = document[0]
    word_patterns = [lemmatizer.lemmatize(word.lower()) for word in word_patterns]

    for word in words:
        bag.append(1) if word in word_patterns else bag.append(0)

    output_row = list(output_empty)
    output_row[classes.index(document[1])] = 1

    training.append([bag, output_row])

random.shuffle(training)

# Separate features and labels
X = np.array([x[0] for x in training])
y = np.array([x[1] for x in training])

# Create and configure the model
model = Sequential()
model.add(Dense(128, input_shape=(len(X[0]),), activation='relu'))
model.add(Dropout(0.5))
model.add(Dense(64, activation='relu'))
model.add(Dropout(0.5))
model.add(Dense(len(y[0]), activation='softmax'))

# Define the optimizer and compile the model using legacy SGD
sgd = tf.keras.optimizers.SGD(learning_rate=0.01, momentum=0.9, nesterov=True)
model.compile(loss='categorical_crossentropy', optimizer=sgd, metrics=['accuracy'])

# Train the model
hist=model.fit(np.array(train_X), (np.array_y), epochs=200, batch_size=5, verbose=1)
model.save('chatbot_model.h5',hist)
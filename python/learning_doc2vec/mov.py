import os
os.environ["CUDA_VISIBLE_DEVICES"] = "-1"  # Force using CPU

import numpy as np
import pandas as pd
from gensim.models import Doc2Vec
from gensim.models.doc2vec import TaggedDocument
from tensorflow.keras.models import Sequential
from tensorflow.keras.layers import LSTM, Dense, Dropout, Embedding
from sklearn.model_selection import train_test_split

data = [
    "I love programming in Python.",
    "Doc2Vec is great for document embeddings.",
    "LSTM networks are powerful for sequential data.",
    "Using GRU can also be effective in many cases.",
    "Deep learning provides a variety of tools.",
    "Natural language processing is a fascinating field."
]

tagged_data = [TaggedDocument(words=doc.lower().split(), tags=[str(i)]) for i, doc in enumerate(data)]
doc2vec_model = Doc2Vec(vector_size=50, window=2, min_count=1, workers=4, epochs=100)
doc2vec_model.build_vocab(tagged_data)
doc2vec_model.train(tagged_data, total_examples=doc2vec_model.corpus_count, epochs=doc2vec_model.epochs)
X = np.array([doc2vec_model.dv[str(i)] for i in range(len(data))])
X = X.reshape((X.shape[0], 1, X.shape[1]))  
y = np.array([0, 1, 0, 1, 0, 1])  
X_train, X_test, y_train, y_test = train_test_split(X, y, test_size=0.2, random_state=42)
model = Sequential()
model.add(LSTM(64, return_sequences=True, input_shape=(1, X.shape[2])))  
model.add(Dropout(0.2))
model.add(LSTM(32))
model.add(Dropout(0.2))
model.add(Dense(1, activation='sigmoid'))
model.compile(loss='binary_crossentropy', optimizer='adam', metrics=['accuracy'])
model.summary()
model.fit(X_train, y_train, epochs=10, batch_size=2)
loss, accuracy = model.evaluate(X_test, y_test)
print(f'Test Loss: {loss}, Test Accuracy: {accuracy}')
predictions = model.predict(X_test)
predicted_classes = (predictions > 0.5).astype(int)
print(f'Predicted classes: {predicted_classes.flatten()}')

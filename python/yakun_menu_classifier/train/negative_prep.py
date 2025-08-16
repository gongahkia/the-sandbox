import tensorflow as tf
from tensorflow import keras
import cv2
import numpy as np

def load_and_preprocess_data():
    class_names = [
        "airplane",
        "automobile",
        "bird",
        "cat",
        "deer",
        "dog",
        "frog",
        "horse",
        "ship",
        "truck",
    ]
    (train_images, train_labels), (
        test_images,
        test_labels,
    ) = keras.datasets.cifar10.load_data()

    max_train_samples = 15000
    train_images = train_images[:max_train_samples]
    train_labels = train_labels[:max_train_samples]

    print(
        "Training data size:", train_images.shape, "Test data size", test_images.shape
    )

    train_images = train_images / 127.5 - 1
    test_images = test_images / 127.5 - 1

    minSize = 96
    resized_train_images = np.zeros((15000, minSize, minSize, 3), dtype=np.float32)
    for i in range(len(train_images)):
        resized_train_images[i] = cv2.resize(
            train_images[i], dsize=(minSize, minSize), interpolation=cv2.INTER_AREA
        )

    resized_test_images = np.zeros((10000, minSize, minSize, 3), dtype=np.float32)
    for i in range(len(test_images)):
        resized_test_images[i] = cv2.resize(
            test_images[i], dsize=(minSize, minSize), interpolation=cv2.INTER_AREA
        )

    return (
        resized_train_images,
        train_labels,
        resized_test_images,
        test_labels,
        class_names,
    )
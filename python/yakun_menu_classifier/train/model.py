import tensorflow as tf
from tensorflow import keras

def create_model(IMG_SHAPE, num_classes):
    base_model = tf.keras.applications.MobileNetV2(
        input_shape=IMG_SHAPE, include_top=False, weights="imagenet"
    )
    base_model.trainable = False

    global_average_layer = tf.keras.layers.GlobalAveragePooling2D()
    prediction_layer = tf.keras.layers.Dense(num_classes, activation="softmax")

    model = tf.keras.Sequential([base_model, global_average_layer, prediction_layer])

    return model

def compile_model(model):
    loss_fn = tf.keras.losses.SparseCategoricalCrossentropy(from_logits=False)
    model.compile(optimizer="adam", loss=loss_fn, metrics=["accuracy"])
    return model

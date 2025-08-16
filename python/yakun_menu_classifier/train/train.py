import tensorflow as tf


def train_model(model, train_images, train_labels, test_images, test_labels):
    es_callback = tf.keras.callbacks.EarlyStopping(
        monitor="val_loss", patience=3, restore_best_weights=True
    )
    batch_size = 128
    epochs = 2
    history = model.fit(
        train_images,
        train_labels,
        epochs=epochs,
        batch_size=batch_size,
        validation_data=(test_images, test_labels),
        callbacks=[es_callback],
    )
    return history


def evaluate_model(model, test_images, test_labels):
    loss, accuracy = model.evaluate(test_images, test_labels, verbose=0)
    print("Test Accuracy: %.4f" % accuracy)
    return loss, accuracy
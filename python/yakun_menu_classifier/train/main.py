from negative_prep import load_and_preprocess_data
from model import create_model, compile_model
from train import train_model, evaluate_model
from visualize import plot_training_history

def main():
    (
        train_images,
        train_labels,
        test_images,
        test_labels,
        class_names,
    ) = load_and_preprocess_data()
    IMG_SHAPE = (96, 96, 3)
    model = create_model(IMG_SHAPE, len(class_names))
    model = compile_model(model)
    history = train_model(model, train_images, train_labels, test_images, test_labels)
    evaluate_model(model, test_images, test_labels)
    plot_training_history(history)

if __name__ == "__main__":
    main()

import os
import torch
import torch.nn as nn
import torch.optim as optim
from torch.utils.data import DataLoader, TensorDataset
import deepspeed
import torchvision

os.environ["TF_ENABLE_ONEDNN_OPTS"] = "0"
torchvision.disable_beta_transforms_warning()

class MyModel(nn.Module):
    def __init__(self):
        super(MyModel, self).__init__()
        self.fc = nn.Linear(10, 2)

    def forward(self, x):
        return self.fc(x)

def create_data_loader(batch_size):
    inputs = torch.randn(1000, 10)
    labels = torch.randint(0, 2, (1000,))
    dataset = TensorDataset(inputs, labels)
    return DataLoader(dataset, batch_size=batch_size, shuffle=True)

def train(args):
    model = MyModel()
    data_loader = create_data_loader(batch_size=args.batch_size)
    model_engine, optimizer, _, _ = deepspeed.initialize(args=args,
                                                         model=model,
                                                         model_parameters=model.parameters())
    for epoch in range(args.epochs):
        for step, (batch_inputs, batch_labels) in enumerate(data_loader):
            batch_inputs = batch_inputs.to(model_engine.device)
            batch_labels = batch_labels.to(model_engine.device)
            outputs = model_engine(batch_inputs)
            loss = nn.CrossEntropyLoss()(outputs, batch_labels)
            model_engine.backward(loss)
            model_engine.step()
            if step % 10 == 0:
                print(f"Epoch [{epoch + 1}/{args.epochs}], Step [{step}/{len(data_loader)}], Loss: {loss.item():.4f}")

# ----- main execution code -----

if __name__ == "__main__":
    import argparse
    parser = argparse.ArgumentParser()
    parser.add_argument("--batch_size", type=int, default=32, help="Batch size for training")
    parser.add_argument("--epochs", type=int, default=5, help="Number of epochs for training")
    deepspeed.add_config_arguments(parser)
    parser.add_argument("--local_rank", type=int, default=0, help="Local rank for distributed training")
    args = parser.parse_args()
    train(args)

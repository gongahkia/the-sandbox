from diffusers import StableDiffusionPipeline
import torch

def load_model():
    device = "cuda" if torch.cuda.is_available() else "cpu"
    model_id = "CompVis/ldm-text2im-large-256"
    alternative_model_id = "stabilityai/stable-diffusion-2-1-base"
    pipe = StableDiffusionPipeline.from_pretrained(model_id)
    pipe = pipe.to(device)
    return pipe

def generate_image(prompt, output_file):
    pipe = load_model()
    image = pipe(prompt).images[0]
    image.save(output_file)

if __name__ == "__main__":
    prompt = input("Enter a text prompt for the image: ")
    output_file = "output_image.png"
    generate_image(prompt, output_file)
    print(f"Image generated and saved to {output_file}")
import math
import os
import time

# Set screen size
screen_width = 80
screen_height = 22
A, B = 0, 0  # Rotation angles

# Function to render the donut
def render_donut(A, B):
    # Initialize output screen
    output = [[' ' for _ in range(screen_width)] for _ in range(screen_height)]
    z_buffer = [[0 for _ in range(screen_width)] for _ in range(screen_height)]

    for theta in range(0, 628, 7):  # theta goes from 0 to 2pi (0-360 degrees)
        for phi in range(0, 628, 2):  # phi goes from 0 to 2pi (0-360 degrees)

            # Calculate 3D coordinates
            cos_A = math.cos(A)
            sin_A = math.sin(A)
            cos_B = math.cos(B)
            sin_B = math.sin(B)
            cos_theta = math.cos(theta / 100)
            sin_theta = math.sin(theta / 100)
            cos_phi = math.cos(phi / 100)
            sin_phi = math.sin(phi / 100)

            # Coordinates of the donut in 3D space
            circle_x = cos_theta * (2 + cos_phi)
            circle_y = sin_phi
            z = sin_theta * (2 + cos_phi)

            # 3D rotation along X and Z axes
            x = circle_x * cos_B - z * sin_B
            z = circle_x * sin_B + z * cos_B

            y = circle_y

            # Project 3D coordinates to 2D space
            ooz = 1 / (z + 5)  # Object is closer when z approaches -5
            xp = int(screen_width / 2 + (x * ooz) * 30)
            yp = int(screen_height / 2 - (y * ooz) * 15)

            if 0 <= xp < screen_width and 0 <= yp < screen_height:
                if ooz > z_buffer[yp][xp]:
                    z_buffer[yp][xp] = ooz
                    luminance_index = int((cos_phi * cos_theta * sin_A - sin_A * sin_theta + sin_B * cos_theta * cos_A - sin_B * cos_phi) * 8)
                    luminance_index = max(0, min(11, luminance_index))

                    # Different shades for lighting
                    luminance_chars = ".,-~:;=!*#$@"
                    output[yp][xp] = luminance_chars[luminance_index]

    # Print the frame
    os.system('cls' if os.name == 'nt' else 'clear')  # Clear the screen
    for row in output:
        print(''.join(row))

# Main loop to continuously render the spinning donut
while True:
    render_donut(A, B)
    A += 0.04
    B += 0.02
    time.sleep(0.1)

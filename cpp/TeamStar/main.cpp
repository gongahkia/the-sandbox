#define _USE_MATH_DEFINES
#include <iostream>
#include <cmath>
#include <thread>
#include <chrono>

const int WIDTH = 20;
const int HEIGHT = 10;

void clearScreen() {
    std::cout << "\033[2J\033[1;1H";
}

void drawStar(float rotation) {
    const char* star = "*";
    int centerX = WIDTH / 2;
    int centerY = HEIGHT / 2;

    for (int y = 0; y < HEIGHT; ++y) {
        for (int x = 0; x < WIDTH; ++x) {
            float angle = atan2(y - centerY, x - centerX);
            float distance = sqrt(pow(x - centerX, 2) + pow(y - centerY, 2));

            if (distance < 5 && (angle + rotation) > -M_PI / 5 && (angle + rotation) < M_PI / 5) {
                std::cout << star;
            } else if (distance < 5 * sqrt(2) && (angle + rotation) > M_PI / 5 && (angle + rotation) < 3 * M_PI / 5) {
                std::cout << star;
            } else if (distance < 5 && (angle + rotation) > 3 * M_PI / 5 && (angle + rotation) < M_PI) {
                std::cout << star;
            } else if (distance < 5 && (angle + rotation) > -M_PI && (angle + rotation) < -3 * M_PI / 5) {
                std::cout << star;
            } else if (distance < 5 * sqrt(2) && (angle + rotation) > -3 * M_PI / 5 && (angle + rotation) < -M_PI / 5) {
                std::cout << star;
            } else {
                std::cout << " ";
            }
        }
        std::cout << std::endl;
    }
}

int main() {
    float rotation = 0.0f;
    const float rotationSpeed = 0.1f;

    while (true) {
        clearScreen();
        drawStar(rotation);
        rotation += rotationSpeed;

        std::this_thread::sleep_for(std::chrono::milliseconds(100));
    }

    return 0;
}
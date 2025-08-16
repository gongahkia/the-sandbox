#include <SDL2/SDL.h>
#include <math.h>
#include <stdio.h>

const int WIDTH = 800;
const int HEIGHT = 600;
const int POLYGON_SIDES = 4;

void drawPolygon(SDL_Renderer *renderer, int centerX, int centerY, int radius, double angle) {
    SDL_Point points[POLYGON_SIDES + 1];
    for (int i = 0; i < POLYGON_SIDES; ++i) {
        double theta = angle + (2.0 * M_PI * i / POLYGON_SIDES);
        points[i].x = centerX + (int)(radius * cos(theta));
        points[i].y = centerY + (int)(radius * sin(theta));
    }
    points[POLYGON_SIDES] = points[0];
    SDL_RenderDrawLines(renderer, points, POLYGON_SIDES + 1);
}

int main() {
    SDL_Init(SDL_INIT_VIDEO);
    SDL_Window *window = SDL_CreateWindow("POLI GOMI", SDL_WINDOWPOS_UNDEFINED, SDL_WINDOWPOS_UNDEFINED, WIDTH, HEIGHT, 0);
    SDL_Renderer *renderer = SDL_CreateRenderer(window, -1, 0);
    double angle = 0.0;
    int radius = 100;
    int centerX = WIDTH / 2;
    int centerY = HEIGHT / 2;
    int running = 1;
    SDL_Event event;
    while (running) {
        while (SDL_PollEvent(&event)) {
            if (event.type == SDL_QUIT) {
                running = 0;
            }
        }
        SDL_SetRenderDrawColor(renderer, 0, 0, 0, 255);
        SDL_RenderClear(renderer);
        SDL_SetRenderDrawColor(renderer, 255, 0, 0, 255);
        drawPolygon(renderer, centerX, centerY, radius, angle);
        angle += 0.01;
        SDL_RenderPresent(renderer);
        SDL_Delay(16);
    }
    SDL_DestroyRenderer(renderer);
    SDL_DestroyWindow(window);
    SDL_Quit();
    return 0;
}
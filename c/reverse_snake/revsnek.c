#include <SDL2/SDL.h>
#include <stdlib.h>
#include <time.h>

const int WIDTH = 800;
const int HEIGHT = 600;
const int BLOCK_SIZE = 20;
const int SNAKE_LENGTH = 5;

typedef struct {
    int x;
    int y;
} Point;

Point apple;
int running = 1;
Point snake[5];

void init() {
    for (int i = 0; i < SNAKE_LENGTH; i++) {
        snake[i].x = WIDTH / 2;
        snake[i].y = HEIGHT / 2 + i * BLOCK_SIZE;
    }
    apple.x = rand() % (WIDTH / BLOCK_SIZE) * BLOCK_SIZE;
    apple.y = rand() % (HEIGHT / BLOCK_SIZE) * BLOCK_SIZE;
}

void draw(SDL_Renderer *renderer) {
    SDL_SetRenderDrawColor(renderer, 0, 0, 0, 255);
    SDL_RenderClear(renderer);
    SDL_SetRenderDrawColor(renderer, 255, 0, 0, 255);
    SDL_Rect appleRect = {apple.x, apple.y, BLOCK_SIZE, BLOCK_SIZE};
    SDL_RenderFillRect(renderer, &appleRect);
    SDL_SetRenderDrawColor(renderer, 0, 255, 0, 255);
    for (int i = 0; i < SNAKE_LENGTH; i++) {
        SDL_Rect snakeRect = {snake[i].x, snake[i].y, BLOCK_SIZE, BLOCK_SIZE};
        SDL_RenderFillRect(renderer, &snakeRect);
    }
    SDL_RenderPresent(renderer);
}

void moveSnake() {
    int direction = rand() % 4;
    Point newHead = snake[0];
    if (direction == 0) newHead.y -= BLOCK_SIZE;
    if (direction == 1) newHead.y += BLOCK_SIZE;
    if (direction == 2) newHead.x -= BLOCK_SIZE;
    if (direction == 3) newHead.x += BLOCK_SIZE;
    for (int i = SNAKE_LENGTH - 1; i > 0; i--) {
        snake[i] = snake[i - 1];
    }
    snake[0] = newHead;
}

void moveApple(SDL_Event *event) {
    if (event->type == SDL_KEYDOWN) {
        switch (event->key.keysym.sym) {
            case SDLK_w: apple.y -= BLOCK_SIZE; break;
            case SDLK_s: apple.y += BLOCK_SIZE; break;
            case SDLK_a: apple.x -= BLOCK_SIZE; break;
            case SDLK_d: apple.x += BLOCK_SIZE; break;
        }
        if (apple.x < 0) apple.x = 0;
        if (apple.x >= WIDTH) apple.x = WIDTH - BLOCK_SIZE;
        if (apple.y < 0) apple.y = 0;
        if (apple.y >= HEIGHT) apple.y = HEIGHT - BLOCK_SIZE;
    }
}

void checkCollision() {
    if (snake[0].x == apple.x && snake[0].y == apple.y) {
        apple.x = rand() % (WIDTH / BLOCK_SIZE) * BLOCK_SIZE;
        apple.y = rand() % (HEIGHT / BLOCK_SIZE) * BLOCK_SIZE;
    }
}

int main() {
    srand(time(NULL));
    SDL_Init(SDL_INIT_VIDEO);
    SDL_Window *window = SDL_CreateWindow("revsnek", SDL_WINDOWPOS_UNDEFINED, SDL_WINDOWPOS_UNDEFINED, WIDTH, HEIGHT, 0);
    SDL_Renderer *renderer = SDL_CreateRenderer(window, -1, 0);
    init();
    while (running) {
        SDL_Event event;
        while (SDL_PollEvent(&event)) {
            if (event.type == SDL_QUIT) running = 0;
            moveApple(&event);
        }
        moveSnake();
        checkCollision();
        draw(renderer);
        SDL_Delay(100);
    }
    SDL_DestroyRenderer(renderer);
    SDL_DestroyWindow(window);
    SDL_Quit();
    return 0;
}

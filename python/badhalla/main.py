# ~ SINGLE FILE IMPLEMENTATION OF A 2D FIGHTING GAME ~

import pygame

pygame.init()

WIDTH, HEIGHT = 800, 600
FPS = 60
WHITE = (255, 255, 255)
BLUE = (0, 0, 255)
RED = (255, 0, 0)
PLAYER_WIDTH, PLAYER_HEIGHT = 50, 50
GRAVITY = 0.5
JUMP_STRENGTH = 10
DODGE_DISTANCE = 20
GROUND_HEIGHT = HEIGHT - PLAYER_HEIGHT

class Player:

    def __init__(self, x, y, color, controls):
        self.rect = pygame.Rect(x, y, PLAYER_WIDTH, PLAYER_HEIGHT)
        self.color = color
        self.velocity_y = 0
        self.on_ground = True
        self.controls = controls
        self.health = 100

    def move(self, keys, other_player):
        if keys[self.controls['left']]:
            self.rect.x -= 5
        if keys[self.controls['right']]:
            self.rect.x += 5
        if keys[self.controls['jump']] and self.on_ground:
            self.velocity_y = -JUMP_STRENGTH
            self.on_ground = False
        if keys[self.controls['dodge']]:
            if keys[self.controls['right']]:
                self.rect.x += DODGE_DISTANCE
            elif keys[self.controls['left']]:
                self.rect.x -= DODGE_DISTANCE
        self.apply_gravity()
        self.check_head_damage(other_player)

    def apply_gravity(self):
        if not self.on_ground:
            self.velocity_y += GRAVITY
            self.rect.y += self.velocity_y
            if self.rect.y >= GROUND_HEIGHT:
                self.rect.y = GROUND_HEIGHT
                self.on_ground = True
                self.velocity_y = 0

    def check_head_damage(self, other_player):
        if self.rect.colliderect(other_player.rect) and self.rect.bottom <= other_player.rect.top + 10:
            other_player.take_damage(10)

    def take_damage(self, damage):
        self.health -= damage
        if self.health < 0:
            self.health = 0

    def draw(self, screen):
        pygame.draw.rect(screen, self.color, self.rect)
        health_text = font.render(f'HP: {self.health}', True, (255, 255, 255))
        screen.blit(health_text, (self.rect.x, self.rect.y - 20))

pygame.font.init()
font = pygame.font.Font(None, 36)

def main():
    screen = pygame.display.set_mode((WIDTH, HEIGHT))
    pygame.display.set_caption("badhalla")
    background = pygame.image.load("asset/sf2.jpg")
    background = pygame.transform.scale(background, (WIDTH, HEIGHT))
    clock = pygame.time.Clock()
    player1_controls = {
        'left': pygame.K_a,
        'right': pygame.K_d,
        'jump': pygame.K_w,
        'dodge': pygame.K_s
    }
    player2_controls = {
        'left': pygame.K_LEFT,
        'right': pygame.K_RIGHT,
        'jump': pygame.K_UP,
        'dodge': pygame.K_DOWN
    }
    player1 = Player(100, GROUND_HEIGHT, BLUE, player1_controls)
    player2 = Player(600, GROUND_HEIGHT, RED, player2_controls)
    running = True
    while running:
        for event in pygame.event.get():
            if event.type == pygame.QUIT:
                running = False
        keys = pygame.key.get_pressed()
        player1.move(keys, player2)
        player2.move(keys, player1)
        if player1.health <= 0 or player2.health <= 0:
            running = False
        screen.blit(background, (0, 0)) # shadow fight 2 background
        # screen.fill(WHITE) # default white background
        player1.draw(screen)
        player2.draw(screen)
        pygame.display.flip()
        clock.tick(FPS)
    winner_text = f"Player 1 Wins!" if player2.health <= 0 else "Player 2 Wins!"
    screen.fill(WHITE)
    winner_surface = font.render(winner_text, True, (0, 0, 0))
    screen.blit(winner_surface, (WIDTH // 2 - winner_surface.get_width() // 2, HEIGHT // 2 - winner_surface.get_height() // 2))
    pygame.display.flip()
    pygame.time.delay(2000)
    pygame.quit()

if __name__ == "__main__":
    main()

import pygame
import random

pygame.init()

WIDTH, HEIGHT = pygame.display.Info().current_w, pygame.display.Info().current_h
FPS = 60
WHITE = (255, 255, 255)
BLUE = (0, 0, 255)
RED = (255, 0, 0)
YELLOW = (255, 255, 0)
GREEN = (0, 255, 0)
BLACK = (0, 0, 0)
PLAYER_WIDTH, PLAYER_HEIGHT = 50, 50
PICKUP_WIDTH, PICKUP_HEIGHT = 30, 30
GRAVITY = 0.5
JUMP_STRENGTH = 10
MOVE_SPEED = 10
GROUND_HEIGHT = HEIGHT - PLAYER_HEIGHT
HEALTH_BAR_WIDTH = 400
HEALTH_BAR_HEIGHT = 20
HEALTH_BAR_Y = 20
MAX_PICKUPS = 10
BOMB_EXPLOSION_TIME = 3000  # Time in milliseconds before bomb explodes
EXPLOSION_RADIUS = 100  # Radius in which the bomb deals damage
BOMB_DAMAGE = 30  # Damage dealt by the bomb

class Player:
    def __init__(self, x, y, color, controls):
        self.rect = pygame.Rect(x, y, PLAYER_WIDTH, PLAYER_HEIGHT)
        self.color = color
        self.velocity_y = 0
        self.on_ground = True
        self.controls = controls
        self.health = 100
        self.pickup_count = 0
        self.bomb_count = 0

    def move(self, keys, other_player, pickups):
        if keys[self.controls['left']] and self.rect.x > 0:  # Prevent moving out of bounds on the left
            self.rect.x -= MOVE_SPEED
        if keys[self.controls['right']] and self.rect.x < WIDTH - PLAYER_WIDTH:  # Prevent moving out of bounds on the right
            self.rect.x += MOVE_SPEED
        if keys[self.controls['jump']] and self.on_ground:
            self.velocity_y = -JUMP_STRENGTH
            self.on_ground = False
        self.apply_gravity()
        self.check_head_damage(other_player)
        self.check_pickup_collisions(pickups)

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

    def check_pickup_collisions(self, pickups):
        for pickup in pickups:
            if self.rect.colliderect(pickup.rect):
                self.pickup_count += 1
                pickups.remove(pickup)

                # Check if 10 pickups have been collected
                if self.pickup_count % 10 == 0:
                    self.bomb_count += 1
                    self.pickup_count -= 10  # Deduct 10 pickups after getting a bomb

    def take_damage(self, damage):
        self.health -= damage
        if self.health < 0:
            self.health = 0

    def draw(self, screen):
        pygame.draw.rect(screen, self.color, self.rect)

class Pickup:
    def __init__(self, x, y):
        self.rect = pygame.Rect(x, y, PICKUP_WIDTH, PICKUP_HEIGHT)
        self.velocity_y = 0

    def apply_gravity(self):
        self.velocity_y += GRAVITY
        self.rect.y += self.velocity_y

        if self.rect.y >= GROUND_HEIGHT:
            self.rect.y = GROUND_HEIGHT
            self.velocity_y = 0

    def draw(self, screen):
        pygame.draw.rect(screen, YELLOW, self.rect)

class DroppedItem:

    def __init__(self, x, y, is_bomb):
        self.rect = pygame.Rect(x, y, PICKUP_WIDTH, PICKUP_HEIGHT)
        self.is_bomb = is_bomb
        self.velocity_y = 0
        self.explosion_time = pygame.time.get_ticks() + BOMB_EXPLOSION_TIME if is_bomb else None  # Set explosion time for bombs

    def apply_gravity(self):
        if self.is_bomb:
            self.velocity_y += GRAVITY
            self.rect.y += self.velocity_y

            if self.rect.y >= GROUND_HEIGHT:
                self.rect.y = GROUND_HEIGHT
                self.velocity_y = 0

    def draw(self, screen):
        color = BLACK if self.is_bomb else GREEN
        pygame.draw.rect(screen, color, self.rect)

    def explode(self, players):
        if self.is_bomb:
            for player in players:
                # Check if player is within explosion radius
                if player.rect.colliderect(self.rect.inflate(EXPLOSION_RADIUS, EXPLOSION_RADIUS)):
                    player.take_damage(BOMB_DAMAGE)

def draw_health_bar(screen, player1_health, player2_health):
    pygame.draw.rect(screen, WHITE, (50, HEALTH_BAR_Y, HEALTH_BAR_WIDTH, HEALTH_BAR_HEIGHT))
    pygame.draw.rect(screen, WHITE, (WIDTH - 50 - HEALTH_BAR_WIDTH, HEALTH_BAR_Y, HEALTH_BAR_WIDTH, HEALTH_BAR_HEIGHT))
    player1_health_ratio = player1_health / 100
    player2_health_ratio = player2_health / 100
    pygame.draw.rect(screen, BLUE, (50, HEALTH_BAR_Y, HEALTH_BAR_WIDTH * player1_health_ratio, HEALTH_BAR_HEIGHT))
    pygame.draw.rect(screen, RED, (WIDTH - 50 - HEALTH_BAR_WIDTH * player2_health_ratio, HEALTH_BAR_Y, HEALTH_BAR_WIDTH * player2_health_ratio, HEALTH_BAR_HEIGHT))
    
    health_text_player1 = font.render(str(player1_health), True, BLUE)
    health_text_player2 = font.render(str(player2_health), True, RED)
    screen.blit(health_text_player1, (50 + HEALTH_BAR_WIDTH + 10, HEALTH_BAR_Y))
    screen.blit(health_text_player2, (WIDTH - 50 - HEALTH_BAR_WIDTH - health_text_player2.get_width() - 10, HEALTH_BAR_Y))

def draw_pickup_counts(screen, player1_count, player2_count):
    count_text_player1 = font.render(f'Pickups: {player1_count}', True, BLUE)
    count_text_player2 = font.render(f'Pickups: {player2_count}', True, RED)
    screen.blit(count_text_player1, (50, HEALTH_BAR_Y + HEALTH_BAR_HEIGHT + 5))
    screen.blit(count_text_player2, (WIDTH - 50 - count_text_player2.get_width(), HEALTH_BAR_Y + HEALTH_BAR_HEIGHT + 5))

def draw_bomb_counts(screen, player1_bombs, player2_bombs):
    bomb_text_player1 = font.render(f'Bombs: {player1_bombs}', True, BLUE)
    bomb_text_player2 = font.render(f'Bombs: {player2_bombs}', True, RED)
    screen.blit(bomb_text_player1, (50, HEALTH_BAR_Y + HEALTH_BAR_HEIGHT + 30))
    screen.blit(bomb_text_player2, (WIDTH - 50 - bomb_text_player2.get_width(), HEALTH_BAR_Y + HEALTH_BAR_HEIGHT + 30))

def spawn_pickups(existing_pickups):
    if len(existing_pickups) < MAX_PICKUPS:
        x = random.randint(0, WIDTH - PICKUP_WIDTH)  # Ensure pickups spawn within bounds
        pickup = Pickup(x, 0)  # Spawn at a random x position at the top
        existing_pickups.append(pickup)

pygame.font.init()
font = pygame.font.Font(None, 36)

def main():
    screen = pygame.display.set_mode((WIDTH, HEIGHT), pygame.FULLSCREEN)
    pygame.display.set_caption("nita")
    background = pygame.image.load("asset/sf2.jpg")
    background = pygame.transform.scale(background, (WIDTH, HEIGHT))
    clock = pygame.time.Clock()
    
    player1_controls = {
        'left': pygame.K_a,
        'right': pygame.K_d,
        'jump': pygame.K_w,
        'drop': pygame.K_f,  # Drop bomb/pickup key for Player 1
    }
    player2_controls = {
        'left': pygame.K_LEFT,
        'right': pygame.K_RIGHT,
        'jump': pygame.K_UP,
        'drop': pygame.K_0,  # Drop bomb/pickup key for Player 2
    }
    
    player1 = Player(100, GROUND_HEIGHT, BLUE, player1_controls)
    player2 = Player(600, GROUND_HEIGHT, RED, player2_controls)

    pickups = []  # Start with an empty list for pickups
    dropped_items = []  # List to store dropped bombs and pickups

    bomb_cooldown_player1 = 0  # Timer for Player 1 bomb cooldown
    bomb_cooldown_player2 = 0  # Timer for Player 2 bomb cooldown
    bomb_cooldown_time = 3000  # 3 seconds in milliseconds

    running = True
    while running:
        for event in pygame.event.get():
            if event.type == pygame.QUIT:
                running = False

        keys = pygame.key.get_pressed()
        player1.move(keys, player2, pickups)
        player2.move(keys, player1, pickups)

        # Check for dropping items for Player 1
        if keys[player1_controls['drop']] and bomb_cooldown_player1 <= 0:
            if player1.bomb_count > 0:  # Drop a bomb
                dropped_items.append(DroppedItem(player1.rect.x + PLAYER_WIDTH // 2 - PICKUP_WIDTH // 2, player1.rect.y, True))
                player1.bomb_count -= 1  # Deduct bomb
                bomb_cooldown_player1 = bomb_cooldown_time  # Start cooldown
            elif player1.pickup_count > 0:  # Drop a pickup if available
                dropped_items.append(DroppedItem(player1.rect.x + PLAYER_WIDTH // 2 - PICKUP_WIDTH // 2, player1.rect.y, False))
                player1.pickup_count -= 1  # Deduct pickup

        # Check for dropping items for Player 2
        if keys[player2_controls['drop']] and bomb_cooldown_player2 <= 0:
            if player2.bomb_count > 0:  # Drop a bomb
                dropped_items.append(DroppedItem(player2.rect.x + PLAYER_WIDTH // 2 - PICKUP_WIDTH // 2, player2.rect.y, True))
                player2.bomb_count -= 1  # Deduct bomb
                bomb_cooldown_player2 = bomb_cooldown_time  # Start cooldown
            elif player2.pickup_count > 0:  # Drop a pickup if available
                dropped_items.append(DroppedItem(player2.rect.x + PLAYER_WIDTH // 2 - PICKUP_WIDTH // 2, player2.rect.y, False))
                player2.pickup_count -= 1  # Deduct pickup

        # Update bomb cooldowns
        bomb_cooldown_player1 -= 1000 / FPS
        bomb_cooldown_player2 -= 1000 / FPS

        # Spawn new pickups
        spawn_pickups(pickups)

        # Update and draw dropped items
        for dropped_item in dropped_items[:]:  # Create a copy of the list to avoid modifying it during iteration
            dropped_item.apply_gravity()
            if dropped_item.is_bomb and pygame.time.get_ticks() >= dropped_item.explosion_time:
                dropped_item.explode([player1, player2])
                dropped_items.remove(dropped_item)  # Remove the bomb after it explodes
            dropped_item.draw(screen)

        # Draw everything else
        screen.blit(background, (0, 0))
        player1.draw(screen)
        player2.draw(screen)
        for pickup in pickups:
            pickup.apply_gravity()
            pickup.draw(screen)
        draw_health_bar(screen, player1.health, player2.health)
        draw_pickup_counts(screen, player1.pickup_count, player2.pickup_count)
        draw_bomb_counts(screen, player1.bomb_count, player2.bomb_count)

        pygame.display.flip()
        clock.tick(FPS)

    pygame.quit()

if __name__ == "__main__":
    main()
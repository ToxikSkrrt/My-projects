import pygame
import random

class Monster(pygame.sprite.Sprite):
    def __init__(self, x, y):
        super().__init__()
        self.spriteSheet = pygame.image.load('assets/assets.png')
        self.image = self.getImage(0, 0)
        self.image.set_colorkey([0, 0, 0])
        self.rect = self.image.get_rect()
        self.position = [x, y]
        self.feet = pygame.Rect(0, 0, self.rect.width * 0.5, 6)
        self.oldPosition = self.position.copy()
        self.moveSpeed = 2
        self.timer = random.randint(0, 300)
        self.random = random.randint(0, 7)
    
    def saveLocation(self):
        self.oldPosition = self.position.copy()
        
    def moveUp(self):
        self.position[1] -= self.moveSpeed
        
    def moveDown(self):
        self.position[1] += self.moveSpeed
        
    def moveLeft(self):
        self.position[0] -= self.moveSpeed
        
    def moveRight(self):
        self.position[0] += self.moveSpeed
        
    def moveUpLeft(self):
        self.position[1] -= self.moveSpeed * 0.75
        self.position[0] -= self.moveSpeed * 0.75
        
    def moveUpRight(self):
        self.position[1] -= self.moveSpeed * 0.75
        self.position[0] += self.moveSpeed * 0.75
        
    def moveDownLeft(self):
        self.position[1] += self.moveSpeed * 0.75
        self.position[0] -= self.moveSpeed * 0.75
        
    def moveDownRight(self):
        self.position[1] += self.moveSpeed * 0.75
        self.position[0] += self.moveSpeed * 0.75
    
    def roaming(self):
        self.timer += 1
        if self.timer == 100:
            self.random = random.randint(0, 7)
        if self.timer > 299:
            self.timer = 0
        if self.timer < 100:
            if self.random == 0:
                self.moveUp()
            elif self.random == 1:
                self.moveDown()
            elif self.random == 2:
                self.moveLeft()
            elif self.random == 3:
                self.moveRight()
            elif self.random == 4:
                self.moveUpLeft()
            elif self.random == 5:
                self.moveUpRight()
            elif self.random == 6:
                self.moveDownLeft()
            elif self.random == 7:
                self.moveDownRight()
    
    def update(self):
        self.rect.topleft = self.position
        self.feet.midbottom = self.rect.midbottom
        
    def moveBack(self):
        self.position = self.oldPosition
        self.rect.topleft = self.position
        self.feet.midbottom = self.rect.midbottom
  
    def getImage(self, x, y):
        image = pygame.Surface([16, 16])
        image.blit(self.spriteSheet, (0, 0), (x, y, 16, 16))
        return image
    
    
class Skeleton(Monster):
    def __init__(self, x, y):
        Monster.__init__(self, x, y)
        self.image = self.getImage(368, 80)
        self.image.set_colorkey([0, 0, 0])
        self.moveSpeed *= 1.2
        
        
class Slime(Monster):
    def __init__(self, x, y):
        Monster.__init__(self, x, y)
        self.image = self.getImage(448, 112)
        self.image.set_colorkey([0, 0, 0])
        self.feet = pygame.Rect(0, 0, self.rect.width * 1.2, 6)
        self.moveSpeed *= 0.45
        
        
class Orc(Monster):
    def __init__(self, x, y):
        Monster.__init__(self, x, y)
        self.image = self.getImage(368, 208)
        self.image.set_colorkey([0, 0, 0])
        self.moveSpeed *= 1.15
        
        
class Troll(Monster):
    def __init__(self, x, y):
        Monster.__init__(self, x, y)
        self.image = self.getImage(368, 176)
        self.image.set_colorkey([0, 0, 0])
        self.moveSpeed *= 1.25
        
        
class Hellion(Monster):
    def __init__(self, x, y):
        Monster.__init__(self, x, y)
        self.image = self.getImage(368, 320)
        self.image.set_colorkey([0, 0, 0])
        self.moveSpeed *= 1.65
        
    def getImage(self, x, y):
        image = pygame.Surface([16, 32])
        image.blit(self.spriteSheet, (0, 0), (x, y, 16, 32))
        return image
        
        
class BigHellion(Monster):
    def __init__(self, x, y):
        Monster.__init__(self, x, y)
        self.image = self.getImage(16, 368)
        self.image.set_colorkey([0, 0, 0])
        self.moveSpeed *= 1
        
    def getImage(self, x, y):
        image = pygame.Surface([32, 32])
        image.blit(self.spriteSheet, (0, 0), (x, y, 32, 32))
        return image


class TrainingDummy(pygame.sprite.Sprite):
    def __init__(self, x, y):
        super().__init__()
        self.spriteSheet = pygame.image.load('assets/assets.png')
        self.image = self.getImage(16, 272)
        self.image.set_colorkey([0, 0, 0])
        self.rect = self.image.get_rect()
        self.position = [x, y]
        self.feet = pygame.Rect(0, 0, self.rect.width * 0.5, 6)
        
    def update(self):
        self.rect.topleft = self.position
        
    def getImage(self, x, y):
        image = pygame.Surface([32, 32])
        image.blit(self.spriteSheet, (0, 0), (x, y, 32, 32))
        return image
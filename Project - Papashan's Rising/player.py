import pygame

class Player(pygame.sprite.Sprite):
    def __init__(self, x, y):
        super().__init__()
        self.spriteSheet = pygame.image.load('assets/player.png')
        self.image = self.getImage(0, 0)
        self.image.set_colorkey([0, 0, 0])
        self.rect = self.image.get_rect()
        self.position = [x, y]
        self.images = {
            'down':self.getImage(0, 0),
            'left':self.getImage(0, 16),
            'right':self.getImage(0, 32),
            'up':self.getImage(0, 48)
        }
        self.feet = pygame.Rect(0, 0, self.rect.width * 0.5, 6)
        self.oldPosition = self.position.copy()
        self.moveSpeed = 2
        
    def saveLocation(self):
        self.oldPosition = self.position.copy()
        
    def changeAnimation(self, name):
        self.image = self.images[name]
        self.image.set_colorkey((0, 0, 0))
    
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
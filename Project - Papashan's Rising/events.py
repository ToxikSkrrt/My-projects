import pygame

class Event(pygame.sprite.Sprite):
    def __init__(self, x, y):
        super().__init__()
        self.spriteSheet = pygame.image.load('assets/assets.png')
        self.image = self.getImage(0, 0)
        self.image.set_colorkey([0, 0, 0])
        self.rect = self.image.get_rect()
        self.position = [x, y]
        self.collideBox = pygame.Rect(0, 0, self.rect.width * 0.8, 6)

    def update(self):
        self.rect.topleft = self.position
        self.collideBox.midbottom = self.rect.midbottom
        
    def getImage(self, x, y):
        image = pygame.Surface([16, 16])
        image.blit(self.spriteSheet, (0, 0), (x, y, 16, 16))
        return image
    
    
class Door(Event):
    def __init__(self, x, y):
        Event.__init__(self, x, y)
        self.image = self.getImage(32, 224)
        self.image.set_colorkey([0, 0, 0])
        
    def getImage(self, x, y):
        image = pygame.Surface([32, 32])
        image.blit(self.spriteSheet, (0, 0), (x, y, 32, 32))
        return image
        
    def changeAnimation(self):
        self.image = self.getImage(80, 224)
        self.image.set_colorkey((0, 0, 0))
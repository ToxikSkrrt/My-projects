import pygame

class NPC(pygame.sprite.Sprite):
    def __init__(self, x, y):
        super().__init__()
        self.spriteSheet = pygame.image.load('assets/assets.png')
        self.image = self.getImage(0, 0)
        self.image.set_colorkey([0, 0, 0])
        self.rect = self.image.get_rect()
        self.position = [x, y]
        
    def update(self):
        self.rect.topleft = self.position
        
    def getImage(self, x, y):
        image = pygame.Surface([16, 16])
        image.blit(self.spriteSheet, (0, 0), (x, y, 16, 16))
        return image
    
    
class Wizard(NPC):
    def __init__(self, x, y):
        Monster.__init__(self, x, y)
        self.image = self.getImage(128, 176)
        self.image.set_colorkey([0, 0, 0])
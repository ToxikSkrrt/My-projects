import pygame
import pytmx
import pyscroll
from player import *
from monsters import *
from npcs import *
from events import *

class Game:
    admin = False
    
    def __init__(self):
        self.screen = pygame.display.set_mode((800, 600), pygame.SCALED)
        pygame.display.set_caption("Papashan's Rising")
        
        global tmxData
        tmxData = pytmx.util_pygame.load_pygame('maps/starting map.tmx')
        mapData = pyscroll.data.TiledMapData(tmxData)
        mapLayer = pyscroll.orthographic.BufferedRenderer(mapData, self.screen.get_size())
        mapLayer.zoom = 3
        
        # Player
        playerPosition = tmxData.get_object_by_name('player')
        
        self.player = Player(playerPosition.x, playerPosition.y)
        
        # Monsters
        skeleton1Position = tmxData.get_object_by_name('skeleton1')
        skeleton2Position = tmxData.get_object_by_name('skeleton2')
        skeleton3Position = tmxData.get_object_by_name('skeleton3')
        skeleton4Position = tmxData.get_object_by_name('skeleton4')
        skeleton5Position = tmxData.get_object_by_name('skeleton5')
        slime1Position = tmxData.get_object_by_name('slime1')
        slime2Position = tmxData.get_object_by_name('slime2')
        slime3Position = tmxData.get_object_by_name('slime3')
        troll1Position = tmxData.get_object_by_name('troll1')
        troll2Position = tmxData.get_object_by_name('troll2')
        troll3Position = tmxData.get_object_by_name('troll3')
        troll4Position = tmxData.get_object_by_name('troll4')
        orc1Position = tmxData.get_object_by_name('orc1')
        orc2Position = tmxData.get_object_by_name('orc2')
        orc3Position = tmxData.get_object_by_name('orc3')
        orc4Position = tmxData.get_object_by_name('orc4')
        orc5Position = tmxData.get_object_by_name('orc5')
        orc6Position = tmxData.get_object_by_name('orc6')
        hellion1Position = tmxData.get_object_by_name('hellion1')
        hellion2Position = tmxData.get_object_by_name('hellion2')
        hellion3Position = tmxData.get_object_by_name('hellion3')
        hellion4Position = tmxData.get_object_by_name('hellion4')
        hellion5Position = tmxData.get_object_by_name('hellion5')
        bighellion1Position = tmxData.get_object_by_name('bighellion1')
        trainingdummy1Position = tmxData.get_object_by_name('trainingdummy1')
        trainingdummy2Position = tmxData.get_object_by_name('trainingdummy2')
        trainingdummy3Position = tmxData.get_object_by_name('trainingdummy3')
        trainingdummy4Position = tmxData.get_object_by_name('trainingdummy4')
        trainingdummy5Position = tmxData.get_object_by_name('trainingdummy5')
        trainingdummy6Position = tmxData.get_object_by_name('trainingdummy6')
        trainingdummy7Position = tmxData.get_object_by_name('trainingdummy7')
        
        monstersPosition = [[skeleton1Position, skeleton2Position, skeleton3Position, skeleton4Position, skeleton5Position],
                            [slime1Position, slime2Position, slime3Position],
                            [troll1Position, troll2Position, troll3Position, troll4Position],
                            [orc1Position, orc2Position, orc3Position, orc4Position, orc5Position, orc6Position],
                            [hellion1Position, hellion2Position, hellion3Position, hellion4Position, hellion5Position],
                            [bighellion1Position],
                            [trainingdummy1Position, trainingdummy2Position, trainingdummy3Position, trainingdummy4Position, trainingdummy5Position, trainingdummy6Position, trainingdummy7Position]
                            ]
        
        self.skeleton1 = Skeleton(monstersPosition[0][0].x, monstersPosition[0][0].y)
        self.skeleton2 = Skeleton(monstersPosition[0][1].x, monstersPosition[0][1].y)
        self.skeleton3 = Skeleton(monstersPosition[0][2].x, monstersPosition[0][2].y)
        self.skeleton4 = Skeleton(monstersPosition[0][3].x, monstersPosition[0][3].y)
        self.skeleton5 = Skeleton(monstersPosition[0][4].x, monstersPosition[0][4].y)
        self.skeleton = [self.skeleton1, self.skeleton2, self.skeleton3, self.skeleton4, self.skeleton5]
        
        self.slime1 = Slime(monstersPosition[1][0].x, monstersPosition[1][0].y)
        self.slime2 = Slime(monstersPosition[1][1].x, monstersPosition[1][1].y)
        self.slime3 = Slime(monstersPosition[1][2].x, monstersPosition[1][2].y)
        self.slime = [self.slime1, self.slime2, self.slime3]
        
        self.troll1 = Troll(monstersPosition[2][0].x, monstersPosition[2][0].y)
        self.troll2 = Troll(monstersPosition[2][1].x, monstersPosition[2][1].y)
        self.troll3 = Troll(monstersPosition[2][2].x, monstersPosition[2][2].y)
        self.troll4 = Troll(monstersPosition[2][3].x, monstersPosition[2][3].y)
        self.troll = [self.troll1, self.troll2, self.troll3, self.troll4]
        
        self.orc1 = Orc(monstersPosition[3][0].x, monstersPosition[3][0].y)
        self.orc2 = Orc(monstersPosition[3][1].x, monstersPosition[3][1].y)
        self.orc3 = Orc(monstersPosition[3][2].x, monstersPosition[3][2].y)
        self.orc4 = Orc(monstersPosition[3][3].x, monstersPosition[3][3].y)
        self.orc5 = Orc(monstersPosition[3][4].x, monstersPosition[3][4].y)
        self.orc6 = Orc(monstersPosition[3][5].x, monstersPosition[3][5].y)
        self.orc = [self.orc1, self.orc2, self.orc3, self.orc4, self.orc5, self.orc6]
        
        self.hellion1 = Hellion(monstersPosition[4][0].x, monstersPosition[4][0].y)
        self.hellion2 = Hellion(monstersPosition[4][1].x, monstersPosition[4][1].y)
        self.hellion3 = Hellion(monstersPosition[4][2].x, monstersPosition[4][2].y)
        self.hellion4 = Hellion(monstersPosition[4][3].x, monstersPosition[4][3].y)
        self.hellion5 = Hellion(monstersPosition[4][4].x, monstersPosition[4][4].y)
        self.hellion = [self.hellion1, self.hellion2, self.hellion3, self.hellion4, self.hellion5]
        
        self.bighellion1 = BigHellion(monstersPosition[5][0].x, monstersPosition[5][0].y)
        self.bighellion = [self.bighellion1]
        
        self.trainingdummy1 = TrainingDummy(monstersPosition[6][0].x, monstersPosition[6][0].y)
        self.trainingdummy2 = TrainingDummy(monstersPosition[6][1].x, monstersPosition[6][1].y)
        self.trainingdummy3 = TrainingDummy(monstersPosition[6][2].x, monstersPosition[6][2].y)
        self.trainingdummy4 = TrainingDummy(monstersPosition[6][3].x, monstersPosition[6][3].y)
        self.trainingdummy5 = TrainingDummy(monstersPosition[6][4].x, monstersPosition[6][4].y)
        self.trainingdummy6 = TrainingDummy(monstersPosition[6][5].x, monstersPosition[6][5].y)
        self.trainingdummy7 = TrainingDummy(monstersPosition[6][6].x, monstersPosition[6][6].y)
        self.trainingdummy = [self.trainingdummy1, self.trainingdummy2, self.trainingdummy3, self.trainingdummy4, self.trainingdummy5, self.trainingdummy6, self.trainingdummy7]
        
        # NPCs
        
        
        # Events
        door1Position = tmxData.get_object_by_name('door1')
        
        eventsPosition = [[door1Position]
                          ]
        
        self.door1 = Door(eventsPosition[0][0].x, eventsPosition[0][0].y)
        self.door = [self.door1]
        
        self.group = pyscroll.PyscrollGroup(map_layer = mapLayer, default_layer = 3)
        self.collidegroup = pyscroll.PyscrollGroup()
        self.eventgroup = pyscroll.PyscrollGroup()
        
        self.group.add(self.player)
        self.group.add(self.skeleton[0])
        self.group.add(self.skeleton[1])
        self.group.add(self.skeleton[2])
        self.group.add(self.skeleton[3])
        self.group.add(self.skeleton[4])
        self.group.add(self.slime[0])
        self.group.add(self.slime[1])
        self.group.add(self.slime[2])
        self.group.add(self.troll[0])
        self.group.add(self.troll[1])
        self.group.add(self.troll[2])
        self.group.add(self.troll[3])
        self.group.add(self.orc[0])
        self.group.add(self.orc[1])
        self.group.add(self.orc[2])
        self.group.add(self.orc[3])
        self.group.add(self.orc[4])
        self.group.add(self.orc[5])
        self.group.add(self.hellion[0])
        self.group.add(self.hellion[1])
        self.group.add(self.hellion[2])
        self.group.add(self.hellion[3])
        self.group.add(self.hellion[4])
        self.group.add(self.bighellion[0])
        self.group.add(self.trainingdummy[0])
        self.group.add(self.trainingdummy[1])
        self.group.add(self.trainingdummy[2])
        self.group.add(self.trainingdummy[3])
        self.group.add(self.trainingdummy[4])
        self.group.add(self.trainingdummy[5])
        self.group.add(self.trainingdummy[6])
        self.group.add(self.door[0])
        
        # Temporaire
        """self.collidegroup.add(self.player)"""
        self.collidegroup.add(self.skeleton[0])
        self.collidegroup.add(self.skeleton[1])
        self.collidegroup.add(self.skeleton[2])
        self.collidegroup.add(self.skeleton[3])
        self.collidegroup.add(self.skeleton[4])
        self.collidegroup.add(self.slime[0])
        self.collidegroup.add(self.slime[1])
        self.collidegroup.add(self.slime[2])
        self.collidegroup.add(self.troll[0])
        self.collidegroup.add(self.troll[1])
        self.collidegroup.add(self.troll[2])
        self.collidegroup.add(self.troll[3])
        self.collidegroup.add(self.orc[0])
        self.collidegroup.add(self.orc[1])
        self.collidegroup.add(self.orc[2])
        self.collidegroup.add(self.orc[3])
        self.collidegroup.add(self.orc[4])
        self.collidegroup.add(self.orc[5])
        self.collidegroup.add(self.hellion[0])
        self.collidegroup.add(self.hellion[1])
        self.collidegroup.add(self.hellion[2])
        self.collidegroup.add(self.hellion[3])
        self.collidegroup.add(self.hellion[4])
        self.collidegroup.add(self.bighellion[0])
        
        self.eventgroup.add(self.door[0])
        
        self.walls = []
        for obj in tmxData.objects:
            if obj.type == 'collision':
                self.walls.append(pygame.Rect(obj.x, obj.y, obj.width, obj.height))
                
        self.layerswitch = []
        for objs in tmxData.objects:
            if objs.type == 'layerswitch':
                self.layerswitch.append(pygame.Rect(objs.x, objs.y, objs.width, objs.height))
                
        self.triggers = []
        for objS in tmxData.objects:
            if objS.type == 'trigger':
                self.triggers.append(pygame.Rect(objS.x, objS.y, objS.width, objS.height))
        
        self.collideBoxes = []
        for obJS in self.eventgroup:
            self.collideBoxes.append(obJS.collideBox)
    
    def handleInput(self):
        pressed = pygame.key.get_pressed()
        
        # Temporaire
        if pressed[pygame.K_SPACE]:
            Game.admin = True
        else:
            Game.admin = False
            
        # Temporaire
        if pressed[pygame.K_UP]:
            self.skeleton[0].moveUp()
        elif pressed[pygame.K_DOWN]:
            self.skeleton[0].moveDown()
        elif pressed[pygame.K_LEFT]:
            self.skeleton[0].moveLeft()
        elif pressed[pygame.K_RIGHT]:
            self.skeleton[0] .moveRight()
            
        if pressed[pygame.K_z] and pressed[pygame.K_q]:
            self.player.moveUpLeft()
            self.player.changeAnimation('left')
        elif pressed[pygame.K_z] and pressed[pygame.K_d]:
            self.player.moveUpRight()
            self.player.changeAnimation('right')
        elif pressed[pygame.K_s] and pressed[pygame.K_q]:
            self.player.moveDownLeft()
            self.player.changeAnimation('left')
        elif pressed[pygame.K_s] and pressed[pygame.K_d]:
            self.player.moveDownRight()
            self.player.changeAnimation('right')
        elif pressed[pygame.K_z]:
            self.player.moveUp()
            self.player.changeAnimation('up')
        elif pressed[pygame.K_s]:
            self.player.moveDown()
            self.player.changeAnimation('down')
        elif pressed[pygame.K_q]:
            self.player.moveLeft()
            self.player.changeAnimation('left')
        elif pressed[pygame.K_d]:
            self.player.moveRight()
            self.player.changeAnimation('right')
            
        if self.player.feet.collidelist(self.triggers) > -1 and pressed[pygame.K_e]:
            for obJ in tmxData.objects:
                if obJ.name == 'trigDoor1':
                    self.door[0].changeAnimation()
                    try:
                        self.collideBoxes.remove(self.door[0].collideBox)
                    except ValueError:
                        pass
                    
                    
    def update(self):
        self.group.update()
        
        # Temporaire
        if Game.admin == False:
            if self.player.feet.collidelist(self.walls) > -1 or self.player.feet.collidelist(self.collideBoxes) > -1:
                self.player.moveBack()
            
        for sprite in self.collidegroup.sprites():
            if sprite.feet.collidelist(self.walls) > -1 or sprite.feet.collidelist(self.collideBoxes) > -1:
                sprite.moveBack()
                
        for spriteL in self.group.sprites():
            if spriteL not in self.eventgroup.sprites():
                if spriteL.feet.collidelist(self.layerswitch) > -1:
                    self.group.change_layer(spriteL, 2)
                else:
                    self.group.change_layer(spriteL, 3)
                        
                
    def run(self):
        clock = pygame.time.Clock()
        
        running =  True

        while running:
            self.player.saveLocation()
            
            self.skeleton[0].saveLocation()
            self.skeleton[1].saveLocation()
            self.skeleton[2].saveLocation()
            self.skeleton[3].saveLocation()
            self.skeleton[4].saveLocation()
            self.slime[0].saveLocation()
            self.slime[1].saveLocation()
            self.slime[2].saveLocation()
            self.troll[0].saveLocation()
            self.troll[1].saveLocation()
            self.troll[2].saveLocation()
            self.troll[3].saveLocation()
            self.orc[0].saveLocation()
            self.orc[1].saveLocation()
            self.orc[2].saveLocation()
            self.orc[3].saveLocation()
            self.orc[4].saveLocation()
            self.orc[5].saveLocation()
            self.hellion[0].saveLocation()
            self.hellion[1].saveLocation()
            self.hellion[2].saveLocation()
            self.hellion[3].saveLocation()
            self.hellion[4].saveLocation()
            self.bighellion[0].saveLocation()
            
            self.skeleton[0].roaming()
            self.skeleton[1].roaming()
            self.skeleton[2].roaming()
            self.skeleton[3].roaming()
            self.skeleton[4].roaming()
            self.slime[0].roaming()
            self.slime[1].roaming()
            self.slime[2].roaming()
            self.troll[0].roaming()
            self.troll[1].roaming()
            self.troll[2].roaming()
            self.troll[3].roaming()
            self.orc[0].roaming()
            self.orc[1].roaming()
            self.orc[2].roaming()
            self.orc[3].roaming()
            self.orc[4].roaming()
            self.orc[5].roaming()
            self.hellion[0].roaming()
            self.hellion[1].roaming()
            self.hellion[2].roaming()
            self.hellion[3].roaming()
            self.hellion[4].roaming()
            self.bighellion[0].roaming()
            
            self.handleInput()
            self.update()
            self.group.center(self.player.rect.center)
            self.group.draw(self.screen)
            
            pygame.display.flip()
            
            for event in pygame.event.get():
                if event.type == pygame.QUIT:
                    running = False
            
            clock.tick(60)
            
        pygame.quit()
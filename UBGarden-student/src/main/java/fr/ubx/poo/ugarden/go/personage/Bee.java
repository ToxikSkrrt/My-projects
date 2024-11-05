package fr.ubx.poo.ugarden.go.personage;

import fr.ubx.poo.ugarden.game.Direction;
import fr.ubx.poo.ugarden.game.Game;
import fr.ubx.poo.ugarden.game.Position;
import fr.ubx.poo.ugarden.go.GameObject;

public class Bee extends GameObject {
    private Direction direction;

    public Direction getDirection() {
        return direction;
    }
    public Bee(Game game, Position position) {
        super(game, position);
        this.direction = Direction.DOWN;
    }
}

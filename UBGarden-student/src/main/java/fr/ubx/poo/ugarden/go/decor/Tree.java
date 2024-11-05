/*
 * Copyright (c) 2020. Laurent Réveillère
 */

package fr.ubx.poo.ugarden.go.decor;
import fr.ubx.poo.ugarden.game.Position;
import fr.ubx.poo.ugarden.go.personage.Player;

public class Tree extends Decor {
    public Tree(Position position) {
        super(position);
    }
    public boolean walkableBy(Player player) {
        return false;
    }
}

#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "game.h"
#include "game_aux.h"
#include "game_ext.h"
#include "game_tools.h"

void usage(int argc, char *argv[]) {
    fprintf(stderr, "Usage: %s <testname> [<...>]\n", argv[0]);
    exit(EXIT_FAILURE);
}

bool test_dummy() {
    return true;
}

bool test_game_is_over() {
    bool test = true;
    game g = game_default_solution();
    if (!game_is_over(g)) test = false;
    game_play_move(g, 0, 0, S_EMPTY);
    if (game_is_over(g)) test = false;
    game g2 = game_new_empty();
    for (int i = 0; i < 3; i++) {
        game_play_move(g2, i, 0, S_ZERO);
        game_play_move(g2, i, 1, S_ZERO);
        game_play_move(g2, i, 2, S_ZERO);
        game_play_move(g2, i, 3, S_ONE);
        game_play_move(g2, i, 4, S_ONE);
        game_play_move(g2, i, 5, S_ONE);
    }
    for (int i = 3; i < DEFAULT_SIZE; i++) {
        game_play_move(g2, i, 0, S_ONE);
        game_play_move(g2, i, 1, S_ONE);
        game_play_move(g2, i, 2, S_ONE);
        game_play_move(g2, i, 3, S_ZERO);
        game_play_move(g2, i, 4, S_ZERO);
        game_play_move(g2, i, 5, S_ZERO);
    }
    if (game_is_over(g2)) test = false;
    game g3 = game_new_empty_ext(4, 4, true, true);
    for (int i = 0; i < 2; i++) {
        game_play_move(g3, i, 0, S_ZERO);
        game_play_move(g3, i, 1, S_ONE);
        game_play_move(g3, i, 2, S_ZERO);
        game_play_move(g3, i, 3, S_ONE);
    }
    for (int i = 2; i < 4; i++) {
        game_play_move(g3, i, 0, S_ONE);
        game_play_move(g3, i, 1, S_ZERO);
        game_play_move(g3, i, 2, S_ONE);
        game_play_move(g3, i, 3, S_ZERO);
    }
    if (game_is_over(g3)) test = false;
    game_delete(g);
    game_delete(g2);
    game_delete(g3);
    return test;
}

bool test_game_restart() {
    bool test = true;
    game g1 = game_default_solution();
    game g2 = game_default();
    game_play_move(g1, 0, 0, S_ONE);
    game_play_move(g1, 0, 0, S_ZERO);
    game_restart(g1);
    game_undo(g1);
    if (game_equal(g1, g2) == false) test = false;
    game_restart(g2);
    if (game_equal(g1, g2) == false) test = false;
    game_delete(g1);
    game_delete(g2);
    return test;
}

bool test_game_play_move() {
    bool test = true;
    game g = game_default();
    game_play_move(g, 0, 0, S_ONE);
    if (game_get_square(g, 0, 0) != S_ONE) test = false;
    game_delete(g);
    return test;
}

bool test_game_new_empty() {
    game g = game_new_empty();
    for (int i = 0; i < DEFAULT_SIZE; i++) {
        for (int j = 0; j < DEFAULT_SIZE; j++) {
            if (game_get_square(g, i, j) != S_EMPTY) {
                game_delete(g);
                return false;
            }
        }
    }
    game_delete(g);
    return true;
}

bool test_game_new() {
    bool test = true;
    square tab[36] = {S_EMPTY};
    game g1 = game_new(tab);
    game g2 = game_new_empty();
    if (game_equal(g1, g2) == false) test = false;
    tab[0] = S_ONE;
    game_set_square(g2, 0, 0, S_ONE);
    game_delete(g1);
    game g3 = game_new(tab);
    if (game_equal(g3, g2) == false) test = false;
    game_delete(g3);
    game_delete(g2);
    return test;
}

bool test_game_default_solution() {
    game g = game_default_solution();
    if (game_is_over(g)) {
        game_delete(g);
        return true;
    }
    game_delete(g);
    return false;
}

bool test_game_print() {
    game g = game_default();
    game_print(g);
    game_delete(g);
    return true;
}

bool test_game_undo() {
    bool test = true;
    game g = game_new_empty();
    game_play_move(g, 0, 0, S_ZERO);
    game_play_move(g, 0, 1, S_ONE);
    game_undo(g);
    if (game_get_square(g, 0, 1) == S_ONE) test = false;
    game_undo(g);
    if (game_get_square(g, 0, 0) == S_ZERO) test = false;
    game_delete(g);
    return test;
}

bool test_game_redo() {
    bool test = true;
    game g = game_new_empty();
    game_play_move(g, 0, 0, S_ZERO);
    game_play_move(g, 0, 1, S_ONE);
    game_undo(g);
    game_undo(g);
    game_redo(g);
    if (game_get_square(g, 0, 0) != S_ZERO) test = false;
    game_undo(g);
    if (game_get_square(g, 0, 0) != S_EMPTY) test = false;
    game_redo(g);
    game_redo(g);
    if (game_get_square(g, 0, 1) != S_ONE) test = false;
    game_delete(g);
    return test;
}

bool test_game_load() {
    bool test = true;
    game g1 = game_default();
    game_play_move(g1, 0, 0, S_ZERO);
    game_play_move(g1, 1, 0, S_ONE);
    game_save(g1, "test1");
    game_delete(g1);

    game g2 = game_load("test1");
    if (game_get_square(g2, 0, 0) != S_ZERO ||
        game_get_square(g2, 1, 0) != S_ONE)
        test = false;
    game_play_move(g2, 1, 1, S_ZERO);
    game_play_move(g2, 1, 2, S_ONE);
    game_save(g2, "test2");
    game_delete(g2);

    game g3 = game_load("test2");
    if (game_get_square(g3, 0, 0) != S_ZERO ||
        game_get_square(g3, 1, 0) != S_ONE ||
        game_get_square(g3, 1, 1) != S_ZERO ||
        game_get_square(g3, 1, 2) != S_ONE)
        test = false;
    game_delete(g3);

    game g4 = game_new_empty_ext(8, 6, true, true);
    game_save(g4, "testExts");
    game_delete(g4);

    game g5 = game_load("testExts");
    if (!game_is_wrapping(g5) || !game_is_unique(g5)) test = false;
    game_delete(g5);

    remove("test1");
    remove("test2");
    remove("testExts");
    return test;
}

bool test_game_save() {
    bool test = true;

    game g1 = game_default_solution();
    game_save(g1, "gsolution");

    game g2 = game_default();
    game_save(g2, "gdefault");

    game g3 = game_load("solution.txt");
    game g4 = game_load("default.txt");

    if (!game_equal(g1, g3) || !game_equal(g2, g4)) test = false;

    game_delete(g1);
    game_delete(g2);
    game_delete(g3);
    game_delete(g4);

    game g5 = game_new_empty_ext(8, 6, true, false);
    game_save(g5, "gexts");

    game g6 = game_load("gexts");

    if (!game_equal(g5, g6)) test = false;

    game_delete(g5);
    game_delete(g6);

    remove("gsolution");
    remove("gdefault");
    remove("gexts");

    return test;
}

bool test_game_solve() {
    bool test = true;
    game g = game_default();
    game gsol = game_default_solution();
    if (game_solve(g) && game_is_over(g)) {
        // the default game has only one solution
        //(which is game_default_solution)
        if (!game_equal(g, gsol)) test = false;
    } else {
        test = false;
    }
    game_delete(gsol);
    game_delete(g);

    game g2 = game_load("4x4_2solutions.txt");
    if (!game_solve(g2) || !game_is_over(g2)) {
        test = false;
    }
    game_delete(g2);

    game g3 = game_load("4x4_72solutions.txt");
    if (!game_solve(g3) && game_is_over(g3)) {
        test = false;
    }
    game_delete(g3);
    return test;
}

bool test_game_nb_solutions() {
    bool test = true;
    game g = game_load("4x4_2solutions.txt");
    if (game_nb_solutions(g) != 2) test = false;
    game_delete(g);

    game g2 = game_load("4x4_72solutions.txt");
    if (game_nb_solutions(g2) != 72) test = false;
    game_delete(g2);

    game g3 = game_load("4x4_0solution.txt");
    if (game_nb_solutions(g3) != 0) test = false;
    game_delete(g3);

    return test;
}

int main(int argc, char *argv[]) {
    if (argc == 1) usage(argc, argv);

    fprintf(stderr, "=> Start test \"%s\"\n", argv[1]);
    bool ok = false;

    if (strcmp("dummy", argv[1]) == 0)
        ok = test_dummy();
    else if (strcmp("game_is_over", argv[1]) == 0)
        ok = test_game_is_over();
    else if (strcmp("game_restart", argv[1]) == 0)
        ok = test_game_restart();
    else if (strcmp("game_play_move", argv[1]) == 0)
        ok = test_game_play_move();
    else if (strcmp("game_new_empty", argv[1]) == 0)
        ok = test_game_new_empty();
    else if (strcmp("game_new", argv[1]) == 0)
        ok = test_game_new();
    else if (strcmp("game_default_solution", argv[1]) == 0)
        ok = test_game_default_solution();
    else if (strcmp("game_print", argv[1]) == 0)
        ok = test_game_print();
    else if (strcmp("game_undo", argv[1]) == 0)
        ok = test_game_undo();
    else if (strcmp("game_redo", argv[1]) == 0)
        ok = test_game_redo();
    else if (strcmp("game_load", argv[1]) == 0)
        ok = test_game_load();
    else if (strcmp("game_save", argv[1]) == 0)
        ok = test_game_save();
    else if (strcmp("game_solve", argv[1]) == 0)
        ok = test_game_solve();
    else if (strcmp("game_nb_solutions", argv[1]) == 0)
        ok = test_game_nb_solutions();
    else {
        fprintf(stderr, "Error: test \"%s\" not found!\n", argv[1]);
        exit(EXIT_FAILURE);
    }

    if (ok) {
        fprintf(stderr, "Test \"%s\" finished: SUCCESS\n", argv[1]);
        return EXIT_SUCCESS;
    } else {
        fprintf(stderr, "Test \"%s\" finished: FAILURE\n", argv[1]);
        return EXIT_FAILURE;
    }
}
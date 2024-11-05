#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "game.h"
#include "game_aux.h"
#include "game_ext.h"
#include "game_struct.h"

bool test_dummy() {
    return true;
}

bool test_game_equal() {
    game g1 = game_default();
    bool test1 = game_equal(g1, g1);
    game g2 = game_copy(g1);
    bool test2 = game_equal(g1, g2) && game_equal(g2, g1);

    if (game_get_square(g2, 0, 0) != S_ZERO)
        game_set_square(g2, 0, 0, S_ZERO);
    else
        game_set_square(g2, 0, 0, S_ONE);
    bool test3 = !game_equal(g1, g2);

    game g3 = game_new_empty();
    bool test4 = !game_equal(g1, g3) && !game_equal(g2, g3);

    game g4 = game_copy(g3);
    game_set_square(g3, 0, 0, S_ZERO);
    game_set_square(g4, 0, 0, S_IMMUTABLE_ZERO);
    bool test5 = !game_equal(g3, g4);
    game g5 = game_copy(g3);
    game_set_square(g3, 1, 2, S_ONE);
    game_set_square(g5, 1, 2, S_IMMUTABLE_ONE);
    test5 = test5 && !game_equal(g3, g5);

    game_delete(g1);
    game_delete(g2);
    game_delete(g3);
    game_delete(g4);
    game_delete(g5);
    bool res = (test1 && test2 && test3 && test4 && test5);
    return res;
}

bool test_game_copy() {
    game g1 = game_default();
    game g2 = game_copy(g1);
    bool test = game_equal(g1, g2);
    game g3 = game_copy(g2);
    bool test2 = game_equal(g2, g3);
    game_delete(g1);
    game_delete(g2);
    game_delete(g3);
    game g4 = game_default_solution();
    game g5 = game_copy(g4);
    bool test3 = game_equal(g5, g4);
    game_delete(g4);
    game_delete(g5);
    game g6 = game_new_empty_ext(4, 4, true, false);
    game g7 = game_copy(g6);
    bool test4 = game_equal(g6, g7);
    game_delete(g6);
    game_delete(g7);
    bool res = (test && test2 && test3 && test4);
    return res;
}

bool test_game_is_empty() {
    game g = game_default();
    bool test = game_is_empty(g, 0, 0);  // S_EMPTY
    test = test && game_is_empty(g, 1, 5);
    test = test && game_is_empty(g, 3, 4);
    test = test && !game_is_empty(g, 0, 1);  // S_IMMUTABLE_ONE
    test = test && !game_is_empty(g, 3, 2);
    test = test && !game_is_empty(g, 0, 2);  // S_IMMUTABLE_ZERO
    test = test && !game_is_empty(g, 5, 5);
    game g2 = game_default_solution();  // toutes les cases vides sont
                                        // maintenant à S_ZERO ou S_ONE
    bool test2 = !game_is_empty(g2, 0, 0);
    test2 = test2 && !game_is_empty(g2, 3, 5);
    test2 = test2 && !game_is_empty(g2, 1, 2);
    test2 = test2 && !game_is_empty(g2, 4, 0);
    test2 = test2 && !game_is_empty(g2, 3, 5);
    game_delete(g);
    game_delete(g2);
    return (test && test2);
}

bool test_game_is_immutable() {
    game g = game_default();
    bool test1 = !game_is_immutable(g, 0, 0);  // S_EMPTY
    test1 = (test1 && !game_is_immutable(g, 1, 4));
    test1 = (test1 && !game_is_immutable(g, 5, 0));
    bool test2 = game_is_immutable(g, 0, 2);  // S_IMMUTABLE_ZERO
    test2 = (test2 && game_is_immutable(g, 2, 1));
    test2 = (test2 && game_is_immutable(g, 5, 5));
    bool test3 = game_is_immutable(g, 0, 1);  // S_IMMUTABLE_ONE
    test3 = (test3 && game_is_immutable(g, 3, 2));
    test3 = (test3 && game_is_immutable(g, 4, 2));
    game_set_square(g, 0, 0, S_ZERO);
    bool test4 = !game_is_immutable(g, 0, 0);  // S_ZERO
    game_set_square(g, 1, 0, S_ONE);
    bool test5 = !game_is_immutable(g, 1, 0);  // S_ONE
    game_delete(g);
    return (test1 && test2 && test3 && test4 && test5);
}

/*
 * On commence par tester sur des cases du jeu par défaut, sans le modifier.
 * On s'attend à ne pas avoir d'erreur (puisqu'aucun coup n'a été joué)
 * On fait ensuite la même chose sur la solution du jeu par défaut
 * Enfin, on insère volontairement des erreurs en modifiant la solution,
 * et on teste à nouveau la fonction
 */
bool test_game_has_error() {
    game g = game_new_empty();
    bool test = true;
    // on ne devrait pas avoir d'erreur sur le jeu vide
    if (game_has_error(g, 0, 0) != 0 || game_has_error(g, 2, 4) != 0)
        test = false;

    // on place trois b à la suite dans une ligne
    game_set_square(g, 0, 0, S_ONE);
    game_set_square(g, 0, 1, S_ONE);
    game_set_square(g, 0, 2, S_ONE);
    if (game_has_error(g, 0, 0) == 0 || game_has_error(g, 0, 1) == 0 ||
        game_has_error(g, 0, 2) == 0)
        test = false;

    // on place dans une colonne trop de b dans une colonne (sans qu'il y en ait
    // 3 à la suite)
    game_set_square(g, 1, 0, S_ONE);
    game_set_square(g, 2, 0, S_ZERO);
    game_set_square(g, 3, 0, S_ZERO);
    game_set_square(g, 4, 0, S_ONE);
    game_set_square(g, 5, 0, S_ONE);
    if (game_has_error(g, 1, 0) == 0) test = false;
    if (game_has_error(g, 4, 0) == 0 || game_has_error(g, 5, 0) == 0)
        test = false;

    // test avec l'option unique
    game g2 = game_new_empty_ext(6, 6, true, true);
    game_set_square(g2, 0, 0, S_ONE);
    game_set_square(g2, 1, 0, S_IMMUTABLE_ONE);
    game_set_square(g2, 2, 0, S_ZERO);
    game_set_square(g2, 3, 0, S_ZERO);
    game_set_square(g2, 4, 0, S_ONE);
    game_set_square(g2, 5, 0, S_IMMUTABLE_ZERO);
    game_set_square(g2, 0, 4, S_ONE);
    game_set_square(g2, 1, 4, S_IMMUTABLE_ONE);
    game_set_square(g2, 2, 4, S_ZERO);
    game_set_square(g2, 3, 4, S_ZERO);
    game_set_square(g2, 4, 4, S_ONE);
    if (game_has_error(g2, 0, 0) != 0 || game_has_error(g2, 4, 4) != 0) {
        test = false;
    }
    game_set_square(g2, 5, 4, S_ZERO);
    if (game_has_error(g2, 0, 0) == 0 || game_has_error(g2, 5, 4) == 0) {
        test = false;
    }

    game_set_square(g2, 5, 0, S_EMPTY);
    // test avec l'option wrapping
    game_set_square(g2, 0, 5, S_ONE);

    if (game_has_error(g2, 0, 0) == 0 || game_has_error(g2, 0, 4) == 0 ||
        game_has_error(g2, 0, 5) == 0)
        test = false;

    game_delete(g2);
    game_delete(g);
    return test;
}
/*
 * On commence par tester pour des coordonnées valides puis invalides
 * On regarde pour des valeurs valides puis invalides du square s
 * Enfin, on teste pour des cases contenant un immutable ou non.
 */
bool test_game_check_move() {
    game g = game_new_empty();
    bool testInEmpty = true;
    bool testInZero = true;
    bool testInOne = true;
    bool testInImmutZero = true;
    bool testInImmutOne = true;
    for (int i = 0; i < DEFAULT_SIZE; i++) {
        // on essaie d'abord avec des valeurs de s valides
        testInEmpty = testInEmpty && game_check_move(g, i, 0, S_EMPTY);
        testInZero = testInZero && game_check_move(g, 0, i, S_ZERO);
        testInOne = testInOne && game_check_move(g, i, i, S_ONE);
        // on essaie aussi avec des valeurs invalides de s
        testInImmutZero =
            testInImmutZero && !game_check_move(g, i, 0, S_IMMUTABLE_ZERO);
        testInImmutOne =
            testInImmutOne && !game_check_move(g, i, 0, S_IMMUTABLE_ONE);
    }
    bool testIn = (testInEmpty && testInZero && testInOne && testInImmutZero &&
                   testInImmutOne);

    bool testOut = !game_check_move(g, DEFAULT_SIZE, 0, S_EMPTY);
    testOut = testOut && !game_check_move(g, 0, DEFAULT_SIZE, S_ONE);
    testOut = testOut && !game_check_move(g, DEFAULT_SIZE * 2, 0, S_ZERO);
    testOut = testOut && !game_check_move(g, 0, -1, S_EMPTY);
    testOut = testOut && !game_check_move(g, -2, 1, S_ZERO);

    game g2 = game_default_solution();
    bool testSol = true;
    for (int i = 0; i < DEFAULT_SIZE; i++) {
        for (int j = 0; j < DEFAULT_SIZE; j++) {
            if (game_is_immutable(g2, i, j))
                testSol = testSol && !game_check_move(g2, i, j, S_ONE);
            else
                testSol = testSol && game_check_move(g2, i, j, S_ONE);
        }
    }
    game_delete(g);
    game_delete(g2);
    bool res = (testIn && testOut && testSol);
    return res;
}

bool test_game_delete() {
    game g = game_default();
    game_delete(g);
    return true;
}

// TESTS GAME_EXT.C

bool test_game_new_ext() {
    bool test = true;
    uint row = 4;
    uint col = 6;
    square tsquares[row * col];
    for (uint i = 0; i < row * col; i++) {
        tsquares[i] = S_EMPTY;
    }
    game g = game_new_ext(row, col, tsquares, true, false);
    game_set_square(g, 0, 0, S_IMMUTABLE_ONE);
    game_set_square(g, 1, 2, S_ZERO);
    if (game_nb_rows(g) != row || game_nb_cols(g) != col) test = false;
    if (game_is_wrapping(g) != true || game_is_unique(g) != false) test = false;
    if (game_get_square(g, 0, 0) != S_IMMUTABLE_ONE ||
        game_get_square(g, 1, 2) != S_ZERO)
        test = false;
    if (game_get_square(g, 3, 4) != S_EMPTY ||
        game_get_square(g, 2, 2) != S_EMPTY)
        test = false;
    game_delete(g);
    return test;
}

bool test_game_new_empty_ext() {
    game g = game_new_empty_ext(6, 4, false, true);
    if (game_is_wrapping(g) != false || game_is_unique(g) != true) {
        game_delete(g);
        return false;
    }
    for (int i = 0; i < 6; i++) {
        for (int j = 0; j < 4; j++) {
            if (game_get_square(g, i, j) != S_EMPTY) {
                game_delete(g);
                return false;
            }
        }
    }
    game_delete(g);
    return true;
}

bool test_game_nb_rows() {
    game g = game_new_empty_ext(2, 6, false, false);
    bool test = true;
    if (game_nb_rows(g) != 2) test = false;
    game g2 = game_new_empty_ext(8, 4, true, true);
    if (game_nb_rows(g2) != 8) test = false;
    game_delete(g);
    game_delete(g2);
    return test;
}

bool test_game_nb_cols() {
    game g = game_new_empty_ext(4, 10, false, false);
    bool test = true;
    if (game_nb_cols(g) != 10) test = false;
    game g2 = game_new_empty_ext(6, 8, true, false);
    if (game_nb_cols(g2) != 8) test = false;
    game_delete(g);
    game_delete(g2);
    return test;
}

int main(int argc, char *argv[]) {
    if (argc == 1) {
        fprintf(stderr, "No arguments received\n");
        exit(EXIT_FAILURE);
    }

    fprintf(stderr, "Start of test: %s\n", argv[1]);

    bool ok = false;
    if (strcmp("dummy", argv[1]) == 0)
        ok = test_dummy();
    else if (strcmp("game_equal", argv[1]) == 0)
        ok = test_game_equal();
    else if (strcmp("game_copy", argv[1]) == 0)
        ok = test_game_copy();
    else if (strcmp("game_is_empty", argv[1]) == 0)
        ok = test_game_is_empty();
    else if (strcmp("game_is_immutable", argv[1]) == 0)
        ok = test_game_is_immutable();
    else if (strcmp("game_has_error", argv[1]) == 0)
        ok = test_game_has_error();
    else if (strcmp("game_check_move", argv[1]) == 0)
        ok = test_game_check_move();
    else if (strcmp("game_delete", argv[1]) == 0)
        ok = test_game_delete();
    else if (strcmp("game_new_ext", argv[1]) == 0)
        ok = test_game_new_ext();
    else if (strcmp("game_new_empty_ext", argv[1]) == 0)
        ok = test_game_new_empty_ext();
    else if (strcmp("game_nb_rows", argv[1]) == 0)
        ok = test_game_nb_rows();
    else if (strcmp("game_nb_cols", argv[1]) == 0)
        ok = test_game_nb_cols();
    else {
        fprintf(stderr, "Error! test \"%s\" not found\n", argv[1]);
        exit(EXIT_FAILURE);
    }

    if (ok) {
        fprintf(stderr, "Test \"%s\": SUCCESS\n", argv[1]);
        return EXIT_SUCCESS;
    } else {
        fprintf(stderr, "Test \"%s\": FAILURE\n", argv[1]);
        return EXIT_FAILURE;
    }
}
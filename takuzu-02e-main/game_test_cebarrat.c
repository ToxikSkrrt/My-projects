#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "game.h"
#include "game_aux.h"
#include "game_ext.h"

void usage(int argc, char *argv[]) {
    fprintf(stderr, "Usage: %s <testname> [<...>]\n", argv[0]);
    exit(EXIT_FAILURE);
}

bool delete (game g) {
    game_delete(g);
    return false;
}

bool test_dummy() {
    return true;
}

bool test_game_default() {
    game test = game_default();
    for (int i = 0; i <= 5; i++) {
        for (int j = 0; j <= 5; j++) {
            if (j == 0 || j == 3) {
                if (game_get_square(test, i, j) != S_EMPTY)
                    return delete (test);
            }
            if (j == 1) {
                if (i == 2 || i == 3) {
                    if (game_get_square(test, i, j) != S_IMMUTABLE_ZERO) {
                        return delete (test);
                    }
                } else if (i == 0) {
                    if (game_get_square(test, i, j) != S_IMMUTABLE_ONE) {
                        return delete (test);
                    }
                } else if (game_get_square(test, i, j) != S_EMPTY) {
                    return delete (test);
                }
            }
            if (j == 2) {
                if (i == 3 || i == 4) {
                    if (game_get_square(test, i, j) != S_IMMUTABLE_ONE) {
                        return delete (test);
                    }
                } else if (i == 0) {
                    if (game_get_square(test, i, j) != S_IMMUTABLE_ZERO) {
                        return delete (test);
                    }
                } else if (game_get_square(test, i, j) != S_EMPTY) {
                    return delete (test);
                }
            }
            if (j == 4) {
                if (i == 2) {
                    if (game_get_square(test, i, j) != S_IMMUTABLE_ZERO) {
                        return delete (test);
                    }
                } else if (game_get_square(test, i, j) != S_EMPTY) {
                    return delete (test);
                }
            }
            if (j == 5) {
                if (i == 4 || i == 5) {
                    if (game_get_square(test, i, j) != S_IMMUTABLE_ZERO) {
                        return delete (test);
                    }
                } else if (game_get_square(test, i, j) != S_EMPTY) {
                    return delete (test);
                }
            }
        }
    }
    if (game_is_wrapping(test) || game_is_unique(test)) {
        return delete (test);
    }
    game_delete(test);
    return true;
}

bool test_game_get_square() {
    game test = game_default();
    if (game_get_square(test, 0, 1) != S_IMMUTABLE_ONE) {
        return delete (test);
    }
    if (game_get_square(test, 0, 2) != S_IMMUTABLE_ZERO) {
        return delete (test);
    }
    if (game_get_square(test, 0, 3) != S_EMPTY) {
        return delete (test);
    }
    game_set_square(test, 1, 3, S_ZERO);
    if (game_get_square(test, 1, 3) != S_ZERO) {
        return delete (test);
    }
    game_set_square(test, 1, 4, S_ONE);
    if (game_get_square(test, 1, 4) != S_ONE) {
        return delete (test);
    }
    game_delete(test);
    return true;
}

bool test_game_get_next_square() {
    game test = game_new_empty_ext(4, 6, false, false);
    game_set_square(test, 0, 2, S_IMMUTABLE_ONE);
    game_set_square(test, 1, 4, S_ONE);
    game_set_square(test, 2, 1, S_IMMUTABLE_ZERO);
    game_set_square(test, 2, 2, S_ZERO);

    if (game_get_next_square(test, 0, 0, RIGHT, 2) != S_IMMUTABLE_ONE) {
        return delete (test);
    }
    if (game_get_next_square(test, 1, 4, DOWN, 0) != S_ONE) {
        return delete (test);
    }
    if (game_get_next_square(test, 1, 5, UP, 1) != S_EMPTY) {
        return delete (test);
    }
    if (game_get_next_square(test, 2, 3, LEFT, 2) != S_IMMUTABLE_ZERO) {
        return delete (test);
    }
    if (game_get_next_square(test, 3, 5, DOWN, 1) != -1) {
        return delete (test);
    }
    game_delete(test);
    // test pour wrapping:
    game test2 = game_new_empty_ext(4, 4, true, true);
    game_set_square(test2, 0, 0, S_ONE);
    game_set_square(test2, 1, 0, S_IMMUTABLE_ONE);
    game_set_square(test2, 2, 1, S_ZERO);
    game_set_square(test2, 0, 2, S_IMMUTABLE_ZERO);
    if (game_get_next_square(test2, 0, 1, UP, 2) != S_ZERO) {
        return delete (test2);
    }
    if (game_get_next_square(test2, 2, 0, DOWN, 2) != S_ONE) {
        return delete (test2);
    }
    if (game_get_next_square(test2, 1, 3, RIGHT, 1) != S_IMMUTABLE_ONE) {
        return delete (test2);
    }
    if (game_get_next_square(test2, 0, 0, LEFT, 2) != S_IMMUTABLE_ZERO) {
        return delete (test2);
    }
    game_delete(test2);
    return true;
}

bool test_game_get_number() {
    game test = game_default();
    if (game_get_number(test, 0, 1) != 1) {
        return delete (test);
    }
    if (game_get_number(test, 0, 2) != 0) {
        return delete (test);
    }
    if (game_get_number(test, 0, 3) != -1) {
        return delete (test);
    }

    game_set_square(test, 1, 3, S_ZERO);
    if (game_get_number(test, 1, 3) != 0) {
        return delete (test);
    }
    game_set_square(test, 1, 4, S_ONE);
    if (game_get_number(test, 1, 4) != 1) {
        return delete (test);
    }
    game_delete(test);
    return true;
}

bool test_game_get_next_number() {
    game test = game_new_empty_ext(4, 6, false, false);
    game_set_square(test, 0, 2, S_IMMUTABLE_ONE);
    game_set_square(test, 1, 4, S_ONE);
    game_set_square(test, 2, 1, S_IMMUTABLE_ZERO);
    game_set_square(test, 2, 2, S_ZERO);

    if (game_get_next_number(test, 0, 0, RIGHT, 2) != 1) {
        return delete (test);
    }
    if (game_get_next_number(test, 1, 4, UP, 0) != 1) {
        return delete (test);
    }
    if (game_get_next_number(test, 1, 5, UP, 1) != -1) {
        return delete (test);
    }
    if (game_get_next_number(test, 2, 3, LEFT, 2) != 0) {
        return delete (test);
    }
    if (game_get_next_number(test, 3, 2, UP, 1) != 0) {
        return delete (test);
    }
    if (!game_get_next_number(test, 3, 5, DOWN, 1)) {
        return delete (test);
    }
    game_delete(test);
    // test pour wrapping:
    game test2 = game_new_empty_ext(4, 4, true, true);
    game_set_square(test2, 0, 0, S_ONE);
    game_set_square(test2, 1, 0, S_IMMUTABLE_ONE);
    game_set_square(test2, 2, 1, S_ZERO);
    game_set_square(test2, 0, 2, S_IMMUTABLE_ZERO);
    if (game_get_next_number(test2, 0, 1, UP, 2) != 0) {
        return delete (test2);
    }
    if (game_get_next_number(test2, 2, 0, DOWN, 2) != 1) {
        return delete (test2);
    }
    if (game_get_next_number(test2, 1, 3, RIGHT, 1) != 1) {
        return delete (test2);
    }
    if (game_get_next_number(test2, 0, 0, LEFT, 2) != 0) {
        return delete (test2);
    }
    if (game_get_next_number(test2, 0, 3, UP, 1) != -1) {
        return delete (test2);
    }
    game_delete(test2);
    return true;
}

bool test_game_set_square() {
    game test = game_default();
    game_set_square(test, 0, 0, S_EMPTY);
    if (game_get_square(test, 0, 0) != S_EMPTY) {
        return delete (test);
    }
    game_set_square(test, 0, 0, S_ZERO);
    if (game_get_square(test, 0, 0) != S_ZERO) {
        return delete (test);
    }
    game_set_square(test, 0, 0, S_ONE);
    if (game_get_square(test, 0, 0) != S_ONE) {
        return delete (test);
    }
    game_set_square(test, 0, 0, S_IMMUTABLE_ZERO);
    if (game_get_square(test, 0, 0) != S_IMMUTABLE_ZERO) {
        return delete (test);
    }
    game_set_square(test, 0, 0, S_IMMUTABLE_ONE);
    if (game_get_square(test, 0, 0) != S_IMMUTABLE_ONE) {
        return delete (test);
    }
    game_delete(test);
    return true;
}

bool test_game_is_unique() {
    game test = game_default();
    if (game_is_unique(test)) {
        return delete (test);
    }
    game_delete(test);
    game test2 = game_new_empty_ext(4, 4, false, true);
    if (!game_is_unique(test2)) {
        return delete (test2);
    }
    game_delete(test2);
    return true;
}

bool test_game_is_wrapping() {
    game test = game_default();
    if (game_is_wrapping(test)) {
        return delete (test);
    }
    game_delete(test);
    game test2 = game_new_empty_ext(4, 4, true, false);
    if (!game_is_wrapping(test2)) {
        return delete (test2);
    }
    game_delete(test2);
    return true;
}

int main(int argc, char *argv[]) {
    if (argc == 1) usage(argc, argv);

    bool ok = false;
    if (strcmp("dummy", argv[1]) == 0)
        ok = test_dummy();
    else if (strcmp("game_default", argv[1]) == 0)
        ok = test_game_default();
    else if (strcmp("game_is_wrapping", argv[1]) == 0)
        ok = test_game_is_wrapping();
    else if (strcmp("game_is_unique", argv[1]) == 0)
        ok = test_game_is_unique();
    else if (strcmp("game_get_square", argv[1]) == 0)
        ok = test_game_get_square();
    else if (strcmp("game_get_next_square", argv[1]) == 0)
        ok = test_game_get_next_square();
    else if (strcmp("game_get_number", argv[1]) == 0)
        ok = test_game_get_number();
    else if (strcmp("game_get_next_number", argv[1]) == 0)
        ok = test_game_get_next_number();
    else if (strcmp("game_set_square", argv[1]) == 0)
        ok = test_game_set_square();
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
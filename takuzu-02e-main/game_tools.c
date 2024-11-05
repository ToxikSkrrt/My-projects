#include "game_tools.h"

#include <assert.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>

#include "game.h"
#include "game_ext.h"
#include "game_private.h"

// converts a character to the corresponding square.
// the parameters should be valid ('e', 'b', 'B', 'w', 'W')
square char_to_square(char c) {
    switch (c) {
        case 'e':
            return S_EMPTY;
        case 'w':
            return S_ZERO;
        case 'W':
            return S_IMMUTABLE_ZERO;
        case 'b':
            return S_ONE;
        case 'B':
            return S_IMMUTABLE_ONE;
        default:
            fprintf(stderr, "invalid square character\n");
            exit(EXIT_FAILURE);
    }
}

/* ************************************************************************** */
/*                                LOAD & SAVE                                 */
/* ************************************************************************** */

game game_load(char *filename) {
    FILE *file_stream = fopen(filename, "r");
    assert(file_stream);
    // reads the parameters on the first line of the file.
    uint nb_rows, nb_cols, wrapping, unique;
    int ret = fscanf(file_stream, "%d %d %d %d\n", &nb_rows, &nb_cols,
                     &wrapping, &unique);
    assert((ret != EOF) || (nb_rows < 10) || (nb_cols < 10) ||
           (wrapping <= 1) || (unique <= 1));

    square squares[nb_rows * nb_cols];
    char read_char;
    for (int i = 0; i < nb_rows; i++) {
        for (int j = 0; j < nb_cols; j++) {
            read_char = fgetc(file_stream);
            squares[i * nb_cols + j] = char_to_square(read_char);
        }
        read_char = fgetc(file_stream);
    }
    game g = game_new_ext(nb_rows, nb_cols, squares, wrapping, unique);
    fclose(file_stream);
    return g;
}

void game_save(cgame g, char *filename) {
    FILE *file_save = fopen(filename, "w");
    if (file_save == NULL || g == NULL) {
        exit(EXIT_FAILURE);
    }
    fprintf(file_save, "%u ", game_nb_rows(g));
    fprintf(file_save, "%u ", game_nb_cols(g));
    fprintf(file_save, "%u ", game_is_wrapping(g));
    fprintf(file_save, "%u\n", game_is_unique(g));
    for (uint i = 0; i < game_nb_rows(g); i++) {
        for (uint j = 0; j < game_nb_cols(g); j++) {
            switch (game_get_square(g, i, j)) {
                case S_EMPTY:
                    fprintf(file_save, "e");
                    break;
                case S_ZERO:
                    fprintf(file_save, "w");
                    break;
                case S_ONE:
                    fprintf(file_save, "b");
                    break;
                case S_IMMUTABLE_ZERO:
                    fprintf(file_save, "W");
                    break;
                case S_IMMUTABLE_ONE:
                    fprintf(file_save, "B");
                    break;
                default:
                    break;
            }
        }
        fprintf(file_save, "\n");
    }
    fclose(file_save);
}

/* ************************************************************************** */
/*                                SOLVE                                       */
/* ************************************************************************** */

// nbsol indique si cette fonction est appelée par game_solve (nbsol=0) ou par
// game_nb_solutions (nbsol=1)
// pos indique la position de la case à modifier
// rows & cols indiquent les nb de lignes et de colonnes. Ils restent constants
void game_solve_aux(int pos, int rows, int cols, game g, unsigned int *count,
                    int nbsol) {
    if (pos == cols * rows) {
        if (game_is_over(g)) {
            (*count)++;
        }
        return;
    }
    uint posi = pos / cols;
    uint posj = pos % cols;
    if (!game_is_immutable(g, posi, posj)) {
        game_set_square(g, posi, posj, S_ZERO);
        if (!game_has_error(g, posi, posj)) {
            game_solve_aux(pos + 1, rows, cols, g, count, nbsol);
        }
        if (nbsol == 0 && (*count) > 0) {
            return;
        }
        game_set_square(g, posi, posj, S_ONE);
        if (!game_has_error(g, posi, posj)) {
            game_solve_aux(pos + 1, rows, cols, g, count, nbsol);
            if (nbsol == 0 && (*count) > 0) {
                return;
            }
            game_set_square(g, posi, posj, S_EMPTY);
        } else {
            game_set_square(g, posi, posj, S_EMPTY);
            return;
        }
    } else {
        game_solve_aux(pos + 1, rows, cols, g, count, nbsol);
    }
};

bool game_solve(game g) {
    assert(g);
    int col = game_nb_cols(g);
    int row = game_nb_rows(g);
    uint count = 0;
    game_solve_aux(0, row, col, g, &count, 0);
    return count;
};

uint game_nb_solutions(cgame g) {
    assert(g);
    game g2 = game_copy(g);
    int col = game_nb_cols(g2);
    int row = game_nb_rows(g2);
    uint count = 0;
    game_solve_aux(0, row, col, g2, &count, 1);
    return count;
};

/* ************************************************************************** */
/*                            Random Game Generator                           */
/* ************************************************************************** */

static game game_set_some_mutable_squares(game g, uint nb_empty) {
    assert(g);
    uint nb_rows = game_nb_rows(g);
    uint nb_cols = game_nb_cols(g);
    uint nb_squares = nb_rows * nb_cols;
    assert(nb_empty <= nb_squares);
    uint nb_empty1 = nb_empty;
    while (nb_empty1 > 0) {
        uint pos = rand() % nb_squares;
        uint row = pos / nb_cols;
        uint col = pos % nb_cols;
        if (game_is_immutable(g, row, col)) {
            square s = game_get_number(g, row, col);
            game_set_square(g, row, col, s + 1);
            nb_empty1--;
        }
    }
    return g;
}

/* ************************************************************************** */

static void game_flip_one_square(game g, int i, int j) {
    square s = game_get_square(g, i, j);
    if (s == S_IMMUTABLE_ONE)
        game_set_square(g, i, j, S_IMMUTABLE_ZERO);
    else
        game_set_square(g, i, j, S_IMMUTABLE_ONE);
}

/* ************************************************************************** */

static bool try_flip(game g, int i1, int j1, int i2, int j2) {
    if (i1 == i2 && j1 == j2) return false;
    assert(game_is_over(g));
    square s11 = game_get_square(g, i1, j1);
    square s22 = game_get_square(g, i2, j2);
    square s12 = game_get_square(g, i1, j2);
    square s21 = game_get_square(g, i2, j1);

    if (s11 == s22 && s12 == s21 && s11 != s12) {
        game_flip_one_square(g, i1, j1);
        game_flip_one_square(g, i2, j2);
        game_flip_one_square(g, i1, j2);
        game_flip_one_square(g, i2, j1);

        if (game_is_over(g)) {
            return true;
        } else {  // undo
            game_flip_one_square(g, i1, j1);
            game_flip_one_square(g, i2, j2);
            game_flip_one_square(g, i1, j2);
            game_flip_one_square(g, i2, j1);
            assert(game_is_over(g));
            return false;
        }
    }
    return false;
}

/* ************************************************************************** */

static void apply_nb_flips(game g, uint nb) {
    uint nb_rows = game_nb_rows(g);
    uint nb_cols = game_nb_cols(g);
    while (nb > 0) {
        uint i1 = rand() % nb_rows;
        uint j1 = rand() % nb_cols;
        uint i2 = rand() % nb_rows;
        uint j2 = rand() % nb_cols;
        if (try_flip(g, i1, j1, i2, j2)) nb--;
    }
}

/* ************************************************************************** */

game game_random(uint nb_rows, uint nb_cols, bool wrapping, uint uniq,
                 bool with_solution) {
    // step 0: create an empty game
    game g = game_new_empty_ext(nb_rows, nb_cols, wrapping, uniq);

    // fill the grid with initial values
    for (uint i = 0; i < nb_rows; i++)
        for (uint j = 0; j < nb_cols; j++)
            game_set_square(g, i, j, (i + j) % 2 + 3);

    // Make it unique if it is square
    if (nb_cols == nb_rows) {
        for (uint i = 0; i < nb_rows / 2; i++) {
            game_flip_one_square(g, 2 * i, 2 * i);
            game_flip_one_square(g, 2 * i, 2 * i + 1);
            game_flip_one_square(g, 2 * i + 1, 2 * i);
            game_flip_one_square(g, 2 * i + 1, 2 * i + 1);
        }
    }
    assert(game_is_over(g));
    // apply some flips
    apply_nb_flips(g, nb_rows * nb_cols * 10);

    assert(game_is_over(g));
    // check

    game_set_some_mutable_squares(g, (nb_rows * nb_cols * 1) / 2);
    if (!with_solution) game_restart(g);
    return g;
}

/* ************************************************************************** */
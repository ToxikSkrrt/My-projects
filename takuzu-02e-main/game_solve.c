#include <assert.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "game.h"
#include "game_aux.h"
#include "game_ext.h"
#include "game_tools.h"

int main(int argc, char* argv[]) {
    if (argc <= 2) {
        exit(EXIT_FAILURE);
    }

    game g = game_load(argv[2]);

    uint nb_solutions = 0;
    if (strcmp(argv[1], "-s") == 0) {
        game_solve(g);
        if (argc == 4) {
            game_save(g, argv[3]);
        } else {
            game_print(g);
        }

    } else if (strcmp(argv[1], "-c") == 0) {
        nb_solutions = game_nb_solutions(g);
        if (argc == 4) {
            FILE* file = fopen(argv[3], "w");
            if (file == NULL) exit(EXIT_FAILURE);
            fprintf(file, "%u\n", nb_solutions);
            fclose(file);
        } else {
            printf("%u\n", nb_solutions);
        }
    }

    return EXIT_SUCCESS;
}
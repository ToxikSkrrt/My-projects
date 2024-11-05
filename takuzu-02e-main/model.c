#include "model.h"

#include <SDL2/SDL.h>
#include <SDL2/SDL_image.h>  // required to load transparent texture from PNG
#include <SDL2/SDL_ttf.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>

#include "game.h"
#include "game_aux.h"
#include "game_ext.h"
#include "game_tools.h"
/* **************************************************************** */

#define FONT "arial.ttf"
#define FONTSIZE 26
#define BACKGROUND "background.jpg"

/* **************************************************************** */

struct Env_t {
    SDL_Texture *background;
    game g;
    SDL_Texture *textVictory;  // message de victoire
    SDL_Rect *cells;           // tableau de cases du jeu
    SDL_Texture *orange;       // Orange = ZERO
    SDL_Texture *sliceOrange;
    SDL_Texture *watermelon;  // Pasteque = ONE
    SDL_Texture *sliceWM;
    int nbItemMenu;
    SDL_Rect *cellsMenu;
    SDL_Texture *restart;
    SDL_Texture *help;
    SDL_Texture *undo;
    SDL_Texture *redo;
    SDL_Texture *solve;
    SDL_Texture *save;
};

/* ***********************FONCTIONS ANNEXES************************ */

int createAskingMessage(char *title, char *message) {
    int buttonId = -1;
    SDL_MessageBoxButtonData buttons[] = {
        {SDL_MESSAGEBOX_BUTTON_RETURNKEY_DEFAULT, 1, "Yes"},
        {SDL_MESSAGEBOX_BUTTON_ESCAPEKEY_DEFAULT, 2, "No"}};
    SDL_MessageBoxData msgBoxData = {
        SDL_MESSAGEBOX_INFORMATION,  // flags
        NULL,                        // window
        title,
        message,
        2,        // number of buttons (always two here)
        buttons,  // array of buttons
        NULL,     // colorScheme
    };
    SDL_ShowMessageBox(&msgBoxData, &buttonId);
    return buttonId;  // return 1 if 'Yes' and 2 if 'No'
}

/* **************************************************************** */

Env *init(SDL_Window *win, SDL_Renderer *ren, int argc, char *argv[]) {
    Env *env = malloc(sizeof(struct Env_t));

    /* init the game*/
    if (argc > 1) {
        env->g = game_load(argv[1]);
    } else {
        env->g = game_default();
    }

    env->nbItemMenu = 6;

    /* init cell array */
    uint nbRows = game_nb_rows(env->g);
    uint nbCols = game_nb_cols(env->g);
    env->cells = malloc(nbRows * nbCols * sizeof(SDL_Rect));
    env->cellsMenu = malloc(env->nbItemMenu * sizeof(SDL_Rect));
    /* get current window size */
    int w, h;
    SDL_GetWindowSize(win, &w, &h);

    /* init background texture from PNG image */
    env->background = IMG_LoadTexture(ren, BACKGROUND);
    if (!env->background) ERROR("IMG_LoadTexture: %s\n", BACKGROUND);

    /* init watermelon texture from PNG image */
    env->watermelon = IMG_LoadTexture(ren, "watermelon.png");
    if (!env->watermelon) ERROR("IMG_LoadTexture: %s\n", "watermelon.png");

    /* init sliceWM texture from PNG image */
    env->sliceWM = IMG_LoadTexture(ren, "sliceWM.png");
    if (!env->sliceWM) ERROR("IMG_LoadTexture: %s\n", "sliceWM.png");

    /* init orange et sliceOrange textures from PNG */
    env->orange = IMG_LoadTexture(ren, "orange.png");
    if (!env->sliceWM) ERROR("IMG_LoadTexture: %s\n", "orange.png");

    env->sliceOrange = IMG_LoadTexture(ren, "sliceOrange.png");
    if (!env->sliceWM) ERROR("IMG_LoadTexture: %s\n", "sliceOrange.png");

    /*ajout texture victoire*/
    SDL_Color yellow = {18, 87, 24, 255};  // vert foncé
    TTF_Font *font50 = TTF_OpenFont(FONT, 50);
    if (!font50) ERROR("TTF_OpenFont: %s\n", FONT);
    TTF_SetFontStyle(font50,
                     TTF_STYLE_BOLD);  // TTF_STYLE_ITALIC | TTF_STYLE_NORMAL
    SDL_Surface *surfVict = TTF_RenderText_Blended(
        font50, "Victory !!",
        yellow);  // blended rendering for ultra nice text
    env->textVictory = SDL_CreateTextureFromSurface(ren, surfVict);

    /* init restart texture from PNG image */
    env->restart = IMG_LoadTexture(ren, "restart.png");
    if (!env->restart) ERROR("IMG_LoadTexture: %s\n", "restart.png");

    /* init help texture from PNG image */
    env->help = IMG_LoadTexture(ren, "help.png");
    if (!env->help) ERROR("IMG_LoadTexture: %s\n", "help.png");

    /* init undo texture from PNG image */
    env->undo = IMG_LoadTexture(ren, "undo.png");
    if (!env->undo) ERROR("IMG_LoadTexture: %s\n", "undo.png");

    /* init redo texture from PNG image */
    env->redo = IMG_LoadTexture(ren, "redo.png");
    if (!env->redo) ERROR("IMG_LoadTexture: %s\n", "redo.png");

    /* init solve texture from PNG image */
    env->solve = IMG_LoadTexture(ren, "solve.png");
    if (!env->solve) ERROR("IMG_LoadTexture: %s\n", "solve.png");

    /* init save texture from PNG image */
    env->save = IMG_LoadTexture(ren, "save.png");
    if (!env->save) ERROR("IMG_LoadTexture: %s\n", "save.png");

    SDL_FreeSurface(surfVict);
    TTF_CloseFont(font50);

    return env;
}

/* **************************************************************** */

void render(SDL_Window *win, SDL_Renderer *ren,
            Env *env) { /* PUT YOUR CODE HERE TO RENDER TEXTURES, ... */
    SDL_Rect rect;

    /* get current window size */
    int w, h;
    SDL_GetWindowSize(win, &w, &h);

    /* render background texture */
    SDL_RenderCopy(ren, env->background, NULL, NULL); /* stretch it */

    /*calcul marge*/
    int coefMarge = 20;
    int margeH = h / coefMarge;
    int margeW = w / coefMarge;

    /*calcul taille cellule*/
    int sizeCell;
    uint nbRows = game_nb_rows(env->g);
    uint nbCols = game_nb_cols(env->g);

    int sizeMaxH = (h - 2 * margeH) / (nbCols + 1);  // +1 pour le menu
    int sizeMaxW = (w - 2 * margeW) / nbRows;
    if (sizeMaxH > sizeMaxW) {
        sizeCell = sizeMaxW;
    } else {
        sizeCell = sizeMaxH;
    }

    /*calcul size case du menu*/
    int sizeCellMenu;
    if (nbCols >= env->nbItemMenu) {
        sizeCellMenu = sizeCell;
    } else {
        sizeCellMenu = (sizeCell * nbCols) / env->nbItemMenu;
    }

    /*calcul du décalage horizontal pour centrer */
    int decalW = ((w - 2 * margeW - nbCols * sizeCell) / 2);

    /*calcul du décalage vertical pour centrer */
    int decalH = ((h - 2 * margeH - nbRows * sizeCell - sizeCellMenu) / 2);

    /* coloriage des cases en fonction du game */
    for (int i = 0; i < nbRows; i++) {
        for (int j = 0; j < nbCols; j++) {
            int pos = i * (nbCols - 1) + j;
            env->cells[pos].x = margeW + j * sizeCell + decalW;
            env->cells[pos].y = margeH + i * sizeCell + decalH;
            env->cells[pos].w = sizeCell;
            env->cells[pos].h = sizeCell;
            /* colorie les cases comportant une erreur */
            if (game_has_error(env->g, i, j)) {
                SDL_SetRenderDrawColor(ren, 227, 143, 152, 255);  // rose
                SDL_RenderFillRect(ren, &env->cells[pos]);
            } else {
                SDL_SetRenderDrawColor(ren, 249, 216, 173, 255);  // orange
                SDL_RenderFillRect(ren, &env->cells[pos]);
            }
            square s = game_get_square(env->g, i, j);
            switch (s) {
                case S_ONE:
                    SDL_RenderCopy(ren, env->sliceWM, NULL, &env->cells[pos]);
                    break;
                case S_ZERO:
                    SDL_RenderCopy(ren, env->sliceOrange, NULL,
                                   &env->cells[pos]);
                    break;
                case S_IMMUTABLE_ONE:
                    SDL_RenderCopy(ren, env->watermelon, NULL,
                                   &env->cells[pos]);
                    break;
                case S_IMMUTABLE_ZERO:
                    SDL_RenderCopy(ren, env->orange, NULL, &env->cells[pos]);
                    break;
                default:
                    break;
            }
        }
    }

    SDL_SetRenderDrawColor(ren, 0, 0, 0, 255); /*noir*/

    /*ligne horizontales*/
    for (int i = 0; i <= nbRows; i++) {
        SDL_RenderDrawLine(ren, decalW + margeW, margeH + i * sizeCell + decalH,
                           decalW + margeW + nbCols * sizeCell,
                           margeH + i * sizeCell + decalH);
    }
    /*ligne verticales*/
    for (int j = 0; j <= nbCols; j++) {
        SDL_RenderDrawLine(ren, decalW + margeW + j * sizeCell, margeH + decalH,
                           decalW + margeW + j * sizeCell,
                           margeH + nbRows * sizeCell + decalH);
    }

    /*calcul du décalage horizontal pour centrer le menu */
    int decalMenu = ((w - 2 * margeW - env->nbItemMenu * sizeCellMenu) / 2);

    /* barre du menu*/
    for (int i = 0; i < env->nbItemMenu; i++) {
        env->cellsMenu[i].x = margeW + i * sizeCellMenu + decalMenu;
        env->cellsMenu[i].y = margeH + nbRows * sizeCell + decalH;
        env->cellsMenu[i].w = sizeCellMenu;
        env->cellsMenu[i].h = sizeCellMenu;
        /*icones selon la case*/
        switch (i) {
            case 0:
                SDL_RenderCopy(ren, env->restart, NULL, &env->cellsMenu[i]);
                break;
            case 1:
                SDL_RenderCopy(ren, env->help, NULL, &env->cellsMenu[i]);
                break;
            case 2:
                SDL_RenderCopy(ren, env->undo, NULL, &env->cellsMenu[i]);
                break;
            case 3:
                SDL_RenderCopy(ren, env->redo, NULL, &env->cellsMenu[i]);
                break;
            case 4:
                SDL_RenderCopy(ren, env->solve, NULL, &env->cellsMenu[i]);
                break;
            case 5:
                SDL_RenderCopy(ren, env->save, NULL, &env->cellsMenu[i]);
                break;
        }
    }

    /* affichage texte victoire */
    if (game_is_over(env->g)) {
        SDL_QueryTexture(env->textVictory, NULL, NULL, &rect.w, &rect.h);
        rect.x = w / 2 - rect.w / 2;
        rect.y = h / 2 - rect.h / 2;
        SDL_RenderCopy(ren, env->textVictory, NULL, &rect);
    }
}

/* **************************************************************** */

bool process(SDL_Window *win, SDL_Renderer *ren, Env *env, SDL_Event *e) {
    int w, h;
    SDL_GetWindowSize(win, &w, &h);

    if (e->type == SDL_QUIT) {
        if (createAskingMessage("WARNING : Quit",
                                "Do you want to quit the game ?") == 1)
            return true;
    }

    else if (e->type == SDL_MOUSEBUTTONDOWN) {
        SDL_Point mouse, startOfGrid, endOfGrid;
        SDL_GetMouseState(&mouse.x, &mouse.y);
        /* récupère les données du plateau de jeu*/
        startOfGrid.x = env->cells[0].x;
        startOfGrid.y = env->cells[0].y;
        int cellSide = env->cells[0].w;
        endOfGrid.x = startOfGrid.x + game_nb_cols(env->g) * cellSide;
        endOfGrid.y = startOfGrid.y + game_nb_rows(env->g) * cellSide;
        /* vérifie si le clic se trouve dans le plateau */
        if (startOfGrid.x < mouse.x && mouse.x < endOfGrid.x) {
            if (startOfGrid.y < mouse.y && mouse.y < endOfGrid.y) {
                int clickedCellx = (mouse.x - startOfGrid.x) / cellSide;
                int clickedCelly = (mouse.y - startOfGrid.y) / cellSide;
                int i = clickedCelly;  // les coordonnées du game sont inversées
                int j = clickedCellx;
                if (!game_is_immutable(env->g, i, j)) {
                    int cellNb = game_get_number(env->g, i, j);
                    if (cellNb == 0) {
                        game_play_move(env->g, i, j, S_ONE);
                    } else if (cellNb == 1) {
                        game_play_move(env->g, i, j, S_EMPTY);
                    } else {
                        game_play_move(env->g, i, j, S_ZERO);
                    }
                }
            }
        }
        /* récupère les données du menu*/
        SDL_Point startOfMenu, endOfMenu;
        startOfMenu.x = env->cellsMenu[0].x;
        startOfMenu.y = env->cellsMenu[0].y;
        int cellSideMenu = env->cellsMenu[0].w;
        endOfMenu.x = startOfMenu.x + env->nbItemMenu * cellSideMenu;
        endOfMenu.y = startOfMenu.y + env->nbItemMenu * cellSideMenu;

        if (startOfMenu.x < mouse.x && mouse.x < endOfMenu.x) {
            if (startOfMenu.y < mouse.y && mouse.y < endOfMenu.y) {
                int clickedCellx = (mouse.x - startOfMenu.x) / cellSide;
                int i = clickedCellx;
                switch (i) {
                    case 0:
                        if (createAskingMessage(
                                "WARNING : Restart",
                                "Do you want to restart the game ?") == 1)
                            game_restart(env->g);
                        break;
                    case 1:
                        SDL_ShowSimpleMessageBox(
                            SDL_MESSAGEBOX_INFORMATION, "HELP",
                            "- click on a case to swap it between watermelon, "
                            "orange or nothing\n"
                            "- press 'z' to undo a move\n"
                            "- press 'y' to redo a move\n"
                            "- press 'r' to restart the game\n"
                            "- press 's' to solve the game\n"
                            "- press 'ESCAPE' or the cross in the top-right "
                            "corner to quit the game",
                            win);
                        break;
                    case 2:
                        game_undo(env->g);
                        break;
                    case 3:
                        game_redo(env->g);
                        break;
                    case 4:
                        game_solve(env->g);
                        break;
                    case 5:
                        game_save(env->g, "savedgame.txt");
                        break;
                }
            }
        }

    }

    else if (e->type == SDL_KEYDOWN) {
        switch (e->key.keysym.sym) {
            case SDLK_h:
                SDL_ShowSimpleMessageBox(
                    SDL_MESSAGEBOX_INFORMATION, "HELP",
                    "- click on a case to swap it between watermelon, "
                    "orange or nothing\n"
                    "- press 'z' to undo a move\n"
                    "- press 'y' to redo a move\n"
                    "- press 'r' to restart the game\n"
                    "- press 's' to solve the game\n"
                    "- press 'ESCAPE' or the cross in the top-right "
                    "corner to quit the game",
                    win);
                break;
            case SDLK_r:
                if (createAskingMessage("WARNING : Restart",
                                        "Do you want to restart the game ?") ==
                    1)
                    game_restart(env->g);
                break;
            case SDLK_z:
                game_undo(env->g);
                break;
            case SDLK_y:
                game_redo(env->g);
                break;
            case SDLK_s:
                game_solve(env->g);
                break;
            case SDLK_ESCAPE:
                if (createAskingMessage("WARNING : Quit",
                                        "Do you want to quit the game ?") == 1)
                    return true;
                break;
        }
    }
    return false;
}

/* **************************************************************** */

void clean(SDL_Window *win, SDL_Renderer *ren, Env *env) {
    SDL_DestroyTexture(env->background);
    SDL_DestroyTexture(env->textVictory);
    SDL_DestroyTexture(env->restart);
    SDL_DestroyTexture(env->help);
    SDL_DestroyTexture(env->redo);
    SDL_DestroyTexture(env->undo);
    SDL_DestroyTexture(env->solve);
    SDL_DestroyTexture(env->save);
    SDL_DestroyTexture(env->orange);
    SDL_DestroyTexture(env->sliceOrange);
    SDL_DestroyTexture(env->watermelon);
    SDL_DestroyTexture(env->sliceWM);

    game_delete(env->g);
    free(env->cellsMenu);
    free(env->cells);
    free(env);
}

/* **************************************************************** */
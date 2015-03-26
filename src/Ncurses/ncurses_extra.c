#include "ncurses_extra.h"

int getLines() {
    return LINES;
}

int getCols() {
    return COLS;
}

int idr_getX(WINDOW* w) {
    int x, y;
    getyx(w, y, x);
    return x;
}

int idr_getY(WINDOW* w) {
    int x, y;
    getyx(w, y, x);
    return y;
}

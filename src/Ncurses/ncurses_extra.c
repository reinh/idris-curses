#include "ncurses_extra.h"

int idr_getLines() {
    return LINES;
}

int idr_getCols() {
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

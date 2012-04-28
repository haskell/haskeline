#include "win_console.h"

BOOL haskeline_SetPosition(HANDLE h, COORD* c) {
    return SetConsoleCursorPosition(h,*c);
}

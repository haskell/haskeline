#include "win_console.h"

BOOL SetPosition(HANDLE h, COORD* c) {
    return SetConsoleCursorPosition(h,*c);
}

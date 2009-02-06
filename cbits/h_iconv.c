#include "h_iconv.h"

// Wrapper functions, since iconv_open et al are macros in libiconv.
iconv_t h_iconv_open(const char *tocode, const char *fromcode) {
    return iconv_open(tocode, fromcode);
}

void h_iconv_close(iconv_t cd) {
    iconv_close(cd);
}

size_t h_iconv(iconv_t cd, char **inbuf, size_t *inbytesleft,
                char **outbuf, size_t *outbytesleft) {
    return iconv(cd, inbuf, inbytesleft, outbuf, outbytesleft);
}

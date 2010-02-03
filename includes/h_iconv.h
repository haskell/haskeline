#include <iconv.h>

iconv_t h_iconv_open(const char *tocode, const char *fromcode);

void h_iconv_close(iconv_t cd);

size_t h_iconv(iconv_t cd, const char **inbuf, size_t *inbytesleft,
                char **outbuf, size_t *outbytesleft);


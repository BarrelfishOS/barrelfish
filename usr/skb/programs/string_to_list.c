#include "eclipse.h"

int p_string_to_list(void);
int p_string_to_list(void)         /* string_to_list(+String, -List) */
{
    pword  list;
    char *s;
    long len;
    int res;

    res = ec_get_string_length(ec_arg(1), &s, &len);
    if (res != PSUCCEED) return res;

    list = ec_nil();    /* build the list backwards */
    while (len--)
        list = ec_list(ec_long(s[len]), list);

    return ec_unify(ec_arg(2), list);
}

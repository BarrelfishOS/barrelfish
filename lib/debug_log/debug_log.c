#include <barrelfish/debug.h>
#include <stdio.h>
#include <stdarg.h>
#include <barrelfish/threads.h>
#include <debug_log/debug_log.h>
#include <barrelfish/barrelfish.h>
#include <barrelfish/systime.h>

#define LOG_SIZE 2048
static char log[LOG_SIZE][256];
static unsigned long int timestamp[LOG_SIZE];
static unsigned int log_size = 0;

void debug_printf_to_log(const char *fmt, ...)
{
    timestamp[log_size & (LOG_SIZE - 1)] = systime_now();
    struct thread *me = thread_self();
    va_list argptr;
    char id[32] = "-";
    char *str = log[log_size++ & (LOG_SIZE - 1)];
    size_t len;
    
    if (me)
        snprintf(id, sizeof(id), "%"PRIuPTR, thread_get_id(me));
    len = snprintf(str, sizeof(log[0]), "\033[34m%.*s.\033[31m%u.%s\033[0m: ",
                   DISP_NAME_LEN, disp_name(), disp_get_core_id(), id);
    if (len < sizeof(log[0])) {
        va_start(argptr, fmt);
        len += vsnprintf(str + len, sizeof(log[0]) - len, fmt, argptr);
        va_end(argptr);
    }
    if (str[len - 1] == '\n')
        str[len - 1] = 0;
}

// #define DEBUG_SHOW

void debug_print_to_log(const char *format, ...)
{
#ifdef DEBUG_SHOW
    struct thread *me = thread_self();
    va_list argptr;
    char id[32] = "-";
    char str[256];
    size_t len;
    
    if (me)
        snprintf(id, sizeof(id), "%"PRIuPTR, thread_get_id(me));
    len = snprintf(str, sizeof(str), "\033[34m%.*s.\033[31m%u.%s\033[0m: ",
                   DISP_NAME_LEN, disp_name(), disp_get_core_id(), id);
    if (len < sizeof(str)) {
        va_start(argptr, format);
        len += vsnprintf(str + len, sizeof(log[0]) - len, format, argptr);
        va_end(argptr);
    }
    str[len++] = '\n';
    sys_print(str, len);
#else
    timestamp[log_size & (LOG_SIZE - 1)] = systime_now();
    char *str = log[log_size++ & (LOG_SIZE - 1)];
    va_list argptr;
    char hex[16] = "0123456789ABCDEF";
    char buff[32];
    int i, j, k;
    
    va_start(argptr, format);
    j = 0;
    for (i = 0; format[i]; i++) {
        if (format[i] != '%') {
            str[j++] = format[i];
        } else if (format[i + 1] == 'x') {
            uint64_t ts = va_arg(argptr, uint64_t);
            for (k = 15; k >= 0; k--) {
                buff[k] = hex[(ts & 15)];
                ts >>= 4;
                if (!ts)
                    break;
            }
            for(; k < 16; k++)
                str[j++] = buff[k];
            i++;
        } else if (format[i + 1] == 'd') {
            uint64_t ts = va_arg(argptr, uint64_t);
            for (k = 31; k >= 0; k--) {
                buff[k] = '0' + ts % 10;
                ts /= 10;
                if (!ts)
                    break;
            }
            for(; k < 32; k++)
                str[j++] = buff[k];
            i++;
        }
    }
    va_end(argptr);
    str[j] = 0;
#endif
}

void debug_print_log(void)
{
    unsigned int i;
    unsigned long lastts = 0;
    int j;

    if (log_size < LOG_SIZE) {
        i = 0;
        j = log_size;
    } else {
        i = log_size & (LOG_SIZE - 1);
        j = LOG_SIZE;
    }
    
    char line[256];
    int s = snprintf(line, sizeof(line), "\nLog:%d %d:%d\n", log_size, i, j);
    sys_print(line, s);
    
    for (; j > 0; j--) {
        s = snprintf(line, sizeof(line), "\033[36;1m(%ld,%ld)\033[0m:\t%s\n",
                   systime_to_ns(timestamp[i]), systime_to_ns(timestamp[i] - lastts), log[i]);
        sys_print(line, s);
        lastts = timestamp[i];
        i = (i + 1) & (LOG_SIZE - 1);
    }
}

unsigned int debug_print_log_to_buffer(char *buffer, int max_size)
{
    unsigned int i, size;
    unsigned long lastts = 0;
    int j;

    if (log_size < LOG_SIZE) {
        i = 0;
        j = log_size;
    } else {
        i = log_size & (LOG_SIZE - 1);
        j = LOG_SIZE;
    }

    size = 0;
    int s = snprintf(buffer + size, max_size - size, "\nLog:%d %d:%d\n", log_size, i, j);
    size += s;

    for (; j > 0; j--) {
        s = snprintf(buffer + size, max_size - size, "\033[36;1m(%ld,%ld)\033[0m:\t%s\n",
                   systime_to_ns(timestamp[i]), systime_to_ns(timestamp[i] - lastts), log[i]);
        size += s;
        lastts = timestamp[i];
        i = (i + 1) & (LOG_SIZE - 1);
    }
    return size;
}

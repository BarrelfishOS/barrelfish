#ifndef DEBUG_LOG_H
#define DEBUG_LOG_H

void debug_printf_to_log(const char *fmt, ...);
void debug_print_to_log(const char *fmt, ...);
void debug_print_log(void);
unsigned int debug_print_log_to_buffer(char *buffer, int max_size);

#endif

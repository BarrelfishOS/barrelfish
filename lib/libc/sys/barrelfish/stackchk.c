#include <assert.h>

void __stack_chk_fail (void)
{
    assert(!"FATAL: finally reached __stack_chk_fail()\n");
}

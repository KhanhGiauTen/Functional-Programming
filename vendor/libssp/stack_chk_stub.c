#include <stdlib.h>
#include <stdint.h>

// Provide a minimal guard that avoids zero-initialised memory being treated as "safe".
uintptr_t __stack_chk_guard = 0x595e9fbd94fda766ULL;

__attribute__((noreturn))
void __stack_chk_fail(void) {
    abort();
}

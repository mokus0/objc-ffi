#import "HSException.h"
#include <ffi.h>

// Returns a retained HSException
HSException *newHSException(const char *reason, void * exc) {
    return [[HSException alloc] initWithReason: [NSString stringWithUTF8String: reason]
                                     exception: exc ];
}

EXC_WRAPPER(ffi_call_with_exceptions, ffi_cif *cif, void (*impl)(), void *ret, void **args) {
    WRAP_EXC(ffi_call(cif, impl, ret, args));
}
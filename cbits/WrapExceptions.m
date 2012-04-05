#import "HSException.h"
#include <ffi.h>

// Returns a retained HSException
HSException *newHSException(const char *reason, void * exc) {
    return [[HSException alloc] initWithReason: [NSString stringWithCString: reason]
                                     exception: exc ];
}

int ffi_call_with_exceptions(ffi_cif *cif, void (*impl)(), void *ret, void **args, void **excOut) {
    @try {
        ffi_call(cif, impl, ret, args);
    } @catch (HSException *exc) {
        *excOut = [exc hsException];
        return 1;
    } @catch (id exc) {
        *excOut = [exc retain];
        return 2;
    }
    
    return 0;
}
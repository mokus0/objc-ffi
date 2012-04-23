#import <Foundation/Foundation.h>
#import <HsFFI.h>

@interface HSException : NSException {
    HsStablePtr hs_exc;
}

- (HSException *)   initWithReason: (NSString *)  reason
                         exception: (HsStablePtr) exc;

- (HsStablePtr)     hsException;

@end

#define EXC_WRAPPER(name, args ...)     \
    int name(args , void **excOut)
#define WRAP_EXC(action)                \
    @try {                              \
        action;                         \
    } @catch (HSException *exc) {       \
        *excOut = [exc hsException];    \
        return 1;                       \
    } @catch (id exc) {                 \
        *excOut = [exc retain];         \
        return 2;                       \
    }                                   \
                                        \
    return 0;

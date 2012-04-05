#import "HSException.h"

static NSString *kHSExceptionString = @"HSException";

@implementation HSException

- (HSException *)   initWithReason: (NSString *)  reason
                         exception: (HsStablePtr) exc
{
    self = [super initWithName: kHSExceptionString
                        reason: reason
                      userInfo: nil];
    
    if (self) {
        hs_exc = exc;
    }
    
    return self;
}

- (HsStablePtr) hsException {
    return hs_exc;
}

- (void) dealloc {
    hs_free_stable_ptr(hs_exc);
    
    [super dealloc];
}

@end

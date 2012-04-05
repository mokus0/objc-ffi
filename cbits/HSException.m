#import "HSException.h"
#include "Foreign/ObjC/Exception_stub.h"

static NSString *kHSExceptionString = @"HSException";

@implementation HSException

- (HSException *)   initWithReason: (NSString *) reason
                         exception: (void     *) exc
{
    self = [super initWithName: kHSExceptionString
                        reason: reason
                      userInfo: nil];
    
    if (self) {
        hs_exc = exc;
    }
    
    return self;
}

- (void *) hsException {
    return hs_exc;
}

- (void) dealloc {
    freeStablePtr(hs_exc);
    
    [super dealloc];
}

@end

#import <Foundation/Foundation.h>
#import <HsFFI.h>

@interface HSException : NSException {
    HsStablePtr hs_exc;
}

- (HSException *)   initWithReason: (NSString *)  reason
                         exception: (HsStablePtr) exc;

- (HsStablePtr)     hsException;

@end

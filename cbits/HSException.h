#import <Foundation/Foundation.h>

@interface HSException : NSException {
    void *hs_exc;
}

- (HSException *)   initWithReason: (NSString *) reason
                         exception: (void     *) exc;

- (void *)      hsException;

@end

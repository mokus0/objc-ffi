#import <Foundation/Foundation.h>


id retainObject(id obj) {
    return [obj retain];
}

void releaseObject(id obj) {
    [obj release];
}

id autoreleaseObject(id obj) {
    return [obj autorelease];
}
#import <Foundation/Foundation.h>
#import <HsFFI.h>

// A protocol for objects that are aware of their interactions with
// the Haskell runtime system.
//
// Objects implementing this protocol should be able to:
//
//  1. Initialize their instance data
//  2. Retain themselves when appropriate.  This is the tricky bit.
//     The object needs to be holding a strong reference to its HSO
//     if and only if there are non-haskell references.
//     
//     So retain and release need to know when and how to turn hsSelf
//     into a strong reference (and when and how to free it).
//     
//     The object's "retain" should create a strong reference if
//     the object is already holding a weak reference.
//     
//     The object's "release" should free that strong reference if
//     it is set and the retain count drops to 1.
//
//     Finally, the object's "dealloc" should free the weak reference
//     (even though it will be zeroed automatically) to avoid leaking
//     the weak-reference cell itself.
//
// The recommended way to implement this protocol and these memory
// management procedures is to call 'registerHSObjectClass' in 
// place of objc_registerClassPair.

@protocol HSObject

// __hsInit :: IO (StablePtr [Dynamic])
// 
// Initialize and return any instance data that needs to be exposed
// to the Haskell garbage collector.
// 
// If the instance wishes to support garbage collection of the HSO's
// references, it should also implement the custom retain/release
// procedures described above.
// 
// If the superclass supports the HSObject protocol, then this
// function should call [super __hsInit] and add its own instance
// data to the HSO returned rather than creating a new one.
- (HsStablePtr) __hsInit;

// __hsGetSelf :: IO (StablePtr HSO)
// 
// This function should either return a cached HSO or create a new
// one by calling __hsInit and creating a ForeignPtr to the object
// itself.  The object should be retained when the ForeignPtr is
// created and its finalizer should release the object.
//
// Even if the HSO already existed, the returned StablePtr must
// be freshly allocated because it will be freed by 'importObject'.
- (HsStablePtr) __hsGetSelf;

@end

Protocol *_HSObject_protocol() {
    return @protocol(HSObject);
}

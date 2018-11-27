#
#
#            Nim's Runtime Library
#        (c) Copyright 2017 Nim contributors
#
#    See the file "copying.txt", included in this
#    distribution, for details about the copyright.
#

type
  AllocatorFlag* {.pure.} = enum  ## flags describing the properties of the allocator
    ThreadLocal ## the allocator is thread local only.
    ZerosMem    ## the allocator always zeros the memory on an allocation

  Allocator* = ptr AllocatorObj
  AllocatorObj* {.inheritable.} = object
    alloc*: proc (a: Allocator; size: int; alignment: int = 8): pointer {.nimcall.}
    dealloc*: proc (a: Allocator; p: pointer; size: int) {.nimcall.}
    realloc*: proc (a: Allocator; p: pointer; oldSize, newSize: int): pointer {.nimcall.}
    deallocAll*: proc (a: Allocator) {.nimcall.}
    flags*: set[AllocatorFlag]

var
  localAllocatorStorage {.threadvar.}: AllocatorObj
  localAllocator {.threadvar.}: Allocator
  sharedAllocator: Allocator

proc getLocalAllocator*(): Allocator =

  result = localAllocator
  if result == nil:
    result = localAllocatorStorage.addr

    result.alloc =
      proc (a: Allocator; size: int; alignment: int = 8): pointer {.nimcall.} =
        system.alloc(size)
    result.dealloc =
      proc (a: Allocator; p: pointer; size: int) {.nimcall.} =
        system.dealloc(p)
    result.realloc =
      proc (a: Allocator; p: pointer; oldSize, newSize: int): pointer {.nimcall.} =
        system.realloc(p, newSize)
    result.deallocAll = nil # *: proc (a: Allocator) {.nimcall.}
    result.flags = {AllocatorFlag.ThreadLocal}
    localAllocator = result

proc setLocalAllocator*(a: Allocator) =
  localAllocator = a

proc getSharedAllocator*(): Allocator =
  result = sharedAllocator

proc setSharedAllocator*(a: Allocator) =
  sharedAllocator = a

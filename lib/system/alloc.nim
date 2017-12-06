#
#
#            Nim's Runtime Library
#        (c) Copyright 2012 Andreas Rumpf
#
#    See the file "copying.txt", included in this
#    distribution, for details about the copyright.
#

# Low level allocator for Nim. Has been designed to support the GC.
#
# Design
# ------
# We manage *chunks* of memory. Each chunk is a multiple of the page size.
# Each chunk starts at an address that is divisible by the page size. Chunks
# are served to us by TLSF's ``memalign``. TLSF gets its memory
# by ``osAlloc``.
{.push profiler:off.}

include osalloc, osalloc_wrapper
include tlsf

template track(op, address, size) =
  when defined(memTracker):
    memTrackerOp(op, address, size)

const
  SmallChunkSize = PageSize

type
  PTrunk = ptr Trunk
  Trunk = object
    next: PTrunk         # all nodes are connected with this pointer
    key: int             # start address at bit 0
    bits: array[0..IntsPerTrunk-1, int] # a bit vector

  TrunkBuckets = array[0..255, PTrunk]
  IntSet = object
    data: TrunkBuckets

type
  AlignType = BiggestFloat
  FreeCell = object
    next: ptr FreeCell  # next free cell in chunk (overlaid with refcount)
    zeroField: int       # 0 means cell is not used (overlaid with typ field)
                         # otherwise a PNimType is stored in there

  PChunk = ptr BaseChunk
  PBigChunk = ptr BigChunk
  PSmallChunk = ptr SmallChunk
  BaseChunk {.pure, inheritable.} = object
    size: int            # if < PageSize it is a small chunk

  SmallChunk = object of BaseChunk
    next, prev: PSmallChunk  # chunks of the same size
    freeList: ptr FreeCell
    free: int            # how many bytes remain
    acc: int             # accumulator for small object allocation
    data: AlignType      # start of usable memory

  BigChunk = object of BaseChunk # not necessarily > PageSize!
    when defined(cpu32):
      align: int
    data: AlignType      # start of usable memory

template smallChunkOverhead(): untyped = sizeof(SmallChunk)-sizeof(AlignType)
template bigChunkOverhead(): untyped = sizeof(BigChunk)-sizeof(AlignType)

# ------------- chunk table ---------------------------------------------------
# We use a PtrSet of chunk starts and a table[Page, chunksize] for chunk
# endings of big chunks. This is needed by the merging operation. The only
# remaining operation is best-fit for big chunks. Since there is a size-limit
# for big chunks (because greater than the limit means they are returned back
# to the OS), a fixed size array can be used.

type
  PAvlNode = ptr AvlNode
  AvlNode = object
    link: array[0..1, PAvlNode] # Left (0) and right (1) links
    key, upperBound: int
    level: int

  MemRegion = object
    minLargeObj, maxLargeObj: int
    freeSmallChunks: array[0..SmallChunkSize div MemAlign-1, PSmallChunk]
    chunkStarts: IntSet
    root, deleted, last, freeAvlNodes: PAvlNode
    locked: bool # if locked, we cannot free pages.
    bottomData: AvlNode
    t: Tlsf

{.push stack_trace: off.}
proc initAllocator() = discard "nothing to do anymore"
{.pop.}

proc getBottom(a: var MemRegion): PAvlNode =
  result = addr(a.bottomData)
  if result.link[0] == nil:
    result.link[0] = result
    result.link[1] = result

proc allocAvlNode(a: var MemRegion, key, upperBound: int): PAvlNode =
  if a.freeAvlNodes != nil:
    result = a.freeAvlNodes
    a.freeAvlNodes = a.freeAvlNodes.link[0]
  else:
    result = cast[PAvlNode](llAlloc(a.t.osa, sizeof(result[])))
    when defined(avlcorruption):
      cprintf("tracking location: %p\n", result)
  result.key = key
  result.upperBound = upperBound
  let bottom = getBottom(a)
  result.link[0] = bottom
  result.link[1] = bottom
  result.level = 1
  #when defined(avlcorruption):
  #  track("allocAvlNode", result, sizeof(AvlNode))
  sysAssert(bottom == addr(a.bottomData), "bottom data")
  sysAssert(bottom.link[0] == bottom, "bottom link[0]")
  sysAssert(bottom.link[1] == bottom, "bottom link[1]")

proc deallocAvlNode(a: var MemRegion, n: PAvlNode) {.inline.} =
  n.link[0] = a.freeAvlNodes
  a.freeAvlNodes = n

include "system/avltree"

proc intSetGet(t: IntSet, key: int): PTrunk =
  var it = t.data[key and high(t.data)]
  while it != nil:
    if it.key == key: return it
    it = it.next
  result = nil

proc intSetPut(a: var MemRegion, t: var IntSet, key: int): PTrunk =
  result = intSetGet(t, key)
  if result == nil:
    result = cast[PTrunk](llAlloc(a.t.osa, sizeof(result[])))
    result.next = t.data[key and high(t.data)]
    t.data[key and high(t.data)] = result
    result.key = key

proc contains(s: IntSet, key: int): bool =
  var t = intSetGet(s, key shr TrunkShift)
  if t != nil:
    var u = key and TrunkMask
    result = (t.bits[u shr IntShift] and (1 shl (u and IntMask))) != 0
  else:
    result = false

proc incl(a: var MemRegion, s: var IntSet, key: int) =
  var t = intSetPut(a, s, key shr TrunkShift)
  var u = key and TrunkMask
  t.bits[u shr IntShift] = t.bits[u shr IntShift] or (1 shl (u and IntMask))

proc excl(s: var IntSet, key: int) =
  var t = intSetGet(s, key shr TrunkShift)
  if t != nil:
    var u = key and TrunkMask
    t.bits[u shr IntShift] = t.bits[u shr IntShift] and not
        (1 shl (u and IntMask))

iterator elements(t: IntSet): int {.inline.} =
  # while traversing it is forbidden to change the set!
  for h in 0..high(t.data):
    var r = t.data[h]
    while r != nil:
      var i = 0
      while i <= high(r.bits):
        var w = r.bits[i] # taking a copy of r.bits[i] here is correct, because
        # modifying operations are not allowed during traversation
        var j = 0
        while w != 0:         # test all remaining bits for zero
          if (w and 1) != 0:  # the bit is set!
            yield (r.key shl TrunkShift) or (i shl IntShift +% j)
          inc(j)
          w = w shr 1
        inc(i)
      r = r.next

proc isSmallChunk(c: PChunk): bool {.inline.} =
  return c.size <= SmallChunkSize-smallChunkOverhead()

proc isCell(p: pointer): bool {.inline.} =
  result = cast[ptr FreeCell](p).zeroField >% 1

iterator allObjects(m: var MemRegion): pointer {.inline.} =
  m.locked = true
  for s in elements(m.chunkStarts):
    # we need to check here again as it could have been modified:
    if s in m.chunkStarts:
      let c = cast[PChunk](s shl PageShift)
      if isSmallChunk(c):
        var c = cast[PSmallChunk](c)

        let size = c.size
        var a = cast[ByteAddress](addr(c.data))
        let limit = a + c.acc
        while a <% limit:
          if isCell(cast[pointer](a)):
            yield cast[pointer](a)
          a = a +% size
      else:
        let c = cast[PBigChunk](c)
        let r = addr(c.data)
        if isCell(r):
          yield r
  m.locked = false

proc iterToProc*(iter: typed, envType: typedesc; procName: untyped) {.
                      magic: "Plugin", compileTime.}

# ------------- chunk management ----------------------------------------------
proc pageIndex(c: PChunk): int {.inline.} =
  result = cast[ByteAddress](c) shr PageShift

proc pageIndex(p: pointer): int {.inline.} =
  result = cast[ByteAddress](p) shr PageShift

proc pageAddr(p: pointer): PChunk {.inline.} =
  result = cast[PChunk](cast[ByteAddress](p) and not PageMask)
  #sysAssert(Contains(allocator.chunkStarts, pageIndex(result)))

proc isAccessible(a: MemRegion, p: pointer): bool {.inline.} =
  result = contains(a.chunkStarts, pageIndex(p))

proc contains[T](list, x: T): bool =
  var it = list
  while it != nil:
    if it == x: return true
    it = it.next

proc listAdd[T](head: var T, c: T) {.inline.} =
  sysAssert(c notin head, "listAdd 1")
  sysAssert c.prev == nil, "listAdd 2"
  sysAssert c.next == nil, "listAdd 3"
  c.next = head
  if head != nil:
    sysAssert head.prev == nil, "listAdd 4"
    head.prev = c
  head = c

proc listRemove[T](head: var T, c: T) {.inline.} =
  sysAssert(c in head, "listRemove")
  if c == head:
    head = c.next
    sysAssert c.prev == nil, "listRemove 2"
    if head != nil: head.prev = nil
  else:
    sysAssert c.prev != nil, "listRemove 3"
    c.prev.next = c.next
    if c.next != nil: c.next.prev = c.prev
  c.next = nil
  c.prev = nil

proc freeBigChunk(a: var MemRegion, c: PBigChunk) =
  sysAssert(c.size >= PageSize, "freeBigChunk")
  dealloc(c)
  excl(a.chunkStarts, pageIndex(c))

proc getBigChunk(a: var MemRegion, size: int): PBigChunk =
  let s = size + bigChunkOverhead()
  result = cast[PBigChunk](memalign(a.t, PageSize, s))
  result.size = s
  incl(a, a.chunkStarts, pageIndex(result))

proc getSmallChunk(a: var MemRegion): PSmallChunk =
  result = cast[PSmallChunk](memalign(a.t, PageSize, PageSize))
  incl(a, a.chunkStarts, pageIndex(result))

# -----------------------------------------------------------------------------
proc isAllocatedPtr(a: MemRegion, p: pointer): bool {.benign.}

when false:
  template allocInv(a: MemRegion): bool = true
else:
  proc allocInv(a: MemRegion): bool =
    ## checks some (not all yet) invariants of the allocator's data structures.
    for s in low(a.freeSmallChunks)..high(a.freeSmallChunks):
      var c = a.freeSmallChunks[s]
      while not (c == nil):
        if c.next == c:
          echo "[SYSASSERT] c.next == c"
          return false
        if not (c.size == s * MemAlign):
          echo "[SYSASSERT] c.size != s * MemAlign"
          return false
        var it = c.freeList
        while not (it == nil):
          if not (it.zeroField == 0):
            echo "[SYSASSERT] it.zeroField != 0"
            c_printf("%ld %p\n", it.zeroField, it)
            return false
          it = it.next
        c = c.next
    result = true

proc rawAlloc(a: var MemRegion, requestedSize: int): pointer =
  sysAssert(allocInv(a), "rawAlloc: begin")
  sysAssert(roundup(65, 8) == 72, "rawAlloc: roundup broken")
  sysAssert(requestedSize >= sizeof(FreeCell), "rawAlloc: requested size too small")
  var size = roundup(requestedSize, MemAlign)
  sysAssert(size >= requestedSize, "insufficient allocated size!")
  #c_fprintf(stdout, "alloc; size: %ld; %ld\n", requestedSize, size)
  if size <= SmallChunkSize-smallChunkOverhead():
    # allocate a small block: for small chunks, we use only its next pointer
    var s = size div MemAlign
    var c = a.freeSmallChunks[s]
    if c == nil:
      c = getSmallChunk(a)
      c.freeList = nil
      c.size = size
      c.acc = size
      c.free = SmallChunkSize - smallChunkOverhead() - size
      c.next = nil
      c.prev = nil
      listAdd(a.freeSmallChunks[s], c)
      result = addr(c.data)
      sysAssert((cast[ByteAddress](result) and (MemAlign-1)) == 0, "rawAlloc 4")
    else:
      sysAssert(allocInv(a), "rawAlloc: begin c != nil")
      sysAssert c.next != c, "rawAlloc 5"
      #if c.size != size:
      #  c_fprintf(stdout, "csize: %lld; size %lld\n", c.size, size)
      sysAssert c.size == size, "rawAlloc 6"
      if c.freeList == nil:
        sysAssert(c.acc + smallChunkOverhead() + size <= SmallChunkSize,
                  "rawAlloc 7")
        result = cast[pointer](cast[ByteAddress](addr(c.data)) +% c.acc)
        inc(c.acc, size)
      else:
        result = c.freeList
        sysAssert(c.freeList.zeroField == 0, "rawAlloc 8")
        c.freeList = c.freeList.next
      dec(c.free, size)
      sysAssert((cast[ByteAddress](result) and (MemAlign-1)) == 0, "rawAlloc 9")
      sysAssert(allocInv(a), "rawAlloc: end c != nil")
    sysAssert(allocInv(a), "rawAlloc: before c.free < size")
    if c.free < size:
      sysAssert(allocInv(a), "rawAlloc: before listRemove test")
      listRemove(a.freeSmallChunks[s], c)
      sysAssert(allocInv(a), "rawAlloc: end listRemove test")
    sysAssert(((cast[ByteAddress](result) and PageMask) - smallChunkOverhead()) %%
               size == 0, "rawAlloc 21")
    sysAssert(allocInv(a), "rawAlloc: end small size")
  else:
    # allocate a large block
    var c = getBigChunk(a, size)
    result = addr(c.data)
    sysAssert((cast[ByteAddress](result) and (MemAlign-1)) == 0, "rawAlloc 13")
    if a.root == nil: a.root = getBottom(a)
    add(a, a.root, cast[ByteAddress](result), cast[ByteAddress](result)+%size)
  sysAssert(isAccessible(a, result), "rawAlloc 14")
  sysAssert(allocInv(a), "rawAlloc: end")
  when logAlloc: cprintf("rawAlloc: %ld %p\n", requestedSize, result)

proc rawAlloc0(a: var MemRegion, requestedSize: int): pointer =
  result = rawAlloc(a, requestedSize)
  zeroMem(result, requestedSize)

proc rawDealloc(a: var MemRegion, p: pointer) =
  sysAssert(isAllocatedPtr(a, p), "rawDealloc: no allocated pointer")
  sysAssert(allocInv(a), "rawDealloc: begin")
  var c = pageAddr(p)
  if isSmallChunk(c):
    # `p` is within a small chunk:
    var c = cast[PSmallChunk](c)
    var s = c.size
    sysAssert(((cast[ByteAddress](p) and PageMask) - smallChunkOverhead()) %%
               s == 0, "rawDealloc 3")
    var f = cast[ptr FreeCell](p)
    #echo("setting to nil: ", $cast[ByteAddress](addr(f.zeroField)))
    sysAssert(f.zeroField != 0, "rawDealloc 1")
    f.zeroField = 0
    f.next = c.freeList
    c.freeList = f
    when overwriteFree:
      # set to 0xff to check for usage after free bugs:
      c_memset(cast[pointer](cast[int](p) +% sizeof(FreeCell)), -1'i32,
               s -% sizeof(FreeCell))
    # check if it is not in the freeSmallChunks[s] list:
    if c.free < s:
      # add it to the freeSmallChunks[s] array:
      listAdd(a.freeSmallChunks[s div MemAlign], c)
      inc(c.free, s)
    else:
      inc(c.free, s)
      if c.free == SmallChunkSize-smallChunkOverhead():
        listRemove(a.freeSmallChunks[s div MemAlign], c)
        dealloc(a.t, c)
    sysAssert(((cast[ByteAddress](p) and PageMask) - smallChunkOverhead()) %%
               s == 0, "rawDealloc 2")
  else:
    # in theory, we have to subtract 'bigChunkOverhead' from the pointer, but since it's
    # page aligned we do not have to do this here:
    sysAssert(cast[int](p) -% bigChunkOverhead() == cast[int](c),
        "rawDealloc: pointer arithmetic does not work out")
    # set to 0xff to check for usage after free bugs:
    when overwriteFree: c_memset(p, -1'i32, c.size -% bigChunkOverhead())
    # free big chunk
    var c = cast[PBigChunk](c)
    a.deleted = getBottom(a)
    del(a, a.root, cast[int](addr(c.data)))
    #cprintf("rawDealloc: freeBigChunk of size %ld\n", c.size)
    freeBigChunk(a, c)
  sysAssert(allocInv(a), "rawDealloc: end")
  when logAlloc: cprintf("rawDealloc: %p\n", p)

proc isAllocatedPtr(a: MemRegion, p: pointer): bool =
  if isAccessible(a, p):
    var c = pageAddr(p)
    if isSmallChunk(c):
      var c = cast[PSmallChunk](c)
      var offset = (cast[ByteAddress](p) and (PageSize-1)) -%
                    smallChunkOverhead()
      result = (c.acc >% offset) and (offset %% c.size == 0) and
        (cast[ptr FreeCell](p).zeroField >% 1)
    else:
      var c = cast[PBigChunk](c)
      result = p == addr(c.data) and cast[ptr FreeCell](p).zeroField >% 1

proc prepareForInteriorPointerChecking(a: var MemRegion) {.inline.} =
  if a.root == nil: a.root = getBottom(a)
  a.minLargeObj = lowGauge(a.root)
  a.maxLargeObj = highGauge(a.root)

proc interiorAllocatedPtr(a: MemRegion, p: pointer): pointer =
  if isAccessible(a, p):
    var c = pageAddr(p)
    if isSmallChunk(c):
      var c = cast[PSmallChunk](c)
      var offset = (cast[ByteAddress](p) and (PageSize-1)) -%
                    smallChunkOverhead()
      if c.acc >% offset:
        sysAssert(cast[ByteAddress](addr(c.data)) +% offset ==
                  cast[ByteAddress](p), "offset is not what you think it is")
        var d = cast[ptr FreeCell](cast[ByteAddress](addr(c.data)) +%
                  offset -% (offset %% c.size))
        if d.zeroField >% 1:
          result = d
          sysAssert isAllocatedPtr(a, result), " result wrong pointer!"
    else:
      var c = cast[PBigChunk](c)
      var d = addr(c.data)
      if p >= d and cast[ptr FreeCell](d).zeroField >% 1:
        result = d
        sysAssert isAllocatedPtr(a, result), " result wrong pointer!"
  else:
    var q = cast[int](p)
    if q >=% a.minLargeObj and q <=% a.maxLargeObj:
      # this check is highly effective! Test fails for 99,96% of all checks on
      # an x86-64.
      var avlNode = inRange(a.root, q)
      if avlNode != nil:
        var k = cast[pointer](avlNode.key)
        var c = cast[PBigChunk](pageAddr(k))
        sysAssert(addr(c.data) == k, " k is not the same as addr(c.data)!")
        if cast[ptr FreeCell](k).zeroField >% 1:
          result = k
          sysAssert isAllocatedPtr(a, result), " result wrong pointer!"

proc ptrSize(p: pointer): int =
  var x = cast[pointer](cast[ByteAddress](p) -% sizeof(FreeCell))
  var c = pageAddr(p)
  result = c.size -% sizeof(FreeCell)
  if not isSmallChunk(c):
    dec result, bigChunkOverhead()

proc alloc(allocator: var MemRegion, size: Natural): pointer {.gcsafe.} =
  result = alloc(allocator.t, size)

proc alloc0(allocator: var MemRegion, size: Natural): pointer =
  result = alloc(allocator, size)
  zeroMem(result, size)

proc dealloc(allocator: var MemRegion, p: pointer) =
  dealloc(allocator.t, p)

proc realloc(allocator: var MemRegion, p: pointer, newsize: Natural): pointer =
  if newsize > 0:
    result = alloc0(allocator, newsize)
    if p != nil:
      copyMem(result, p, min(ptrSize(p), newsize))
      dealloc(allocator, p)
  elif p != nil:
    dealloc(allocator, p)

proc getFreeMem(a: MemRegion): int {.inline.} = result = a.t.osa.freeMem
proc getTotalMem(a: MemRegion): int {.inline.} = result = a.t.osa.currMem
proc getOccupiedMem(a: MemRegion): int {.inline.} =
  result = a.t.osa.currMem - a.t.osa.freeMem

proc deallocOsPages(a: var MemRegion) = deallocOsPages(a.t.osa)

# ---------------------- thread memory region -------------------------------

template instantiateForRegion(allocator: untyped) =
  {.push stackTrace: off.}

  when defined(fulldebug):
    proc interiorAllocatedPtr*(p: pointer): pointer =
      result = interiorAllocatedPtr(allocator, p)

    proc isAllocatedPtr*(p: pointer): bool =
      let p = cast[pointer](cast[ByteAddress](p)-%ByteAddress(sizeof(Cell)))
      result = isAllocatedPtr(allocator, p)

  proc deallocOsPages = deallocOsPages(allocator.t.osa)

  proc alloc(size: Natural): pointer =
    result = alloc(allocator, size)

  proc alloc0(size: Natural): pointer =
    result = alloc0(allocator, size)

  proc dealloc(p: pointer) =
    dealloc(allocator, p)

  proc realloc(p: pointer, newsize: Natural): pointer =
    result = realloc(allocator, p, newSize)

  when false:
    proc countFreeMem(): int =
      # only used for assertions
      var it = allocator.freeChunksList
      while it != nil:
        inc(result, it.size)
        it = it.next

  proc getFreeMem(): int =
    result = allocator.t.osa.freeMem
    #sysAssert(result == countFreeMem())

  proc getTotalMem(): int = return allocator.t.osa.currMem
  proc getOccupiedMem(): int = return getTotalMem() - getFreeMem()
  proc getMaxMem*(): int = return getMaxMem(allocator.t.osa)

  # -------------------- shared heap region ----------------------------------
  when hasThreadSupport:
    var sharedHeap: MemRegion
    var heapLock: SysLock
    initSysLock(heapLock)

  proc allocShared(size: Natural): pointer =
    when hasThreadSupport:
      acquireSys(heapLock)
      result = alloc(sharedHeap, size)
      releaseSys(heapLock)
    else:
      result = alloc(size)

  proc allocShared0(size: Natural): pointer =
    result = allocShared(size)
    zeroMem(result, size)

  proc deallocShared(p: pointer) =
    when hasThreadSupport:
      acquireSys(heapLock)
      dealloc(sharedHeap, p)
      releaseSys(heapLock)
    else:
      dealloc(p)

  proc reallocShared(p: pointer, newsize: Natural): pointer =
    when hasThreadSupport:
      acquireSys(heapLock)
      result = realloc(sharedHeap, p, newsize)
      releaseSys(heapLock)
    else:
      result = realloc(p, newSize)

  when hasThreadSupport:

    template sharedMemStatsShared(v: int) {.immediate.} =
      acquireSys(heapLock)
      result = v
      releaseSys(heapLock)

    proc getFreeSharedMem(): int =
      sharedMemStatsShared(sharedHeap.getFreeMem)

    proc getTotalSharedMem(): int =
      sharedMemStatsShared(sharedHeap.getTotalMem)

    proc getOccupiedSharedMem(): int =
      sharedMemStatsShared(sharedHeap.getOccupiedMem)
  {.pop.}

{.pop.}

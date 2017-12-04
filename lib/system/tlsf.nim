#
#
#            Nim's Runtime Library
#        (c) Copyright 2017 Nim Authors
#
#    See the file "copying.txt", included in this
#    distribution, for details about the copyright.
#

# TLSF allocator for Nim. This implementation is heavily inspired by
# Matthew Conte's BSD licensed TLSF
# allocator (`https://github.com/mattconte/tlsf`_).
#
# TLSF achieves O(1) cost for malloc and free operations by limiting
# the search for a free block to a free list of guaranteed size
# adequate to fulfill the request, combined with efficient free list
# queries using bitmasks and architecture-specific bit-manipulation
# routines.
#
# Most modern processors provide instructions to count leading zeroes
# in a word, find the lowest and highest set bit, etc. These
# specific implementations will be used when available, falling back
# to a reasonably efficient generic implementation.
#
# NOTE: TLSF spec relies on ffs/fls returning value 0..31.
# ffs/fls return 1-32 by default, returning 0 for error.

proc flsGeneric*(word: uint32): int {.inline.} =
  ## Fall back to generic implementation.
  result = 32
  var word = word
  if word == 0u32: dec(result, 1)
  if (word and 0xFFFF0000u32) == 0u32:
    word = word shl 16
    dec(result, 16)
  if (word and 0xFF000000u32) == 0u32:
    word = word shl 8
    dec(result, 8)
  if (word and 0xF0000000u32) == 0u32:
    word = word shl 4
    dec(result, 4)
  if (word and 0xC0000000u32) == 0u32:
    word = word shl 2
    dec(result, 2)
  if (word and 0x80000000u32) == 0u32:
    word = word shl 1
    dec(result, 1)

# Implement ffs in terms of fls.
proc ffs*(word: uint32): int {.inline.} =
  flsGeneric(word and (not word + 1u32)) - 1

proc fls*(word: uint32): int {.inline.} =
  flsGeneric(word) - 1

# Possibly 64-bit version of fls.

when sizeof(ByteAddress) == 8:
  proc flsInt*(size: ByteAddress): int {.inline.} =
    var h = size shr 32
    if h != 0:
      result = 32 + fls(uint32 h)
    else:
      result = fls(uint32(size) and 0xFFFFFFFFu32)

else:
  template flsInt(size: ByteAddress): int = fls(uint32(size))

const
  SL_INDEX_COUNT_LOG2 = 5  ## \
    ## log2 of number of linear subdivisions of block sizes. Larger
    ## values require more memory in the control structure. Values of
    ## 4 or 5 are typical. Constant allowed to be modified.

  ALIGN_SIZE_LOG2 = 3 ## All allocation sizes and addresses are aligned to 8 bytes.
  ALIGN_SIZE = (1 shl ALIGN_SIZE_LOG2)
  FL_INDEX_MAX = 32
  SL_INDEX_COUNT = (1 shl SL_INDEX_COUNT_LOG2) ## \
    ## We support allocations of sizes up to (1 shl FL_INDEX_MAX) bits.
    ## However, because we linearly subdivide the second-level lists, and
    ## our minimum size granularity is 4 bytes, it doesn't make sense to
    ## create first-level lists for sizes smaller than SL_INDEX_COUNT * 4,
    ## or (1 shl (SL_INDEX_COUNT_LOG2 + 2)) bytes, as there we will be
    ## trying to split size ranges into more slots than we have available.
    ## Instead, we calculate the minimum threshold size, and place all
    ## blocks below that size into the 0th first-level list.

  FL_INDEX_SHIFT = (SL_INDEX_COUNT_LOG2 + ALIGN_SIZE_LOG2)
  FL_INDEX_COUNT = (FL_INDEX_MAX - FL_INDEX_SHIFT + 1)
  SMALL_BLOCK_SIZE = (1 shl FL_INDEX_SHIFT)


when false:
  static:
    doAssert(ALIGN_SIZE == SMALL_BLOCK_SIZE div SL_INDEX_COUNT)

template mycast(t, ex): untyped = cast[t](ex)

type
  HeaderPtr = ptr Header
  Header = object ## Block header structure.
    prevPhysBlock*: HeaderPtr ## Points to the previous physical block. \
      ## Only valid if the previous block is free.
      ## This field is actually stored at the end of the previous block.
      ## It appears at the beginning of this structure only to
      ## simplify the implementation.
    size*: int  ## The size of this block, excluding the block header.
    nextFree*, prevFree*: HeaderPtr ## Next and previous free blocks. Only \
        ## valid if the block is free.

# Since block sizes are always at least a multiple of 4, the two least
# significant bits of the size field are used to store the block status:
# - bit 0: whether block is busy or free
# - bit 1: whether previous block is busy or free
const
  headerFreeBit: ByteAddress = 1 shl 0
  headerPrevFreeBit: ByteAddress = 1 shl 1

# The size of the block header exposed to used blocks is the size field.
# The prevPhysBlock field is stored *inside* the previous free block.
const
  headerOverhead: ByteAddress = sizeof(int)
  startOffset: ByteAddress = sizeof(HeaderPtr) + sizeof(int)
  ## User data starts directly after the size field in a used block.

# A free block must be large enough to store its header minus the size of
# the prevPhysBlock field, and no larger than the number of addressable
# bits for FL_INDEX.

template sizeMin(): int = sizeof(Header) - sizeof(HeaderPtr)
template sizeMax(): int = 1 shl FL_INDEX_MAX


type
  Tlsf = object ## The memory region control structure.
    null*: Header   ## Empty lists point at this block to indicate they are free.
    flBitmap*: uint32  ## Bitmaps for free lists.
    slBitmap*: array[FL_INDEX_COUNT, uint32] ## Head of free lists.
    blocks*: array[FL_INDEX_COUNT, array[SL_INDEX_COUNT, HeaderPtr]]
    osa: OsAllocs
  Pool = pointer

proc size(blk: HeaderPtr): ByteAddress =
  return blk.size and not (headerFreeBit or headerPrevFreeBit)

proc setSize(blk: HeaderPtr; size: ByteAddress) =
  var oldsize = blk.size
  blk.size = size or (oldsize and (headerFreeBit or headerPrevFreeBit))

proc isLast(blk: HeaderPtr): bool = size(blk) == 0

proc isFree(blk: HeaderPtr): bool =
  (blk.size and headerFreeBit) != 0

proc setFree(blk: HeaderPtr) =
  blk.size = blk.size or headerFreeBit

proc setUsed(blk: HeaderPtr) =
  blk.size = blk.size and not headerFreeBit

proc isPrevFree(blk: HeaderPtr): bool =
  (blk.size and headerPrevFreeBit) != 0

proc setPrevFree(blk: HeaderPtr) =
  blk.size = blk.size or headerPrevFreeBit

proc setPrevUsed(blk: HeaderPtr) =
  blk.size = blk.size and not headerPrevFreeBit

proc fromPtr(z: pointer): HeaderPtr =
  return cast[HeaderPtr](cast[ByteAddress](z) - startOffset)

proc toPtr(blk: HeaderPtr): pointer =
  return cast[pointer](cast[ByteAddress](blk) + startOffset)

proc offsetToBlock(z: pointer; size: ByteAddress): HeaderPtr =
  ## Return location of next block after block of given size.
  return cast[HeaderPtr](cast[ByteAddress](z) + size)

proc prev(blk: HeaderPtr): HeaderPtr =
  ## Return location of previous block.
  sysAssert(isPrevFree(blk), "previous block must be free")
  result = blk.prevPhysBlock

proc next(blk: HeaderPtr): HeaderPtr =
  ## Return location of next existing block.
  result = offsetToBlock(toPtr(blk), size(blk) - headerOverhead)
  sysAssert(not isLast(blk), "next: not isLast(blk)")

proc linkNext(blk: HeaderPtr): HeaderPtr =
  ## Link a new block with its physical neighbor, return the neighbor.
  result = next(blk)
  result.prevPhysBlock = blk

proc markAsFree(blk: HeaderPtr) =
  ## Link the block to the next block, first.
  let next = linkNext(blk)
  setPrevFree(next)
  setFree(blk)

proc markAsUsed(blk: HeaderPtr) =
  let next = next(blk)
  setPrevUsed(next)
  setUsed(blk)

proc alignUp(x: ByteAddress; align: ByteAddress): ByteAddress =
  sysAssert(0 == (align and (align - 1)), "must align to a power of two")
  result = (x + (align - 1)) and not (align - 1)

proc alignDown(x: ByteAddress; align: ByteAddress): ByteAddress =
  sysAssert(0 == (align and (align - 1)), "must align to a power of two")
  result = x - (x and (align - 1))

proc alignPtr(z: pointer; align: ByteAddress): pointer =
  let aligned: int = (cast[ByteAddress](z) + (align - 1)) and not (align - 1)
  sysAssert((align and (align - 1)) == 0, "must align to a power of two")
  result = cast[pointer](aligned)

proc adjustRequestSize(size: ByteAddress; align: ByteAddress): ByteAddress =
  ## Adjust an allocation size to be aligned to word size, and no smaller
  ## than internal minimum.
  result = 0
  sysAssert(size != 0, "adjustRequestedSize is 0!")
  if size < sizeMax:
    let aligned = alignUp(size, align)
    result = max(aligned, sizeMin)

# TLSF utility functions. In most cases, these are direct translations of
# the documentation found in the white paper.

proc mappingInsert(size: ByteAddress): tuple[fli, sli: int] =
  if size < SMALL_BLOCK_SIZE:
    # Store small blocks in first list.
    result = (0, size div (SMALL_BLOCK_SIZE div SL_INDEX_COUNT))
  else:
    var fl = flsInt(size)
    let sl = (size shr (fl - SL_INDEX_COUNT_LOG2)) xor
             (1 shl SL_INDEX_COUNT_LOG2)
    dec(fl, (FL_INDEX_SHIFT - 1))
    result = (fl, sl)

proc mappingSearch(size: ByteAddress): tuple[fli, sli: int] =
  ## This version rounds up to the next block size (for allocations)
  var size = size
  if size >= SMALL_BLOCK_SIZE:
    let round = (1 shl (flsInt(size) - SL_INDEX_COUNT_LOG2)) - 1
    inc(size, round)
  result = mappingInsert(size)

proc insertFreeBlock(r: var Tlsf; blk: HeaderPtr; fl, sl: int) =
  ## Insert a free block into the free block list.
  var current = r.blocks[fl][sl]
  sysAssert(current != nil, "free list cannot have a null entry")
  sysAssert(blk != nil, "cannot insert a null entry into the free list")
  blk.nextFree = current
  blk.prevFree = addr(r.null)
  current.prevFree = blk
  sysAssert(toPtr(blk) == alignPtr(toPtr(blk), ALIGN_SIZE),
      "block not aligned properly")
  # Insert the new block at the head of the list, and mark the first-
  # and second-level bitmaps appropriately.
  r.blocks[fl][sl] = blk
  r.flBitmap = r.flBitmap or (1u32 shl fl)
  r.slBitmap[fl] = r.slBitmap[fl] or (1u32 shl sl)

proc insert(r: var Tlsf; blk: HeaderPtr) =
  ## Insert a given block into the free list.
  var (fl, sl) = mappingInsert(size(blk))
  insertFreeBlock(r, blk, fl, sl)

template poolOverhead(): ByteAddress =
  ## Overhead of the TLSF structures in a given memory block passed to
  ## ``addPool``, equal to the overhead of a free blk and the
  ## sentinel block.
  2 * headerOverhead

proc addPool(r: var Tlsf; mem: Pool; bytes: int) =
  let poolOverhead = poolOverhead()
  let poolBytes: ByteAddress = alignDown(bytes - poolOverhead, ALIGN_SIZE)
  sysAssert(cast[ByteAddress](mem) mod ALIGN_SIZE == 0, "addPool: mem pointer must be aligned to 8 bytes")
  if poolBytes < sizeMin or poolBytes > sizeMax:
    sysAssert(false, "addPool: invalid memory size")
  let blk = offsetToBlock(mem, -headerOverhead)
  setSize(blk, poolBytes)
  setFree(blk)
  setPrevUsed(blk)
  insert(r, blk)
  # Split the block to create a zero-size sentinel block.
  let next = linkNext(blk)
  setSize(next, 0)
  setUsed(next)
  setPrevFree(next)

proc searchSuitableBlock(r: var Tlsf; fli, sli: var int): HeaderPtr =
  var fl = fli
  var sl = sli
  # First, search for a block in the list associated with the given
  # fl/sl index.
  var slMap: uint32 = r.slBitmap[fl] and (not 0u32 shl sl)
  if slMap == 0:
    # No block exists. Search in the next largest first-level list.
    var flMap: uint32 = r.flBitmap and (not 0u32 shl (fl + 1))
    if flMap == 0:
      # No free blocks available, memory has been exhausted.
      let (p, s) = requestOsChunk(r.osa)
      addPool(r, p, s)
      return searchSuitableBlock(r, fli, sli)
    fl = ffs(flMap)
    fli = fl
    slMap = r.slBitmap[fl]
  sysAssert(slMap != 0, "internal error - second level bitmap is null")
  sl = ffs(slMap)
  sli = sl
  # Return the first block in the free list.
  result = r.blocks[fl][sl]

proc removeFreeBlock(r: var Tlsf; blk: HeaderPtr; fl, sl: int) =
  ## Remove a free block from the free list.
  var prev = blk.prevFree
  var next = blk.nextFree
  sysAssert(prev != nil, "prev_free field can not be null")
  sysAssert(next != nil, "next_free field can not be null")
  next.prevFree = prev
  prev.nextFree = next
  # If this block is the head of the free list, set new head.
  if r.blocks[fl][sl] == blk:
    r.blocks[fl][sl] = next
    # If the new head is null, clear the bitmap.
    if next == addr(r.null):
      r.slBitmap[fl] = r.slBitmap[fl] and not (1u32 shl sl)
      # If the second bitmap is now empty, clear the fl bitmap.
      if r.slBitmap[fl] == 0:
        r.flBitmap = r.flBitmap and not (1u32 shl fl)

proc remove(r: var Tlsf; blk: HeaderPtr) =
  ## Remove a given block from the free list.
  var (fl, sl) = mappingInsert(size(blk))
  removeFreeBlock(r, blk, fl, sl)

proc canSplit(blk: HeaderPtr; size: ByteAddress): bool =
  result = size(blk) >= sizeof((Header)) + size

proc split(blk: HeaderPtr; size: ByteAddress): HeaderPtr =
  ## Split a block into two, the second of which is free.
  # Calculate the amount of space left in the remaining block.
  result = offsetToBlock(toPtr(blk), size - headerOverhead)
  var remainSize: ByteAddress = size(blk) - (size + headerOverhead)
  sysAssert(toPtr(result) == alignPtr(toPtr(result), ALIGN_SIZE),
      "remaining block not aligned properly")
  sysAssert(size(blk) == remainSize + size + headerOverhead,
      "remaining block still wrong")
  setSize(result, remainSize)
  sysAssert(size(result) >= sizeMin, "block split with invalid size")
  setSize(blk, size)
  markAsFree(result)

proc absorb(prev: HeaderPtr; blk: HeaderPtr): HeaderPtr =
  ## Absorb a free block's storage into an adjacent previous free block.
  sysAssert(not isLast(prev), "previous block can't be last")
  # Note: Leaves flags untouched.
  inc(prev.size, size(blk) + headerOverhead)
  discard linkNext(prev)
  result = prev

proc mergePrev(r: var Tlsf; blk: HeaderPtr): HeaderPtr =
  ## Merge a just-freed block with an adjacent previous free block.
  result = blk
  if isPrevFree(blk):
    var prev: HeaderPtr = prev(blk)
    sysAssert(prev != nil, "prev physical block can't be null")
    sysAssert(isFree(prev), "prev block is not free though marked as such")
    remove(r, prev)
    result = absorb(prev, blk)

proc mergeNext(r: var Tlsf; blk: HeaderPtr): HeaderPtr =
  ## Merge a just-freed block with an adjacent free block.
  var next: HeaderPtr = next(blk)
  sysAssert(next != nil, "next physical block can't be null")
  result = blk
  if isFree(next):
    sysAssert(not isLast(blk), "previous block can't be last")
    remove(r, next)
    result = absorb(blk, next)

proc trimFree(r: var Tlsf; blk: HeaderPtr; size: ByteAddress) =
  ## Trim any trailing block space off the end of a block, return to pool.
  sysAssert(isFree(blk), "block must be free")
  if canSplit(blk, size):
    var remainingBlock: HeaderPtr = split(blk, size)
    discard linkNext(blk)
    setPrevFree(remainingBlock)
    insert(r, remainingBlock)

proc trimUsed(r: var Tlsf; blk: HeaderPtr; size: ByteAddress) =
  ## Trim any trailing block space off the end of a used block, return to pool.
  sysAssert(not isFree(blk), "block must be used")
  if canSplit(blk, size):
    # If the next block is free, we must coalesce.
    var remainingBlock: HeaderPtr = split(blk, size)
    setPrevUsed(remainingBlock)
    remainingBlock = mergeNext(r, remainingBlock)
    insert(r, remainingBlock)

proc trimFreeLeading(r: var Tlsf; blk: HeaderPtr; size: ByteAddress): HeaderPtr =
  result = blk
  if canSplit(blk, size):
    # We want the 2nd block.
    result = split(blk, size - headerOverhead)
    setPrevFree(result)
    discard linkNext(blk)
    insert(r, blk)

proc locateFree(r: var Tlsf; size: ByteAddress): HeaderPtr =
  sysAssert(size != 0, "locateFree: size must zero")
  var (fl, sl) = mappingSearch(size)
  result = searchSuitableBlock(r, fl, sl)
  if result != nil:
    sysAssert(size(result) >= size, "locateFree: wrong sizes")
    removeFreeBlock(r, result, fl, sl)

proc prepareUsed(r: var Tlsf; blk: HeaderPtr; size: ByteAddress): pointer =
  result = nil
  if blk != nil:
    sysAssert(size != 0, "size must be non-zero")
    trimFree(r, blk, size)
    markAsUsed(blk)
    result = toPtr(blk)

proc setup(r: var Tlsf) =
  ## Clear structure and point all empty lists at the null block.
  setup(r.osa)
  r.null.nextFree = addr(r.null)
  r.null.prevFree = addr(r.null)
  r.flBitmap = 0
  var i = 0
  while i < FL_INDEX_COUNT:
    r.slBitmap[i] = 0
    var j = 0
    while j < SL_INDEX_COUNT:
      r.blocks[i][j] = addr(r.null)
      inc(j)
    inc(i)

# Debugging utilities.

type
  Integrity = object
    prevStatus: int
    status: int

template insist(x, msg: untyped) =
  sysAssert(x, msg)
  if not x:
    dec(status)

proc integrityWalker(p: pointer; size: ByteAddress; used: bool; env: pointer): bool =
  var blk = fromPtr(p)
  var integ = mycast(ptr Integrity, env)
  var thisPrevStatus: int = if isPrevFree(blk): 1 else: 0
  var thisStatus: int = if isFree(blk): 1 else: 0
  var thisBlockSize = size(blk)
  var status = 0
  insist(integ.prevStatus == thisPrevStatus, "prev status incorrect")
  insist(size == thisBlockSize, "block size incorrect")
  integ.prevStatus = thisStatus
  inc(integ.status, status)

proc check(r: var Tlsf): int =
  ## Check that the free lists and bitmaps are accurate.
  var i = 0
  var status = 0
  while i < FL_INDEX_COUNT:
    var j = 0
    while j < SL_INDEX_COUNT:
      var flMap = r.flBitmap and (1u32 shl i)
      var slList = r.slBitmap[i]
      var slMap = slList and (1u32 shl j)
      var blk = r.blocks[i][j]
      # Check that first- and second-level lists agree.
      if flMap == 0:
        insist(slMap == 0, "second-level map must be null")
      if slMap == 0:
        insist(blk == addr(r.null), "block list must be null")
        continue
      insist(slList != 0, "no free blocks in second-level map")
      insist(blk != addr(r.null), "block should not be null")
      while blk != addr(r.null):
        insist(isFree(blk), "block should be free")
        insist(not isPrevFree(blk), "blocks should have coalesced")
        insist(not isFree(next(blk)), "blocks should have coalesced")
        insist(isPrevFree(next(blk)), "block should be free")
        insist(size(blk) >= sizeMin, "block not minimum size")
        let (fli, sli) = mappingInsert(size(blk))
        insist(fli == i and sli == j, "block size indexed in wrong list")
        blk = blk.nextFree
      inc(j)
    inc(i)
  result = status

type
  Walker = proc(z: pointer; size: ByteAddress; used: bool; env: pointer): bool {.nimcall.}

proc defaultWalker*(z: pointer; size: ByteAddress; used: bool; env: pointer): bool =
  proc printf(fmt: cstring) {.importc, header: "<stdio.h>", varargs.}

  printf("\t%p %s size: %ld (origin %p)\n", z, if used: "used" else: "free",
         size, fromPtr(z))
  # do not stop iteration:
  return false

proc walkPool(pool: Pool; walker: Walker; env: pointer) =
  var poolWalker = if walker != nil: walker else: defaultWalker
  var blk = offsetToBlock(pool, -headerOverhead)
  while blk != nil and not isLast(blk):
    if poolWalker(toPtr(blk), size(blk), not isFree(blk), env): break
    blk = next(blk)

proc checkPool(pool: Pool): int =
  ## Check that the blocks are physically correct.
  var integ = Integrity()
  walkPool(pool, integrityWalker, addr(integ))
  result = integ.status

proc removePool(r: var Tlsf; pool: Pool) =
  var blk = offsetToBlock(pool, -headerOverhead)
  sysAssert(isFree(blk), "blk should be free")
  sysAssert(not isFree(next(blk)), "next blk should not be free")
  sysAssert(size(next(blk)) == 0, "next blk size should be zero")
  let (fl, sl) = mappingInsert(size(blk))
  removeFreeBlock(r, blk, fl, sl)

# TLSF main interface.

const tlsfDebug = false

when tlsfDebug:
  proc testFfsFls*() =
    ## Verify ffs/fls work properly.
    var rv: int = 0
    inc(rv, if (ffs(0) == - 1): 0 else: 1)
    inc(rv, if (fls(0) == - 1): 0 else: 2)
    inc(rv, if (ffs(1) == 0): 0 else: 4)
    inc(rv, if (fls(1) == 0): 0 else: 8)
    inc(rv, if (ffs(0x80000000u32) == 31): 0 else: 0x10)
    inc(rv, if (ffs(0x80008000u32) == 15): 0 else: 0x20)
    inc(rv, if (fls(0x80000008u32) == 31): 0 else: 0x40)
    inc(rv, if (fls(0x7FFFFFFFu32) == 30): 0 else: 0x80)
    when sizeof(ByteAddress) == 8:
      inc(rv, if (flsInt(int 0x80000000) == 31): 0 else: 0x100)
      inc(rv, if (flsInt(int(0x0000000100000000'i64)) == 32): 0 else: 0x200)
      inc(rv, if (flsInt(int(0xFFFFFFFFFFFFFFFF'i64)) == 63): 0 else: 0x400)
    if rv != 0:
      echo("test_ffs_fls: ", rv, " ffs/fls tests failed.")

proc createWithPool(r: var Tlsf; mem: pointer; bytes: int) =
  when tlsfDebug:
    testFfsFls()
  setup(r)
  addPool(r, mem, bytes)

proc alloc(r: var Tlsf; size: ByteAddress): pointer =
  let adjust = adjustRequestSize(size, ALIGN_SIZE)
  let blk = locateFree(r, adjust)
  return prepareUsed(r, blk, adjust)

proc memalign(r: var Tlsf; align: ByteAddress; size: ByteAddress): pointer =
  var adjust = adjustRequestSize(size, ALIGN_SIZE)
  # We must allocate an additional minimum blk size bytes so that if
  # our free blk will leave an alignment gap which is smaller, we can
  # trim a leading free blk and release it back to the pool. We must
  # do this because the previous physical blk is in use, therefore
  # the prev_phys_block field is not valid, and we can't simply adjust
  # the size of that blk.
  var gapMinimum: ByteAddress = sizeof(Header)
  var sizeWithGap: ByteAddress = adjustRequestSize(adjust + align + gapMinimum, align)
  # If alignment is less than or equals base alignment, we're done.
  # If we requested 0 bytes, return null, as tlsf_malloc(0) does.
  var alignedSize: ByteAddress = if (adjust != 0 and align > ALIGN_SIZE): sizeWithGap else: adjust
  var blk: HeaderPtr = locateFree(r, alignedSize)
  # This can't be a static sysAssert.
  sysAssert(sizeof(Header) == sizeMin + headerOverhead, "memalign: wrong sizeof computation")
  if blk != nil:
    var z: pointer = toPtr(blk)
    var aligned: pointer = alignPtr(z, align)
    var gap: ByteAddress = mycast(int, aligned) - mycast(int, z)
    # If gap size is too small, offset to next aligned boundary.
    if gap != 0 and gap < gapMinimum:
      var gapRemain: ByteAddress = gapMinimum - gap
      var offset: ByteAddress = max(gapRemain, align)
      var nextAligned: pointer = mycast(pointer, mycast(int, aligned) + offset)
      aligned = alignPtr(nextAligned, align)
      gap = cast[int](aligned) - mycast(int, z)
    if gap != 0:
      sysAssert(gap >= gapMinimum, "gap size too small")
      blk = trimFreeLeading(r, blk, gap)
  return prepareUsed(r, blk, adjust)

proc dealloc(r: var Tlsf; z: pointer) =
  sysAssert(z != nil, "nil passed to dealloc")
  var blk = fromPtr(z)
  sysAssert(not isFree(blk), "block already marked as free")
  inc(r.osa.freeMem, blk.size)
  markAsFree(blk)
  blk = mergePrev(r, blk)
  blk = mergeNext(r, blk)
  insert(r, blk)

# The TLSF blk information provides us with enough information to
# provide a reasonably intelligent implementation of realloc, growing or
# shrinking the currently allocated block as required.
#
# This routine handles the somewhat esoteric edge cases of realloc:
# - a non-zero size with a null pointer will behave like malloc
# - a zero size with a non-null pointer will behave like free
# - a request that cannot be satisfied will leave the original buffer
#   untouched
# - an extended buffer size will leave the newly-allocated area with
#   contents undefined

proc realloc(r: var Tlsf; z: pointer; size: ByteAddress): pointer =
  ## Zero-size requests are treated as free.
  if z != nil and size == 0:
    dealloc(r, z)
  elif z == nil:
    result = alloc(r, size)
  else:
    var blk: HeaderPtr = fromPtr(z)
    var next: HeaderPtr = next(blk)
    var cursize: ByteAddress = size(blk)
    var combined: ByteAddress = cursize + size(next) + headerOverhead
    var adjust: ByteAddress = adjustRequestSize(size, ALIGN_SIZE)
    sysAssert(not isFree(blk), "blk already marked as free")
    # If the next blk is used, or when combined with the current
    # block, does not offer enough space, we must reallocate and copy.
    if adjust > cursize and (not isFree(next) or adjust > combined):
      result = alloc(r, size)
      if result != nil:
        var minsize: ByteAddress = min(cursize, size)
        copyMem(result, z, minsize)
        dealloc(r, z)
    else:
      # Do we need to expand to the next block?
      if adjust > cursize:
        discard mergeNext(r, blk)
        markAsUsed(blk)
      trimUsed(r, blk, adjust)
      result = z

# Todo:
# - remove SMALL_BLOCK_SIZE?


const nimMaxHeap {.intdefine.} = 0

const
  InitialMemoryRequest = 128 * PageSize
  MaxMemoryRequest = 128 * 1024 * 1024 # grow the heap in 128MB steps

type
  HeapLinks = object
    len: int
    chunks: array[30, (pointer, int)]
    next: ptr HeapLinks

  PLLChunk = ptr LLChunk
  LLChunk = object ## *low-level* chunk
    size: int                # remaining size
    acc: int                 # accumulator
    next: PLLChunk           # next low-level chunk; only needed for dealloc

  OsAllocs = object
    currMem, maxMem, freeMem: int # memory sizes (allocated from OS)
    heapLinks: HeapLinks
    llmem: PLLChunk
    nextChunkSize: int
    preventChunkSizeIncrease: bool

proc setup(a: var OsAllocs) =
  a.nextChunkSize = InitialMemoryRequest
  when defined(emscripten):
    a.preventChunkSizeIncrease = true

proc incCurrMem(a: var OsAllocs, bytes: int) {.inline.} =
  inc(a.currMem, bytes)

proc decCurrMem(a: var OsAllocs, bytes: int) {.inline.} =
  a.maxMem = max(a.maxMem, a.currMem)
  dec(a.currMem, bytes)

proc getMaxMem(a: var OsAllocs): int =
  # Since we update maxPagesCount only when freeing pages,
  # maxPagesCount may not be up to date. Thus we use the
  # maximum of these both values here:
  result = max(a.currMem, a.maxMem)

proc llAlloc(a: var OsAllocs, size: int): pointer =
  # *low-level* alloc for the memory managers data structures. Deallocation
  # is done at the end of the allocator's life time.
  if a.llmem == nil or size > a.llmem.size:
    # the requested size is ``roundup(size+sizeof(LLChunk), PageSize)``, but
    # since we know ``size`` is a (small) constant, we know the requested size
    # is one page:
    sysAssert roundup(size+sizeof(LLChunk), PageSize) == PageSize, "roundup 6"
    var old = a.llmem # can be nil and is correct with nil
    a.llmem = cast[PLLChunk](osAllocPages(PageSize))
    incCurrMem(a, PageSize)
    a.llmem.size = PageSize - sizeof(LLChunk)
    a.llmem.acc = sizeof(LLChunk)
    a.llmem.next = old
  result = cast[pointer](cast[ByteAddress](a.llmem) + a.llmem.acc)
  dec(a.llmem.size, size)
  inc(a.llmem.acc, size)
  zeroMem(result, size)

proc addHeapLink(a: var OsAllocs; p: pointer, size: int) =
  var it = addr(a.heapLinks)
  while it != nil and it.len >= it.chunks.len: it = it.next
  if it == nil:
    var n = cast[ptr HeapLinks](llAlloc(a, sizeof(HeapLinks)))
    n.next = a.heapLinks.next
    a.heapLinks.next = n
    n.chunks[0] = (p, size)
    n.len = 1
  else:
    let L = it.len
    it.chunks[L] = (p, size)
    inc it.len

proc llDeallocAll(a: var OsAllocs) =
  var it = a.llmem
  while it != nil:
    # we know each block in the list has the size of 1 page:
    var next = it.next
    osDeallocPages(it, PageSize)
    it = next

proc deallocOsPages(a: var OsAllocs) =
  # we free every 'ordinarily' allocated page by iterating over the page bits:
  var it = addr(a.heapLinks)
  while true:
    let next = it.next
    for i in 0..it.len-1:
      let (p, size) = it.chunks[i]
      when defined(debugHeapLinks):
        cprintf("owner %p; dealloc A: %p size: %ld; next: %p\n", addr(a),
          it, it.origSize, next)
      sysAssert size >= PageSize, "origSize too small"
      osDeallocPages(p, size)
    it = next
    if it == nil: break
  # And then we free the pages that are in use for the page bits:
  llDeallocAll(a)

proc requestOsChunk(a: var OsAllocs): (pointer, int) =
  result[1] = a.nextChunkSize
  while true:
    result[0] = osTryAllocPages(result[1])
    if result[0] != nil: break
    if result[1] > InitialMemoryRequest:
      result[1] = result[1] div 2
      a.preventChunkSizeIncrease = true
    else:
      raiseOutOfMem()
  if result[1] == a.nextChunkSize and not a.preventChunkSizeIncrease and
      a.nextChunkSize < MaxMemoryRequest:
    a.nextChunkSize = a.nextChunkSize * 2

  incCurrMem(a, result[1])
  inc(a.freeMem, result[1])
  a.addHeapLink(result[0], result[1])
  sysAssert((cast[ByteAddress](result) and PageMask) == 0, "requestOsChunks 1")

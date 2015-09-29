import winlean, asyncdispatch, os, future

type
  INPUT_RECORD = object {.importc, header: "Wincon.h".}

proc kbhit(): cint {.importc: "_kbhit", header: "<conio.h>".}
proc getch(): cint {.importc: "_getch", header: "<conio.h>".}

proc readConsoleInput(handle: Handle, lpBuffer: pointer, length: DWord,
                      numberOfEvents: PDWord): bool
                      {.stdcall, importc: "ReadConsoleInputA", dynlib: "kernel32".}

proc onStdinEvent(handle: Handle, buffer: var string,
                  retFuture: Future[string]) =
  echo("Ready")
  if kbhit() != 0:
    let ch = getch().chr()
    if ch == '\13':
      retFuture.complete(buffer)
      return
    else:
      buffer.add(ch)
      echo(ch.repr)
  else:
    var r: array[512, INPUT_RECORD]
    var read: DWORD
    discard readConsoleInput(handle, addr r[0], 512, addr read)
    #echo(read)

proc readConsoleLine(): Future[string] =
  # Shamelessly stolen from: http://stackoverflow.com/a/21749034/492186
  # Yes, it's inefficient. But it works. Wanna improve it? Make a PR :)
  var retFuture = newFuture[string]()
  var buffer = ""
  let handle = getStdHandle(STD_INPUT_HANDLE)
  asyncdispatch.addEventHandle(handle,
    (handle: Handle) -> void => onStdinEvent(handle, buffer, retFuture))
  return retFuture

proc main() {.async.} =
  while true:
    let line = await readConsoleLine()
    echo("Got line: ", line)

waitFor main()

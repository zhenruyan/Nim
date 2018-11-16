discard """
  output: '''@[1]
(10, (20, ))'''
"""

import strutils, sequtils

# bug #668

type
  TThing = ref object
    data: int
    children: seq[TThing]

proc `$`(t: TThing): string =
  result = "($1, $2)" % @[$t.data, join(map(t.children, proc(th: TThing): string = $th), ", ")]

proc somethingelse(): seq[TThing] =
  result = @[TThing(data: 20, children: @[])]

proc dosomething(): seq[TThing] =
  result = somethingelse()

  result = @[TThing(data: 10, children: result)]

# bug #9684
var s2 = @[2, 2]
s2 = @[s2.len]
echo s2

when isMainModule:
  echo($dosomething()[0])

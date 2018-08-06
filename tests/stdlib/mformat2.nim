
import strformat

proc fails*(a: static[int]) =
  echo &"formatted {a:2}"

proc fails2*[N: static[int]](a: int) =
  echo &"formatted {a:2}"

proc works*(a: int) =
  echo &"formatted {a:2}"

proc fails0*(a: int or uint) =
  echo &"formatted {a:2}"

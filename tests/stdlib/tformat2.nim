discard """
  output: '''formatted  5
formatted  6
formatted  7
formatted  8'''
"""

# bug #7632
import mformat2

works(5)

fails0(6)

fails(7)

fails2[0](8)

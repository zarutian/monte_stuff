
import "lib/tubes" =~ [ => Fount, => Drain ]
import "guards" =~ [ => Tuple, => Bytes ]
exports(makeSimpleByteChannelMuxer, makeSimpleByteChannelDemuxer)

def bF := Fount[Bytes]
def SixteenBytesFounts := Tuple[bF, bF, bF, bF, bF, bF, bF, bF, bF, bF, bF, bF, bF, bF, bF, bF]

def makeSimpleByteChannelMuxer (Fs :SixteenBytesFounts, D :Drain[Bytes]) :List[Drain[Bytes]] {
  def drains := [].diverge()
  for (i in (0 .. 15)) {
    hide {
      def idx := i
      object drain extends as Drain {
        to recieve(data :Bytes) {
          var buffer := data
          do {
            if (buffer.size() > 15) {
              def slice := buffer.slice(0, 14)
              buffer := buffer.slice(15, (buffer.size() - 14))
              D.recieve( ((idx<<4)+15).asByte().concat(slice))
            } else {
              D.recieve( ((idx<<4)+buffer.size()).asByte().concat(buffer))
            }
          } until (buffer.size() == 0)
        }
        # other methods left as an exercise for the 'student'
      }
      Fs[idx] <- flowInto(drain)
      drains.push(drain)
    }
  }
  return drains.snapshot()
}

def bD := Drain[Bytes]
def SixteenBytesDrains := Tuple[bD, bD, bD, bD, bD, bD, bD, bD, bD, bD, bD, bD, bD, bD, bD, bD]

def makeSimpleByteChannelDemuxer (F :Fount, Ds :SixteenBytesDrains) :Drain[Bytes] {
  def buffer := b``.diverge()
  object drain as Drain {
    to recieve (data :Bytes) {
      buffer += data
      do {
        if (buffer.size() < 1) { return }
        def chanId := (buffer[0].asInt() & 0xF0) >> 4
        def length := (buffer[0].asInt() & 0x0F)
        if (buffer.size() < (1 + length)) { return }
        def slice := buffer.slice(1, length)
        buffer := buffer.slice((length + 1), (buffer.size() - length))
        Ds[chanId].recieve(slice)
      } until (buffer.size() == 0)
    }
  }
  F.flowInto(drain)
  return drain
}

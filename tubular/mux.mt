
import "lib/tubes" =~ [ => Fount, => Drain ]
import "guards" =~ [ => Tuple ]
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
              buffer := buffer.slice(15, buffer.size())
              D.recieve( ((idx<<4)+15).asByte().concat(slice))
            } else {
              D.recieve( ((idx<<4)+buffer.size()).asByte().concat(buffer))
            }
          } until (buffer.size() == 0)
        }
        # other methods left as an exercise for the 'student'
      }
      drains.push(drain)
    }
  }
  return drains.snapshot()
}

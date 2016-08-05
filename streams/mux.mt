import "bufferedStream" =~ [ => makeBufferedSink]

# Simple channel muxing protocol:
#  1 byte which is split into two nybbles:
#    1 nybble (4 bits): channel_id     (0 to 15)
#    1 nybble (4 bits): payload_length (1 to 16)
#  payload_length bytes: payload for channel channel_id

def makeMuxingSink (theOnwardSink :Sink) :List[Sink] {
  # does no prioritization
  def sinks := [].diverge()
  def makeSink (chanId) {
    return object as Sink {
      to run(packet :Bytes) {
        var buffer := packet
        while (true) { # Because Monte doesnt do do loops
          def s := buffer.size()
          if (s > 16) {
            def out := (_makeBytes.fromInts([((chanId << 4) + 16)])) + buffer.slice(0, 16)
            buffer := buffer.slice(16, s)
            theOnwardSink(out)
          } else if (s == 0) {
            break
          } else {
            def out := (_makeBytes.fromInts([((chanId << 4) + s + 1)])) + buffer
            theOnwardSink(out)
            break
          }
        }
        to abort(problem) :Vow[Void] {
          return theOnwardSink.abort(problem)
        }
        to complete() :Vow[Void] {
          return theOnwardSink.complete()
        }
      }
    }
  }
  for (i in (0 .. 15)) {
    sinks.push(makeSink(i))
  }
  return sinks.snapshot()
}
def makeDemuxingSink (theOnwardSinks :List[Sink]) :Sink {
  if (theOnwardSinks.size() != 16) {
    throw("theOnwardSinks list be exactly 16 elements long")
  }
  def completeTo (buffer :Bytes) :Nat {
    if (buffer.size() == 0) { return 0 }
    def length := (buffer[0] & 0x0F) + 1
    if (buffer.size() >= (length + 1)) {
      return length
    } else {
      return 0
    }
  }
  def outSink := object as Sink {
    to run(packet :Bytes) {
      # this should now be only getting segments
      def chanId := (packet[0] >> 4) & 0x0F
      theOnwardSinks[chanId].run(packet.slice(1, packet.size()))
    }
    to abort(problem) :Vow[Void] {
      def vows := [].diverge()
      for (i in (0 .. 15)) {
        vows.push(theOnwardSinks[i].abort(problem))
      }
      return whenAllSettled(vows, null)
    }
    to complete() :Vow[Void] {
      def vows := [].diverge()
      for (i in (0 .. 15)) {
        vows.push(theOnwardSinks[i].complete())
      }
      return whenAllSettled(vows, null)
    }
  }
  def bufferedSink := makeBufferedSink(b``, completeTo, outSink)
  return bufferedSink
}

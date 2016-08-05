import "bufferedStream" =~ [ => makeBufferedSink]

# Simple channel muxing protocol:
#  1 byte which is split into two nybbles:
#    1 nybble (4 bits): channel_id     (0 to 15)
#    1 nybble (4 bits): payload_length (1 to 16)
#  payload_length bytes: payload for channel channel_id

def makeMuxingPipe () :Tuple[Source, List[Sink]] {
  # does no prioritization
  var [theOnwardSink, resolveOnwardSink] := Ref.promise()
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
            theOnwardSink <- run(out)
          } else if (s == 0) {
            break
          } else {
            def out := (_makeBytes.fromInts([((chanId << 4) + s + 1)])) + buffer
            theOnwardSink <- run(out)
            break
          }
        }
        to abort(problem) :Vow[Void] {
          return theOnwardSink <- abort(problem)
        }
        to complete() :Vow[Void] {
          return theOnwardSink <- complete()
        }
      }
    }
  }
  for (i in (0 .. 15)) {
    sinks.push(makeSink(i))
  }
  def source as Source (sink :Sink) {
    resolveOnwardSink.resolve(sink)
  }
  return [source, sinks]
}

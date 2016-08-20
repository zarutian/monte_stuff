
# this uses the streams instead of tubes.

import "../streams/bufferedStream.mt" =~ [ => makeBufferedSink ]

# takes in stream of bytes and sinks whole banana tokens onwards
def makeBananaTokensSink (onward :Sink) :Sink {
  def completeTo (buffer :Bytes) :Nat {
    # this is a pure function
    def buf_size = buffer.size()
    if (buf_size == 0) { return 0 }
    var header := b``
    var idx := 0
    var type_found :Bool := false
    while (idx < buf_size) {
      if ((buffer[idx] & 0x80) == 0x80) {
        type_found := true
        break
      } else {
        header += buffer[idx]
        idx += 1
      }
    }
    if (type_found == false) { return 0 }
  }
}

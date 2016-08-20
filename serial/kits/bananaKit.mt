
# this uses the streams instead of tubes.

import "../streams/bufferedStream.mt" =~ [ => makeBufferedSink ]

def LIST_old   := 0x80
def INT        := 0x81
def STRING     := 0x82
def NEG        := 0x83
def FLOAT      := 0x84
def OLDLONGINT := 0x85
def OLDLONGNEG := 0x86
def VOCAB      := 0x87
def OPEN       := 0x88
def CLOSE      := 0x89
def ABORT      := 0x8A
def LONGINT    := 0x8B
def LONGNEG    := 0x8C
def ERROR      := 0x8D
def PING       := 0x8E
def PONG       := 0x8F

def fromLittleEndianBase128toNatural (input :Bytes) :Nat {
  var acc := 0
  var idx := 0
  while (idx < input.size()) {
    acc := acc + (input[idx] << (idx * 7))
    idx += 1
  }
  return acc
}

# takes in stream of :Bytes and sinks whole banana tokens (still :Bytes though but broken up) onwards
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
    switch (buffer[idx]) {
      match ==LIST_old { return idx }
      match ==INT      { return idx }
      match ==STRING {
      
      }
    }
  }
}

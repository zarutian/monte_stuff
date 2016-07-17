import "guards" =~ [ => Tuple, => Nat]
exports(msgpckKit)

# see ebnf.txt at https://gist.github.com/zarutian/fb21d0a8c910ab255401

def makeInteger (bytes) :Any {
  return object {
    to kind () { return "msgpck_Integer" }
    to get () { return bytes.asInteger() }
  }
}
def makeNil () :Any {
  return object {
    to kind () { return "msgpck_Nil" }
    to get () { return null }
  }
}
def makeBool(boolean) :Any {
  return object {
    to kind () { return "msgpck_Bool" }
    to get () { return boolean }
  }
}

object msgpckParser {
  to parse (buffer :Bytes) :Tuple[Nat, Any] {
    if (buffer.size() < 1) { return [0, null] }
    if ((buffer[0] & 0x80) == 0x00) {
      # pos fixint
      return [1, makeInteger(buffer[0] & 0x7f)]
    } else {
            if ((buffer[0] & 0xF0) == 0x80) {
        # fixmap
        def numberOfElements := (buffer[0] & 0x0F).asInteger()
        def [consumed, map] := parseMap(buffer.slice(1, (buffer.size() - 1)), numberOfElementes)
        return [consumed + 1, map]
      } elseif ((buffer[0] & 0xF0) == 0x90) {
        # fixarray
        def numberOfElements := (buffer[0] & 0x0F).asInteger()
        def [consumed, arr] := parseArray(buffer.slice(1, (buffer.size() - 1)), numberOfElementes)
        return [consumed + 1, arr]
      } elseif ((buffer[0] & 0xE0) == 0xA0) {
        # fixstr
        def numberOfBytes ;= (buffer[0] & 0x1F).asInteger()
        def [consumed, string] := parseString(buffer.slice(1, (buffer.size() -1)), numberOfBytes)
        return [consumed + 1, string]
      } else {
        switch (buffer[0]) {
          match ==0xC0 {
            # nil
            return [1, makeNil()]
          }
          match ==0xC1 {
            # never used
            throw("error: value tag 0xC1 encountered")
          }
          match ==0xC2 {
            # false
            return [1, makeBool(false)]
          }
          match ==0xC3 {
            # true
            return [1, makeBool(true)]
          }
          mathc ==0xC4 {
          }
        }
      }
    }
  }
}

object msgpckKit {

}

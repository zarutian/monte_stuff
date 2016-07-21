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
        if (consumed == 0) { return [0, null] }
        return [consumed + 1, map]
      } elseif ((buffer[0] & 0xF0) == 0x90) {
        # fixarray
        def numberOfElements := (buffer[0] & 0x0F).asInteger()
        def [consumed, arr] := parseArray(buffer.slice(1, (buffer.size() - 1)), numberOfElementes)
        if (consumed == 0) { return [0, null] }
        return [consumed + 1, arr]
      } elseif ((buffer[0] & 0xE0) == 0xA0) {
        # fixstr
        def numberOfBytes ;= (buffer[0] & 0x1F).asInteger()
        def [consumed, string] := parseString(buffer.slice(1, (buffer.size() -1)), numberOfBytes)
        if (consumed == 0) { return [0, null] }
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
          match ==0xC4 {
            # bin 8
            if (buffer.size() < 2) { return [0, null] }
            def numberOfBytes := buffer[1].asInteger()
            def [consumed, bin] := parseBinary(buffer.slice(2, (buffer.size() - 1)), numberOfBytes)
            if (consumed == 0) { return [0, null] }
            return [consumed + 2, bin]
          }
          match ==0xC5 {
            # bin 16
            if (buffer.size() < 3) { return [0, null] }
            def numberOfBytes := buffer.slice(1, 2).asInteger()
            def [consumed, bin] := parseBinary(buffer.slice(3, (buffer.size() - 1)), numberOfBytes)
            if (consumed == 0) { return [0, null] }
            return [consumed + 3, bin]
          }
          match ==0xC6 {
            # bin 32
            if (buffer.size() < 5) { return [0, null] }
            def numberOfBytes := buffer.slice(1, 4).asInteger()
            def [consumed, bin] := parseBinary(buffer.slice(5, (buffer.size() - 1)), numberOfBytes)
            if (consumed == 0) { return [0, null] }
            return [consumed + 5, bin]
          }
          match ==0xC7 {
            # ext 8
            if (buffer.size() < 2) { return [0, null] }
            def numberOfBytes := buffer[1].asInteger()
            def [consumed, ext] := parseExt(buffer.slice(1, (buffer.size() - 1)), numberOfBytes)
            if (consumed == 0) { return [0, null] }
            return [consumed + 2, ext]
          }
          match ==0xC8 {
            # ext 16
            if (buffer.size() < 3) { return [0, null] }
            def numberOfBytes := buffer.slice(1, 2).asInteger()
            def [consumed, ext] := parseExt(buffer.slice(3, (buffer.size() - 1)), numberOfBytes)
            if (consumed == 0) { return [0, null] }
            return [consumed + 3, ext]
          }
          match ==0xC9 {
            # ext 32
            if (buffer.size() < 5) { return [0, null] }
            def numberOfBytes := buffer.slice(1, 4).asInteger()
            def [consumed, ext] := parseExt(buffer.slice(5, (buffer.size() - 1)), numberOfBytes)
            if (consumed == 0) { return [0, null] }
            return [consumed + 5, ext]
          }
          match ==0xCA {
            # float 32
            if (buffer.size() < 5) { return [0, null] }
            return [5, makeFloat32(buffer.slice(1, 4))]
          }
          match ==0xCB {
            # float 64
            if (buffer.size() < 9) { return [0, null] }
            return [9, makeFloat64(buffer.slice(1, 8))]
          }
          match ==0xCC {
            # uint 8
            if (buffer.size() < 2) { return [0, null] }
            return [2, makeUint(buffer[1])]
          }
          match ==0xCD {
            # uint 16
            if (buffer.size() < 3) { return [0, null] }
            return [3, makeUint(buffer.slice(1, 2))]
          }
          match ==0xCE {
            # uint 32
            if (buffer.size() < 5) { return [0, null] }
            return [5, makeUint(buffer.slice(1, 4))]
          }
          match ==0xCF {
            # uint 64
            if (buffer.size() < 9) { return [0, null] }
            return [9, makeUint(buffer.slice(1, 8))]
          }
          match ==0xD0 {
            # int 8
            if (buffer.size() < 2) { return [0, null] }
            return [2, makeInt(buffer[1])]
          }
          match ==0xD1 {
            # int 16
            if (buffer.size() < 3) { return [0, null] }
            return [3, makeInt(buffer.slice(1, 2))]
          }
          match ==0xD2 {
            # int 32
            if (buffer.size() < 5) { return [0, null] }
            return [5, makeInt(buffer.slice(1, 4))]
          }
          match ==0xD3 {
            # int 64
            if (buffer.size() < 9) { return [0, null] }
            return [9, makeInt(buffer.slice(1, 8))]
          }
          match ==0xD4 {
            # fixext 1
            if (buffer.size() < 3) { return [0, null] }
            return parseExt(buffer.slice(1, 2), 2)
          }
          match ==0xD5 {
            # fixext 2
            if (buffer.size() < 4) { return [ 0, null] }
            return parseExt(buffer.slice(1, 3), 3)
          }
          match =0xD6 {
            # fixext 4
            if (buffer.sice() < 6) { return [0, null] }
            return parseExt(buffer.slice(1, 5), 5)
          }
          match =0xD7 {
            # fixext 8
            if (buffer.sice() < 10) { return [0, null] }
            return parseExt(buffer.slice(1, 9), 9)
          }
        }
      }
    }
  }
}

object msgpckKit {

}

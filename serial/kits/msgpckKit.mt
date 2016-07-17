import "guards" =~ [ => Tuple, => Nat]
exports(msgpckKit)

# see ebnf.txt at https://gist.github.com/zarutian/fb21d0a8c910ab255401

def makeInteger (bytes) {
  return object {
    to kind () { return "msgpck_Integer" }
    to get () { return bytes.asInteger() }
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
      } elseif ((buffer[0] & 0xF0) == 0x90) {
        # fixarray
      } elseif ((buffer[0] & 0xE0) == 0xA0) {
        # fixstr
      } else {
      }
    }
  }
}

object msgpckKit {

}

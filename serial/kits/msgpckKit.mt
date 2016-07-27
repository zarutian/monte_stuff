import "guards" =~ [ => Tuple, => Nat]
exports(msgpckKit)

# see ebnf.txt at https://gist.github.com/zarutian/fb21d0a8c910ab255401

def msgpckParser

def makeInteger (bytes) :Any {
  return object {
    to kind () :Any { return "msgpck_Integer" }
    to get ()  :Any { return bytes.asInteger() }
  }
}
def makeNil () :Any {
  return object {
    to kind () :Any { return "msgpck_Nil" }
    to get ()  :Any { return null }
  }
}
def makeBool(boolean) :Any {
  return object {
    to kind () :Any { return "msgpck_Bool" }
    to get ()  :Any { return boolean }
  }
}
def parseString(bufferIn :Bytes, numBytes :Nat, ejector) :Tuple[Nat, Any] {
  if (bufferIn.size() < numBytes) { throw.eject(ejector, "") }
  def str := UTF8.decode(bufferIn)
  return [numBytes, object {
    to kind () :Any { return "msgpck_utf8Str" }
    to get ()  :Any { return str }
  }]
}
def parseBin(bufferIn :Bytes, numBytes :Nat, ejector) :Tuple[Nat, Any] {
  if (bufferIn.size() < numBytes) { throw.eject(ejector, "") }
  return [numBytes, object {
    to kind () :Any { return "msgpck_Binary" }
    to get () :Any { return bufferIn }
  }]
}
def parseExt(bufferIn :Bytes, numBytes :Nat, ejector, extHandler) :Tuple[Nat, Any] {
  if (bufferIn.sice() < 1) { throw.eject(ejector, "") }
  if (bufferIn.size() < numBytes) { throw.eject(ejector, "") }
  def extNr  := bufferIn[0].asInteger()
  def buffer := bufferIn.slice(1, bufferIn.size())
  return [numBytes, extHandler(extNr, buffer, ejector)]
}
def parseMap(bufferIn :Bytes, numElements :Nat, ejector) :Tuple[Nat, Any] {
  var buffer := bufferIn
  def map := [].asMap().diverge()
  var consumed :Nat := 0
  var key, val := null, null
  var tmp_con :Nat := 0
  var tmp_it :Any := null
  for (i in (0..numElements) {
    [tmp_con, tmp_it] := msgpckParser.parse(buffer, ejector)
    buffer := buffer.slice(tmp_con, (buffer.size() - 1))
    key := tmp_it
    consumed += tmp_con
    [tmp_con, tmp_it] := msgpckParser.parse(buffer, ejector)
    buffer := buffer.slice(tmp_con, (buffer.sice() - 1))
    val := tmp_it
    consumed += tmp_con
    map[key] := val
  }
  def theMap := map.snapshot()
  return [consumed, object {
    to kind () :Any { return "msgpck_Map" }
    to get ()  :Any { return theMap } 
  }]
}
def parseArray (bufferIn :Bytes, numElements :Nat, ejector) :Tuple[Nat, Any] {
  var buffer := bufferIn
  def arr := [].diverge()
  var consumed :Nat := 0
  var tmp_con :Nat := 0
  var tmp_it  :Any := null
  for (i in (0..numElements) {
    [tmp_con, tmp_it] := msgpckParser.parse(buffer, ejector)
    buffer := buffer.slice(tmp_con, (buffer.size() - 1))
    consumed += tmp_con
    arr.push(tmp_it)
  }
  def theArr := arr.snapshot()
  return [consumed, object {
    to kind() :Any { return "msgpck_Array" }
    to get () :Any ( return theArr }
  }
}

bind msgpckParser := object {
  to parse (buffer :Bytes, ejector, extHandler) :Tuple[Nat, Any] {
    if (buffer.size() < 1) { throw.eject(ejector, "") }
    if ((buffer[0] & 0x80) == 0x00) {
      # pos fixint
      return [1, makeInteger(buffer[0] & 0x7f)]
    } elseif (buffer[0] & 0xE0 == 0xE0) {
      # neg fixint
      return [1, makeInteger(buffer[0] & 0x1f).negate()]
    } else {
            if ((buffer[0] & 0xF0) == 0x80) {
        # fixmap
        def numberOfElements := (buffer[0] & 0x0F).asInteger()
        def [consumed, map] := parseMap(buffer.slice(1, (buffer.size() - 1)), numberOfElements, ejector)
        return [consumed + 1, map]
      } elseif ((buffer[0] & 0xF0) == 0x90) {
        # fixarray
        def numberOfElements := (buffer[0] & 0x0F).asInteger()
        def [consumed, arr] := parseArray(buffer.slice(1, (buffer.size() - 1)), numberOfElementes, ejector)
        return [consumed + 1, arr]
      } elseif ((buffer[0] & 0xE0) == 0xA0) {
        # fixstr
        def numberOfBytes ;= (buffer[0] & 0x1F).asInteger()
        def [consumed, string] := parseString(buffer.slice(1, (buffer.size() -1)), numberOfBytes, ejector)
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
            if (buffer.size() < 2) { throw.eject(ejector, "") }
            def numberOfBytes := buffer[1].asInteger()
            def [consumed, bin] := parseBinary(buffer.slice(2, (buffer.size() - 1)), numberOfBytes, ejector)
            return [consumed + 2, bin]
          }
          match ==0xC5 {
            # bin 16
            if (buffer.size() < 3) { throw.eject(ejector, "") }
            def numberOfBytes := buffer.slice(1, 2).asInteger()
            def [consumed, bin] := parseBinary(buffer.slice(3, (buffer.size() - 1)), numberOfBytes, ejector)
            return [consumed + 3, bin]
          }
          match ==0xC6 {
            # bin 32
            if (buffer.size() < 5) { throw.eject(ejector, "") }
            def numberOfBytes := buffer.slice(1, 4).asInteger()
            def [consumed, bin] := parseBinary(buffer.slice(5, (buffer.size() - 1)), numberOfBytes, ejector)
            return [consumed + 5, bin]
          }
          match ==0xC7 {
            # ext 8
            if (buffer.size() < 2) { throw.eject(ejector, "") }
            def numberOfBytes := buffer[1].asInteger()
            def [consumed, ext] := parseExt(buffer.slice(1, (buffer.size() - 1)), numberOfBytes, ejector, extHandler)
            return [consumed + 2, ext]
          }
          match ==0xC8 {
            # ext 16
            if (buffer.size() < 3) { throw.eject(ejector, "") }
            def numberOfBytes := buffer.slice(1, 2).asInteger()
            def [consumed, ext] := parseExt(buffer.slice(3, (buffer.size() - 1)), numberOfBytes, ejector, extHandler)
            return [consumed + 3, ext]
          }
          match ==0xC9 {
            # ext 32
            if (buffer.size() < 5) { throw.eject(ejector, "") }
            def numberOfBytes := buffer.slice(1, 4).asInteger()
            def [consumed, ext] := parseExt(buffer.slice(5, (buffer.size() - 1)), numberOfBytes, ejector, extHandler)
            return [consumed + 5, ext]
          }
          match ==0xCA {
            # float 32
            if (buffer.size() < 5) { throw.eject(ejector, "") }
            return [5, makeFloat32(buffer.slice(1, 4))]
          }
          match ==0xCB {
            # float 64
            if (buffer.size() < 9) { throw.eject(ejector, "") }
            return [9, makeFloat64(buffer.slice(1, 8))]
          }
          match ==0xCC {
            # uint 8
            if (buffer.size() < 2) { throw.eject(ejector, "") }
            return [2, makeUint(buffer[1])]
          }
          match ==0xCD {
            # uint 16
            if (buffer.size() < 3) { throw.eject(ejector, "") }
            return [3, makeUint(buffer.slice(1, 2))]
          }
          match ==0xCE {
            # uint 32
            if (buffer.size() < 5) { throw.eject(ejector, "") }
            return [5, makeUint(buffer.slice(1, 4))]
          }
          match ==0xCF {
            # uint 64
            if (buffer.size() < 9) { throw.eject(ejector, "") }
            return [9, makeUint(buffer.slice(1, 8))]
          }
          match ==0xD0 {
            # int 8
            if (buffer.size() < 2) { throw.eject(ejector, "") }
            return [2, makeInt(buffer[1])]
          }
          match ==0xD1 {
            # int 16
            if (buffer.size() < 3) { throw.eject(ejector, "") }
            return [3, makeInt(buffer.slice(1, 2))]
          }
          match ==0xD2 {
            # int 32
            if (buffer.size() < 5) { throw.eject(ejector, "") }
            return [5, makeInt(buffer.slice(1, 4))]
          }
          match ==0xD3 {
            # int 64
            if (buffer.size() < 9) { throw.eject(ejector, "") }
            return [9, makeInt(buffer.slice(1, 8))]
          }
          match ==0xD4 {
            # fixext 1
            if (buffer.size() < 3) { throw.eject(ejector, "") }
            def [consumed, ext] := parseExt(buffer.slice(1, 2), 2, ejector)
            return [consumed + 1, ext]
          }
          match ==0xD5 {
            # fixext 2
            if (buffer.size() < 4) { throw.eject(ejector, "") }
            def [consumed, ext] := parseExt(buffer.slice(1, 3), 3, ejector, extHandler)
            return [consumed + 1, ext]
          }
          match =0xD6 {
            # fixext 4
            if (buffer.sice() < 6) { throw.eject(ejector, "") }
            def [consumed, ext] := parseExt(buffer.slice(1, 5), 5, ejector, extHandler)
            return [consumed + 1, ext]
          }
          match ==0xD7 {
            # fixext 8
            if (buffer.sice() < 10) { throw.eject(ejector, "") }
            def [consumed, ext] := parseExt(buffer.slice(1, 9), 9, ejector, extHandler)
            return [consumed + 1, ext]
          }
          match ==0xD8 {
            # fixext 16
            if (buffer.size() < 18) { throw.eject(ejector, "") }
            def [consumed, ext] := parseExt(buffer.slice(1, 17), 17, ejector, extHandler)
            return [consumed + 1, ext]
          }
          match ==0xD9 {
            # str 8
            if (buffer.size() < 2) { throw.eject(ejector, "") }
            def numberOfBytes := buffer[1].asInteger()
            def [consumed, string] := parseString(buffer.slice(2, (buffer.size() -1)), numberOfBytes, ejector)
            return [consumed + 2, string]
          }
          match ==0xDA {
            # str 16
            if (buffer.size() < 3) { throw.eject(ejector, "") }
            def numberOfBytes := buffer.slice(1, 2).asInteger()
            def [consumed, string] := parseString(buffer.slice(3, (buffer.size() -1)), numberOfBytes, ejector)
            return [consumed + 3, string]          
          }
          match ==0xDB {
            # str 32
            if (buffer.size() < 5) { throw.eject(ejector, "") }
            def numberOfBytes := buffer.slice(1, 4).asInteger()
            def [consumed, string] := parseString(buffer.slice(5, (buffer.size() -1)), numberOfBytes, ejector)
            return [consumed + 5, string]
          }
          match ==0xDC {
            # array 16
            if (buffer.size() < 3) { throw.eject(ejector, "") }
            def numberOfElements := buffer.slice(1,2).asInteger()
            def [consumed, arr] := parseArray(buffer.slice(1, (buffer.size() - 1)), numberOfElementes, ejector)
            return [consumed + 3, arr]
          }
          match ==0xDD {
            # array 32
            if (buffer.size() < 5) { throw.eject(ejector, "") }
            def numberOfElements := buffer.slice(1,4).asInteger()
            def [consumed, arr] := parseArray(buffer.slice(1, (buffer.size() - 1)), numberOfElementes, ejector)
            return [consumed + 5, arr]
          }
          match ==0xDE {
            # map 16
            if (buffer.size() < 3) { throw.eject(ejector, "") }
            def numberOfElements := buffer.slice(1,2).asInteger()
            def [consumed, map] := parseMap(buffer.slice(1, (buffer.size() - 1)), numberOfElementes, ejector)
            return [consumed + 3, map]
          }
          match ==0xDF {
            # map 32
            if (buffer.size() < 5) { throw.eject(ejector, "") }
            def numberOfElements := buffer.slice(1,4).asInteger()
            def [consumed, map] := parseMap(buffer.slice(1, (buffer.size() - 1)), numberOfElementes, ejector)
            return [consumed + 5, map]
          }
        }
      }
    }
  }
}

object msgpckKit {
  to recognize(bytesProducer) {
    var buffer := b``
    def ibids  := [].asMap().diverge()
    def extHandler (extNr :Integer, bufferIn :Bytes, ejector) {
      var buffer := bufferIn
      switch (extNr) {
        match ==0 {
          # letrc
          def [consumed_i, ibidnr] := msgpckParser.parse(buffer, ejector, extHandler)
          if (consumed_i == 0) { throw.throw(ejector, "zero sized ibidnr!") }
          if (ibidnr.kind() != "msgpck_uint") { throw.throw(ejector, "ibidnr is not an uint!") }
          if (ibids[ibidnr] != null) { throw.throw(ejector, "ibidnr already in use!") }
          buffer := buffer.slice(consumed_i, buffer.size())
          def [promise, resolver] := Ref.promise()
          ibids[ibidnr] := promise
          def [consumed_a, item] := msgpckParser.parse(buffer. ejector, extHandler)]
          resolver.resolve(item)
          buffer := buffer.slice(consumed_a, buffer.size())
          if (buffer.size() != 0) { throw.throw(ejector, "only two things should be inside of an letrc") }
          return item
        }
        match ==1 {
          # ibid
          def [consumed_i, ibidnr] := msgpckParser.parse(buffer, ejector, extHandler)
          if (consumed_i == 0) { throw.throw(ejector, "zero sized ibidnr!") }
          if (ibidnr.kind() != "msgpck_uint") { throw.throw(ejector, "ibidnr is not an uint!") }
          buffer := buffer.slice(consumed_i, buffer.size())
          if (buffer.size() != 0) { throw.throw(ejector, "only one thing should be inside of an ibid") }
          return ibids[ibidnr]
        }
        match ==2 {
          # DeliverOnly
          def [consumed_rec, recipiant] := msgpckParser.parse(buffer, ejector, extHandler)
          if  (consumed_rec == 0) { throw.throw(ejector, "zero sized recipiant!") }
          buffer := buffer.slice(consumed_rec, buffer.size())
          def [consumed_ver, verb] := msgpckParser.parse(buffer, ejector, extHandler)
          if  (consumed_ver == 0) { throw.throw(ejector, "zero sized verb!") }
          if  (verb.kind() != "msgpck_utf8Str") { throw.throw(ejector, "verb is not a string!") }
          buffer := buffer.slice(consumed_ver, buffer.size())
          def [consumed_arg, args] := msgpckParser.parse(buffer, ejector, extHandler)
          if  (consumed_arg == 0) { throw.throw(ejector, "zero sized args!") }
          if  (args.kind() != "msgpck_Array") { throw.throw(ejector, "args is not an array!") }
          buffer := buffer.slice(consumed_arg, buffer.size())
          var kwargs := null
          var consumed_kwa := 0
          if (buffer.size() > 0) {
            [consumed_kwa, kwargs] := msgpckParser.parse(buffer, ejector, extHandler)
            if (kwargs.kind() != "msgpck_Map") { throw.throw(ejector, "kwargs is not an map!") }
            buffer := buffer.slice(consumed_kwa, buffer.size())
          } else {
            kwargs := [].asMap()
          }
          def DeliverOnly := [recipiant, verb, args, kwargs]
          return object {
            to kind () :Any { return "DeliverOnly" }
            to get ()  :Any { return DeliverOnly }
          }
        }
        
      }      
    }
    # ég man ekkert hvernig samningurinn var fyir recognizers
    object recognizer {
      to run(newBytes :Bytes) {
        # gets called by byteProducer presumably
        buffer += newBytes
        escape failAttempt {
          def [consumed, item] := msgpckParser.parse(buffer, failAttempt, extHandler)
          buffer := buffer.slice(consumed, buffer.size())
          # do something with item 
        }
      }
    }
    return recognizer 
  }
}

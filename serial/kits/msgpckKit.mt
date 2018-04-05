import "guards" =~ [ => Tuple, => Nat]
exports(msgpckKit)

# utility streamcaps that might be moved elsewhere.

object makeBufferSrc {
  to run (src :Src, numOfItemsWanted :Nat) :Source {
    var count := numOfItemsWanted
    def buffer := [].diverge()
    var inUse :Bool := false
    object my_src {
      to run(sink :Sink) :Vow[Void] {
        if (inUse) { throw("This BufferSrc is already in use by another sink!") }
        inUse := true
        object my_sink {
          to run(datum :Byte) :Vow[Void] {
            buffer.push(datum)
            count -= 1
            if (count > 0) {
              return src <- run(my_sink) # Ask for more
            } else {
              return sink <- run(buffer.snapshot())
            }
          }
          to complete() :Vow[Void] {
            # success?
            if (count > 0) {
              return sink <- abort("Buffer count not fullfilled!")
            } else {
              return sink <- complete()
            }
          }
          to abort(problem :Any) :Vow[Void] {
            # failure.
            return sink <- abort(problem)
          }
        }
        return src <- run(my_sink) # Ask for the first one
      }
    }
    return my_src
  }
}

object makeBytesBufferSrc {
  to run (src :ByteSrc, numOfBytesWanted :Nat) :Source {
    return makeBufferSrc(src, numOfBytesWanted)
  }
}

object makeByteSrcFromBuffer {
  to run(buffer :Bytes) :Src {
    var index := 0
    object my_src {
      to run(consumer :Sink) :Vow[Void] {
        if (index < buffer.size()) {
          def byte := buffer[index]
          index += 1
          return consumer <- run(byte)
        } else {
          return consumer <- complete()
        }
      }
      to leftover() :Bool {
        return (index < buffer.size()
      }
    }
    return my_src
  }
}

# see ebnf.txt at https://gist.github.com/zarutian/fb21d0a8c910ab255401


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
def parseString(bufferIn :BytesSrc, numBytes :Nat, consumer :Sink) :Vow[Void] {
  def str_src := makeBytesBufferSrc(bufferIn, numBytes)
  object my_sink {
    to run(bytes :Bytes) :Vow[Void] {
      def str := UTF8.decode(bytes)
      object str_wrap {
        to kind () :Any { return "msgpck_utf8Str" }
        to get ()  :Any { return str }
      }
      return consumer <- run(str_wrap)
    }
    to complete() :Vow[Void] {
      return consumer <- complete()
    }
    to abort(problem :Any) :Vow[Void] {
      return consumer <- abort(problem)
    }
  }
  return str_src <- run(my_sink)
}
def parseBin(bufferIn :BytesSrc, numBytes :Nat, consumer :Sink) :Vow[Void] {
  def bin_src := makeBytesBufferSrc(bufferIn, numBytes)
  object my_sink {
    to run(bytes :Bytes) :Vow[Void] {
      object bin_wrap {
        to kind () :Any { return "msgpck_Binary" }
        to get () :Any { return bytes }
      }
      return consumer <- run(bin_wrap)
    }
    to complete() :Vow[Void] {
      return consumer <- complete()
    }
    to abort(problem :Any) :Vow[Void] {
      return consumer <- abort(problem)
    }
  }
  return bin_src <- run(my_sink)
}
def parseExt(bufferIn :BytesSrc, numBytes :Nat, extHandler :Any, consumer :Sink) :Vow[Void] {
  object my_first_sink {
    to run (byte :Bytes[1]) :Vow[Void] {
      def extNr := byte.asInteger()
      def ext_src := makeBytesBufferSrc(bufferIn, numBytes)
      object my_second_sink {
        to run (bytes :Bytes) :Vow[Void] {
          return extHandler(extNr, bytes, consumer)
        }
        to complete () :Vow[Void] {
          return consumer <- complete()
        }
        to abort (problem :Any) :Vow[Void] {
          return consumer <- abort(problem)
        }
      }
      return ext_src <- run(my_second_sink)
    }
    to complete () :Vow[Void] {
      return conumser <- complete()
    }
    to abort (problem :Any) :Vow[Void] {
      return consumer <- abort(problem)
    }
  }
  return bufferIn <- run(my_first_sink)
}

def parseMap(parserSrc :MsgpckParserSrc, numElements :Nat, consumer :Sink) :Vow[Void] {
  def map := [].asMap().diverge()
  var elementsLeft := numElements
  object my_key_sink {
    to run(key :Any) :Vow[Void] {
      object my_value_sink {
        to run(value :Any) :Vow[Void] {
          map[key] := value
          elementsLeft -= 1
          if (elementsLeft > 0) {
            return parserSrc <- run(my_key_sink)
          } else {
            def theMap := map.snapshot()
            object map_wrap {
              to kind () :Any { return "msgpck_Map" }
              to get ()  :Any { return theMap }
            }
            return consumer <- run(map_wrap)
          }
        }
        to complete() :Vow[Void] {
          return consumer <- complete()
        }
        to abort(problem :Any) :Vow[Void] {
          return consumer <- abort(problem)
        }
      }
      return parserSrc <- run(my_value_sink)
    }
    to complete() :Vow[Void] {
      return consumer <- complete()
    }
    to abort(problem :Any) :Vow[Void] {
      return consumer <- abort(problem)
    }
  }
  return parserSrc <- run(my_key_sink)
}
def parseArray (parserSrc :MsgpckParserSrc, numElements :Nat, consumer :Sink) :Vow[Void] {
  def arr := [].diverge()
  var elementsLeft := numElements
  object my_item_sink {
    to run(item :Any) {
      arr.push(item)
      elementsLeft -= 1
      if (elementsLeft > 0) {
        return parserSrc <- run(my_item_sink)
      } else {
        def theArr := arr.snapshot()
        object arr_warp {
          to kind() :Any { return "msgpck_Array" }
          to get () :Any ( return theArr }
        }
        return consumer <- run(arr_wrap)
      }
    }
    to complete() :Vow[Void] {
      return consumer <- complete()
    }
    to abort(problem :Any) :Vow[Void] {
      return consumer <- abort(problem)
    }
  }
}

object makeMsgpckParserSrc {
  to run(byteSrc :ByteSrc, extHandler :Any) :Src {
    object my_src {
      to run(consumer :Sink) {
        object my_sink {
          to run(byte :Byte) :Vow[Void] {
            if ((byte & 0x80) == 0x00) {
              # positive fixint
              return consumer <- run(makeInteger(byte & 0x7f))
            } elseif ((byte & 0xE0) == 0xE0) {
              # negative fixint
              return consumer <- run(makeInteger((byte & 0x1f).negate()))
            } else {
              if ((byte & 0xF0) == 0x80) {
                # fixmap
                def numberOfElements := (byte & 0x0F).asInteger()
                return parseMap(my_src, numberOfElements, consumer)
              } elseif ((byte & 0xF0) == 0x90) {
                # fixarray
                def numberOfElements := (byte & 0x0F).asInteger()
                return parseArray(my_src, numberOfElements, consumer)
              } else {
                switch (byte) {
                  match ==0xC0 {
                    # nil
                    return consumer <- run(makeNil())
                  }
                  match ==0xC1 {
                    # never used
                    return consumer <- abort("came across a type value of 0xC1")
                  }
                  match ==0xC2 {
                    # false
                    return consumer <- run(makeBool(false))
                  }
                  match ==0xC3 {
                    # true
                    return consumer <- run(makeBool(true))
                  }
                  match ==0xC4 {
                    # bin 8
                    object my_bin_8_sink {
                      to run(baeti :Byte) :Vow[Void] {
                        def numberOfBytes := baeti.asInteger()
                        return parseBin(byteSrc, numberOfBytes, consumer)
                      }
                      to complete() :Vow[Void] {
                        return consumer <- complete()
                      }
                      to abort(problem :Any) :Vow[Void] {
                        return consumer <- abort(problem)
                      }
                    }
                    return byteSrc <- run(my_bin_8_sink)
                  }
                  match ==0xC5 {
                    # bin 16
                    def bin_len_src := makeBytesBufferSrc(byteSrc, 2)
                    object my_bin_16_sink {
                      to run(bytes :Bytes) :Vow[Void] {
                        def numberOfBytes := bytes.asInteger()
                        return parseBin(byteSrc, numberOfBytes, consumer)
                      }
                      to complete() :Vow[Void] {
                        return consumer <- complete()
                      }
                      to abort(problem :Any) :Vow[Void] {
                        return consumer <- abort(problem)
                      }
                    }
                    return bin_len_src <- run(my_bin_16_sink)
                  }
                  match ==0xC6 {
                    # bin 32
                    def bin_len_src := makeBytesBufferSrc(byteSrc, 4)
                    object my_bin_32_sink {
                      to run(bytes :Bytes) :Vow[Void] {
                        def numberOfBytes := bytes.asInteger()
                        return parseBin(byteSrc, numberOfBytes, consumer)
                      }
                      to complete() :Vow[Void] {
                        return consumer <- complete()
                      }
                      to abort(problem :Any) :Vow[Void] {
                        return consumer <- abort(problem)
                      }
                    }
                    return bin_len_src <- run(my_bin_32_sink)
                  }
                  match ==0xC7 {
                    # ext 8
                    object my_ext_8_sink {
                      to run(baeti :Bytes[1]) {
                        def numberOfBytes := baeti.asInteger()
                        return parseExt(byteSrc, numberOfBytes, extHandler, consumer)
                      }
                      to complete() :Vow[Void] {
                        return consumer <- complete()
                      }
                      to abort(problem :Any) :Vow[Void] {
                        return consumer <- abort(problem)
                      }
                    }
                    return byteSrc <- run(my_ext_8_sink)
                  }
                  match ==0xC8 {
                    # ext 16
                    def ext_len_src := makeBytesBufferSrc(byteSrc, 2)
                    object my_ext_16_sink {
                      to run(bytes :Bytes[2]) :Vow[Void] {
                        def numberOfBytes := bytes.asInteger()
                        return parseExt(byteSrc, numberOfBytes, extHandler, consumer)
                      }
                      to complete() :Vow[Void] {
                        return consumer <- complete()
                      }
                      to abort(problem :Any) :Vow[Void] {
                        return consumer <- abort(problem)
                      }
                    }
                    return ext_len_src <- run(my_ext_16_sink)
                  }
                  match ==0xC9 {
                    # ext 32
                    def ext_len_src := makeBytesBufferSrc(byteSrc, 4)
                    object my_ext_32_sink {
                      to run(bytes :Bytes[4]) :Vow[Void] {
                        def numberOFBytes := bytes.asInteger()
                        return parseExt(byteSrc, numberOfBytes, extHandler, consumer)
                      }
                      to complete() :Vow[Void] {
                        return consumer <- complete()
                      }
                      to abort(problem :Any) :Vow[Void] {
                        return consumer <- abort(problem)
                      }
                    }
                    return ext_len_src <- run(my_ext_32_sink)
                  }
                  match ==0xCA {
                    # float32
                    def bytes_src := makeBytesBufferSrc(byteSrc, 4)
                    object my_float32_sink {
                      to run(bytes :Bytes[4]) :Vow[Void] {
                        return consumer <- run(makeFloat32(bytes))
                      }
                      to complete() :Vow[Void] {
                        return consumer <- complete()
                      }
                      to abort(problem :Any) :Vow[Void] {
                        return consumer <- abort(problem)
                      }
                    }
                    return bytes_src <- run(my_float32_sink)
                  }
                  match ==0xCB {
                    # float 64
                    def bytes_src := makeBytesBufferSrc(byteSrc, 8)
                    object my_float64_sink {
                      to run(bytes :Bytes[8]) :Vow[Void] {
                        return consumer <- run(makeFloat64(bytes))
                      }
                      to complete() :Vow[Void] {
                        return consumer <- complete()
                      }
                      to abort(problem :Any) :Vow[Void] {
                        return consumer <- abort(problem)
                      }
                    }
                    return bytes_src <- run(my_float64_sink)
                  }
                  match ==0xCC {
                    # uint8
                    object my_uint_8_sink {
                      to run(byte :Bytes[1]) {
                        return consumer <- run(makeUint(byte))
                      }
                      to complete() :Vow[Void] {
                        return consumer <- complete()
                      }
                      to abort(problem :Any) :Vow[Void] {
                        return consumer <- abort(problem)
                      }
                    }
                    return byteSrc <- run(my_uint_8_sink)
                  }
                  match ==0xCD {
                    # uint16
                    def bytes_src := makeBytesBufferSrc(byteSrc, 2)
                    object my_uint_16_sink {
                      to run(bytes :Bytes[2]) :Vow[Void] {
                        return consumer <- run(makeUint(bytes))
                      }
                      to complete() :Vow[Void] {
                        return consumer <- complete()
                      }
                      to abort(problem :Any) :Vow[Void] {
                        return consumer <- abort(problem)
                      }
                    }
                    return bytes_src <- run(my_uint_16_sink)
                  }
                  match ==0xCE {
                    # uint32
                    def bytes_src := makeBytesBufferSrc(byteSrc, 4)
                    object my_uint_32_sink {
                      to run(bytes :Bytes[2]) :Vow[Void] {
                        return consumer <- run(makeUint(bytes))
                      }
                      to complete() :Vow[Void] {
                        return consumer <- complete()
                      }
                      to abort(problem :Any) :Vow[Void] {
                        return consumer <- abort(problem)
                      }                    
                    }
                    return bytes_src <- run(my_uint_32_sink)
                  }
                  match ==0xCF {
                    # uint64
                    def bytes_src := makeBytesBufferSrc(byteSrc, 8)
                    object my_uint_64_sink {
                      to run(bytes :Bytes[2]) :Vow[Void] {
                        return consumer <- run(makeUint(bytes))
                      }
                      to complete() :Vow[Void] {
                        return consumer <- complete()
                      }
                      to abort(problem :Any) :Vow[Void] {
                        return consumer <- abort(problem)
                      }                    
                    }
                    return bytes_src <- run(my_uint_64_sink)
                  }
                  match ==0xD0 {
                    # int 8
                    object my_int_8_sink {
                      to run(byte :Bytes[1]) :Vow[Void] {
                        return consumer <- run(makeInt(byte))
                      }
                      to complete() :Vow[Void] {
                        return consumer <- complete()
                      }
                      to abort(problem :Any) :Vow[Void] {
                        return consumer <- abort(problem)
                      }
                    }
                    return byteSrc <- run(my_int_8_sink)
                  }
                  match ==0xD1 {
                    # int 16
                    def bytes_src := makeBytesBufferSrc(byteSrc, 2)
                    object my_int_16_sink {
                      to run(bytes :Bytes[2]) :Vow[Void] {
                        return consumer <- run(makeInt(bytes))
                      }
                      to complete() :Vow[Void] {
                        return consumer <- complete()
                      }
                      to abort(problem :Any) :Vow[Void] {
                        return consumer <- abort(problem)
                      }
                    }
                    return bytes_src <- run(my_int_16_sink)
                  }
                  match ==0xD2 {
                    # int 32
                    def bytes_src := makeBytesBufferSrc(byteSrc, 4)
                    object my_int_32_sink {
                      to run(bytes :Bytes[4]) :Vow[Void] {
                        return consumer <- run(makeInt(bytes))
                      }
                      to complete() :Vow[Void] {
                        return consumer <- complete()
                      }
                      to abort(problem :Any) :Vow[Void] {
                        return consumer <- abort(problem)
                      }
                    }
                    return bytes_src <- run(my_int_32_sink)
                  }
                  match ==0xD3 {
                    # int 64
                    def bytes_src := makeBytesBufferSrc(byteSrc, 8)
                    object my_int_64_sink {
                      to run(bytes :Bytes[8]) :Vow[Void] {
                        return consumer <- run(makeInt(bytes))
                      }
                      to complete() :Vow[Void] {
                        return consumer <- complete()
                      }
                      to abort(problem :Any) :Vow[Void] {
                        return consumer <- abort(problem)
                      }
                    }
                    return bytes_src <- run(my_int_64_sink)
                  }
                  match ==0xD4 {
                    # fixext 1
                    # takes 2 bytes
                    return parseExt(byteSrc, 2, extHandler, consumer)
                  }
                  match ==0xD5 {
                    # fixext 2
                    # takes 3 bytes
                    return parseExt(byteSrc, 3, extHandler, consumer)
                  }
                  match ==0xD6 {
                    # fixext 4
                    # takes 5 bytes
                    return parseExt(byteSrc, 5, extHandler, consumer)
                  }
                  match ==0xD7 {
                    # fixext 8
                    # takes 9 bytes
                    return parseExt(byteSrc, 9, extHandler, consumer)
                  }
                  match ==0xD8 {
                    # fixext 16
                    # takes 17 bytes
                    return parseExt(byteSrc, 17, extHandler, consumer)
                  }
                  match ==0xD9 {
                    # str 8
                    object my_str_8_sink {
                      to run(byte :Bytes[1]) :Vow[Void] {
                        def numberOfBytes := byte.asInteger()
                        return parseString(byteSrc, numberOfBytes, consumer)
                      }
                      to complete() :Vow[Void] {
                        return consumer <- complete()
                      }
                      to abort(problem :Any) :Vow[Void] {
                        return consumer <- abort(problem)
                      }
                    }
                    return byteSrc <- run(my_str_8_sink)
                  }
                  match ==0xDA {
                    # str 16
                    def bytes_src := makeBytesBufferSrc(byteSrc, 2)
                    object my_str_16_sink {
                      to run(bytes :Bytes[2]) :Vow[Void] {
                        def numberOfBytes := byte.asInteger()
                        return parseString(byteSrc, numberOfBytes, consumer)
                      }
                      to complete() :Vow[Void] {
                        return consumer <- complete()
                      }
                      to abort(problem :Any) :Vow[Void] {
                        return consumer <- abort(problem)
                      }
                    }
                    return bytes_src <- run(my_str_16_sink) 
                  }
                  match ==0xDB {
                    # str 32
                    def bytes_src := makeBytesBufferSrc(byteSrc, 4)
                    object my_str_32_sink {
                      to run(bytes :Bytes[4]) :Vow[Void] {
                        def numberOfBytes := byte.asInteger()
                        return parseString(byteSrc, numberOfBytes, consumer)
                      }
                      to complete() :Vow[Void] {
                        return consumer <- complete()
                      }
                      to abort(problem :Any) :Vow[Void] {
                        return consumer <- abort(problem)
                      }
                    }
                    return bytes_src <- run(my_str_32_sink)                     
                  }
                  match ==0xDC {
                    # array 16
                    def bytes_src := makeBytesBufferSrc(byteSrc, 2)
                    object my_array_16_sink {
                      to run(bytes :Bytes[2]) :Vow[Void] {
                        def numberOfElements := byte.asInteger()
                        return parseArray(my_src, numberOfElements, consumer)
                      }
                      to complete() :Vow[Void] {
                        return consumer <- complete()
                      }
                      to abort(problem :Any) :Vow[Void] {
                        return consumer <- abort(problem)
                      }
                    }
                    return bytes_src <- run(my_array_16_sink)                     
                  }
                  match ==0xDD {
                    # array 32
                    def bytes_src := makeBytesBufferSrc(byteSrc, 4)
                    object my_array_32_sink {
                      to run(bytes :Bytes[4]) :Vow[Void] {
                        def numberOfElements := byte.asInteger()
                        return parseArray(my_src, numberOfElements, consumer)
                      }
                      to complete() :Vow[Void] {
                        return consumer <- complete()
                      }
                      to abort(problem :Any) :Vow[Void] {
                        return consumer <- abort(problem)
                      }
                    }
                    return bytes_src <- run(my_array_32_sink)                     
                  }
                  match ==0xDE {
                    # map 16
                    def bytes_src := makeBytesBufferSrc(byteSrc, 2)
                    object my_map_16_sink {
                      to run(bytes :Bytes[2]) :Vow[Void] {
                        def numberOfElements := byte.asInteger()
                        return parseMap(my_src, numberOfElements, consumer)
                      }
                      to complete() :Vow[Void] {
                        return consumer <- complete()
                      }
                      to abort(problem :Any) :Vow[Void] {
                        return consumer <- abort(problem)
                      }
                    }
                    return bytes_src <- run(my_map_16_sink)                     
                  }  
                  match ==0xDF {
                    # map 32
                    def bytes_src := makeBytesBufferSrc(byteSrc, 4)
                    object my_map_32_sink {
                      to run(bytes :Bytes[4]) :Vow[Void] {
                        def numberOfElements := byte.asInteger()
                        return parseMap(my_src, numberOfElements, consumer)
                      }
                      to complete() :Vow[Void] {
                        return consumer <- complete()
                      }
                      to abort(problem :Any) :Vow[Void] {
                        return consumer <- abort(problem)
                      }
                    }
                    return bytes_src <- run(my_map_32_sink)           
                  }
                }
              }
            }
          }
        }
        return byteSrc <- run(my_first_sink)
      }
    }
    return my_src
  }
}

object msgpckKit {
  to recognize(bytesSrc) {
    
    def ibids  := [].asMap().diverge()
    def extHandler (extNr :Integer, bufferIn :Bytes, consumer :Sink) :Vow[Void] {
      def byte_src := makeByteSrcFromBuffer(bufferIn)
      def parserSrc := makeMsgpckParserSrc(byte_src, extHandler)
      switch (extNr) {
        match ==0 {
          # letrc
          var ibidnr :Msgpck(uint)
          var item   :Any
          object my_letrc_ibid_item_sink {
            to run(items) :Vow[Void] {
              ibidnr := items[0]
              item   := items[1]
              if (ibids[ibidnr] != null) {
                return consumer <- abort("ibidnr already in use!")
              } else {
                def [promise, resolver] := Ref.promise()
                ibids[ibidnr] := promise
                resolver.resolve(item)
                return consumer <- run(item)
              }
            }
            to complete() :Vow[Void] { return consumer <- complete() }
            to abort(problem :Any) :Vow[Void] { return consumer <- abort(problem) }
          }
          def ii_buff := makeBufferSrc(parserSrc, 2)
          return ii_buff <- run(my_letrc_ibid_item_sink)
          # if (consumed_i == 0) { throw.throw(ejector, "zero sized ibidnr!") }
          # if (buffer.size() != 0) { throw.throw(ejector, "only two things should be inside of an letrc") }
        }
        match ==1 {
          # ibid
          object my_ibid_sink {
            to run(ibidnr :Msgpck["uint"]) :Vow[Void] {
              return consumer <- run(ibids[ibidnr])
            }
            to complete() :Vow[Void] { return consumer <- complete() }
            to abort(problem :Any) :Vow[Void] { return consumer <- abort(problem) }
          }
          return parserSrc <- run(my_ibid_sink)
          # if (buffer.size() != 0) { throw.throw(ejector, "only one thing should be inside of an ibid") }
        }
        match ==2 {
          # DeliverOnly
          var recipiant :Any := null
          var verb   :Msgpck["utf8Str"]
          var args   :Msgpck["array"]
          var kwargs :Msgpck["map"]
          object my_DeliverOnly_done {
            to run(kwargs_in :Any[Msgpck["map"], Any]) :Vow[Void] {
              kwargs := kwargs_in
              def DeliverOnly := [recipiant, verb, args, kwargs]
              return consumer <- run(object {
                to kind () :Any { return "DeliverOnly" }
                to get ()  :Any { return DeliverOnly }
              })
            }
            to complete() :Vow[Void] { return consumer <- complete() }
            to abort(problem :Any) :Vow[Void] { return consumer <- abort(problem) }
          }
          object my_DeliverOnly_recipiant_verb_args_sink {
            to run(items) :Vow[Void] {
              recipiant := item[0]
              verb      := item[1]
              args      := item[2]
              if (byteSrc.leftover()) {
                return parserSrc <- run(my_DeliverOnly_done)
              } else {
                return my_DeliverOnly_done(msgpck_empty_map)
              }
            }
            to complete() :Vow[Void] { return consumer <- complete() }
            to abort(problem :Any) :Vow[Void] { return consumer <- abort(problem) }
          }
          def rva_buff := makeBufferSrc(parserSrc, 3)
          return rva_buff <- run(my_DeliverOnly_recipiant_verb_args_sink)
        }
        match ==3 {
          # Deliver
          var answer_pos :Msgpck["uint"]
          var rdr        :Any
          var recipiant  :Any
          var verb       :Msgpck["utf8Str"]
          var args       :Msgpck["array"]
          var kwargs     :Msgpck["map"]
          object my_Deliver_done {
            to run (kwargs_in :Any[Msgpck["map"], Any]) :Vow[Void] {
              kwargs := kwargs_in
              def Deliver := [answer_pos, rdr, recipiant, verb, args, kwargs]
              return consumer <- run(object {
                to kind () :Any { return "Deliver" }
                to get ()  :Any { return Deliver }
              })
            }
            to complete() :Vow[Void] { return consumer <- complete() }
            to abort(problem :Any) :Vow[Void] { return consumer <- abort(problem) }
          }
          object my_Deliver_answerPos_rd_rec_verb_args_sink {
            to run(item) :Vow[Void] {
              answer_pos := item[0]
              rdr        := item[1]
              recipiant  := item[2]
              verb       := item[3]
              args       := item[4]
              if (byteSrc.leftover()) {
                return parserSrc <- run(my_Deliver_Done)
              } else {
                return my_Deliver_Done(msgpck_empty_map)
              }
            }
            to complete() :Vow[Void] { return consumer <- complete() }
            to abort(problem :Any) :Vow[Void] { return consumer <- abort(problem) }          
          }
          def arrva_buff := makeBufferSrc(parserSrc, 5)
          return arrva_buff <- run(my_Deliver_anserPos_rdr_rec_verb_args_sink)
        }
        match ==4 {
          # GCExport
          var export_pos :Msgpck["uint"]
          var wireDelta  :Msgpck["uint"]
          object my_GCExport_exportPos_wireDelta_sink {
            to run(items) :Vow[Void] {
              export_pos := items[0]
              wireDelta  := items[1]
              def GCExport := [export_pos, wireDelta]
              return consumer <- run(object {
                to kind () :Any { return "GCExport" }
                to get ()  :Any { return GCExport }
              })
            }
            to complete() :Vow[Void] { return consumer <- complete() }
            to abort(problem :Any) :Vow[Void] { return consumer <- abort(problem) }
          }
          def ew_buff := makeBufferSrc(parserSrc, 2)
          return ew_buff <- run(my_GCExport_exportPost_wireDelta_sink)
          # if (buffer.size() != 0) { throw.throw(ejector, "only two things should be in a GCExport!") }
        }
        match ==5 {
          # GCAnswer
          object my_GCAnswer_answerPos_sink {
            to run(answer_pos :Msgpck["uint"]) :Vow[Void] {
              def GCAnswer := [answer_pos]
              return consumer <- run(object {
                to kind () :Any { return "GCAnswer" }
                to get ()  :Any { return GCAnswer }
              })
            }
            to complete() :Vow[Void] { return consumer <- complete() }
            to abort(problem :Any) :Vow[Void] { return consumer <- abort(problem) }
          }
          return parserSrc <- run(my_GCAnswer_answerPos_sink)
          # if (buffer.size() != 0) { throw.throw(ejector, "only on thing should be in a GCAnswer!") }
        }
        match ==6 {
          # Shutdown
          object my_Shutdown_sink {
            to run(recieved_count :Msgpck["uint"]) :Vow[Void] {
              def Shutdown := [recieved_count]
              return consumer <- run(object {
                to kind () :Any { return "Shutdown" }
                to get ()  :Any { return Shutdown }
              }
            }
            to complete() :Vow[Void] { return consumer <- complete() }
            to abort(problem :Any) :Vow[Void] { return consumer <- abort(problem) }
          }
          return parserSrc <- run(my_Shutdown_sink)
          # if (buffer.size() != 0) { throw.throw(ejector, "only on thing should be in a Shutdown!") }
        }
        match ==7 {
          # Terminated
          object my_Terminated_sink {
            to run(problemo :Any) :Vow[Void] {
              def Terminated := [problemo]
              return consumer <- run(object {
                to kind () :Any { return "Terminated" }
                to get ()  :Any { return Terminated }
              })
            }
            to complete() :Vow[Void] { return consumer <- complete() }
            to abort(problem :Any) :Vow[Void] { return consumer <- abort(problem) }
          }
          return parserSrc <- run(my_Terminated_sink)
          # if (buffer.size() != 0) {  throw.throw(ejector, "only on thing should be in a Terminated!") }
        }
        match ==8 {
          # Export
          object my_Export_sink {
            to run(export_pos :Msgpck["uint"]) :Vow[Void] {
              def Exported := [export_pos]
              return consumer <- run(object {
                to kind () :Any { return "Export" }
                to get ()  :Any { return Exported }
              })              
            }
            to complete() :Vow[Void] { return consumer <- complete() }
            to abort(problem :Any) :Vow[Void] { return consumer <- abort(problem) }
          }
          return parserSrc <- run(my_Export_sink)
          # if (buffer.size() != 0) {  throw.throw(ejector, "only on thing should be in an Export!") }
        }
        match ==9 {
          # Answer
          object my_Answer_sink {
            to run(answer_pos :Msgpck["uint"]) :Vow[Void] {
             def Answer := [answer_pos]
             return consumer <- run(object {
               to kind () :Any { return "Answer" }
               to get ()  :Any { return Answer }
             })
            }
            to complete() :Vow[Void] { return consumer <- complete() }
            to abort(problem :Any) :Vow[Void] { return consumer <- abort(problem) }
          }
          return parserSrc <- run(my_Answer_sink)
          # if (buffer.size() != 0) { throw.throw(ejector, "only on thing should be in an Answer!") }
        }
        match ==10 {
          # Import
          object my_Import_sink {
            to run(import_pos :Msgpck["uint"]) :Vow[Void] {
              def Imported := [import_pos]
              return consumer <- run (object {
                to kind () :Any { return "Import" }
                to get ()  :Any { return Imported }
              })
            }
            to complete() :Vow[Void] { return consumer <- complete() }
            to abort(problem :Any) :Vow[Void] { return consumer <- abort(problem) }
          }
          return parserSrc <- run(my_Import_sink)
          # if (buffer.size() != 0) { throw.throw(ejector, "only on thing should be in an Import!") }
        }
        match ==11 {
          # Question
          object my_Question_sink {
            to run (question_pos :Msgpck["uint"]) :Vow[Void] {
              def Question := [question_pos]
              return consumer <- run (object {
                to kind () :Any { return "Question" }
                to get ()  :Any { return Question }
              })
            }
            to complete() :Vow[Void] { return consumer <- complete() }
            to abort(problem :Any) :Vow[Void] { return consumer <- abort(problem) }          
          }
          return parserSrc <- run(my_Question_sink)
          # if (buffer.size() != 0) { throw.throw(ejector, "only on thing should be in an Question!") }
        }
        match ==12 {
          # newFarDesc
          var import_pos :Msgpck["uint"]
          var swissHash :Msgpck["Sha256_cryptohash"] # this could be optional, havent updated the spec though
          object my_newFarDesc_importPos_hash_sink {
            to run(items) :Vow[Void] {
              import_pos := items[0]
              swissHash  := items[1]
              def newFarDesc := [import_pos, swissHash]
              return consumer <- run(object {
                to kind () :Any { return "newFarDesc" }
                to get ()  :Any { return newFarDesc }
              })
            }
            to complete() :Vow[Void] { return consumer <- complete() }
            to abort(problem :Any) :Vow[Void] { return consumer <- abort(problem) }          
          }
          def ih_buff := makeBufferSrc(parserSrc, 2)
          return ih_buff <- run(my_newFarDesc_importPost_hash_sink)
          # if (buffer.size() != 0) { throw.throw(ejector, "only one or two things should be in a newFarDesc!") }
        }
        match ==13 {
          # newRemotePromiseDesc
          def [consumed_imp, import_pos] := msgpckParser.parse(buffer, ejector, extHandler)
          if (consumed_imp == 0) { throw.throw(ejector, "zero sized import pos!") }
          if (import_pos.kind() != "msgpck_uint") { throw.throw(ejector, "import pos is not a number!") }
          buffer := buffer.slice(consumed_imp, buffer.size())
          def [consumed_rdr, rdr_pos] := msgpckParser.parse(buffer, ejector, extHandler)
          if (consumed_rdr == 0) { throw.throw(ejector, "zero sized rdr pos!") }
          if (rdr_pos.kind() != "msgpck_uint") { throw.throw(ejector, "rdr pos is not a number!") }
          buffer := buffer.slice(consumed_rdr, buffer.size())
          def [consumed_rdrBase, rdrBase] := msgpckParser.parse(buffer, ejector, extHandler)
          if (consumed_rdrBase == 0) { throw.throw(ejector, "zero sized rdrBase!") }
          if (rdrBase.kind() != "msgpck_Binary") { throw.throw(ejector, "rdrBase is not a Binary!") }
          buffer := buffer.slice(consumed_rdrBase, buffer.size())
          if (buffer.size() != 0) { throw.throw(ejector, "only three things should be in a newRemotePromiseDesc!") }
          def newRemotePromiseDesc := [import_pos, rdr_pos, rdrBase]
          return object {
            to kind () :Any { return "newRemotePromiseDesc" }
            to get ()  :Any { return newRemotePromiseDesc }
          }
        }
        match ==14 {
          # newPromise3Desc
          def [consumed_sp, searchPath] := msgpckParser.parse(buffer, ejector, extHandler)
          if (consumed_sp == 0) { throw.throw(ejector, "zero sized search path! (should be at least be an empty array)") }
          if (searchPath.kind() != "msgpck_Array") { throw.throw(ejector, "search path must be an array!") }
          buffer := buffer.slice(consumed_sp, buffer.size())
          def [consumed_hostId, hostId] := msgpckParser.parse(buffer, ejector, extHandler)
          if (consumed_hostId == 0) { throw.throw(ejector, "zero sized host VatId!") }
          # todo: add other kinds of cryptohashes
          if (hostId.kind() != "Sha256_cryptohash") { throw.throw(ejector, "VatID is not a Sha256 hash!") }
          buffer := buffer.slice(consumed_hostId, buffer.size())
          def [consumed_nonce, nonce] := msgpckParser.parse(buffer, ejector, extHandler)
          if (consumed_nonce == 0) { throw.throw(ejector, "zero sized nonce!") }
          if (nonce.kind() != "msgpck_Binary") { throw.throw(ejector, "nonce is not a binary string!") }
          buffer := buffer.slice(consumed_nonce, buffer.size())
          def [consumed_vine, vine] := msgpckParser.parse(buffer, ejector, extHandler)
          if (consumed_vine == 0) { throw.throw(ejector, "zero sized vine!") }
          # a vine can be anything.
          buffer := buffer.slice(consumed_vine, buffer.size())
          if (buffer.size() != 0) { throw.throw(ejector, "only four things should be in in a newPromise3Desc") }
          def newPromise3Desc := [searchPath, hostId, nonce, vine]
          return object {
            to kind () :Any { return "newPromise3Desc" }
            to get ()  :Any { return newPromise3Desc }
          }
        }
        match ==15 {
          # RemoteDeliver
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
          def RemoteDeliver := [recipiant, verb, args, kwargs]
          return object {
            to kind () :Any { return "RemoteDeliver" }
            to get ()  :Any { return RemoteDeliver }
          }
        }
        match ==16 {
          # RemoteCall
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
          def RemoteCall := [recipiant, verb, args, kwargs]
          return object {
            to kind () :Any { return "RemoteCall" }
            to get ()  :Any { return RemoteCall }
          }
        }
        match ==17 {
          # LocatorUnumDesc
          if (buffer.size() != 0) { throw.throw(ejector, "LocatorUnumDesc should be empty!") }
        }
        match ==18 {
          # SturdyRef
          def [consumed_loc, locator] := msgpckParser.parse(buffer, ejector, extHandler)
          if (consumed_loc == 0) { throw.throw(ejector, "zero sized locator!") }
          buffer := buffer.slice(consumed_loc, buffer.size())
          def [consumed_sp, searchPath] := msgpckParser.parse(buffer, ejector, extHandler)
          if (consumed_sp == 0) { throw.throw(ejector, "zero sized search path! (should be at least be an empty array)") }
          if (searchPath.kind() != "msgpck_Array") { throw.throw(ejector, "search path must be an array!") }
          buffer := buffer.slice(consumed_sp, buffer.size())
          def [consumed_hostId, hostId] := msgpckParser.parse(buffer, ejector, extHandler)
          if (consumed_hostId == 0) { throw.throw(ejector, "zero sized host VatId!") }
          # todo: add other kinds of cryptohashes
          if (hostId.kind() != "Sha256_cryptohash") { throw.throw(ejector, "VatID is not a Sha256 hash!") }
          buffer := buffer.slice(consumed_hostId, buffer.size())
          def [consumed_sn, swissNum] := msgpckParser.parse(buffer, ejector, extHandler)
          if (consumed_sn == 0) { throw.throw(ejector, "zero sized swissNum!") }
          # todo: add other kinds of cryptohashes
          if (swissNum.kind() != "Sha256_cryptohash") { throw.throw(ejector, "swissNum is not a Sha256 hash!") }
          buffer := buffer.slice(consumed_sn, buffer.size())
          var expiration :Any
          if (buffer.size() > 0) {
            def [consumed_exp, exp] := msgpckParser.parse(buffer, ejector, extHandler)
            buffer := buffer.slice(consumed_exp, buffer.size())
            expiration := object {
              to kind () :Any { return "ISO8601_Date" }
              to get ()  :Any { return exp }
            }
          } else {
            expiration := object {
              to kind () :Any { return "ISO8601_Date" }
              to get ()  :Any { return "empty" }
            }
          }
          if (buffer.size() != 0) { throw.throw(ejector, "there should be three or four things in an sturdyref!") }
          def sturdyref := [locator, searchPath, hostId, swissNum, expiration]
          return object {
            to kind () :Any { return "SturdyRef" }
            to get ()  :Any { return sturdyref }
          }
        }
        match ==19 {
          # crypto hash. (SHA256)
          if (buffer.size() != 32) { throw.throw(ejector, "bytes inside an SHA256 cryptohash mush be 32") }
          return object {
            to kind () :Any { return "Sha256_cryptohash" }
            to get ()  :Any { return bytes }
          }
        }
        match ==20 {
          # ActiveCapCert
          throw.throw(ejector, "reserved for ActiveCapCert")
        }
        match ==21 {
          # PostalRefACC
          throw.throw(ejector, "reserved for PostalRefs that use ActiveCapCerts as carrier")
        }
        match ==22 {
          # Macaroony
          throw.throw(ejector, "reserved for Macaroons or Macaroonesque things")
        }
        match ==23 {
          # PostalRefM
          throw.throw(ejector, "reserved for PostalRefs that use Macaroons or Macaroonesque things as carrier")
        }
        match ==24 {
          # crypto hash (Blake2b)
          throw.throw(ejector, "reserved for Blake2b cryptographic hashes")
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

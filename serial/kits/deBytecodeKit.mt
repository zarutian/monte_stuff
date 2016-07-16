import "lib/tubes" => [ => Drain ]
exports(deBytecodeKit)

def OP_ROOT         := 1
def OP_LIT_WHOLENUM := 2
def OP_LIT_NEGINT   := 3
def OP_LIT_FLOAT64  := 4
def OP_LIT_CHAR     := 5
def OP_LIT_STRING   := 6
def OP_IMPORT       := 7
def OP_IBID         := 8
def OP_CALL         := 9
def OP_DEFINE       := 10
def OP_PROMISE      := 11
def OP_DEFREC       := 12


def read_WHOLENUM (state, byte) :stateAndInt {
  def done? := ((byte & 0x80) != 0x80)
  def value := (byte & 0x7ff)
  state["last_WHOLENUM"] := (state["last_WHOLENUM"] << 7) + value
  if (done?) {
    def returned_to := state["continuation_rstack"].pop()
    return returned_to(state, b``)
  } else {
    return [state, 1]
  }
}
def read_UTF_length (state, bytes) :stateAndInt {
  state["continuation"] := state["continuation_rstack"].pop()
  return [state, bytes.asInt()]
}
def read_UTF_bytes (state, bytes) :stateAndInt {
  state["continuation_pstack"].push(bytes.asUTF())
  def returned_to := state["continuation_rstack"].pop()
  return returned_to(state, b``)
}

def got_OP_byte (state, byte) :stateAndInt {
  var newSize := 1
  state["continuation_rstack"].push(get_OP_byte)
  switch (byte) {
    match ==OP_ROOT {
      state["continuation_rstack"].push(done)
      return pop_ROOT(state, b``)
    }
    match ==OP_LIT_WHOLENUM {
      state["continuation_rstack"].push(push_LIT_WHOLENUM)
      state["last_WHOLENUM"] := 0
      state["continuation"] := read_WHOLENUM
    }
    match ==OP_LIT_NEGINT {
      state["continuation_rstack"].push(push_LIT_NEGINT)
      state["last_WHOLENUM"] := 0
      state["continuation"] := read_WHOLENUM
    }
    match ==OP_LIT_FLOAT64 {
      state["continuation"] := push_LIT_FLOAT64
      newSize := 8
    }
    match ==OP_LIT_CHAR {
      state["continuation"] := push_LIT_CHAR
    }
    match ==OP_LIT_STRING {
      state["continuation_rstack"].push(push_LIT_STRING)
      state["continuation_rstack"].push(read_UTF_bytes)
      state["continuation"] := read_UTF_length
      newSize := 2
    }
    match ==OP_IMPORT {
      state["continuation_rstack"].push(push_IMPORT)
      state["continuation_rstack"].push(read_UTF_bytes)
      state["continuation"] := read_UTF_length
      newSize := 2
    }
    match ==OP_IBID {
      state["continuation_rstack"].push(push_IBID)
      state["last_WHOLENUM"] := 0
      state["continuation"] := read_WHOLENUM
    }
    match ==OP_CALL {
      state["continuation_rstack"].push(do_a_Call)
      state["continuation_rstack"].push(read_WHOLENUM)
      state["continuation_rstack"].push(read_UTF_bytes)
      state["last_WHOLENUM"] := 0
      state["continuation"] := read_UTF_length
      newSize := 2
    }
    match ==OP_DEFINE {
      return do_a_define(state, b``)
    }
    match ==OP_PROMISE {
      return make_a_promise(state, b``)
    }
    match ==OP_DEFREC {
      state["continuation_rstack"].push(do_a_DEFREC)
      state["last_WHOLENUM"] := 0
      state["continuation"] := read_WHOLENUM
    }
    match :Any {
      throw("error text still in TBD")
    }
  }
  return [state, newSize]
}

object deBytecodeMachine {
  to getStateGuard () :Any {
    return Any
  }
  to getInitialState () :stateAndInt {
    def init_state := [].asMap().diverge()
    init_state["continuation"] := got_OP_byte
    init_state["continuation_rstack"] := [].diverge()
    init_state["continuation_pstack"] := [].diverge()
    return [init_state, 1]
  }
  to advance (prior_state, data) :stateAndInt {
    return prior_state["continuation"](prior_state, data)
  }
  to results() :Any {
  
  }
}



object deBytecodeKit {
  to makeBuilder () :Near {
  
  }
  to makeStreamBulder(dos :DataOutputStream) :Near {
    var nextTemp := 0
    object deBytecodeBuilder implements DEBuilderOf(Void, Void) {
      to getNodeType() :Near { return Void }
      to getRootType() :Near { return Void }
      to buildRoot(_) :any {
        dos.writeByte(OP_ROOT)
        dos.flush()
      }
      to buildLiteral(value) :Void {
        switch (value) {
          match i :Int {
            if (i >= 0) {
              dos.writeByte(OP_LIT_WHOLENUM)
              dos.writeWholeNum(i)
            } else {
              dos.writeByte(OP_LIT_NEGINT)
              dos.writeWholeNum(-i)
            }
          }
          match f :Double {
            dos.writeByte(OP_LIT_FLOAT64)
            dos.writeDouble(f)
          }
          match c :Char {
            dos.writeByte(OP_LIT_CHAR)
            dos.writeChar(c)
          }
          match str :Str {
            dos.writeByte(OP_LIT_STRING)
            dos.writeUTF(str)
          }
        }
      }
      to buildImport(varName :Str) :Void {
        dos.writeByte(OP_IMPORT)
        dos.writeUTF(varName)
      }
      to buildIbid(tempIndex :Int) :Void {
        dos.writeByte(OP_IBID)
        dos.writeWholeNum(tempIndex)
      }
      to buildCall(recipiant :Any, verb :String, args :List[Void]) :Void {
        dos.writeByte(OP_CALL)
        dos.writeUTF(verb)
        dos.writeWholeNum(args.size())
      }
      to buildDefine(_) :Tuple[Void, Int] {
        def tempIndex := nextTemp
        nextTemp += 1
        dos.writeByte(OP_DEFINE)
        return [null, tempIndex]
      }
      to buildPromise() :Int {
        def promIndex := nextTemp
        def resIndex := promIndex + 1
        nextTemp += 2
        dos.writeByte(OP_PROMISE)
        return promIndex
      }
      to buildDefrec(resIndex :Int, _) :Void {
        dos.writeByte(OP_DEFREC)
        dos.writeWholeNum(resIndex)
      }
    }
    return deBytecodeBuilder
  }
  
  to recognize(depiction :Bytes, builder) :(builder.getRootType()) {
    def bais := makeByteArrayInputStream(depiction)
    def dis := makeDataInputStream(bais)
    deBytecodeKit.recognizeStream(dis, builder)
  }
  
  to recognizeStream(dis :DataInputStream, builder) :(builder.getRootType()) {
    # to be filled in
  }
}
return deBytecodeKit

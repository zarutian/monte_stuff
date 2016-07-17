import "lib/tubes" =~ [ => Fount, => Drain ]
import "guards" =~ [ => Tuple, => Nat ]
import "lib/codec/utf8" =~ [=> UTF8]
import "serial/streams" =~ [=> DataOutputStream, => makeBytestringOutputStream, => makeDataOutputStream, => makeBytestringFount ]
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


def parse_WHOLENUM (buffer :Bytes) :Tuple[Nat, Any] {
  var idx := 0
  var value := 0
  while (true) {
    if (idx >= buffer.size()) { return [0, null] }
    def byte := buffer[idx]
    value := (value << 7) + (byte & 0x7f)
    idx += 1
    if (byte & 0x80) == 0x80) { return [idx, value] }
  }
}
def parse_UTF8str (buffer :Bytes) :Tuple[Nat, Any] {
  if (buffer.size() < 2) { return [0, null] }
  def length := (buffer.slice(0, 1)).asInt() ; # a Short in network byte order
  if (buffer.size() < 2 + length) { return [0, null] }
  def string := UTF8.decode(buffer.slice(2, (2 + length)), null)
  return [(2 + length), string]
}
def parse_FLOAT64 (buffer :Bytes) :Tuple[Nat, Any] {
  if (buffer.size() < 8) { return [0, null] }
  return [8, buffer.slice(0, 7).asDouble()]
}


object deBytecodeKit {
  to makeBuilder () :Near {
    def bsos := makeBytestringOutputStream()
    def dos := makeDataOutputStream(baos)
    def subBuilder := deBytecodeKit.makeStreamBuilder(dos)
    def wrappingBuilder extends subBuilder implements DEBuilderOf(Void, Bytes) {
      to getRootType() :Near { return Bytes }
      to buildRoot(root) :Bytes {
        super.buildRoot(root)
        dos.close()
        return bsos.toBytestring()
      }
    }
    return wrappingBulder
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
  
  to recognize(depiction :Bytes, builder) :Vow[builder.getRootType()] {
    def fount := makeBytestringFount(depiction)
    return deBytecodeKit.recognizeStream(fount, builder)
  }
  
  to recognizeStream(F :Fount[Bytes], builder) :Vow[builder.getRootType()] {
    def [root_promise, resolver] := Ref.makePromise()
    def stack := [].diverge()
    def buffer := b``
    object drain as Drain {
      to recieve (data :Bytes) {
        buffer += data
        do {
          if (buffer.size() < 1) { return }
          switch (buffer[0]) {
            match ==OP_ROOT {
              resolver.resolve(builder.buildRoot(stack.pop()))
            }
            match ==OP_LIT_WHOLENUM {
              def [consumed, wholenum] := parse_WHOLENUM(buffer.slice(1, (buffer.size() - 1)))
              if (consumed == 0) { return }
              # commit point
              buffer := buffer.slice((1 + consumed), (buffer.size() - 1))
              stack.push(builder.buildLiteral(wholenum))
            }
            match ==OP_LIT_NEGINT {
              def [consumed, wholenum] := parse_WHOLENUM(buffer.slice(1, (buffer.size() - 1)))
              if (consumed == 0) { return }
              # commit point
              buffer := buffer.slice((1 + consumed), (buffer.size() - 1))
              stack.push(builder.buildLiteral(-wholenum))
            }
            match ==OP_LIT_FLOAT64 {
              def [consumed, slumptala] := parse_FLOAT64(buffer.slice(1, (buffer.size() - 1)))
              if (consumed == 0) { return }
              # commit point
              buffer := buffer.slice((1 + consumed), (buffer.size() - 1))
              stack.push(builder.buildLiteral(slumptala))
            }
            match ==OP_LIT_CHAR {
              def [consumed, shady_character] := parse_CHAR(buffer.slice(1, (buffer.size() - 1)))
              if (consumed == 0) { return }
              # commit point
              buffer := buffer.slice((1 + consumed), (buffer.size() - 1))
              stack.push(builder.buildLiteral(shady_character))
            }
            match ==OP_LIT_STRING {
              def [consumed, handy_string] := parse_UTF8str(buffer.slice(1, (buffer.size() - 1)))
              if (consumed == 0) { return }
              # commit point
              buffer := buffer.slice((1 + consumed), (buffer.size() - 1))
              stack.push(builder.buildLiteral(handy_string))
            }
            match ==OP_IMPORT {
              def [consumed, handy_string] := parse_UTF8str(buffer.slice(1, (buffer.size() - 1)))
              if (consumed == 0) { return }
              # commit point
              buffer := buffer.slice((1 + consumed), (buffer.size() - 1))
              stack.push(builder.buildImport(handy_string))
            }
            match ==OP_IBID {
              def [consumed, ibid] := parse_WHOLENUM(buffer.slice(1, (buffer.size() - 1)))
              if (consumed == 0) { return }
              # commit point
              buffer := buffer.slice((1 + consumed), (buffer.size() - 1))
              stack.push(builder.buildIbid(ibid))
            }
            match ==OP_CALL {
              def [consumed1, verb]  := parse_UTF8str(buffer.slice(1, (buffer.size() - 1)))
              if (consumed1 == 0) { return }
              def [consumed2, arity] := parse_WHOLENUM(buffer.slice((1 + consumed1), (buffer.size() - 1)))
              if (consumed2 == 0) { return }
              # commit point
              buffer := buffer.slice((1 + consumed1 + consumed2), (buffer.size() - 1))
              def stackSize := stack.size()
              def firstArgIndex := stackSize - arity
              def args := stack.removeRun(firstArgIndex, stackSize)
              def rec := stack.pop()
              stack.push(builder.buildCall(rec, verb, args))
            }
            match ==OP_DEFINE {
              stack.push(builder.buildDefine(stack.pop())[0])
            }
            match ==OP_PROMISE {
              builder.buildPromise()
            }
            match ==OP_DEFREC {
              def [consumed, wholenum] := parse_WHOLENUM(buffer.slice(1, (buffer.size() - 1)))
              if (consumed == 0) { return }
              # commit point
              buffer := buffer.slice((1 + consumed), (buffer.size() - 1))
              stack.push(builder.buildDefrec(wholenum, stack.pop()))
            }
          }
        } until (buffer.size() == 0)
      }
    }
    F.flowInto(drain)
    return root_promise
  }
}
return deBytecodeKit

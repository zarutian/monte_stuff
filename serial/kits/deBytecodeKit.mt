import "serial/streams" =~ [ => DataOutputStream, 
                             => DataInputStream,
                             => makeByteArrayInputStream,
                             => makeDataInputStream]
export(deBytecodeKit)

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

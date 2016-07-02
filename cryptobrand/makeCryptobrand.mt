
import "tables/makeEphimeronTable" =~ [=> makeEphimeronTable]
import "serial/importer" =~ [=> importer]
export(makeCryptobrand)

def quine := "
def quine := \"$quine\"
object makeCryptobrand {
  to run(brandName :Str) {
    return run(brandName, null, null)
  }
  to run(brandName :Str, initPubKey :NullOk[Any], initPrivKey :NullOk[Any]) {
    def box2thing := makeEphimeronTable()
    var [pubKey, privKey] := [initPubKey, initPrivKey]
    
    object sealer {
      to run(thing :Any) :Any {
        object box {
          to _uncall() :Any {
            initCryptokeys()
            return [makeCryptobrand, "makeBox", [brandName, serializeAndEncrypt(box2thing[box])], [].asMap()]
          }
          to _printOn(printer) {
            printer.print("<Box of $brand>")
          }
        }
        box2thing[box] := thing
        return box
      }
      to _uncall() :Any {
        initCryptokeys()
        return [makeCryptobrand, "makeSealer", [brandName, pubKey], [].asMap()]
      }
    }
    
    object unsealer {
      to run(box) :Any {
        return box2thing[box]
      }
      to _uncall() :Any {
        initCryptokeys()
        return [makeCryptobrand, "makeUnsealer", [brandName, privKey], [].asMap()]
      }
    }
    
    return [sealer, unsealer]
  }
  to _uncall () :Any {
  
  }
}"

eval(quine, safeScope)
return makeCryptobrand

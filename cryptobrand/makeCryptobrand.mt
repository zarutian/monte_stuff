
import "tables/makeEphimeronTable" =~ [=> makeEphimeronTable]
import "serial/importer" =~ [=> importer]
export(makeCryptobrand)

def quine := `
def quine := "$quine"
def cryptobrands := [].asMap().diverge()
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
    
    object privateAccessor {
      to getSealer()    :Any  { return sealer }
      to getUnsealer()  :Any  { return unsealer }
      to putPubkey(pk)  :Void { pubKey := pk }
      to putPrivkey(pk) :Void { privKey := pk }
    }
    cryptobrands[brandName] = privateAccessor
    
    return [sealer, unsealer]
  }
  to makeSealer (brandName :Str, pubKey) {
    if (cryptobrands[brandName] == null) {
      makeCryptobrand(brandName, pubKey, null)
    } else {
      cryptobrands[brandName].putPubkey(pubKey)
    }
    return cryptobrands[brandName].getSealer()
  }
  to makeUnsealer (brandName :Str, privKey) {
    if (cryptobrands[brandName] == null) {
      makeCryptobrand(brandName, null, privKey)
    } else {
      cryptobrands[brandName].putPrivkey(privKey)
    }
    return cryptobrands[brandName].getUnsealer()
  }
  to makeBox (brandName :Str, encryptedDepiction :Bytes) {
  }
  to _uncall () :Any {
  
  }
}`

eval(quine, safeScope)
return makeCryptobrand

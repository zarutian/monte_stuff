
import "tables/makeEphimeronTable" =~ [=> makeEphimeronTable_original]
import "serial/importer" =~ [=> importer_original]
export(makeCryptobrand)

def quine := `
def importer := $importer_original
def makeEphimeronTable := importer("tables/makeEphimeronTable")
def keyMaker           := importer("crypt").keyMaker
def quine_str := "$quine"

def cryptobrands := [].asMap().diverge()

object makeCryptobrand {
  to run(brandName :Str) {
    return run(brandName, null, null)
  }
  to run(brandName :Str, initPubKey :NullOk[Any], initPrivKey :NullOk[Any]) {
    def box2thing := makeEphimeronTable()
    
    var [pubKey, privKey] := [initPubKey, initPrivKey]
    def initCryptoKeys() :Void {
      if ((pubKey != null) || (privKey != null)) {
        return
      }
      def [pubK1, privK1] := keyMaker(); # encryption keypair
      def [pubK2, privK2] := keyMaker(); # authentication keypair
      pubKey  := ["sodium_publickey", pubK1.asBytes(), privK2.asBytes()]
      privKey := ["sodium_privatekey", privK1.asBytes(), pubK2.asBytes()]
    }
    def serializeAndEncrypt(thing) {
      if (pubKey == null) {
        throw("no public key aviable!")
      }
      
      def surgeon := theSurgeon.diverge()
      def exits := [].asMap().diverge()
      var count := 0
      # need an uncaller at the bottom of the uncallers list,
      # that uncaller just adds things that cant be serialized by
      # this surgeon as depiction graph exits
      
      # look into <opaque>
      object gunga {
        to Uncall(specimen :Any) :Any {
          def name := "internal_exit" + counter
          counter += 1
          surgeon.addExit(specimen, name)
          exits[name] := specimen
          return surgeon.serialize(specimen)
        }
      }
      surgeon.addLastUncaller(gunga)
      def depiction := surgeon.serialize(thing)
      
      def pubK1  := keyMaker.fromPublicBytes(pubKey[1])
      def privK2 := keyMaker.fromSecretBytes(pubKey[2])
      def keypair := privK2.pairWith(pubK1)
      return [keypair.seal(depiction), exits]
    }
    def decryptAndDeserialize(encryptedDepictionWithExits :List) {
      def [encryptedDepiction :List, depictionExits :Map[Str, Any]] := encryptedDepictionWithExits
      if (privKey == null) {
        throw("no private key aviable!")
      }
      
      def pubK2  := keyMaker.fromPublicBytes(privKey[2])
      def privK1 := keyMaker.fromSecretBytes(privKey[1])
      def keypair := privK1.pairWith(pubK2)
      
      def depiction := keypair.unseal(encryptedDepiction[0], encryptedDepiction[1])
      
      def surgeon := theSurgeon.diverge()
      # add in the exits
      return surgeon.deserialize(depiction)
    }
    
    object sealer {
      to run(thing :Any) :Any {
        object box {
          to encrypted() :Bool {
            return false
          }
          to _uncall() :Any {
            initCryptokeys()
            return [makeCryptobrand, "makeBox", [brandName, serializeAndEncrypt(thing)], [].asMap()]
          }
          to _printOn(printer) {
            printer.print("<Box of $brand cryptobrand>")
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
        if (box.encrypted()) {
          def [_, verb, [bn, encryptedDepictionWithExits], _] := box._uncall()
          if (bn != brandName) {
            throw(`not an encrypted box of cryptobrand $brandName`)
          }
          return decryptAndDeserialize(encryptedDepictionWithExits)
        } else {
          return box2thing[box]
        }
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
  to makeBox (brandName :Str, encryptedDepictionWithExits :List) {
    def [encryptedDepiction :Bytes, depictionExists :Map[Str, Any]] := encryptedDepictionWithExits
    object box {
      to encrypted :Bool {
        return true
      }
      to _uncall() {
        return [makeCryptobrand, "makeBox", [brandName, encryptedDepictionWithExits], [].asMap()]
      }
      to _printOn(printer) {
        printer.print("<Encrypted Box of $brand cryptobrand>")
      }
    }
    return box
  }
  to _uncall () :Any {
    object q {
      to _uncall() {
        return [simple, "valueMaker", [quine_str], [].asMap()]
      }
    }
    return [eval, "run", [q, safeScope], [].asMap()]
  }
}`

eval(quine.substitute([=>quine]), safeScope)
return makeCryptobrand

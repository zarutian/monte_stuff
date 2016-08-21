
# this uses the streams instead of tubes.

import "../streams/bufferedStream.mt" =~ [ => makeBufferedSink ]
import "../streams/mappingSink.mt" =~ [ => makeMappingSink ]

def LIST_old   := 0x80
def INT        := 0x81
def STRING     := 0x82
def NEG        := 0x83
def FLOAT      := 0x84
def OLDLONGINT := 0x85
def OLDLONGNEG := 0x86
def VOCAB      := 0x87
def OPEN       := 0x88
def CLOSE      := 0x89
def ABORT      := 0x8A
def LONGINT    := 0x8B
def LONGNEG    := 0x8C
def ERROR      := 0x8D
def PING       := 0x8E
def PONG       := 0x8F

def fromLittleEndianBase128toNatural (input :Bytes) :Nat {
  var acc := 0
  var idx := 0
  while (idx < input.size()) {
    acc := acc + (input[idx] << (idx * 7))
    idx += 1
  }
  return acc
}

# makes an sink that takes in stream of :Bytes and sinks whole banana tokens (still :Bytes though but broken up) onwards
def makeBananaTokensSink_Bytes (onward :Sink) :Sink {
  def completeTo (buffer :Bytes) :Nat {
    # this is a pure function
    def buf_size = buffer.size()
    if (buf_size == 0) { return 0 }
    var header := b``
    var idx := 0
    var type_found :Bool := false
    while (idx < buf_size) {
      if ((buffer[idx] & 0x80) == 0x80) {
        type_found := true
        break
      } else {
        header += buffer[idx]
        idx += 1
      }
    }
    # for the purpose of offsetting an off by one errors below when idx is being used as length
    idx += 1
    if (type_found == false) { return 0 }
    switch (buffer[idx]) {
      match ==LIST_old { return idx }
      match ==INT      { return idx }
      match ==STRING {
        def length := fromLittleEndianBase128toNatural(header)
        return (idx + length)
      }
      match ==NEG      { return idx }
      match ==FLOAT    { return (idx + 8) }
      match ==OLDLONGINT { return idx }
      match ==OLDLONGNEG { return idx }
      match ==VOCAB    { return idx }
      match ==OPEN     { return idx }
      match ==CLOSE    { return idx }
      match ==ABORT    { return idx }
      match ==LONGINT  {
        def length := fromLittleEndianBase128toNatural(header)
        return (idx + length)
      }
      match ==LONGNEG  {
        def length := fromLittleEndianBase128toNatural(header)
        return (idx + length)
      }
      match ==ERROR {
        def length := fromLittleEndianBase128toNatural(header)
        return (idx + length)
      }
      match ==PING { return idx }
      match ==PONG { return idx }
      default {
        throw.throw("Unknown banana token type")
      }
    }
    return 0
  }
  return makeBufferedSink(b``, completeTo, onward)
}
def bananaTokenTuple := Tuple[Nat, Bytes[1], Bytes]
# makes an sink that takes :Bytes and sinks tuples of the form [header, type, contents] onwards
def makeBananaTokensSink (onward :Sink) :Sink[Bytes] {
  def tupler (packet :Bytes) :bananaTokenTuple {
    def size = packet.size()
    if (size == 0) { throw.throw("an empty packet was recieved") }
    var header := b``
    var type_byte := b``
    var idx := 0
    var type_found :Bool := false
    while (idx < size) {
      if ((packet[idx] & 0x80) == 0x80) {
        type_found := true
        type_byte  := packet[idx]
        idx += 1
        break
      } else {
        header += packet[idx]
        idx += 1
      }
    }
    def body := packet.slice(idx, size)
    return [fromLittleEndianBase128toNatural(header), type_byte, body]
  }
  return makeBananaTokensSink_Bytes(makeMappingSink(tupler, onward))
}

# these should live in their own file(s) probably

def makeUnicode_unslicer (protocol) :Unslicer {
  def opentype := "unicode"
  var daStr := null
  return object as Unslicer {
    "Unicode_unslicer (OPEN(unicode) STRING(<str>) CLOSE)"
    to start(count :Nat) :Void {}
    to checkToken(typebyte :Bytes[1], size :Nat) :Void {
      if ((typebyte != STRING) && (typebyte != VOCAB)) {
        throw.throw("BananaError: Unicode_unslicer only accepts strings")
      }
    }
    to receiveChild(obj :Any, ready_promise :NullOk[Promise]) :Void {
      assert(!Ref.isPromise(obj))
      assert(ready_promise == null)
      if (daStr != null) {
        throw.throw("BananaError: already received a string")
      }
      daStr := unicode.decode(obj, "utf-8")
    }
    to receiveClose() :Tuple[Any, Any] {
      return [daStr, null]
    }
    
    to doOpen(opentype :List[Str]) :Unslicer {}
  }
}
def makeList_unslicer (protocol) :Unslicer {
  def opentype := "list"
  def list := [].diverge()
  def ready_promises := [].diverge()
  return object as Unslicer {
    "List_unslicer (OPEN(list) value* CLOSE)"
    to start(count :Nat) :Void {}
    to checkToken(typebyte :Bytes[1], size :Nat) :Void {}
    to receiveChild(obj :Any, ready_promise :NullOk[Promise]) :Void {
      if (ready_promise != null) {
        ready_promise.push(ready_promise)
      }
      if (Ref.isPromise(obj)) {
        def idx := list.size()
        list.push("This very string is a placeholder!")
        when (obj -> o) {
          list[idx] := o
        } catch (problem) {
        
        }
      } else {
        list.push(obj)
      }
    }
    to receiveClose() :Tuple[Any, Any] {
      var ready_promise := null
      if (ready_promises.size() > 0) {
        ready_promise := asyncAnd(ready_promises)
      }
      return [list, ready_promise]
    }
    
    to doOpen(opentype :List[Str]) :Unslicer {
      return protocol.open(opentype)
    }
  }
}
def makeTuple_unslicer (protocol) :Unslicer {
  def opentype := "tuple"
  var list := [].diverge()
  var ready_promises := [].diverge()
  var unfinished_childs :Nat := 0
  var finished :Boolean := false
  var [promise, resolver] := Ref.newPromise()
  return object as Unslicer {
    "Tuple_unslicer (OPEN(tuple) value* CLOSE)"
    to start(count :Nat) :Void {}
    to checkToken(typebyte :Bytes[1], size :Nat) :Void {}
    to receiveChild(obj :Any, ready_promise :NullOk[Promise]) :Void {
      if (ready_promise != null) {
        ready_promises.push(ready_promise)
      }
      if (Ref.isPromise(obj)) {
        def idx := list.size()
        list.push("placeholder")
        unfinished_childs += 1
        when (obj -> o) {
          list[idx] := o
        } catch (problem) {
        
        }
      } else {
        list.push(obj)
      }
    }
    to receiveClose() :Tuple[Any, Any] {
      finished := true
      if (unfinished_childs > 0) {
        var ready_promise := null
        if (ready_promises.size() > 0) {
          ready_promise := asyncAnd(ready_promises)
          when (ready_promise -> p) {
            resolver.resolve(makeTuple(list))
          } catch (problem) {
            resolver.smash(problem)
          }
        }
        return [promise, ready_promise]
      }
      return [makeTuple(list), null]
    }
    
    to doOpen(opentype :List[Str]) :Unslicer {
      return protocol.open(opentype)
    }
  }
}
def makeDict_unslicer (protocol) :Unslicer {
  def opentype := "dict"
  var gettingKey :Boolean := true
  var key :Any := null
  var ready_promises := [].diverge()
  var m :Map := [].asMap().diverge()
  return object as Unslicer {
    "Dict_unslicer (OPEN(dict) (key, value)* CLOSE)"
    to start(count :Nat) :Void {}
    to checkToken(typebyte :Bytes[1], size :Nat) :Void {}
    to receiveChild(obj :Any, ready_promise :NullOk[Promise]) :Void {
      if (ready_promise != null) {
        ready_promises.push(ready_promise)
      }
      if (gettingKey == true) {
        key := obj
      } else {
        m[key] := obj
      }
      gettingKey := gettingKey.not()
    }
    to recieveClose() :Tuple[Any, Any] {
      var ready_promise := null
      if (ready_promises.size() > 0) {
        ready_promise := asyncAnd(ready_promises)
      }
      return [m, ready_promise]
    }
    
    to doOpen(opentype :List[Str]) :Unslicer {
      return protocol.open(opentype)
    }
  }
}
def makeNone_unslicer (protocol) :Unslicer {
  def opentype := "none"
  return object as Unslicer {
    "None_unslicer (OPEN(none) CLOSE)"
    to start(count :Nat) :Void {}
    to checkToken(typebyte :Bytes[1], size :Nat) :Void {
      throw.throw("BananaError: None_unslicer does not accept any tokens")
    }
    to receiveChild(obj :Any, ready_promise :NullOk[Promise]) :Void {}
    to receiveClose() :Tuple[Any, Any] {
      return [null, null]
    }
    
    to doOpen(opentype :List[Str]) :Unslicer {}
  }
}
def makeBoolean_unslicer (protocol) :Unslicer {
  def opentype := "boolean"
  var value := null
  return object as Unslicer {
    "Boolean_unslicer (OPEN(boolean) (INT(0) | INT(1)) CLOSE)"
    to start(count :Nat) :Void {}
    to checkToken(typebyte :Bytes[1], size :Nat) :Void {
    
    }
    to receiveChild(obj :Any, ready_promise :NullOk[Promise]) :Void {
      assert(!Ref.isPromise(obj))
      assert(ready_promise == null)
      assert(Ref.isInteger(obj))
      value := if (obj == 1) { true } else { false }
    }
    to receiveClose() :Tuple[Any, Any] {
      return [value, null]
    }
    
    to doOpen(opentype :List[Str]) :Unslicer {}
  }
}
def makeVocab_unslicer (protocol) :Unslicer {
  "OPEN(set-vocab) (INT(vocabNum) STRING(vocabString))* CLOSE"
  "OPEN(add-vocab) INT(vocabNum) STRING(vocabString) CLOSE"
}
def makeReference_unslicer (protocol) :Unslicer {
  def opentype := "reference"
  var finished := false
  var daObj := null
  return object as Unslicer {
    "Reference_unslicer (OPEN(reference) INT(openIdNr) CLOSE)"
    to start(count :Nat) :Void {}
    to checkToken(typebyte :Bytes[1], size :Nat) :Void {
      if (typebyte != INT) {
        throw.throw("BananaError: Reference_unslicer only accepts INTs")
      }
    }
    to receiveChild(obj :Any, ready_promise :NullOk[Promise]) :Void {
      assert(!Ref.isPromise(obj))
      assert(ready_promise == null)
      if (finished) {
        throw.throw("BananaError: Reference_unslicer only accepts one int")
      }
      daObj := protocol.getObject(obj)
      finished := true
    }
    to receiveClose() :Tuple[Any, Any] {
      return [daObj, null]
    }
    
    to doOpen(opentype :List[Str]) :Unslicer { }
  }
}

def unslicers := [].asMap().diverge()
# safe unslicers
unslicers["unicode"]   = Unicode_unslicer
unslicers["list"]      = list_unslicer
unslicers["tuple"]     = tuple_unslicer
unslicers["dict"]      = dict_unslicer
unslicers["none"]      = none_unslicer
unslicers["boolean"]   = boolean_unslicer
unslicers["vocab"]     = vocabDict_unslicer
unslicers["reference"] = reference_unslicer
# 

# 'unsafe' unslicers
unslicers["instance"] = pythonInstance_unslicer
unslicers["module"]   = pythonModule_unslicer
unslicers["class"]    = pythonClassType_unslicer
unslicers["method"]   = pythonMethodType_unslicer
unslicers["function"] = pythonFunctionType_unslicer

# newpb unslicers
unslicers["call"]     = newpb_call_unslicer
unslicers["answer"]   = newpb_answer_unslicer
unslicers["error"]    = newpb_error_unslicer
unslicers["decref"]   = newpb_decref_unslicer
unslicers["my-reference"] = newpb_myref_unslicer
unslicers["your-reference"] = newpb_yourref_unslicer
unslicers["copyable"] = newpb_copyable_unslicer

#
unslicers["exit"] = exit_unslicer
# OPEN(exit) STRING(<exitName>) CLOSE

def makeUnslicerRecognizer ()[kwargs] {
  
  def unslicers  := kwargs["unslicers"]
  def rootUnslicer {
    to doOpen (opentype) :Any {
      def res := unslicers[opentype[0]]
    }
  }
  
  def vocabulary := kwargs["vocab"].diverge()
  def unvocaber (packet :bananaTokenTuple) :bananaTokenTuple {
    if (packet[1] == VOCAB) {
      def daStr := vocabulary[packet[0]]
      return [daStr.size(), STRING, daStr]
    } else {
      return packet
    }
  }

  def recognizer {
    to recognize(src :Source, builder) {
      var unslicerStack := [].diverge()
      unslicerStack.push(kwargs["root_unslicer"])
      
      var inOpen :Bool := false
      var opentype := null
      def mySink as Sink {
        to run(packet :bananaTokenTuple) {
          if (inOpen) {
            opentype.push(packet)
            def res := unslicerStack[unslicerStack.size()-1].doOpen(opentype)
            if (res != null) {
              unslicerStack.push(res)
              inOpen := false
            }
          } else (
            if (packet[1] == OPEN) {
              inOpen := true
              opentype := [].diverge()
            } else {
              # in content phase
            }
          }
        }
        to complete() :Vow[Void] {}
        to abort(problem) :Vow[Void] {}
      }
      src(makeBananaTokensSink(makeMappingSink(unvocaber, mySink)))
    }
  }
  return recognizer
}


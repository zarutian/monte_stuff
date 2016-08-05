def makeBufferedSink (startBuffer, completeTo :OneArgFunc, nextSink :Sink) :Sink {
  var buffer := startBuffer
  return object as Sink {
    to run(packet) {
      buffer += packet
      def length :Nat := completeTo(buffer)
      if (length == 0) { return }
      def segment := buffer.slice(0, length)
      buffer := buffer.slice(length, buffer.size())
      nextSink(segment)
    }
    to complete() :Vow[Void] {
      return nextSink.complete()
    }
    to abort(problem) :Vow[Void] {
      return nextSink.abort(problem)
    }
  }
}


export(Portrayal)

object Portrayal {
  to coerce (specimen, ejector) {
    throw("Not yet implemented")
  }
  to _uncall () {
    return [_makeOrderedSpace, "run", [Portrayal, "<PortrayalGuard>"], [].asMap()]
  }
  to _printOn (printer) {
    printer.print("<PortrayalGuard>")
  }
}
return Portrayal

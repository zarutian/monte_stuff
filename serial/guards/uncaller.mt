
export(Uncaller)

object Uncaller {
  to coerce(specimen, ejector) {
    if (specimen._respondsTo("Uncall", 1)) {
      return specimen
    } else {
      throw.eject(ejector, `$specimen doesnt implement Uncall/1`)
    }
  }
  to _uncall() {
    return [_makeOrderedSpace, "run", [Uncaller, "<UncallerGuard>"], [].asMap()]
  }
  to _printOn(printer) {
    printer.print("<UncallerGuard>")
  }
}
return Uncaller

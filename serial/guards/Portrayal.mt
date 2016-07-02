
export(Portrayal)

object Portrayal {
  to coerce (specimen, ejector) {
    def ej1 (reason) {
      throw.eject(ejector, `$specimen is not a List`)
    }
    def sp := List.coerce(specimen, ej1)
    if (sp.size() != 4) {
      throw.eject(ejector, `$specimen is not a List of four elements`)
    }
    def recipiant := sp[0]
    def ej2 (reason) {
      throw.eject(ejector, `$sp does not have an verb that passes :Str as second element`)
    }
    def verb := Str.coerce(sp[1], ej2)
    def ej3 (reason) {
      throw.eject(ejector, `$sp does not have an arguments List as its third element`)
    }
    def args := List.coerce(sp[2], ej3)
    def ej4 (reason) {
      throw.eject(ejector, `$sp does not have an keyword arguments Map as its fourth element`)
    }
    def kwargs := Map.coerce(sp[3], ej4)
    # Multipass!
    return specimen
  }
  to _uncall () {
    return [_makeOrderedSpace, "run", [Portrayal, "<PortrayalGuard>"], [].asMap()]
  }
  to _printOn (printer) {
    printer.print("<PortrayalGuard>")
  }
}
return Portrayal

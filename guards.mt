
exports(ZeroArgFunc, OneArgFunc, Tuple)

object OneArgFunc {
  to coerce(specimen, ejector) {
    if (specimen._respondsTo("run", 1)) {
      return specimen
    } else {
      throw.eject(ejector, `$specimen is not a One argument function`)
    }
  }
}

object ZeroArgFunc {
  to coerce(specimen, ejector) {
    if (specimen._respondsTo("run", 0)) {
      return specimen
    } else {
      throw.eject(ejector, `$specimen is not a Zero argument function`)
    }
  }
}

object Tuple {
  match (verb, Tuple_arguments) {
    if (verb == "get") {
      object guard {
        to coerce(specimen, ejector) {
          List.coerce(specimen, ejector)
          def specimen_size := specimen.size()
          if (specimen_size != Tuple_arguments.size()) {
            throw.eject(ejector, `missmatch of lengths between specimen and Tuple`)
          }
          var result := [].diverge()
          var idx := 0
          while (idx <= specimen_size) {
            result.push(Tuple_arguments[idx].coerce(specimen[idx], ejector))
            idx += 1
          }
          return result
        }
      }
    }
  }
}

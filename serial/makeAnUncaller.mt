
 # is an import require for the guards NullOk and notNull ?

import "serial/makeRemoteCall" =~ [=> makeRemoteCall]
import "serial/guards/Portrayal" =~ [=> Portrayal]
import "serial/guards/Uncaller" =~ [=> Uncaller]
export(makeAnUncaller)

object minimalUncaller {
  to Uncall(obj) :NullOk[Portrayal] {
    if (Ref.isNear(obj)) {
      return obj._uncall()
    } elseif (Ref.isBroken(obj)) {
      return [Ref, "broken", [Ref.problem(obj)]]
    } else {
      return makeRemoteCall.Uncall(obj)
    }
  }
}

object importer {
  to Uncall(obj) :NullOk[Portrayal] {
    throw("To Be Determined")
  }
}
object typer {
  to Uncall(obj) :NullOk[Portrayal] {
    throw("To Be Determined")
  }
}

def minimalUncallers := [minimalUncaller, importer]
def defaultUncallers := minimalUncallers.with(typer); # and how is this going to work?

object makeAnUncaller {
  "
  Makes an uncall function that, when applied to a transparent-enough object,
  will return the ingredients of a call expression that, when performed, will
  reconstruct a new object adequately similar to the original.
  <p>
  An uncall function is used as a component in making a subgraph recognizer,
  ie, an uneval function.
  
  @docstring_author Mark S. Miller
  "
  
  to getMinimalUncallers() :List[Uncaller] { return minimalUncallers }
  to getDefaultUncallers() :List[Uncaller] { return defaultUncallers }

  to makeAmplifier(unsealer) :Uncaller {
    object amplifier {
      to Uncall(obj) :NullOk[Portrayal] {
        if (unsealer.unseal(obj) =~ [result]) {
          return result
        } else {
          return null
        }
      }
    }
    return amplifier
  }
  to onlySelfless(baseUncallers) :Uncaller {
    object onlySelflessUncaller {
      to Uncall(obj) :NullOk[Portrayal] {
        if (Ref.isSelfless(obj)) {
          for baseUncaller in baseUncallers {
            if (baseUncaller.Uncall(obj) =~ result :notNull) {
              return result
            }
          }
        }
        return null
      }
    }
    return onlySelflessUncaller
  }
  
}

return makeAnUncaller


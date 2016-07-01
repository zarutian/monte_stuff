throw ("Not yet implemented"); # this will be removed later

import "serial/makeRemoteCall" =~ [=> makeRemoteCall]

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
    

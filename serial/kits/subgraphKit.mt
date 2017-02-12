

import 'serialize/guards/Uncaller' => [ => Uncaller]
import 'serialize/guards/Portrayal' => [ => Portrayal]
import 'guards/Near' => [ => Near]
import 'guards/CycleBreaker' => [ => CycleBreaker]
import 'utils/makeCycleBreaker' => [=> makeCycleBreaker]

def makeUnevaler (uncallerList :List[Uncaller], unscopeLayout :CycleBreaker) :Near {
  object unevaler {
    to recognize(root, builder) :(def Root := builder.getRootType()) {
      def Node := builder.getNodeType()
      def uncallers := uncallerList.snapshot()
      def unscope := unscopeLayout.snapshot()
      # Map from objects to temp indexes the builder knows.
      def temps := makeCycleBreaker().diverge()
      def generate  # Name scoped but not yet bound
      # traverse an uncall portrayal
      def genCall(rec, verb :String, args :List, kwargs :Map) :Node {
        def recExpr := generate(rec)
        var argExprs := []
        for arg in args {
          argExprs with= generate(arg)
        }
        var kwargExprs := [].asMap()
        for [k, w] in kwargs {
          kwargExprs with= [generate(k), generate(w)]
        }
        return builder.buildCall(recExpr, verb, argExprs, kwargExprs)
      }
    }
  }
  return unevaler
}

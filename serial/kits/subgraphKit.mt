

import 'serialize/guards/Uncaller' =~ [ => Uncaller]
import 'serialize/guards/Portrayal' =~ [ => Portrayal]
import 'serialize/guards/Builder' =~ [ => Builder]
import 'guards/Near' =~ [ => Near]
import 'guards/CycleBreaker' =~ [ => CycleBreaker]
import 'utils/makeCycleBreaker' =~ [=> makeCycleBreaker]

def makeUnevaler (uncallerList :List[Uncaller], unscopeLayout :CycleBreaker) :Near {
  object unevaler {
    to recognize(root :Any, builder :Builder) :(def Root := builder.getRootType()) {
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
      
      def genObject(obj :Any) :Node {
        # scalars are transparent, but can't be uncalled.
        # They are instead translated to literal expressions.
        # The scalars null, true, and false should have already
        # been picked up by the unscope -- they should be in the
        # provided unscopeLayout.
        if (obj =~ i :Int)     { return builder.buildLiteral(i) }
        if (obj =~ f :Double)  { return builder.buildLiteral(f) }
        if (obj =~ c :Char)    { return builder.buildLiteral(c) }

        # Bare strings are transparent and aren't scalars, but
        # still can't be uncalled. Instead, they are also
        # translated into literal expressions
        if (obj =~ twine :Str) {
          return builder.buildLiteral(twine)
        }
        # try one uncaller after another until a Portrayal is returned instead of null
        for uncaller in uncallers {
          if (uncaller.optUncall(obj) =~ [rec, verb, args, kwargs] :Portrayal) {
            return genCall(rec, verb, args, kwargs)
          }
        }
        throw(`Can't uneval ${M.toQuote(obj)}`)
      }

    }
  }
  return unevaler
}

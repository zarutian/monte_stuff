

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

    }
  }
  return unevaler
}

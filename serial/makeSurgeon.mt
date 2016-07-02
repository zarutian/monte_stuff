
import "serial/kits/subgraphKit" =~ [ => subgraphKit]
import "serial/kits/deBytecodeKit" =~ [ => deBytecodeKit]
export(makeSurgeon)

def argValue (optArg, func) :Any {
  if (null == optArg) {
    return func()
  } else {
    return optArg
  }
}

def makeForgivingMap (map) :Near {
  object forgivingMap extends map {
    to get (key) :Any {
      if (map.maps(key)) {
        return map[key]
      } else {
        return Ref.broken(`there is no $key`)
      }
    }
    to snapshot () :Near { return makeForgivingMap(map.snapshot()) }
    to diverge  () :Near { return makeForgivingMap(map.diverge()) }
    to readOnly () :Near { return makeForgivingMap(map.readOnly()) }
  }
  return forgivingMap
}

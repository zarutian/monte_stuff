
import "serial/kits/subgraphKit" =~ [ => subgraphKit]
import "serial/kits/deBytecodeKit" =~ [ => deBytecodeKit]
export(makeSurgeon)

def argValue(optArg, func) :Any {
  if (null == optArg) {
    return func()
  } else {
    return optArg
  }
}


import "tables/makeEphimeronTable" =~ [=> makeEphimeronTable]
export(makeCryptobrand)

def quine := "object makeCryptobrand {
  to run(brandName :Str) {
    def box2thing := makeEphimeronTable()
    
    object sealer {
      to run(thing) :Any {
      
      }
    }
    
    object unsealer {
      to run(box) :Any {
        return box2thing[box]
      }
      to _uncall() :Any {
        # to be fleshed out
      }
    }
    
    return [sealer, unsealer]
  }
}"

eval(quine, safeScope)
return makeCryptobrand


# Because 'time' here in this context is elastic and that reminded me 
# of that Dali painting "Time, the and Water, the" (ísl nafn: "Tíminn og vatnið") 

export(makeDaliTimer)

object makeDaliTimer {
  to run () :Tuple[Any, ZeroArgFunc] {
    var ticks :Nat := 0
    var schedule :Map := [].asMap().diverge()
    object Timer {
      to fromNow(delay :Nat) :Promise {
        def [promise, resolver] := Ref.makePromise()
        def after :Nat := ticks + delay
        if (schedule[after] == null) {
          schedule[after] := [].diverge()
        }
        schedule[after].push(resolver)
        return promise
      }
      to sendTimestamp (callable) :Void {
        callable <- run(ticks)
      }
    }
    def tickOnward() :Void {
      if (schedule[ticks] != null) {
        for (item in schedule[ticks]) {
          item.resolve(ticks)
        }
        schedule[ticks] := null
      }
      ticks += 1
    }
    return [Timer, tickOnward]
  }
}

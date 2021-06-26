package chisel

import ir._
import internal._
import Utils._

object setClockAndReset {
  def apply(clock: Bits, reset: Bits): Unit = {
    setClock(clock)
    setReset(reset)
  }
}

object setClock {
  def apply[T](clock: Bits): Unit =  {
    require(clock.tpe == ClockType, clock.tpe)
    Builder.currentClock = Some(clock)
  }
}

object setReset {
  def apply[T](reset: Bits): Unit = {
    require(isResetType(reset), reset.tpe)
    Builder.currentReset = Some(reset)
  }
}

object withClockAndReset {
  def apply[T](clock: Bits, reset: Bits)(block: => T): T = {
    require(clock.tpe == ClockType, clock.tpe)
    require(isResetType(reset), reset.tpe)
    // Save parentScope
    val parentClock = Builder.currentClock
    val parentReset = Builder.currentReset

    Builder.currentClock = Some(clock)
    Builder.currentReset = Some(reset)

    val res = block // execute block

    // Return to old scope
    Builder.currentClock = parentClock
    Builder.currentReset = parentReset
    res
  }
}

object withClock {
  def apply[T](clock: Bits)(block: => T): T =  {
    require(clock.tpe == ClockType, clock.tpe)
    // Save parentScope
    val parentClock = Builder.currentClock
    Builder.currentClock = Some(clock)
    val res = block // execute block
                    // Return to old scope
    Builder.currentClock = parentClock
    res
  }
}

object withReset {
  def apply[T](reset: Bits)(block: => T): T = {
    require(isResetType(reset), reset.tpe)
    // Save parentScope
    val parentReset = Builder.currentReset
    Builder.currentReset = Some(reset)
    val res = block // execute block
                    // Return to old scope
    Builder.currentReset = parentReset
    res
  }
}

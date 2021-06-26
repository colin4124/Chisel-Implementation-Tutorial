package chisel

import internal._
import internal.Builder._
import ir._

object Reg {
  def apply[T <: Bits](t: T): T = {
    requireIsChiselType(t, "reg type")
    val clock = Node(Builder.forcedClock)
    t.bind(RegBinding(Builder.forcedUserModule))
    pushStatement(DefRegister(t.ref, clock, None, None))
    t
  }
}

object RegNext {
  def apply[T <: Bits](next: T): T = {
    requireIsHardware(next, "reg next")
    val model = next.cloneType
    val reg = Reg(model)
    reg := next
    reg
  }

  def apply[T <: Bits](next: T, init: T): T = {
    requireIsHardware(next, "reg next")
    val model = next.cloneType
    val reg = RegInit(model, init)
    reg := next
    reg
  }
}

object RegInit {
  def apply[T <: Bits](reg: T, init: T): T = {
    requireIsChiselType(reg, "reg type")
    requireIsHardware(init, "reg initializer")

    val clock = Node(Builder.forcedClock)
    val reset = Node(Builder.forcedReset)

    reg.bind(RegBinding(Builder.forcedUserModule))
    pushStatement(DefRegister(reg.ref, clock, Some(reset), Some(init.ref)))
    reg
  }

  def apply[T <: Bits](init: T): T = {
    val model = init.cloneType
    RegInit(model, init)
  }
}

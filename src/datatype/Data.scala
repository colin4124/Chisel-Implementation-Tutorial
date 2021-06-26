package chisel

import ir._
import internal._
import internal.Builder._

sealed abstract class SpecifiedDirection
object SpecifiedDirection {
  case object Unspecified extends SpecifiedDirection
  case object Output      extends SpecifiedDirection
  case object Input       extends SpecifiedDirection
  case object InOut       extends SpecifiedDirection
  case object Internal    extends SpecifiedDirection

  def flip(dir: SpecifiedDirection): SpecifiedDirection = dir match {
    case Output      => Input
    case Input       => Output
    case InOut       => InOut
    case _  => error(s"use Input/Output before Flip!")
  }
}

object Input {
  def apply[T<:Bits](source: T): T = {
    source.direction = SpecifiedDirection.Input
    source
  }
}

object Output {
  def apply[T<:Bits](source: T): T = {
    source.direction = SpecifiedDirection.Output
    source
  }
}

object InOut {
  def apply[T<:Bits](source: T): T = {
    source.direction = SpecifiedDirection.InOut
    source
  }
}

abstract class Data extends HasId {
  var _binding: Option[Binding] = None

  def bindingOpt: Option[Binding] = _binding

  def binding: Binding = _binding.get
  def binding_=(target: Binding): Unit = {
    if (_binding.isDefined) {
      throw RebindingException(s"Attempted reassignment of binding to $this")
    }
    _binding = Some(target)
  }

  def bind(target: Binding): Unit

  def isSynthesizable: Boolean = _binding match {
    case Some(_) => true
    case None => false
  }

  def lref: Node = {
    requireIsHardware(this)
    bindingOpt match {
      case Some(binding: ReadOnlyBinding) => throwException(s"internal error: attempted to generate LHS ref to ReadOnlyBinding $binding")
      case Some(binding: Binding) => Node(this)
      case None => throwException(s"internal error: unbinding in generating LHS ref")
    }
  }

  def ref: Expression = {
    requireIsHardware(this)
    bindingOpt match {
      case Some(_) => Node(this)
      case None => throwException(s"internal error: unbinding in generating RHS ref")
    }
  }

}

package chisel.internal

import chisel._

object requireIsHardware {
  def apply(node: Data, msg: String = ""): Unit = {
    if (!node.isSynthesizable) {
      val prefix = if (msg.nonEmpty) s"$msg " else ""
      throw ExpectedHardwareException(s"$prefix'$node' must be hardware, " +
                                        "not a bare Chisel type. Perhaps you forgot to wrap it in Wire(_) or IO(_)?")
    }
  }
}

object requireIsChiselType {
  def apply(node: Data, msg: String = ""): Unit = if (node.isSynthesizable) {
    val prefix = if (msg.nonEmpty) s"$msg " else ""
    throw ExpectedChiselTypeException(s"$prefix'$node' must be a Chisel type, not hardware")
  }
}

sealed trait Binding {
  def location: Option[BaseModule]
}

sealed trait UnconstrainedBinding extends Binding {
  def location: Option[BaseModule] = None
}

sealed trait ConstrainedBinding extends Binding {
  def module: BaseModule
  def location: Option[BaseModule] = Some(module)
}

sealed trait ReadOnlyBinding extends Binding

case class OpBinding  (module: RawModule ) extends ConstrainedBinding with ReadOnlyBinding
case class PortBinding(module: BaseModule) extends ConstrainedBinding
case class RegBinding (module: RawModule ) extends ConstrainedBinding
case class WireBinding(module: RawModule ) extends ConstrainedBinding

case object LitBinding extends UnconstrainedBinding with ReadOnlyBinding

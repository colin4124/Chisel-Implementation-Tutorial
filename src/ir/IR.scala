package chisel.ir

import chisel._

case class Circuit(name: String, modules: Seq[Component])

case class Port(
  data : Bits,
  dir  : Direction,
) {
  def tpe: Type = data.tpe
}

abstract class Component {
  val name  : String
  val ports : Seq[Port]
}

case class DefModule(name: String, ports: Seq[Port], stmts: Seq[Statement]) extends Component

/** Primitive Operation
  */
abstract class PrimOp

abstract class Expression

case class Reference(serialize: String, tpe: Type) extends Expression

case class Node(id: Data) extends Expression

case class DoPrim(op: PrimOp, args: Seq[Expression], consts: Seq[BigInt]) extends Expression
case class ILit(n: BigInt) extends Expression

abstract class Literal extends Expression {
  val value: BigInt
  var width: Width
  def minWidth: BigInt
  def getWidth: BigInt = width match {
    case IntWidth(w)  => w
    case UnknownWidth => minWidth
  }
}

case class UIntLiteral(value: BigInt, specifiedWidth: Width) extends Literal {
  def minWidth: BigInt = 1 max value.bitLength
  def tpe = width match {
    case w: IntWidth => UIntType(w)
    case UnknownWidth => UIntType(IntWidth(minWidth))
  }

  var width: Width = specifiedWidth
}
object UIntLiteral {
  def minWidth(value: BigInt): Width = IntWidth(math.max(value.bitLength, 1))
  def apply(value: BigInt): UIntLiteral = new UIntLiteral(value, minWidth(value))
}
case class SIntLiteral(value: BigInt, w: Width) extends Literal {
  def tpe = SIntType(width)
  def minWidth: BigInt = 1 + value.bitLength
  def serialize = {
    val stringLiteral = value.toString(16)
    stringLiteral.head match {
      case '-' => s"-${getWidth}'sh${stringLiteral.tail}"
      case _   => s"${getWidth}'sh${stringLiteral}"
    }
  }
  var width: Width = w
}
object SIntLiteral {
  def minWidth(value: BigInt): Width = IntWidth(value.bitLength + 1)
  def apply(value: BigInt): SIntLiteral = new SIntLiteral(value, minWidth(value))
}

/** Statement
  */
abstract class Statement

case class DefWire(e: Expression) extends Statement

case class RegInfo(clock: Expression, reset: Option[Expression], init: Option[Expression])
case class DefRegister(expr: Expression, clock: Expression, reset: Option[Expression], init: Option[Expression]) extends Statement {
  val info = RegInfo(clock, reset, init)
}
//case class DefInstance(inst: Instance, module: String, params: Seq[Param]) extends Statement
case class Connect(loc: Expression, expr: Expression) extends Statement
case class RegInfoMap(info: Map[Expression, RegInfo]) extends Statement

/** Width
  */
abstract class Width {
  val value: BigInt
  def known: Boolean

  def op(that: Width, f: (BigInt, BigInt) => BigInt) = {
    IntWidth(f(this.value, that.value))
  }

  def max(that: Width ) = op(that, _ max _)
  def +  (that: Width ) = op(that, _  +  _)
  def +  (that: BigInt) = op(this, (a, b) => a + that)
  def shiftRight(that: BigInt) = IntWidth(BigInt(0) max (value - that))
  def dynamicShiftLeft(that: Width) =
    this.op(that, (a, b) => a + (1 << b.toInt) - 1)
}

object IntWidth {
  private val maxCached = 1024
  private val cache = new Array[IntWidth](maxCached + 1)
  def apply(width: BigInt): IntWidth = {
    if (0 <= width && width <= maxCached) {
      val i = width.toInt
      var w = cache(i)
      if (w eq null) {
        w = new IntWidth(width)
        cache(i) = w
      }
      w
    } else new IntWidth(width)
  }
  def unapply(w: IntWidth): Option[BigInt] = Some(w.value)
}
class IntWidth(val value: BigInt) extends Width {
  def known: Boolean = true
}
object UnknownWidth extends Width {
  val value: BigInt = 0
  def known: Boolean = false
}

object Width {
  def apply(x: BigInt) = IntWidth(x)
  def apply() = UnknownWidth
}

/** Type
  */
abstract class Type {
  var width: Width
}


case object UnknownType extends Type {
  var width: Width = UnknownWidth
}

case class UIntType(var width: Width) extends Type
case class SIntType(var width: Width) extends Type

case object ClockType extends Type {
  var width: Width = IntWidth(1)
}

trait ResetType extends Type {
  var width: Width = IntWidth(1)
}

case object SyncResetType     extends ResetType
case object AsyncNegResetType extends ResetType
case object AsyncPosResetType extends ResetType

/** Direction
  */
sealed abstract class Direction

case object Input  extends Direction
case object Output extends Direction
case object InOut  extends Direction

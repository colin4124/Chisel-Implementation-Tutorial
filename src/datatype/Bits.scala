
package chisel

import internal._
import internal.Builder._
import ir._
import Utils._
import ir.PrimOps._

class Bits(specifiedType: Type) extends Data {
  var tpe = specifiedType
  var direction: SpecifiedDirection = SpecifiedDirection.Internal

  var width = tpe.width
  def setWidth(w: Width ) = tpe.width = w
  def setWidth(w: BigInt) = tpe.width = IntWidth(w)

  def cloneType: this.type = new Bits(specifiedType).asInstanceOf[this.type]

  override def bind(target: Binding): Unit = {
    binding = target
  }

  def connect(that: Bits): Unit = {
    requireIsHardware(this, "data to be connected")
    requireIsHardware(that, "data to be connected")
    val (lhs_width, rhs_width) = (this.width, that.width) match {
      case (IntWidth(l_w), IntWidth(r_w)) =>
        (l_w, r_w)
      case (IntWidth(l_w), UnknownWidth) =>
        that.setWidth(l_w)
        (l_w, l_w)
      case (UnknownWidth, IntWidth(r_w)) =>
        setWidth(r_w)
        (r_w, r_w)
      case _ => error(s"Can't infer width")
    }

    require(lhs_width >= rhs_width, s"LHS's width ${lhs_width} < RHS's width ${rhs_width}")
    that._ref match {
      case Some(r) => r match {
        case ref: Literal =>
          ref.width = IntWidth(lhs_width)
          that.setRef(ref)
        case _ =>
      }
      case None =>
    }

    this.binding match {
      case _: ReadOnlyBinding => throwException(s"Cannot reassign to read-only $this")
      case _ =>  // fine
    }
    try {
      MonoConnect.connect(this, that, Builder.forcedUserModule)
    } catch {
      case MonoConnectException(message) =>
        throwException(
          s"Connection between sink ($this) and source ($that) failed @$message"
        )
    }
  }

  final def := (that: Bits): Unit = this.connect(that)

  /*
   * Operations
   */
  def unop[T <: Data](dest: T, op: PrimOp): T = {
    requireIsHardware(this, "bits operated on")
    pushOp(dest, op, this.ref)
  }

  def binop[T <: Data](dest: T, op: PrimOp, other: BigInt): T = {
   requireIsHardware(this, "bits operated on")
   pushOp(dest, op, this.ref, ILit(other))
  }

  def binop[T <: Data](dest: T, op: PrimOp, other: Bits): T = {
    requireIsHardware(this, "bits operated on")
    requireIsHardware(other, "bits operated on")
    pushOp(dest, op, this.ref, other.ref)
  }

  def compop(op: PrimOp, other: Bits): Bits = {
    binop(Bool(), op, other)
  }

  def redop(op: PrimOp): Bits = {
    unop(Bool(), op)
  }

  def tail(n: BigInt): Bits = { // UInt
   val w = width match {
     case IntWidth(x) =>
       require(x >= n, s"Can't tail($n) for width $x < $n")
       Width(x - n)
     case UnknownWidth => Width()
   }
   binop(UInt(width = w), Tail, n)
  }

  /*
   * PrimOps
   */
  def same_type_binop (that: BigInt, width: Width, op: PrimOp): Bits = {
   tpe match {
     case UIntType(_) =>
       binop(UInt(width), op, that)
     case SIntType(_) =>
       binop(SInt(width), op, that)
   }
  }

  def same_type_binop (that: Bits, width: Width, op: PrimOp): Bits = {
    require(sameType(this, that))
    tpe match {
      case UIntType(_) =>
        binop(UInt(width), op, that)
      case SIntType(_) =>
        binop(SInt(width), op, that)
    }
  }
  def same_type_compop (that: Bits, op: PrimOp): Bits = {
    require(sameType(this, that))
    compop(op , that)
  }


  def + (that: Bits): Bits =
    same_type_binop(that, this.width max that.width, Add)
  def - (that: Bits): Bits =
    same_type_binop(that, this.width max that.width, Sub)
  def & (that: Bits): Bits =
    same_type_binop(that, this.width max that.width, And)
  def | (that: Bits): Bits =
    same_type_binop(that, this.width max that.width, Or)
  def ^ (that: Bits): Bits =
    same_type_binop(that, this.width max that.width, Xor)

  def / (that: Bits): Bits =
    same_type_binop(that, this.width, Div)
  def % (that: Bits): Bits =
    same_type_binop(that, this.width, Rem)

  def <   (that: Bits): Bits = same_type_compop(that, Lt )
  def >   (that: Bits): Bits = same_type_compop(that, Gt )
  def <=  (that: Bits): Bits = same_type_compop(that, Leq)
  def >=  (that: Bits): Bits = same_type_compop(that, Geq)
  def =/= (that: Bits): Bits = same_type_compop(that, Neq)
  def === (that: Bits): Bits = same_type_compop(that, Eq )

  def || (that: Bits): Bits = { // Bool Only
    require(width.value == 1)
    require(that.width.value == 1)
    this | that
  }
  def && (that: Bits): Bits = { // Bool Only
    require(width.value == 1)
    require(that.width.value == 1)
    this & that
  }

  def << (that: Int): Bits = {
    if (that < 0) throwException(s"Negative shift amounts are illegal (got $that)")
    else if (that == 0 ) this
    else same_type_binop(that, this.width + that, Shl)
  }
  def << (that: Bits) =
    same_type_binop(that, this.width.dynamicShiftLeft(that.width), Dshl)

  def >> (that: Int): Bits = {
    if (that < 0)
      throwException(s"Negative shift amounts are illegal (got $that)")
    if (that == 0) this
    else
      tpe match {
        case UIntType(_) =>
          binop(UInt(this.width.shiftRight(that)), Shr, that)
        case SIntType(_) =>
          binop(SInt(this.width.shiftRight(that)), Shr, that)
      }
  }
  def >> (that: Bits): Bits = {
    require(sameType(this, that))
    val res_width = (this.width, that._ref) match {
      case (IntWidth(w), Some(l: Literal)) =>
        if (w < l.value) error(s"LHS'width: $l < RHS's shift amount: ${l.value}")
        else w - l.value
      case (IntWidth(w), _) => w
      case (_, _) => error(s"Not support UnknownWidth!")
    }
    tpe match {
      case UIntType(_) =>
        binop(UInt(Width(res_width)), Dshr, that)
      case SIntType(_) =>
        binop(SInt(Width(res_width)), Dshr, that)
    }
  }

  def orR : Bits = redop(Orr )
  def andR: Bits = redop(Andr)
  def xorR: Bits = redop(Xorr)

  def unary_! : Bits = this === 0.U(1.W)

  def unary_~ : Bits = {
    tpe match {
      case UIntType(_) =>
        unop(UInt(width), Not)
      case SIntType(_) =>
        unop(SInt(width), Not)
    }
  }

  def zext(): Bits = pushOp(SInt(width + 1), Cvt, ref)

  // As Type Converter
  def asUInt = tpe match {
    case _: UIntType => this
    case SIntType(w) =>
      getRef match {
        case _: UIntLiteral =>
          error(s"SIntType shouldn't contain UIntLiteral")
        case s@SIntLiteral(v, _) =>
          val ref = UIntLiteral(v + (if (v < 0) BigInt(1) << s.getWidth.toInt else 0), IntWidth(s.getWidth))
          setRef(ref)
        case _ =>
      }
      tpe = UIntType(w)
      this
  }
  def asSInt = tpe match {
    case _: SIntType => this
    case UIntType(w) =>
      getRef match {
        case u@UIntLiteral(v, _) =>
          val ref = SIntLiteral(v - ((v >> (u.getWidth.toInt - 1)) << u.getWidth.toInt), IntWidth(u.getWidth))
          setRef(ref)
        case _: SIntLiteral =>
          error(s"UIntType shouldn't contain SIntLiteral")
        case _ =>
      }
      tpe = SIntType(w)
      this
  }

  def cvt_1_bit_type (t: Type): Bits = {
    width match {
      case IntWidth(w) if w == 1 => tpe = t; this
      case _ => throwException(s"can't covert ${this.getClass.getSimpleName}$width to $t")
    }
  }
  def asBool  = cvt_1_bit_type(UIntType(width))
  def asClock = cvt_1_bit_type(ClockType      )
  def asReset = cvt_1_bit_type(SyncResetType  )
  def asAsyncPosReset = cvt_1_bit_type(AsyncPosResetType)
  def asAsyncNegReset = cvt_1_bit_type(AsyncNegResetType)
}

object UInt {
  def apply(): Bits = apply(Width())
  def apply(width: Width): Bits = new Bits(UIntType(width))

  def Lit(value: BigInt, width: Width): Bits = {
    val lit = UIntLiteral(value, width)
    val result = UInt(IntWidth(lit.getWidth))
    result.bind(LitBinding)
    result.setRef(lit)
    result
  }
}

object SInt {
  def apply(): Bits = apply(Width())
  def apply(width: Width): Bits = new Bits(SIntType(width))

  def Lit(value: BigInt, width: Width): Bits = {
    val lit = SIntLiteral(value, width)
    val result = SInt(IntWidth(lit.getWidth))
    result.bind(LitBinding)
    result.setRef(lit)
    result
  }
}

object Bool {
  def apply(): Bits = UInt(1.W)

  def Lit(x: Boolean): Bits = {
    val result = Bool()
    val lit = UIntLiteral(if (x) 1 else 0, IntWidth(1))
    result.bind(LitBinding)
    result.setRef(lit)
    result
  }
}

object Clock {
  def apply(): Bits = new Bits(ClockType)
}

object Reset {
  def apply(): Bits = new Bits(SyncResetType)
}

object AsyncPosReset {
  def apply(): Bits = new Bits(AsyncPosResetType)
}

object AsyncNegReset {
  def apply(): Bits = new Bits(AsyncNegResetType)
}

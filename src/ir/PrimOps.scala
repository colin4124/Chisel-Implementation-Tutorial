package chisel.ir

object PrimOps {
  /** Addition */
  case object Add extends PrimOp { override def toString = "add" }
  /** Subtraction */
  case object Sub extends PrimOp { override def toString = "sub" }
  /** Multiplication */
  case object Mul extends PrimOp { override def toString = "mul" }
  /** Division */
  case object Div extends PrimOp { override def toString = "div" }
  /** Remainder */
  case object Rem extends PrimOp { override def toString = "rem" }
  /** Less Than */
  case object Lt  extends PrimOp { override def toString = "lt"  }
  /** Less Than Or Equal To */
  case object Leq extends PrimOp { override def toString = "leq" }
  /** Greater Than */
  case object Gt  extends PrimOp { override def toString = "gt"  }
  /** Greater Than Or Equal To */
  case object Geq extends PrimOp { override def toString = "geq" }
  /** Equal To */
  case object Eq  extends PrimOp { override def toString = "eq"  }
  /** Not Equal To */
  case object Neq extends PrimOp { override def toString = "neq" }
  /** Interpret Casting */
  case object Cast extends PrimOp { override def toString = "Cast" }
  /** Arithmetic Convert to Signed */
  case object Cvt extends PrimOp { override def toString = "cvt" }
  /** Static Shift Left */
  case object Shl extends PrimOp { override def toString = "shl" }
  /** Static Shift Right */
  case object Shr extends PrimOp { override def toString = "shr" }
  /** Dynamic Shift Left */
  case object Dshl extends PrimOp { override def toString = "dshl" }
  /** Dynamic Shift Right */
  case object Dshr extends PrimOp { override def toString = "dshr" }
  /** Bitwise Complement */
  case object Not extends PrimOp { override def toString = "not" }
  /** Bitwise And */
  case object And extends PrimOp { override def toString = "and" }
  /** Bitwise Or */
  case object Or  extends PrimOp { override def toString = "or"  }
  /** Bitwise Exclusive Or */
  case object Xor extends PrimOp { override def toString = "xor" }
  /** Bitwise And Reduce */
  case object Andr extends PrimOp { override def toString = "andr" }
  /** Bitwise Or Reduce */
  case object Orr extends PrimOp { override def toString = "orr" }
  /** Bitwise Exclusive Or Reduce */
  case object Xorr extends PrimOp { override def toString = "xorr" }
  /** Concatenate */
  case object CatOp extends PrimOp { override def toString = "cat" }
  /** Bit Extraction */
  case object Bits extends PrimOp { override def toString = "bits" }
  /** Tail */
  case object Tail extends PrimOp { override def toString = "tail" }
}

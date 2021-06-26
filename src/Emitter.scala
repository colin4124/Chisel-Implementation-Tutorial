package chisel

import scala.collection.mutable.{ArrayBuffer, ListMap, HashMap}

import internal.Builder.error
import ir._
import ir.PrimOps._
import Utils._
import chisel.internal.Builder

object Emitter {
  def emit(circuit: Circuit): Map[String, String] = {
    (circuit.modules.flatMap {
      case m: DefModule =>
        val renderer = new VerilogRender()
        val result = renderer.emit_verilog(m)
        Some(m.name -> result)
      case m => error(s"Unknown modules: $m")
    }).toMap
  }
}

class VerilogRender() {
  val cfg_tab = "  "
  def indent(s: String, i: Int) = cfg_tab * i + s

  val result = ArrayBuffer[String]()

  def str_of_type(tpe: Type): String = {
    val wx = tpe.width.value - 1
    if (wx > 0) s"[$wx:0] " else ""
  }

  def type_of_expr(e: Expression): Type = e match {
    case Reference(_, t) => t
    case Node(id) => type_of_expr(id.getRef)
    case _ =>
      Builder.error(s"Type for $e N/A!")
  }
  def str_of_expr(e: Expression): String = e match {
    case Reference(s, _) => s
    case Node(id) =>
      str_of_expr(id.getRef)
    case d: DoPrim => str_of_op(d)
    case ILit(n) => n.toString()
    case u: UIntLiteral =>
      s"${u.getWidth}'h${u.value.toString(16)}"
    case s: SIntLiteral =>
      val stringLiteral = s.value.toString(16)
      stringLiteral.head match {
        case '-' => s"-${s.getWidth}'sh${stringLiteral.tail}"
        case _   => s"${s.getWidth}'sh${stringLiteral}"
      }
  }

  def str_of_op(doprim: DoPrim): String = {
    def is_same_op(a: Expression) = a match {
      case d: DoPrim if (d.op != doprim.op) => false
      case _ => true
    }

    def cast_as(e: Expression): String = str_of_expr(e)

    def checkArgumentLegality(e: Expression): Expression = e match {
      case (Node(id))    => checkArgumentLegality(id.getRef)
      case r: Reference  => r
      case d: DoPrim     => d
      case _: UIntLiteral | _: SIntLiteral => e
      case _ => error(s"Can't emit ${e.getClass.getName} as PrimOp argument")
    }

    val args = doprim.args map checkArgumentLegality

    def a0: Expression = args.head
    def a1: Expression = args(1)
    def a0_seq = if (is_same_op(a0)) cast_as(a0) else s"(${cast_as(a0)})"
    def a1_seq = if (is_same_op(a1)) cast_as(a1) else s"(${cast_as(a1)})"

    def c0: Int = doprim.consts.head.toInt
    def c1: Int = doprim.consts(1).toInt

    doprim.op match {
      case Add => a0_seq + " + "  + a1_seq
      case Sub => a0_seq + " - "  + a1_seq
      case Mul => a0_seq + " * "  + a1_seq
      case Div => a0_seq + " / "  + a1_seq
      case Rem => a0_seq + " % "  + a1_seq
      case Lt  => a0_seq + " < "  + a1_seq
      case Leq => a0_seq + " <= " + a1_seq
      case Gt  => a0_seq + " > "  + a1_seq
      case Geq => a0_seq + " >= " + a1_seq
      case Eq  => a0_seq + " == " + a1_seq
      case Neq => a0_seq + " != " + a1_seq
      case Dshl => a0_seq + " << " + a1_seq
      case Shl => a0_seq + " << " + s"$c0"
      case Not => "~ " + a0_seq
      case And => a0_seq + " & " + a1_seq
      case Or  => a0_seq + " | " + a1_seq
      case Xor => a0_seq + " ^ " + a1_seq
      case Xorr => "^" + a0_seq
    }
  }

  def build_ports(ports: Seq[Port], tab: Int): Seq[String]= {
    val portdefs = ArrayBuffer[String]()
    // Turn directions into strings (and AnalogType into inout)
    val dirs = ports map { case Port(_, dir) =>
      dir match {
        case ir.Input  => "input "
        case ir.Output => "output"
        case ir.InOut  => "inout"
      }
    }
    // Turn types into strings, all ports must be GroundTypes
    val tpes = ports map { p  => str_of_type(p.tpe) }

    // dirs are already padded
    (dirs, padToMax(tpes), ports).zipped.toSeq.zipWithIndex.foreach {
      case ((dir, tpe, Port(id, _)), i) =>
        val name = id.getRef match {
          case Reference(n, _) => n
          case _ => error(s"Port must be Reference ${id.getRef}")
        }
        if (i != ports.size - 1) {
          portdefs += indent(s"$dir $tpe$name,", tab)
        } else {
          portdefs += indent(s"$dir $tpe$name" , tab)
        }
    }
    portdefs
  }

  def str_of_if_block(cond: Seq[(String, String)], default: Option[String], tab: Int): Seq[String] = {
    val result = ArrayBuffer[String]()
    if (cond.size > 0) {
      result += indent(s"if (${cond(0)._1}) begin", tab)
      result += indent(s"${cond(0)._2}", tab+1)
      result ++ (cond.drop(1) flatMap { case (c, v) =>
        Seq(
          indent(s"end else if ($c) begin", tab),
          indent(s"$v", tab*2),
        )
      })
      default match {
        case Some(v) =>
          result ++= Seq(
            indent(s"end else begin", tab),
            indent(s"${v}", tab+1),
          )
        case None =>
      }
      result += indent("end", tab)
    } else {
      require(default.nonEmpty)
      result += indent(default.get, tab)
    }
    result.toSeq
  }

  def wrap_always_block(reg_info: RegInfo, block: Seq[String], tab: Int): Seq[String ] = {
    val head = reg_info match {
      case RegInfo(clk, Some(rst), _) =>
        type_of_expr(rst) match {
          case AsyncPosResetType =>
            s"always @(posedge ${str_of_expr(clk)} or posedge ${str_of_expr(rst)}) begin"
          case AsyncNegResetType =>
            s"always @(posedge ${str_of_expr(clk)} or negedge ${str_of_expr(rst)}) begin"
          case SyncResetType =>
            s"always @(posedge ${str_of_expr(clk)}) begin"
          case t =>
            error(s"Not support reset type: $rst ${t} ${str_of_expr(rst)}")
        }
      case RegInfo(clk, None, _) =>
        s"always @(posedge ${str_of_expr(clk)}) begin"
    }
    Seq(indent(head, tab)) ++ block ++ Seq(indent("end", tab))
  }

  def gen_always_block(reg: Expression, info: RegInfo, default: Option[String], tab: Int): Seq[String] = {
    val init_cond = (info.reset, info.init) match {
      case (Some(rst_val), Some(init_val)) =>
        val rst_cond = type_of_expr(rst_val) match {
          case AsyncNegResetType =>
            s"!${str_of_expr(rst_val)}"
          case _ =>
            s"${str_of_expr(rst_val)}"
        }
        val rst_conn = s"${str_of_expr(reg)} <= ${str_of_expr(init_val)};"
        Seq((rst_cond, rst_conn))
      case (Some(rst_val), _) =>
        Builder.error(s"Reset Without init value!")
      case (None, Some(init_val)) =>
        Builder.error(s"Init Without Reset value!")
      case (_, None) =>
        if (default.nonEmpty) {
          Seq()
        } else {
          Builder.error(s"Default value not exist!")
        }
    }
    str_of_if_block(init_cond, default, tab*2)
  }

  def build_streams(stmts: Seq[Statement], tab: Int): Seq[String ]= {
    val declares    = ArrayBuffer[String]()
    val assigns     = ArrayBuffer[String]()
    val reg_assigns = ArrayBuffer[String]()

    var regs_info = Map[Expression, RegInfo]()
    val regs_default = HashMap[Expression, Expression]()
    val result = ArrayBuffer[String]()

    stmts foreach {
      case DefWire(e) =>
        val tpe = str_of_type(type_of_expr(e))
        declares += s"wire ${tpe}${str_of_expr(e)};"
      case r @ DefRegister(e, _, _, _) =>
        val tpe = str_of_type(type_of_expr(e))
        declares += s"reg  ${tpe}${str_of_expr(e)};"
      case Connect(loc, e) =>
        if (regs_info.contains(loc)) {
          regs_default(loc) = e
        } else {
          assigns += s"assign ${str_of_expr(loc)} = ${str_of_expr(e)};"
        }
      case RegInfoMap(info) =>
        regs_info = info
    }

    reg_assigns ++= regs_info flatMap { case (reg, info) =>
      val default = if (regs_default.contains(reg)) {
        val conn = s"${str_of_expr(reg)} <= ${str_of_expr(regs_default(reg))};"
        Some(conn)
      } else None
      val block = gen_always_block(reg, info, default, tab)
      wrap_always_block(info, block, tab)
    }
    result ++= declares map { x => indent(x, tab) }
    result ++= assigns  map { x => indent(x, tab) }
    result ++= reg_assigns
    result
  }

  def emit_verilog(m: DefModule): String = {
    val result = ArrayBuffer[String]()
    result += s"module ${m.name} ("
    result ++= build_ports(m.ports, 1)
    result += ");"
    result ++= build_streams(m.stmts, 1)
    result += s"endmodule"
    result.mkString("\n")
  }
}


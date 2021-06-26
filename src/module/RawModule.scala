package chisel

import collection.mutable.{HashMap, ArrayBuffer}

import internal._
import ir._

abstract class RawModule extends BaseModule {
  val _stmts = ArrayBuffer[Statement]()
  def addStatement(s: Statement): Unit = {
    require(!_closed, "Can't write to module after module close")
    s match {
      case r: DefRegister =>
        pushRegInfo(r)
      case _ =>
    }
    _stmts += s
  }
  def getStatements = {
    require(_closed, "Can't get commands before module close")
    _stmts.toSeq
  }

  val _regs_info = HashMap[Expression, RegInfo]()
  def pushRegInfo(reg: DefRegister): Unit = {
    if (_regs_info.contains(reg.expr)) {
      Builder.error("Define register twice")
    } else {
      _regs_info += (reg.expr -> reg.info)
    }
  }

  def generateComponent(): Component = {
    require(!_closed, "Can't generate module more than once")
    _closed = true

    val names = nameIds(classOf[RawModule])

    namePorts(names)

    for ((node, name) <- names) {
      node.suggestName(name)
    }

    // All suggestions are in, force names to every node.
    for (id <- getIds) {
      id match {
        case id: Bits  =>
          if (id.isSynthesizable) {
            id.bindingOpt match {
              case Some(_) =>
                id.forceName(default="_T", _namespace)
              case _ =>  // don't name literals
            }
          } // else, don't name unbound types
      }
    }

    val modulePorts = getModulePorts map { port: Bits =>
      val dir = port.direction match {
        case SpecifiedDirection.Output => ir.Output
        case SpecifiedDirection.Input  => ir.Input
        case SpecifiedDirection.InOut  => ir.InOut
        case _ => Builder.error(s"Port Dir Error: ${port.direction}")
      }
      Port(port, dir)
    }

    DefModule(name, modulePorts, Seq(RegInfoMap(_regs_info.toMap)) ++ getStatements)
  }
}

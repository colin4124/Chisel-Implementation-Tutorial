package chisel.internal

import scala.util.DynamicVariable
import scala.collection.mutable.ArrayBuffer

import chisel._
import chisel.ir._

class IdGen {
  private var counter = -1L
  def next: Long = {
    counter += 1
    counter
  }
}

// Mutable global state for chisel that can appear outside a Builder context
class ChiselContext() {
  val idGen = new IdGen
}

class DynamicContext() {
  val globalNamespace = Namespace.empty
  val components = ArrayBuffer[Component]()
  var currentModule: Option[BaseModule] = None

  var readyForModuleConstr: Boolean = false
  var currentClock: Option[Bits] = None
  var currentReset: Option[Bits] = None
}

object Builder {
  // All global mutable state must be referenced via dynamicContextVar!!
  private val dynamicContextVar = new DynamicVariable[Option[DynamicContext]](None)
  private def dynamicContext: DynamicContext = {
    require(dynamicContextVar.value.isDefined, "must be inside Builder context")
    dynamicContextVar.value.get
  }

  private val chiselContext = new DynamicVariable[ChiselContext](new ChiselContext)

  def idGen: IdGen = chiselContext.value.idGen

  def globalNamespace: Namespace = dynamicContext.globalNamespace
  def components: ArrayBuffer[Component] = dynamicContext.components

  def currentModule: Option[BaseModule] = dynamicContextVar.value match {
    case Some(dyanmicContext) => dynamicContext.currentModule
    case _ => None
  }
  def currentModule_=(target: Option[BaseModule]): Unit = {
    dynamicContext.currentModule = target
  }

  def readyForModuleConstr: Boolean = dynamicContext.readyForModuleConstr
  def readyForModuleConstr_=(target: Boolean): Unit = {
    dynamicContext.readyForModuleConstr = target
  }

  def currentClock: Option[Bits] = dynamicContext.currentClock
  def currentClock_=(newClock: Option[Bits]): Unit = {
    dynamicContext.currentClock = newClock
  }

  def currentReset: Option[Bits] = dynamicContext.currentReset
  def currentReset_=(newReset: Option[Bits]): Unit = {
    dynamicContext.currentReset = newReset
  }

  def forcedClock: Bits = currentClock.getOrElse(
    throwException("Error: No implicit clock.")
  )
  def forcedReset: Bits = currentReset.getOrElse(
    throwException("Error: No implicit reset.")
  )

  def forcedUserModule: RawModule = currentModule match {
    case Some(module: RawModule) => module
    case _ => throwException(
      "Error: Not in a UserModule. Likely cause: Missed Module() wrap, bare chisel API call, or attempting to construct hardware inside a BlackBox."
    )
  }

  def pushStatement[T <: Statement](c: T): T = {
    forcedUserModule.addStatement(c)
    c
  }

  def pushOp[T <: Data](dest: T, op: PrimOp, all_args: Expression*): T = {
    val consts = all_args.collect { case ILit(i) => i }
    val args = all_args.flatMap {
      case _: ILit => None
      case other => Some(other)
    }
    // Bind each element of the returned Data to being a Op
    dest.bind(OpBinding(forcedUserModule))
    dest.setRef(DoPrim(op, args, consts))
    dest
  }

  def build[T <: RawModule](f: => T): (Circuit, T) = {
    chiselContext.withValue(new ChiselContext) {
      dynamicContextVar.withValue(Some(new DynamicContext())) {
        println("Elaborating design...")
        val mod = f
        println(s"Done elaborating: ${components.last.name}.")

        (Circuit(components.last.name, components.toSeq), mod)
      }
    }
  }
  def error(str: String, cause: Throwable = null) = throw new ChiselException(str, cause)

  def nameRecursively(prefix: String, nameMe: Any, namer: (HasId, String) => Unit): Unit = nameMe match {
    case (id: HasId) => namer(id, prefix)
    case Some(elt) => nameRecursively(prefix, elt, namer)
    case m: Map[_, _] =>
      m foreach { case (k, v) =>
        nameRecursively(s"${k}", v, namer)
      }
    case (iter: Iterable[_]) if iter.hasDefiniteSize =>
      for ((elt, i) <- iter.zipWithIndex) {
        nameRecursively(s"${prefix}_${i}", elt, namer)
      }

    case _ => // Do nothing
  }
}

package chisel

import scala.collection.mutable.{ArrayBuffer, HashMap}

import ir._
import internal._

abstract class BaseModule {
  if (!Builder.readyForModuleConstr) {
    throwException("Error: attempted to instantiate a Module without wrapping it in Module().")
  }
  Builder.readyForModuleConstr = false

  Builder.currentModule = Some(this)

  var _closed = false
  def isClosed = _closed

  val _namespace = Namespace.empty

  val _ids = ArrayBuffer[HasId]()
  def addId(d: HasId): Unit = {
    require(!_closed, "Can't write to module after module close")
    _ids += d
  }
  def getIds = {
    require(_closed, "Can't get ids before module close")
    _ids.toSeq
  }

  val _ports = new ArrayBuffer[Bits]()

  def getModulePorts = {
    require(_closed, "Can't get ports before module close")
    _ports.toSeq
  }

  def desiredName: String = this.getClass.getName.split('.').last

  final lazy val name = desiredName

  def namePorts(names: HashMap[HasId, String]): Unit = {
    for (port <- getModulePorts) {
      port.suggestedName.orElse(names.get(port)) match {
        case Some(name) =>
          if (_namespace.contains(name)) {
            Builder.error(s"""Unable to name port $port to "$name" in ${this.name},""" +
              " name is already taken by another port!")
          }
          port.setRef(Reference(_namespace.name(name), port.tpe))
        case None => throwException(s"Unable to name port $port in $this, " +
                                      "try making it a public field of the Module")
      }
    }
  }

  def generateComponent(): Component

  def getPublicFields(rootClass: Class[_]): Seq[java.lang.reflect.Method] = {
    def is_final(modifier: Int) =
      (modifier & java.lang.reflect.Modifier.FINAL) == java.lang.reflect.Modifier.FINAL
    // Suggest names to nodes using runtime reflection
    def getValNames(c: Class[_]): Set[String] = {
      if (c == rootClass) {
        Set()
      } else {
        getValNames(c.getSuperclass) ++ c.getDeclaredFields.filter(x => is_final(x.getModifiers())).map(_.getName)
      }
    }
    val valNames = getValNames(this.getClass)
    def isPublicVal(m: java.lang.reflect.Method) =
      m.getParameterTypes.isEmpty && valNames.contains(m.getName) && !m.getDeclaringClass.isAssignableFrom(rootClass)

    this.getClass.getMethods.sortWith(_.getName < _.getName).filter(isPublicVal(_))
  }

  def nameIds(rootClass: Class[_]): HashMap[HasId, String] = {
    val names = new HashMap[HasId, String]()
    def name(node: HasId, name: String): Unit = {
      if (!names.contains(node)) {
        names.put(node, name)
      }
    }
    for (m <- getPublicFields(rootClass)) {
      Builder.nameRecursively(m.getName, m.invoke(this), name)
    }
    names
  }

  def bindIoInPlace(iodef: Bits): Unit = {
    iodef.bind(PortBinding(this))
    _ports += iodef
  }

  def IO[T <: Bits](iodef: T, name: String = ""): T = {
    val module = Builder.currentModule.get
    require(!module.isClosed, "Can't add more ports after module close")

    requireIsChiselType(iodef, "io type")

    module.bindIoInPlace(iodef)

    if (name != "") {
      iodef.suggestName(name)
    }
    iodef
  }
}

object Module {
  def apply[T <: BaseModule](bc: => T): T = {
    if (Builder.readyForModuleConstr) {
      Builder.error("Error: Called Module() twice without instantiating a Module.")
    }
    Builder.readyForModuleConstr = true

    val parent = Builder.currentModule

    val (saveClock, saveReset)  = (Builder.currentClock, Builder.currentReset)
    Builder.currentClock = None
    Builder.currentReset = None

    val module: T = bc

    if (Builder.readyForModuleConstr) {
      Builder.error("Error: attempted to instantiate a Module, but nothing happened. " +
                       "This is probably due to rewrapping a Module instance with Module().")
    }

    Builder.currentModule = parent
    Builder.currentClock = saveClock
    Builder.currentReset = saveReset

    val component = module.generateComponent()
    Builder.components += component

    module
  }
}

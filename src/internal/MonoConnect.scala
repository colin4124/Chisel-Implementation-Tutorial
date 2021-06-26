package chisel.internal

import chisel._
import chisel.ir._
import chisel.Utils._
import Builder._

object MonoConnect {
  def UnwritableSinkException =
    MonoConnectException(": Sink is unwriteable by current module.")
  def UnknownRelationException =
    MonoConnectException(": Sink or source unavailable to current module.")
  def MismatchedException(sink: String, source: String) =
    MonoConnectException(s": Sink ($sink) and Source ($source) have different types.")

  def connect(
    sink: Data,
    source: Data,
    context_mod: RawModule): Unit =
    (sink, source) match {
      case (sink_e: Bits, source_e: Bits) =>
        require(sameType(sink_e, source_e))
        elemConnect(sink_e, source_e, context_mod)
      case (sink, source) => throw MismatchedException(sink.toString, source.toString)
    }

  def elemConnect(sink: Bits, source: Bits, context_mod: RawModule): Unit = {
    import SpecifiedDirection.{Internal, Input, Output}
    val sink_mod: BaseModule   = sink.binding.location.getOrElse(throw UnwritableSinkException)
    val source_mod: BaseModule = source.binding.location.getOrElse(context_mod)

    val sink_direction   = sink.direction
    val source_direction = source.direction

    // CASE: Context is same module that both left node and right node are in
    if( (context_mod == sink_mod) && (context_mod == source_mod) ) {
      ((sink_direction, source_direction): @unchecked) match {
        //    SINK          SOURCE
        //    CURRENT MOD   CURRENT MOD
        case (Output,       _) => issueConnect(sink, source)
        case (Internal,     _) => issueConnect(sink, source)
        case (Input,        _) => throw UnwritableSinkException
      }
    }
    // CASE: Context is same module as sink node and right node is in a child module
    else if( (sink_mod == context_mod) && (source_mod != context_mod) ) {
      // pushStatement(DefWire(sink))
      // source.setConn(Node(sink))
      // Thus, right node better be a port node and thus have a direction
      ((sink_direction, source_direction): @unchecked) match {
        //    SINK        SOURCE
        //    CURRENT MOD CHILD MOD
        case (Internal,   Output)   =>
          //val exp = Node(source)
          //pushStatement(DefWire(exp))
          //source.setConn(exp)
          issueConnect(sink, source)
        case (Internal,   Input)    =>
          //val exp = Node(source)
          //pushStatement(DefWire(exp))
          //source.setConn(exp)
          issueConnect(sink, source)
        case (Output,     Output)   => //source.setConn(Node(sink))
        case (Output,     Input)    => //source.setConn(Node(sink))
        case (Input,      _)        => throw UnwritableSinkException
      }
    }
    // CASE: Context is same module as source node and sink node is in child module
    else if( (source_mod == context_mod) && (sink_mod != context_mod) ) {
      // pushStatement(DefWire(Node(sink)))
      //sink.setConn(Node(source))
      // Thus, left node better be a port node and thus have a direction
      ((sink_direction, source_direction): @unchecked) match {
        //    SINK          SOURCE
        //    CHILD MOD     CURRENT MOD
        case (Input,    Internal) =>
        case (Input,    _) =>
        case (Output,   _) => throw UnwritableSinkException
        case (Internal, _) => throw UnwritableSinkException
      }
    }
    // CASE: Context is the parent module of both the module containing sink node
    //                                        and the module containing source node
    //   Note: This includes case when sink and source in same module but in parent
    else {
      // Thus both nodes must be ports and have a direction
      ((sink_direction, source_direction): @unchecked) match {
        //    SINK      SOURCE
        //    CHILD MOD CHILD MOD
        case (Input,    _) =>
          //val exp = PortConnNode(source, sink)
          //pushStatement(DefWire(exp))
          //source.setConn(exp)
          //sink.setConn(exp)
        case (Output,   _) => throw UnwritableSinkException
        case (Internal, _) => throw UnwritableSinkException
      }
    }
  }

  private def issueConnect(sink: Bits, source: Bits): Unit = {
    pushStatement(Connect(sink.lref, source.ref))
  }
}

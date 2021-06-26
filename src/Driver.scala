package chisel

import java.nio.file.{Files, Paths}
import java.io.{File, FileWriter}

import internal._

object Driver {
  def execute(dut: () => RawModule, dump_dir: String): Unit = {
    val (circuit, _) = Builder.build(Module(dut()))
    val contents = Emitter.emit(circuit) map { case (_, v) => v } reduce { _ + _ }
    Files.createDirectories(Paths.get(dump_dir));
    val file = new File(s"${dump_dir}/${circuit.name}.v")
    val w = new FileWriter(file)
    w.write(contents)
    w.close()
  }
}

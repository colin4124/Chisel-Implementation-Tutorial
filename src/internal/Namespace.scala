package chisel.internal

import chisel._

class Namespace(keywords: Set[String]) {
  private val names = collection.mutable.HashMap[String, Long]()
  for (keyword <- keywords)
    names(keyword) = 1

  def contains(elem: String): Boolean = names.contains(elem)

  def name(elem: String): String = {
    if (this contains elem) {
      name(rename(elem))
    } else {
      names(elem) = 1
      elem
    }
  }

  private def rename(n: String): String = {
    val index = names(n)
    val tryName = s"${n}_${index}"
    names(n) = index + 1
    if (this contains tryName) rename(n) else tryName
  }
}

object Namespace {
  def empty: Namespace = new Namespace(Set.empty[String])
}

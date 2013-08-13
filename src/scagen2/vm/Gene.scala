package scagen2.vm

import Instruction.Operation

class Gene(instructions: Seq[Instruction]) {

  lazy val execute = {
    val stack = new SimpleStack
    instructions foreach (_(stack))
    stack.pop()
  }

  lazy          val numInstructions = instructions.length
  private[this] var _toString       = ""

  override def toString() = {
    if(_toString.isEmpty)
      _toString = s"${this.getClass.getName}( ${instructions.map(_.toString()).mkString(" ")} )"

    _toString
  }
}

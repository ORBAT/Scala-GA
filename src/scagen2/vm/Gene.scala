package scagen2.vm

import Instruction.Operation

class Gene(instructions: Seq[Instruction]) {

  lazy val execute = {
    new Context(instructions).execute()
  }

  lazy          val numInstructions = instructions.length
  private[this] var _toString       = ""

  override def toString() = {
    if(_toString.isEmpty)
      _toString = s"${this.getClass.getName}( ${instructions.map(_.toString()).mkString(" ")} )"

    _toString
  }
}

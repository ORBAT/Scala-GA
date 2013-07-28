package scagen2.vm
import Instruction.Operation

class Gene(instructions: Seq[Operation]) {
  def execute() = {
    val stack = new SimpleStack
    instructions foreach (_(stack))
    stack.pop()
  }

  lazy val numInstructions = instructions.length
}

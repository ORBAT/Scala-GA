package scagen2.vm

class Gene(instructions: Vector[(SimpleStack) => Unit]) {

  def execute() = {
    val stack = new SimpleStack
    instructions foreach (_(stack))
    stack.pop()
  }
}

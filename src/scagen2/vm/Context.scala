package scagen2.vm

import scagen2.vm.SimpleStack.ItemType

class Context(instructions:Seq[Instruction]) {
  private[this] val numInstructions = instructions.length
  val stack = new SimpleStack
  def execute():ItemType = {
    while(_ip < numInstructions) {
      instructions(_ip)(this)
      _ip += 1;
    }
    stack.pop()
  }
  private[this] var _ip: Int = 0
  def ip: Int = _ip

  /**
   * Skips a given amount of instructions. If an amount of 0 or less will have no effect on execution.
   * @param amount How many instructions to skip.
   * @example
   *    `skip(1)` would mean that the instruction after the current one would not be run.
   */
  def skip(amount:Int) {
    if(amount > 0)
      _ip += amount
  }
}
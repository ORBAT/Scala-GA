package scagen2.vm

import scagen2.vm.SimpleStack.ItemType

class Context(instructions:Seq[Instruction]) {
  val stack = new SimpleStack
  def execute():ItemType = {
    instructions.foreach(_(this))
    stack.pop()
  }
  private[this] var _ip: Int = _
  def ip: Int = ???
  def ++ {???}
  def -- {???}
  def reljmp(to:Int) {???}
  def absjmp(to:Int) {???}
}
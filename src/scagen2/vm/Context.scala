package scagen2.vm

class Context(instructions:Nothing) {
  private[this] var _ip: Int = ???
  def ip: Int = ???
  def ++ {???}
  def -- {???}
  def reljmp(to:Int) {???}
  def absjmp(to:Int) {???}
}
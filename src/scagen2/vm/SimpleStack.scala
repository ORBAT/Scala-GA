package scagen2.vm

object SimpleStack {
  type ItemType = Double
  protected[SimpleStack] val defaultContent: ItemType = Double.NaN
}

class SimpleStack {

  import SimpleStack._

  private[this] val stack = collection.mutable.ArrayStack[ItemType]()

  def clear() {
    stack.clear()
  }

  def push(i: ItemType) {
    stack.push(i)
  }

  def pop() = {
    val ret =
      if(stack.length == 0)
        defaultContent
      else {
        stack.pop()
      }
    ret
  }

  def dup() {
    if(stack.length != 0) {
      stack.dup()
    }
  }

  def drop() {
    if(stack.length != 0)
      stack.pop() // note that this is discarded since drop returns Unit
  }

  /**
   * swap ( a b -- b a )
   */
  def swap() {
    if(stack.length > 2) {
      val first = stack.pop()
      val second = stack.pop()
      stack.push(first)
      stack.push(second)
    }
  }

  def peek = {
    val ret = if(stack.length == 0)
      defaultContent
    else {
      stack.top
    }
    ret
  }

  //  over ( a b -- a b a )
  //  rot  ( a b c -- b c a )
  def rot() = ???

  def over() = ???

  def length = stack.length

  /*

 peek ( a -- a )
 pop ( a -- )
 push ( -- a )

  dup  ( a -- a a )
drop ( a -- )
swap ( a b -- b a )
over ( a b -- a b a )
rot  ( a b c -- b c a )

   */


}

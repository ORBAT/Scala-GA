import org.scalatest.FlatSpec
import scagen2.vm.SimpleStack

class StackSpec extends FlatSpec {

  trait TwoItemStack {

    val stack = new SimpleStack
    stack.push(1)
    stack.push(2)
  }

  trait EmptyStack {
    val stack = new SimpleStack
  }

  abstract class StackWithLen(val i: Int) {
    val stack = new SimpleStack
    if(i > 0) {
      1 to i foreach { i =>
        stack.push(i)
      }
    }
  }

  behavior of "A SimpleStack"

  it should "should work in LIFO order" in new StackWithLen(2) {
    assert(stack.pop() === 2)
    assert(stack.pop() === 1)
    assert(stack.pop().isNaN)
  }

  it should "should preserve a coherent stack " +
                 "even when too many pops are done" in new StackWithLen(0) {
    assert(stack.pop().isNaN)
    assert(stack.pop().isNaN)
    stack.push(1)
    stack.push(2)
    assert(stack.pop() === 2)
    assert(stack.pop() === 1)
    assert(stack.pop().isNaN)
  }


  it should "peek" in new StackWithLen(2) {
    assert(stack.peek === 2)
    assert(stack.pop === 2)
    assert(stack.peek === 1)
    assert(stack.pop === 1)
  }

  it should "dup" in new StackWithLen(2) {
    stack.dup()
    assert(stack.pop === 2)
    assert(stack.pop === 2)
    assert(stack.pop === 1)
  }

  it should "handle peeks on empty stacks" in new StackWithLen(0) {
    assert(stack.peek.isNaN)
    stack.push(1)
    assert(stack.peek === 1)
  }

  it should "handle dups on empty stacks" in new StackWithLen(0) {
    stack.dup()
    stack push 1
    stack dup();
    assert(stack.pop() === 1)
    assert(stack.pop() === 1)
  }

  it should "rot" in new StackWithLen(3) {
    /*
     1
     2
     3

     2
     3
     1

     */
//    stack.rot()
    assert(stack.pop === 1)
    assert(stack.pop === 3)
    assert(stack.pop === 2)
  }

  it should "handle rots on empty stacks" in new StackWithLen(0) {
//    stack.rot()
    fail
  }

  it should "handle rots on one item stacks" in new StackWithLen(1) {
//    stack.rot()
    assert(stack.pop === 1)
  }

  it should "over" in new StackWithLen(3) {
    // 3 2 1
//    stack.over()
    // 1 3 2 1
    assert(stack.pop === 1)
    assert(stack.pop === 3)
    assert(stack.pop === 2)
    assert(stack.pop === 1)
  }

  it should "swap" in new StackWithLen(3) {
    // 3 2 1
    // 2 3 1
    stack.swap()
    assert(stack.pop === 2)
    assert(stack.pop === 3)
    assert(stack.pop === 1)
  }

  it should "drop" in new StackWithLen(3) {
    stack.drop()
    assert(stack.pop() == 2)
    assert(stack.pop() == 1)
    assert(stack.pop().isNaN)
  }

  it should "calculate length" in new StackWithLen(5) {
    assert(stack.length === 5)
    stack.pop()
    assert(stack.length === 4)
  }

}

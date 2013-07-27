package scagen2.vm

object SimpleStack {
   type ItemType = Double
   protected[SimpleStack] val defaultContent: ItemType = Double.NaN
 }

class SimpleStack {

   import SimpleStack._

   private[this] var stack = Vector.empty[ItemType]

   def clear() {
     stack = Vector.empty
   }

   def push(i: ItemType) {
     stack = i +: stack
   }

   def pop() = {
     val length = stack.length
     val ret =
       if(length == 0)
         defaultContent
       else {
         val out = stack.head
         stack = stack.tail
         out
       }
     ret
   }

   def dup() {
     if(stack.length != 0) {
       val item = peek
       push(item)
     }
   }

   def drop() {
     stack = stack.tail
   }

   /**
    * swap ( a b -- b a )
    */
   def swap() {
     val first = stack.head
     val second = stack.tail.head
     stack = second +: first +: stack.drop(2)
   }

   /**
    * over ( a b -- a b a )
    */
   def over() {
     if(stack.length > 0) {
       push(stack.last)
     }
   }

   def rot() {
     if(stack.length > 1) {
       val last = stack.last
       stack = stack.dropRight(1)
       stack = last +: stack
     }
   }

   def peek = {
     val length = stack.length
     val ret = if(length == 0)
       defaultContent
     else {
       stack.head
     }
     ret
   }

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

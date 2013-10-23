package scagen2.utils.data_transformers
import scagen2.vm.SimpleStack.ItemType


class EMA(val alpha: Double) extends (Stream[ItemType] => Stream[ItemType]) {
  def apply(source: Stream[ItemType]): Stream[ItemType] = {
    def tail(sourceStream: Stream[ItemType], oldVal: ItemType): Stream[ItemType] = {
      val myAlpha = alpha
      (oldVal + myAlpha * (sourceStream.head - oldVal)) #:: tail(sourceStream.tail, sourceStream.head)
    }
    tail(source, source.head)
  }
}

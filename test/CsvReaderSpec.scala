import java.io.File
import org.scalatest.FlatSpec
import scagen2.utils.CsvReader
import scala.collection.immutable
import scala.io.{Codec, Source}

class CsvReaderSpec extends FlatSpec {

  trait TestMaterialReader {
    val testMaterialRows = 5
    val headerMaxChar = 'D'

    implicit val codec = Codec.UTF8
    /**
     * Test header
     */
    val header: Seq[String] = 'A' to headerMaxChar map (c => s"$c $c")
    /**
     *
     */
    val columns: Seq[Seq[String]] = (0 to header.length - 1) map {
      colIdx => (1 to testMaterialRows).map(itemIdx => (itemIdx + colIdx * testMaterialRows).toString) // note toString
    }
    val testSource: Source = {
      /**
       * A test CSV string that looks something like (with testMaterialRows = 5 and headerMaxChar = 'D')
* {{{A A,B B,C C,D D
*  1,5,9,13
*  2,6,10,14
*  3,7,11,15
*  4,8,12,16}}}
       */
      val testMaterial: String = {
        /* A seq of seqs that contains the test data for each _column_. Later transposed to rows.
         *
         * The value for an item on a row is itemIndex (starts from 1) + columnIndex (starts from 0) * testMaterialRows
         */
        header.mkString("", ",","\n") + columns.transpose.map(_.mkString(",")).mkString("\n")
      }
      Source.fromChars(testMaterial.toCharArray)
    }

    val csvReader = new CsvReader(testSource, ",")

  }

  behavior of "A CsvReader"

  it should "give the correct header" in new TestMaterialReader {
    assert(csvReader.header === header)
  }

  it should "give the correct raw columns" in new TestMaterialReader {
    fail
  }

  it should "give the correct column using apply(Header)" in new TestMaterialReader {
    fail
  }


  /*  it should "should work in LIFO order" in new StackWithLen(2) {
      assert(stack.pop() === 2)
      assert(stack.pop() === 1)
      assert(stack.pop().isNaN)
    }*/

}

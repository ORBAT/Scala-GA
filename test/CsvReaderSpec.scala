import java.io.File
import java.util.Date
import org.scalatest.FlatSpec
import scagen2.utils._
import scala.collection.immutable
import scala.io.{Codec, Source}

class CsvReaderSpec extends FlatSpec {

  trait TestMaterialReader {
    private implicit val codec = Codec.UTF8
    private val testMaterialRows = 20000
    private[this] val headerMaxChar: Char = ('A'.toInt + 3).toChar
    /**
     * Test header
     */
    private[this] val headerStr: Seq[String] = 'A' to headerMaxChar map (c => s"$c $c")
    protected[this] val headerObjects: Seq[ColumnMetadata] = headerStr map ColumnMetadata
    /**
     *
     */
    val testSource: Source = {
      /**
       * A test CSV string that looks something like (with testMaterialRows = 4 and headerMaxChar = 'D')
       * {{{A A,B B,C C,D D
       *  1,5,9,13
       *  2,6,10,14
       *  3,7,11,15
       *  4,8,12,16}}}
       */
      val testMaterial: String = {
        val columns: Seq[Seq[String]] = (0 to headerStr.length - 1) map {
          colIdx =>
            (1 to testMaterialRows)
            .map(itemIdx => (itemIdx + colIdx * testMaterialRows).toString) // note toString
        }
        /* A seq of seqs that contains the test data for each _column_. Later transposed to rows.
         *
         * The value for an item on a row is itemIndex (starts from 1) + columnIndex (starts from 0) * testMaterialRows
         */
        headerStr.mkString("", ",", "\n") + columns.transpose.map(_.mkString(",")).mkString("\n")
      }
      Source.fromChars(testMaterial.toCharArray)
    }

    /**
     * Checks whether the value found at the `itemIndex` (starts from 0) position on column `colIdx` (starts from 0)
     * @param colIdx The column index
     * @param itemIndex The item index inside the column
     * @param found What we actually found at the given indices
     * @return `true` when the found value matches what we expect, `false` otherwise.
     */
    def checkElem(colIdx: Int)(itemIndex: Int, found: String) {
      val wanted = (itemIndex + 1 + colIdx * testMaterialRows).toString
      assert(wanted === found, s"Checking column $colIdx, item $itemIndex. Wanted $wanted but found $found")
    }

    def checkColumnStreams(columnStreams: Seq[Stream[String]]) {
      // each column is zipped with its column idx, and each item is zipped with its item idx
      val zipped = columnStreams.map(stream => stream.zipWithIndex).zipWithIndex

      zipped.foreach {
        tuple =>
          val colStream = tuple._1
          val colIdx = tuple._2
          val checker = checkElem(colIdx) _
          colStream.foreach {
            tup =>
              val found = tup._1
              val itemIdx = tup._2
              checker(itemIdx, found)
          }
      }
    }

    val csvReader = new CsvReader(testSource, ',')

  }

  behavior of "A CsvReader"

  it should "give the correct header" in new TestMaterialReader {
    assert(csvReader.header === headerObjects)
  }

  it should "give correct columns using apply(Int)" in new TestMaterialReader {
    val columnStreams = 0 to headerObjects.length - 1 map {
      headIdx => csvReader(headIdx)
    }
    checkColumnStreams(columnStreams)
  }

  it should "give correct columns after using apply(Int) multiple times" in new TestMaterialReader {
    def giveColStream() = (0 to headerObjects.length - 1) map {
      headIdx => csvReader(headIdx)
    }

    val lotsaColStreams = (1 to 10).map(_ => giveColStream())
    lotsaColStreams.map(columnStreams => checkColumnStreams(columnStreams))

  }

  it should "give the correct columns using rawColumns" in new TestMaterialReader {
    val columnStreams: Seq[Stream[String]] = csvReader.rawColumns
    checkColumnStreams(columnStreams)
  }

  it should "give the correct columns using apply(ColumnMetadata)" in new TestMaterialReader {
    val columnStreams = csvReader.header.map {
      header => csvReader(header)
    }
    checkColumnStreams(columnStreams)
  }


  it should "return columns with filters applied" in new TestMaterialReader {
    val filteredFirst = csvReader.columnMap(0, CsvReader.toOptInt)
    fail
  }

  it should "provide a few correct default filters" in new {

    import CsvReader._

    def compareDouble(d: Double) {
      def fmt(d: Double) = d.formatted("%.2g")
      val od = toOptDouble(fmt(d))
      assert(od.isDefined, s"Trying to convert ${fmt(d)} to Option[Double] got us a None. That's usually bad.")
      val ddiff = math.abs(od.get - d)
      val maxdDiff = 0.0000001
      assert(
        ddiff < maxdDiff,
        s"Converting ${fmt(d)} to Option[Double] got us Some(${od.get}). The difference is $ddiff, with " +
        s"maxdDiff being $maxdDiff")
    }

    val maxDoubles = 40
    1 to maxDoubles map {
      val min = -10d
      val max = 10d
      val step = (max - min) / maxDoubles
      x => min + (step * x)
    } foreach compareDouble



    def compareInt(i: Int) {
      val id = toOptInt(i.toString)
      assert(id.isDefined, s"Trying to convert $i to Option[Int] got us a None. That's usually bad.")
      val idiff = math.abs(id.get - i)
      assert(idiff === 0, s"Converting $i to Option[Int] got us Some(${id.get}). The difference is $idiff when it " +
                         s"should be 0")
    }

    -20 to 20 foreach compareInt



    val formatStr = "yyyy-MM-dd"
    def compareDate(dateStr: String) {
      val toDateWithFormat = toOptDate(formatStr)
      val dateFormatter = new java.text.SimpleDateFormat(formatStr)
      val maybeDateFromStr = toDateWithFormat(dateStr)
      assert(maybeDateFromStr.isDefined, s"maybeDateFromStr was None for ${dateStr}")
      val dateFromStr = maybeDateFromStr.get
      Console.println(dateStr)
      assert(dateFormatter.format(dateFromStr) === dateStr)
    }

    /*
    val start = 100
    val num = 3
    val step = 1
    val end = start (step * (num-1))
     */

    val numDates = 20
    val dates = (0 to numDates - 1).map { idx =>
      new Date(60l * 60l * 24l * 1000 * idx)
    } map {
      val dateFormatter = new java.text.SimpleDateFormat(formatStr)
      dateFormatter.format(_)
    }

    dates foreach compareDate
  }

  /*  it should "should work in LIFO order" in new StackWithLen(2) {
      assert(stack.pop() === 2)
      assert(stack.pop() === 1)
      assert(stack.pop().isNaN)
    }*/
}

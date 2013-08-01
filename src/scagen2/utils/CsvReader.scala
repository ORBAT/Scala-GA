package scagen2.utils

import scala.io.Source
import java.util.Date

case class ColumnMetadata(text: String)

object CsvReader {

  def toOptInt(s: String): Option[Int] =
      try Some(java.lang.Integer.parseInt(s))
      catch {
        case _: Throwable => None
      }

  def toOptDouble(s: String): Option[Double] =
      try Some(java.lang.Double.parseDouble(s))
      catch {
        case _: Throwable => None
      }

  def toOptDate(formatString: String): (String) => Option[Date] = {
    val format = new java.text.SimpleDateFormat(formatString)
    (dateStr: String) =>
      try Some(format.parse(dateStr))
      catch {
        case _: Throwable => None
      }
  }

}

/**
 * Reads CSV data from a file and gives some tools for handling it.
 * @param src Source to read the CSV lines form
 * @param separator Separator character. Defaults to ','
 */
class CsvReader(src: Source, separator: Char = ',') {

  import collection.mutable.{Map => MutMap}

  def columnMap[T](c: ColumnMetadata, f: (String) => T): Stream[T] = {
    this(c) map f
  }

  def columnMap[T](idx: Int, f: (String) => T): Stream[T] = {
    this(idx) map f
  }

  private[this] val rawLineStream: Stream[Seq[String]] = {
    def explodeStr(s: String): Seq[String] = s.split(separator).toSeq
    src.getLines().toStream.map(explodeStr(_))
  }
  private[this] val rawLineStreamNoHeader = rawLineStream.tail
  /**
   * Returns everything on the first row of the CSV file as a ColumnMetadata object.
   * @return The first row
   */
  private[this] lazy val headerToIdxMap: Map[ColumnMetadata, Int] = header.zipWithIndex.toMap
   val header: Seq[ColumnMetadata] = rawLineStream.head.map(colName =>
    ColumnMetadata(colName))
  /**
   * A stream of all columns. Each element is a column stream, and there may be arbitrarily many elements
   */
  lazy val rawColumns: Stream[Stream[String]] = header.map(col => this(col))(collection.breakOut)

  /**
   * Returns a Stream that contains every element in the column with index `colIdx`. The stream should not be infinite,
   * but itc an be arbitrarily large.
   * @param colIdx Get the data for the column with this index, e.g. 0 would be the first column of the data.
   * @return A Stream that contains the specified column
   */
  def apply(colIdx: Int): Stream[String] = {
    rawLineStreamNoHeader.map(column => column(colIdx))
  }

  /**
   * Returns a Stream that contains the data for the given ColumnMetadata object.
   * @param header
   * @return
   */
  def apply(header: ColumnMetadata): Stream[String] = {
    val colIdx = headerToIdxMap(header)
    rawLineStreamNoHeader.map(column => column(colIdx))
  }
}

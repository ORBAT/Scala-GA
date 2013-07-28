package scagen2.utils

import scala.io.Source

case class ColumnMetadata(text: String)

object CsvReader {
  val toOptInt: (String) => Option[Int] =
    (s: String) =>
      try
        Some(java.lang.Integer.parseInt(s))
      catch {
        case _: Throwable => None
      }
  val toOptDouble: (String) => Option[Double] =
    (s: String) =>
      try Some(java.lang.Double.parseDouble(s))
      catch {
        case _: Throwable => None
      }

  def toOptDate(formatString: String) = {
    val format = new java.text.SimpleDateFormat(formatString)
    (dateStr: String) => {
      try
        Some(format.parse(dateStr))
      catch {
        case _: Throwable => None
      }
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
  lazy val header: Seq[ColumnMetadata] = rawLineStream.head.map(colName =>
    ColumnMetadata(colName))
  /**
   * Simply all columns as Streams of Strings (both of which might be arbitrarily large)
   */
  lazy val rawColumns: Seq[Stream[String]] = header.map(col => this(col))

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

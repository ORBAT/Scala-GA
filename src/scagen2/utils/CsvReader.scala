package scagen2.utils

import scala.io.Source

case class Header(text: String)

/**
 * Reads CSV data from a file and gives some tools for handling it.
 * @param src
 * @param separator
 * @param hasHeader
 */
class CsvReader(src: Source, separator: Char) {
  private[this] val rawLineStream = {
    def explodeStr(s: String): Seq[String] = {
      s.split(separator).toSeq
    }
    src.getLines().toStream.map(explodeStr)
  }
  private[this]      val rawLineStreamNoHeader            = rawLineStream.tail
  /**
   * Returns everything on the first row of the CSV file as a Header object.
   * @return The first row
   */
  private[this] lazy val headerToIdxMap: Map[Header, Int] = header.zipWithIndex.toMap

  lazy val header: Seq[Header] = rawLineStream.head.map(colName => Header(colName))
  /**
   * Simply all columns as Streams of Strings (both of which might be arbitrarily large)
   */
  lazy val rawColumns: Seq[Stream[String]] = {
    ???
  }

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
   * Returns a Stream that contains the data for the given Header object.
   * @param header
   * @return
   */
  def apply(header: Header): Stream[String] = {
    val colIdx = headerToIdxMap(header)
    rawLineStreamNoHeader.map(column => column(colIdx))
  }
}

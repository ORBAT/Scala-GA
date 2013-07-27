package scagen2.utils

import scala.io.Source

object CsvReader {
  type Ret = Map[String, Seq[String]]
}

class CsvReader(src:Source, separator:String, hasHeader: Boolean = true) extends (() => CsvReader.Ret) {

  private[this] lazy val csv: CsvReader.Ret = ???

  /**
   * Returns either everything on the first row of the CSV file. Throws an exception if things don't work out.
   * @return The first row
   */
  def header: Seq[String] = ???
  def apply() = csv
}

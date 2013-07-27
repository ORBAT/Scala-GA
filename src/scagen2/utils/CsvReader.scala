package scagen2.utils

import scala.io.Source

object CsvReader {
  type Ret = Map[String, Seq[String]]
}

class CsvReader(src:Source) extends (() => Option[CsvReader.Ret]) {
  private[this] lazy val csv: Option[CsvReader.Ret] = ???
  def apply(): Option[CsvReader.Ret] = csv
}

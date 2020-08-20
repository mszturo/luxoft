package luxoft.domain

final case class WrongReading(got: String) extends Exception {
  override def getMessage: String = {
    s"Expected number between $minHumidity and $maxHumidity, got $got"
  }
}

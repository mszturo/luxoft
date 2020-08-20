package luxoft.domain

import scala.util.Try

sealed trait Measurement extends Ordered[Measurement] {
  def max(other: Measurement): Measurement

  def min(other: Measurement): Measurement
}

object Measurement {
  def apply(value: String): Either[WrongReading, Measurement] = {
    Try(value.toInt).map { humidity =>
      if (inAcceptableRange(humidity))
        Right(Humidity(humidity))
      else
        Left(WrongReading(value))
    }.getOrElse {
      if (value == "NaN")
        Right(NaN)
      else
        Left(WrongReading(value))
    }
  }

  private def inAcceptableRange(humVal: Int): Boolean =
    humVal >= minHumidity && humVal <= maxHumidity
}

final case class Humidity(value: Int) extends Measurement {
  override def max(other: Measurement): Measurement = other match {
    case Humidity(otherValue) =>
      if (value >= otherValue)
        this
      else
        other
    case NaN => this
  }

  override def min(other: Measurement): Measurement = other match {
    case Humidity(otherValue) =>
      if (value <= otherValue)
        this
      else
        other
    case NaN => this
  }

  override def compare(that: Measurement): Int = that match {
    case Humidity(otherVal) => value.compare(otherVal)
    case NaN => 1
  }

  override def toString: String = value.toString
}

case object NaN extends Measurement {
  override def max(other: Measurement): Measurement = other

  override def min(other: Measurement): Measurement = other

  override def compare(that: Measurement): Int = -1
}
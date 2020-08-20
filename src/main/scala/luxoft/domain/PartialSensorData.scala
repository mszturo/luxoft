package luxoft.domain

final case class PartialSensorData(min: Measurement, sum: Long, count: Long, max: Measurement) {
  def combine(other: PartialSensorData): PartialSensorData = {
    val newMin = min.min(other.min)
    val newMax = min.max(other.max)
    val newSum = sum + other.sum
    val newCount = count + other.count

    PartialSensorData(newMin, newSum, newCount, newMax)
  }
}

object PartialSensorData {
  lazy val empty: PartialSensorData = PartialSensorData(NaN, 0, 0, NaN)

  def apply(measurement: Measurement): PartialSensorData = measurement match {
    case h: Humidity => PartialSensorData(h, h.value, 1, h)
    case NaN => empty
  }
}

package luxoft.domain

import cats.implicits.catsSyntaxSemigroup
import cats.kernel.Semigroup

case class ParsingResults(readings: Map[SensorId, PartialSensorData], failed: Long) {
  def addMeasurement(sensorId: SensorId, measurement: Measurement): ParsingResults = {
    val oldData = readings.getOrElse(sensorId, PartialSensorData.empty)
    val updatedData = oldData.combine(PartialSensorData(measurement))
    val updatedReadings = readings.updated(sensorId, updatedData)
    val updatedFailed = measurement match {
      case _: Humidity => failed
      case NaN => failed + 1
    }

    this.copy(updatedReadings, updatedFailed)
  }

  def combine(other: ParsingResults): ParsingResults = {
    val updatedReadings = readings.combine(other.readings)
    val updatedFailed = failed + other.failed

    ParsingResults(updatedReadings, updatedFailed)
  }

  private implicit val readingsSemigroup: Semigroup[Map[SensorId, PartialSensorData]] =
    (x: Map[SensorId, PartialSensorData], y: Map[SensorId, PartialSensorData]) => {
      (x.toSeq ++ y).groupMap(_._1)(_._2)
        .map { case (id, data) => id -> data.reduceOption(_ combine _).getOrElse(PartialSensorData.empty) }
    }
}

object ParsingResults {
  lazy val empty: ParsingResults = ParsingResults(Map.empty.withDefaultValue(PartialSensorData.empty), 0)
}

package luxoft.domain

final case class SensorData(sensorId: SensorId, min: Measurement, avg: Measurement, max: Measurement)

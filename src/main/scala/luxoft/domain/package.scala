package luxoft

package object domain {
  case class SensorId(value: String) extends AnyVal
  val minHumidity = 0
  val maxHumidity = 100
}

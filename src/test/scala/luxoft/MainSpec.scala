package luxoft

import java.io.File

import luxoft.domain.{Humidity, NaN, SensorData, SensorId}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatest.{BeforeAndAfterAll, BeforeAndAfterEach}

class MainSpec extends AnyFlatSpec with Matchers
  with BeforeAndAfterEach with BeforeAndAfterAll {
  val path = "src/test/resources/testCsv"
  val dir = new File(path)

  it should "download and store files" in {
    val (files, finalResults, measurements, failed) =
      Main.processReadingsFrom(path).unsafeRunSync()

    files should be(2)
    (measurements + failed) should be(7)
    failed should be(2)

    finalResults.toList.sortBy(_.avg).reverse should contain theSameElementsInOrderAs List(
      SensorData(SensorId("s2"), Humidity(78), Humidity(82), Humidity(88)),
      SensorData(SensorId("s1"), Humidity(10), Humidity(54), Humidity(98)),
      SensorData(SensorId("s3"), NaN, NaN, NaN)
    )
  }
}

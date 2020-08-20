package luxoft

import java.io.File

import cats.effect.IO
import cats.implicits.{catsStdInstancesForList, toTraverseOps}
import luxoft.domain._

import scala.io.{BufferedSource, Source, StdIn}
import scala.util.Try

object Main {
  def main(args: Array[String]): Unit = {
    val program = for {
      path <- IO(StdIn.readLine())
      processedData <- processReadingsFrom(path)
      (files, finalResults, measurements, failed) = processedData
      _ <- processedFiles(files)
      _ <- processedMeasurements(measurements + failed)
      _ <- failedMeasurements(failed)
      top3 = finalResults.toVector.sortBy(_.avg).reverse.take(3)
      _ <- highestAvg(top3)
    } yield ()

    program.unsafeRunSync()
  }

  def processReadingsFrom(path: String): IO[(Int, Iterable[SensorData], Long, Long)] = {
    for {
      csvs <- getCsvFiles(path)
      partialResults <- readFiles(csvs)
      (finalResults, measurements, failed) = aggregateAllData(partialResults)
    } yield (csvs.length, finalResults, measurements, failed)
  }

  private def getCsvFiles(path: String): IO[Array[File]] = {
    IO(new File(path).listFiles().filter(_.getName.endsWith(".csv")))
  }

  private def readFiles(csvs: Array[File]): IO[List[ParsingResults]] = {
    csvs.toList.traverse(f => IO(Source.fromFile(f)).bracket(aggregateFileData)(f => IO(f.close())))
  }

  private def aggregateFileData(source: BufferedSource): IO[ParsingResults] = {
    for {
      lines <- IO(source.getLines().drop(1))
      parsingResults <- aggregateLines(lines)
    } yield parsingResults
  }

  private def aggregateLines(lines: Iterator[String]): IO[ParsingResults] = {
    lines.foldLeft(IO.pure(ParsingResults.empty)) {
      case (runningResult, line) =>
        line.split(",") match {
          case Array(id, reading) =>
            Measurement(reading.trim) match {
              case Left(ex) =>
                IO(println(ex.getMessage)) *> runningResult
              case Right(measurement) =>
                runningResult.map(_.addMeasurement(SensorId(id.trim), measurement))
                  .map { i =>
                    println(s"In IO: $i")
                    i
                  }
            }
          case _ =>
            IO(println(s"Got unexpected line: $line")) *> runningResult
        }
    }
  }

  private def aggregateAllData(partialResults: List[ParsingResults]): (Iterable[SensorData], Long, Long) = {
    val results = partialResults.reduceOption(_ combine _).getOrElse(ParsingResults.empty)
    val sensorData = results.readings.map { case (id, data) =>
      val avg = Try(data.sum / data.count)
        .map(_.toInt)
        .map(Humidity)
        .getOrElse(NaN)
      SensorData(id, data.min, avg, data.max)
    }
    val successReadings = results.readings.map(_._2.count).sum

    (sensorData, successReadings, results.failed)
  }

  private def processedFiles(filesCount: Int): IO[Unit] = {
    IO(println(s"Num of processed files: $filesCount"))
  }

  private def processedMeasurements(measurements: Long): IO[Unit] = {
    IO(println(s"Num of processed measurements: $measurements"))
  }

  private def failedMeasurements(failedMeasurements: Long): IO[Unit] = {
    IO(println(s"Num of processed files: $failedMeasurements"))
  }

  private def highestAvg(finalResults: Iterable[SensorData]): IO[Unit] = {
    val top3 = finalResults
      .map { sd =>
        s"${sd.sensorId.value},${sd.min},${sd.avg},${sd.max}"
      }.mkString("\n")
    val headers = "sensor-id,min,avg,max"
    IO(println(s"$headers\n$top3"))
  }
}

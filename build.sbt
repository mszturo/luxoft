name := "luxoft"

version := "0.1"

scalaVersion := "2.13.3"

val catsVer = "2.1.4"
val fs2Ver = "2.4.0"

libraryDependencies ++= Seq(
  "org.typelevel" %% "cats-effect" % catsVer,
  "org.scalatest" %% "scalatest" % "3.2.0" % "test")
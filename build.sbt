ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "2.13.8"

lazy val root = (project in file("."))
  .settings(
    name := "HeatPumpEmulator"
  )

libraryDependencies += "com.fazecast" % "jSerialComm" % "[2.0.0,3.0.0)"


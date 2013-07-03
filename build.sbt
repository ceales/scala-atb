// Scala-atb
// copyright 2013 Craig Eales
//
// distributed under the MIT License (MIT)
//

name := "scala-atb"

version := "0.1"

scalaVersion := "2.10.2"

scalacOptions += "-deprecation"

scalacOptions += "-unchecked"

scalacOptions += "-feature"

scalacOptions in doc += "-diagrams"

libraryDependencies += "org.scalatest" %% "scalatest" % "1.9.1" % "test"

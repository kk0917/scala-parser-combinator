name := "scala-parser-combinator"

version := "0.1"

scalaVersion := "2.12.7"

scalastyleSources in Compile := (unmanagedSourceDirectories in Compile).value

scalacOptions ++= Seq("-encoding", "UTF-8") // for scoverage

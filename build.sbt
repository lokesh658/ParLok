name := """ParLok"""
organization := "com.ParLok"

version := "1.0-SNAPSHOT"

lazy val root = (project in file(".")).enablePlugins(PlayScala)

scalaVersion := "2.13.15"

libraryDependencies += guice
libraryDependencies += "org.scalatestplus.play" %% "scalatestplus-play" % "7.0.1" % Test

//Adding mongoDB driver
libraryDependencies += "org.mongodb.scala" %% "mongo-scala-driver" % "5.2.0"

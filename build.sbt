import sbt._
import Process._
import Keys._

name := "fp"

version := "1.0"

scalaVersion := "2.10.2"

libraryDependencies += "org.scalatest" %% "scalatest" % "2.0.RC1-SNAP4" % "test"

initialCommands in console := """println("Functional Programming in Scala!");import com.fp._;"""

testOptions in Test += Tests.Argument("-oD")

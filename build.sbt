import sbt._
import Process._
import Keys._

name := "fp"

version := "1.0"

scalaVersion := "2.10.2"

initialCommands in console := """println("Functional Programming in Scala!");import com.fp._;"""

import sbt._
import Keys._

object YarrBuild extends Build {
  object Dependencies {
    val slf4s = RootProject(uri("git://github.com/anta-baka/slf4s.git"))
  }

  lazy val yarr = Project("yarr", file(".")) dependsOn (Dependencies.slf4s) settings (
      name := "yarr",
      version := "0.0.1",
      scalaVersion := "2.10.2",
      scalacOptions += "-deprecation",
      resolvers += "Typesafe Repository" at "http://repo.typesafe.com/typesafe/releases/",
      libraryDependencies ++=
        Seq("com.typesafe.akka" %% "akka-actor" % "2.2-M3",
            "com.github.nscala-time" %% "nscala-time" % "0.4.0",
            "org.scalaz" %% "scalaz-core" % "7.0.0",
            "ch.qos.logback" % "logback-classic" % "1.0.13",
            "org.scalatest" % "scalatest_2.10" % "1.9.1" % "test")
    )
}

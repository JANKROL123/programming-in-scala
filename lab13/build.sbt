name := "lab13"
version := "1.0.0"

scalaVersion := "3.1.2"

scalacOptions := Seq("-unchecked", "-deprecation", "-encoding", "utf8")

libraryDependencies ++= {
  val akkaV = "2.6.19"
	Seq(
    "com.typesafe.akka" %% "akka-actor" % akkaV,
    "io.github.etspaceman" % "scalacheck-faker_2.13" % "7.0.0"
  )
}

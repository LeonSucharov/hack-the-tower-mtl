scalaOrganization := "org.typelevel"

scalacOptions ++= Seq(
  "-Ypartial-unification", 
  "-Yliteral-types"
)

scalaVersion := "2.11.8"

libraryDependencies += "org.scalatest" % "scalatest_2.11" % "2.2.1" % "test"

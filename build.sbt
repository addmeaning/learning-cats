name := "learning-cats"

version := "0.1"

scalaVersion := "2.12.5"

libraryDependencies +=
  "org.typelevel" %% "cats-core" % "1.0.0"
scalacOptions ++= Seq(
  "-Xfatal-warnings",
  "-Ypartial-unification"
)
val catsVersion = "0.9.0"
val catsAll = "org.typelevel" %% "cats" % catsVersion
val macroParadise = compilerPlugin("org.scalamacros" % "paradise" % "2.1.0" cross CrossVersion.full)
val kindProjector = compilerPlugin("org.spire-math" %% "kind-projector" % "0.6.3")
val resetAllAttrs = "org.scalamacros" %% "resetallattrs" % "1.0.0-M1"

val specs2Version = "3.8.8" // use the version used by discipline
val specs2Core  = "org.specs2" %% "specs2-core" % specs2Version
val specs2Scalacheck = "org.specs2" %% "specs2-scalacheck" % specs2Version
val scalacheck = "org.scalacheck" %% "scalacheck" % "1.13.4"

lazy val root = (project in file(".")).settings(
    organization := "com.github.wgx731",
    name := "learning-cats",
    scalaVersion := "2.11.8",
    libraryDependencies ++= Seq(
        catsAll,
        specs2Core % Test, specs2Scalacheck % Test, scalacheck % Test,
        macroParadise, kindProjector, resetAllAttrs
    ),
    scalacOptions ++= Seq(
        "-deprecation",
        "-encoding", "UTF-8",
        "-feature",
        "-language:_"
    )
)

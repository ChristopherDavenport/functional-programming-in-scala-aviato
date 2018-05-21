lazy val core = project.in(file("."))
    .settings(commonSettings)
    .settings(
      name := "functional-programming-in-scala-aviato"
    )

val catsV = "1.1.0"
val catsEffectV = "0.10.1"

val specs2V = "4.2.0"
val disciplineV = "0.8"
val scShapelessV = "1.1.6"


lazy val contributors = Seq(
  "ChristopherDavenport" -> "Christopher Davenport"
)

lazy val commonSettings = Seq(
  organization := "io.chrisdavenport",

  scalaVersion := "2.12.6",
  crossScalaVersions := Seq(scalaVersion.value, "2.11.12"),

  addCompilerPlugin("org.spire-math" % "kind-projector" % "0.9.6" cross CrossVersion.binary),
  addCompilerPlugin("com.olegpy" %% "better-monadic-for" % "0.2.4"),

  libraryDependencies ++= Seq(
    "org.typelevel"               %% "cats-core"                  % catsV,
    "org.typelevel"               %% "cats-effect"                % catsEffectV,

    "org.specs2"                  %% "specs2-core"                % specs2V       % Test,
    "org.specs2"                  %% "specs2-scalacheck"          % specs2V       % Test,
    "org.typelevel"               %% "discipline"                 % disciplineV   % Test,
    "com.github.alexarchambault"  %% "scalacheck-shapeless_1.13"  % scShapelessV  % Test
  )
)

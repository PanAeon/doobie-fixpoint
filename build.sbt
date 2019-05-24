

lazy val root = (project in file(".")).
  settings(
    inThisBuild(List(
      organization := "com.example",
      scalaVersion := "2.12.7",
      version      := "0.1.0-SNAPSHOT"
    )),
    name := "Doobie Fixpoint",
    scalacOptions += "-Ypartial-unification", // 2.11.9+
    libraryDependencies ++= Seq(

        "org.typelevel" %% "cats-core" % "1.4.0",
     // Start with this one
  "org.tpolecat" %% "doobie-core"      % "0.6.0",

  "org.tpolecat" %% "atto-core"    % "0.6.4",
  "org.tpolecat" %% "atto-refined" % "0.6.4",

  // And add any of these as needed
  "org.tpolecat" %% "doobie-h2"        % "0.6.0",          // H2 driver 1.4.197 + type mappings.
  "org.tpolecat" %% "doobie-hikari"    % "0.6.0",          // HikariCP transactor.
  "org.tpolecat" %% "doobie-postgres"  % "0.6.0",          // Postgres driver 42.2.5 + type mappings.
  "org.tpolecat" %% "doobie-specs2"    % "0.6.0" % "test", // Specs2 support for typechecking statements.
  "org.tpolecat" %% "doobie-scalatest" % "0.6.0" % "test",  // ScalaTest support for typechecking statements.

      "org.scalatest" %% "scalatest" % "3.0.5" % "test"
    )
  )

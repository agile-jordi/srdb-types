import com.typesafe.sbt.GitPlugin.autoImport._
import com.typesafe.sbt.GitVersioning
import com.gilcloud.sbt.gitlab.{GitlabCredentials,GitlabPlugin}

organization := "com.agilogy"

name := "srdb-types"

scalaVersion := "2.12.13"

crossScalaVersions := Seq("2.11.7","2.12.13")

resolvers += "Agilogy GitLab" at "https://gitlab.com/api/v4/groups/583742/-/packages/maven"

libraryDependencies ++= Seq(
  "org.postgresql" % "postgresql" % "9.3-1102-jdbc41" % "test",
  "org.scalatest" %% "scalatest" % "3.0.5" % "test",
  "org.scalamock" %% "scalamock-scalatest-support" % "3.6.0" % "test",
  "com.agilogy" %% "srdb-core" % "2.2" % "test"
)

// --> Linters

// See tinyurl.com/sd15lint

// https://tpolecat.github.io/2014/04/11/scalac-flags.html
scalacOptions ++= Seq(
  "-deprecation",
  "-encoding", "UTF-8",       // yes, this is 2 args
  "-feature",
  "-language:existentials",
  "-language:higherKinds",
  "-language:implicitConversions",
  "-unchecked",
  "-Xlint",
  "-Yno-adapted-args",
  "-Ywarn-numeric-widen",
  "-Ywarn-value-discard",
  "-Xfuture"
)

scalacOptions in (Compile,doc) ++= Seq("-groups", "-implicits", "-diagrams", "-skip-packages","")

// Execute static analysis via `lint:compile`
val LintTarget = config("lint").extend(Compile)

inConfig(LintTarget) {

  Defaults.compileSettings ++
    Seq(
      sources in LintTarget := {
        val lintSources = (sources in LintTarget).value
        lintSources ++ (sources in Compile).value
      },
      scalacOptions in LintTarget ++= Seq(
        "-Xfatal-warnings",
        "-Ywarn-unused-import",
        "-Ywarn-dead-code",
        "-P:linter:disable:PreferIfToBooleanMatch"
      ),
      wartremoverErrors ++= Warts.allBut(Wart.DefaultArguments, Wart.MutableDataStructures)
    )
}

scalacOptions in Compile := (scalacOptions in Compile).value filterNot { switch =>
  switch.startsWith("-P:wartremover:") ||
    "^-Xplugin:.*/org[.]brianmckenna/.*wartremover.*[.]jar$".r.pattern.matcher(switch).find ||
    switch.startsWith("-P:linter:") ||
    "^-Xplugin:.*/com[.]foursquare[.]lint/.*linter.*[.]jar$".r.pattern.matcher(switch).find
}

resolvers += "Linter Repository" at "https://hairyfotr.github.io/linteRepo/releases"

addCompilerPlugin("org.psywerx.hairyfotr" %% "linter" % "0.1.17")

scalastyleFailOnError := true

// <-- Linters

// Reformat at every compile.
// See https://github.com/sbt/sbt-scalariform

coverageExcludedPackages := "<empty>"

coverageHighlighting := false

enablePlugins(spray.boilerplate.BoilerplatePlugin)

// --> gitlab

GitlabPlugin.autoImport.gitlabGroupId := None
GitlabPlugin.autoImport.gitlabProjectId := Some(26236490)
GitlabPlugin.autoImport.gitlabDomain := "gitlab.com"

GitlabPlugin.autoImport.gitlabCredentials := {
    val token = sys.env.get("GITLAB_DEPLOY_TOKEN") match {
        case Some(token) => token
        case None =>
            sLog.value.warn(s"Environment variable GITLAB_DEPLOY_TOKEN is undefined, 'publish' will fail.")
            ""
    }
    Some(GitlabCredentials("Deploy-Token", token))
}

// <-- gitlab

enablePlugins(GitVersioning)

git.useGitDescribe := true

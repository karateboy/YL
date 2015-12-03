name := """YL"""

version := "1.0"

lazy val root = (project in file(".")).enablePlugins(PlayScala)

scalaVersion := "2.11.7"

libraryDependencies ++= Seq(
  jdbc,
  cache,
  ws,
  specs2 % Test,
  "com.github.nscala-time" %% "nscala-time" % "2.6.0",
  "org.scalikejdbc" %% "scalikejdbc"                  % "2.3.1",
  "org.scalikejdbc" %% "scalikejdbc-config"           % "2.3.1",
  "org.scalikejdbc" %% "scalikejdbc-play-initializer" % "2.4.3"  
)

resolvers += "scalaz-bintray" at "http://dl.bintray.com/scalaz/releases"

// Play provides two styles of routers, one expects its actions to be injected, the
// other, legacy style, accesses its actions statically.
//routesGenerator := InjectedRoutesGenerator


fork in run := false
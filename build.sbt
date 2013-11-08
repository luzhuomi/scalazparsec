import AssemblyKeys._

name := "scalazparsec"

organization := "com.github.luzhuomi"

version := "0.1.0"

scalaVersion := "2.9.2"


resolvers += "Apache HBase" at "https://repository.apache.org/content/repositories/releases"

resolvers += "Typesafe Repository" at "http://repo.typesafe.com/typesafe/releases/"

resolvers += "Maven Repository" at "http://mvnrepository.com/artifact/"

libraryDependencies += "org.scalaz" %% "scalaz-core" % "7.0.4"


seq(assemblySettings: _*)


mergeStrategy in assembly <<= (mergeStrategy in assembly) { (old) =>
  {
    case _ => MergeStrategy.last // leiningen build files
  }
}

excludedFiles in assembly := { (bases: Seq[File]) =>
  bases.filterNot(_.getAbsolutePath.contains("seshet")) flatMap { base => 
    //Exclude all log4j.properties from other peoples jars
    ((base * "*").get collect {
      case f if f.getName.toLowerCase == "log4j.properties" => f
    }) ++ 
    //Exclude the license and manifest from the exploded jars
    ((base / "META-INF" * "*").get collect {
      case f => f
    })
  }
}


publishMavenStyle := true


publishTo := {
  val nexus = "https://oss.sonatype.org/"
  Some("releases"  at nexus + "service/local/staging/deploy/maven2")
}

publishArtifact in Test := false

pomIncludeRepository := { _ => false }


pomExtra := (
  <url>http://luzhuomi.github.com/scalazparsec</url>
  <licenses>
    <license>
      <name>BSD-style</name>
      <url>http://www.opensource.org/licenses/bsd-license.php</url>
      <distribution>repo</distribution>
    </license>
  </licenses>
  <scm>
    <url>git@github.com:luzhuomi/scalazparsec.git</url>
    <connection>scm:git:git@github.com:luzhuomi/scalazparsec.git</connection>
  </scm>
  <developers>
    <developer>
      <id>luzhuomi</id>
      <name>Kenny Zhuo Ming Lu</name>
      <url>http://sites.google.com/site/luzhuomi</url>
    </developer>
  </developers>)
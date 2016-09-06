name := "scalazparsec-examples"

organization := "com.github.luzhuomi"

version := "0.1.1"

scalaVersion := "2.11.8"


resolvers += "Apache HBase" at "https://repository.apache.org/content/repositories/releases"

resolvers += "Typesafe Repository" at "http://repo.typesafe.com/typesafe/releases/"

resolvers += "Maven Repository" at "http://mvnrepository.com/artifact/"

resolvers += "OSS Sonatype" at "https://oss.sonatype.org/content/repositories/snapshots"

libraryDependencies += "org.scalaz" %% "scalaz-core" % "7.2.4"

libraryDependencies += "com.github.luzhuomi" %% "scalazparsec" % "0.1.3"


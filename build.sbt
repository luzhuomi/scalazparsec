name := "my-scalaz-test"

organization := "Nanyang Polytechnic"

version := "0.1.0"

scalaVersion := "2.9.2"


resolvers += "Apache HBase" at "https://repository.apache.org/content/repositories/releases"

resolvers += "Typesafe Repository" at "http://repo.typesafe.com/typesafe/releases/"

resolvers += "Maven Repository" at "http://mvnrepository.com/artifact/"

libraryDependencies += "org.scalaz" %% "scalaz-core" % "7.0.4"

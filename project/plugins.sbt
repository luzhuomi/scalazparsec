resolvers += Resolver.url("artifactory", url("http://scalasbt.artifactoryonline.com/scalasbt/sbt-plugin-releases"))(Resolver.ivyStylePatterns)

addSbtPlugin("com.eed3si9n" % "sbt-assembly" % "0.14.5")

// addSbtPlugin("net.virtual-void" % "sbt-dependency-graph" % "0.8.1")
// addSbtPlugin("net.virtual-void" % "sbt-dependency-graph" % "0.8.3-SNAPSHOT") // not yet read

addSbtPlugin("org.xerial.sbt" % "sbt-sonatype" % "2.0")

addSbtPlugin("com.jsuereth" % "sbt-pgp" % "1.1.0")
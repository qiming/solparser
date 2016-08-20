name := "solparsec"

version := "0.0.1"

scalaVersion := "2.11.8"

organization := "com.cloakapps"

resolvers += "Apache Repo" at "https://repository.apache.org/content/repositories/releases"

resolvers += "Typesafe Repository" at "http://repo.typesafe.com/typesafe/releases/"

resolvers += "Maven Repository" at "http://mvnrepository.com/artifact/"

//resolvers += "scalaz" at "git://github.com/scalaz/scalaz.git"

libraryDependencies += "org.scalaz" %% "scalaz-core" % "7.2.5"

libraryDependencies += "com.github.luzhuomi" %% "scalazparsec" % "0.1.0-SNAPSHOT"

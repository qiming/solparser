name := "solparsec"

version := "0.0.1"

scalaVersion := "2.11.8"

scalacOptions += "-feature"

organization := "com.cloakapps"

resolvers += "Apache Repo" at "https://repository.apache.org/content/repositories/releases"

resolvers += "Typesafe Repository" at "http://repo.typesafe.com/typesafe/releases/"

resolvers += "Maven Repository" at "http://mvnrepository.com/artifact/"

resolvers += "luzhuomi github repo" at "https://raw.githubusercontent.com/luzhuomi/mavenrepo/master/"

//resolvers += "scalaz" at "http://github.com/scalaz/mavenrepo/raw/master"

//libraryDependencies += "org.scalaz" %% "scalaz-core" % "7.2.5"

libraryDependencies += "com.github.luzhuomi" %% "scalazparsec" % "0.1.2"

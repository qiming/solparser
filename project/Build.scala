import sbt._  
object MyBuild extends Build {
    lazy val root = Project("root", file(".")) dependsOn scalazProject
    lazy val scalazProject =
        RootProject(uri("git://github.com/scalaz/scalaz.git"))
}

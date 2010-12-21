import sbt._

class Plugins(info: ProjectInfo) extends PluginDefinition(info) {
  val spde_sbt = "us.technically.spde" % "spde-sbt-plugin" % "0.4.2"
}

name := "robinson"

version := "0.1"

scalaVersion := "2.11.0"

libraryDependencies ++= Seq(
  "org.scalanlp"             %% "breeze"      % "1.0",
  "com.github.wookietreiber" %% "scala-chart" % "0.4.2",
  "org.jzy3d"                 % "jzy3d"       % "0.9" from ("http://www.jzy3d.org/release/0.9/org.jzy3d-0.9.jar", allowInsecureProtocol =
    true),
  ("org.jzy3d" % "jzy3d-deps" % "0.9" from ("http://www.jzy3d.org/release/0.9/org.jzy3d-0.9-dependencies.zip", allowInsecureProtocol =
    true)),
//  ("org.jzy3d" % "jzy3d-native-jogl-awt" % "0.9" from ("http://www.jzy3d.org/release/0.9/applet/jogl-all-natives-macosx-universal.jar", allowInsecureProtocol =
//    true)),
//  ("org.jzy3d" % "jzy3d-native-jogl-swing" % "0.9" from ("http://www.jzy3d.org/release/0.9/applet/jogl-all-natives-macosx-universal.jar", allowInsecureProtocol =
//    true)),
//  ("org.jzy3d" % "jzy3d-native-jogl-newt" % "0.9" from ("http://www.jzy3d.org/release/0.9/applet/jogl-all-natives-macosx-universal.jar", allowInsecureProtocol =
//    true)),
//  ("org.jzy3d" % "jzy3d-native-jogl-swt" % "0.9" from ("http://www.jzy3d.org/release/0.9/applet/jogl-all-natives-macosx-universal.jar", allowInsecureProtocol =
//    true)),
//  ("org.jzy3d" % "jzy3d-tester-native" % "0.9" from ("http://www.jzy3d.org/release/0.9/applet/jogl-all-natives-macosx-universal.jar", allowInsecureProtocol =
//    true)),
  "com.ardor3d" % "ardor3d-jogl" % "0.9"
)

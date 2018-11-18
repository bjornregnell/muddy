scalaVersion := "2.12.7"
fork                := true // https://stackoverflow.com/questions/18676712
connectInput        := true // http://www.scala-sbt.org/1.x/docs/Forking.html
outputStrategy      := Some(StdoutOutput)
scalacOptions       := Seq("-unchecked", "-deprecation")

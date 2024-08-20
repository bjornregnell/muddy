           scalaVersion := "2.12.19"
          scalacOptions := Seq("-unchecked", "-deprecation")
                   fork := true // https://stackoverflow.com/questions/18676712
           connectInput := true // http://www.scala-sbt.org/1.x/docs/Forking.html
         outputStrategy := Some(StdoutOutput)
   cancelable in Global := true // https://stackoverflow.com/questions/5137460

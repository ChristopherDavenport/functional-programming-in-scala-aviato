package fpis

object Main {

  def main(args: Array[String]): Unit = {
    import fpis.chapter07.Parallelism._
    import java.util.concurrent.Executors

    val es = Executors.newFixedThreadPool(10)

    val p = Par.parMap(List.range(1, 10))(math.sqrt(_))
    
    
    try {
      val structure  = println(p)
      val output = Par.unsafeRunSync(p)(es)
      println(output)
    } finally {
      es.shutdown()
    }
    

  }

}
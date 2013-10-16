import org.scalatest.FunSuite
import org.scalatest.Matchers
import com.fp.ch7._
import java.util.concurrent.{ExecutorService, TimeUnit, LinkedBlockingQueue, ThreadPoolExecutor}

class Ch7 extends FunSuite with Matchers {
  def partest(name: String, core: Int = 5, max: Int = 20)(f: ExecutorService => Unit): Unit = {
    test(name) {
      val executor = new ThreadPoolExecutor(
        core, max,
        1000L, TimeUnit.MILLISECONDS,
        new LinkedBlockingQueue[Runnable]())
      executor should not be (null)
      f(executor)
      executor.shutdown()
    }
  }

  partest("7.2: basic execution") { es =>
    pending
    val p1 = Par.unit(123)
    Par.run(es)(p1).get should be (123)

    val p2 = Par.async(123)
    Par.run(es)(p2).get(1000L, TimeUnit.MILLISECONDS) should be (123)
  }

  partest("7.1: map2") { es =>
    pending
    val p1 = Par.unit(123)
    val p2 = Par.unit(456)
    val p3 = Par.map2(p1, p2)((x, y) => x + y)
    Par.run(es)(p3).get should be (579)
  }

  // TODO test async, timeout, ...

  partest("7.4 asyncF") { es =>
    pending
    val f = Par.asyncF[Int, String](_.toString)
    Par.run(es)(f(123)).get should be ("123")
  }

 partest("?.? parMap", 10, 100) { es =>
   pending
   val strs = Par.parMap(List(1,2,3,4,5))(_.toString)
   Par.run(es)(strs).get should be (List("1", "2", "3", "4", "5"))
 }
}

import org.scalatest.FunSuite
import org.scalatest.Matchers
import com.fp.ch3._

class Ch3 extends FunSuite with Matchers {
  import List._

  test("3.1: case reasoning") {
    val x = List(1, 2, 3, 4, 5) match {
      case Cons(x, Cons(2, Cons(4, _)))          => x
      case Nil                                   => 42
      case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
      case Cons(h, t)                            => h + sum(t)
      case _                                     => 101
    }
    x should be (3)
  }

  test("3.2: tail") {
    List(1, 2, 3).tail should be (List(2, 3))
    List(2, 3).tail should be (List(3))
    List(3).tail should be (Nil)
    // Nil.tail is undefined
  }

  test("3.3: setHead") {
    List(1, 2).setHead(3) should be (List(3, 2))
    List(1).setHead(3) should be (List(3))
    Nil.setHead(2) should be (Nil)
  }

  test("3.4: drop") {
    List(1, 2, 3).drop(1) should be (List(2, 3))
    List(1, 2, 3).drop(2) should be (List(3))
    List(1, 2).drop(2) should be (Nil)
    List(1).drop(1) should be (Nil)
  }

  test("3.5: dropWhile") {
    List[String]("the", "lazy", "dog").dropWhile(_.length <= 3) should be (List("lazy", "dog"))
    List(1, 2, 3).dropWhile(_ > 3) should be (List(1, 2, 3))
    Nil.dropWhile(_ => false) should be (Nil)
  }

  test("3.6: init") {
    List(1, 2, 3).init should be (List(1, 2))
    List(1).init should be (Nil)
    // Nil.init is undefined
  }

  test("?.?: foldRight") {
    (Nil: List[Int]).foldRight(1)(_ + _) should be (1)
    List(3.0, 2.0).foldRight(2.0)(_ * _) should be (12)
    List(1, 2, 3).foldRight(Nil: List[Int])(Cons.apply) should be (List(1,2,3))
  }

  test("?.?: length") {
    Nil.length should be (0)
    List(1).length should be (1)
    List(1, 2).length should be (2)
  }

  test("?.?: foldLeft") {
    (Nil: List[Int]).foldLeft(1)(_ + _) should be (1)
    List(2.0).foldLeft(3.0)(_ * _) should be (6.0)
    List(1, 3, 2).foldLeft(0)((a, b) => if (b > a) b else a) should be (3)
  }

  test("?.?: lengthLeft") {
    // same tests as length
    Nil.length should be (0)
    List(1).length should be (1)
    List(1, 2).length should be (2)
  }

  test("?.?: reverse") {
    Nil.reverse should be (Nil)
    List(1).reverse should be (List(1))
    List(1, 2).reverse should be (List(2, 1))
    List(1, 2, 3).reverse should be (List(3, 2, 1))
  }

  test("?.?: concat") {
    Nil concat Nil should be (Nil)
    Nil concat List(1, 2, 3) should be (List(1, 2, 3))
    List(1, 2, 3) concat Nil should be (List(1, 2, 3))
    List(1) concat List(2, 3) should be (List(1, 2, 3))
    List(1) concat List(2, 3) should be (List(1, 2, 3))
    List(1, 2) concat List(3) should be (List(1, 2, 3))
  }

  test("?.?: append") {
    Nil append 1 should be (List(1))
    List(1) append 2 should be (List(1, 2))
    List(1, 2) append 3 should be (List(1, 2, 3))
  }

  test("?.?: foldRight2") {
    // same tests as foldRight
    (Nil: List[Int]).foldRight(1)(_ + _) should be (1)
    List(3.0, 2.0).foldRight(2.0)(_ * _) should be (12)
    List(1, 2, 3).foldRight(Nil: List[Int])(Cons.apply) should be (List(1,2,3))
  }

  test("?.?: map") {
    (Nil: List[Int]) map (_ * 2) should be (Nil)
    List(1) map (_ * 2) should be (List(2))
    List(1, 2) map (_ * 2) should be (List(2, 4))
  }

  test("?.?: filter") {
    (Nil: List[Int]) filter (_ > 1) should be (Nil)
    List(1) filter (_ > 1) should be (Nil)
    List(1, 2, 3) filter (_ % 2 == 0) should be (List(2))
  }

  test("?.?: filter2") {
    (Nil: List[Int]) filter2 (_ > 1) should be (Nil)
    List(1) filter2 (_ > 1) should be (Nil)
    List(1, 2, 3) filter2 (_ % 2 == 0) should be (List(2))
  }

  test("?.?: zip") {
    (Nil: List[Int]).zip(List("the", "lazy", "dog"))(_ -> _) should be (Nil)
    List(1).zip(List("the", "lazy", "dog"))(_ -> _) should be (List(1 -> "the"))
    List(1, 2).zip(List("lazy"))(_ -> _) should be (List(1 -> "lazy"))
    List(1, 2).zip(List("lazy", "dog"))(_ -> _) should be (List(1 -> "lazy", 2 -> "dog"))
  }

  test("?.?: hasSubsequence") {
    (Nil: List[Int]).hasSubsequence(List(1)) should be (false)
    (Nil: List[Int]).hasSubsequence(Nil) should be (true)
    List(1, 2, 3).hasSubsequence(List(2, 3)) should be (true)
    List(1, 2, 3).hasSubsequence(List(2, 3, 1)) should be (false)
    List(1, 2, 3).hasSubsequence(List(2, 3, 1)) should be (false)
    List(1, 2, 3).hasSubsequence(Nil) should be (true)
    List(1, 2, 3).hasSubsequence(List(1)) should be (true)
  }

  test("?.?: sum") {
    sum(Nil) should be (0)
    sum(List(1)) should be (1)
    sum(List(1, 2)) should be (3)
    sum(List(1, 2, 3)) should be (6)
  }

  test("?.?: product") {
    product(Nil) should be (1)
    product(List(2.0)) should be (2.0)
    product(List(2.0, 3.0)) should be (6.0)
    product(List(2.0, 3.0, 4.0)) should be (24.0)
  }

  test("?.?: sum2") {
    sum2(Nil) should be (0)
    sum2(List(1)) should be (1)
    sum2(List(1, 2)) should be (3)
    sum2(List(1, 2, 3)) should be (6)
  }

  test("?.?: product2") {
    product2(Nil) should be (1)
    product2(List(2.0)) should be (2.0)
    product2(List(2.0, 3.0)) should be (6.0)
    product2(List(2.0, 3.0, 4.0)) should be (24.0)
  }

  test("?.?: sumLeft") {
    sumLeft(Nil) should be (0)
    sumLeft(List(1)) should be (1)
    sumLeft(List(1, 2)) should be (3)
    sumLeft(List(1, 2, 3)) should be (6)
  }

  test("?.?: productLeft") {
    productLeft(Nil) should be (1)
    productLeft(List(2.0)) should be (2.0)
    productLeft(List(2.0, 3.0)) should be (6.0)
    productLeft(List(2.0, 3.0, 4.0)) should be (24.0)
  }

  test("?.?: flatten") {
    flatten(Nil) should be (Nil)
    flatten(List(Nil)) should be (Nil)
    flatten(List(List(1),List(2, 3), List(4, 5, 6))) should be (List(1, 2, 3, 4, 5, 6))
  }

  test("?.?: incrAll") {
    incrAll(Nil) should be (Nil)
    incrAll(List(1)) should be (List(2))
    incrAll(List(1, 2, 3)) should be (List(2, 3, 4))
  }

  test("?.?: doubleStrings") {
    doubleStrings(Nil) should be (Nil)
    doubleStrings(List(1.0)) should be (List("1.0"))
    doubleStrings(List(1.0, 2.0, 3.0)) should be (List("1.0", "2.0", "3.0"))
  }

  test("?.?: zipAdd") {
    zipAdd(Nil, List(1, 2, 3)) should be (Nil)
    zipAdd(List(1, 2), List(3)) should be (List(4))
    zipAdd(List(1, 2, 3), List(3, 2, 1)) should be (List(4, 4, 4))
  }

  import Tree._

  test("?.?: Tree maxInt") {
    maxInt(Leaf(5)) should be (5)
    maxInt(Branch(Leaf(1), Leaf(2))) should be (2)
    maxInt(Branch(Leaf(3), Leaf(2))) should be (3)
    maxInt(Branch(Leaf(3), Leaf(4))) should be (4)
  }

  test("?.?: Tree size") {
    Leaf(1).size should be (1)
    Branch(Leaf(3), Branch(Leaf(4), Leaf(3))).size should be (5)
  }

  test("?.?: Tree depth") {
    Leaf(1).depth should be (0)
    Branch(Leaf(3), Leaf(2)).depth should be (1)
    Branch(Branch(Leaf(1), Leaf(2)), Leaf(3)).depth should be (2)
    Branch(Leaf(3), Branch(Leaf(1), Leaf(2))).depth should be (2)
  }

  test("?.?: Tree map") {
    Leaf(1).map(_ * 2) should be (Leaf(2))
    Branch(Leaf(3), Leaf(2)).map(_ * 2) should be (Branch(Leaf(6), Leaf(4)))
  }

  test("?.?: Tree fold") {
    Leaf(1).fold[Int](_ * 2, (_, _) => 1) should be (2)
    Branch(Leaf(1), Leaf(2)).fold[Int](_ * 2, _ + _) should be (6)
  }

  test("?.?: Tree sizeFold") {
    Leaf(1).sizeFold should be (1)
    Branch(Leaf(3), Branch(Leaf(4), Leaf(3))).sizeFold should be (5)
  }

  test("?.?: Tree depthFold") {
    Leaf(1).depthFold should be (0)
    Branch(Leaf(3), Leaf(2)).depthFold should be (1)
    Branch(Branch(Leaf(1), Leaf(2)), Leaf(3)).depthFold should be (2)
    Branch(Leaf(3), Branch(Leaf(1), Leaf(2))).depthFold should be (2)
  }

  test("?.?: Tree mapFold") {
    Leaf(1).mapFold(_ * 2) should be (Leaf(2))
    Branch(Leaf(3), Leaf(2)).mapFold(_ * 2) should be (Branch(Leaf(6), Leaf(4)))
  }

  test("?.?: Tree maxIntFold") {
    // same tests as maxInt
    maxIntFold(Leaf(5)) should be (5)
    maxIntFold(Branch(Leaf(1), Leaf(2))) should be (2)
    maxIntFold(Branch(Leaf(3), Leaf(2))) should be (3)
    maxIntFold(Branch(Leaf(3), Leaf(4))) should be (4)
  }
}

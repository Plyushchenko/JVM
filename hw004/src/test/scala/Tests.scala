import org.scalatest._

class Tests extends FlatSpec with MustMatchers {
  "ImmutableMultiset" should "have correct size" in {
    ImmutableMultiset(1, 2, 3).size must equal (3)
    ImmutableMultiset(1, 2, 3, 2, 2, 2).size must equal (6)
    ImmutableMultiset().size must equal (0)
    ImmutableMultiset.empty[Double].size must equal (0)
    ImmutableMultiset.empty[String].isEmpty must equal (true)
    ImmutableMultiset().isEmpty must equal (true)
    ImmutableMultiset().nonEmpty must equal (false)
    ImmutableMultiset(1, 2, 3, 2, 2, 2).nonEmpty must equal (true)
  }

  "Element occurrences" should "be counted correctly" in {
    val a = ImmutableMultiset("Back", "back", "back", "back", "up")
    a.count("back") must equal (3)
    a("Back") must equal (1)
    a.contains("up") must equal (true)
    a.count(123456) must equal (0)
    a.contains(15.96) must equal (false)
    (a - "back" - "back").count("back") must equal (1)
    val b = ImmutableMultiset("up", "up", "up")
    (a & b).contains("Back") must equal(false)
    (a | b).count("up") must equal(3)
    (b + "x" + "x" + "L").count("x") must equal(2)
  }

  "Comparison" should "be correct" in {
    var a = ImmutableMultiset(1, 2, 2, 2, 3, 3, 999, 5, 4, 8, 1)
    var b = ImmutableMultiset(1, 2, 999, 4, 8, 5)
    (a | b) must equal (a)
    a mustNot equal (b)
    (a & b) must equal (b)
    (b + 2 + 2 + 3 + 3 + 1) must equal (a)
    a = ImmutableMultiset()
    (a | b) must equal (b)
    (a & b) must equal (a)
    a mustNot equal (b)
    b = ImmutableMultiset()
    a must equal (b)
  }

  "Find and filter" should "be correct" in {
    val a = ImmutableMultiset(1.0, math.Pi, math.E, math.Pi, math.Pi, math.E)
    a.find(x => x > 3) must equal (Some(math.Pi))
    a.find(x => a.count(x) == 2) must equal (Some(math.E))
    a.find(x => x < 0) must equal (None)
    a.filter(x => x > 1) must equal (a - 1.0)
    a.filter(x => 1 < x && x < 3) must equal(ImmutableMultiset(math.E, math.E))
  }

  "For-comprehension and pattern matching" should "be correct" in {
    val a = ImmutableMultiset(("A", "a"), ("B", "b"), ("C", "c"))
    for (x <- a)
      x._1.toLowerCase must equal (x._2)
    for ((y, z) <- a)
      z.toUpperCase must equal(y)
    (a match {
      case ImmutableMultiset(_) => -1
      case ImmutableMultiset(_*) => 1
    }) must equal (1)
  }

  "Map and flatMap" should "be correct" in {
    val a = ImmutableMultiset(5, 5, 5, 3, 3, 3)
    a.map((x: Int) => x * x) must equal (ImmutableMultiset(9, 9, 25, 25, 9, 25))
    a.map((x: Int) => x / 20.0) must equal (ImmutableMultiset(0.25, 0.25, 0.25, 0.15, 0.15, 0.15))
    a.flatMap((x: Int) => List.fill(10)(0)) must equal (ImmutableMultiset(List.fill(60)(0)))
  }

}

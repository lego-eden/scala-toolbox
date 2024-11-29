class ToolboxTest extends munit.FunSuite:
  
  test("intersperse between all elements"):
    val xs = Vector(1, 2, 3)
    assertEquals(Vector(1, 0, 2, 0, 3), xs.intersperse[Int](0))

    val s = "123"
    assertEquals("1 2 3", s.intersperse(' '))

  test("intersperse with larger spacing"):
    val xs = (1 to 9).toVector
    assertEquals(Vector(1, 2, 3, 0, 4, 5, 6, 0, 7, 8, 9), xs.intersperse[Int](0, 3))

    val ys = (1 to 8).toVector
    assertEquals(Vector(1, 2, 0, 3, 4, 0, 5, 6, 0, 7, 8), ys.intersperse[Int](0, 2))

  test("padleft"):
    val xs = Vector(1, 2, 3).padLeft[Int](5, 0)
    assertEquals(xs, Vector(0, 0, 1, 2, 3))
    assertEquals("hej".padLeft(6, ' '), "   hej")

  test("filterWithIndex"):
    val xs = Vector(1, 2, 3, 4).filterWithIndex: (e, i) =>
      i % 2 == 0
    assertEquals(xs, Vector(1, 3))

  test("isEven"):
    val result = (0 to 1000 by 2).forall(i => i.isEven)
    assertEquals(result, true)

  test("isOdd"):
    val result = (1 to 1001 by 2).forall(i => i.isOdd)

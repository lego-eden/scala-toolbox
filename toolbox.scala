import scala.util.{Try, Success, Failure}

private opaque type Invariant[A] = Nothing

infix type ==[A, B] <: Boolean = Invariant[A] match
  case Invariant[B] => true
  case _            => false

infix type *:*[A, B] <: Tuple = (A, B) match
  case (Tuple, Tuple) => Tuple.Concat[A, B]
  case (Tuple, _)     => Tuple.Append[A, B]
  case (_, Tuple)     => A *: B
  case (_, _)         => (A, B)

extension [T](n: T)(using num: Numeric[T])
  def **(other: Int): T =
    if other == 0 then num.one
    else if other == 1 then n
    else
      (1 to other).foldLeft(num.one): (acc, _) =>
        num.times(acc, n)

extension [T](n: T)(using num: Integral[T])
  def isEven: Boolean = num.rem(n, num.fromInt(2)) == num.zero
  def isOdd: Boolean = !n.isEven

extension [A](a: A)
  def *:*[B](b: B): A *:* B = ((a, b) match
    case (a: Tuple, b: Tuple) => a ++ b
    case (a: Tuple, b)        => a :* b
    case (a, b: Tuple)        => a *: b
    case (a, b)               => (a, b)
  ).asInstanceOf[A *:* B]

  def tap[U](f: A => U): A =
    f(a)
    a

  def pipe[B](f: A => B): B = f(a)

  def log(): A = a.tap(println)

extension [A, CC[+x] <: scala.collection.SeqOps[x, CC, CC[x]]](xs: CC[A])
  def padLeft[B >: A](len: Int, elem: B): CC[B] =
    val padSize = (len - xs.size) max 0
    val padding = xs.iterableFactory.fill(padSize)(elem)
    xs.prependedAll(padding)

  def intersperse[B >: A](elem: B, spacing: Int): CC[B] =
    val indices = spacing to (xs.size - 1) by spacing
    xs.flatMap(x =>
      if indices contains x then xs.iterableFactory(x, elem)
      else xs.iterableFactory(x)
    )

  def intersperse[B >: A](elem: B): CC[B] =
    xs.init.flatMap(x => xs.iterableFactory(x, elem)) :+ xs.last

  def interleave[B >: A](elems: IterableOnce[B]): CC[B] =
    xs.init.zip(elems).flatMap((a, b) => xs.iterableFactory(a, b)) :+ xs.last

  def spread[B >: A](elems: IterableOnce[B]): CC[B] =
    val iter = elems.iterator
    val spacing = Try:
      xs.size / iter.size
    spacing match
      case Failure(_) => xs
      case Success(spacing) if spacing <= 1 =>
        xs.interleave(elems)
      case Success(spacing) =>
        val indices = spacing to (xs.size - 1) by spacing
        xs.zipWithIndex.flatMap: (x, i) =>
          if indices contains i then xs.iterableFactory(xs(i), iter.next())
          else xs.iterableFactory(x)

  def remove(index: Int): CC[A] =
    xs.patch(index, Nil, 1)

extension [A, CC[+x] <: scala.collection.IterableOnceOps[x, CC, CC[x]]](xs: CC[A])
  def filterWithIndex(p: (A, Int) => Boolean): CC[A] =
    xs.zipWithIndex.collect:
      case (a, i) if p(a, i) => a 

extension (s: String)
  def padLeft(len: Int, elem: Char): String =
    val padSize = (len - s.size) max 0
    val padding: String = Seq.fill(padSize)(elem).mkString
    padding + s    

  def intersperse(elem: Char): String =
    s.init.flatMap(c => s"$c$elem") :+ s.last

  def intersperse(elem: String): String =
    s.init.flatMap(c => s"$c$elem") :+ s.last
  
  def interleave(elems: String): String =
    s.init.zip(elems).flatMap((c1, c2) => s"$c1$c2").mkString :+ s.last

  def spread(elems: String): String = 
    val iter = elems.iterator
    val spacing = Try:
      s.size / iter.size
    spacing match
      case Failure(_) => s
      case Success(spacing) if spacing <= 1 => 
        s.interleave(elems)
      case Success(spacing) =>
        val indices = spacing to (s.size - 1) by spacing
        s.zipWithIndex.flatMap: (c, i) =>
          if indices contains i then s"${s(i)}${iter.next()}"
          else c.toString
        .mkString

  def remove(index: Int): String =
    s.patch(index, Nil, 1)
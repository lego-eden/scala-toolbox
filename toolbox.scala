private opaque type Invariant[A] = Nothing 
infix type ==[A, B] <: Boolean = Invariant[A] match
  case Invariant[B] => true
  case _ => false

infix type *:*[A, B] <: Tuple = (A, B) match
  case (Tuple, Tuple) => Tuple.Concat[A, B]
  case (Tuple, _) => Tuple.Append[A, B]
  case (_, Tuple) => A *: B
  case (_, _) => (A, B)

extension [A](a: A)
  def *:*[B](b: B): A *:* B = ((a, b) match
    case (a: Tuple, b: Tuple) => a ++ b
    case (a: Tuple, b) => a :* b
    case (a, b: Tuple) => a *: b
    case (a, b) => (a, b)
  ).asInstanceOf[A *:* B]

  def tap(proc: A => Unit): A =
    proc(a)
    a
  
  def log(): A = a.tap(println)

extension [A, CC[x] <: IterableOnce[x]](xs: scala.collection.SeqOps[A, CC, CC[A]])
  def padLeft[B >: A](len: Int, elem: B): CC[B] =
    val padSize = (len - xs.size) max 0
    val padding = xs.iterableFactory.fill(padSize)(elem)
    xs.prependedAll(padding)

extension (s: String)
  def padLeft(len: Int, elem: Char): String =
    val padSize = (len - s.size) max 0
    val padding: String = Seq.fill(padSize)(elem).mkString
    padding + s

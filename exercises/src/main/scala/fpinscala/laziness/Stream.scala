package fpinscala.laziness

import Stream._
trait Stream[+A] {

  def foldRight[B](z: => B)(f: (A, => B) => B): B = // The arrow `=>` in front of the argument type `B` means that the function `f` takes its second argument by name and may choose not to evaluate it.
    this match {
      case Cons(h,t) => f(h(), t().foldRight(z)(f)) // If `f` doesn't evaluate its second argument, the recursion never occurs.
      case _ => z
    }

  def exists(p: A => Boolean): Boolean = 
    foldRight(false)((a, b) => p(a) || b) // Here `b` is the unevaluated recursive step that folds the tail of the stream. If `p(a)` returns `true`, `b` will never be evaluated and the computation terminates early.

  @annotation.tailrec
  final def find(f: A => Boolean): Option[A] = this match {
    case Empty => None
    case Cons(h, t) => if (f(h())) Some(h()) else t().find(f)
  }

  def toList: List[A] = {
    @annotation.tailrec
    def go(s: Stream[A], acc: List[A]): List[A] = s match {
      case Cons(h, t) => go(t(), h() :: acc)
      case _ => acc
    }
    go(this, List()).reverse
  }

  def take(n: Int): Stream[A] = this match {
    case Cons(h, t) if n > 1 => cons(h(), t().take(n-1))
    case Cons(h, _) if n == 1 => cons(h(), empty)
    case _ => empty
  }

  @annotation.tailrec
  final def drop(n: Int): Stream[A] = this match {
    case Cons(_, t) if n > 0 => t().drop(n-1)
    case _ => this
  }

  def takeWhile(p: A => Boolean): Stream[A] = this match {
    case Cons(h, t) if (p(h()) == false) => cons(h(), t().takeWhile(p))
    case _ => empty
  }

  def takeWhile1(p: A => Boolean): Stream[A] = {
    foldRight(empty[A])((a, b) => if (p(a)) cons(a, b) else empty)
  }

  def forAll(p: A => Boolean): Boolean = {
    foldRight(true)((a, b) => p(a) && b)
  }

  def headOption: Option[A] = {
    foldRight(None: Option[A])((a, _) => Some(a))
  }

  // 5.7 map, filter, append, flatmap using foldRight. Part of the exercise is
  // writing your own function signatures.

  def map[B](f: A => B): Stream[B] = {
    foldRight(empty[B])((h, t) => cons(f(h), t))
  }

  def filter(f: A => Boolean): Stream[A] = {
    foldRight(empty[A])((a, b) =>  if (f(a)) b else cons(a, b))
  }

  def flatMap[B](f: A => Stream[B]): Stream[B] = {
    foldRight(empty[B])((a, b) => f(a) append b)
  }

  def append[B>:A](s: => Stream[B]): Stream[B] = {
    foldRight(s)((a, b) => cons(a, b))
  }


  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = f(z) match {
    case Some((h, s)) => cons(h, unfold(s)(f))
    case None => empty
  }

  def takeViaUnfold(n: Int): Stream[A] = {
    unfold((this, n)){
      case (Cons(_, _), 0) => None
      case (Cons(h, t), n) => Some(h(), (t(), n-1))
    }
  }

  def takeWhileViaUnfold(p: A => Boolean): Stream[A] = {
    unfold(this){
      case Cons(h, t) if p(h()) => Some((h(), t()))
      case _ => None
    }
  }

  def mapViaUnfold[B](f: A => B): Stream[B] = {
    unfold(this){
      case Cons(h, t) => Some(f(h()), t())
      case _ => None
    }
  }

  def zipWithViaUnfold[B,C](s2: Stream[B])(f: (A,B) => C): Stream[C] = {
    unfold((this, s2)){
      case (Cons(a1, a2), Cons(b1, b2)) => Some(f(a1(), b1()), (a2(), b2()))
      case _ => None
    }
  }

  def zipAllViaUnfold[B](s2: Stream[B]): Stream[(Option[A], Option[B])] = {
    unfold((this, s2)){
      case (Cons(h, t), Cons(a, b)) => Some((Some(h()), Some(a())), (t(), b()))
      case (Cons(h, t), Empty) => Some((Some(h()), None), (t(), empty))
      case (Empty, Cons(h, t)) => Some((None, Some(h())), (empty, t()))
      case _ => None
    }
  }

  def tails: Stream[Stream[A]] = {
    unfold(this) {
      case p@Cons(_, t) => Some((p, t()))
      case _ => None
    } append Stream(empty)
  }

  def scanRight[B](z: => B)(f: (A, => B) => B): Stream[B] = {
    unfold(this) {
      case Empty => None
      case s => Some(s.foldRight(z)(f), s drop 1)
    } append Stream(z)
  }

  def scanRight1[B](z: => B)(f: (A, => B) => B): Stream[B] = {
    foldRight((z, Stream(z)))((a, p0) => {
      lazy val p1 = p0
      val b2 = f(a, p1._1)
      (b2, cons(b2, p1._2))
    })._2
  }

  def startsWith[B](s: Stream[B]): Boolean = {
    zipWithViaUnfold(s)(_ == _) forAll(_ == true)
  }
}
case object Empty extends Stream[Nothing]
case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

object Stream {
  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)
  }

  def empty[A]: Stream[A] = Empty

  def apply[A](as: A*): Stream[A] =
    if (as.isEmpty) empty 
    else cons(as.head, apply(as.tail: _*))

  val ones: Stream[Int] = Stream.cons(1, ones)

  def constant[A](a: A): Stream[A] = {
    lazy val tail: Stream[A] = Cons(() => a, () => tail)
    tail
  }

  def from(n: Int): Stream[Int] = cons(n, from(n+1))

  def fibs: Stream[Int] = {
    def helper(a: Int, b: Int): Stream[Int] = cons(a, helper(b, a+b))
    helper(0, 1)
  }

  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = f(z) match {
    case Some((h, s)) => cons(h, unfold(s)(f))
    case None => empty
  }

  val ones1: Stream[Int] = unfold(1)(_ => Some((1, 1)))

  def from1(n: Int): Stream[Int] = {
    unfold(n)(x => Some(x, x+1))
  }

  def fibs1: Stream[Int] = {
    unfold[Int, (Int, Int)]((0, 1)) { case (x, y) => Some(x, (y, x + y)) }
  }

  def constant1[A](a: A): Stream[A] = {
    unfold(a)(_ => Some((a, a)))
  }

  def map[A, B](a: Stream[A])(f: A => B): Stream[B] = {
    unfold(a) {
      case Cons(h, t) => Some((f(h()), t()))
      case Empty => None
    }
  }

  def take[A](a: Stream[A], n: Int): Stream[A] = {
    unfold[A, (Stream[A], Int)]((a, n)){
      case (_, 0) => None
      case (Cons(h, t), b) => Some((h(), (t(), b-1)))
    }
  }

  def takeWhile[A](a: Stream[A])(p: A => Boolean): Stream[A] = {
    unfold(a){
      case Cons(h, t) if p(h()) => Some((h(), t()))
      case _ => None
    }
  }

  def zipAll[A, B](s1: Stream[A], s2: Stream[B]): Stream[(Option[A], Option[B])] = {
    unfold((s1, s2)){
      case (Cons(h, t), Cons(a, b)) => Some((Some(h()), Some(a())), (t(), b()))
      case (Cons(h, t), Empty) => Some((Some(h()), None), (t(), empty))
      case (Empty, Cons(h, t)) => Some((None, Some(h())), (empty, t()))
      case _ => None
    }
  }

  def main(args: Array[String]): Unit = {
    println(Stream(1,2,3).take(2).toList)
    println(Stream(1,2,3).drop(2).toList)
    println(Stream(1,2,3).takeWhile(x => if (x==3) true else false).toList)
    println(Stream(1,2,3).takeWhile1(x => if (x!=1) true else false).toList)
    println(Stream(1,2,3).takeWhile1(x => if (x!=2) true else false).toList)
    println(Stream(1,2,3).takeWhile1(x => if (x!=3) true else false).toList)
    println(Stream(1,2,3).forAll( x => x != 0))
    println(Stream(1,2,3).headOption)
    println(Stream[Int]().headOption)
    println(Stream(1,2,3).map(x => x.toString).toList)
    println(Stream(1,2,3).map(x => x + 1).toList)
    println(Stream(1,2,3).filter(x => x % 2 != 0).toList)
    println(Stream(1,2,3).append(Stream(4,5,6)).toList)
    println(Stream(1,2,3).flatMap(x => cons(x, cons(x, empty))).toList)
    println(ones.map(_ + 1).exists(_ % 2 == 0))
    println(ones.takeWhile(_ == 1))
    println(ones.forAll(_ != 1))
    println(fibs1.take(10).toList)
    println(from1(7).take(10).toList)
    println(constant1(8).take(10).toList)
    println(ones1.map(_ + 1).exists(_ % 2 == 0))
    println(ones1.takeWhile(_ == 1))
    println(ones1.forAll(_ != 1))
    println(map(Stream(1,2,3))(x => x.toString).toList)
    println(map(Stream(1,2,3))(x => x + 1).toList)
    println(take(Stream(1,2,3), 2).toList)
    println(takeWhile(Stream(1,2,3))(x => if (x>0) true else false).toList)
    println(takeWhile(Stream(1,2,3))(x => if (x>1) true else false).toList)
    println(takeWhile(Stream(1,2,3))(x => if (x>2) true else false).toList)
    println(zipAll(Stream(1,2,3), Stream(4,5,6)).toList)
    println(zipAll(Stream(1,2,3,4), Stream(5,6,7)).toList)
    println(zipAll(Stream(1,2,3), Stream(4,5,6,7)).toList)
    println(Stream(1,2,3).startsWith(Stream(1,2)))
    println(Stream(1,2,3).startsWith(Stream(1,2,3)))
    println(Stream(1,2,3).startsWith(Stream(2,3)))
    println(Stream(1,2,3).tails.map(_.toList).toList)
    println(Stream(1,2,3).scanRight(0)(_ + _).toList)
  }

}
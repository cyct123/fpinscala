package fpinscala.datastructures

import fpinscala.datastructures.List.x

sealed trait List[+A] // `List` data type, parameterized on a type, `A`
case object Nil extends List[Nothing] // A `List` data constructor representing the empty list
/* Another data constructor, representing nonempty lists. Note that `tail` is another `List[A]`,
which may be `Nil` or another `Cons`.
 */
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List { // `List` companion object. Contains functions for creating and working with lists.
  def sum(ints: List[Int]): Int = ints match { // A function that uses pattern matching to add up a list of integers
    case Nil => 0 // The sum of the empty list is 0.
    case Cons(x,xs) => x + sum(xs) // The sum of a list starting with `x` is `x` plus the sum of the rest of the list.
  }

  def product(ds: List[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x,xs) => x * product(xs)
  }

  def apply[A](as: A*): List[A] = // Variadic function syntax
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  val x = List(1,2,3,4,5) match {
    case Cons(x, Cons(2, Cons(4, _))) => x
    case Nil => 42
    case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
    case Cons(h, t) => h + sum(t)
    case _ => 101
  }

  def append[A](a1: List[A], a2: List[A]): List[A] =
    a1 match {
      case Nil => a2
      case Cons(h,t) => Cons(h, append(t, a2))
    }

  def foldRight[A,B](as: List[A], z: B)(f: (A, B) => B): B = // Utility functions
    as match {
      case Nil => z
      case Cons(x, xs) => f(x, foldRight(xs, z)(f))
    }

  def sum2(ns: List[Int]) =
    foldRight(ns, 0)((x,y) => x + y)

  def product2(ns: List[Double]) =
    foldRight(ns, 1.0)(_ * _) // `_ * _` is more concise notation for `(x,y) => x * y`; see sidebar


  def tail[A](l: List[A]): List[A] = l match {
    case Nil => Nil
    case Cons(_, xs) => xs
  }

  def setHead[A](l: List[A], h: A): List[A] = l match {
    case Nil => Nil
    case Cons(_, xs) => Cons(h, xs)
  }

  def drop[A](l: List[A], n: Int): List[A] = {
    def helper(xs: List[A], n: Int): List[A] = xs match {
      case Nil => Nil
      case Cons(x, rs) => n match {
        case 0 => rs
        case _ => Cons(x, helper(rs, n-1))
      }
    }
    helper(l, n)
  }

  def dropWhile[A](l: List[A], f: A => Boolean): List[A] = {
    def helper(xs: List[A]): List[A] = xs match {
      case Nil => Nil
      case Cons(x, rs) => {
        if (f(x)) helper(rs)
        else Cons(x, helper(rs))
      }
    }
    helper(l)
  }


  def init[A](l: List[A]): List[A] = l match {
    case Nil => Nil
    case Cons(_, Nil) => Nil
    case Cons(x, xs) => Cons(x, init(xs))
  }

  def length[A](l: List[A]): Int =
    foldRight(l, 0)((x, y) => 1 + y)

  def foldLeft[A,B](l: List[A], z: B)(f: (B, A) => B): B = {
    @annotation.tailrec
    def helper(xs: List[A], res: B): B = xs match {
      case Nil => res
      case Cons(x, rs) => helper(rs, f(res, x))
    }
    helper(l, z)
  }

  def foldRight2[A,B](as: List[A], z: B)(f: (A, B) => B): B = {
    foldLeft(as, z)((x, y) => f(y, x))
  }

  def sum3(ns: List[Int]): Int = {
    foldLeft(ns, 0)(_ + _)
  }

  def product3(ns: List[Int]): Int = {
    foldLeft(ns, 1)(_ * _)
  }

  def length2[A](l: List[A]): Int =
    foldLeft(l, 0)((x, _) => x + 1)

  def sum4(ns: List[Int]): Int = {
    foldRight2(ns, 0)(_ + _)
  }

  def product4(ns: List[Int]): Int = {
    foldRight2(ns, 1)(_ * _)
  }

  def length3[A](l: List[A]): Int =
    foldRight2(l, 0)((_, y) => 1 + y)

  def append2[A](a1: List[A], a2: List[A]): List[A] = {
    foldLeft(reverse(a1), a2)((x, y) => Cons(y, x))
    }

  def append3[A](a1: List[A], a2: List[A]): List[A] = {
    foldRight2(reverse(a1), a2)((x, y) => Cons(x, y))
  }

  def reverse[A](l: List[A]): List[A] = {
    foldLeft(l, List[A]())((x, y) => Cons(y, x))
  }

  def concatLists[A](ls: List[List[A]]): List[A] = {
    foldLeft(ls, List[A]())(append2(_, _))
  }

  def map[A,B](l: List[A])(f: A => B): List[B] = {
    foldLeft(reverse(l), List[B]())((x, y) => Cons(f(y), x))
  }

  def addOne(l: List[Int]): List[Int] = {
    foldLeft(reverse(l), List[Int]())((x, y) => Cons(y+1, x))
  }

  def addOne1(l: List[Int]): List[Int] = {
    map(l)(x => x + 1)
  }

  def double2Str(l: List[Double]): List[String] = {
    map(l)(x => x.toString())
  }

  def filter[A](as: List[A])(f: A => Boolean): List[A] = {
    foldLeft(reverse(as), List[A]())((x, y) => {if (f(y)) x else Cons(y, x)})
  }

  def filter1[A](as: List[A])(f: A => Boolean): List[A] = {
    foldLeft(reverse(as), List[A]())((x, y) => f(y) match {
      case true => x
      case false => Cons(y, x)
    })
  }

  def filter2[A](as: List[A])(f: A => Boolean): List[A] = {
    flatMap(as)(x => {if (f(x)) List() else List(x)})
  }

  def flatMap[A,B](as: List[A])(f: A => List[B]): List[B] = {
    foldLeft(reverse(as), List[B]())((x, y) => append(f(y), x))
  }

  def addLists(l1: List[Int], l2: List[Int]): List[Int] = (l1, l2) match {
    case (Nil, Nil) => Nil
    case (Cons(x, xs), Cons(y, ys)) => Cons(x+y, addLists(xs, ys))
  }

  def zipWith[A](l1: List[A], l2: List[A])(f: (A, A) => A): List[A] = (l1, l2) match {
    case (Nil, Nil) => Nil
    case (Cons(x, xs), Cons(y, ys)) => Cons(f(x, y), zipWith(xs, ys)(f))
  }

  def addLists1(l1: List[Int], l2: List[Int]): List[Int] = {
    zipWith(l1, l2)(_ + _)
  }

  def hasSubsequence[A](sup: List[A], sub: List[A]): Boolean = {
    def helper(l1: List[A], l2: List[A], isSub: Boolean): Boolean = (l1, l2) match {
      case (_, Nil) => true
      case (Nil, _) => false
      case (Cons(x, xs), Cons(y, ys)) => {
        if (x == y) helper(xs, ys, true)
        else {
          if (isSub) false
          else helper(xs, sub, false)
        }
      }
    }
    helper(sup, sub, false)
  }

  def main(args: Array[String]): Unit = {
    //println(drop(List(1,2,3,4), 0))
    //println(dropWhile(List(1,2,3,4), (x: Int) => x % 2 == 1))
    //println(length3(List(1)))
    //println(length3(List(1,2,3,4)))
    //println(sum4(List(2,2)))
    //println(sum4(List(2,3,4)))
    //println(reverse(List(1,2)))
    //println(reverse(List(1,2,3,4)))
    //println(append2(List(1,2,3,4), List(5,6,7)))
    //println(append3(List(1,2,3,4), List(5,6,7)))
    //println(concatLists(List(List(1,2,3), List(4,5,6), List(7,8,9))))
    //val a1 = List(1,2,3)
    //println(addOne1(a1))
    //println(a1)
    //val a2 = List(1.0, 2.1, 3.2)
    //println(double2Str(a2))
    //println(filter(List(1,2,3,4))(x => x % 2 == 0))
    //println(filter1(List(1,2,3,4))(x => x % 2 == 1))
    //println(flatMap(List(1,2,3))(i => List(i,i)))
    //println(filter2(List(1,2,3,4))(x => x % 2 == 1))
    //println(filter2(List(1,2,3,4))(x => x % 2 == 0))
    //println(addLists1(List(1,2,3,4), List(2,3,4,5)))
    println(hasSubsequence(List(1,2,3), List(1)))
    println(hasSubsequence(List(1,2,3), List(2)))
    println(hasSubsequence(List(1,2,3), List(3)))
    println(hasSubsequence(List(1,2,3), List(1,2,3)))
    println(hasSubsequence(List(1,2,3), List(1,3,2)))
    println(hasSubsequence(List(1,2,3), List(2,1,3)))
    println(hasSubsequence(List(1,2,3), List(2,3,1)))
    println(hasSubsequence(List(1,2,3), List(3,1,2)))
    println(hasSubsequence(List(1,2,3), List(3,2,1)))
    println(hasSubsequence(List(1,2,3), List(1,2)))
    println(hasSubsequence(List(1,2,3), List(1,3)))
    println(hasSubsequence(List(1,2,3), List(2,1)))
    println(hasSubsequence(List(1,2,3), List(2,3)))
    println(hasSubsequence(List(1,2,3), List(3,1)))
    println(hasSubsequence(List(1,2,3), List(3,2)))
    println(hasSubsequence(List(1,2,3), List(1,1)))
    println(hasSubsequence(List(1,2,3), List(2,2)))
    println(hasSubsequence(List(1,2,3), List(3,3)))
  }
}

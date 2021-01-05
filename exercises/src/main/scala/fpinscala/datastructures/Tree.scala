package fpinscala.datastructures

sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]


object Tree {

	def size[A](ts: Tree[A]): Int = ts match {
		case Leaf(_) => 1
		case Branch(l, r) => 1 + size(l) + size(r)
	}

	def maximum(ts: Tree[Int]): Int = ts match {
		case Leaf(x) => x
		case Branch(l, r) => maximum(l) max maximum(r)
	}

	def depth(ts: Tree[Int]): Int = ts match {
		case Leaf(_) => 0
		case Branch(l, r) => 1 + (depth(l) max depth(r))
	}

	def map[A,B](ts: Tree[A])(f: A => B): Tree[B] = ts match {
		case Leaf(x) => Leaf(f(x))
		case Branch(l, r) => Branch(map(l)(f), map(r)(f))
	}

	def fold[A,B](t: Tree[A])(f: A => B)(g: (B, B) => B): B = t match {
		case Leaf(x) => f(x)
		case Branch(l, r) => g(fold(l)(f)(g), fold(r)(f)(g))
	}

	def size1[A](ts: Tree[A]): Int = {
		fold(ts)(_ => 1)(_ + _ + 1)
	}

	def maximum1(ts: Tree[Int]): Int = {
		fold(ts)(x => x)(_ max _)
	}

	def depth1(ts: Tree[Int]): Int = {
		fold(ts)(_ => 0)((x, y) => 1 + (x max y))
	}

	def map1[A,B](ts: Tree[A])(h: A => B): Tree[B] = {
		fold(ts)(x => Leaf(h(x)): Tree[B])(Branch(_, _))
	}

	def main(args: Array[String]): Unit = {
		println(size(Branch(Branch(Leaf(1), Leaf(2)), Branch(Leaf(3), Leaf(4)))))
		println(size1(Branch(Branch(Leaf(1), Leaf(2)), Branch(Leaf(3), Leaf(4)))))
		println(maximum(Branch(Branch(Leaf(1), Leaf(2)), Branch(Leaf(3), Leaf(4)))))
		println(maximum1(Branch(Branch(Leaf(1), Leaf(2)), Branch(Leaf(3), Leaf(4)))))
		println(depth(Branch(Branch(Leaf(1), Branch(Leaf(2), Leaf(4))), Leaf(3))))
		println(depth1(Branch(Branch(Leaf(1), Branch(Leaf(2), Leaf(4))), Leaf(3))))
		println(map(Branch(Branch(Leaf(1), Branch(Leaf(2), Leaf(4))), Leaf(3)))(_ + 1))
		println(map1(Branch(Branch(Leaf(1), Branch(Leaf(2), Leaf(4))), Leaf(3)))(_ + 1))
	}
}
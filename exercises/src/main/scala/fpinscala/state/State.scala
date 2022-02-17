package fpinscala.state

import fpinscala.state.RNG.Rand


trait RNG {
	def nextInt: (Int, RNG) // Should generate a random `Int`. We'll later define other functions in terms of `nextInt`.
}

object RNG {
	// NB - this was called SimpleRNG in the book text

	case class Simple(seed: Long) extends RNG {
		def nextInt: (Int, RNG) = {
			val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL // `&` is bitwise AND. We use the current seed to generate a new seed.
			val nextRNG = Simple(newSeed) // The next state, which is an `RNG` instance created from the new seed.
			val n = (newSeed >>> 16).toInt // `>>>` is right binary shift with zero fill. The value `n` is our new pseudo-random integer.
			(n, nextRNG) // The return value is a tuple containing both a pseudo-random integer and the next `RNG` state.
		}
	}

	type Rand[+A] = RNG => (A, RNG)

	val int: Rand[Int] = _.nextInt

	def unit[A](a: A): Rand[A] =
		rng => (a, rng)

	def map[A,B](s: Rand[A])(f: A => B): Rand[B] =
		rng => {
			val (a, rng2) = s(rng)
			(f(a), rng2)
		}

	def nonNegativeInt(rng: RNG): (Int, RNG) = {
		val (i, rng2) = rng.nextInt
		(i ^ 0x80000000, rng2)
	}

	def double(rng: RNG): (Double, RNG) = {
		val (i, rng2) = nonNegativeInt(rng)
		(i.toDouble / Int.MaxValue, rng2)
	}

	def doubleViaMap: Rand[Double] = {
		map(nonNegativeInt)(i => i.toDouble / Int.MaxValue)
	}

	def intDouble(rng: RNG): ((Int,Double), RNG) = {
		val (i, rng2) = nonNegativeInt(rng)
		val (d, rng3) = double(rng2)
		((i, d), rng3)
	}

	def doubleInt(rng: RNG): ((Double,Int), RNG) = {
		val (d, rng2) = double(rng)
		val (i, rng3) = nonNegativeInt(rng2)
		((d, i), rng3)
	}

	def double3(rng: RNG): ((Double,Double,Double), RNG) = {
		val (d1, rng2) = double(rng)
		val (d2, rng3) = double(rng2)
		val (d3, rng4) = double(rng3)
		((d1, d2, d3), rng4)
	}

	def ints(count: Int)(rng: RNG): (List[Int], RNG) = {
		@annotation.tailrec
		def helper(n: Int, l: (List[Int], RNG)): (List[Int], RNG) = n match {
			case 0 => l
			case _ =>
				val (i, rng2) = l._2.nextInt
				helper(n-1, (i::l._1, rng2))
		}
		helper(count, (List[Int](), rng))
	}

	def ints1(count: Int)(rng: RNG): (List[Int], RNG) =
		if (count <= 0)
			(List(), rng)
		else {
			val (x, r1)  = rng.nextInt
			val (xs, r2) = ints(count - 1)(r1)
			(x :: xs, r2)
		}

	// A tail-recursive solution
	def ints2(count: Int)(rng: RNG): (List[Int], RNG) = {
		@annotation.tailrec
		def go(count: Int, r: RNG, xs: List[Int]): (List[Int], RNG) =
			if (count <= 0)
				(xs, r)
			else {
				val (x, r2) = r.nextInt
				go(count - 1, r2, x :: xs)
			}
		go(count, rng, List())
	}

	def map2[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = {
		rng => {
			val (a, rng1) = ra(rng)
			val (b, rng2) = rb(rng1)
			(f(a, b), rng2)
		}
	}

	def sequence[A](fs: List[Rand[A]]): Rand[List[A]] = {
		fs.foldRight(unit(List[A]()))((x, y) => map2(x, y)(_ :: _))
	}

	def intsViaSequence(count: Int): Rand[List[Int]] = {
		sequence(List.fill(count)(int))
	}

	def flatMap[A,B](f: Rand[A])(g: A => Rand[B]): Rand[B] = {
		rng => {
			val (a, rng1) = f(rng)
			g(a)(rng1)
		}
	}

	def nonNegativeLessThan(n: Int): Rand[Int] =
		flatMap(nonNegativeInt)(i => {
			val mod = i % n
			if (i + (n-1) - mod>= 0) unit(mod) else nonNegativeLessThan(n)
		})

	def map[A,B](s: Rand[A])(f: A => B): Rand[B] = flatMap(s)(x => unit(f(x)))


	def map2[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = {
		flatMap(ra)(x => flatMap(rb)(y => unit(f(x, y))))
	}

	def main(args: Array[String]): Unit = {
		println(ints(4)(Simple(1L)))
		println(ints(5)(Simple(1L)))
		println(ints(6)(Simple(1L)))
		println(ints(7)(Simple(1L)))
		println(ints1(4)(Simple(1L)))
		println(ints1(5)(Simple(1L)))
		println(ints1(6)(Simple(1L)))
		println(ints1(7)(Simple(1L)))
		println(ints2(4)(Simple(1L)))
		println(ints2(5)(Simple(1L)))
		println(ints2(6)(Simple(1L)))
		println(ints2(7)(Simple(1L)))
		println(intsViaSequence(4)(Simple(1L)))
		println(intsViaSequence(5)(Simple(1L)))
		println(intsViaSequence(6)(Simple(1L)))
		println(intsViaSequence(7)(Simple(1L)))
	}
}

import State._

case class State[S,+A](run: S => (A, S)) {

	def map[B](f: A => B): State[S, B] =
		flatMap(a => unit(f(a)))

	def map2[B,C](sb: State[S, B])(f: (A, B) => C): State[S, C] = {
		flatMap(x => sb map(y => f(x, y)))
	}

	def flatMap[B](f: A => State[S, B]): State[S, B] = State(
		(s: S) => {
			val (a, s1) = run(s)
			f(a).run(s1)
		})

	def main(args: Array[String]): Unit = {
		println()
	}
}


sealed trait Input
case object Coin extends Input
case object Turn extends Input

case class Machine(locked: Boolean, candies: Int, coins: Int)

object State {

	type Rand[A] = State[RNG, A]

	def unit[S, A](a: A) : State[S, A] =
		State(s => (a, s))

	def sequence[S,A](fs: List[State[S,A]]): State[S,List[A]] =
		fs.foldRight(unit[S, List[A]](List[A]()))((x, y) => x.map2(y)(_ :: _))

	def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] = ???
}

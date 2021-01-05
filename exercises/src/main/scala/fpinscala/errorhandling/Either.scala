package fpinscala.errorhandling


import scala.{Option => _, Either => _, Left => _, Right => _, _} // hide std library `Option` and `Either`, since we are writing our own in this chapter

sealed trait Either[+E,+A] {
	def map[B](f: A => B): Either[E, B] = this match {
		case Right(a) => Right(f(a))
		case Left(a) => Left(a)
	}

	def flatMap[EE >: E, B](f: A => Either[EE, B]): Either[EE, B] = this match {
		case Right(a) => f(a)
		case Left(a) => Left(a)
	}

	def orElse[EE >: E, B >: A](b: => Either[EE, B]): Either[EE, B] = this match {
		case Left(_) => b
		case Right(a) => Right(a)
	}

	def map2[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C): Either[EE, C] = {
		flatMap(aa => b map (bb => f(aa, bb)))
	}
	def map2_[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C): Either[EE, C] = {
		for {
			aa <- this
			bb <- b
		} yield f(aa, bb)
	}

}
case class Left[+E](get: E) extends Either[E,Nothing]
case class Right[+A](get: A) extends Either[Nothing,A]

object Either {
	def traverse[E,A,B](es: List[A])(f: A => Either[E, B]): Either[E, List[B]] = {
		es.foldRight[Either[E, List[B]]](Right(Nil))((a, b) => f(a).map2(b)(_ :: _))
	}

	def sequence[E,A](es: List[Either[E,A]]): Either[E,List[A]] = {
		es.foldRight[Either[E,List[A]]](Right(Nil))((a, b) => a.map2(b)(_ :: _))
	}

	def traverse1[E,A,B](es: List[A])(f: A => Either[E, B]): Either[E, List[B]] = {
		sequence(es.map(f))
	}

	def sequence1[E,A](es: List[Either[E,A]]): Either[E,List[A]] = {
		traverse(es)(x => x)
	}

	def mean(xs: IndexedSeq[Double]): Either[String, Double] =
		if (xs.isEmpty)
			Left("mean of empty list!")
		else
			Right(xs.sum / xs.length)

	def safeDiv(x: Int, y: Int): Either[Exception, Int] =
		try Right(x / y)
		catch { case e: Exception => Left(e) }

	def Try[A](a: => A): Either[Exception, A] =
		try Right(a)
		catch { case e: Exception => Left(e) }

	def main(args: Array[String]): Unit = {
		println(traverse(List(1,2,3,4))((x: Int) => if (x % 2 ==0) Right(x) else Left("should not be odd")))
		println(traverse1(List(1,2,3,4))((x: Int) => if (x % 2 ==0) Right(x) else Left("should not be odd")))
		println(traverse(List(1,1,1,1))((x: Int) => if (x % 2 ==0) Right(x) else Left("should not be odd")))
		println(traverse(List(2,2,2,2))((x: Int) => if (x % 2 ==0) Right(x) else Left("should not be odd")))
		println(traverse1(List(1,1,1,1))((x: Int) => if (x % 2 ==0) Right(x) else Left("should not be odd")))
		println(traverse1(List(2,2,2,2))((x: Int) => if (x % 2 ==0) Right(x) else Left("should not be odd")))
		println(sequence(List(Right(1),Right(2),Right(3),Right(4))))
		println(sequence1(List(Right(1),Right(2),Right(3),Right(4))))
		println(sequence(List(Right(1),Right(2),Right(3),Left("should not be odd"))))
		println(sequence1(List(Left("should not be odd"),Right(1),Right(2),Right(3),Right(4))))
	}

}
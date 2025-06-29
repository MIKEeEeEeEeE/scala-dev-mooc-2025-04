package ru.otus.module1

import ru.otus.module1.opt.{Animal, Cat, Dog}

import scala.annotation.tailrec
import scala.language.postfixOps
import org.scalatest.Assertions._


/**
 * referential transparency
 */


// recursion

object recursion {

	/**
	 * Реализовать метод вычисления n!
	 * n! = 1 * 2 * ... n
	 */

	def fact(n: Int): Int = {
		var _n = 1
		var i = 2
		while (i <= n) {
			_n *= i
			i += 1
		}
		_n
	}


	def factRec(n: Int): Int = if (n <= 0) 1 else n * factRec(n - 1)


	def factTailRec(n: Int, accum: Int): Int = {
		@tailrec
		def loop(n: Int, accum: Int): Int =
			if (n <= 0) accum
			else loop(n - 1, n * accum)

		loop(n, 1)
	}


	/**
	 * реализовать вычисление N числа Фибоначчи
	 * F0 = 0, F1 = 1, Fn = Fn-1 + Fn - 2
	 */


}


object hof {

	def dumb(string: String): Unit = {
		Thread.sleep(1000)
		println(string)
	}

	// обертки

	def logRunningTime[A, B](f: A => B): A => B = a => {
		val start = System.currentTimeMillis()
		val result = f(a)
		val end = System.currentTimeMillis()
		println(s"Running time: ${end - start}")
		result
	}



	// изменение поведения ф-ции

	def isOdd(i: Int): Boolean = i % 2 > 0

	def not[A](f: A => Boolean): A => Boolean = a => !f(a)

	val isEven: Int => Boolean = not(isOdd)

	isOdd(5) // true
	isEven(5) // false


	// изменение самой функции

	def sum(x: Int, y: Int): Int = x + y

	def partial[A, B, C](a: A)(f: (A, B) => C): B => C =
		f.curried(a)


}


/**
 * Реализуем тип Option
 */


object opt {


	trait Animal

	case class Cat() extends Animal

	case class Dog() extends Animal

	/**
	 *
	 * Реализовать структуру данных Option, который будет указывать на присутствие либо отсутствие результата
	 */

	// Invariance
	// + Covariance Если А является подтипом В, то Option[A] является подтипом Option[B]
	// - Contravariance Если А является подтипом В, то Option[A] является супер типом Option[B]

	// Function1[-R, +T]
	//	val f1: String => Unit = ???
	//	val f2: Any => Unit = ???

	def foo(f: String => Unit) = f("Hello")

	//	foo(f2)
	//	foo(f1)


	sealed trait Option[+T] {
		def isEmpty: Boolean = if (this.isInstanceOf[None.type]) true else false

		//    def get: T =  if(this.isInstanceOf[None.type]) throw new Exception("None get")
		//      else{
		//        val r = this.asInstanceOf[Some[T]]
		//        r.v
		//      }

		def map[B](f: T => B): Option[B] = flatMap(v => Option(f(v)))

		def flatMap[B](f: T => Option[B]): Option[B] = this match {
			case Some(v) => Option(f(v))
			case None => None
		}
	}

	case class Some[T](v: T) extends Option[T]

	case object None extends Option[Nothing]

	object Option {
		def apply[T](v: T): Option[T] =
			if (v == null) None else Some(v)

		def apply[T](v: Option[T]): Option[T] = v match {
			case t@Some(_) => t
			case None => None
		}
	}

	val opt1: Option[Int] = Some(1)

	val opt2: Option[Option[Int]] = opt1.map(i => Option(i + 1))
	val opt3: Option[Int] = opt1.flatMap(i => Option(i + 1))

	println(opt2)
	println(opt3)


	/**
	 *
	 * Реализовать метод printIfAny, который будет печатать значение, если оно есть
	 */

	def printIfAny[T](value: Option[T]): Unit = value match {
		case Some(v) => println(v)
		case None =>
	}

	printIfAny(Some(1))
	printIfAny(None)

	/**
	 *
	 * Реализовать метод zip, который будет создавать Option от пары значений из 2-х Option
	 */

	def zip[A, B](v1: Option[A], v2: Option[B]): Option[(A, B)] = for (
		// v1.flatMap(x => v2.flatMap(y => Some(x, y)))
		x <- v1;
		y <- v2
	) yield (x, y)

	assert(zip(Some(1), Some(2)) == Some(1, 2))
	assert(zip(Some(1), None) == None)

	/**
	 *
	 * Реализовать метод filter, который будет возвращать не пустой Option
	 * в случае если исходный не пуст и предикат от значения = true
	 */

	def filter[T](v: Option[T], pred: T => Boolean): Option[T] = v.map(pred) match {
		case Some(true) => v
		case _ => None
	}

	assert(filter(Some(1), (i: Int) => i > 0) == Some(1))
	assert(filter(Some(0), (i: Int) => i > 0) == None)
	assert(filter(None, (i: Int) => i > 0) == None)

	def main(args: Array[String]): Unit =
		println("Assertions passed!")

}

object list {
	/**
	 *
	 * Реализовать одно связанный иммутабельный список List
	 * Список имеет два случая:
	 * Nil - пустой список
	 * Cons - непустой, содержит первый элемент (голову) и хвост (оставшийся список)
	 */

	def treat(a: Option[Animal]) = ???

	sealed trait List[+T] {

		// prepend
		def ::[TT >: T](elem: TT): List[TT] = new::(elem, this)

		@tailrec
		private def _reverse[TT >: T](acc: List[TT] = Nil): List[TT] = this match {
			case head :: tail => tail._reverse(acc = head :: acc)
			case Nil => acc
		}

		def reverse[TT >: T]: List[TT] = this._reverse()

		def map[B](f: T => B): List[B] = this match {
			case head :: tail => f(head) :: tail.map(f)
			case Nil => Nil
		}

		def concat[TT >: T](l2: List[TT]): List[TT] = this match {
			case head :: tail => head :: tail.concat(l2)
			case Nil => l2
		}

		def mkString(prefix: String = "", postfix: String = ""): String = this match {
			case head :: tail => prefix ++ head.toString ++ postfix ++ tail.mkString(prefix, postfix)
			case Nil => ""
		}

		def flatMap[A](f: T => List[A]): List[A] = this match {
			case head :: tail => f(head).concat(tail.flatMap(f))
			case Nil => Nil
		}

		def filter(pred: T => Boolean): List[T] = this match {
			case head :: tail => if (pred(head)) head :: tail.filter(pred) else tail.filter(pred)
			case Nil => Nil
		}
	}

	case class ::[+T](elem: T, tail: List[T]) extends List[T]

	case object Nil extends List[Nothing]


// Advanced Programming, A. Wąsowski, IT University of Copenhagen
//
// Group number: _____
//
// AUTHOR1: __________
// TIME1: _____ <- how much time have you used on solving this exercise set
// (excluding reading the book, fetching pizza, and going out for a smoke)
//
// AUTHOR2: __________
// TIME2: _____ <- how much time have you used on solving this exercise set
// (excluding reading the book, fetching pizza, and going out for a smoke)
//
// You should work with the file by following the associated exercise sheet
// (available in PDF from the course website).
//
// This file is compiled with 'sbt compile' and tested with 'sbt test'.
//
// The file shall always compile and run after you are done with each exercise
// (if you do them in order).  Please compile and test frequently. Of course,
// some tests will be failing until you finish. Only hand in a solution that
// compiles and where tests pass for all parts that you finished.    The tests
// will fail for unfnished parts.  Comment such out.

package adpro

import adpro.Tree.ExercisesOption.mean
import adpro.Tree.{ExercisesOption, Some}


// Exercise  1

/* We create OrderedPoint as a trait instead of a class, so we can mix it into
 * Points (this allows to use java.awt.Point constructors without
 * reimplementing them). As constructors are not inherited, We would have to
 * reimplement them in the subclass, if classes not traits are used.  This is
 * not a problem if I mix in a trait construction time. */

trait OrderedPoint extends scala.math.Ordered[java.awt.Point] {

  this: java.awt.Point =>

  override def compare (that: java.awt.Point): Int =  {
    if (this.x < that.x && this.y < that.y) -1
    else if(this.x == that.x && this.y == that.y) 0
    else 1
  }

}

// Try the following (and similar) tests in the repl (sbt console):
// val p = new java.awt.Point(0,1) with OrderedPoint
// val q = new java.awt.Point(0,2) with OrderedPoint
// assert(p < q)

// Chapter 3


sealed trait Tree[+A]
case class Leaf[A] (value: A) extends Tree[A]
case class Branch[A] (left: Tree[A], right: Tree[A]) extends Tree[A]

object Tree {

  def size[A](t: Tree[A]): Int = t match {
    case Leaf(a) => 1
    case Branch(l, r) => 1 + size(l) + size(r)
  }


  // Exercise 3 (3.26)

  def maximum(t: Tree[Int]): Int = t match {
    case Leaf(a) => a
    case Branch(l, r) => maximum(l) max maximum(r)
  }

  // Exercise 4 (3.28)

  def map[A, B](t: Tree[A])(f: A => B): Tree[B] = t match {
    case Leaf(a) => Leaf(f(a))
    case Branch(left, right) => Branch(map(left)(f), map(right)(f))
  }

  // Exercise 5 (3.29)
  //  Generalize size, maximum, and map, writing a new function fold that abstracts over
  //  their similarities. Reimplement them in terms of this more general function.5

  def fold[A, B](t: Tree[A])(f: (B, B) => B)(g: A => B): B = t match {
    case Leaf(a) => g(a)
    case Branch(l, r) => f(fold(l)(f)(g), fold(r)(f)(g))
  }

  //see the aruguments and what they take and return


  def size1[A](t: Tree[A]): Int = fold(t)((a: Int, b: Int) => 1 + a + b)(_ => 1)



  def maximum1[A](t: Tree[Int]): Int = fold(t)((a: Int, b: Int) => a max b)(a => a)


  def map1[A,B] (t: Tree[A]) (f: A=>B): Tree[B] = fold[A,Tree[B]](t)((l,r)=>Branch(l,r))(a=>Leaf(f(a)))
  //apply funtion to left and also to right with a funtion


  sealed trait Option[+A] {

    // Exercise 6 (4.1)

    //map and getoRElse are

    def map[B](f: A => B): Option[B] = this match {
      case Some(a) => Some(f(a))
      case None => None

    }




    // You may Ignore the arrow in default's type below for the time being.
    // (it should work (almost) as if it was not there)
    // It prevents the argument "default" from being evaluated until it is needed.
    // So it is not evaluated in case of Some (the term is 'call-by-name' and we
    // should talk about this soon).

    def getOrElse[B >: A](default: => B): B = this match{
      case Some(a) => a
      case None => default
    }   ////the default value  is written when nothing match or if something match just return the value



    def flatMap[B](f: A => Option[B]): Option[B] = this match{
      case None => None
      case Some(a) => f(a)

    }

    def filter(p: A => Boolean): Option[A] = this match{
      case Some(a) => if(p(a))  Some(a) else  None
      case None => None
    }

  }

  case class Some[+A](get: A) extends Option[A]

  case object None extends Option[Nothing]

  object ExercisesOption {

    // Remember that mean is implemented in Chapter 4 of the text book

    def mean(xs: Seq[Double]): Option[Double] =
      if (xs.isEmpty) None
      else Some(xs.sum / xs.length)

    // Exercise 7 (4.2)

//    Implement the variance function in terms of flatMap. If the mean of a sequence is m,
//    the variance is the mean of math.pow(x - m, 2) for each element x in the sequence.7

    def variance(xs: Seq[Double]): Option[Double] =
    mean(xs).flatMap(m => mean(xs.map(x => math.pow(x-m,2))))

     // mean(xs.map((x => math.pow(x-m,2)))) this is the mean of all the elements of the sequence after applying the funtion






    // Exercise 8 (4.3)
//    Write a generic function map2 that combines two Option values using a binary
//    function.

    def map2[A, B, C](ao: Option[A], bo: Option[B])(f: (A, B) => C): Option[C] =
      ao.flatMap(a => bo.map(b => f(a,b)))

    // Exercise 9 (4.4)
//    Write a function sequence that combines a list of Options into one Option containing
//      a list of all the Some values in the original list:

    //from book
    //  def foldRight[A,B](as: List[A], z: B)(f: (A, B) => B): B =
    //    as match {
    //      case Nil => z
    //      case Cons(x, xs) => f(x, foldRight(xs, z)(f))
    //    }\

    //List(Some(1), SOme(2), Nil)  will yield Some(List(1, 2))


    def sequence[A](aos: List[Option[A]]): Option[List[A]] =
      aos.foldRight[Option[List[A]]](Some(Nil))((oa,ob)=> map2(oa,ob)(_::_))



    // Exercise 10 (4.5)
//    The function behaves like map executed sequentially on the list a, where the mapped function f can
//      fail. If at least one application fails, then the entire computation of the mapping (traversal) fails.

    def traverse[A, B](as: List[A])(f: A => Option[B]): Option[List[B]] =
      as.foldRight[Option[List[B]]](Some(Nil)) ((a,b) => map2(f(a),b)(_::_))

  }

}

object Exercises extends App {
  //5
  val tre = Branch
  (
    Branch(
      Leaf(5),
      Branch(
        Leaf(10),
        Leaf(15))),
    Leaf(20))

  //  val sizeeee = Tree.size1(tre)
  //  println("tree size from fold is " + sizeeee)
  //
  //  val maxi= Tree.maximum1(tre)
  //  println("tree maximum from fold is " + maxi)
  //
  //  val mapping = Tree.map1(tre)(_+5)
  //  println("tree after mapping from fold is " + mapping)

  val x = Some(1)
  println("Option map " + x.map(_ + 5))
  println("Option filter " + x.filter(_ != 1))

  val s = Seq(1.0, 2.0, 3.0)
  println("mean of the sequence" + mean(s))
  println("Variance  of the sequence" + ExercisesOption.variance(s))

  val seqquence = List(Some(1), Some(2))
  println("Sequence   of the sequence" + ExercisesOption.sequence(seqquence))

  val trav = List(1,2)
    println("Traverse   of the sequence" + ExercisesOption.traverse(trav)(Some(_)))


}












// Advanced Programming, Exercises by A. Wąsowski, IT University of Copenhagen
//
// AUTHOR1:
// AUTHOR2:
//
// Write ITU email addresses of both group members that contributed to
// the solution of the exercise (in lexicographic order).
//
// You should work with the file by following the associated exercise sheet
// (available in PDF from the course website).
//
// The file is meant to be compiled inside sbt, using the 'compile' command.
// To run the compiled file use the 'run' or 'runMain' command.
// To load the file int the REPL use the 'console' command.
// Now you can interactively experiment with your code.
//
// Continue solving exercises in the order presented in the PDF file. The file
// shall always compile, run, and pass tests, after you are done with each
// exercise (if you do them in order).  Please compile and test frequently.

// The extension of App allows writing the main method statements at the top
// level (the so called default constructor). For App objects they will be
// executed as if they were placed in the main method in Java.
package fpinscala

object Exercises extends App with ExercisesInterface {

  import fpinscala.List._

  // Exercise 3

  // add @annotation.tailrec to make the compiler check that your solution is
  // tail recursive

  def fib(n: Int): Int = {
  @annotation.tailrec
  def go(n: Int, curr: Int, prev: Int): Int = {
    if (n <= 1) {
      curr
    } else {
      go(n - 1, curr = prev, prev = prev + curr)
    }
  }
  go(n, curr = 0, prev = 1)
}


  // Exercise 4

  // add @annotation.tailrec to make the compiler check that your solution is
  // tail recursive
  def isSorted[A] (as: Array[A], ordered: (A,A) =>  Boolean) :Boolean = {
    @annotation.tailrec
    def loop(n:Int):Boolean= {
      if(n == as.length-1) true   //checks here
      else if (ordered(as(n),as(n+1))) loop(n+1)
      else false
    }
    loop(0)
  }


 

  // Exercise 5

  def curry[A,B,C] (f: (A,B)=>C): A => (B => C) = 
   // a:A => (b:B =>f(a,b))
    (a:A) => (b:B) => f(a,b)

  // Exercise 6

  def uncurry[A,B,C] (f: A => B => C): (A,B) => C =
    (a:A,b:B) => f(a)(b)


  // Exercise 7

  def compose[A,B,C] (f: B => C, g: A => B) : A => C =
    (a:A) => f(g(a))

  def add2(x:Int) = x + 2
  def add3(x:Int) = x + 3
  val result = compose(add2, add3)(2)
  println(" result of Compose" + result)

  // Exercise 8 requires no programming


//  List(1,2,3,4,5) match {
//  2 case Cons(x, Cons(2, Cons(4, _))) => x
//    3 case Nil => 42
//    4 case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y  returns 3
//    5 case Cons(h, t) => h + sum(t)  // returns 15
//    6 case _ => 101   //101
//    7 }



  // Exercise 9

  def tail[A] (as: List[A]) :List[A] = as match {
    case Nil => sys.error("List empty")
    case Cons(_,t) => t
  }

//  val list = List(1,2,3)
//  val newList = tail(list)
//  println(newList)


  // Exercise 10

  // @annotation.tailrec
  // Uncommment the annotation after solving to make the
  // compiler check whether you made the solution tail recursive

  //removes first n elements from the list
  def drop[A] (l: List[A], n: Int) : List[A] =
    if(n==0) l
   else
      l match{
      case Cons(h,t) => drop(t,n-1)

    }

//    val list = List(5,6,7,8)
//    val dropped = drop(list,3)
//    println("Dropped List is " + dropped)

  // Exercise 11

  // @annotation.tailrec
  // Uncommment the annotation after solving to make the
  // compiler check whether you made the solution tail recursive





  def dropWhile[A](l: List[A], f: A => Boolean): List[A] =
    l match {
      case Cons(h, t) if f(h) => dropWhile(t, f)
      case _ => l
    }

//  val check = (x: Int) => x < 6
//  val listed = List(1,2,3,7,8)
//  println("After dropWhile " + dropWhile(listed, check))

  // Exercise 12

  def init[A](l: List[A]): List[A] = l match {
      case Nil => sys.error("init of empty list")
      case Cons(_, Nil) => Nil
    case Cons(h,t) => Cons(h, init(t))
  }

//  val list = List(3,4,5,6)
//  println("After init " + init(list))



  // Exercise 13


  def length[A] (as: List[A]): Int =
    foldRight(as,0)((_,acc) => acc + 1)



//  val list = List(1,2,3,4)
//  println("length using foldRight " + length(list))



  //from book
//  def foldRight[A,B](as: List[A], z: B)(f: (A, B) => B): B =
//    as match {
//      case Nil => z
//      case Cons(x, xs) => f(x, foldRight(xs, z)(f))
//    }



//  For comparison consider that:
//    foldLeft (List(1,2,3,4),0) (_ + _) computes (((0 + 1) + 2) + 3) + 4 while
//  foldRight (List(1,2,3,4),0) (_ + _) computes 1 + (2 + (3 + (4 + 0))).
  // Exercise 14

  // @annotation.tailrec
  // Uncommment the annotation after solving to make the
  // compiler check whether you made the solution tail recursive
  def foldLeft[A,B] (as: List[A], z: B) (f: (B, A) => B): B = as match {
    case Nil => z
    case Cons(x, xs) => foldLeft(xs,f(z,x))(f)
  }

//  def reverse[A] (as: List[A]): List[A] =
//    foldLeft(as,List[A]())((acc,h)=> Cons(h,acc))

  // Exercise 15 using Fold Left

  def product (as: List[Int]): Int =
    foldLeft(as, 1)(_*_)

  def length1 (as: List[Int]): Int =
    foldLeft(as,0)((acc,h)=> acc + 1)

  // Exercise 16

  //ask

  def reverse[A] (as: List[A]): List[A] =

    foldRight(as,List[A]())((acc,h) => Cons(acc,h))

    //foldLeft(as,List[A]())((acc,h)=> Cons(h,acc))

  val l0 = List(1,2,3)
  println("reverse using foldLeft  " + reverse(l0))



  // Exercise 17

  def foldRight1[A,B] (as: List[A], z: B) (f: (A, B) => B): B =
    foldLeft(reverse(as),z)((b,a)=> b)


  // Exercise 18

  def foldLeft1[A,B] (as: List[A], z: B) (f: (B,A) => B): B = ???

  // Exercise 19

  def append[A](a1: List[A], a2: List[A]): List[A] = a1 match {
    case Nil => a2
    case Cons(h,t) => Cons(h, append(t, a2))
  }
  val l1 = List(1,2)
  val l2 = List(3,4)
  println("Append  " + append(l1,l2))


  def concat[A] (as: List[List[A]]): List[A] =
    foldRight(as,List[A]())(append)


//  val l = List(List(1,2),List(3,4),List(5,6,7,8))
//  println("Concat using foldRight" + concat(l))

  // Exercise 20

//  def map[B](f: A => B): List[B] =
//    foldRight(Nil: List[B])((a, acc) => Cons(f(a), acc))


//  Exercise 20. Write a function filter that removes elements from a list unless they satisfy a given
//  predicate f.18

  def filter[A] (as: List[A]) (f: A => Boolean): List[A] = as match {
    case Nil => Nil
    case Cons(h,t) => if(f(h)) Cons(h,filter(t)(f)) else filter(t)(f)
  }

  val l = List(1,2,3)
  println("Result after Filter " + Exercises.filter(l)(_ >=3) )
  // Exercise 21

  def flatMap[A,B](as: List[A])(f: A => List[B]): List[B] = as match{
    case Nil => List[B]()
    case Cons(h,t) => append(f(h),flatMap(t)(f))
  }

  val listi = List(5,10,15,20)
  println("Result after flatMAp using filter " + Exercises.flatMap(listi)(x=>List(x,x)) )



  // Exercise 22

// def filter1[A] (l: List[A]) (p: A => Boolean) :List[A] = l match {
//   case Cons(h,t) =>
//     //h and t if f(h) filter1 tail and  Cons the tail
//     flatMap(t)()
// }

  // Exercise 23

//  def add (l: List[Int]) (r: List[Int]): List[Int] = (l,r) match {
//    case (Cons(h1,t1),Cons(h2,t2)) => Cons(foldLeft((Cons(h1,h2),0)(_+_), add(t1)(t2))
//  }

  def add (l: List[Int]) (r: List[Int]): List[Int] = (l,r) match {
    case (Cons(h1,t1),Cons(h2,t2)) => Cons(h1 + h2,add(t1)(t2))
  }
  val list1 = List(1,2)
  val list2 = List(2,3)
  println("Result of corresponding add " + add(list1)(list2))
  // Exercise 24

  def zipWith[A,B,C] (f: (A,B)=>C) (l: List[A], r: List[B]): List[C] = ???

  // Exercise 25

  def hasSubsequence[A] (sup: List[A], sub: List[A]): Boolean = ???

  // Exercise 26

  def pascal (n: Int): List[Int] = ???

}

object Main extends App{
  println("fib 6 is "+ Exercises.fib(6))

  //Exercise 4
  val myList = Array(1,2,4,5,2)
  def f(a:Int,b:Int) = if(b > a) true else false

//println("result1" + Exercises.isSorted(myList,(a:Int,b:Int) => a<b?1:0)
//or
println("result2" + Exercises.isSorted(myList,f))

  //println(s"the result return $result")

  //Exercise 5 curry

  def add(x:Int, y:Int) = x + y
  val curriedAdd = Exercises.curry(add)
  println("Result is " + curriedAdd)


  // def compose[A,B,C] (f: B => C, g: A => B) : A => C =
  //    (a:A) => f(g(a))
  //def sub(x:Int, y:Int):Int = x - y

  //val result = compose(add,sub)


}

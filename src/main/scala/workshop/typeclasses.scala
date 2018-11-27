package workshop

import workshop.adts._
import workshop.model.rawText
import simulacrum.typeclass
import scala.concurrent.Future
import workshop.typeclasses.Show.ops._
import workshop.typeclasses.Eq.ops._
import workshop.typeclasses.Monoid.ops._
import workshop.typeclasses.Functor.ops._
import scala.concurrent.ExecutionContext.Implicits.global

object typeclasses {

  //Show
  @typeclass trait Show[A] {
    def show(a: A): String
  }

  implicit def showInt: Show[Int] = new Show[Int] {
    def show(a: Int): String = a.toString
  }

  implicit def showString: Show[String] = new Show[String] {
    def show(a: String): String = a
  }

  implicit def showChessPiece: Show[ChessPiece] = new Show[ChessPiece] {
    def show(a: ChessPiece): String = s"${a.chessPieceType.getClass.getName} with color ${a.color} at position ${a.currentPos}"
  }

  implicit def showOption[A: Show]: Show[Option[A]] = new Show[Option[A]]  {
    def show(a: Option[A]): String = a match {
      case Some(value) => Show[A].show(value)
      case None => "[No Value]"
    }
  }




  //Eq

  @typeclass trait Eq[A] {
    def eqv(x: A, y: A): Boolean
    def ===(x: A)(y: A): Boolean = eqv(x, y)
  }

  implicit def eqInt: Eq[Int] = new Eq[Int] {
    def eqv(x: Int, y: Int): Boolean = x == y
  }

  implicit def eqString: Eq[String] = new Eq[String] {
    def eqv(x: String, y: String): Boolean = x.equals(y)
  }

  implicit def eqOption[A: Eq]: Eq[Option[A]] = (x, y) => (x, y) match {
    case (Some(x), Some(y)) => x === y
    case (None, None) => true
    case _ => false
  }

  implicit def eqEither[A: Eq, B: Eq]: Eq[Either[A, B]] = (x, y) => (x, y) match {
    case (Left(x), Left(y)) => x === y
    case (Right(x), Right(y)) => x === y
    case _ => false
  }





  //Monoid

  @typeclass trait Monoid[A] {
    def empty: A
    def combine(x: A, y: A): A

    def |+|(x: A)(y: A): A = combine(x, y)
  }

  implicit def intMonoid: Monoid[Int] = new Monoid[Int] {
    def empty: Int = 0

    def combine(x: Int, y: Int): Int = x + y
  }

  implicit def stringMonoid: Monoid[String] = new Monoid[String] {
    def empty: String = ""

    def combine(x: String, y: String): String = x + y
  }

  implicit def timespanMonoid: Monoid[TimeSpan] = new Monoid[TimeSpan] {
    def empty: TimeSpan = TimeSpan(0, Seconds())

    def combine(x: TimeSpan, y: TimeSpan): TimeSpan = x.add(y)
  }

  implicit def listMonoid[A]: Monoid[List[A]] = new Monoid[List[A]] {
    def empty: List[A] = List.empty[A]

    def combine(x: List[A], y: List[A]) = x ++ y
  }

  // The intMonoid further up uses `addition` as its method of combination, but that is not the only monoid for `Int`!
  // We can also define one for multiplication, though if we just define it for `Int` again the compiler won't know which to pick
  // What we can do instead is define a small wrapper that allows us to multiply
  case class Mult(value: Int)

  implicit def multMonoid: Monoid[Mult] = new Monoid[Mult] {
    def empty = Mult(0)

    def combine(x: Mult, y: Mult): Mult = Mult(x.value * y.value)
  }

  def combineAll[A: Monoid](list: List[A]): A = list.foldLeft(Monoid[A].empty)(Monoid[A].combine)

  def foldMap[A, B: Monoid](list: List[A])(f: A => B): B = combineAll(list.map(f))

  implicit def tupleMonoid[A: Monoid, B: Monoid]: Monoid[(A, B)] = new Monoid[(A, B)] {
    def empty: (A, B) = (Monoid[A].empty, Monoid[B].empty)

    def combine(x: (A, B), y: (A, B)): (A, B) = (x._1 |+| y._1, x._2 |+| y._2)
  }

  implicit def tuple3Monoid[A: Monoid, B: Monoid, C: Monoid]: Monoid[(A, B, C)] = new Monoid[(A, B, C)] {
    def empty: (A, B, C) = (Monoid[A].empty, Monoid[B].empty, Monoid[C].empty)

    def combine(x: (A, B, C), y: (A, B, C)): (A, B, C) = (x._1 |+| y._1, x._2 |+| y._2, x._3 |+| y._3)
  }

  implicit def tuple4Monoid[A: Monoid, B: Monoid, C: Monoid, D: Monoid]: Monoid[(A, B, C, D)] = new Monoid[(A, B, C, D)] {
    def empty: (A, B, C, D) = (Monoid[A].empty, Monoid[B].empty, Monoid[C].empty, Monoid[D].empty)

    def combine(x: (A, B, C, D), y: (A, B, C, D)): (A, B, C, D) = (x._1 |+| y._1, x._2 |+| y._2, x._3 |+| y._3, x._4 |+| y._4)
  }

  implicit def mapMonoid[A, B: Monoid]: Monoid[Map[A, B]] = new Monoid[Map[A, B]] {
    def empty: Map[A, B] = Map.empty

    def combine(x: Map[A, B], y: Map[A, B]): Map[A, B] = ???
  }

  implicit def futureMonoid[A: Monoid]: Monoid[Future[A]] = ???


  //Monoid word count
  //Use foldMap with a Monoid to count the number of words, the number of characters and the number of occurences of each word
  //Tip: the Map and Tuple3 Monoid can help

  val words: List[String] = rawText.split(" ").toList

  def wordCount(text: List[String]) = foldMap(text)(s => 1)
  def characterCount(text: List[String]) = foldMap(text)(word => word.length)
  def wordOccurences(text: List[String]) = foldMap(text)(word => Map( word -> 1))

  def result = (wordCount(words), characterCount(words), wordOccurences(words))


  //Now that you have the word count let's extend it with the ability to get the longest word of the text.
  //Tip: Define a Maximum Monoid to do so

  case class Max(value: String)
  implicit def maxMonoid = new Monoid[Max] {
    override def empty: Max = Max("")

    override def combine(x: Max, y: Max): Max = if (x.value.length > y.value.length) x else y
  }


  //Functor

  @typeclass trait Functor[F[_]] {
    def map[A, B](fa: F[A])(f: A => B): F[B]
  }

  implicit def optionFunctor: Functor[Option] = new Functor[Option] {
    override def map[A, B](fa: Option[A])(f: A => B): Option[B] = fa match {
      case Some(value) => Some(f(value))
      case _ => None
    }
  }

  implicit def listFunctor: Functor[List] = new Functor[List] {
    override def map[A, B](fa: List[A])(f: A => B): List[B] = fa.map(f)
  }



  sealed trait Tree[+A]
  case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]
  case class Leaf[A](value: A) extends Tree[A]

  implicit def treeFunctor: Functor[Tree] = new Functor[Tree] {
    override def map[A, B](fa: Tree[A])(f: A => B): Tree[B] = fa match {
      case Branch(left, right) => Branch(map(left)(f), map(right)(f))
      case Leaf(value) => Leaf(f(value))
    }
  }






  //Cardinality

  @typeclass trait Cardinality[A] {
    def cardinality: BigInt
  }

  implicit def cardinalityUnit: Cardinality[Unit] = new Cardinality[Unit] {
    override def cardinality: BigInt = 1
  }

  implicit def cardinalityBoolean: Cardinality[Boolean] = new Cardinality[Boolean] {
    override def cardinality: BigInt = 2
  }

  implicit def cardinalityByte: Cardinality[Byte] = new Cardinality[Byte] {
    override def cardinality: BigInt = Byte.MaxValue.toInt
  }

  implicit def cardinalityShort: Cardinality[Short] = new Cardinality[Short] {
    override def cardinality: BigInt = Short.MaxValue.toInt
  }

  implicit def cardinalityInt: Cardinality[Int] = new Cardinality[Int] {
    override def cardinality: BigInt = Int.MaxValue
  }

  implicit def cardinalityTuple[A: Cardinality, B: Cardinality]: Cardinality[(A, B)] = new Cardinality[(A, B)] {
    override def cardinality: BigInt = Cardinality[A].cardinality * Cardinality[B].cardinality
  }

  implicit def cardinalityEither[A: Cardinality, B: Cardinality]: Cardinality[Either[A, B]] = new Cardinality[Either[A, B]] {
    override def cardinality: BigInt = Cardinality[A].cardinality + Cardinality[B].cardinality
  }

  implicit def cardinalitySize: Cardinality[Size] = new Cardinality[Size] {
    override def cardinality: BigInt = 3
  }

  implicit def cardinalityNothing: Cardinality[Nothing]= new Cardinality[Nothing] {
    override def cardinality: BigInt = 0
  }

  implicit def cardinalityFunction[A: Cardinality, B: Cardinality]: Cardinality[A => B] = new Cardinality[A => B] {
    override def cardinality: BigInt = Cardinality[B].cardinality pow Cardinality[A].cardinality.toInt
  }



}

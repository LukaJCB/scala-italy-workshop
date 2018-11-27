package workshop

import workshop.typeclasses._
import workshop.model._
import simulacrum.typeclass

import scala.concurrent.Future
import abstractions.Monoidal.ops._
import abstractions.Traverse.ops._
import abstractions.Foldable.ops._
import typeclasses.Monoid.ops._

import scala.collection.immutable
import scala.concurrent.ExecutionContext.Implicits.global
import scala.util.Success

object abstractions {


  //Multiplicative Monoidal Functors

  @typeclass trait Monoidal[F[_]] extends Functor[F] {
    def product[A, B](fa: F[A], fb: F[B]): F[(A, B)]

    def unit: F[Unit]

    def pure[A](a: A): F[A] = map(unit)(_ => a)

    def map2[A, B, C](fa: F[A], fb: F[B])(f: (A, B) => C): F[C] =
      map(product(fa, fb)) { case (a, b) => f(a, b) }
  }

  implicit def optionMonoidal: Monoidal[Option] = new Monoidal[Option] {
    override def product[A, B](fa: Option[A], fb: Option[B]): Option[(A, B)] = ???

    override def unit: Option[Unit] = Some(())

    override def map[A, B](fa: Option[A])(f: A => B): Option[B] = fa.map(f)

  }

  implicit def futureMonoidal: Monoidal[Future] = new Monoidal[Future] {
    override def product[A, B](fa: Future[A], fb: Future[B]): Future[(A, B)] =
      for {
        x <- fa
        y <- fb
      } yield (x, y)

    override def unit: Future[Unit] = Future.successful(())

    override def map[A, B](fa: Future[A])(f: A => B): Future[B] = fa.map(f)
  }

  // There are two possible solutions here, can you figure out which?
  implicit def listMonoidal: Monoidal[List] = new Monoidal[List] {
    override def product[A, B](fa: List[A], fb: List[B]): List[(A, B)] = for {
      a <- fa
      b <- fb
    } yield (a, b)

    override def unit: List[Unit] = List(())

    override def map[A, B](fa: List[A])(f: A => B): List[B] = fa.map(f)
  }

  def bundle[F[_] : Monoidal, A](x: F[A], y: F[A]): F[List[A]] = Monoidal[F].map2(x, y) { (a: A, b: A) => List(a, b) }

  def appendM[F[_] : Monoidal, A](x: F[A], y: F[List[A]]): F[List[A]] = {
    Monoidal[F].map2(x, y)((a: A, b: List[A]) => {
      a :: b
    })
  }

  def sequence[F[_] : Monoidal, A](list: List[F[A]]): F[List[A]] = {
    list.foldLeft(Monoidal[F].pure(List.empty[A])) { (acc: F[List[A]], cur: F[A]) =>
      appendM(cur, acc)
    }
  }

  def traverse[F[_] : Monoidal, A, B](list: List[A])(f: A => F[B]): F[List[B]] =
    list.foldLeft(Monoidal[F].pure(List.empty[B])) { (acc: F[List[B]], curr: A) =>
      appendM(f(curr), acc)
    }


  def ap[F[_] : Monoidal, A, B](ff: F[A => B], fa: F[A]): F[B] =
    Monoidal[F].map2(ff, fa) { (f: A => B, a: A) =>
      f(a)
    }

  //Given two Option[Int] multiply the int values if they exist or leave them unchanged if one of them doesn't
  def combineOptions(x: Option[Int], y: Option[Int]): Option[Int] ={
//    (x, y) match {
//      case (Some(a), Some(b)) => Some(a * b)
//      case _ => None
//    }

      x.map2(y)((a, b) => a * b)
}

  //Foldable

  @typeclass trait Foldable[F[_]] {
    def foldMap[A, B: Monoid](fa: F[A])(f: A => B): B

    def combineAll[A: Monoid](fa: F[A]): A = foldMap(fa)(identity)
  }

  implicit def optionFoldable: Foldable[Option] = new Foldable[Option] {
    override def foldMap[A, B: Monoid](fa: Option[A])(f: A => B): B =
      fa.foldLeft(Monoid[B].empty)((b: B, a: A) => b |+| f(a) )
  }

  implicit def listFoldable: Foldable[List] = new Foldable[List] {
    override def foldMap[A, B: Monoid](fa: List[A])(f: A => B): B =
      fa.foldLeft(Monoid[B].empty)((b: B, a: A) => b |+| f(a))
  }

  implicit def setFoldable: Foldable[Set] = new Foldable[Set] {
    override def foldMap[A, B: Monoid](fa: Set[A])(f: A => B): B =
      fa.foldLeft(Monoid[B].empty)((b: B, a: A) => b |+| f(a))
  }

  // Turn this foldable into a List
  def fromFoldable[F[_] : Foldable, A](fa: F[A]): List[A] =
    fa.foldMap(a => List(a))

  // Find the first element that matches the predicate
  // Hint: YOu might need to defne a new type with a new monoid
  def find[F[_] : Foldable, A](fa: F[A], f: A => Boolean): Option[A] = {
    case class FindMonoid(value: Option[A])

    implicit def findMonoid: Monoid[FindMonoid] = new Monoid[FindMonoid] {
      def empty: FindMonoid = FindMonoid(None)

      def combine(x: FindMonoid, y: FindMonoid): FindMonoid = (x.value, y.value) match {
        case (Some(a), _) => FindMonoid(Some(a))
        case (None, Some(a)) => FindMonoid(Some(a))
        case _ => FindMonoid(None)
       }
    }

    fa.foldMap(a => if (f(a)) FindMonoid(Some(a)) else FindMonoid(None)).value
  }


  //Traversable
  @typeclass trait Traverse[F[_]] {
    def traverse[G[_] : Monoidal, A, B](fa: F[A])(f: A => G[B]): G[F[B]]

    def sequence[G[_] : Monoidal, A](fga: F[G[A]]): G[F[A]] = traverse(fga)(identity)
  }

  implicit def listTraversable: Traverse[List] = new Traverse[List] {
    override def traverse[G[_] : Monoidal, A, B](fa: List[A])(f: A => G[B]): G[List[B]] =
      fa.foldLeft(Monoidal[G].pure(List.empty[B])) {(acc: G[List[B]], cur: A) =>
        appendM(f(cur), acc)
      }
  }

  implicit def optionTraversable: Traverse[Option] = new Traverse[Option] {
    override def traverse[G[_] : Monoidal, A, B](fa: Option[A])(f: A => G[B]): G[Option[B]] = fa match {
      case Some(a) => f(a).map(Some(_))
      case None => Monoidal[G].pure(None)
    }
  }

  implicit def eitherTraversable[E]: Traverse[Either[E, ?]] = new Traverse[Either[E, ?]] {
    override def traverse[G[_] : Monoidal, A, B](fa: Either[E, A])(f: A => G[B]): G[Either[E, B]] = fa match {
      case Right(a) => f(a).map(Right(_))
      case Left(e) => Monoidal[G].pure(Left(e))
    }
  }


  //Validated

  sealed trait Validated[+E, +A]

  case class Valid[+A](a: A) extends Validated[Nothing, A]

  case class Invalid[+E](e: E) extends Validated[E, Nothing]

  type ValidatedList[+E, +A] = Validated[List[E], A]

  def toEither[E, A](v: Validated[E, A]): Either[E, A] = v match {
    case Valid(x) => Right(x)
    case Invalid(e) => Left(e)
  }

  def toValidated[E, A](e: Either[E, A]): Validated[E, A] = e match {
    case Left(e) => Invalid(e)
    case Right(v) => Valid(v)
  }

  implicit def validatedMonoidal[E: Monoid]: Monoidal[Validated[E, ?]] = new Monoidal[Validated[E, ?]] {
    override def unit: Validated[E, Unit] = Valid(())

    override def product[A, B](fa: Validated[E, A], fb: Validated[E, B]): Validated[E, (A, B)] = (fa, fb) match {
      case (Valid(a), Valid(b)) => Valid((a, b))
      case (Invalid(e), Invalid(e2)) => Invalid(e |+| e2)
      case (Invalid(e), _) => Invalid(e)
      case (_, Invalid(e)) => Invalid(e)
    }

    override def map[A, B](fa: Validated[E, A])(f: A => B): Validated[E, B] = fa match {
      case Valid(a) => Valid(f(a))
      case Invalid(e) => Invalid(e)
    }
  }

  implicit def validatedTraversable[E]: Traverse[Validated[E, ?]] = new Traverse[Validated[E, ?]] {
    override def traverse[G[_] : Monoidal, A, B](fa: Validated[E, A])(f: A => G[B]): G[Validated[E, B]] = fa match {
      case Valid(v) => f(v).map(Valid(_))
      case Invalid(e) => Monoidal[G].pure(Invalid(e))
    }
  }


  // Validation exercise
  // Use `Validated` and everything you've learned so far to solve this one
  // In the `model` object you can find two lists of unvalidated strings representing users
  // Your job is to check all of them whether they are valid or not.
  // To do so, you should use the `User.validate` function.
  // Once you're done, you can check the difference to Either
  def allUsers: ValidatedList[String, List[User]] = userList1.traverse(User.validate)


  // Next we want to write a function that takes a String representing a user
  // and return the UserReport for that user using the `User.fetchReport` function
  def reportForUser(u: String): Future[ValidatedList[String, UserReport]] = {
    User.validate(u).traverse(User.fetchReport(_))
  }


  // Hard: Now get all reports for all the users
  def allReports = {
    val x: Future[List[ValidatedList[String, UserReport]]] = userList1.traverse(reportForUser)

    x.map(l => l.sequence)
  }


  // Nested Monoidals

  case class Nested[F[_], G[_], A](value: F[G[A]])

  implicit def nestedMonoidal[F[_] : Monoidal, G[_] : Monoidal]: Monoidal[Nested[F, G, ?]] =
    new Monoidal[Nested[F, G, ?]] {
      override def unit: Nested[F, G, Unit] = Nested(Monoidal[F].pure(Monoidal[G].unit))

      override def map[A, B](fa: Nested[F, G, A])(f: A => B): Nested[F, G, B] = Nested(fa.value.map((ga: G[A]) => ga.map(f)))

      override def product[A, B](fa: Nested[F, G, A], fb: Nested[F, G, B]): Nested[F, G, (A, B)] = {
        val fga = fa.value
        val fgb = fb.value

        val tupledF = fga.product(fgb)

        Nested(tupledF.map { tuple =>
          val ga = tuple._1
          val gb = tuple._2
          ga.product(gb)
        })
      }
    }

  // Try implementing `allReports` using `Nested`, it should be much easier this way
  def allReportsUsingNested(list: List[String]): Future[ValidatedList[String, List[UserReport]]] = list.traverse(s => {
    Nested(reportForUser(s))

  }).value


  @typeclass trait ContravariantFunctor[F[_]] {
    def contramap[A, B](fa: F[A])(f: B => A): F[B]
  }

  case class Predicate[A](run: A => Boolean)

  case class StringEncoder[A](run: A => String)

  implicit def predicateContravariant: ContravariantFunctor[Predicate] = new ContravariantFunctor[Predicate] {
    override def contramap[A, B](fa: Predicate[A])(f: B => A): Predicate[B] = Predicate(b => fa.run(f(b)))
  }

  implicit def stringEncoderContravariant: ContravariantFunctor[StringEncoder] = new ContravariantFunctor[StringEncoder] {
    override def contramap[A, B](fa: StringEncoder[A])(f: B => A): StringEncoder[B] = StringEncoder(b => fa.run(f(b)))
  }
}

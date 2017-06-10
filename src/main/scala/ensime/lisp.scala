package ensime
package sexp

import scalaz.Functor

import matryoshka._

sealed trait SExp[+A]

import cats._
import cats.implicits._

import matryoshka.data._
import matryoshka.implicits._

object SExp {

  case class Pair[A](left: A, right: A) extends SExp[A]
  case class Symbol(symbol: String) extends SExp[Nothing]
  case class Keyword(keyword: String) extends SExp[Nothing]
  case class Int(value: _root_.scala.Int) extends SExp[Nothing]
  case class Float(value: _root_.scala.Float) extends SExp[Nothing]
  case class Char(c: _root_.scala.Char) extends SExp[Nothing]

  case object Nil extends SExp[Nothing]
  case object True extends SExp[Nothing]

  implicit val ensimeSExpFunctor: Functor[SExp] = new Functor[SExp] {
    def map[A, B](fa: SExp[A])(f: A => B): SExp[B] = fa match {
      case Pair(l, r) => Pair(f(l), f(r))
      case s @ Symbol(_) => s
      case k @ Keyword(_) => k
      case n @ Int(_) => n
      case n @ Float(_) => n
      case c @ Char(_) => c  
      case Nil => Nil
      case True => True  
    }
  }

  implicit def ensimeSExpShow[F[_[_]]](
    implicit T0: Recursive.Aux[F[SExp], SExp],
    T1: Birecursive.Aux[F[RTree[String, ?]], RTree[String, ?]]
  ): Show[F[SExp]] = new Show[F[SExp]] {

    def show(sexp: F[SExp]): String =
      sexp.cata(tree).cata(RTree.show)

  }

  def tree[F[_[_]]](
    implicit T: Birecursive.Aux[F[RTree[String, ?]], RTree[String, ?]]
  ): Algebra[SExp, F[RTree[String, ?]]] = {
    case Nil => T.embed(RTree.Node(List.empty))
    case Pair(l, r) => r.project match {
      case RTree.Node(t) => T.embed(RTree.Node(l :: t))
      case RTree.Leaf(r) => T.embed(RTree.Leaf(s"($l . $r)"))
    }
    case Int(v) => T.embed(RTree.Leaf(v.toString))
    case Float(v) => T.embed(RTree.Leaf(v.toString))
    case True => T.embed(RTree.Leaf("t"))  
    case Char(c) => T.embed(RTree.Leaf(s"?$c"))
    case Keyword(k) => T.embed(RTree.Leaf(s":$k"))
    case Symbol(s) => T.embed(RTree.Leaf(s"'$s"))
  }

}

sealed trait RTree[+A, +B]

object RTree {

  case class Leaf[A](a: A) extends RTree[A, Nothing] { self =>
    //TODO: do we need this?
    def narrow[BB]: RTree[A, BB] = self
  }
  case class Node[B](ns: List[B]) extends RTree[Nothing, B]

  implicit def ensimeRTreeFunctor[A]: Functor[RTree[A, ?]] = new Functor[RTree[A, ?]] {
    def map[AA, B](fa: RTree[A, AA])(f: AA => B): RTree[A, B] = fa match {
      case Leaf(a) => Leaf(a)
      case Node(xs) => Node(xs.map(f))
    }
  }

  //TODO: StringBuilder if needed
  val show: Algebra[RTree[String, ?], String] = {
    case Node(Nil) => "nil"
    case Node(xs) => s"""(${xs.mkString(" ")})"""  
    case Leaf(s) => s
  }
}

object Test {

  import SExp._

  val lispy: Fix[SExp] = Fix[SExp](Pair(Fix[SExp](Char('f')), Fix[SExp](Pair(Fix[SExp](Int(1)), Fix[SExp](Nil)))))

  //Fix[SExp](Pair(Fix[SExp](SExp.Nil), Fix[SExp](SExp.Pair(Fix[SExp](SExp.Number(1)), Fix[SExp](SExp.Nil)))))

  def main(args: Array[String]): Unit = {
    println(lispy.show)
  }
}

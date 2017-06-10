// Copyright: 2010 - 2017 https://github.com/ensime/ensime-server/graphs
// License: http://www.gnu.org/licenses/lgpl-3.0.en.html
package ensime
package sexp

import cats._
import cats.implicits._

import matryoshka._

trait SExpParser[F[_[_]]] {
  def parse(text: String): Either[Throwable, F[SExp]]
}

final class ELispParser[F[_[_]]](implicit T: Corecursive.Aux[F[SExp], SExp]) extends SExpParser[F] {

  import fastparse.all._

  val newline = P(StringIn("\n", "\r\n", "\r", "\f"))
  val whitespace: Parser[Unit] = P(" " | "\t" | newline)
  //TODO: simplify
  val charChunks: Parser[Unit] = P(CharsWhile(c => c != '\n' && c != '\r' && c != '\f'))
  val comment: Parser[Unit] = P(";;" ~ charChunks.rep ~ &(newline | End))

  val lb: Parser[Unit] = P(whitespace.rep ~ "(" ~ whitespace.rep)
  val rb: Parser[Unit] = P(whitespace.rep ~ ")" ~ whitespace.rep)


  val charUnicodes = '\u0021' to '\u007e'
  def normalChar = CharPred(c => charUnicodes.contains(c) && c != '\"' && c != '\\')

  val operators = CharIn("+-*/_~!@$%^&=:<>{}")

  def sequence[G[_]: Foldable](sexp: G[F[SExp]]): F[SExp] =
    sexp.foldLeft(T.embed(SExp.Nil))((b, a) => T.embed(SExp.Pair(a, b)))

  val nil: Parser[F[SExp]] = P("nil").map(_ => T.embed(SExp.Nil))
  val t: Parser[F[SExp]] = P("t").map(_ => T.embed(SExp.True))
  val char: Parser[F[SExp]] = P("?" ~ normalChar.!).map(c => T.embed(SExp.Char(c(0))))

  //TODO: alleycats
  val list: Parser[F[SExp]] = P(lb ~ (list | t | char | nil).rep ~ rb).map(sexp => sequence(sexp.toList))

  val sexp: Parser[F[SExp]] = list | nil | t | char

  def parse(text: String): Either[Throwable, F[SExp]] =
    sexp.parse(text) match {
      case Parsed.Success(sexp, _) => sexp.asRight
      case _ => new IllegalArgumentException("problem").asLeft 
    }
}

object ELispParser {
  def apply[F[_[_]]](implicit T: Corecursive.Aux[F[SExp], SExp]): SExpParser[F] =
    new ELispParser
}

import cats.implicits._
import matryoshka.data._

trait SExpSyntax {

  private val elispParser = ELispParser[Fix]

  implicit class ELispStringContext(val sc: StringContext) {
    def elisp(args: Fix[SExp]*): Fix[SExp] = {
      val strings = sc.parts.iterator
      val subexps = args.iterator
      var sb = new StringBuilder(strings.next)
      while(strings.hasNext) {
        sb
          .append(subexps.next.show)
          .append(strings.next)
      }
      elispParser.parse(sb.toString) match {
        case Left(e) => throw e
        case Right(a) => a 
      }
    }
  }

}


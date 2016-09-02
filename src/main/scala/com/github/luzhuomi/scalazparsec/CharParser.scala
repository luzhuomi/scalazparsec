
package com.github.luzhuomi.scalazparsec

import scalaz._
import Scalaz._

object CharParser // CharParser is a Nonbacktracking Parsec with parametrized state 'S'
{

  /* Haskell
   * data Result a = Consumed a | Empty a deriving Show
   */
  abstract class Result[A]

  case class Consumed[A](value: A) extends Result[A]

  case class Empty[A](value: A) extends Result[A]

  type Token = Char


  // Haskell
  // newtype Parserr st a = Parser ((st,[Token]) -> [(a,(st,[Token]))]
  case class Parser[S, A](p: ((S, List[Token])) => Result[Option[(A, (S, List[Token]))]])

  def run[S, A](parser: Parser[S, A])(st_toks: (S, List[Token])): Result[Option[(A, (S, List[Token]))]] = {
    parser match {
      case Parser(p) => p(st_toks)
    }
  }

  // Scala instances are declared in a lexically scoped, named and implicit object/def
  // implicit def allows type variables, implicit object does not. 
  // Lambda is to replace a partially applied type constructor Parser[S], B is extensitial?
  // 
  
  
  // implicit def parserUnapply[S] = new Unapply[Bind, Parser[S,Token]]{ }

  implicit def parserMonadPlus[S] = new MonadPlus[({type Lambda[B] = Parser[S,B]})#Lambda] {
    // Applicative point
    override def point[A](a: => A): Parser[S, A] = // point is 'pure' from applicative, is the haskell 'return' from monad, scala cleans up the mess with applicative and monad
    Parser(st_toks => Empty(Some((a, st_toks))))
    // Empty empty
    override def empty[A]: Parser[S,A] = Parser(st_toks => Empty(None))
    // Plus plus
    override def plus[A](p: Parser[S,A], q: => Parser[S,A]): Parser[S,A] =
    {
      Parser(st_toks => run(p)(st_toks) match {
        case Empty(None) => run(q)(st_toks) // only back track when it is empty
        case Empty(some) => run(q)(st_toks) match {
          case Empty(_) => Empty(some) // if q fails we report the first errpor
          case consumed => consumed
        }
        case consumed => consumed
      })
    }
    override def bind[A, B](p: Parser[S, A])(f: A => Parser[S, B]): Parser[S, B] =
    Parser(st_toks =>
      run(p)(st_toks) match {
        case Consumed(mb) => {
          lazy val cont = // this has to be lazy, otherwise we are going down the parse even if it is within an attempt(_) {
            mb match {
              case None => None
              case Some((a, st_toks_)) => run(f(a))(st_toks_) match {
                case Consumed(x) => x
                case Empty(x) => x
              }
            }
          Consumed(cont)
        }
        case Empty(mb) => mb match {
          case None => Empty(None)
          case Some((a, sttoks_)) => run((f(a)))(sttoks_)
        }
      }
    )
  } 
  
  // wrappers
  def plus[S,A](p: Parser[S,A], q: => Parser[S,A])(implicit m: MonadPlus[({type Lambda[B] = Parser[S,B]})#Lambda]): Parser[S,A] = m.plus(p, q)

  def empty[S,A](implicit m: MonadPlus[({type Lambda[B] = Parser[S,B]})#Lambda]): Parser[S,A] = m.empty

  def point[S,A](a: => A)(implicit m: MonadPlus[({type Lambda[B] = Parser[S,B]})#Lambda]): Parser[S,A] = m.point(a)

  // combinators
  // determinstic first choice
  def +++[S,A](p: Parser[S,A])(q: => Parser[S,A]): Parser[S,A] = plus(p, q)

  def getState[S]: Parser[S,(S,List[Token])] = Parser(st_toks => Empty(Some((st_toks, st_toks))))

  def setState[S](st_toks: (S,List[Token])): Parser[S,Unit] = Parser(_ => Empty(Some(((), st_toks))))

  // unconditionally parse a single item
  def item[S]: Parser[S,Token] =
    Parser(st_toks => {
      st_toks match {
        case (st,Nil)     => Empty(none: Option[(Token, (S,List[Token]))])
        case (st,(c::cs)) => Consumed((c, (st,cs)).some: Option[(Token, (S,List[Token]))])
      }
    })


  def sat[S](p: Token => Boolean): Parser[S,Token] =
  /*  this doesn't work because item has changed the status to consumed from empty. Even though mzero is executed, the bind op returns Consumed

for { c <- item;
r <- if (p(c)) point(c) else empty
   } yield r
*/
    Parser(st_toks => {
      st_toks match {
        case (st,Nil) => Empty(None)
        case (st,(c :: cs_)) if p(c) => Consumed(Some((c, (st,cs_))))
        case (st,(c :: cs_)) => Empty(None) // otherwise
      }
    })


  // explicit try and backtrack if fails
  def attempt[S,A](p: Parser[S,A]): Parser[S,A] =
    Parser(st_toks => run(p)(st_toks) match {
      case Consumed(None) => Empty(None) // undo the consumed effect if it fails
      case otherwise => otherwise
    })


  // one or more
  def many1[S,A](p: Parser[S,A]): Parser[S,List[A]] = {
    for (a <- p;
         as <- many(p))
      yield (a :: as)
  }

  // zero or more
  def many[S,A](p: Parser[S,A]): Parser[S,List[A]] = {
    Parser(st_toks =>
      run(manyOp(p))(st_toks) match {
        case Empty(Some((x, st_toks_))) => Empty(Some((x.reverse, st_toks_)))
        case Empty(None) => Empty(None)
        case Consumed(Some((x, st_toks_))) => Consumed(Some((x.reverse, st_toks_)))
        case Consumed(None) => Consumed(None)
      })
  }

  def manyOp[S,A](p: Parser[S,A]): Parser[S,List[A]] = {
    def walk(acc: List[A])(ts: (S,List[Token]))(r: Result[Option[(A, (S,List[Token]))]]): Option[(List[A], (S,List[Token]))] =
      r match {
        case Empty(None) => Some((acc, ts))
        // case Empty(_)    => error("many is applied to an empty parser")
        case Consumed(None) => None
        case Consumed(Some((x, ts_))) =>
          val acc_ = (x :: acc)
          walk(acc_)(ts_)(run(p)(ts_))
      }
    Parser(st_toks =>
      run(p)(st_toks) match {
        case Empty(None) => Empty(Some((Nil, st_toks)))
        // case Empty(_)    => error("many is applied to an empty parser")
        case Consumed(x) => Consumed(walk(Nil)(st_toks)(Consumed(x)))
      })
  }

  // interleave As with B as delimeter
  def interleave[S, A, B](pa: Parser[S,A])(pb: Parser[S,B]): Parser[S,List[A]] = {
    lazy val p1 = for (a <- pa;
                       b <- pb;
                       as <- interleave(pa)(pb)
    ) yield (a :: as)
    lazy val p2 = for (a <- pa
    ) yield (List(a))
    +++(attempt(p1))(p2) // for comprehesion is not composable			?
  }

  // either one
  def either1[S, A, B](pa: Parser[S,A])(pb: Parser[S,B]): Parser[S,\/[A, B]] = {
    val p1: Parser[S, \/[A, B]] = for (a <- pa) yield (-\/(a))
    lazy val p2: Parser[S, \/[A, B]] = for (b <- pb) yield (\/-(b))
    plus(attempt(p1), p2)
  }

  // optional
  def optional[S,A](pa: Parser[S,A]): Parser[S,\/[A, Unit]] = {
    val p1: Parser[S,\/[A, Unit]] = for (a <- pa) yield (-\/(a))
    lazy val p2: Parser[S,\/[A, Unit]] = point(\/-(()))
    plus(attempt(p1), p2)
  }
  
  def everythingUntil[S](p: Token => Boolean): Parser[S,List[Token]] =
  {
    for (c <- item[S]; // a bug here? 
        /* require type arg to be explicit, otherwise implicit of Unapply[Bind,Parser[S,Token]] can't be resolved with the following error.
        Kind inference error arising from below
        Implicit not found: scalaz.Unapply[scalaz.Bind, com.github.luzhuomi.scalazparsec.CharParser.Parser[S,com.github.luzhuomi.scalazparsec.CharParser.Token]]. 
        Unable to unapply type `com.github.luzhuomi.scalazparsec.CharParser.Parser[S,com.github.luzhuomi.scalazparsec.CharParser.Token]` 
        into a type constructor of kind `M[_]` that is classified by the type class `scalaz.Bind`.
        */ 
        r <- if (!p(c)) 
        {
          for (cs <- everythingUntil[S](p)) yield (c :: cs)
        }
        else 
        {
          point[S,List[Token]](Nil)
        }
    ) yield r
  }
    
  // get something without consuming the input
  def lookAhead[S,A](p: Parser[S,A]): (Parser[S,A]) =
  {
    for (st_toks <- getState[S];
         x <- p;
         _ <- setState(st_toks))
    yield x
  }
}



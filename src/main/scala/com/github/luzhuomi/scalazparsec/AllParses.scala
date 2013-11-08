package com.github.luzhuomi.scalazparsec

import scalaz._
import Scalaz._


object AllParses // a parsec that returns all possible parse
{

 type Token = Char
  

  // Haskell
  // newtype Parserr a = Parser ([Token] -> [(a,[Token])]
  case class Parser[A] (p : List[Token] => List[(A,List[Token])]) 

  def run[A] (parser:Parser[A]) (tokens:List[Token]) : List[(A,List[Token])] = 
    {
      parser match 
      {
        case Parser(p) => p(tokens)
      }
    }

  /* in Haskell 
   * instance Monad Parser where
   *   -- retrn :: a -> Parser a
   *   return a = Parser (\cs -> [(a,cs)]
   *   -- (>>=) :: (Parser a) -> (a -> Parser b) -> Parser b
   *    p >>= f  = Parser (\cs -> concat [run (f a) cs' | (a,cs') <- run p cs])
   * instance MonadPlus Parser where
   * -- mzero :: Parser a 
   *  mzero = Parser (\cs ->[]) -- fail parser
   * -- mplus :: Parser a -> Parser a -> Parser a
   * p `mplus` q = Parser (\cs -> run p cs ++ run q cs)

   */
  // Scala instances are declared in a lexically scoped, named and implicit object
  implicit object ParserMonadPlus extends MonadPlus[Parser] {
    // Applicative point
    override def point[A](a: => A):Parser[A] = // point is 'pure' from applicative, is the haskell 'return' from monad, scala cleans up the mess with applicative and monad
      Parser( cs => List((a,cs)) )
    // Bind bind
    override def bind[A, B](p: Parser[A])(f: A => Parser[B]): Parser[B] = 
      Parser(cs => 
	{
	  val xs = for ( (a,cs_) <- run(p)(cs) ) yield run(f(a))(cs_)
	  ((List():List[(B,List[Token])]) /: xs) ( (l,x) => l ++ x )
	})
    // Empty empty
    override def empty[A]:Parser[A] = 
      Parser( cs => List() )
    // Plus plus
    override def plus[A](p:Parser[A],q: => Parser[A]) :Parser[A] =
      Parser( cs => (run(p)(cs) ++ (run(q)(cs))) )
  }

  // wrappers
  def plus[A](p:Parser[A],q: => Parser[A])(implicit m:MonadPlus[Parser]):Parser[A] = m.plus(p,q)
  def empty[A](implicit m:MonadPlus[Parser]):Parser[A] = m.empty
  def point[A](a: => A)(implicit m:MonadPlus[Parser]):Parser[A] = m.point(a)

  // combinators
  // determinstic first choice
  def +++[A](p:Parser[A])(q: => Parser[A]) : Parser[A] =
    Parser(cs => 
      { 
	val r = run( plus(p,q) )(cs)
	r match 
	{ 
	  case Nil     => Nil 
	  case (x::xs) => List(x)
	}
      })
  
  // unconditionally parse a single item
  def item : Parser[Token] = 
    Parser(cs => 
      { cs match 
       { case Nil     => Nil
	 case (c::cs) => List((c,cs))
       }
      })
  
  def sat(p:Token => Boolean) : Parser[Token] = 
    // item >>= (c => if (p(c)) { point(c) } else { empty } )  
    // check whether the above is the same as the following // should be the same

    for { c <- item; 
          r <- if (p(c)) point(c) else empty
        } yield r

  // zero or more
  def many[A](p:Parser[A]):Parser[List[A]] = {
    +++ (many1(p)) (point(Nil))
  }
  
  // one or more 
  def many1[A](p:Parser[A]):Parser[List[A]] = {
    for ( a <- p;
	  as <- many(p)
	) yield (a::as)
  }
  // interleave As with B as delimeter
  def interleave[A,B](pa:Parser[A])(pb:Parser[B]) : Parser[List[A]] = {
    lazy val p1 = for (a <- pa ;
		       b <- pb ;
		       as <- interleave(pa)(pb)
		      ) yield (a::as)
    lazy val p2 = for ( a <- pa 
		      ) yield (List(a)) 
    +++(p1)(p2)  // for comprehesion is not composable			?
  }

  // either one
  def either1[A,B](pa:Parser[A])(pb:Parser[B]) : Parser[\/[A,B]] = {
    val p1:Parser[\/[A,B]] = for (a <- pa) yield (-\/(a))
    lazy val p2:Parser[\/[A,B]] = for (b <- pb) yield (\/-(b))
    plus(p1,p2)
  }
  
  // optional
  def optional[A](pa:Parser[A]) : Parser[\/[A,Unit]] = {
    val p1 : Parser[\/[A,Unit]] = for (a <- pa) yield (-\/(a))
    lazy val p2 : Parser[\/[A,Unit]] = point(\/-(()))
    plus(p1,p2)
  }

  // everything until condition
  def everythingUntil(p:Token => Boolean) : Parser[List[Token]] =
    for (c <- item;
	 r <- if (!p(c)) { for (cs <- everythingUntil(p)) yield (c::cs) }
	      else { point(Nil) }
        ) yield r
	 

 
  
}




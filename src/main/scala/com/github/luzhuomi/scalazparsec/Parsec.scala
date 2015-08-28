package com.github.luzhuomi.scalazparsec

import scalaz._
import Scalaz._


object Parsec
{

  type Token = Char
  

  // Haskell
  // newtype Parserr a = Parser ([Token] -> [(a,[Token])]
  case class Parser[A] (p : List[Token] => Option[(A,List[Token])]) 

  def run[A] (parser:Parser[A]) (tokens:List[Token]) : Option[(A,List[Token])] = 
    {
      parser match 
      {
        case Parser(p) => p(tokens)
      }
    }

  /* in Haskell 
 instance Monad Parser where
    -- return :: a -> Parser a
    return a = Parser (\cs -> Just (a,cs))
    -- (>>=)  :: (Parser a) -> (a -> Parser b) -> Parser b
    p >>= f  = Parser (\cs -> case run p cs of
                                Nothing -> Nothing
                                Just (a,cs') -> run (f a) cs')


instance MonadPlus Parser where
    -- mzero :: Parser a 
    mzero = Parser (\cs ->Nothing) -- fail parser
    -- mplus :: Parser a -> Parser a -> Parser a
    p `mplus` q = Parser (\cs -> run p cs `mplus` run q cs)
   */
  // Scala instances are declared in a lexically scoped, named and implicit object
  implicit object ParserMonadPlus extends MonadPlus[Parser] {
    // Applicative point
    override def point[A](a: => A):Parser[A] = // point is 'pure' from applicative, is the haskell 'return' from monad, scala cleans up the mess with applicative and monad
      Parser( cs => Some((a,cs)) )
    // Bind bind
    override def bind[A, B](p: Parser[A])(f: A => Parser[B]): Parser[B] = 
      Parser(cs => 
	run(p)(cs) match {
	  case None => None
	  case Some((a,cs_)) => run(f(a))(cs_) 
	})
    // Empty empty
    override def empty[A]:Parser[A] = 
      Parser( cs => none:Option[(A,List[Token])] )
    // Plus plus
    override def plus[A](p:Parser[A],q: => Parser[A]) :Parser[A] =
      Parser( cs => run(p)(cs) match 
	     { case None => run(q)(cs)
	       case Some(x) => Some(x) })

	      
  }

  // wrappers
  def plus[A](p:Parser[A],q: => Parser[A])(implicit m:MonadPlus[Parser]):Parser[A] = m.plus(p,q)
  def empty[A](implicit m:MonadPlus[Parser]):Parser[A] = m.empty
  def point[A](a: => A)(implicit m:MonadPlus[Parser]):Parser[A] = m.point(a)

  // combinators
  // determinstic first choice
  def +++[A](p:Parser[A])(q: => Parser[A]) : Parser[A] = plus(p,q)
  
  // unconditionally parse a single item
  def item : Parser[Token] = 
    Parser(cs => 
      { cs match 
       { case Nil     => none:Option[(Token,List[Token])]
	 case (c::cs) => (c,cs).some:Option[(Token,List[Token])]
       }
      })
  
  def sat(p:Token => Boolean) : Parser[Token] = 
    // item >>= (c => if (p(c)) { point(c) } else { empty } )  
    // check whether the above is the same as the following // should be the same

    for { c <- item; 
          r <- if (p(c)) point(c) else empty
        } yield r

  // zero or more
  def many1[A](p:Parser[A]):Parser[List[A]] = {
    for (a <- p;
	 as <- many(p)) 
      yield (a::as)
  }
  
  // zero or more 
  def many[A](p:Parser[A]):Parser[List[A]] = {
    Parser ( cs => 
      run (manyOp(p)) (cs) match {
	case Some((x,cs_)) => Some((x.reverse , cs_))
	case None          => None
      })
  }

  def manyOp[A](p:Parser[A]):Parser[List[A]] = {
    def walk(acc:List[A])(ts:List[Token])(r:Option[(A,List[Token])]):Option[(List[A],List[Token])] = 
      r match {
	case None => Some((acc,ts))
	case Some ((x,ts_)) => 
	  val acc_ = (x::acc)
	  walk(acc_)(ts_)(run(p)(ts_))
      }
    Parser( cs => 
      run(p)(cs) match {
	case None => Some((Nil,cs))
	case Some(x) => walk(Nil)(cs)(Some(x))
      })
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




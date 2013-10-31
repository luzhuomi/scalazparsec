
package scala.parsec

import scalaz._
import Scalaz._


object NonBacktracking
{
  /* Haskell
   * data Result a = Consumed a | Empty a deriving Show
   */
  abstract class Result[A]

  case class Consumed[A] (value : A) extends Result[A] 

  case class Empty[A] (value : A)  extends Result[A]

  type Token = Char
  

  // Haskell
  // newtype Parserr a = Parser ([Token] -> [(a,[Token])]
  case class Parser[A] (p : List[Token] => Result[Option[(A,List[Token])]]) 

  def run[A] (parser:Parser[A]) (tokens:List[Token]) : Result[Option[(A,List[Token])]] = 
    {
      parser match 
      {
        case Parser(p) => p(tokens)
      }
    }

  // Scala instances are declared in a lexically scoped, named and implicit object
  implicit object ParserMonadPlus extends MonadPlus[Parser] {
    // Applicative point
    override def point[A](a: => A):Parser[A] = // point is 'pure' from applicative, is the haskell 'return' from monad, scala cleans up the mess with applicative and monad
      Parser( cs => Empty(Some((a,cs))) )
    // Bind bind
    override def bind[A, B](p: Parser[A])(f: A => Parser[B]): Parser[B] = 
      Parser(cs => 
	run(p)(cs) match {
	  case Consumed(mb) => 
	    lazy val cont = // this has to be lazy, otherwise we are going down the parse even if it is within an attempt(_)
	      {
		mb match {
		  case None => None
		  case Some((a,cs_)) => run(f(a))(cs_) match 
		  {
		    case Consumed(x) => x
		    case Empty(x) => x
		  }
		}
	      }
	    Consumed(cont)
	  case Empty(mb) => mb match 
	  {
	    case None => Empty(None)
	    case Some((a,cs_)) => run((f(a)))(cs_)
	  }
	})
    // Empty empty
    override def empty[A]:Parser[A] = 
      Parser( cs => Empty(None) )
    // Plus plus
    override def plus[A](p:Parser[A],q: => Parser[A]) :Parser[A] =
      Parser( cs => run(p)(cs) match 
	     { case Empty(None) => run(q)(cs) // only back track when it is empty 
	       case Empty(some) => run(q)(cs) match 
	       {
	 	 case Empty(_)  => Empty (some) // if q fails we report the first errpor
		 case consumed  => consumed
	        }
	       case consumed   => consumed })

	      
  }

  // wrappers
  def plus[A](p:Parser[A],q: => Parser[A])(implicit m:MonadPlus[Parser]):Parser[A] = m.plus(p,q)
  def empty[A](implicit m:MonadPlus[Parser]):Parser[A] = m.empty
  def point[A](a: => A)(implicit m:MonadPlus[Parser]):Parser[A] = m.point(a)

  // combinators
  // determinstic first choice
  def +++[A](p:Parser[A])(q: => Parser[A]) : Parser[A] = plus(p,q)
  
  def getState:Parser[List[Token]] = Parser( cs => Empty (Some((cs,cs))) )
  
  def setState(cs:List[Token]) : Parser[Unit] = Parser( _ => Empty (Some (((),cs))))

  // unconditionally parse a single item
  def item : Parser[Token] = 
    Parser(cs => 
      { cs match 
       { case Nil     => Empty(none:Option[(Token,List[Token])])
	 case (c::cs) => Consumed((c,cs).some:Option[(Token,List[Token])])
       }
      })
  
  def sat(p:Token => Boolean) : Parser[Token] = 
    /*  this doesn't work because item has changed the status to consumed from empty. Even though mzero is executed, the bind op returns Consumed

    for { c <- item; 
          r <- if (p(c)) point(c) else empty
        } yield r
	*/
    Parser(cs =>
      { cs match 
       { case Nil => Empty(None)
	 case (c::cs_) if p(c) => Consumed(Some((c,cs_)))
	 case (c::cs_) => Empty(None)   // otherwise
      }})
	    

  // explicit try and backtrack if fails
  def attempt[A](p:Parser[A]) : Parser[A] = 
    Parser (cs => run(p)(cs) match 
	    { 
	      case Consumed(None) => Empty(None) // undo the consumed effect if it fails
	      case otherwise      => otherwise 
	    })


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
	case Empty(Some((x,cs_))) => Empty(Some((x.reverse , cs_)))
	case Empty(None)          => Empty(None)
	case Consumed(Some((x,cs_))) => Consumed(Some((x.reverse , cs_)))
	case Consumed(None)          => Consumed(None)
      })
  }

  def manyOp[A](p:Parser[A]):Parser[List[A]] = {
    def walk(acc:List[A])(ts:List[Token])(r:Result[Option[(A,List[Token])]]):Option[(List[A],List[Token])] = 
      r match {
	case Empty(None) => Some((Nil,ts))
	// case Empty(_)    => error("many is applied to an empty parser")
	case Consumed(None) => None
	case Consumed(Some ((x,ts_))) => 
	  val acc_ = (x::acc)
	  walk(acc_)(ts_)(run(p)(ts_))
      }
    Parser( cs => 
      run(p)(cs) match {
	case Empty(None) => Empty(Some((Nil,cs)))
	// case Empty(_)    => error("many is applied to an empty parser")
	case Consumed(x) => Consumed(walk(Nil)(cs)(Consumed(x)))
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
    +++(attempt(p1))(p2)  // for comprehesion is not composable			?
  }

  // either one
  def either1[A,B](pa:Parser[A])(pb:Parser[B]) : Parser[\/[A,B]] = {
    val p1:Parser[\/[A,B]] = for (a <- pa) yield (-\/(a))
    lazy val p2:Parser[\/[A,B]] = for (b <- pb) yield (\/-(b))
    plus(attempt(p1),p2)
  }
  
  // optional
  def optional[A](pa:Parser[A]) : Parser[\/[A,Unit]] = {
    val p1 : Parser[\/[A,Unit]] = for (a <- pa) yield (-\/(a))
    lazy val p2 : Parser[\/[A,Unit]] = point(\/-(()))
    plus(attempt(p1),p2)
  }

  // everything until condition 
  def everythingUntil(p:Token => Boolean) : Parser[List[Token]] =
    for (c <- item; // a bug here?
	 r <- if (!p(c)) { for (cs <- everythingUntil(p)) yield (c::cs) }
	      else { point(Nil) }
        ) yield r
	 

  // get something without consuming the input
  def lookAhead[A](p:Parser[A]) : (Parser[A]) =
    for (cs <- getState;
	 x  <- p;
	 _  <- setState(cs))
    yield x
	 

  // parsing json
	      
  abstract class JSON { }
  case class JInt(i:Int) extends JSON
  case class JStr(s:String) extends JSON
  case class JMap(nvps:List[NVP]) extends JSON
  case class JList(jsons:List[JSON]) extends JSON

  case class NVP (name:Name)(json:JSON) 

  type Name = String
  
  def parseJSON:Parser[JSON] = 
    +++ (attempt(parseJInt)) ( +++ (attempt(parseJStr)) ( +++ (attempt(parseJMap)) (parseJList)))


  def parseJMap:Parser[JSON] = 
    for (_ <- sat( _ === '{');
	 nvps <- interleave(parseNVP)(sat( _ === ','));
	 _ <- sat( _ === '}') )
      yield JMap(nvps)

  def parseJList:Parser[JSON] =
    for (_ <- sat (_ === '[');
	 jsons <- interleave(parseJSON)(sat( _ === ','));
	 _ <- sat ( _ === ']') ) 
      yield JList(jsons)

  def parseJInt:Parser[JSON] =
    for (ts <- many1(parseDigit) ) yield {
      val x: String = ts.mkString
      JInt(x.toInt)
    }
  
  def parseJStr:Parser[JSON] = 
    for (q <- sat(_ === '"');
	 ts <- everythingUntil( _ === '"') ) yield JStr(ts.mkString) 


  def parseDigit:Parser[Token] = sat ( (t:Token) => t.toInt <= 57 && t.toInt >= 48 )
    

  def parseNVP:Parser[NVP] = 
    for (n <- parseName;
	 c <- sat( _ === ':');
	 v <- parseJSON) 
      yield NVP(n)(v)

  def parseName:Parser[Name] = 
    for (q <- sat (_ === '"');
	 ts <- everythingUntil ( _ === '"'))
      yield (ts.mkString)
  
}



/*
scala> import scala.parsec.NonBacktracking._
import scala.parsec.NonBacktracking._

scala> run (parseJSON) ("{\"x\":1,\"y\":[2]}".toList)

*/

package com.github.luzhuomi.scalazparsec.example

import scalaz._
import Scalaz.{interleave => _, char => _, _}
import com.github.luzhuomi.scalazparsec.CharParser.{
	sat => psat, everythingUntil => peverythingUntil,  
	lookAhead => plookAhead, getState => pgetState, setState => psetState, 
	point => ppoint, _
} // p for polymorphic


object JsonCharParser {

	// parsing json


	abstract class JSON { }
	case class JInt(i:Int) extends JSON
	case class JStr(s:String) extends JSON
	case class JMap(nvps:List[NVP]) extends JSON
	case class JList(jsons:List[JSON]) extends JSON

	case class NVP (name:Name)(json:JSON) 

	type Name = String

	case class State(line:Int)

	val initState = State(0)

	def incrLine(s:(State,List[Token])):(State,List[Token]) = s match 
	{
		case (State(l),tks) => (State(l+1),tks)
	}


	def isWhiteSpace(c:Char):Boolean = c match
	{
		case ' ' => true
		case '\t' => true
		case '\n' => true
		case '\r' => true
		case _ => false
	}

	def sat(p: Token => Boolean): Parser[State,Token] = for 
	{
		c <- psat[State](p)
		_ <- incrLineIfRet(c)
	} yield c

	type L[X] = Parser[State,X]
	def everythingUntil(p: Token => Boolean): Parser[State,List[Token]] = for 
	{
		cs <- peverythingUntil[State](p)
		_  <- cs.map( (c:Token) => incrLineIfRet(c)).sequence[L,Unit] //[({type l[A]=Parser[State,A]})#l,Unit]
	} yield cs
	def lookAhead[A](p: Parser[State,A]): (Parser[State,A]) = plookAhead[State,A](p)
	def getState: Parser[State,(State,List[Token])] = pgetState
	def setState(st_toks: (State,List[Token])): Parser[State,Unit] = psetState(st_toks)
	def point[A](x:A):Parser[State,A] = ppoint(x)

	def incrLineIfRet(c:Token) : Parser[State,Unit] = 
	{
		if ((c == '\n') || (c == '\r'))
		{	
			for 
			{ 
				st <- getState
				_  <- setState(incrLine(st))
			} yield ()
		} 
		else 
		{
			point(())
		}
	} 

	def whiteSpace:Parser[State,Char] = for 
	{
		c <- sat(x => isWhiteSpace(x))
		_ <- incrLineIfRet(c)
	} yield c
	

	def parseJSON:Parser[State,JSON] = for 
	{	
		_ <- many(whiteSpace)
		json <- +++ (attempt(parseJInt)) ( +++ (attempt(parseJStr)) ( +++ (attempt(parseJList)) (parseJMap)))
	} yield json

	def parseJSONFast:Parser[State,JSON] =
	for {
			_ <- many(whiteSpace)
			c <- lookAhead(item)
			r <- c match {
				case '"' => parseJStr
				case '{' => parseJMap
				case '[' => parseJList
				case _	 => parseJInt
			}
		} yield r


	def parseJMap:Parser[State,JSON] = 
	for { 
			_ <- sat( _ == '{')
			nvps <- interleave(parseNVP)(sat( _ == ','))
			_ <- sat( _ == '}') 
		}
	yield JMap(nvps)

	def parseJList:Parser[State,JSON] =
	for {
			_ <- sat (_ == '[')
			jsons <- interleave(parseJSON)(sat( _ == ','))
			_ <- sat ( _ == ']') 
		}
	yield JList(jsons)

	def parseJInt:Parser[State,JSON] =
	for {
			ts <- many1(parseDigit) 
		} yield 
		{
			val x: String = ts.mkString
			JInt(x.toInt)
		}

	def parseJStr:Parser[State,JSON] = 
	for {
			q <- sat(_ == '"')
			ts <- everythingUntil( _ == '"') 
		} yield JStr(ts.mkString) 


	def parseDigit:Parser[State,Token] = sat ( (t:Token) => t.toInt <= 57 && t.toInt >= 48 )


	def parseNVP:Parser[State,NVP] = 
	for {
			n <- parseName
			c <- sat( _ == ':')
			v <- parseJSON
		}
	yield NVP(n)(v)

	def parseName:Parser[State,Name] = 
	for {
			q <- sat (_ == '"')
			ts <- everythingUntil ( _ == '"')
		}
	yield (ts.mkString)

	val testStr = """
	{"x":1,
	"y":[2]}
	"""

	def parse(s:String):Unit = {
		run(parseJSON)(initState,s.toList) match 
		{
			case Consumed(Fail(err,(State(ln),toks))) => println(err + s"at line $ln with '" + toks.take(20).mkString+ "'")
			case Empty(Fail(err,(State(ln),toks))) => println(err + s"at line $ln with '" + toks.take(20).mkString + "'")
			case otherwise => println(otherwise)
		}
	}

}

/*
$ sbt console

scala> import com.github.luzhuomi.scalazparsec.example.JsonCharParser._

scala> import com.github.luzhuomi.scalazparsec.CharParser._
import com.github.luzhuomi.scalazparsec.Parsec._

scala> run(parseJSON)(initState,testStr.toList)
scala> run(parseJSONFast)(initState,testStr.toList)

*/

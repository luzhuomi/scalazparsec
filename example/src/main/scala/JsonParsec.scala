package com.github.luzhuomi.scalazparsec.example

import scalaz._
import Scalaz.{interleave => _, char => _, _}
import com.github.luzhuomi.scalazparsec.Parsec._

object JsonParsec {

  // parsing json
	      
  abstract class JSON { }
  case class JInt(i:Int) extends JSON
  case class JStr(s:String) extends JSON
  case class JMap(nvps:List[NVP]) extends JSON
  case class JList(jsons:List[JSON]) extends JSON

  case class NVP (name:Name)(json:JSON) 

  type Name = String
  
  def parseJSON:Parser[JSON] = 
    +++ (parseJInt) ( +++ (parseJStr) ( +++ (parseJMap) (parseJList)))

  def parseJMap:Parser[JSON] = 
    for (_ <- sat( _ == '{');
	 nvps <- interleave(parseNVP)(sat( _ == ','));
	 _ <- sat( _ == '}') )
      yield JMap(nvps)

  def parseJList:Parser[JSON] =
    for (_ <- sat (_ == '[');
	 jsons <- interleave(parseJSON)(sat( _ == ','));
	 _ <- sat ( _ == ']') ) 
      yield JList(jsons)

  def parseJInt:Parser[JSON] =
    for (ts <- many1(parseDigit) ) yield {
      val x: String = ts.mkString
      JInt(x.toInt)
    }
  
  def parseJStr:Parser[JSON] = 
    for (q <- sat(_ == '"');
	 ts <- everythingUntil( _ == '"') ) yield JStr(ts.mkString) 


  def parseDigit:Parser[Token] = sat ( (t:Token) => t.toInt <= 57 && t.toInt >= 48 )
    

  def parseNVP:Parser[NVP] = 
    for (n <- parseName;
	 c <- sat( _ == ':');
	 v <- parseJSON) 
      yield NVP(n)(v)

  def parseName:Parser[Name] = 
    for (q <- sat (_ == '"');
	 ts <- everythingUntil ( _ == '"'))
      yield (ts.mkString)
}


/*
$ sbt console

scala> import com.github.luzhuomi.scalazparsec.example.JsonParsec._
import com.github.luzhuomi.scalazparsec.example.JsonParsec._

scala> import com.github.luzhuomi.scalazparsec.Parsec._
import com.github.luzhuomi.scalazparsec.Parsec._

scala> run(parseJSON)("{\"x\":1,\"y\":[2]}".toList)
res2: Option[(com.github.luzhuomi.scalazparsec.Parsec.JSON, List[com.github.luzhuomi.scalazparsec.Parsec.Token])] = Some((JMap(List(NVP(x), NVP(y))),List()))
*/

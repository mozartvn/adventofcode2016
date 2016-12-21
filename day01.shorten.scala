import scala.io.Source

var input : List[String] = Source.fromFile("day01.input")
								  .getLines()
								  .toList
								  .head
								  .split(",")
								  .toList
								  .map(_.trim)

val direct = Map("NR" -> "E", 
				 "NL" -> "W", 
				 "SR" -> "W", 
				 "SL" -> "E", 
				 "ER" -> "S", 
				 "EL" -> "N", 
				 "WR" -> "N", 
				 "WL" -> "S")

def nav(p: (String, Int, Int), step: String) : (String, Int, Int) = 
	direct( p._1 + step.head.toString ) match 
		{
			case "N" => ( "N", p._2 + step.tail.toInt, p._3 )
			case "S" => ( "S", p._2 - step.tail.toInt, p._3 )
			case "E" => ( "E", p._2, p._3 + step.tail.toInt )
			case "W" => ( "W", p._2, p._3 - step.tail.toInt )
		}

val result = input.scanLeft(("N", 0, 0))(nav)
			.last
			.productIterator
			.toList
			.tail
			.map(t => Math.abs(t.asInstanceOf[Int]))
			.sum

println(result)

//part II
//@Todo
var path : List[(Int, Int)] = input.scanLeft(("N", 0, 0))(nav)
								   .map(t => (t._2, t._3))								   

println(path)

def cross(a: (Int, Int), b: (Int, Int)) : List[(Int, Int)] = {
	
	val x = if ( a._1 > b._1 ) Range( b._1, a._1 )
		else if ( a._1 < b._1 ) Range( a._1, b._1 )
		else a._1

	val y = if ( a._2 > b._2 ) Range( b._2, a._2)
		else if ( a._2 < b._2 ) Range(a._2, b._2)
		else a._2

	//for (i <- x; j <- y) yield (i, j)
	List()
}
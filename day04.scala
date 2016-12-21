import scala.io.Source

val regex = "([a-z-]+)-(\\d+)\\[([a-z]{5})\\]".r

def checksum(str: String) : String = str.replace("-", "").toList
										.groupBy(identity)
										.map(t => (t._1, t._2.length))
										.toList
										.sortWith( (a,b) => ( a._2 > b._2 ) || ( (a._2 == b._2) && (a._1 < b._1) ) )
										.slice(0, 5)
										.flatMap(_._1.toString)
										.mkString

val results = for ( line <- Source.fromFile("day04.input").getLines() )		
					yield regex.findAllIn(line)
							 	.matchData
							 	.map(t => (t.group(1), t.group(2), t.group(3)))
							 	.toList
							 	.filter(t => checksum(t._1) == t._3 )

println(results.toList.filter(!_.isEmpty).flatMap(t => t).map(_._2.toInt).sum)

//part 2
val res = for (line <- Source.fromFile("day04.input").getLines())
				yield regex.findAllIn(line)
					 .matchData
					 .map( t => ( t.group(1), t.group(2) ) )
					 .toList

def decrypt(m: (String, String)) : (String, String) = {
			val decrypt = m._1.toList.map(c => if (c != '-')								   				 
								   				 {
								   				 	val trans = (c.toInt + m._2.toInt % 26)
								   				 	if (trans > 'z'.toInt) 
								   				 	 	(trans - 'z'.toInt + 'a'.toInt -1 ).toChar
								   				 	else
								   				 		trans.toChar
								   				 } else c)				

			(decrypt.mkString.replace("-"," "), m._2)
}

val result2 = res.flatMap(t => t).map(decrypt(_)).filter(r => r._1.contains("northpole object storage"))

result2.toList.foreach(println)
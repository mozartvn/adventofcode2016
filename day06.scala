import scala.io.Source

val input = (for ( line <- Source.fromFile("day06.input").getLines() ) yield line).toList

val res = for (i <- Range(0, input.head.length) )
			yield input.map(_(i)).groupBy(identity)
					   .map(c => (c._1, c._2.length))
					   .toList
					   .sortWith((a,b) => a._2 > b._2)
					   .head._1

println(res.mkString)
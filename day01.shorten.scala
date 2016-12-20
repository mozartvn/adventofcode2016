val input = List("R3", "L5", "R1", "R2", "L5", "R2", "R3", "L2", "L5", "R5", "L4", "L3", "R5", "L1", "R3", "R4", "R1", "L3", "R3", "L2", "L5", "L2", "R4", "R5", "R5", "L4", "L3", "L3", "R4", "R4", "R5", "L5", "L3", "R2", "R2", "L3", "L4", "L5", "R1", "R3", "L3", "R2", "L3", "R5", "L194", "L2", "L5", "R2", "R1", "R1", "L1", "L5", "L4", "R4", "R2", "R2", "L4", "L1", "R2", "R53", "R3", "L5", "R72", "R2", "L5", "R3", "L4", "R187", "L4", "L5", "L2", "R1", "R3", "R5", "L4", "L4", "R2", "R5", "L5", "L4", "L3", "R5", "L2", "R1", "R1", "R4", "L1", "R2", "L3", "R5", "L4", "R2", "L3", "R1", "L4", "R4", "L1", "L2", "R3", "L1", "L1", "R4", "R3", "L4", "R2", "R5", "L2", "L3", "L3", "L1", "R3", "R5", "R2", "R3", "R1", "R2", "L1", "L4", "L5", "L2", "R4", "R5", "L2", "R4", "R4", "L3", "R2", "R1", "L4", "R3", "L3", "L4", "L3", "L1", "R3", "L2", "R2", "L4", "L4", "L5", "R3", "R5", "R3", "L2", "R5", "L2", "L1", "L5", "L1", "R2", "R4", "L5", "R2", "L4", "L5", "L4", "L5", "L2", "L5", "L4", "R5", "R3", "R2", "R2", "L3", "R3", "L2", "L5")

val direct = Map("NR" -> "E", "NL" -> "W", "SR" -> "W", "SL" -> "E", "ER" -> "S", "EL" -> "N", "WR" -> "N", "WL" -> "S")

def nav(p: (String, Int, Int), step: String) : (String, Int, Int) = direct( p._1 + step.head.toString ) match {
		case "N" => ( "N", p._2 + step.tail.toInt, p._3 )
		case "S" => ( "S", p._2 - step.tail.toInt, p._3 )
		case "E" => ( "E", p._2, p._3 + step.tail.toInt )
		case "W" => ( "W", p._2, p._3 - step.tail.toInt )
}

val caculate = input.scanLeft(("N", 0, 0))(nav)

val result = caculate
			.last
			.productIterator
			.toList
			.tail
			.map(t => Math.abs(t.asInstanceOf[Int]))
			.sum

println(result)

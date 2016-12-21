/*

		http://adventofcode.com/2016/day/1

							                 col--			  col++
							
		
                                       ^
               row++                 L | R
                                       |                                       
                                       N           
                                       |
                                       |
		 				              	R          |             L
					            <-------W --------------- E------>
	     					            L          |             R
                                       |
                                       |
                                       S                                       
                                       |
               row--       	         R | L
                                       |
                                       v

*/

val input = List("R3", "L5", "R1", "R2", "L5", "R2", "R3", "L2", "L5", "R5", "L4", "L3", "R5", "L1", "R3", "R4", "R1", "L3", "R3", "L2", "L5", "L2", "R4", "R5", "R5", "L4", "L3", "L3", "R4", "R4", "R5", "L5", "L3", "R2", "R2", "L3", "L4", "L5", "R1", "R3", "L3", "R2", "L3", "R5", "L194", "L2", "L5", "R2", "R1", "R1", "L1", "L5", "L4", "R4", "R2", "R2", "L4", "L1", "R2", "R53", "R3", "L5", "R72", "R2", "L5", "R3", "L4", "R187", "L4", "L5", "L2", "R1", "R3", "R5", "L4", "L4", "R2", "R5", "L5", "L4", "L3", "R5", "L2", "R1", "R1", "R4", "L1", "R2", "L3", "R5", "L4", "R2", "L3", "R1", "L4", "R4", "L1", "L2", "R3", "L1", "L1", "R4", "R3", "L4", "R2", "R5", "L2", "L3", "L3", "L1", "R3", "R5", "R2", "R3", "R1", "R2", "L1", "L4", "L5", "L2", "R4", "R5", "L2", "R4", "R4", "L3", "R2", "R1", "L4", "R3", "L3", "L4", "L3", "L1", "R3", "L2", "R2", "L4", "L4", "L5", "R3", "R5", "R3", "L2", "R5", "L2", "L1", "L5", "L1", "R2", "R4", "L5", "R2", "L4", "L5", "L4", "L5", "L2", "L5", "L4", "R5", "R3", "R2", "R2", "L3", "R3", "L2", "L5")

//build map to dertermine begining position and Easter Bunny HQ coordinates
val rows = 500
val cols = 500

var matrix = Array.ofDim[Int](rows, cols)

def direct(d: Char, step: Char): Char = d match {
  case 'N' => step match {
    case 'R' => 'E'
    case 'L' => 'W'
  }
  case 'S' => step match {
    case 'R' => 'W'
    case 'L' => 'E'
  }
  case 'E' => step match {
    case 'R' => 'S'
    case 'L' => 'N'
  }
  case 'W' => step match {
    case 'R' => 'N'
    case 'L' => 'S'
  }
}

//init
var _d = 'N'
val start_x = 200
val start_y = 200
var _x = start_x
var _y = start_y

var isFound: Boolean = false
var msg: String = null

def meetTwice(x: Int, y: Int) = {
  if (!isFound && matrix(x)(y) == 2) {
    isFound = true

    msg = s"${Math.abs(start_x - x) + Math.abs(start_y - y)}"
  }
}

for (step <- input) {
  val pre = _d
  _d = direct(_d, step.head)

  println(s"${pre} -- ${step} --> ${_d}")

  _d match {
    case 'N' => {
      for (i <- _x + 1 to _x + step.tail.toInt) {
        matrix(i)(_y) += 1
        meetTwice(i, _y)
      }
      _x = _x + step.tail.toInt
    }
    case 'S' => {
      for (i <- _x - step.tail.toInt to _x - 1) {
        matrix(i)(_y) += 1
        meetTwice(i, _y)
      }
      _x = _x - step.tail.toInt
    }
    case 'E' => {
      for (j <- _y + 1 to _y + step.tail.toInt) {
        matrix(_x)(j) += 1
        meetTwice(_x, j)
      }
      _y = _y + step.tail.toInt
    }
    case 'W' => {
      for (j <- _y - step.tail.toInt to _y - 1) {
        matrix(_x)(j) += 1
        meetTwice(_x, j)
      }
      _y = _y - step.tail.toInt
    }
  }
}

println(s"No blocks of shortest path : ${(Math.abs(start_x - _x) + Math.abs(start_y - _y))}")
println(s"No blocks away of the first location meet twice : ${msg}")
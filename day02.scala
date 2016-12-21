/* 
	http://adventofcode.com/2016/day/2

	1  	2 	3
	4	5	6
	7	8	9

	U <=> (x--, y)
	D <=> (x++, y)
	L <=> (x, y--)
	R <=> (x, y++)

*/

import scala.io.Source

val remote = List(List(1, 2, 3), List(4, 5, 6), List(7, 8, 9))

case class Button(x: Int = 1, y: Int = 1)

def move(cb: Button, m: Char): Button = {
  m match {
    case 'U' => if (cb.x - 1 >= 0) new Button(cb.x - 1, cb.y) else cb
    case 'D' => if (cb.x + 1 <= 2) new Button(cb.x + 1, cb.y) else cb
    case 'L' => if (cb.y - 1 >= 0) new Button(cb.x, cb.y - 1) else cb
    case 'R' => if (cb.y + 1 <= 2) new Button(cb.x, cb.y + 1) else cb
    case _ => cb
  }
}

var preButton = new Button(1, 1)

for (line <- Source.fromFile("day02.input").getLines()) {
  val code = line.toList.scanLeft(preButton)(move).last
  preButton = code
  print(remote(code.x)(code.y))
}

//part II
val remote2 = List(
  List('0', '0', '1', '0', '0'),
  List('0', '2', '3', '4', '0'),
  List('5', '6', '7', '8', '9'),
  List('0', 'A', 'B', 'C', '0'),
  List('0', '0', 'D', '0', '0')
)

def move2(cb: Button, m: Char): Button = {
  m match {
    case 'U' => if (cb.x - 1 >= 0 && remote2(cb.x - 1)(cb.y) != '0') new Button(cb.x - 1, cb.y) else cb
    case 'D' => if (cb.x + 1 <= 4 && remote2(cb.x + 1)(cb.y) != '0') new Button(cb.x + 1, cb.y) else cb
    case 'L' => if (cb.y - 1 >= 0 && remote2(cb.x)(cb.y - 1) != '0') new Button(cb.x, cb.y - 1) else cb
    case 'R' => if (cb.y + 1 <= 4 && remote2(cb.x)(cb.y + 1) != '0') new Button(cb.x, cb.y + 1) else cb
    case _ => cb
  }
}

preButton = new Button(2, 0) //5
println

for (line <- Source.fromFile("day02.input").getLines()) {
  val code = line.toList.scanLeft(preButton)(move2).last
  preButton = code
  print(remote2(code.x)(code.y))
}
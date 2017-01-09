package src;

object day22 extends App {

  val re = "/dev/grid/node-(x\\d+-y\\d+)\\s+(\\d+)T\\s+(\\d+)T\\s+(\\d+)T\\s+(\\d+)%".r

  val input = (for (line <- Source.fromFile("day22.input").getLines())
    yield re.findAllIn(line)
      .matchData
      .map(t => new Node(t.group(1), t.group(2).toInt, t.group(3).toInt, t.group(4).toInt))
      .toList
    ).toList.flatMap(t => t)

  val pairs = for (
    a <- input.filter(_.used > 0);
    b <- input;
    if (a.used <= b.avaiable) && (a.name != b.name)
  ) yield (a, b)


  //part 2
  val row = 32

  println(pairs.toList.size)
  val col = 28
  var matrix = Array.ofDim[Int](row, col)

  def isConnected(a: Node, b: Node): Boolean = {
    ((a.x == b.x) && (Math.abs(a.y - b.y) == 1)) || ((a.y == b.y) && (Math.abs(a.x - b.x) == 1))
  }

  case class Node(name: String, size: Int, used: Int, avaiable: Int) {
    def x = name.split("-").head.tail.mkString.toInt

    def y = name.split("-").tail.toList.head.tail.toInt
  }

  pairs.filter(p => isConnected(p._1, p._2)).foreach(p => {
    matrix(p._1.x)(p._2.y) = 1
    println(p)
  })

  for (x <- Range(0, row)) {
    for (y <- Range(0, col)) {
      print("  " + matrix(x)(y).toString)
    }
    println
  }

}

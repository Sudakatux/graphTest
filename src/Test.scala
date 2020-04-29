
object Graph {
  val vertix = Set('B', 'D', 'E', 'F', 'H')
  val side = Set('A', 'B', 'C', 'E', 'H', 'I', 'G')
  val bottomTop = Set('A', 'C', 'D', 'E', 'I', 'G', 'F')

  val legalRoutes = Map(
    'D' -> Set(Set('A', 'G')),
    'B' -> Set(Set('A', 'C')),
    'F' -> Set(Set('C', 'I')),
    'H' -> Set(Set('G', 'I')),
    'E' -> Set(Set('B', 'H'), Set('D', 'F'), Set('A', 'I'), Set('G', 'C'))
  )

  val graph2 = Map(
    'A' -> vertix,
    'B' -> bottomTop,
    'C' -> vertix,
    'D' -> side,
    'E' -> Set('A', 'B', 'C', 'D', 'F', 'G', 'H', 'I'),
    'F' -> side,
    'G' -> vertix,
    'H' -> bottomTop,
    'I' -> vertix,
  )

  val countPatternsFrom = (startingPoint: Char, length: Int) => {

    def moveInGraph(possibleNextMovements: Set[Char], currentMovements: List[Char], solutions: Int): Int = {
      if (possibleNextMovements.isEmpty) return solutions

      val currentPoint = possibleNextMovements.head
      val movement: List[Char] = currentMovements :+ currentPoint;
      val newRoutes: Set[Char] = legalRoutes
        .mapValues(_.filter(pair => pair.contains(currentPoint)))
        .filter((t) => !t._2.isEmpty && movement.contains(t._1))
        .values
        .flatten
        .flatten
        .toSet

      // println(s"\nCurrent point ${currentPoint}\n New routes ${newRoutes} \n")

      val nextMovements: Set[Char] = (graph2
        .get(currentPoint).get ++ newRoutes)
        .filter(el => !(movement :+ startingPoint).contains(el))

      if (movement.length == length) {
        println(movement)
        return moveInGraph(possibleNextMovements.tail, currentMovements, solutions + 1)
      }

      moveInGraph(nextMovements, movement, solutions) + moveInGraph(possibleNextMovements.tail, currentMovements, solutions)
    }

    moveInGraph(graph2.get(startingPoint).get, List(startingPoint), 0)
  }

}


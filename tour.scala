object Knight {
    val moves = (for (
      x  <- Set(1, 2); 
      xd <- Set(-1, 1); 
      y  <- Set(1, 2) - x; 
      yd <- Set(-1, 1)
    ) yield (x * xd, y * yd))
}
 
case class Square(x: Int, y: Int) {
    def +(move: (Int, Int)) = move match {
        case (mx: Int, my: Int) => Square(x + mx, y + my)
    }
}
 
class Board(width: Int, height: Int) {
    val squares = for (x <- (0 to width - 1)) yield (for (y <- (0 to height - 1)) yield Square(x, y))
    def contains(square : Square) = squares.flatten.contains(square)
    val size = squares.flatten.size
}
 
type Tour = List[Square]
 
 
 
 
def bruteTours(board: Board) = {
    def addMove(tour: Tour) = 
        Knight.moves.
            map(tour.last + _).
            filter(board.contains).
            filter(!tour.contains(_)).
            map(tour :+ _)
    
    def buildTour(tours: List[Tour]): List[Tour] = 
        if (tours.filter(_.size < board.size).isEmpty) tours else buildTour(tours.flatMap(
            tour => if (tour.size < board.size) addMove(tour) else List(tour)))
 
    board.squares.flatten.flatMap((s: Square) => buildTour(List(List(s))))
}
 
 
 
def streamTours(board: Board) = {
    import Stream.Empty
    
    def trySquare(tour: Tour, square: Square, moves: List[(Int,Int)]): Stream[Tour] =
        if (!tour.contains(square) && board.contains(square)) buildTour(tour :+ square) #:::
            addMove(tour, moves) else addMove(tour, moves)
    
    def addMove(tour: Tour, moves: List[(Int,Int)]) = moves match {
        case move::movesTail => trySquare(tour, tour.last + move, movesTail)
        case Nil => Empty
    }
    
    def buildTour(tour: Tour) =
        if (tour.size == board.size) Stream(tour) else addMove(tour, Knight.moves.toList)
    
    def start(startingSquares: List[Square]) : Stream[Tour] = startingSquares match {
        case square::squaresTail => buildTour(List(square)) #::: start(squaresTail)
        case Nil => Empty
    }
    
    start(board.squares.flatten.toList)
}
package strategygames.abalone
package variant

import strategygames.Player
import strategygames.abalone.format.FEN

case object Dohyo
    extends Variant(
      id = 3,
      key = "dohyo",
      name = "Dohyō",
      standardInitialPosition = false,
      boardType = Hex4
    ) {
  override def perfIcon: Char = '\ue92C' // TODO

  override def perfId: Int = 702

  override def maxUsable: Option[Int] = Option(2)

  override def winningScore: Int = 9

  override def repetitionEnabled: Boolean = false

  override def winner(situation: Situation): Option[Player] = {
    var res = super.winner(situation)

    if (!res.isDefined) {
      // TODO the game stops after 24 rounds without capture, the winner is last having ejected, player 2 if none
    }

    res
  }

  /** 2-1 pushes only. */
  override def validMoves_lineCore_filter(
      situation: Situation,
      orig: Pos,
      dest: Pos,
      out: Boolean
  ): Boolean = {
    boardType.norm.dist(orig, dest) == 2
  }

  /** Rotations are allowed iff there is no push available. */
  override def validMovesCore(situation: Situation): List[(Pos, List[Move])] = {
    val res = validMoves_line(situation)
    (if (res.isEmpty) validMoves_jump(situation) else res).toList
  }

  /** Rotations of a given cell are allowed iff there is no push available anywhere. */
  override def validMovesCore(situation: Situation, a: Pos): List[Move] = {
    if (
      situation.board.pieces
        .filter(t => isUsable(situation, t._2))
        .exists { case (a, _) => !validMoves_lineCore(situation, a).isEmpty }
    ) validMoves_lineCore(situation, a)
    else validMoves_jumpCore(situation, a)
  }

  /** Rotations around a neighbouring pivot. */
  override def validMoves_jumpCore(situation: Situation, a: Pos): List[Move] = {
    boardType.norm
      .getNeigh(a)
      .flatMap { case (vect, _) =>
        var dests = List[Pos]()
        val pivot = a - vect;

        if (situation.board(pivot) == situation.board(a)) {
          var nvect    = boardType.norm.getNext(vect)
          var obstacle = false

          while (!obstacle) {
            val d = pivot + nvect
            obstacle = situation.board.isPiece(d) || !boardType.isCell(d)

            if (!obstacle) {
              dests :+= d
              nvect = boardType.norm.getNext(nvect)
            }
          }

          nvect = boardType.norm.getPrev(vect)
          obstacle = false
          var stop = false;

          while (!stop && !obstacle) {
            val d = pivot + nvect
            stop = dests.contains(d)

            if (!stop) {
              obstacle = situation.board.isPiece(d) || !boardType.isCell(d)

              if (!obstacle) {
                dests :+= d
                nvect = boardType.norm.getPrev(nvect)
              }
            }
          }
        }

        dests
      }
      .map(b => computeMove(a, b, situation))
      .toList
  }

  override protected def boardAfter_pieces(pieces: PieceMap, orig: Pos, dest: Pos): PieceMap = {
    if (boardType.norm(dest - orig) < 3 && !pieces.contains(dest))
      pieces - orig + (dest -> pieces(orig)) // Rotation
    else super.boardAfter_pieces(pieces, orig, dest)
  }

  override def initialFen: FEN =
    format.FEN("1SS1/SSSSS/1SSSS1/7/1ssss1/sssss/1ss1 0 0 b 0 1")
}

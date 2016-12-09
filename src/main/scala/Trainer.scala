/**
  * Created by wschoi on 2016-10-11.
  */
object Trainer {

/*  def train (max_Epoch: Int, num_of_player: Int, list: List[Player]) : Double = train(0, max_Epoch, num_of_player, list)
  def train (epoch: Int, max_Epoch: Int, num_of_player: Int, list: List[Player]) : Double = {
    0
  }*/

  def genPair[Type] (A: List[Type], B: List[Type]):List[(Type, Type)] = A.flatMap(x => B.map(y => (x,y)))
  def genPair[Type] (A: List[Type]):List[(Type, Type)] = genPair(A,A)

  def element_count[Type] (list: List[Type]): List[(Type, Int)] = {
    list.map(elem=> (elem, 1))
      .groupBy(x => x._1)
      .map(pair => (pair._1, pair._2.size))
      .toList.sortBy(pair => -pair._2)
  }


  def main(args: Array[String]) {

    val rulenum = 50
    val a = 0
    val size  = 100
    val max_Epoch = 60

    var gen:List[Player] = List.fill(size)(new RandomPlayer(rulenum))

    for ( i <- 1 to max_Epoch){

      val match_result: List[(Player, Int)] = tournament(gen)
      //val rule_set = element_count(match_result.map(_._1).flatMap(_.game_Rule)).take(rulenum * 10).map(_._1)
      //val next_gen_by_rule = List.fill( size/2 )( new RandomPlayer_withPool(rulenum, rule_set))


      val next_gen: List[combinedPlayer] = make_babies(rulenum, size/10 , tournament(gen) )


      gen = next_gen

      print(i+"th trial: " + gen + " \n")

    }

/*
    val random_gen:List[Player] = List.fill(10)(new RandomPlayer(10))
    val pair =gen.flatMap(x => random_gen.map(y => (x,y))).filterNot( pair => pair._1 == pair._2)
    val take_winners = pair.map( pair  => (pair._1, new GameBoard(pair._1, pair._2).fight_and_get_winner()))
    val take_winners_who_is_in_gen = pair.map( pair  => (pair._1, new GameBoard(pair._1, pair._2).fight_and_get_winner())).filter(_ != null).filter( pair => pair._1 == pair._2)
    val take_winners_who_is_notin_gen = pair.map( pair  => (pair._1, new GameBoard(pair._1, pair._2).fight_and_get_winner())).filter( pair => pair._1 != pair._2)
*/

    val take15 = tournament(gen).take(15).map(_._1)
    val ruleset = take15.flatMap(x => x.game_Rule).toSet

    val intersection = ruleset.map(player => (player, 1)).groupBy(x => x._1).map(pair => (pair._1, pair._2.size)).toList.sortBy(pair =>  - pair._2 ).take(15)

    println()


  }

  def make_babies(rulenum: Int, size: Int, match_result: List[(Player, Int)]): List[combinedPlayer] = {
    val best_genpair = genPair(match_result.take(size).map(_._1)).filterNot(pair => pair._1 == pair._2)
    val next_gen = best_genpair.map(pair => new combinedPlayer(pair._1, pair._2, rulenum))
    next_gen
  }

  def tournament(gen: List[Player]): List[(Player, Int)] = {
    val pair = genPair(gen).filterNot(pair => pair._1 == pair._2)
    val take_winners = pair.par.map(pair => new GameBoard(pair._1, pair._2).get_Stronger())

    val sorted = take_winners
      .map(player => (player, 1))
      .groupBy(x => x._1)
      .map(pair => (pair._1, pair._2.size))
      .toList.sortBy(pair => -pair._2)
    
    sorted
  }
}


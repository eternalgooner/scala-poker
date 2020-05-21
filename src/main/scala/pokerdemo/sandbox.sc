

//take in list of ints
//check for consecutive incremental numbers
//e.g. 2,3,4,5,6        is true as 5 consecutive increments
//e.g. 3,4,5,6,8,9,10   is false as no 5 consecutive increments
//return consecutive numbers in list, or empty

def getPossibleStraightSubsets(xs: List[Int]):List[List[Int]] = {
  val subset1 = xs.slice(0,5)
  val subset2 = xs.slice(1,6)
  val subset3 = xs.slice(2,7)

  List(subset1, subset2, subset3)
}

def checkForStraight (xs: List[Int]):List[Int] = {
  val sortedDistinctValues = xs.distinct.sorted
  val subsets = getPossibleStraightSubsets(sortedDistinctValues)

  //check for 4 consecutive incremental numbers after 1st num
  val isStraight: List[List[Int]] = for {
    subset <- subsets
    consecutiveIncrements = subset.sliding(2).count(a => a(0)+1 == a(1))
    if consecutiveIncrements == 4
  } yield subset

  isStraight.flatten
}

val cards = List(8,9,4,3,2,5,6)  //yes
val cards2 = List(8,9,4,2,2,5,6) //no
val cards3 = List(8,9,4,3,2,5,7) //no
val cards4 = List(2,14,10,8,13,11,12) //yes
val cards5 = List(8,6,4,3,2,3,6) //no
val result = checkForStraight(cards)
val result2 = checkForStraight(cards2)
val result3 = checkForStraight(cards3)
val result4 = checkForStraight(cards4)
val result5 = checkForStraight(cards5)

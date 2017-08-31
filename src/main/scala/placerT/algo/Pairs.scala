package placerT.algo

/**
 * Created by rdl on 31-08-17.
 */
object Pairs {

  def makeAllSortedPairs[T](l:List[T]):List[(T,T)] = {
    def makeAllSortedPairsWithHead(head:T,
                                   tail:List[T],
                                   toAppend:List[(T,T)]):List[(T,T)] = {
      tail match{
        case Nil => toAppend
        case other::newTail => (head,other) :: makeAllSortedPairsWithHead(head,newTail,toAppend)
      }
    }
    l match{
      case Nil => Nil
      case h::t => makeAllSortedPairsWithHead(h,t,makeAllSortedPairs(t))
    }
  }

}

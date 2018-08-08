/*
 * Copyright 2017 CETIC www.cetic.be
 * This is being developed for the TANGO Project: http://tango-project.eu
 *
 * Placer is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * Placer is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License  for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License along with Placer.
 * If not, see http://www.gnu.org/licenses/lgpl-3.0.en.html
 *
 */

package placer.algo

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

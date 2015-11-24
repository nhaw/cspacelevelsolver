package com.nhaw.cspacelevelsolver.solver

import com.nhaw.cspacelevelsolver.puzzle._
import com.nhaw.cspacelevelsolver.color._

import collection._
import scala.collection.mutable.ListBuffer

private[this] object TransitionPossibilities {
  def problemsFromRequirements(to: Node, es: Requirement.EntityStates) = to.reqs.filter(!_.satisfied(es))
  def subjectsFromProblems(problems: Seq[Requirement]) = problems.flatMap(_.subjects).toSet

  /**
   * Accept only unique actions (reject the same actions in different orders)
   */
  def uniqueActions(actions: List[Action]): List[Action] = immutable.Set[Action](actions: _*).toList
}

/**
 * Possible transitions from one Node to the
 * @param from Node which the player is leaving. May be null.
 * @param to Node to which player may travel. It is assumed that this node is topologically adjacent to the node from
 *           which the player is leaving.
 */
private[solver] class TransitionPossibilities(val from: Node, val to: Node,
                                              val inventory: immutable.Seq[Color],
                                              val entityStates: Requirement.EntityStates)
{
  import TransitionPossibilities._

  val entryProblems = problemsFromRequirements(to, entityStates)
  val RequirementTypes = entryProblems.isEmpty
  val problemSubjects: Requirement.EntitySet = subjectsFromProblems(entryProblems)

  //println(s"solving at node ${if (from != null) from.id else "null"}->${to.id}")
  //println(s"  to reqs=${to.reqs.mkString(",")}")

  private[this] def recursSolveProblems(currentProblems: Seq[Requirement], currentEntityStates: Requirement.EntityStates,
                                        effectedEnts: Set[Entity],
                                        compoundAction: CompoundColorChangeAction, remainingInventory: List[Color]): List[Action] = {
    //subjectsFromProblems(currentProblems).foreach { ent => // NOTE: Entities are reiterated because problem-list changes
    to.reqs.flatMap(_.subjects).toSet.toList.filter(!effectedEnts.contains(_)).foldLeft(List[Action]()) {
      (actions: List[Action], ent: Entity) => {
        //println(s"    testEnt:${ent.toString} problems:${currentProblems.size} actions:${compoundAction.actions.size} inv:${remainingInventory.mkString(",")} ")
        val newActions: List[Action] = {
          if (remainingInventory.nonEmpty) {
            val inventoryColor = remainingInventory.head
            //println(s"  testInventory ${}")
            if (compoundAction.contains(ent) || currentEntityStates(ent).color == inventoryColor) {
              // Already operated on this ent in a compound action (optimization) or this entity is this color!
              Nil
            } else {
              val newEntityStates = currentEntityStates + ((ent, currentEntityStates(ent) withColor inventoryColor))
              val newProblems = problemsFromRequirements(to, newEntityStates)
              val action = ColorChangeAction(inventoryColor, ent)
              if (newProblems.isEmpty) {
                // TODO: Include superfluous options in the solution if requested. It's useful to know how many solutions there actually are
                //println(s"      solved ${compoundAction}")
                (compoundAction + action) :: Nil // problems solved. Any further compound actions are superfluous so we'll exclude them
              } else {
                // NOTE: Can do this with the same color (different instance) applied to different entities.
                // Gels need to be Fungible so that there are no redundant solutions at a given level
                recursSolveProblems(newProblems, newEntityStates, effectedEnts + ent, compoundAction + action, remainingInventory.tail) // Attempt to fix the rest of the problem having consumed this inventory
              }
            }
          } else {
            Nil
          }
        }
        actions ++ newActions
      }
    }
  }

  val validActions: Seq[Action] = {
    if (problemSubjects.isEmpty) {
      // No actions required. Action can always be deferred until the last moment
      Seq[Action](NoAction())
    } else {
      // TODO: understand relationship between problems and inventory content to reduce brute force searching

      // For all the inventory colors, attack all problems with that inventory color plus, possibly, those that follow

      // Note: This approach requires that the order of color changes not affect the outcome
      // Note: This approach requires that there be no reason for 2 color-changes to ever be applied to the same entity
      // TODO: This does not evaluate all solutions since it may skip inventory application combinations that do not solve the problem - it is thus dependent on inventory order.
      // A proper implementation would either need to revisit inventory items earlier in the list when the problem set changes or disregard the problem set .. Whichever is smaller
      //val actions = mutable.ListBuffer[Action]()
      //for (i <- inventory.indices) {
      //  val inventoryColor = inventory(i)



      val rawActions = recursSolveProblems(entryProblems, entityStates, Set[Entity](), CompoundColorChangeAction(), inventory.toList)

      val actions = uniqueActions(rawActions)

      //}

      val filteredActions = actions.filter(_ != null).map {
          case cca: ColorChangeAction => cca
          case ccca: CompoundColorChangeAction if ccca.actions.size > 1 => ccca
          case ccca: CompoundColorChangeAction if ccca.actions.size == 1 => ccca.actions.head // simplify
          case ccca: CompoundColorChangeAction if ccca.actions.isEmpty => throw new RuntimeException("Do not know how an empty CompoundColorChangeAction was retrieved")
          case other => throw new RuntimeException(s"Do not know how to handle object $other")
        }

      filteredActions

      // Original 1-action-at-a-time approach
//      inventory.flatMap { inventoryColor =>
//        problemSubjects.filter { ent =>
//          entityStates(ent).color != inventoryColor  &&
//            {
//              val newEntityStates = entityStates + ((ent, entityStates(ent) withColor inventoryColor ))
//              val newProblems = problemsFromRequirements(newEntityStates)
//              newProblems.isEmpty
//            }
//        }.map(ColorChangeAction(inventoryColor ,_))
//      }
    }
  }

  val isDeadEnd = validActions.isEmpty
  val isPassable = !isDeadEnd
}

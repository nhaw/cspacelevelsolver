package com.nhaw.cspacelevelsolver.solver

import com.nhaw.cspacelevelsolver.puzzle._
import com.nhaw.cspacelevelsolver.color._

import collection._


/**
 * Possible transitions from one Node to the next
 */
class TransitionPossibilities(val from:Node, val to:Node,
                              val inventory:immutable.Seq[Color],
                              val entityStates:Requirement.EntityStates)
{
  def problems(es:Requirement.EntityStates) = to.reqs.filter(!_.satisfied(es))
  val entryProblems = problems(entityStates)
  val RequirementTypes = entryProblems.isEmpty
  val problemSubjects: Requirement.EntitySet = entryProblems.flatMap(_.subjects).toSet

  val validActions: Seq[Action] = {
    if (problemSubjects.isEmpty) {
      // No actions required. Action can always be deferred until the last moment
      Seq[Action](NoAction())
    } else {
      // TODO: All combinations of exclusive valid actions are valid... not limited to ONE AT A TIME
      // TODO: understand relationship between problems and inventory content to reduce brute force searching
      inventory.flatMap { invColor =>
        problemSubjects.filter { ent =>
          entityStates(ent).color != invColor &&
            {
              val newEntityStates = entityStates + ((ent, entityStates(ent) withColor invColor))
              val newProblems = problems(newEntityStates)
              newProblems.isEmpty
            }
        }.map(ColorChangeAction(invColor,_))
      }
    }
  }

  val isDeadEnd = validActions.isEmpty
  val isPassable = !isDeadEnd
}

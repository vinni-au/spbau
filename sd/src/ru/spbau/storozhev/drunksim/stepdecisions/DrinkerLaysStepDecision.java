package ru.spbau.storozhev.drunksim.stepdecisions;

import ru.spbau.storozhev.drunksim.core.Cell;
import ru.spbau.storozhev.drunksim.objects.DrinkerCellObject;
import ru.spbau.storozhev.drunksim.objects.DrinkerCellObject.DrinkerState;

public class DrinkerLaysStepDecision extends AbstractStepDecision {

	public DrinkerLaysStepDecision(int x, int y, Cell c) {
		super(x, y, c);
	}

	@Override
	public void doIt() {
		((DrinkerCellObject)cell.getObject()).setState(DrinkerState.Laying);
	}

	@Override
	public boolean isConflictedWith(AbstractStepDecision other) {
		return false;
	}

}

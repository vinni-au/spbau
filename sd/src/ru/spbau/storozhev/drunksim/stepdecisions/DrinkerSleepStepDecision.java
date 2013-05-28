package ru.spbau.storozhev.drunksim.stepdecisions;

import ru.spbau.storozhev.drunksim.core.*;
import ru.spbau.storozhev.drunksim.objects.*;
import ru.spbau.storozhev.drunksim.objects.DrinkerCellObject.DrinkerState;

public class DrinkerSleepStepDecision extends AbstractStepDecision {

	public DrinkerSleepStepDecision(int x, int y, Cell c) {
		super(x, y, c);
	}

	@Override
	public void doIt() {
		DrinkerCellObject drinker = (DrinkerCellObject)cell.getObject();		
		drinker.setState(DrinkerState.Sleeping);
	}

}

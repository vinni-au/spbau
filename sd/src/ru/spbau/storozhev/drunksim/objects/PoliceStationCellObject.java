package ru.spbau.storozhev.drunksim.objects;

import ru.spbau.storozhev.drunksim.core.*;
import ru.spbau.storozhev.drunksim.stepdecisions.*;

public class PoliceStationCellObject extends AbstractCellObject {

	public PoliceStationCellObject(Cell c) {
		super(c);
	}

	@Override
	public boolean isWalkableThru() {
		return false;
	}

	@Override
	public char toChar() {
		return 'S';
	}

	@Override
	public AbstractStepDecision makeStep() {
		return null;
	}

}

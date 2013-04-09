package ru.spbau.storozhev.drunksim.objects;

import ru.spbau.storozhev.drunksim.core.*;
import ru.spbau.storozhev.drunksim.stepdecisions.*;

public class PillarCellObject extends AbstractCellObject {

	public PillarCellObject(Cell c) {
		super(c);
	}

	@Override
	public boolean isWalkableThru() {
		return false;
	}

	@Override
	public char toChar() {
		return 'I';
	}

	@Override
	public AbstractStepDecision makeStep(int no) {
		return null;
	}

}

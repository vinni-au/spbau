package ru.spbau.storozhev.drunksim.objects;

import ru.spbau.storozhev.drunksim.core.*;
import ru.spbau.storozhev.drunksim.stepdecisions.*;

public class LamppostCellObject extends AbstractCellObject {

	public LamppostCellObject(Cell c) {
		super(c);
	}

	@Override
	public boolean isWalkableThru() {
		return false;
	}

	@Override
	public char toChar() {
		return 'L';
	}

	@Override
	public AbstractStepDecision makeStep(int no) {
		// TODO Auto-generated method stub
		return null;
	}

}
